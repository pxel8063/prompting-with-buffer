;;; pwb.el --- Prompting with buffer  -*- lexical-binding: t; -*-

;; Copyright (C)   2026 pxel8063
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.20
;; Keywords:   comm, convenience
;; Package-Requires: ((emacs "29.1"))
;; URL:        https://github.com/pxel8063/prompting-with-buffer

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; pwb (Prompting with Buffer) is a lightweight Emacs client for
;; Anthropic's Claude API.  It sends buffer contents as prompts and
;; displays responses in a dedicated response buffer.  Conversation
;; history is maintained per source buffer, so you can carry on
;; multi-turn dialogues without leaving Emacs.
;;
;; Features:
;;
;;   - Synchronous and asynchronous request commands.
;;   - Per-buffer conversation history (buffer-local `pwb-messages').
;;   - Optional mid-conversation system messages via a prefix
;;     argument.
;;   - Save and restore conversations to disk.
;;   - Customizable model, max tokens, system prompt, and arbitrary
;;     additional request parameters via `pwb-body-params'.
;;   - API key retrieval through `auth-source'.
;;
;; Basic usage:
;;
;;   M-x pwb-current-buffer RET
;;
;; This sends the current buffer to Claude (synchronously) and
;; appends the response to the buffer named by `pwb-response-buffer'
;; (default: *Claude*).  For non-blocking requests, use
;; `pwb-async-current-buffer' instead.  With a prefix argument
;; (\\[universal-argument]), either command prompts for a
;; mid-conversation system message.
;;
;; Authentication:
;;
;; The API key is looked up via `auth-source' using the host given
;; by `pwb-api-host' (default: api.anthropic.com).  For example,
;; add a line to ~/.authinfo.gpg:
;;
;;   machine api.anthropic.com password sk-ant-...
;;
;; Conversation management:
;;
;;   - `pwb-clear-conversation' resets the history for the current
;;     buffer.
;;   - `pwb-save-conversation' / `pwb-restore-conversation' persist
;;     the history to a file as a readable Lisp form.
;;   - `pwb-set-system-prompt' uses the current buffer's contents
;;     as the system prompt for subsequent requests;
;;     `pwb-clear-system-prompt' clears it.
;;   - `pwb-print-assistant-turns' inserts all assistant replies
;;     from the current buffer's history into the response buffer.
;;
;; Customization:
;;
;; See the `pwb' customization group.  Key variables include
;; `pwb-model', `pwb-max-tokens', `pwb-system-prompt',
;; `pwb-api-url', `pwb-anthropic-version', `pwb-response-buffer',
;; `pwb-response-before-hook', and `pwb-body-params'.  Parameters
;; in `pwb-body-params' take precedence over the dedicated
;; customization variables and may be used to enable features such
;; as extended thinking or prompt caching.
;;
;; Requirements:
;;
;; Emacs 29.1 or later and the `curl' command-line tool on PATH.

;;; Code:
(require 'cl-lib)
(require 'auth-source)

(defgroup pwb nil
  "Custom variables of pwb."
  :group 'local)

(defcustom pwb-model "claude-opus-4-8"
  "String to specify claude model."
  :group 'pwb
  :type 'string)

(defcustom pwb-max-tokens 1024
  "Maximum number of tokens in the API response."
  :group 'pwb
  :type 'natnum)

(defcustom pwb-system-prompt "When possible, use org-mode syntax."
  "System prompt set with every API request."
  :group 'pwb
  :type 'string)

(defcustom pwb-api-url "https://api.anthropic.com/v1/messages"
  "Specifying the Claude message API host."
  :group 'pwb
  :type 'string)

(defcustom pwb-api-host "api.anthropic.com"
  "Machine name of the api in `auth-source'."
  :group 'pwb
  :type 'string)

(defcustom pwb-anthropic-version "2023-06-01"
  "Anthropic API version string sent in the
`anthropic-version` header."
  :group 'pwb
  :type 'string)

(defcustom pwb-response-before-hook nil
  "Hook run just before writing `pwb-response-buffer'
as the current buffer with the response from the API."
  :group 'pwb
  :type 'hook)

(defcustom pwb-response-buffer "*Claude*"
  "The name of buffer for the response from Claude."
  :group 'pwb
  :type 'string)

(cl-defstruct pwb-messages (turns []))
(defvar pwb-messages (make-pwb-messages)
  "Conversation history holding multiple turns.")

(defcustom pwb-body-params nil "Alist of additional parameters to include in API requests.
These take precedence over `pwb-model', `pwb-max-tokens', and
`pwb-system-prompt'.  See the Anthropic Messages API documentation
for available parameters."
  :group 'pwb
  :type 'sexp)

(defun pwb-param-key= (a b)
  "Compare the key of the argument intended to serialize to JSON.
If the key A and the key B are the same, return true."
  (eq (car a) (car b)))

(defun pwb-merge-params (&rest params)
  "Merge alist of PARAMS.
The order of the precedents is from left to right."
  (let ((accum (apply #'append params)))
    (seq-uniq accum #'pwb-param-key=)))

(defun pwb-build-alist-from-custom ()
  "Construct the params from the customized variables.
These params are essential for the query."
  (list (cons 'max_tokens pwb-max-tokens)
        (cons 'model pwb-model)
        (cons 'system pwb-system-prompt)))

(defun pwb-messages-param (messages)
  "Construct MESSAGES params."
  (list (cons 'messages messages)))

(defun pwb-concat-turns (previous-turns &rest new-turns)
  "Combine PREVIOUS-TURNS with NEW-TURNS into a single Turn sequence."
  (apply #'vconcat previous-turns new-turns))

(defun pwb-user-turn (content)
  "Construct the user turn form CONTENT."
  (vector (list (cons 'role "user") (cons 'content content))))

(defun pwb-assistant-turn (content)
  "Construct the assistant turn from CONTENT."
  (vector (list (cons 'role "assistant") (cons 'content content))))

(defun pwb-system-turn (content)
  "Construct the system turn from CONTENT."
  (if content
      (vector (list (cons 'role "system") (cons 'content content)))
    nil))

(defun pwb-add-conversation (turns u-turn s-turn a-turn)
  "Add conversation of U-CONTENT(user content), IBASE64 if non-nil
,S-CONTENT if non-nil and A-CONTENT. Return the vector of TURNS'."
  (pwb-concat-turns turns
                    u-turn
                    (when s-turn
                      s-turn)
                    a-turn))

(defun pwb-credential (host)
  "Get the credential for HOST from the `auth-source'."
  (auth-source-pick-first-password :host host))

(defun pwb-curl (payload)
  "Invoke curl with PAYLOAD."
  (let ((url pwb-api-url)
        (api-key (pwb-credential pwb-api-host))
        (anthropic-version pwb-anthropic-version)
        (application-json "content-type: application/json"))
    (unless api-key
      (error "%s can not be found in `auth-source'" pwb-api-host))
    (with-temp-buffer
      (let ((status (call-process "curl" nil t nil url "-s"
                                  "-H" (concat "x-api-key: " api-key)
                                  "-H" (concat "anthropic-version: " anthropic-version)
                                  "-H" application-json
                                  "-d" payload)))
        (unless (zerop status)
          (error "Curl failed with status %d: %s" status (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(defun pwb-buffer-string ()
  "Return prompt string, which is,  if the region is active, one in the region,
if narrowed, one in the narrowed part."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max))))

(defun pwb-curl-async (payload callback)
  "Invoke curl with PAYLOAD asynchronously.
CALLBACK is called with the parsed response alist when the
process finishes.  On failure, an error is signaled."
  (let* ((url pwb-api-url)
         (api-key (pwb-credential pwb-api-host))
         (anthropic-version pwb-anthropic-version)
         (buf (generate-new-buffer " *pwb-curl*")))
    (unless api-key
      (error "%s can not be found in `auth-source'" pwb-api-host))
    (let ((proc (make-process
                 :name "pwb-curl"
                 :buffer buf
                 :command (list "curl" url "-s"
                                "-H" (concat "x-api-key: " api-key)
                                "-H" (concat "anthropic-version: " anthropic-version)
                                "-H" "content-type: application/json"
                                "-d" payload)
                 :sentinel
                 (lambda (process event)
                   (unwind-protect
                       (let ((exit-status (process-exit-status process))
                             (output-buf (process-buffer process)))
                         (cond
                          ((not (eq (process-status process) 'exit))
                           (message "pwb: curl process %s" (string-trim event)))
                          ((not (zerop exit-status))
                           (message "pwb: curl failed with status %d: %s"
                                    exit-status
                                    (with-current-buffer output-buf
                                      (buffer-string))))
                          (t
                           (let ((response
                                  (with-current-buffer output-buf
                                    (goto-char (point-min))
                                    (json-parse-buffer :object-type 'alist))))
                             (funcall callback response)))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf)))))))
      proc)))

;;;###autoload
(defun pwb-async-current-buffer (&optional arg)
  "Send a prompt based on the current buffer to api.
With one \\[universal-argument], prompt for an image file.  With two
\\[universal-argument], prompt for a mid-conversation system message."
  (interactive "P")
  (make-local-variable 'pwb-messages)
  (let* ((prompt (pwb-buffer-string))
         (system (if (equal arg '(32))
                     (read-string "Enter mid-conversation system message: ")
                   nil))
         (image (if (equal arg '(4))
                    (let* ((image-file
                            (read-file-name "Image jpeg file: ")))
                      (pwb-convert-file-base64 image-file))
                  nil))
         (turns (pwb-messages-turns pwb-messages))
         (msgs
          (pwb-messages-param
           (pwb-concat-turns turns
                             (if image
                                 (pwb-user-turn-with-image prompt image)
                               (pwb-user-turn prompt))
                             (pwb-system-turn system))))
         (alst (pwb-merge-params msgs
                                 pwb-body-params
                                 (pwb-build-alist-from-custom)))
         (payload (json-serialize alst)))
    (message "pwb: sending request...")
    (pwb-curl-async
     payload
     (lambda (response)
       (if (pwb-response-ok-p response)
            (let ((response-text (pwb-get-content-text response))
                  (response-thinking (pwb-get-content-thinking response))
                  (response-stop-reason (pwb-get-stop-reason response))
                  (response-usage (pwb-get-usage response)))
              (setf (pwb-messages-turns pwb-messages)
                    (pwb-add-conversation turns
                                          (if image
                                              (pwb-user-turn-with-image prompt image)
                                            (pwb-user-turn prompt))
                                          (if system
                                              (pwb-system-turn system)
                                            nil)
                                          (pwb-assistant-turn response-text)))
              (when response-thinking
                (message "thinking: %s" response-thinking))
              (pwb-render-response response-text)
              (display-buffer pwb-response-buffer)
              (message "pwb: response received.")
              (message "stop reason: %s, usage: %s" response-stop-reason response-usage)
              t)
         (pwb-render-error-response response)
         (message "pwb: error; %S" response)
         (message "pwb: response received."))))))

;;;###autoload
(defun pwb-current-buffer (&optional arg)
  "Send a prompt based on the current buffer to api.
With one \\[universal-argument], prompt for an image file.  With two
\\[universal-argument], prompt for a mid-conversation system message."
  (interactive "P")
  (make-local-variable 'pwb-messages)
  (let* ((prompt (pwb-buffer-string))
         (system (if (equal arg '(32))
                     (read-string "Enter mid-conversation system message: ")
                   nil))
         (image (if (equal arg '(4))
                    (let* ((image-file
                            (read-file-name "Image jpeg file: ")))
                      (pwb-convert-file-base64 image-file))
                  nil))
         (turns (pwb-messages-turns pwb-messages))
         (msgs
          (pwb-messages-param
           (pwb-concat-turns turns
                             (if image
                                 (pwb-user-turn-with-image prompt image)
                               (pwb-user-turn prompt))
                             (pwb-system-turn system))))
         (alst (pwb-merge-params msgs
                                 pwb-body-params
                                 (pwb-build-alist-from-custom)))
         (response (pwb-curl (json-serialize alst))))
    (if (pwb-response-ok-p response)
         (let ((response-text (pwb-get-content-text response))
               (response-thinking (pwb-get-content-thinking response))
               (response-stop-reason (pwb-get-stop-reason response))
               (response-usage (pwb-get-usage response)))
           (setf (pwb-messages-turns pwb-messages)
                 (pwb-add-conversation turns
                                       (if image
                                           (pwb-user-turn-with-image prompt image)
                                         (pwb-user-turn prompt))
                                       (if system
                                              (pwb-system-turn system)
                                            nil)
                                       (pwb-assistant-turn response-text)))
           (when response-thinking
             (message "thinking: %s" response-thinking))
           (message "stop reason: %s, usage: %s" response-stop-reason response-usage)
           (pwb-render-response response-text)
           (display-buffer pwb-response-buffer)
           t)
      (pwb-render-error-response response)
      (message "pwb: error; %S" response)
      nil)))

;;;###autoload
(defun pwb-large-current-buffer (&optional arg)
  "Send a prompt based on the current buffer to api.
With one \\[universal-argument], prompt for an image file.  With two
\\[universal-argument], prompt for a mid-conversation system message."
  (interactive "P")
  (make-local-variable 'pwb-messages)
  (let* ((prompt (pwb-buffer-string))
         (system (if (equal arg '(32))
                     (read-string "Enter mid-conversation system message: ")
                   nil))
         (image (if (equal arg '(4))
                    (let* ((image-file
                            (read-file-name "Image jpeg file: ")))
                      (pwb-convert-file-base64 image-file))
                  nil))
         (turns (pwb-messages-turns pwb-messages))
         (msgs
          (pwb-messages-param
           (pwb-concat-turns turns
                             (if image
                                 (pwb-user-turn-with-image prompt image)
                               (pwb-user-turn prompt))
                             (pwb-system-turn system))))
         (alst (pwb-merge-params msgs
                                 pwb-body-params
                                 (pwb-build-alist-from-custom)))
         (response (pwb-curl-with-config alst)))
    (if (pwb-response-ok-p response)
        (let ((response-text (pwb-get-content-text response))
              (response-thinking (pwb-get-content-thinking response))
              (response-stop-reason (pwb-get-stop-reason response))
              (response-usage (pwb-get-usage response)))
          (setf (pwb-messages-turns pwb-messages)
                (pwb-add-conversation turns
                                      (if image
                                          (pwb-user-turn-with-image prompt image)
                                        (pwb-user-turn prompt))
                                      (if system
                                          (pwb-system-turn system)
                                        nil)
                                      (pwb-assistant-turn response-text)))
          (when response-thinking
            (message "thinking: %s" response-thinking))
          (message "stop reason: %s, usage: %s" response-stop-reason response-usage)
          (pwb-render-response response-text)
          (display-buffer pwb-response-buffer)
          t)
      (pwb-render-error-response response)
      (message "pwb: error; %S" response)
      nil)))

(defun pwb-curl-with-config (payload)
  "Make a curl config file based on PAYLOAD, invoke curl by calling
process, return the response."
  (let ((config (pwb-make-curl-config-file payload))
        (response))
    (with-temp-buffer
      (let ((status (call-process "curl" nil t nil "--no-progress-meter"
                                  "--config"
                                  config)))
        (unless (zerop status)
          (error "Curl failed with status %d: %s" status (buffer-string))))
      (goto-char (point-min))
      (setq response (json-parse-buffer :object-type 'alist))
      (delete-file config)
      response)))

;;;###autoload
(defun pwb-save-conversation (file)
  "Save the conversation to FILE."
  (interactive "FFile to save conversation: ")
  ;; Store pwb-messgages before the current buffer change to with-temp-file
  ;; buffer.
  (let ((msgs pwb-messages))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (princ ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
               (current-buffer))
        (prin1 msgs (current-buffer))))))

;;;###autoload
(defun pwb-restore-conversation (file)
  "Restore the conversation from FILE."
  (interactive "fFile to restore conversation: ")
  (make-local-variable 'pwb-messages)
  (let ((cbuf (current-buffer)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data (read (current-buffer))))
        (unless (pwb-messages-p data)
          (error "File does not contain a valid pwb-messages struct"))
        (with-current-buffer cbuf
          (setf pwb-messages data))))))

;;;###autoload
(defun pwb-set-system-prompt ()
  "Set system prompt string to the current buffer."
  (interactive)
  (setq pwb-system-prompt (pwb-buffer-string)))

;;;###autoload
(defun pwb-clear-system-prompt ()
  "Clear system prompt."
  (interactive)
  (setq pwb-system-prompt ""))

;;;###autoload
(defun pwb-clear-conversation ()
  "Clear the conversation history."
  (interactive)
  (make-local-variable 'pwb-messages)
  (setf pwb-messages (make-pwb-messages)))

(defmacro pwb-with-response-buffer (&rest body)
    "Execute BODY with `pwb-response-buffer' as the current buffer.
Point is moved to the end of the buffer and `pwb-response-before-hook'
is run before BODY."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create pwb-response-buffer)
     (save-excursion
       (goto-char (point-max))
       (run-hooks 'pwb-response-before-hook)
       ,@body)))

;;;###autoload
(defun pwb-print-assistant-turns ()
  "Print the assistant turn into `pwb-response-buffer'."
  (interactive)
  (let ((cbuf (current-buffer)))
    (pwb-with-response-buffer
     (insert (seq-reduce
              (lambda (acc x)
                (if (equal (alist-get 'role x) "assistant")
                    (concat acc (alist-get 'content x))
                  acc))
              (with-current-buffer cbuf
                (pwb-messages-turns pwb-messages))
              "")))))

(defun pwb-get-content-text (response)
  "Return content text in the RESPONSE."
  (pwb-find-type-from-content "text" (alist-get 'content response)))

(defun pwb-get-content-thinking (response)
  "Return content thinking in the RESPONSE."
  (pwb-find-type-from-content "thinking" (alist-get 'content response)))

(defun pwb-get-stop-reason (response)
  "Return a stop reason in the RESPONSE."
  (alist-get 'stop_reason response))

(defun pwb-get-usage (response)
  "Return a stop reason in the RESPONSE."
  (alist-get 'usage response))

(defun pwb-find-type-from-content (type content)
  "Return the text that belongs to TYPE.
The CONTENT argument must be STRING."
  (alist-get (intern type)
             (seq-find (lambda (x) (equal (alist-get 'type x) type)) content)))

(defun pwb-render-response (string)
  "Create a buffer for displaying the response.
Then insert STRING and newline in this buffer."
  (pwb-with-response-buffer
    (newline 2)
    (insert string)))

(defun pwb-render-error-response (response)
  "Render error RESPONSE in the response buffer.
RESPONSE is an alist parsed from the API's JSON error body."
  (let ((json (json-serialize response)))
    (pwb-with-response-buffer
     (let ((marker (make-marker)))
       (newline 2)
       (set-marker marker (point))
       (insert json)
       (json-pretty-print marker (point))))))

(defun pwb-response-ok-p (response)
  "Test whether the RESPONSE is error or not."
  (pcase (alist-get 'type response)
    ("error" nil)
    ("message" t)
    (other (message "pwb: unexpected response type: %S" other) nil)))

(defun pwb-convert-file-base64 (file)
  "Return the base 64 string of the image FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pwb-user-turn-with-image (content ibase64)
  "Construct the user turn from CONTENT and image string IBASE64."
  (vector
   (list (cons 'role "user")
         (cons 'content (vconcat (list
                                  (list (cons 'type "image")
                                        (list 'source
                                              (cons 'type "base64")
                                              (cons 'media_type "image/jpeg")
                                              (cons 'data ibase64))))
                                 (list
                                  (list (cons 'type "text")
                                        (cons 'text content))))))))

(defun pwb-make-curl-config-file (payload)
  "Make a temporary curl config file and return its filename. PAYLOAD is
alist. The caller is responsible to delete the temporary file after it
has done."
  (let ((tmpfile (make-temp-file "pwb-"))
        (coding-system-for-write 'utf-8))
    (with-temp-file tmpfile
      (let ((key (pwb-credential pwb-api-host)))
        (unless key
          (error "%s can not be found in `auth-source'" pwb-api-host))
        (insert "url " pwb-api-url "\n")
        (insert "-H " "\"x-api-key: " key "\"\n")
        (insert "-H " "\"anthropic-version: " pwb-anthropic-version "\"\n")
        (insert "-H " "\"content-type: application/json\"\n")
        (insert "-d " (prin1-to-string (decode-coding-string
                                        (json-serialize payload)
                                        'utf-8)))))
    tmpfile))

(provide 'pwb)
;;; pwb.el ends here
