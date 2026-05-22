;;; pwb.el --- Prompting with buffer  -*- lexical-binding: t; -*-

;; Copyright (C)   2026 pxel8063

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.9
;; Keywords:   lisp
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

;; pwb (Prompting with Buffer) is an Emacs interface for the Claude API.
;;
;; It allows you to send buffer content as prompts to Claude and receive
;; responses directly in Emacs.  Features include:
;; - Conversation history tracking across multiple prompts
;; - Customizable system prompts
;; - Support for prefilled assistant responses
;;
;; Basic usage:
;;   M-x pwb-current-buffer RET
;;
;; This will send the current buffer to Claude and display the response
;; in the *Claude* buffer.

;;; Code:
(require 'cl-lib)
(require 'auth-source)

(defgroup pwb nil
  "Custom variables of pwb."
  :group 'local)

(defcustom pwb-model "claude-haiku-4-5"
  "String to specify claude model."
  :group 'pwb
  :type 'string)

(defcustom pwb-max-tokens 1024
  "The number of max_tokens."
  :group 'pwb
  :type 'natnum)

(defcustom pwb-system-prompt "When possible, use org-mode syntax."
  "The string of system prompt."
  :group 'pwb
  :type 'string)

(defcustom pwb-api-url "https://api.anthropic.com/v1/messages"
  "Specifing the Claude message API host."
  :group 'pwb
  :type 'string)

(defcustom pwb-api-host "api.anthropic.com"
  "Machine name of the api in `auth-source'."
  :group 'pwb
  :type 'string)

(defcustom pwb-anthropic-version "2023-06-01"
  "Specifing the Claude anthropic version.
Like curl -H anthropic-version: 2023-06-01"
  :group 'pwb
  :type 'string)

(defconst pwb-response-buffer "*Claude*"
  "The name of buffer for the response from Claude.")

(cl-defstruct pwb-messages (turns []))
(defvar pwb-messages (make-pwb-messages) "Holding multiple turns.")

(defvar pwb-body-params nil)

(defun pwb-param-key= (a b)
  "Compare the key of the lisp object intended to serialize to JSON. If the
keys are the same, return true."
  (eq (car a) (car b)))

(defun pwb-merge-params (&rest params)
  "Merge alist of params. The order of the precedents is from left to right"
  (let ((accum (apply #'append params)))
    (seq-uniq accum #'pwb-param-key=)))

(defun pwb-build-alist-from-custom ()
  (list (cons 'max_tokens pwb-max-tokens)
        (cons 'model pwb-model)
        (cons 'system pwb-system-prompt)))

(defun pwb-messages-param (messages)
  (list (cons 'messages messages)))

(defun pwb-concat-turns (turns &rest new-turns)
  "Combine TURNS with NEW-TURNS into a single turns sequence."
  (apply #'vconcat turns new-turns))

(defun pwb-user-turn (content)
  (vector (list (cons 'role "user") (cons 'content content))))

(defun pwb-assistant-turn (content)
  (vector (list (cons 'role "assistant") (cons 'content content))))

(defun pwb-add-conversation (turns u-content a-content)
  "Add conversation of U-CONTENT(user content) and A-CONTENT.
Return the vector of turns'."
  (pwb-concat-turns turns
                    (pwb-user-turn u-content)
                    (pwb-assistant-turn a-content)))

(defun pwb-credential (host)
  "Get the credential from the `auth-source'."
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
  "Parse the current buffer, if narrowed, the narrowed part."
  (buffer-substring-no-properties (point-min) (point-max)))

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
(defun pwb-async-current-buffer ()
  "Send a prompt based on the current buffer to api."
  (interactive)
  (let* ((prompt (pwb-buffer-string))
         (turns (pwb-messages-turns pwb-messages))
         (msgs (pwb-messages-param (vconcat turns (pwb-user-turn prompt))))
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
                  (response-thinking (pwb-get-content-thinking response)))
              (setf (pwb-messages-turns pwb-messages)
                    (pwb-add-conversation turns prompt response-text))
              (when response-thinking
                (message "thinking: %s" response-thinking))
              (pwb-render-response response-text)
              (display-buffer pwb-response-buffer)
              (message "pwb: response received.")
              t)
         (progn (message "pwb: error; %S" response)
                (message "pwb: response received.")
                nil))))))

;;;###autoload
(defun pwb-current-buffer ()
  "Send a prompt based on the current buffer to api."
  (interactive)
  (let* ((prompt (pwb-buffer-string))
         (turns (pwb-messages-turns pwb-messages))
         (msgs
          (pwb-messages-param (pwb-concat-turns turns
                                                (pwb-user-turn prompt))))
         (alst (pwb-merge-params msgs
                                 pwb-body-params
                                 (pwb-build-alist-from-custom)))
         (response (pwb-curl (json-serialize alst))))
    (if (pwb-response-ok-p response)
         (let ((turns (pwb-messages-turns pwb-messages))
               (response-text (pwb-get-content-text response))
               (response-thinking (pwb-get-content-thinking response)))
           (setf (pwb-messages-turns pwb-messages)
                 (pwb-add-conversation turns prompt response-text))
           (when response-thinking
             (message "thinking: %s" response-thinking))
           (pwb-render-response response-text)
           (display-buffer pwb-response-buffer)
           t)
      (progn (message "pwb: error; %S" response)
             nil))))

;;;###autoload
(defun pwb-save-conversation (file)
  "Save the conversation to FILE."
  (interactive "FFile to save conversation: ")
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 pwb-messages (current-buffer)))))

;;;###autoload
(defun pwb-restore-conversation (file)
  "Restore the conversation from FILE."
  (interactive "fFile to restore conversation: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((data (read (current-buffer))))
      (unless (pwb-messages-p data)
        (error "File does not contain a valid pwb-messages struct"))
      (setf pwb-messages data))))

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
  (setf pwb-messages (make-pwb-messages)))

(defmacro pwb-with-response-buffer (&rest body)
  "Macro for response buffer printing."
  `(with-current-buffer (get-buffer-create pwb-response-buffer)
     (save-excursion
       (goto-char (point-max))
       ,@body)))

;;;###autoload
(defun pwb-print-assistant-turns ()
  "Print the assistant turns into `'pwb-response-buffer'."
  (pwb-with-response-buffer
   (insert (seq-reduce
            (lambda (acc x)
              (if (equal (alist-get 'role x) "assistant")
                  (concat acc (alist-get 'content x))
                acc))
            (pwb-messages-turns pwb-messages)
            ""))))

(defun pwb-get-content-text (response)
  "Return content text in the RESPONSE."
  (pwb-find-type-from-content "text" (alist-get 'content response)))

(defun pwb-get-content-thinking (response)
  "Return content thinking in the RESPONSE."
  (pwb-find-type-from-content "thinking" (alist-get 'content response)))


(defun pwb-find-type-from-content (type content)
  "Return the text that belongs to type \"type\".
The first argument must be STRING."
  (alist-get (intern type)
             (seq-find (lambda (x) (equal (alist-get 'type x) type)) content)))

(defun pwb-render-response (string)
  "Create a buffer for displaying the response.
Then insert STRING and newline in this buffer."
  (pwb-with-response-buffer
    (newline 2)
    (insert string)))

(defun pwb-response-ok-p (response)
  "Test whether the RESPONSE is error or not."
  (pcase (alist-get 'type response)
    ("error" nil)
    ("message" t)
    (other (message "pwb: unexpected response type: %S" other) nil)))

(provide 'pwb)
;;; pwb.el ends here
