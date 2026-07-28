;;; pwb.el --- Prompting with buffer  -*- lexical-binding: t; -*-

;; Copyright (C)   2026 pxel8063
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.26
;; Keywords:   comm, convenience
;; Package-Requires: ((emacs "30.1"))
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
  "Anthropic API version string sent in the `anthropic-version` header."
  :group 'pwb
  :type 'string)

(defcustom pwb-response-before-hook nil
  "Run this hook just before writing `pwb-response-buffer'."
  :group 'pwb
  :type 'hook)

(defcustom pwb-response-buffer "*Claude*"
  "The name of buffer for the response from Claude."
  :group 'pwb
  :type 'string)

(cl-defstruct pwb-messages (turns []))
(defvar pwb-messages (make-pwb-messages)
  "Conversation history holding multiple turns.")

(defcustom pwb-body-params nil "The additional parameters for API.
These take precedence over `pwb-model', `pwb-max-tokens', and
`pwb-system-prompt'.  See the Anthropic Messages API documentation for
available parameters."
  :group 'pwb
  :type 'sexp)

(defun pwb-assistant-turn-2 (content)
  "Construct the assistant turn from CONTENT."
  (list (cons 'role "assistant") (cons 'content content)))

(defun pwb-credential (host)
  "Get the credential for HOST from the `auth-source'."
  (auth-source-pick-first-password :host host))

(defun pwb-buffer-string ()
  "Return prompt string from current buffer.
If the region is active, one in the region, if narrowed, one in the
narrowed part."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pwb-prepare-payload (arg)
  "Return payload alist.
ARG is the unversal argument."
  (let ((prompt (pwb-buffer-string)))
    (cond ((equal arg '(16))
           (let* ((system (read-string "Enter mid-conversation system message.")))
             (pwb-payload-with-prompt-and-system (pwb-messages-turns pwb-messages)
                                                 prompt
                                                 system
                                                 pwb-max-tokens
                                                 pwb-model
                                                 pwb-system-prompt
                                                 pwb-body-params )))
          ((equal arg '(4))
           (let* ((image-file
                   (read-file-name "Image png file: "))
                  (image (pwb-convert-file-base64 image-file)))
             (pwb-payload-with-prompt-and-image (pwb-messages-turns pwb-messages)
                                                prompt
                                                image
                                                pwb-max-tokens
                                                pwb-model
                                                pwb-system-prompt
                                                pwb-body-params )))
          (t (pwb-payload-with-prompt (pwb-messages-turns pwb-messages)
                                      prompt
                                      pwb-max-tokens
                                      pwb-model
                                      pwb-system-prompt
                                      pwb-body-params)))))


;;;###autoload
(defun pwb-current-buffer (&optional arg)
  "Send a prompt based on the current buffer to api.
By taking ARG, with one \\[universal-argument], prompt for an image
file.  With two \\[universal-argument], prompt for a mid-conversation
system message."
  (interactive "P")
  (make-local-variable 'pwb-messages)
  (let* ((alst (pwb-prepare-payload arg))
         (response (pwb-curl-with-config alst))
         (assistant-turn
          (pwb-response-to-assistant-turn response)))
    (if assistant-turn ; If an error is returned, do nothing
        (setf (pwb-messages-turns pwb-messages)
              (pwb-concat-turns-2 (alist-get 'messages alst)
                                  assistant-turn)))))

;;;###autoload
(defun pwb-async-current-buffer (&optional arg)
  "Send a prompt based on the current buffer to api.
By taking ARG, with one \\[universal-argument], prompt for an image
file.  With two \\[universal-argument], prompt for a mid-conversation
system message."
  (interactive "P")
  (make-local-variable 'pwb-messages)
  (let* ((alst (pwb-prepare-payload arg)))
    (message "pwb: sending request...")
    (pwb-async-curl-with-config
     alst
     (lambda (response)
       (let ((assistant-turn
              (pwb-response-to-assistant-turn response)))
         (if assistant-turn ; If an error is returned, do nothing
             (setf (pwb-messages-turns pwb-messages)
                   (pwb-concat-turns-2 (alist-get 'messages alst)
                                       assistant-turn))))))))

(defun pwb-response-to-assistant-turn (response)
  "Return assistant turn from RESPONSE.
Render response in `pwb-response-buffer'.  If the RESPONSE is error,
render the error in `pwb-response-buffer' and return nil."
  (if (pwb-response-ok-p response)
      (let ((response-text (pwb-get-content-text response))
            (response-thinking (pwb-get-content-thinking response))
            (response-stop-reason (pwb-get-stop-reason response))
            (response-usage (pwb-get-usage response)))
        (when response-thinking
          (message "thinking: %s" response-thinking))
        (pwb-render-response response-text)
        (display-buffer pwb-response-buffer)
        (message "pwb: response received.")
        (message "stop reason: %s, usage: %s" response-stop-reason response-usage)
        (pwb-assistant-turn-2 response-text))
    (pwb-render-error-response response)
    (message "pwb: error; %S" response)
    (message "pwb: response received.")
    nil))

(defun pwb-make-curl-config-file (payload)
  "Make a temporary curl config file and return its filename.
PAYLOAD is
alist.  The caller is responsible to delete the temporary file after it
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

(defun pwb-curl-with-config (payload)
  "Make a curl config file based on PAYLOAD.
The external curl program called by `'call-process', return the
response."
  (let ((config (pwb-make-curl-config-file payload))
        (response))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process "curl" nil t nil "--silent"
                                      "--config"
                                      config)))
            (unless (zerop status)
              (error "Curl failed with status %d: %s" status (buffer-string))))
          (goto-char (point-min))
          (setq response (json-parse-buffer :object-type 'alist)))
      (when (file-exists-p config)
        (delete-file config)))
    response))

(defun pwb-async-curl-with-config (payload callback)
  "Invoke curl with PAYLOAD asynchronously with large file.
CALLBACK is called with the parsed response alist when the
process finishes.  On failure, an error is signaled."
  (let* ((config (pwb-make-curl-config-file payload))
         (buf (generate-new-buffer " *pwb-curl*"))
         (proc (make-process
                :name "pwb-curl"
                :buffer buf
                :command (list "curl" "--silent"
                               "--config" config)
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
                    (progn
                      (when (file-exists-p config)
                        (delete-file config))
                      (when (buffer-live-p buf)
                        (kill-buffer buf))))))))
    proc))

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
               ""))
      (insert "\n\n\C-l\n\n"))))

(defun pwb-get-content-text (response)
  "Return content text in the RESPONSE."
  (pwb-find-type-from-content "text" (alist-get 'content response)))

(defun pwb-find-type-from-content (type content)
  "Return the text that belongs to TYPE.
The CONTENT argument must be STRING."
  (alist-get (intern type)
             (seq-find (lambda (x) (equal (alist-get 'type x) type)) content)))

(defun pwb-get-content-thinking (response)
  "Get the content of thinking from RESPONSE."
  (alist-get 'thinking (pwb-find-content-block-by-type
                        "thinking"
                        (pwb-get-content response))))

;;;
;;; The accessor functions for the response parameters
;;;
(defun pwb-get-stop-reason (response)
  "Return a stop reason in the RESPONSE."
  (alist-get 'stop_reason response))

(defun pwb-get-usage (response)
  "Return a stop reason in the RESPONSE."
  (alist-get 'usage response))

(defun pwb-get-content (response)
  "Return an array of ContentBlock from RESPONSE."
  (alist-get 'content response))

;;;
;;; The accessor functions for the CONTENTBLOCK
;;;
(defun pwb-content-block-type (contentblock)
  "Return the type of the CONTENTBLOCK.
Type are such as \"text\", \"thinking\" etc."
  (alist-get 'type contentblock))

(defun pwb-content-block-type-predicate (type)
  "Return the fucntion to check whether the contentblock is TYPE."
  (lambda (contentblock)
    (equal (pwb-content-block-type contentblock) type)))

(defun pwb-find-content-block-by-type (type contentblocks)
  "Return the content-block whose type is TYPE from an array of CONTENTBLOCKS."
  (seq-find (pwb-content-block-type-predicate type) contentblocks))



;;;
;;; Render response
;;;
(defun pwb-render-response (string)
  "Create a buffer for displaying the response.
Then insert STRING and newline in this buffer."
  (pwb-with-response-buffer
    (insert string)
    (insert "\n\n\C-l\n\n")))

(defun pwb-render-error-response (response)
  "Render error RESPONSE in the response buffer.
RESPONSE is an alist parsed from the API's JSON error body."
  (let ((json (json-serialize response)))
    (pwb-with-response-buffer
      (let ((marker (make-marker)))
        (set-marker marker (point))
        (insert json)
        (json-pretty-print marker (point))
        (insert "\n\n\C-l\n\n")))))

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

(defun pwb-text-block-param-sh (text)
  "TextBlockParam with TEXT.
The shorthand of text block param."
  text)

(defun pwb-text-block-param (text)
  "TextBlockParam {TEXT, type, cache_control, citations}."
  `((type . "text")
    (text . ,text)))

(defun pwb-image-block-param (data)
  "ImageBlockParam with DATA {source, type, cache_control}."
  `((type . "image")
    (source (type . "base64")
            (media_type . "image/png")
            (data . ,data))))

(defun pwb-make-message-param-content (&rest content-block-params)
  "Return the content of MessageParam.
The content is array of ContentBlockParam(CONTENT-BLOCK-PARAMS)."
  `(content . ,(if (stringp (car content-block-params))
                   (car content-block-params) ; For the shorthand TextBlockParam
                 (vconcat content-block-params))))

(defun pwb-make-message-param (role message-param-content)
  "MessageParam Constructor taking ROLE and MESSAGE-PARAM-CONTENT."
  `((role . ,role)
    ,message-param-content))

;;; The payload top level These are called Body Parameters.
(defun pwb-make-body-param-max-tokens (int)
  "Constructor for max_tokens body parameter by INT."
  `(max_tokens . ,int))

(defun pwb-make-body-param-messages (message-param)
  "Constructor for messages body parameter(MESSAGE-PARAM)."
  `(messages . ,message-param))

(defun pwb-make-body-param-model (model)
  "Constructor for model body parameter by MODEL."
  `(model . ,model))

(defun pwb-make-body-param-system (string)
  "Constructor for system body parameter by STRING."
  `(system . ,string))

;;; The constructor payload
(defun pwb-make-payload (optional-body-params &rest body-params)
  "Construct payload from OPTIONAL-BODY-PARAMS and BODY-PARAMS."
  (append body-params optional-body-params))

(defun pwb-concat-turns-2 (history current)
  "Concatenate HISTORY of turn, a.k.a Messages and CURRENT MessageParam.
This function can be used to add conversation."
  (vconcat history (vector current)))

(defun pwb-payload-with-prompt (messages prompt max-tokens model system optional-body-params)
  "Taking arguments below, Return payload alist.
MESSAGES: Message Body Param
PROMPT: string
MAX-TOKENS: integer
MODEL: string
SYSTEM: string
OPTIONAL-BODY-PARAMS: alist."
  (pwb-make-payload
   optional-body-params
   (pwb-make-body-param-messages
    (pwb-concat-turns-2
     messages
     (pwb-make-message-param "user"
                             (pwb-make-message-param-content
                              (pwb-text-block-param prompt)))))
   (pwb-make-body-param-max-tokens max-tokens)
   (pwb-make-body-param-model model)
   (pwb-make-body-param-system system)))

(defun pwb-payload-with-prompt-and-image (messages prompt data max-tokens model system optional-body-params)
  "Taking arguments below, Return payload alist.
MESSAGES: Message Body Param
PROMPT: string
DATA: base64 image data
MAX-TOKENS: integer
MODEL: string
SYSTEM: string
OPTIONAL-BODY-PARAMS: alist."
  (pwb-make-payload
   optional-body-params
   (pwb-make-body-param-messages
    (pwb-concat-turns-2
     messages
     (pwb-make-message-param "user"
                             (pwb-make-message-param-content
                              (pwb-image-block-param data)
                              (pwb-text-block-param prompt)))))
   (pwb-make-body-param-max-tokens max-tokens)
   (pwb-make-body-param-model model)
   (pwb-make-body-param-system system)))

(defun pwb-payload-with-prompt-and-system (messages prompt mid-system max-tokens model system optional-body-params)
  "Taking arguments below, Return payload alist.
MESSAGES: Message Body Param
PROMPT: string
MID-SYSTEM: string mid conversation system message
MAX-TOKENS: integer
MODEL: string
SYSTEM: string
OPTIONAL-BODY-PARAMS: alist."
  (pwb-make-payload
   optional-body-params
   (pwb-make-body-param-messages
    (pwb-concat-turns-2
     (pwb-concat-turns-2
      messages
      (pwb-make-message-param "user"
                              (pwb-make-message-param-content
                               (pwb-text-block-param prompt))))
     (pwb-make-message-param "system"
                             (pwb-make-message-param-content
                              (pwb-text-block-param mid-system)))))
   (pwb-make-body-param-max-tokens max-tokens)
   (pwb-make-body-param-model model)
   (pwb-make-body-param-system system)))

;; Body Param are messages, model, max_tokens, system, etc.
;;
;; Messages is an array of MessageParam
;;   MessageParam is {array of ContentBlockParam, role}

;; {"role": "user", "content": "Hello, Claude"} <= MessageParam
;;
;; messages: [
;;  {"role": "user", "content": "Hello, Claude"} <= MessageParam
;;  {"role": "assistant", "content": "May I help you?"} <= MessageParam
;; ] <= Message body param
;;
;;
;;         ContentBlockParam is one of following:
;;         TextBlockParam
;;         ImageBlockParam

;; ImageBlockParam
;; "messages": [
;;    { "role": "user", "content": [
;;       { "type": "image", "source": {
;;              "type": "base64",
;;              "media_type": "'$IMAGE_MEDIA_TYPE'",
;;              "data": "'$IMAGE_BASE64'"
;;       }}, <= ImageBlockParam(ContentBlockParam)
;;       { "type": "text", "text": "What is in the above image?"} <= TextBlockParam(ContentBlockParam)
;;    ]}
;;  ]

;;                                                                                                                                          { "type": "text", "text": "What is in the above image?"}]}]
;; { "content": "Hello, Claude"} <= MessageParam
;; max_tokens is integer.
;; model is string

;; turns is a vector of ContentBlockParam
;; (pwb-messages-turns pwb-messages) => [((role . "user")(content . "foo bar"))
;;                                       ((role . "assistant") (content . "May I help you?"))]

(provide 'pwb)
;;; pwb.el ends here
