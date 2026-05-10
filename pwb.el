;;; pwb.el --- Prompting with buffer  -*- lexical-binding: t; -*-

;; Copyright (C)   2026 pxel8063

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.4
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

(cl-defstruct pwb-claude-api model max-tokens system)

;; (defvar *claude-api* (make-pwb-claude-api
;;                       :model pwb-claude-model
;;                       :max-tokens pwb-claude-max-tokens
;;                       :system ""))
(defgroup pwb nil
  "Custom variables of pwb."
  :group 'local)

(defcustom pwb-claude-model "claude-haiku-4-5"
  "String to specify claude model."
  :group 'pwb
  :type 'string)

(defcustom pwb-claude-max-tokens 1024
  "The number of max_tokens."
  :group 'pwb
  :type 'natnum)

(defcustom pwb-claude-system-prompt "When possible, use org-mode syntax."
  "The string of system prompt."
  :group 'pwb
  :type 'string)

(defcustom pwb-claude-message-api-url "https://api.anthropic.com/v1/messages"
  "Specifing the Claude message API host."
  :group 'pwb
  :type 'string)

(defcustom pwb-claude-api-host "api.anthropic.com"
  "Machine name of the api in `auth-source'."
  :group 'pwb
  :type 'string)

(defcustom pwb-claude-anthropic-version "2023-06-01"
  "Specifing the Claude anthropic version.
Like curl -H anthropic-version: 2023-06-01"
  :group 'pwb
  :type 'string)

(defconst pwb-claude-response-buffer "*Claude*"
  "The name of buffer for the response from Claude.")

(defconst pwb-claude-api-parameters-from-org-property '("MAX_TOKENS" "MODEL" "SYSTEM")
  "The list of the claude message API parameters.")

(cl-defstruct pwb-messages conversation)
(defvar pwb-messages (make-pwb-messages) "Holding conversation history.")

(defun pwb-filter-org-property (seq)
  (seq-filter
   (lambda (elt) (member (car elt) pwb-claude-api-parameters-from-org-property))
   seq))

(defun pwb-push-org-property (ret)
  (let ((prop (org-entry-properties (point))))
    (dolist (elt (pwb-filter-org-property prop) ret)
      (if (equal (car elt) "MAX_TOKENS")
          (push (cons (car elt) (string-to-number (cdr elt))) ret)
        (push elt ret)))))

(defun pwb-accu-org-property ()
  (let* ((ret)
         (accum (pwb-push-org-property ret)))
    (save-excursion
      (goto-char (point-min))
      (pwb-push-org-property accum))))

(defmacro pwb-set-alist (param alist val)
  `(setf (alist-get ,param ,alist nil nil #'equal) ,val))

(defun pwb-build-alist-from-custom ()
  (let (ret)
    (pwb-set-alist 'max_tokens ret pwb-claude-max-tokens)
    (pwb-set-alist 'model ret pwb-claude-model)
    (pwb-set-alist 'system ret pwb-claude-system-prompt)
    ret))

(defun pwb-seq-set-alist (seq ret)
  (mapcar (lambda (x)
            (pwb-set-alist (intern (downcase (car x)))
                           ret
                           (cdr x)))
          seq)
  ret)

(defun pwb-build-whole-alist ()
  (let ((ret (pwb-build-alist-from-custom))
        (a (pwb-accu-org-property)))
    (pwb-seq-set-alist a ret)))

(defun pwb-build-alist (alist messages input)
  (let ((mes (pwb-messages-conversation messages)))
    (setq mes (vconcat mes
                       (vector (list (cons 'role "user") (cons 'content input)))))
    (pwb-set-alist 'messages alist mes)
    alist))

(defun pwb-get-credential (host)
  "Get the credential from the `auth-source'."
  (auth-source-pick-first-password :host host))

(defun pwb-curl (payload)
  "Invoke curl with PAYLOAD."
  (let ((url pwb-claude-message-api-url)
        (api-key (pwb-get-credential pwb-claude-api-host))
        (anthropic-version pwb-claude-anthropic-version)
        (application-json "content-type: application/json"))
    (unless api-key
      (error "%s can not be found in `auth-source'" pwb-claude-api-host))
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

;;;###autoload
(defun pwb-current-buffer ()
  "Send a prompt based on the current buffer to api.
PREFILL from minibuffer is used."
  (interactive)
  (let* ((prompt (pwb-buffer-string))
         (alst (pwb-build-alist (pwb-build-whole-alist) pwb-messages prompt))
         (response (pwb-curl (json-serialize alst))))
    (pwb-render-response
     (if (pwb-test-response response)
         (let ((response-text (pwb-get-content-text response)))
           (setq pwb-messages (pwb-add-conversation pwb-messages prompt response-text))
           response-text)
       (format "%S" response)))))

;;;###autoload
(defun pwb-save-conversation (file)
  "Save the conversation to FILE."
  (interactive "FFile to save conversation: ")
  (with-temp-file file
    (prin1 (pwb-messages-conversation pwb-messages) (current-buffer))))

;;;###autoload
(defun pwb-restore-conversation (file)
  "Restore the conversation from FILE."
  (interactive "fFile to restore conversation: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((data (read (current-buffer))))
      (setq pwb-messages (make-pwb-messages :conversation data)))))

;;;###autoload
(defun pwb-set-system-prompt ()
  "Set system prompt string to the current buffer."
  (interactive)
  (customize-set-variable 'pwb-claude-system-prompt (pwb-buffer-string)))

;;;###autoload
(defun pwb-clear-system-prompt ()
  "Clear system prompt."
  (interactive)
  (customize-set-variable 'pwb-claude-system-prompt ""))

;;;###autoload
(defun pwb-clear-conversation ()
  "Clear the conversation history."
  (interactive)
  (setf pwb-messages (make-pwb-messages)))

(defun pwb-add-conversation (messages u-content a-content)
  "Add conversation of U-CONTENT(user content) and A-CONTENT.
Return MESSAGES as `pwb-messages'."
  (let ((history (pwb-messages-conversation messages)))
    (make-pwb-messages :conversation
                       (vconcat history
                                (vector (list (cons 'role "user") (cons 'content u-content)))
                                (vector (list (cons 'role "assistant") (cons 'content a-content)))))))

(defun pwb-get-content-text (response)
  "Return content text in the RESPONSE."
  (alist-get 'text (aref (alist-get 'content response) 0)))

(defun pwb-render-response (string)
  "Create a buffer for displaying the response.
Then insert STRING and newline in this buffer."
  (with-current-buffer (get-buffer-create pwb-claude-response-buffer)
    (save-excursion
      (goto-char (point-max))
      (newline 2)
      (insert string))))

(defun pwb-test-response (response)
  "Test whether the RESPONSE is error or not."
  (pcase (alist-get 'type response)
    ("error" nil)
    ("message" t)))

(defun pwb-buffer-to-list-of-list ()
  "Build the list of plist."
  (if (= (point) (point-max))
      nil
    (cons (json-parse-buffer :object-type 'plist)
          (pwb-buffer-to-list-of-list))))

(provide 'pwb)
;;; pwb.el ends here
