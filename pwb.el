;;; pwb.el --- Prompting with buffer  -*- lexical-binding: t; -*-

;; Copyright (C)   2026 pxel8063

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.1
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

(defcustom pwb-claude-message-api-host "https://api.anthropic.com/v1/messages"
  "Specifing the Claude message API host."
  :group 'pwb
  :type 'string)

(defcustom pwb-claude-anthropic-version "2023-06-01"
  "Specifing the Claude anthropic version.
Like curl -H anthropic-version: 2023-06-01"
  :group 'pwb
  :type 'string)

(defconst pwb-claude-response-buffer "*Claude*"
  "The name of buffer for the response from Claude.")

(cl-defstruct pwb-messages conversation)
(defvar pwb-messages (make-pwb-messages) "Holding conversation history.")

(defun pwb-curl (payload)
  "Invoke curl with PAYLOAD."
  (let ((host pwb-claude-message-api-host)
        (api-key (getenv "ANTHROPIC_API_KEY"))
        (anthropic-version pwb-claude-anthropic-version)
        (application-json "content-type: application/json"))
    (unless api-key
      (error "ANTHROPIC_API_KEY environment variable not set"))
    (with-temp-buffer
      (let ((status (call-process "curl" nil t nil host "-s"
                                  "-H" (concat "x-api-key: " api-key)
                                  "-H" (concat "anthropic-version: " anthropic-version)
                                  "-H" application-json
                                  "-d" payload)))
        (unless (zerop status)
          (error "Curl failed with status %d: %s" status (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))

(defun pwb-buffer-string ()
  "Parse the current buffer, if narrowed, the narrowed part."
  (buffer-substring-no-properties (point-min) (point-max)))

;;;###autoload
(defun pwb-current-buffer (prefill)
  "Send a prompt based on the current buffer to api.
PREFILL from minibuffer is used."
  (interactive "sPrefill: ")
  (let* ((prompt (pwb-buffer-string))
         (api (make-pwb-claude-api
               :model pwb-claude-model
               :max-tokens pwb-claude-max-tokens
               :system pwb-claude-system-prompt))
         (plst (pwb-build-plist api pwb-messages prompt prefill))
         (response (pwb-curl (json-serialize plst))))
    (pwb-render-response
     (if (pwb-test-response response)
         (let ((response-text (pwb-get-content-text response)))
           (setq pwb-messages (pwb-add-conversation pwb-messages prompt response-text))
           response-text)
       (format "%S" response)))))

(defun pwb-build-plist (api messages input prefill)
  "Return the API plist with INPUT and PREFILL.
The MESSAGES so far are prepended."
  (list :model (pwb-claude-api-model api)
        :max_tokens  (pwb-claude-api-max-tokens api)
        :system  (pwb-claude-api-system api)
        :messages (vconcat (pwb-messages-conversation messages)
                           (vector (list :role "user" :content input))
                           (if (string-equal prefill "")
                               []
                             (vector (list :role "assistant" :content prefill))))))

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

(defun pwb-message-vector-clear ()
  "Clear the conversation history."
  (setf pwb-messages (make-pwb-messages)))

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
                                (vector (list :role "user" :content u-content))
                                (vector (list :role "assistant" :content a-content))))))

(defun pwb-get-content-text (response)
  "Return content text in the RESPONSE."
  (plist-get (aref (plist-get response :content) 0) :text))

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
  (pcase (plist-get response :type)
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
