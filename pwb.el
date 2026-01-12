;;; pwb-curl.el --- Prompting with buffer -*- lexical-binding: t; indent-tabs-mode: nil; -*-

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

(defcustom pwb-claude-system-prompt ""
  "The string of system prompt."
  :group 'pwb
  :type 'string)

(cl-defstruct messages conversation)
(defvar *messages* (make-messages) "Holding conversation history.")

(defun pwb-curl (payload)
  "Invoke curl with PAYLOAD."
  (let ((host "https://api.anthropic.com/v1/messages")
        (api-key (getenv "ANTHROPIC_API_KEY"))
        (anthropic-version "anthropic-version: 2023-06-01")
        (application-json "content-type: application/json"))
    (unless api-key
      (error "ANTHROPIC_API_KEY environment variable not set"))
    (with-temp-buffer
      (let ((status (call-process "curl" nil t nil host "-s"
                                  "-H" (concat "x-api-key: " api-key)
                                  "-H" anthropic-version
                                  "-H" application-json
                                  "-d" payload)))
        (unless (zerop status)
          (error "curl failed with status %d: %s" status (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))

(defun pwb-buffer-string ()
  "Parse the current buffer, if narrowed, the narrowed part, "
  (interactive)
  (buffer-substring-no-properties (point-min) (point-max)))

(defun pwb (prefill)
  "Send a prompt based on the current buffer to api."
  (interactive "sPrefill: ")
  (let* ((prompt (pwb-buffer-string))
         (api (make-pwb-claude-api
               :model pwb-claude-model
               :max-tokens pwb-claude-max-tokens
               :system pwb-claude-system-prompt))
         (plst (pwb-build-plist api *messages* prompt prefill))
         (response (pwb-curl (json-serialize plst))))
    (pwb-render-response
     (if (pwb-test-response response)
         (let ((response-text (pwb-get-content-text response)))
           (setq *messages* (pwb-add-conversation *messages* prompt response-text))
           response-text)
       (format "%S" response)))))

(defun pwb-build-plist (api messages input prefill)
  "Return the plist of api and input."
  (list :model (pwb-claude-api-model api)
        :max_tokens  (pwb-claude-api-max-tokens api)
        :system  (pwb-claude-api-system api)
        :messages (vconcat (messages-conversation messages)
                           (vector (list :role "user" :content input))
                           (if (string-equal prefill "")
                               []
                             (vector (list :role "assistant" :content prefill))))))

(defun pwb-set-system-prompt ()
  "Set system prompt string to the current buffer."
  (interactive)
  (customize-set-variable 'pwb-claude-system-prompt (pwb-buffer-string)))

(defun pwb-clear-system-prompt ()
  "Clear system prompt."
  (interactive)
  (customize-set-variable 'pwb-claude-system-prompt ""))

(defun pwb-message-vector-clear ()
  "Clear the conversation history."
  (setf *messages* (make-messages)))

(defun pwb-clear-messages ()
  "Clear the conversation history."
  (interactive)
  (setf *messages* (make-messages)))

(defun pwb-add-conversation (messages u-content a-content)
  "Add conversation history."
  (let ((history (messages-conversation messages)))
    (make-messages :conversation
                   (vconcat history
                            (vector (list :role "user" :content u-content))
                            (vector (list :role "assistant" :content a-content))))))

(defun pwb-get-content-text (response)
  (plist-get (aref (plist-get response :content) 0) :text))

(defun pwb-render-response (string)
  "Create `*Anthropic*' buffer and insert STRING and newline in this buffer."
  (get-buffer-create "*Anthropic*")
  (set-buffer "*Anthropic*")
  (set-mark (point))
  (insert string)
  (newline 2))

(defun pwb-test-response (response)
  "Test whether the response is error or not."
  (pcase (plist-get response :type)
    ("error" nil)
    ("message" t)))

(defun pwb-buffer-to-list-of-list ()
  (if (= (point) (point-max))
      nil
    (cons (json-parse-buffer :object-type 'plist)
          (pwb-buffer-to-list-of-list))))

(provide 'pwb)
