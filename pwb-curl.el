;;; pwb-curl.el --- Prompting with buffer -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun pwb-curl (payload)
  "Invoke curl with PAYLOAD."
  (interactive)
  (let ((host "https://api.anthropic.com/v1/messages")
	(api-key (concat "x-api-key: " (getenv "ANTHROPIC_API_KEY")))
	(anthropic-version "anthropic-version: 2023-06-01")
	(application-json "content-type: application/json"))
    (with-temp-buffer
      (call-process "curl" nil t nil host "-s"
		    "-H" api-key
		    "-H" anthropic-version
		    "-H" application-json
		    "-d" payload)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))

(defun pwb-buffer-string ()
  "Parse the current buffer, if narrowed, the narrowed part, "
  (interactive)
  (buffer-substring-no-properties (point-min) (point-max)))

(defun pwb-launch ()
  "Send a prompt based on the current buffer to api."
  (interactive)
  (let* ((prompt (pwb-buffer-string))
	 (response (pwb-curl (pwb-build-json prompt))))
    (pwb-render-response
     (if (pwb-test-response response)
	 (pwb-get-content-text response)
       (format "%S" response)))))

(defun pwb-build-json (input)
  "Make a json string based on INPUT."
  (json-serialize (list :model "claude-haiku-4-5"
			:max_tokens 1000
			:system ""
			:messages (vector (list :role "user" :content input)))))

(cl-defstruct messages conversation)
(defvar *messages* (make-messages))
(defun pwb-make-messages ()
  (setf (messages-conversation *messages*) (vector (list :role "user" :content "Hello"))))

(defun pwb-get-content-text (response)
  (plist-get (aref (plist-get response :content) 0) :text))

(defun pwb-render-response (string)
  "Create `*Anthropic*' buffer and insert STRING and newline in this buffer."
  (get-buffer-create "*Anthropic*")
  (set-buffer "*Anthropic*")
  (set-mark)
  (insert string)
  (newline 2))

(defun pwb-test-response (response)
  (pcase (plist-get response :type)
    ("error" nil)
    ("message" t)))

(defun pwb-buffer-to-list-of-list ()
  (if (= (point) (point-max))
      nil
    (cons (json-parse-buffer :object-type 'plist)
	  (pwb-buffer-to-list-of-list))))

(provide 'pwb-curl)
