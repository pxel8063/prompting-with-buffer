;;; pwb-curl.el --- Prompting with buffer -*- lexical-binding: t; -*-

(defun pwb-curl ()
  (interactive)
  (let ((host "https://api.anthropic.com/v1/messages")
	(api-key (concat "x-api-key: " (getenv "ANTHROPIC_API_KEY")))
	(anthropic-version "anthropic-version: 2023-06-01")
	(application-json "content-type: application/json")
	(payload
	 (json-serialize (list :model "claude-haiku-4-5"
			       :max_tokens 1000
			       :system ""
			       :messages [(:role "user" :content "hello")]))))
    (with-temp-buffer
      (call-process "curl" nil t nil host "-s"
		    "-H" api-key
		    "-H" anthropic-version
		    "-H" application-json
		    "-d" payload)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))

(defun pwb-buffer-string ()
  (interactive)
  (buffer-substring-no-properties (point-min) (point-max)))

(defun pwb-launch ()
  (interactive)
  (let ((prompt (pwb-buffer-string)))
    (message "%s" (pwb-curl prompt))))

(defun pwb-build-json (input)
  (json-serialize (list :model "claude-haiku-4-5"
			:max_tokens 1000
			:system ""
			:messages (vector (list :role "user" :content input)))))

(provide 'pwb-curl)
