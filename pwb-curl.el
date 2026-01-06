(defun pwb-curl ()
  (interactive)
  (let ((host "https://api.anthropic.com/v1/messages")
	(api-key (concat "x-api-key: " (getenv "ANTHROPIC_API_KEY")))
	(anthropic-version "anthropic-version: 2023-06-01")
	(application-json "content-type: application/json")
	(payload
	 (json-serialize '(:model "claude-haiku-4-5" :max_tokens 1000 :system "" :messages [(:role "user" :content "hello")]))))
    (with-temp-buffer
      (call-process "curl" nil t nil host "-s"
		    "-H" api-key
		    "-H" anthropic-version
		    "-H" application-json
		    "-d" payload)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))
