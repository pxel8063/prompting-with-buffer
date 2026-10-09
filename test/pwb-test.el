;;; pwb-test.el --- Test for pwb -*- lexical-binding: t -*-

;;; Copyright (C) 2026 pxel8063
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'pwb)

(defmacro pwb-with-custom (&rest body)
  (let ((gbod (gensym)))
    `(let ((,gbod #'(lambda () ,@body)))
       (pwb-with-custom-fn ,gbod))))

(defun pwb-with-custom-fn (body)
  (progn
    (let ((pwb-messages (make-pwb-messages))
          (pwb-system-prompt "Be honest.")
          (pwb-system-cache nil)
          (pwb-model "claude-haiku-4-5")
          (pwb-max-tokens 256)
          (pwb-body-params '((cache_control (type . "ephemeral"))))
          (pwb-api-url "https://api.anthropic.com/v1/messages")
          (pwb-api-batch-url "https://api.anthropic.com/v1/messages/batches")
          (pwb-api-file-url "https://api.anthropic.com/v1/files")
          (pwb-api-host "api.anthropic.com"))
      (funcall body))))

(defvar pwb-buffer-org-file "Hello?")

(ert-deftest pwb-check-response ()
  "Test to whether the API response."
  (skip-unless nil)
  (pwb-with-custom
   (should (eq (vectorp
                (with-temp-buffer
                  (insert pwb-buffer-org-file)
                  (pwb-current-buffer)))
               t))))

(defvar pwb-buffer-with-local-variables "Hello?

# Local Variables:
# pwb-max-tokens: 128
# pwb-body-params: '((max_tokens . 2048))
# End:
")

(ert-deftest pwb-check-batch-request ()
  "Test whether the batch request succeeds."
  (skip-unless nil)
  ;;{"custom_id":"my-custom-id-1","result":{"type":"succeeded","message":{"model":"claude-haiku-4-5-20251001","id":"msg_011CfeiEy185S3ns8CtWBEaj","type":"message","role":"assistant","content":[{"type":"text","text":"Hello! 👋 How can I help you today?"}],"container":null,"stop_reason":"end_turn","stop_sequence":null,"stop_details":null,"usage":{"input_tokens":10,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":16,"service_tier":"batch","inference_geo":"not_available"},"diagnostics":null}}}
  (pwb-with-custom
   (should (eq t
               (pwb-batch-request)))))

(ert-deftest pwb-local-variables-test ()
  (let ((enable-local-variables :all))
    (should (eq (with-temp-buffer
                  (insert pwb-buffer-with-local-variables)
                  (hack-local-variables)
                  (local-variable-p 'pwb-max-tokens))
                t))))

(ert-deftest pwb-object-get-content-text-test()
  "Test `pwb-get-content-text' can get a text properly."
  (should (equal (pwb-get-content-text
		  (list (cons 'model "claude-haiku-4-5-20251001")
                        (cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
                        (cons 'type "message")
                        (cons 'role "assistant")
                        (cons 'content [((type . "text") (text . "Hello! How can I help you today?"))])
                        (cons 'stop_reason "end_turn")
                        (cons 'stop_sequence 'null)
                        (cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard"))))
                  )
		 "Hello! How can I help you today?")))

(ert-deftest pwb-message-vector-clear-test ()
  "Make sure that `pwb-messages' holds the empty `messages'."
  (should (equal (progn (pwb-clear-conversation)
			pwb-messages)
		 #s(pwb-messages []))))

(ert-deftest pwb-render-error-response-test ()
  "Test to render an errorresponse."
  (let ((error-res (list (cons 'type "error")
			 (cons 'error
			       (list (cons 'type "invalid_request_error")
				     (cons 'message "Input does not match the expected shape.")))
			 (cons 'request_id "req_011CWsDcj4HTJuWosWP8djPz"))))
    (pwb-render-error-response error-res)
    (with-current-buffer (get-buffer-create pwb-response-buffer)
      (save-excursion
        (goto-char (point-max))
        (backward-list)
        (let ((error-json (buffer-substring-no-properties
                           (point)
                           (point-max))))
          (should (equal "{\n  \"type\": \"error\",\n  \"error\": {\n    \"type\": \"invalid_request_error\",\n    \"message\": \"Input does not match the expected shape.\"\n  },\n  \"request_id\": \"req_011CWsDcj4HTJuWosWP8djPz\"\n}\n\n\f\n\n"
                         error-json)))))))

(ert-deftest pwb-response-to-assistant-turn-test ()
  "Test whether to return correct assistant turn"
  (should (equal nil (pwb-response-to-assistant-turn
		      (list (cons 'type "error")
			    (cons 'error
			          (list (cons 'type "invalid_request_error")
				        (cons 'message "Input does not match the expected shape.")))
			    (cons 'request_id "req_011CWsDcj4HTJuWosWP8djPz")))))
  (should (equal '((role . "assistant")
                   (content . "Hello! How can I help you today?"))
                 (pwb-response-to-assistant-turn
		  (list (cons 'model "claude-haiku-4-5-20251001")
			(cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
			(cons 'type "message")
			(cons 'role "assistant")
			(cons 'content [((type . "text") (text . "Hello! How can I help you today?"))])
			(cons 'stop_reason "end_turn")
			(cons 'stop_sequence 'null)
			(cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard"))))))))

(ert-deftest pwb-success-or-error ()
  "Test response.  Return nil if error."
  (should (equal nil (pwb-response-ok-p
		      (list (cons 'type "error")
			    (cons 'error
			          (list (cons 'type "invalid_request_error")
				        (cons 'message "Input does not match the expected shape.")))
			    (cons 'request_id "req_011CWsDcj4HTJuWosWP8djPz")))))
  (should (equal t (pwb-response-ok-p
		    (list (cons 'model "claude-haiku-4-5-20251001")
			  (cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
			  (cons 'type "message")
			  (cons 'role "assistant")
			  (cons 'content [((type . "text") (text . "Hello! How can I help you today?"))])
			  (cons 'stop_reason "end_turn")
			  (cons 'stop_sequence 'null)
			  (cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard")))))))
  (should (equal t (pwb-response-ok-p
		    (list (cons 'model "claude-haiku-4-5-20251001")
			  (cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
			  (cons 'type "message")
			  (cons 'role "assistant")
			  (cons 'content [((type . "text") (text . "Hello! How can I help you today?"))])
			  (cons 'stop_reason "max_tokens")
			  (cons 'stop_sequence 'null)
			  (cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard"))))))))

(ert-deftest pwb-get-content-test ()
  (let ((response
         (list
          (cons 'content [((type . "thinking")
                           (thinking . "Let me analyze this step by step...")
                           (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
                          ((type . "text")
                           (text . "Hello! How can I help you today?"))]))))
    (should (equal
             (pwb-get-content response)
             [((type . "thinking")
               (thinking . "Let me analyze this step by step...")
               (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
              ((type . "text")
               (text . "Hello! How can I help you today?"))]))))

(ert-deftest pwb-get-content-block-find-by-type-test ()
  (let ((content-blocks
         [((type . "thinking")
           (thinking . "Let me analyze this step by step...")
           (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
          ((type . "text")
           (text . "Hello! How can I help you today?"))]))
    (should (equal
             (pwb-find-content-block-by-type "thinking" content-blocks)
             '((type . "thinking") (thinking . "Let me analyze this step by step...")
               (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))))
    (should (equal
             (pwb-find-content-block-by-type "text" content-blocks)
             '((type . "text") (text . "Hello! How can I help you today?"))))))

(ert-deftest pwb-get-content-thinking-test ()
  (let ((response (list (cons 'model "claude-haiku-4-5-20251001")
			(cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
			(cons 'type "message")
			(cons 'role "assistant")
			(cons 'content [((type . "thinking")
                                         (thinking . "Let me analyze this step by step...")
                                         (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
                                        ((type . "text") (text . "Hello! How can I help you today?"))])
			(cons 'stop_reason "end_turn")
			(cons 'stop_sequence 'null)
			(cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard"))))))
    (should (equal
             (pwb-get-content-thinking response)
             "Let me analyze this step by step..."))))

(ert-deftest pwb-find-type-from-content-test ()
  (let ((content [((type . "thinking")
                   (thinking . "Let me analyze this step by step...")
                   (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
                  ((type . "text")
                   (text . "Hello! How can I help you today?"))] ))
    (should (equal
             (pwb-find-type-from-content "text"
                                         content)
             "Hello! How can I help you today?"))
    (should (equal
             (pwb-find-type-from-content "thinking"
                                         content)
             "Let me analyze this step by step..."))))

(ert-deftest pwb-get-content-block-test ()
  (let ((content [((type . "thinking")
                   (thinking . "Let me analyze this step by step...")
                   (signature . "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."))
                  ((type . "text")
                   (text . "Hello! How can I help you today?"))] ))
    (should (equal
             (pwb-find-type-from-content "text"
                                         content)
             "Hello! How can I help you today?"))
    (should (equal
             (pwb-find-type-from-content "thinking"
                                         content)
             "Let me analyze this step by step..."))))

(defun pwb-make-curl-config-file-test-fn (body)
  (pwb-with-custom
   (let (filename)
     (unwind-protect
         (let ((alst (pwb-payload
                      (pwb-messages-turns pwb-messages)
                      (list "Hello.")
                      nil
                      pwb-max-tokens
                      pwb-model
                      (list pwb-system-prompt)
                      pwb-body-params)))
           (setq filename (pwb-make-curl-config-file alst "MYSECRET"))
           (find-file-literally filename)

           ;; Delete the line containing "x-api-key"
           (goto-char (point-min))
           (when (search-forward "x-api-key" nil t)
             (beginning-of-line)
             (kill-whole-line))
           (funcall body (buffer-substring-no-properties (point-min) (point-max))))
       (progn
         (set-buffer-modified-p nil)
         (kill-buffer (current-buffer))
         (when (file-exists-p filename)
           (delete-file filename)))))))

(ert-deftest pwb-make-curl-config-file-test ()
  (pwb-make-curl-config-file-test-fn
   (lambda (x)
     (should (equal x "url https://api.anthropic.com/v1/messages
-H \"anthropic-version: 2023-06-01\"
-H \"content-type: application/json\"
-d \"{\\\"messages\\\":[{\\\"role\\\":\\\"user\\\",\\\"content\\\":[{\\\"type\\\":\\\"text\\\",\\\"text\\\":\\\"Hello.\\\"}]}],\\\"max_tokens\\\":256,\\\"model\\\":\\\"claude-haiku-4-5\\\",\\\"system\\\":[{\\\"type\\\":\\\"text\\\",\\\"text\\\":\\\"Be honest.\\\"}],\\\"cache_control\\\":{\\\"type\\\":\\\"ephemeral\\\"}}\"")))))

(defun pwb-make-curl-config-file-upload-file-test-fn (body)
  (pwb-with-custom
   (let (filename)
     (unwind-protect
         (progn
           (setq filename (pwb-make-curl-config-file-upload-file "/tmp/image.png" "MYSECRET"))
           (find-file-literally filename)

           ;; Delete the line containing "x-api-key"
           (goto-char (point-min))
           (when (search-forward "x-api-key" nil t)
             (beginning-of-line)
             (kill-whole-line))
           (funcall body (buffer-substring-no-properties (point-min) (point-max))))
       (progn
         (set-buffer-modified-p nil)
         (kill-buffer (current-buffer))
         (when (file-exists-p filename)
           (delete-file filename)))))))

(ert-deftest pwb-make-curl-config-file-upload-file-test ()
  (pwb-make-curl-config-file-upload-file-test-fn
   (lambda (x)
     (should (equal x "url https://api.anthropic.com/v1/files
-H \"anthropic-version: 2023-06-01\"
-H \"Content-Type: multipart/form-data\"
-F \"file=@/tmp/image.png\"")))))

(defun pwb-make-curl-config-file-delete-file-test-fn (body)
  (pwb-with-custom
   (let (filename)
     (unwind-protect
         (progn
           (setq filename (pwb-make-curl-config-file-delete-file "file_011CeD2P9vhVeHdkaFXqh36v" "MYSECRET"))
           (find-file-literally filename)

           ;; Delete the line containing "x-api-key"
           (goto-char (point-min))
           (when (search-forward "x-api-key" nil t)
             (beginning-of-line)
             (kill-whole-line))
           (funcall body (buffer-substring-no-properties (point-min) (point-max))))
       (progn
         (set-buffer-modified-p nil)
         (kill-buffer (current-buffer))
         (when (file-exists-p filename)
           (delete-file filename)))))))

(ert-deftest pwb-make-curl-config-file-delete-file-test ()
  (pwb-make-curl-config-file-delete-file-test-fn
   (lambda (x)
     (should (equal x "url https://api.anthropic.com/v1/files/file_011CeD2P9vhVeHdkaFXqh36v
-H \"anthropic-version: 2023-06-01\"
-X \"DELETE\"
")))))

(ert-deftest pwb-build-payload-prompt-and-image-test ()
  (pwb-with-custom
   (should (equal (pwb-payload-with-prompt-and-image (pwb-messages-turns pwb-messages)
                                                     "Hello."
                                                     (list (cons "image/png/base64" "IMAGE_BASE64"))
                                                     pwb-max-tokens
                                                     pwb-model
                                                     pwb-system-prompt
                                                     pwb-body-params)
                  '((messages . [((role . "user") (content . [((type . "image")
                                                               (source (type . "base64")
                                                                       (media_type . "image/png")
                                                                       (data . "IMAGE_BASE64")))
                                                              ((type . "text") (text . "Hello."))]))])
                    (max_tokens . 256) (model . "claude-haiku-4-5") (system . [((type . "text") (text . "Be honest."))])
                    (cache_control (type . "ephemeral")))))))

(ert-deftest pwb-build-payload-prompt-and-system-test ()
  (pwb-with-custom
   (should (equal (pwb-payload-with-prompt-and-system (pwb-messages-turns pwb-messages)
                                                      "Hello."
                                                      "Mid conversation"
                                                      pwb-max-tokens
                                                      pwb-model
                                                      pwb-system-prompt
                                                      pwb-body-params)
                  '((messages . [((role . "user") (content . [((type . "text") (text . "Hello."))]))
                                 ((role . "system") (content . [((type . "text") (text . "Mid conversation"))]))])
                    (max_tokens . 256) (model . "claude-haiku-4-5") (system . [((type . "text") (text . "Be honest."))])
                    (cache_control (type . "ephemeral")))))))

(ert-deftest pwb-payload-with-prompt-test ()
  (pwb-with-custom
   (should (equal (pwb-payload-with-prompt (pwb-messages-turns pwb-messages)
                                           "Hello."
                                           pwb-max-tokens
                                           pwb-model
                                           pwb-system-prompt
                                           pwb-body-params)
                  '((messages . [((role . "user") (content . [((type . "text") (text . "Hello."))]))])
                    (max_tokens . 256) (model . "claude-haiku-4-5") (system . [((type . "text") (text . "Be honest."))])
                    (cache_control (type . "ephemeral")))))))

(ert-deftest pwb-build-payload-prompt-and-uploaded-files-test ()
  (pwb-with-custom
   (should (equal (pwb-payload-with-prompt-and-uploaded-files (pwb-messages-turns pwb-messages)
                                                              "Hello."
                                                              pwb-max-tokens
                                                              pwb-model
                                                              pwb-system-prompt
                                                              pwb-body-params
                                                              (list (cons "image/png" "file_011A1zQEgJqRFP2t2o7MoGr1")))
                  '((messages . [((role . "user") (content . [((type . "image")
                                                               (source (type . "file")
                                                                       (file_id . "file_011A1zQEgJqRFP2t2o7MoGr1")))
                                                              ((type . "text") (text . "Hello."))]))])
                    (max_tokens . 256) (model . "claude-haiku-4-5") (system . [((type . "text") (text . "Be honest."))])
                    (cache_control (type . "ephemeral")))))))

(ert-deftest pwb-payload-test ()
  (pwb-with-custom
   (let ((prmts (list "Hello.")))
     ;; pwb-payload-with-prompt-test
     (should (equal (pwb-payload (pwb-messages-turns pwb-messages)
                                 prmts
                                 nil
                                 pwb-max-tokens
                                 pwb-model
                                 (list pwb-system-prompt)
                                 pwb-body-params)
                    '((messages . [((role . "user")
                                    (content . [((type . "text")
                                                 (text . "Hello."))]))])
                      (max_tokens . 256)
                      (model . "claude-haiku-4-5")
                      (system . [((type . "text")
                                  (text . "Be honest."))])
                      (cache_control (type . "ephemeral"))))))

   (let ((prmts (list (cons "image/png/base64" "IMAGE_BASE64") "Hello.")))
     ;; pwb-build-payload-prompt-and-image-test
     (should (equal (pwb-payload (pwb-messages-turns pwb-messages)
                                 prmts
                                 nil
                                 pwb-max-tokens
                                 pwb-model
                                 (list pwb-system-prompt)
                                 pwb-body-params)
                    '((messages . [((role . "user")
                                    (content . [((type . "image")
                                                 (source (type . "base64")
                                                         (media_type . "image/png")
                                                         (data . "IMAGE_BASE64")))
                                                ((type . "text")
                                                 (text . "Hello."))]))])
                      (max_tokens . 256)
                      (model . "claude-haiku-4-5")
                      (system . [((type . "text")
                                  (text . "Be honest."))])
                      (cache_control (type . "ephemeral"))))))

   (let ((prmts (list (cons "image/png/base64" "IMAGE_BASE64") "Hello.")))
     ;; pwb-build-payload-prompt-and-uploaded-files-test
     (should (equal (pwb-payload (pwb-messages-turns pwb-messages)
                                 prmts
                                 nil
                                 pwb-max-tokens
                                 pwb-model
                                 (list pwb-system-prompt)
                                 pwb-body-params)
                    '((messages . [((role . "user")
                                    (content . [((type . "image")
                                                 (source (type . "base64")
                                                         (media_type . "image/png")
                                                         (data . "IMAGE_BASE64")))
                                                ((type . "text")
                                                 (text . "Hello."))]))])
                      (max_tokens . 256)
                      (model . "claude-haiku-4-5")
                      (system . [((type . "text")
                                  (text . "Be honest."))])
                      (cache_control (type . "ephemeral"))))))

   (let ((prmts (list "Hello."))
         (mids  (list "Mid conversation")))
     ;; pwb-payload-with-prompt-and-system-test
     (should (equal (pwb-payload (pwb-messages-turns pwb-messages)
                                 prmts
                                 mids
                                 pwb-max-tokens
                                 pwb-model
                                 (list pwb-system-prompt)
                                 pwb-body-params)
                    '((messages . [((role . "user")
                                    (content . [((type . "text")
                                                 (text . "Hello."))]))
                                   ((role . "system")
                                    (content . [((type . "text")
                                                 (text . "Mid conversation"))]))])
                      (max_tokens . 256)
                      (model . "claude-haiku-4-5")
                      (system . [((type . "text")
                                  (text . "Be honest."))])
                      (cache_control (type . "ephemeral"))))))))

;;; The Claude API test
(ert-deftest pwb-api-test ()
  "Test primitive functions for the api"
  (pwb-with-custom
   (should (equal
            (pwb-max-tokens 128)
            '((max_tokens . 128))))
   (should (equal
            (pwb-model "claude-sonnet-5")
            '((model . "claude-sonnet-5"))))
   (should (equal
            (pwb-cache-control "5m")
            '((cache_control (type . "ephemeral")))))
   (should (equal
            (pwb-system (pwb-array-text-block-param "The system prompt" nil))
            '((system . [((type . "text") (text . "The system prompt"))]))))
   (should (equal
            (pwb-system (pwb-array-text-block-param "" nil))
            nil))
   (should (equal
            (pwb-system (pwb-array-text-block-param "The system prompt" "5m"))
            '((system . [((type . "text") (text . "The system prompt")
                          (cache_control (type . "ephemeral")))]))))
   (should (equal
            (pwb-thinking "summerized")
            '((thinking (type . "adaptive") (display . "summerized")))))))

(ert-deftest pwb-array-content-block-param-test ()
  "Test block param constructor."
  (should (equal
           (pwb-messages (pwb-array-message-param
                          "user"
                          (pwb-array-content-block-param '("application/pdf" . "file_011A1zQEgJqRFP2t2o7MoGr1"))))
           '((messages . [((role . "user")
                           (content . [((type . "document")
                                        (source (type . "file")
                                                (file_id . "file_011A1zQEgJqRFP2t2o7MoGr1")))]))]))))
  (should (equal
           (pwb-messages (pwb-array-message-param
                          "user"
                          (pwb-array-content-block-param '("image/png" . "file_011A1zQEgJqRFP2t2o7MoGr1"))))
           '((messages . [((role . "user")
                           (content . [((type . "image")
                                        (source (type . "file")
                                                (file_id . "file_011A1zQEgJqRFP2t2o7MoGr1")))]))]))))
  (should (equal
           (pwb-messages (pwb-array-message-param
                          "user"
                          (pwb-array-content-block-param '("image/png" . "file_011A1zQEgJqRFP2t2o7MoGr1"))))
           '((messages . [((role . "user")
                           (content . [((type . "image")
                                        (source (type . "file")
                                                (file_id . "file_011A1zQEgJqRFP2t2o7MoGr1")))]))]))))
  (should (equal
           (pwb-messages (pwb-array-message-param
                          "user"
                          (pwb-array-content-block-param '("image/png/base64" . "IMAGE_DATA"))))
           '((messages . [((role . "user")
                           (content . [((type . "image")
                                        (source (type . "base64")
                                                (media_type . "image/png")
                                                (data . "IMAGE_DATA")))]))]))))
  (should-error (pwb-array-content-block-param
                 '(("text/plain" . "file_011A1zQEgJqRFP2t2o7MoGr1"))))
  (should (equal
           (pwb-messages (pwb-array-message-param
                          "user"
                          (pwb-array-content-block-param "Hello, Claude!")))
           '((messages . [((role . "user")
                           (content . [((type . "text")
                                        (text . "Hello, Claude!"))]))])))))

(ert-deftest pwb-get-assistant-param-test ()
  (should (equal (pwb-get-assistant-param
		  (list (cons 'model "claude-haiku-4-5-20251001")
			(cons 'id "msg_01F1rvRpZWutMkCnaUYFjLai")
			(cons 'type "message")
			(cons 'role "assistant")
			(cons 'content [((type . "text") (text . "Hello! How can I help you today?"))])
			(cons 'stop_reason "end_turn")
			(cons 'stop_sequence 'null)
			(cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard")))))
                 '((role . "assistant")
                   (content . [((type . "text") (text . "Hello! How can I help you today?"))])))))

(ert-deftest pwb-with-array-test ()
  (let ((input '("You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style."
                 ("<the entire contents of Pride and Prejudice>" . "5m"))))
    (should (equal [((type . "text")
                     (text . "You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style."))
                    ((type . "text")
                     (text . "<the entire contents of Pride and Prejudice>")
                     (cache_control (type . "ephemeral")))]
                   (pwb-with-array (x input)
                     (pwb-array-content-block-param x))))))

(ert-deftest pwb-batch-prompt ()
  (skip-unless t)
  (should (equal
           '((requests
              . [((custom_id . "my-first-request")
                  (params
                   (messages . [((role . "user")
                                 (content . [((type . "text")
                                              (text . "Analyze the major themes in Pride and Prejudice."))]))])
                   (max_tokens . 1024)
                   (model . "claude-opus-5-5")
                   (system . [((type . "text")
                               (text . "You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style."))
                              ((type . "text")
                               (text . "<the entire contents of Pride and Prejudice>")
                               (cache_control (type . "ephemeral")))])))
                 ((custom_id . "my-second-request")
                  (params
                   (messages . [((role . "user")
                                 (content . [((type . "text")
                                              (text . "Write a summary of Pride and Prejudice."))]))])
                   (max_tokens . 1024)
                   (model . "claude-opus-5-5")
                   (system . [((type . "text")
                               (text . "You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style."))
                              ((type . "text")
                               (text . "<the entire contents of Pride and Prejudice>")
                               (cache_control (type . "ephemeral")))])))]))
           (pwb-batch-payload-with-prompt-and-uploaded-files nil nil nil nil nil nil nil))))

(provide 'pwb-test)

;;; pwb-test.el ends here
