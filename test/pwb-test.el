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
          (pwb-system-prompt "")
          (pwb-model "claude-haiku-4-5")
          (pwb-max-tokens 256))
      (funcall body))))

(defvar pwb-buffer-org-file "Hello?")

(ert-deftest pwb-check-response ()
  "Test to whether the API response."
  (skip-unless nil)
  (pwb-with-custom
   (should (eq (with-temp-buffer
                 (insert pwb-buffer-org-file)
                 (pwb-current-buffer))
               t))))

(ert-deftest pwb-build-alist-from-custom-test ()
  "Test to build alist from the customized variables."
  (pwb-with-custom
   (should (equal (pwb-build-alist-from-custom)
                  '((max_tokens . 256)(model . "claude-haiku-4-5")(system . ""))))))

(ert-deftest pwb-add-conversation-test ()
  "Test to add conversation. pwb-user-turn, pwb-assistant-turn,
pwb-concat-turns are also tested."
  (should (equal (pwb-add-conversation []
                                       (pwb-user-turn "First.")
                                       nil
                                       (pwb-assistant-turn "First response."))
                 [((role . "user")(content . "First."))
                  ((role . "assistant")(content . "First response."))]))
  (should (equal (pwb-add-conversation []
                                       (pwb-user-turn "First.")
                                       (pwb-system-turn "System message")
                                       (pwb-assistant-turn "First response."))
                 [((role . "user")(content . "First."))
                  ((role . "system")(content . "System message"))
                  ((role . "assistant")(content . "First response."))])))

(ert-deftest pwb-merge-params-test ()
  "Test pwb-merge-params function.  Test also the precedence of the params"
  (let ((msgs-param '((messages . [(role . "user")(content . " First.")])))
        (body-params '((thinking . adaptive)))
        (custom-params '((max_tokens . 256)(model . "claude-haiku-4-5")(system . ""))))
    (should (equal (pwb-merge-params msgs-param body-params custom-params)
                   '((messages . [(role . "user")(content . " First.")])
                     (thinking . adaptive)
                     (max_tokens . 256)(model . "claude-haiku-4-5")(system . ""))))
    (should (equal (pwb-merge-params msgs-param (append body-params '((max_tokens . 128))) custom-params)
                   '((messages . [(role . "user")(content . " First.")])
                     (thinking . adaptive)
                     (max_tokens . 128)(model . "claude-haiku-4-5")(system . ""))))
    (should (equal (pwb-merge-params msgs-param (append body-params '((model . "claude-sonnet-4-6"))) custom-params)
                   '((messages . [(role . "user")(content . " First.")])
                     (thinking . adaptive)(model . "claude-sonnet-4-6")
                     (max_tokens . 256)(system . ""))))
    (should (equal (pwb-merge-params msgs-param (append body-params '((system . "prompt")) ) custom-params)
                   '((messages . [(role . "user")(content . " First.")])
                     (thinking . adaptive)(system . "prompt")
                     (max_tokens . 256)(model . "claude-haiku-4-5"))))))

(defvar pwb-buffer-with-local-variables "Hello?

# Local Variables:
# pwb-max-tokens: 128
# pwb-body-params: '((max_tokens . 2048))
# End:
")
(ert-deftest pwb-local-variables-test ()
  (let ((enable-local-variables :all))
    (should (eq (with-temp-buffer
                  (insert pwb-buffer-with-local-variables)
                  (hack-local-variables)
                  (local-variable-p 'pwb-max-tokens))
                t))))

(ert-deftest pwb-build-alist-test-basic ()
  "Test basic request alist."
  (let ((alist '((model . "claude-sonnet-4-5")
		 (max_tokens . 1024)
		 (system . "")))
        (json "{\"messages\":[{\"role\":\"user\",\"content\":\"Hello, Claude\"}],\"model\":\"claude-sonnet-4-5\",\"max_tokens\":1024,\
\"system\":\"\"}"))
    (should
     (equal json
	    (json-serialize
             (pwb-merge-params (list (cons 'messages (pwb-concat-turns (vector)
                                                                       (pwb-user-turn "Hello, Claude"))))
                               alist))))))

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

(ert-deftest pwb-vector-messages-test ()
  "Proper message vector can be built? Test `'pwd-add-conversation'."
  (let ((messages (make-pwb-messages)))
    (should (equal (make-pwb-messages :turns
				      (vconcat (vector (list (cons 'role "user") (cons 'content "Hi")))
					       (vector (list (cons 'role "assistant") (cons 'content "May I help you?")))))
		   (make-pwb-messages :turns (pwb-add-conversation (pwb-messages-turns messages)
                                                                   (pwb-user-turn "Hi") nil (pwb-assistant-turn "May I help you?")))))))

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
          (should (equal "{\n  \"type\": \"error\",\n  \"error\": {\n    \"type\": \"invalid_request_error\",\n    \"message\": \"Input does not match the expected shape.\"\n  },\n  \"request_id\": \"req_011CWsDcj4HTJuWosWP8djPz\"\n}\n\f\n"
                         error-json)))))))

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
			  (cons 'usage (list (cons 'input_tokens 9) (cons 'cache_creation_input_tokens 0) (cons 'cache_read_input_tokens 0) (cons 'cache_creation (list (cons 'ephemeral_5m_input_tokens 0) (cons 'ephemeral_1h_input_tokens 0))) (cons 'output_tokens 12) (cons 'service_tier "standard"))))))))

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

(ert-deftest pwb-user-turn-with-image-test ()
  (should
   (equal [((role . "user")
            (content
             . [((type . "image")
                 (source (type . "base64") (media_type . "image/png")
                         (data . "IMAGE_BASE64")))
                ((type . "text") (text . "What is in the above image?"))]))]
          (pwb-user-turn-with-image "What is in the above image?" "IMAGE_BASE64"))))

(defun pwb-make-curl-config-file-test-fn (body)
  (pwb-with-custom
   (let (filename)
     (unwind-protect
         (let* ((prompt "Hello?")
                (pwb-messages (make-pwb-messages))
                (pwb-body-params nil)
                (turns (pwb-messages-turns pwb-messages))
                (msgs
                 (pwb-messages-param
                  (pwb-concat-turns turns
                                    (pwb-user-turn prompt))))
                (alst (pwb-merge-params msgs
                                        pwb-body-params
                                        (pwb-build-alist-from-custom))))
           (setq filename (pwb-make-curl-config-file alst))
           (find-file-literally filename)

           ;; Delete the line containing "x-api-key"
           (goto-char (point-min))
           (when (search-forward "x-api-key" nil t)
             (beginning-of-line)
             (kill-whole-line))
           (funcall body (buffer-substring-no-properties (point-min) (point-max))))
       (delete-file filename)))))

(ert-deftest pwb-make-curl-config-file-test ()
  (pwb-make-curl-config-file-test-fn
   (lambda (x)
     (should (equal x "url https://api.anthropic.com/v1/messages
-H \"anthropic-version: 2023-06-01\"
-H \"content-type: application/json\"
-d \"{\\\"messages\\\":[{\\\"role\\\":\\\"user\\\",\\\"content\\\":\\\"Hello?\\\"}],\\\"max_tokens\\\":256,\\\"model\\\":\\\"claude-haiku-4-5\\\",\\\"system\\\":\\\"\\\"}\"")))))

(ert-deftest pwb-make-body-param-max-tokens-test ()
  (should (equal '(max_tokens . 1024)
                 (pwb-make-body-param-max-tokens 1024))))

(ert-deftest pwb-make-body-param-model-test ()
  (should (equal '(model . "claude-haiku-4-5")
                 (pwb-make-body-param-model "claude-haiku-4-5"))))


(provide 'pwb-test)

;;; pwb-test.el ends here
