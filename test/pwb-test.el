;;; pwb-test.el --- Test for pwb -*- lexical-binding: t -*-

;;; Copyright (C) 2024 pxel8063

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
(require 'pwb)
(require 'ert)

(defvar pwb-buffer-org-file "Hello?")

(ert-deftest pwb-check-response ()
  "Test to whether the API response."
  (let* ((pwb-messages (make-pwb-messages))
         (pwb-model "claude-haiku-4-5")
         (pwb-max-tokens 256)
         (res (with-temp-buffer
                (insert pwb-buffer-org-file)
                (pwb-current-buffer))))
    (should (eq res t))))

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

(ert-deftest pwb-build-param-test ()
  "Test"
  (let* (;; Override customize variables
         (pwb-system-prompt "")
         (pwb-max-tokens 16)
         (pwb-model "claude-opus-4-6")
         ;; Override the variable
         (pwb-body-params nil)
         ;; Prepare a local environment
         (prompt "Hi.")
         (messages (make-pwb-messages))
         (turns (pwb-messages-turns messages))
         (msgs
          (pwb-messages-param (pwb-concat-turns turns
                                                (pwb-user-turn prompt)))))
    (should (equal turns
                   (vector)))
    (should (equal msgs
                   (cons 'messages (vector (list (cons 'role "user") (cons 'content "Hi."))))))
    (should (equal (pwb-build-alist (pwb-merge-params
                                     pwb-body-params
                                     (pwb-build-alist-from-custom))
                                    msgs)
                   (list (cons 'messages (vector (list (cons 'role "user") (cons 'content "Hi."))))
                         (cons 'max_tokens 16)
                         (cons 'model "claude-opus-4-6")
                         (cons 'system ""))))))

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
             (pwb-build-alist alist
                              (cons 'messages (pwb-concat-turns (vector)
                                                                (pwb-user-turn "Hello, Claude")))))))))

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
		   (make-pwb-messages :turns (pwb-add-conversation (pwb-messages-turns messages) "Hi" "May I help you?"))))))

(ert-deftest pwb-message-vector-clear-test ()
  "Make sure that `pwb-messages' holds the empty `messages'."
  (should (equal (progn (pwb-clear-conversation)
			pwb-messages)
		 #s(pwb-messages []))))

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

(provide 'pwb-test)

;;; pwb-test.el ends here
