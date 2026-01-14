;;; -*- lexical-binding: t -*-

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

(ert-deftest pwb-build-plist-test ()
  "Test api plist."
  (let ((api (make-pwb-claude-api :model "claude-haiku-4-5"
				  :max-tokens 1000
				  :system ""))
	(messages (make-pwb-messages)))
    (should (equal (list :model "claude-haiku-4-5"
			 :max_tokens 1000
			 :system ""
			 :messages [(:role "user" :content "hello")
				    (:role "assistant" :content "prefill")])
		   (pwb-build-plist api messages "hello" "prefill")))))

(ert-deftest pwb-build-plist-without-prefill-test ()
  "Test api plist."
  (let ((api (make-pwb-claude-api :model "claude-haiku-4-5"
				  :max-tokens 1000
				  :system ""))
	(messages (make-pwb-messages)))
    (should (equal (list :model "claude-haiku-4-5"
			 :max_tokens 1000
			 :system ""
			 :messages [(:role "user" :content "hello")])
		   (pwb-build-plist api messages "hello" "")))))


(ert-deftest pwb-object-get-content-text-test()
  "Test pwb-get-content-text can get a text properly."
  (should (equal (pwb-get-content-text
		  '(:model "claude-haiku-4-5-20251001" :id "msg_01F1rvRpZWutMkCnaUYFjLai" :type "message" :role "assistant" :content [(:type "text" :text "Hello! How can I help you today?")] :stop_reason "end_turn" :stop_sequence :null :usage (:input_tokens 9 :cache_creation_input_tokens 0 :cache_read_input_tokens 0 :cache_creation (:ephemeral_5m_input_tokens 0 :ephemeral_1h_input_tokens 0) :output_tokens 12 :service_tier "standard"))
		  )
		 "Hello! How can I help you today?")))

(ert-deftest pwb-vector-messages-test ()
  "Proper message vector can be built? Test pwd-add-conversation"
  (let ((messages (make-pwb-messages)))
    (should (equal (make-pwb-messages :conversation
				      (vconcat (vector (list :role "user" :content "Hi"))
					       (vector (list :role "assistant" :content "May I help you?"))))
		   (pwb-add-conversation messages "Hi" "May I help you?")))))

(ert-deftest pwb-message-vector-clear-test ()
  "Make sure that *message* hold the empty `messages'."
  (should (equal (progn (pwb-message-vector-clear)
			*messages*)
		 #s(pwb-messages nil))))

(ert-deftest pwb-success-or-error ()
  "Test response. Return nil if error."
  (should (equal nil (pwb-test-response
		      '(:type "error"
			      :error
			      (:type "invalid_request_error"
				     :message "Input does not match the expected shape.")
			      :request_id "req_011CWsDcj4HTJuWosWP8djPz"))))
  (should (equal t (pwb-test-response
		    '(:model "claude-haiku-4-5-20251001"
			     :id "msg_01F1rvRpZWutMkCnaUYFjLai"
			     :type "message"
			     :role "assistant"
			     :content [(:type "text" :text "Hello! How can I help you today?")]
			     :stop_reason "end_turn"
			     :stop_sequence :null
			     :usage (:input_tokens 9 :cache_creation_input_tokens 0 :cache_read_input_tokens 0 :cache_creation (:ephemeral_5m_input_tokens 0 :ephemeral_1h_input_tokens 0) :output_tokens 12 :service_tier "standard"))))))

(defun pwb-buffer-to-list-of-list-fixture (body)
  (let ((buffer (get-buffer-create "*test-temp*")))
    (unwind-protect
	(progn (insert "{
  \"type\": \"message\",
  \"role\": \"assistant\"
}
{
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null
}")
	       (goto-char (point-min))
               (funcall body)))
    (kill-buffer "*test-temp*")))

(ert-deftest pwb-buffer-to-list-of-list-test ()
  (pwb-buffer-to-list-of-list-fixture
   (lambda ()
     (should (equal (pwb-buffer-to-list-of-list)
		    '((:type "message" :role "assistant")
		      (:stop_reason "end_turn" :stop_sequence :null)))))))

(setq pwb-test-response-str "{
  \"model\": \"claude-haiku-4-5-20251001\",
  \"id\": \"msg_01SAFhgzYRjdc9oTKMkygSHG\",
  \"type\": \"message\",
  \"role\": \"assistant\",
  \"content\": [
    {
      \"type\": \"text\",
      \"text\": \"Hello! How can I help you today?\"
    }
  ],
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"usage\": {
    \"input_tokens\": 9,
    \"cache_creation_input_tokens\": 0,
    \"cache_read_input_tokens\": 0,
    \"cache_creation\": {
      \"ephemeral_5m_input_tokens\": 0,
      \"ephemeral_1h_input_tokens\": 0
    },
    \"output_tokens\": 12,
    \"service_tier\": \"standard\"
  }
}
{
  \"model\": \"claude-haiku-4-5-20251001\",
  \"id\": \"msg_01F97D5d6BSkLdmuNbVFxTKT\",
  \"type\": \"message\",
  \"role\": \"assistant\",
  \"content\": [
    {
      \"type\": \"text\",
      \"text\": \"Hello! Nice to see you again. How can I help you this time?\"
    }
  ],
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"usage\": {
    \"input_tokens\": 11,
    \"cache_creation_input_tokens\": 0,
    \"cache_read_input_tokens\": 0,
    \"cache_creation\": {
      \"ephemeral_5m_input_tokens\": 0,
      \"ephemeral_1h_input_tokens\": 0
    },
    \"output_tokens\": 19,
    \"service_tier\": \"standard\"
  }
}")

(setq pwb-test-response-str-1 "{
  \"model\": \"claude-haiku-4-5-20251001\",
  \"id\": \"msg_01SAFhgzYRjdc9oTKMkygSHG\",
  \"type\": \"message\",
  \"role\": \"assistant\",
  \"content\": [
    {
      \"type\": \"text\",
      \"text\": \"Hello! How can I help you today?\"
    }
  ],
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"usage\": {
    \"input_tokens\": 9,
    \"cache_creation_input_tokens\": 0,
    \"cache_read_input_tokens\": 0,
    \"cache_creation\": {
      \"ephemeral_5m_input_tokens\": 0,
      \"ephemeral_1h_input_tokens\": 0
    },
    \"output_tokens\": 12,
    \"service_tier\": \"standard\"
  }
}")
(setq pwb-test-response-str-2 "{
  \"model\": \"claude-haiku-4-5-20251001\",
  \"id\": \"msg_01F97D5d6BSkLdmuNbVFxTKT\",
  \"type\": \"message\",
  \"role\": \"assistant\",
  \"content\": [
    {
      \"type\": \"text\",
      \"text\": \"Hello! Nice to see you again. How can I help you this time?\"
    }
  ],
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"usage\": {
    \"input_tokens\": 11,
    \"cache_creation_input_tokens\": 0,
    \"cache_read_input_tokens\": 0,
    \"cache_creation\": {
      \"ephemeral_5m_input_tokens\": 0,
      \"ephemeral_1h_input_tokens\": 0
    },
    \"output_tokens\": 19,
    \"service_tier\": \"standard\"
  }
}")

(provide 'pwb-test)
;;; mylisp-test.el ends here
