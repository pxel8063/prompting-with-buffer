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
(require 'pwb-curl)
(require 'ert)

(ert-deftest addition-test ()
  (should (= 1 1)))

(ert-deftest pwb-json-string-build-test ()
  (should (equal (json-serialize (list :model "claude-haiku-4-5"
				       :max_tokens 1000
				       :system ""
				       :messages [(:role "user" :content "hello")]))
		 (pwb-build-json "hello"))))

(ert-deftest pwb-object-get-content-text-test()
  (should (equal (pwb-get-content-text
		  '(:model "claude-haiku-4-5-20251001" :id "msg_01F1rvRpZWutMkCnaUYFjLai" :type "message" :role "assistant" :content [(:type "text" :text "Hello! How can I help you today?")] :stop_reason "end_turn" :stop_sequence :null :usage (:input_tokens 9 :cache_creation_input_tokens 0 :cache_read_input_tokens 0 :cache_creation (:ephemeral_5m_input_tokens 0 :ephemeral_1h_input_tokens 0) :output_tokens 12 :service_tier "standard"))
		  )
		 "Hello! How can I help you today?")))
;
(provide 'pwb-test)
;;; mylisp-test.el ends here
