;;; codestral-fim.el --- CodeStral For Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel D

;; Author: Samuel D <samueld@mailo.com>
;; Keywords: codestral, completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is handling the api of codestral

;;; Code:

;; COMPLETION

(require 'json)
(require 'codestral-api)

(defcustom codestral-fim-model "codestral-2405"
  "ID of the model to use. Only compatible for now with:
  - codestral-2405
  - codestral-latest"
   :type 'string
   :group 'codestral)

(defcustom codestral-fim-temperature 0.7
  "What sampling temperature to use, between 0.0 and 1.0. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both."
   :type 'float
   :group 'codestral)

(defcustom codestral-fim-top-p 1
  "Nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered. We generally recommend altering this or temperature but not both."
   :type 'float
   :group 'codestral)

(defcustom codestral-fim-max-tokens 40
  "Nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered. We generally recommend altering this or temperature but not both."
   :type 'float
   :group 'codestral)

(defvar codestral-fim-endpoint "/v1/fim/completions")

(defun codestral-fim--get-completions (prefix suffix callback)
  "Invoke CodeStral completions API.

This function will complete code between PREFIX and SUFFIX, which are usually
the content before cursor and after cursor, and put the result to the current
buffer. CALLBACK is launched with the list of proposed completions (string)"
  (let ((url (concat codestral-url codestral-fim-endpoint))
        (data (codestral-fim--generate-json-data prefix suffix)))
    (codestral-api--post url data callback)))

(defun codestral-fim--get-completion-content (choice)
  (codestral-fim--plist-get choice :message :content))

(defun codestral-fim--generate-json-data (prefix suffix)
  "Create Json-encoded data to send to codestral API"
  (json-encode
   `(:prompt ,prefix
             :model ,codestral-fim-model
             :temperature ,codestral-fim-temperature
             :top_p ,codestral-fim-top-p
             :max_tokens ,codestral-fim-max-tokens
             :suffix ,suffix
             )))

(defun codestral-fim--plist-get (plist &rest keys)
  "Browse PLIST object for each KEYS and return the element
where it stop.
KEYS can be either numbers or properties symbols"
  (let ((res plist))
    (dolist (key keys)
      (cond
       ((symbolp key)
        (setq res (plist-get res key)))
       ((numberp key)
        (setq res (nth key res)))
       (t
        (error "Key '%s' format not supported" key))))
    res))


(provide 'codestral-fim)
;;; codestral-fim.el ends here
