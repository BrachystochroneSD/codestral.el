;;; codestral-api.el --- CodeStral For Emacs      -*- lexical-binding: t; -*-

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

(require 'url)
(require 'json)

(defcustom codestral-url "https://codestral.mistral.ai"
  "Url of the codestral api"
   :type 'string
   :group 'codestral)

(defcustom codestral-api-key "YOU-API-KEY"
  "API key to generate in your mistral workspace for codestral at
https://console.mistral.ai/codestral"
   :type 'string
   :group 'codestral)

(defvar codestral-api-request-cache nil
  "Mainly for debugging but maybe will be useful")

(defvar codestral-api-response-cache nil
  "Mainly for debugging but maybe will be useful")

(defvar codestral-json-string-cache nil
  "Mainly for debugging but maybe will be useful")

(defun codestral-api--post (url json-data callback)
  "Default url retrieve as POST with json data"
  (let ((headers
         `(("Authorization" . ,(format "Bearer %s" codestral-api-key))
           ("Content-Type" . "application/json"))))
    (codestral-api--request "POST" url headers json-data callback)))

(defun codestral-api--request (method url headers data callback)
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data))
    (setq codestral-api-request-cache data)
    (url-retrieve
     url
     (lambda (status init-buffer callback)
       (let ((result (codestral-api--get-json-result)))
         (setq codestral-api-response-cache result)
         (with-current-buffer init-buffer
           (funcall callback result))))
     `(,(current-buffer) ,callback) t)))

(defun codestral-api--get-json-result ()
  "Get the code string from the json response"
  (goto-char (point-min))
  (re-search-forward "^$")
  (let ((json-string (buffer-substring (point) (point-max))))
    (setq codestral-json-string-cache json-string)
    (json-parse-string json-string
                       :object-type 'plist
                       :array-type 'list)))

(provide 'codestral-api)
;;; codestral-api.el ends here
