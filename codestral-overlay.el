;;; codestral-overlay.el --- Codestral for Emacs       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel D

;; Author: Samuel D <samueld@mailo.com>
;; Keywords: convenience

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

;; Taken from copilot.el
;; Create and handle the overlay shown during
;; buffer completion process

;;; Code:

(defface codestral-overlay-face
  '((t :inherit shadow))
  "Face for codestral overlay.")

(defvar-local codestral--overlay nil
  "Overlay for Codestral completion.")

(defvar-local codestral--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defconst codestral-overlay-completion-map (make-sparse-keymap)
  "Keymap for Codestral completion overlay.")

(defun codestral--get-overlay ()
  "Create or get overlay for Codestral."
  (unless (overlayp codestral--overlay)
    (setq codestral--overlay (make-overlay 1 1 nil nil t))
    (overlay-put codestral--overlay
                 'keymap codestral-overlay-completion-map)
    (overlay-put codestral--overlay 'priority 100))
  codestral--overlay)

(defun codestral--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defun codestral--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))
  (let* ((tail (buffer-substring (codestral--overlay-end ov) (line-end-position)))
         (p-completion (concat (propertize completion 'face 'codestral-overlay-face)
                               tail)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq codestral--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun codestral--display-overlay-completion (completion start end)
  "Show COMPLETION between START and END."
  (setq end start)
  (codestral-clear-overlay)
  (when (and (s-present-p completion)
             (or (= start (point))      ; up-to-date completion
                 (and (< start (point)) ; special case for removing indentation
                      (s-blank-p (s-trim (buffer-substring-no-properties start (point)))))))
    (goto-char start)                   ; indentation
    (let ((ov (codestral--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (codestral--set-overlay-text ov completion))))

(defun codestral-clear-overlay ()
  "Clear Codestral overlay"
  (interactive)
  (when (codestral--overlay-visible)
    (delete-overlay codestral--overlay)
    (setq codestral--real-posn nil)))

(defsubst codestral--overlay-visible ()
  "Return whether the `codestral--overlay' is avaiable."
  (and (overlayp codestral--overlay)
       (overlay-buffer codestral--overlay)))


(provide 'codestral-overlay)
;;; codestral-overlay.el ends here
