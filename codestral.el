;;; codestral.el --- CodeStral For Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Samuel Dawant

;; Author: Samuel Dawant <samueld@mailo.com>
;; Keywords: codestral, completion
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is mostly an adaptation to copilot.el to codestral.mistral.ai

;;; Code:

(require 's)
(require 'codestral-fim)
(require 'codestral-overlay)

(defvar-local codestral--completion-cache nil)
(defvar-local codestral--completion-idx 0)
(defvar-local codestral--last-pos nil)

(defcustom codestral-idle-delay 0.5
  "Time in seconds to wait before starting completion. Complete immediately if set to 0."
  :type 'float
  :group 'codestral)

(defcustom codestral-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'codestral
  :type '(repeat function))

(defun codestral-completion--show-completion (completion)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((p (point))
             (start p)
             (end (+ p (length completion))))
        (codestral--display-overlay-completion
         completion start end)))))

(defun codestral-completion--get-completions (callback)
  "Retrieve context (prefix and suffix at cursor) and
invoke `codestral-fim--get-completions'
CALLBACK is launched with json result of the call"
  (let ((prefix (buffer-substring (point-min) (point)))
        (suffix (buffer-substring (point) (point-max))))
    (codestral-fim--get-completions
     prefix suffix callback)))

;;;###autoload
(defun codestral-complete ()
  "Get completion at point, showing the completion in an overlay"
  (interactive)
  (setq codestral--completion-cache nil)
  (setq codestral--last-pos (point))
  (codestral-completion--get-completions
   (lambda (json-result)
     (let ((completions (codestral-get-completions json-result)))
       (when (equal (point) codestral--last-pos)
         (let* ((completions
                 (cl-remove-if
                  (lambda (e)
                    (string= "" (string-trim e)))
                  (cl-remove-duplicates
                   completions)))
                (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
           (setq codestral--completion-cache completions)
           (if completion
               (codestral-completion--show-completion completion)
             (message "No completion available"))))))))

(defun codestral--cycle-completion (direction)
  "Cycle completion with DIRECTION."
  (let ((completions codestral--completion-cache))
    (cond ((seq-empty-p completions)
           (message "No completion is available."))
          ((= (length completions) 1)
           (message "Only one completion is available."))
          (t (let ((idx (mod (+ codestral--completion-idx direction)
                             (length completions))))
               (setq codestral--completion-idx idx)
               (let ((completion (elt completions idx)))
                 (codestral-completion--show-completion completion)))))))

;;;###autoload
(defun codestral-next-completion ()
  "Cycle to next completion."
  (interactive)
  (when (codestral--overlay-visible)
    (codestral--cycle-completion 1)))

;;;###autoload
(defun codestral-previous-completion ()
  "Cycle to previous completion."
  (interactive)
  (when (codestral--overlay-visible)
    (codestral--cycle-completion -1)))

(defun codestral-get-completions (json-result)
    (mapcar 'codestral-fim--get-completion-content
            (codestral-fim--plist-get json-result :choices)))

;;;###autoload
(defun codestral-complete-at-point ()
  "Get first completion proposed and insert it at point
Can be used without having to be in `codestral-mode'"
  (interactive)
  (codestral-completion--get-completions
   (lambda (json-result)
     (let* ((completions (codestral-get-completions json-result))
            (completion (car completions)))
       (if completion
         (insert completion)
       (message "No completion available"))))))

;;;###autoload
(defun codestral-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (codestral--overlay-visible)
    (let* ((completion (overlay-get codestral--overlay 'completion))
           (start (overlay-get codestral--overlay 'start))
           (end (codestral--overlay-end codestral--overlay))
           (uuid (overlay-get codestral--overlay 'uuid))
           (t-completion (funcall (or transform-fn #'identity) completion)))
      (codestral-clear-overlay)
      (if (eq major-mode 'vterm-mode)
          (progn
            (vterm-delete-region start end)
            (vterm-insert t-completion))
        (delete-region start end)
        (insert t-completion))
      ;; if it is a partial completion
      (when (and (s-prefix-p t-completion completion)
                 (not (s-equals-p t-completion completion)))
        (codestral--set-overlay-text (codestral--get-overlay) (s-chop-prefix t-completion completion)))
      t)))

;; minor mode

(defvar codestral--post-command-timer nil)

(defcustom codestral-disable-predicates nil
  "A list of predicate functions with no argument to disable Codestral.
Codestral will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'codestral)

(defcustom codestral-enable-predicates '(evil-insert-state-p codestral--buffer-changed)
  "A list of predicate functions with no argument to enable Codestral.
Codestral will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'codestral)

(defcustom codestral-disable-display-predicates nil
  "A list of predicate functions with no argument to disable Codestral.
Codestral will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'codestral)

(defcustom codestral-enable-display-predicates nil
  "A list of predicate functions with no argument to enable Codestral.
Codestral will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'codestral)

(defmacro codestral--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun codestral--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (codestral--satisfy-predicates codestral-enable-predicates codestral-disable-predicates))

(defun codestral--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (codestral--satisfy-predicates codestral-enable-display-predicates codestral-disable-display-predicates))

(defvar codestral-mode-map (make-sparse-keymap)
  "Keymap for Codestral minor mode.
Use this for custom bindings in `codestral-mode'.")

(defun codestral--mode-enter ()
  "Set up codestral mode when entering."
  (add-hook 'post-command-hook #'codestral--post-command nil 'local))

(defun codestral--mode-exit ()
  "Clean up codestral mode when exiting."
  (remove-hook 'post-command-hook #'codestral--post-command 'local))

(defun codestral--posn-advice (&rest args)
  "Remap posn if in codestral-mode."
  (when codestral-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and codestral--real-posn
                 (eq pos (car codestral--real-posn)))
        (cdr codestral--real-posn)))))

(defun codestral--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "codestral-" (symbol-name this-command))
                        (member this-command codestral-clear-overlay-ignore-commands)
                        (codestral--self-insert this-command)))))
    (codestral-clear-overlay)
    (when codestral--post-command-timer
      (cancel-timer codestral--post-command-timer))
    (setq codestral--post-command-timer
          (run-with-idle-timer codestral-idle-delay
                               nil
                               #'codestral--post-command-debounce
                               (current-buffer)))))

(defun codestral--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (codestral--overlay-visible)
             (codestral--satisfy-display-predicates))
    (let* ((ov codestral--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (codestral-accept-completion)
          (codestral--set-overlay-text ov (substring completion 1)))))))

(defun codestral--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             codestral-mode
             (codestral--satisfy-trigger-predicates))
    (codestral-complete)))

;;;###autoload
(define-global-minor-mode global-codestral-mode
  codestral-mode codestral-mode)

;;;###autoload
(define-minor-mode codestral-mode
  "Minor mode for Codestral."
  :init-value nil
  :lighter " Codestral"
  (codestral-clear-overlay)
  (advice-add 'posn-at-point :before-until #'codestral--posn-advice)
  (if codestral-mode
      (codestral--mode-enter)
    (codestral--mode-exit)))

(provide 'codestral)
;;; codestral.el ends here
