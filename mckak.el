;; -*- lexical-binding: t; -*-

(require 'multiple-cursors)
(require 'cl)

(defun mk/select-to-next-word-start (&optional count)
  "Selects up to next word start."
  (interactive "p")
  (setq count (or count 1))
  (dotimes (_ count)
    (set-mark (point))
    (skip-syntax-forward "w")
    (skip-syntax-forward "^w")))

(defun mk/select-to-next-word-end (&optional count)
  "Selects up to next word end."
  (interactive "p")
  (setq count (or count 1))
  (message "Count is %d" count)
  (dotimes (_ count)
    (set-mark (point))
    (skip-syntax-forward "^w")
    (skip-syntax-forward "w")))

(defun mk/forward-char (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (deactivate-mark)
  (setq count (or count 1))
  (dotimes (_ count)
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (ignore-errors (forward-char count)))))

(defun mk/backward-char (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (deactivate-mark)
  (setq count (or count 1))
  (dotimes (_ count)
    (save-restriction
      (narrow-to-region (point) (line-beginning-position))
      (ignore-errors (backward-char count)))))

(defvar mckak-mode-map (make-sparse-keymap) "McKak mode's keymap")
(suppress-keymap mckak-mode-map)
(define-key mckak-mode-map "w" 'mk/select-to-next-word-start)
(define-key mckak-mode-map "e" 'mk/select-to-next-word-end)
(define-key mckak-mode-map "j" 'next-line)
(define-key mckak-mode-map "k" 'previous-line)
(define-key mckak-mode-map "h" 'mk/backward-char)
(define-key mckak-mode-map "l" 'mk/forward-char)
(dotimes (i 10)
  (define-key mckak-mode-map (number-to-string i) 'digit-argument))
(global-set-key [escape] 'mckak-mode)

;;;###autoload
(define-minor-mode mckak-mode
  "McKak mode - vim meets multiple cursors."
  :global t
  :keymap mckak-mode-map)

(provide 'mckak)
