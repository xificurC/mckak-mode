(require 'multiple-cursors)
(require 'cl)

(defun mk--select-to-next-word-start (count &optional keep)
  "Selects up to next word start."
  (dotimes (_ count)
    (unless (and keep mark-active)
      (set-mark (point)))
    (skip-syntax-forward "w")
    (skip-syntax-forward "^w")))

(defun mk/select-to-next-word-start (&optional count)
  "Selects up to next word start."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-next-word-start count))

(defun mk/select-to-next-word-start-keep (&optional count)
  "Selects up to next word start, adding to the selection."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-next-word-start count t))

(defun mk--select-to-next-word-end (count &optional keep)
  "Selects up to next word end."
  (dotimes (_ count)
    (unless (and keep mark-active)
      (set-mark (point)))
    (skip-syntax-forward "^w")
    (skip-syntax-forward "w")))

(defun mk/select-to-next-word-end (&optional count)
  "Selects up to next word end."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-next-word-end count))

(defun mk/select-to-next-word-end-keep (&optional count)
  "Selects up to next word end."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-next-word-end count t))

(defun mk--select-to-previous-word-start (count &optional keep)
  "Selects up to previous word start."
  (dotimes (_ count)
    (unless (and keep mark-active)
      (set-mark (point)))
    (skip-syntax-backward "^w")
    (skip-syntax-backward "w")))

(defun mk/select-to-previous-word-start (&optional count)
  "Selects up to previous word start."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-previous-word-start count))

(defun mk/select-to-previous-word-start-keep (&optional count)
  "Selects up to previous word start."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-previous-word-start count t))

(defun mk--select-to-previous-word-end (count &optional keep)
  "Selects up to previous word end."
  (dotimes (_ count)
    (unless (and keep mark-active)
      (set-mark (point)))
    (skip-syntax-backward "w")
    (skip-syntax-backward "^w")))

(defun mk/select-to-previous-word-end (&optional count)
  "Selects up to previous word end."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-previous-word-end count))

(defun mk/select-to-previous-word-end-keep (&optional count)
  "Selects up to previous word end."
  (interactive "p")
  (setq count (or count 1))
  (mk--select-to-previous-word-end count t))

(defun mk--forward-char (count &optional keep)
  "Moves forward COUNT or 1 chars but stays on current line"
  (dotimes (_ count)
    (if keep
        (unless mark-active
          (set-mark (point)))
      (deactivate-mark))
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (ignore-errors (forward-char)))))

(defun mk/forward-char (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (setq count (or count 1))
  (mk--forward-char count))

(defun mk/forward-char-keep (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (setq count (or count 1))
  (mk--forward-char count t))

(defun mk--backward-char (count &optional keep)
  "Moves backward COUNT or 1 chars but stays on current line."
  (dotimes (_ count)
    (if keep
        (unless mark-active
          (set-mark (point)))
      (deactivate-mark))
    (save-restriction
      (narrow-to-region (point) (line-beginning-position))
      (ignore-errors (backward-char)))))

(defun mk/backward-char (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (setq count (or count 1))
  (mk--backward-char count))

(defun mk/backward-char-keep (&optional count)
  "Moves forward COUNT or 1 chars but stays on current line."
  (interactive "p")
  (setq count (or count 1))
  (mk--backward-char count t))

(defun mk/next-line (&optional count)
  "Selects from cursor to next line."
  (interactive "p")
  (setq count (or count 1))
  (deactivate-mark)
  (dotimes (_ count)
    (next-line)))

(defun mk/previous-line (&optional count)
  "Selects from cursor to previous line."
  (interactive "p")
  (setq count (or count 1))
  (deactivate-mark)
  (dotimes (_ count)
    (previous-line)))

(defvar mckak-mode-map (make-sparse-keymap) "McKak mode's keymap")
(suppress-keymap mckak-mode-map)
(define-key mckak-mode-map "w" 'mk/select-to-next-word-start)
(define-key mckak-mode-map "e" 'mk/select-to-next-word-end)
(define-key mckak-mode-map "j" 'mk/next-line)
(define-key mckak-mode-map "k" 'mk/previous-line)
(define-key mckak-mode-map "h" 'mk/backward-char)
(define-key mckak-mode-map "l" 'mk/forward-char)
(define-key mckak-mode-map "b" 'mk/select-to-previous-word-start)
(define-key mckak-mode-map "v" 'mk/select-to-previous-word-end)
(define-key mckak-mode-map "N" 'mc/mark-next-like-this)
(define-key mckak-mode-map (kbd "M-s") 'mc/edit-lines)
(define-key mckak-mode-map "s" 'mc/mark-all-in-region-regexp-keep)
(define-key mckak-mode-map "W" 'mk/select-to-next-word-start-keep)
(define-key mckak-mode-map "E" 'mk/select-to-next-word-end-keep)
;; (define-key mckak-mode-map "J" 'mk/next-line-keep)
;; (define-key mckak-mode-map "K" 'mk/previous-line-keep)
(define-key mckak-mode-map "H" 'mk/backward-char-keep)
(define-key mckak-mode-map "L" 'mk/forward-char-keep)
(define-key mckak-mode-map "B" 'mk/select-to-previous-word-start-keep)
(define-key mckak-mode-map "V" 'mk/select-to-previous-word-end-keep)

(dotimes (i 10)
  (define-key mckak-mode-map (number-to-string i) 'digit-argument))
(global-set-key [escape] 'mckak-mode)

;;;###autoload
(define-minor-mode mckak-mode
  "McKak mode - vim meets multiple cursors."
  :global t
  :keymap mckak-mode-map)

(provide 'mckak)
