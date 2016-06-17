;; -*- lexical-binding: t -*-

(require 'multiple-cursors)
(require 'cl)
(require 'mk-macros)
(require 'phi-search)

(mk/defmotion to-next-word-start ()
  "word start"
  (select extend)
  (skip-syntax-forward "w")
  (skip-syntax-forward "^w"))

(mk/defmotion to-next-word-end ()
  "word end"
  (select extend)
  (skip-syntax-forward "^w")
  (skip-syntax-forward "w"))

(mk/defmotion to-previous-word-start ()
  "previous word start"
  (select extend)
  (skip-syntax-backward "^w")
  (skip-syntax-backward "w"))

(mk/defmotion to-previous-word-end ()
  "previous word end"
  (select extend)
  (skip-syntax-backward "w")
  (skip-syntax-backward "^w"))

(mk/defmotion to-next-char ()
  "character"
  (move extend)
  (save-restriction
    (narrow-to-region (point) (line-end-position))
    (ignore-errors (forward-char))))

(mk/defmotion to-previous-char ()
  "previous character"
  (move extend)
  (save-restriction
    (narrow-to-region (point) (line-beginning-position))
    (ignore-errors (backward-char))))

(mk/defmotion to-next-line ()
  "line"
  (move extend)
  (next-line))

(mk/defmotion to-previous-line ()
  "previous line"
  (move extend)
  (previous-line))

(defun mk--generate-select-to-next-found-char (char count)
  (lambda ()
    (interactive)
    (dotimes (_ count)
      (mk/create-selection
        (let ((start (point)) case-fold-search)
          (condition-case nil
              (progn
                (mk/move-to-next-char)
                (search-forward (char-to-string char) (line-end-position)))
            (error (goto-char start))))))))

(defun mk/select-to-next-found-char (char &optional count)
  "Selects COUNT'th found character"
  (interactive "cChar:\np")
  (setq count (or count 1))
  (let ((cmd (mk--generate-select-to-next-found-char char count)))
    (call-interactively cmd)
    (setq this-command cmd
          this-original-command cmd
          mc--this-command cmd)))
(add-to-list 'mc/cmds-to-run-for-all 'mk/select-to-next-found-char)

(add-to-list 'mc/cmds-to-run-for-all 'mk/extend-to-next-found-char)

(defvar mckak-local-mode-map (make-sparse-keymap) "McKak mode's keymap")
(suppress-keymap mckak-local-mode-map)
(define-key mckak-local-mode-map "w"         'mk/select-to-next-word-start)
(define-key mckak-local-mode-map "e"         'mk/select-to-next-word-end)
(define-key mckak-local-mode-map "j"         'mk/move-to-next-line)
(define-key mckak-local-mode-map "k"         'mk/move-to-previous-line)
(define-key mckak-local-mode-map "h"         'mk/move-to-previous-char)
(define-key mckak-local-mode-map "l"         'mk/move-to-next-char)
(define-key mckak-local-mode-map "b"         'mk/select-to-previous-word-start)
(define-key mckak-local-mode-map "v"         'mk/select-to-previous-word-end)
(define-key mckak-local-mode-map "f"         'mk/select-to-next-found-char)
(define-key mckak-local-mode-map "N"         'mc/mark-next-like-this)
(define-key mckak-local-mode-map (kbd "M-s") 'mc/edit-lines)
(define-key mckak-local-mode-map "s"         'mc/mark-all-in-region-regexp)
(define-key mckak-local-mode-map "&"         'mc/vertical-align-with-space)
(define-key mckak-local-mode-map "W"         'mk/extend-to-next-word-start)
(define-key mckak-local-mode-map "E"         'mk/extend-to-next-word-end)
(define-key mckak-local-mode-map "J"         'mk/extend-to-next-line)
(define-key mckak-local-mode-map "K"         'mk/extend-to-previous-line)
(define-key mckak-local-mode-map "H"         'mk/extend-to-previous-char)
(define-key mckak-local-mode-map "L"         'mk/extend-to-next-char)
(define-key mckak-local-mode-map "B"         'mk/extend-to-previous-word-start)
(define-key mckak-local-mode-map "V"         'mk/extend-to-previous-word-end)
(define-key mckak-local-mode-map "F"         'mk/extend-to-next-found-char)
(define-key mckak-local-mode-map (kbd "M-f") 'mk/select-to-previous-found-char)
(define-key mckak-local-mode-map (kbd "M-F") 'mk/extend-to-previous-found-char)
(define-key mckak-local-mode-map "/"         'phi-search)
(define-key mckak-local-mode-map "?"         'phi-search-backward)
(define-key mckak-local-mode-map "%"         'mark-whole-buffer)


(dotimes (i 10)
  (define-key mckak-local-mode-map (number-to-string i) 'digit-argument))
(global-set-key [escape] 'mckak-local-mode)

(define-minor-mode mckak-local-mode
  "McKak mode - vim meets multiple cursors."
  :keymap mckak-local-mode-map)

(defun mckak-initialize ()
  (unless (minibufferp)
    (mckak-local-mode 1)))

;;;### autoload
(define-globalized-minor-mode mckak-mode
  mckak-local-mode mckak-initialize)

(provide 'mckak)
