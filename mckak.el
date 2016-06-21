;; -*- lexical-binding: t -*-

(require 'multiple-cursors)
(require 'cl)
(require 'mk-macros)
(require 'phi-search)
(require 'evil-core)

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

(evil-define-state mckak
  "McKak state."
  :tag " <K> "
  :suppress-keymap t
  (progn
    ;; TODO with the setq evil-default-state lower evil starts with mckak state
    ;; however this remove-hook only gets called after re-entering it.
    ;; This makes the first e.g. `w' call flip into visual state,
    ;; which is exactly what we are trying to avoid.
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)))

(add-to-list 'mc/cmds-to-run-once 'evil-mckak-state)

(setq evil-default-state 'mckak)
(define-key evil-insert-state-map [escape] 'evil-mckak-state)

(setq evil-mckak-state-map (copy-keymap evil-normal-state-map))
(define-key evil-mckak-state-map "w"         'mk/select-to-next-word-start)
(define-key evil-mckak-state-map "e"         'mk/select-to-next-word-end)
(define-key evil-mckak-state-map "j"         'mk/move-to-next-line)
(define-key evil-mckak-state-map "k"         'mk/move-to-previous-line)
(define-key evil-mckak-state-map "h"         'mk/move-to-previous-char)
(define-key evil-mckak-state-map "l"         'mk/move-to-next-char)
(define-key evil-mckak-state-map "b"         'mk/select-to-previous-word-start)
(define-key evil-mckak-state-map "v"         'mk/select-to-previous-word-end)
(define-key evil-mckak-state-map "f"         'mk/select-to-next-found-char)
(define-key evil-mckak-state-map "N"         'mc/mark-next-like-this)
(define-key evil-mckak-state-map (kbd "M-s") 'mc/edit-lines)
(define-key evil-mckak-state-map "s"         'mc/mark-all-in-region-regexp)
(define-key evil-mckak-state-map "&"         'mc/vertical-align-with-space)
(define-key evil-mckak-state-map "W"         'mk/extend-to-next-word-start)
(define-key evil-mckak-state-map "E"         'mk/extend-to-next-word-end)
(define-key evil-mckak-state-map "J"         'mk/extend-to-next-line)
(define-key evil-mckak-state-map "K"         'mk/extend-to-previous-line)
(define-key evil-mckak-state-map "H"         'mk/extend-to-previous-char)
(define-key evil-mckak-state-map "L"         'mk/extend-to-next-char)
(define-key evil-mckak-state-map "B"         'mk/extend-to-previous-word-start)
(define-key evil-mckak-state-map "V"         'mk/extend-to-previous-word-end)
(define-key evil-mckak-state-map "F"         'mk/extend-to-next-found-char)
(define-key evil-mckak-state-map (kbd "M-f") 'mk/select-to-previous-found-char)
(define-key evil-mckak-state-map (kbd "M-F") 'mk/extend-to-previous-found-char)
(define-key evil-mckak-state-map "/"         'phi-search)
(define-key evil-mckak-state-map "?"         'phi-search-backward)
(define-key evil-mckak-state-map "%"         'mark-whole-buffer)

(defun mk/kill-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'delete-char)))

(defun mk/change-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'delete-char))
  (evil-insert-state 1))

;; TODO keep the regions untouched
;; or expand them, still to be decided.
(defun mk/insert ()
  (interactive)
  (when (region-active-p)
    (goto-char (region-beginning)))
  (evil-insert-state 1))

;; TODO keep and expand the region.
(defun mk/append ()
  (interactive)
  (if (region-active-p)
      (goto-char (region-end))
    (goto-char (1+ (point))))
  (evil-insert-state 1))

(define-key evil-mckak-state-map "d"         'mk/kill-region)
(define-key evil-mckak-state-map "c"         'mk/change-region)
(define-key evil-mckak-state-map "i"         'mk/insert)
(define-key evil-mckak-state-map "a"         'mk/append)

(add-to-list 'mc/cmds-to-run-for-all 'mk/kill-region)
(add-to-list 'mc/cmds-to-run-for-all 'mk/change-region)
(add-to-list 'mc/cmds-to-run-for-all 'mk/insert)
(add-to-list 'mc/cmds-to-run-for-all 'mk/append)

(dotimes (i 10)
  (define-key evil-mckak-state-map (number-to-string i) 'digit-argument))
;; (global-set-key [escape] 'mckak-local-mode)

;; (define-minor-mode mckak-local-mode
;;   "McKak mode - vim meets multiple cursors."
;;   :keymap evil-mckak-state-map)

;; (defun mckak-initialize ()
;;   (unless (minibufferp)
;;     (mckak-local-mode 1)))

;; ;;;### autoload
;; (define-globalized-minor-mode mckak-mode
;;   mckak-local-mode mckak-initialize)

(provide 'mckak)
