(defmacro mk/extend-selection (&rest body)
  "Extends the current selection."
  (declare (indent 0))
  (let ((pnt (gensym "POINT")))
    `(let ((,pnt (save-excursion (progn ,@body (point)))))
       (unless mark-active
         (set-mark (point)))
       (goto-char ,pnt))))

(defmacro mk/create-selection (&rest body)
  "Creates a selection starting from point."
  (declare (indent 0))
  (let ((pnt (gensym "POINT")))
    `(let ((,pnt (save-excursion (progn ,@body (point)))))
       (set-mark (point))
       (goto-char ,pnt))))

(defmacro mk/move (&rest body)
  "Ensures there's no active selection after running BODY."
  (declare (indent 0))
  (let ((pnt (gensym "POINT")))
    `(let ((,pnt (save-excursion (progn ,@body (point)))))
       (deactivate-mark)
       (goto-char ,pnt))))

(defun mkstr (&rest args)
  (with-output-to-string
    (dolist (a args)
      (princ a))))

(defun symb (&rest args)
  (intern (apply #'mkstr args)))

(defun mk--defmotion (name args doc motion body)
  (let* ((has-interactive (and (listp (car body))
                               (eq 'interactive (caar body))))
         (inter (if has-interactive (car body) '(interactive "p")))
         (args (or args '(&optional count)))
         (body (if has-interactive (cdr body) body))
         (body (case motion
                 (select `(mk/create-selection ,@body))
                 (extend `(mk/extend-selection ,@body))
                 (move `(mk/move ,@body)))))
    `(defun ,(symb 'mk/ motion '- name) ,args
       ,(ecase motion
          (select (mkstr "Selects COUNT'th " doc))
          (extend (mkstr "Extends current selection to COUNT'th " doc))
          (move (mkstr "Moves to COUNT'th " doc)))
       ,inter
       (setq count (or count 1))
       (dotimes (_ count)
         ,body))))

(defmacro mk/defmotion (name args doc motions &rest body)
  "Generates definitions for MOTIONS.

NAME is the suffix of the defun's name.
ARGS is a list of arguments used.
DOC is the variable part of the docstring.
MOTIONS is a list of 'select, 'extend or 'move.
A defun will be generated for each motion.
BODY is the variable part of defun's body. "
  (declare (indent 2))
  `(progn
     ,@(mapcar (lambda (motion)
                 (mk--defmotion name args doc motion body))
               motions)))

(provide 'mk-macros)
