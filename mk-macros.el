(defmacro mk--extend-selection (&rest body)
  "Extends the current selection."
  (declare (indent 0))
  `(progn
     (unless mark-active
       (set-mark (point)))
     ,@body))

(defmacro mk--create-selection (&rest body)
  "Creates a selection."
  (declare (indent 0))
  `(progn
     (set-mark (point))
     ,@body))

(provide 'mk-macros)
