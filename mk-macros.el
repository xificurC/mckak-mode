(defmacro mk--with-selection-option (keep &rest body)
  "Run BODY manipulating selection based on KEEP.
:select sets mark as point before running BODY.
:extend keeps mark untouched or sets it at point before running BODY if isn't active.
:deactivate makes sure there's no selection after running BODY."
  (declare (indent 1))
  (let ((pnt (gensym "POINT")))
    (ecase keep
      (:select
       `(let ((,pnt (save-excursion (progn ,body (point)))))
          (set-mark (point))
          (goto-char ,pnt)))
      (:extend
       `(let ((,pnt (save-excursion (progn ,body (point)))))
          (unless mark-active
            (set-mark (point)))
          (goto-char ,pnt)))
      (:deactivate
       `(progn
          (deactivate-mark)
          body)))))

(provide 'mk-macros)
