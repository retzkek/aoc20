(defparameter *tests* nil)

(defmacro test (body)
  `(setq *tests* (cons ',body *tests*)))

(defun run-tests ()
  (dolist (tst *tests*) (format t "~a: ~a~%" tst (eval tst))))
