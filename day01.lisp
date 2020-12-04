(load "aoc.lisp")

(defun read-report (in)
  (do ((line (read-line in nil) (read-line in nil))
       (r nil))
      ((null line) (reverse r))
    (setq r (cons (read-from-string line) r))))

(defun combinations (ns)
  "Return all two-element combinations of ns."
  (if (null ns)
      nil
      (append (mapcar (lambda (x) (cons (first ns) x)) (rest ns))
              (combinations (rest ns)))))

(defun sums-and-products (ns)
  "Return pairs of the sums and products of all two-element combinations of ns."
  (mapcar (lambda (x) (cons (+ (car x) (cdr x)) (* (car x) (cdr x))))
          (combinations ns)))

(defun product-where-sum-is (ns sum)
  (cdr (find-if (lambda (x) (= (car x) sum))
                (sums-and-products ns))))

(defparameter *test-in* "1721
979
366
299
675
1456")

(defparameter *test-r* (with-input-from-string (is *test-in*)
                         (read-report is)))

(test (= (product-where-sum-is *test-r* 2020) 514579))

(defun main ()
  (with-open-file (is "input/day01.txt")
    (product-where-sum-is (read-report is) 2020)))
