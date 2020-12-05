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

(defun sums-and-products-3 (ns)
  "Return pairs of the sums and products of all three-element permutations of ns."
  (loop for a in ns
        append
        (loop for b in (remove-if (lambda (x) (= x a)) ns)
              append
              (loop for c in (remove-if (lambda (x) (or (= x a) (= x b))) ns)
                    collect (cons (+ a b c) (* a b c))))))

(defun product-where-sum-is (ns k sum)
  (cdr (find-if (lambda (x) (= (car x) sum))
                (cond ((= k 2) (sums-and-products ns))
                      ((= k 3) (sums-and-products-3 ns))))))

(defparameter *test-in* "1721
979
366
299
675
1456")

(defparameter *test-r* (with-input-from-string (is *test-in*)
                         (read-report is)))

(test (= (product-where-sum-is *test-r* 2 2020) 514579))
(test (= (product-where-sum-is *test-r* 3 2020) 241861950))

(defun main ()
  (with-open-file (is "input/day01.txt")
    (let ((r (read-report is)))
      (format t "~a~%" (product-where-sum-is r 2 2020))
      (format t "~a~%" (product-where-sum-is r 3 2020)))))
