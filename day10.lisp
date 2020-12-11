(defconstant +testinput+ "16
10
15
5
1
11
7
19
6
12
4")

(defun read-lines (in)
  (loop for line = (read-line in nil)
        until (null line)
        collect (read-from-string line)))

(defun differences (ns)
  (let* ((a (cons 0 (sort (copy-seq ns) #'<)))
         (b (append (rest a) (mapcar (lambda (x) (+ 3 x)) (last a)))))
    (loop for m in a
          for n in b
          collect (- n m))))

(defun arrangements (ns)
  (let ((sv (cons 0 (sort (copy-seq ns) #'<))))
    (loop with origins = (make-array (mapcar #'1+ (last sv)) :initial-element 1)
          for n in (rest sv)
          do (setf (aref origins n)
                   (loop for x in sv
                         when (< (- n 4) x n)
                           sum (aref origins x)))
          finally (return (aref origins (1- (array-dimension origins 0)))))))


(defun main ()
  (with-open-file (is "input/day10.txt")
    (let ((d (differences (read-lines is))))
      (format t "part 1: ~a~%" (* (count-if (lambda (x) (= x 1)) d)
                                  (count-if (lambda (x) (= x 3)) d)))))
  (with-open-file (is "input/day10.txt")
    (format t "part 2: ~a~%" (arrangements (read-lines is)))))
