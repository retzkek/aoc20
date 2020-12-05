(load "aoc.lisp")

(defconstant +testinput+ "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defun read-map (in)
  (let* ((m (loop for line = (read-line in nil)
                  until (null line)
                  collect (map 'list (lambda (x) (char= x #\#)) line)))
         (rows (length m))
         (cols (length (first m))))
    (make-array (list rows cols) :initial-contents m)))

(defun trees (m xs ys)
  (loop for j below (array-dimension m 0) by ys
        for i by xs
        count (aref m j (mod i (array-dimension m 1)))))
(test
 (with-input-from-string (is +testinput+)
   (let ((m (read-map is)))
     (and
      (= (trees m 3 1) 7)
      (= (trees m 1 1) 2)
      (= (trees m 5 1) 3)
      (= (trees m 7 1) 4)
      (= (trees m 1 2) 2)))))

(defun main ()
  (with-open-file (is "input/day03.txt")
    (let ((m (read-map is)))
      (format t "part 1: ~a~%" (trees m 3 1))
      (format t "part 2: ~a~%"
              (reduce #'* (mapcar (lambda (x) (trees m (first x) (second x)))
                                  '((1 1) (3 1) (5 1) (7 1) (1 2))))))))
