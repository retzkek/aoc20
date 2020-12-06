(load "aoc.lisp")

;;;    byr (Birth Year) - four digits; at least 1920 and at most 2002.
;;;    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;;;    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;;;    hgt (Height) - a number followed by either cm or in:
;;;        If cm, the number must be at least 150 and at most 193.
;;;        If in, the number must be at least 59 and at most 76.
;;;    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;;;    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;;;    pid (Passport ID) - a nine-digit number, including leading zeroes.
;;;    cid (Country ID) - ignored, missing or not.
(defstruct (passport
            (:conc-name nil)
            (:print-function print-passport))
  (byr 0 :type integer)
  (iyr 0 :type integer)
  (eyr 0 :type integer)
  (hgt "na" :type string)
  (hcl "na" :type string)
  (ecl "na" :type string)
  (pid "na" :type string)
  (cid "na" :type string)
  (valid nil))

(defun print-passport (p stream depth)
  (print-unreadable-object (p stream :type t)
    (format stream "~abyr:~a iyr:~a eyr:~a hgt:~a hcl:~a ecl:~a pid:~a cid:~a"
            (if (valid p) "" "INVALID ")
            (byr p) (iyr p) (eyr p) (hgt p) (hcl p) (ecl p) (pid p) (cid p))))

(defun byr-valid (p) (<= 1920 (byr p) 2002))
(defun iyr-valid (p) (<= 2010 (iyr p) 2020))
(defun eyr-valid (p) (<= 2020 (eyr p) 2030))
(defun hgt-valid (p)
  (let ((a (position-if #'digit-char-p (hgt p)))
        (b (position-if #'alpha-char-p (hgt p))))
    (if (and (eq a 0) b)
        (let ((v (parse-integer (hgt p) :end b))
              (u (subseq (hgt p) b)))
          (cond
            ((string= u "cm") (<= 150 v 193))
            ((string= u "in") (<= 59 v 76))
            (:otherwise nil)))
        nil)))
(defun hcl-valid (p)
  (and (= (length (hcl p)) 7)
       (string-equal (hcl p) "#" :end1 1)
       (ignore-errors (parse-integer (hcl p) :start 1 :radix 16))))
(defun ecl-valid (p)
  (member (ecl p) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string-equal))
(defun pid-valid (p)
  (and
   (= (length (pid p)) 9)
   (ignore-errors (parse-integer (pid p)))))

(defun passport-valid (p)
  (and (byr-valid p) (iyr-valid p) (eyr-valid p) (hgt-valid p)
       (hcl-valid p) (ecl-valid p) (pid-valid p)))

(defun is-char-p (char)
  (lambda (x) (char= char x)))

;;; "byr:1980..." -> (:BYR 1980...)
(defun parse-fields (s &optional (prev nil))
  (let* ((p (position-if (is-char-p #\:) s))
         (q (position-if (is-char-p #\space) s))
         (k (read-from-string
             (concatenate 'string ":" (if p
                                          (subseq s 0 p)
                                          (if q
                                              (subseq s 0 q)
                                              s)))))
         (v (if p
                (subseq s (1+ p) q)
                "")))
    (append
     prev
     (list k (ecase k
               ((:byr :iyr :eyr) (parse-integer v))
               ((:hgt :hcl :ecl :pid :cid) v)))
     (when q (parse-fields (subseq s (1+ q)))))))
(test
 (and
  (equal (parse-fields "byr:1234") '(:byr 1234))
  (equal (parse-fields "byr:1234 iyr:5678") '(:byr 1234 :iyr 5678))))

(defconstant +passport-required-fields+ '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defun validate-and-make-passport (fields)
  "Makes passport struct and validates that it has all required fields"
  (apply #'make-passport
         (append fields
                 (list :valid
                       (= (length (intersection fields +passport-required-fields+))
                          (length +passport-required-fields+))))))

(defun read-passports (s)
  "read blank-line-delimited passports from stream s."
  (loop with partial = nil
        for line = (read-line s nil)
        until (null line)
        when (or
              ;; end of file
              (null (peek-char nil s nil))
              ;; blank line separates passports; consume.
              (and (char= (peek-char nil s nil) #\newline) (read-char s)))
          collect (validate-and-make-passport (append partial (parse-fields line)))
          and do (setq partial nil)
        else
          ;; passport is continued on next line
          do (setq partial (append partial (parse-fields line)))))

(defun main ()
  (with-open-file (is "input/day04.txt")
    (let ((ps (read-passports is)))
      (format t "part 1: ~a~%" (count-if (lambda (x) (valid x)) ps))
      (mapcar (lambda (x) (setf (valid x) (passport-valid x))) ps)
      (format t "part 2: ~a~%" (count-if (lambda (x) (valid x)) ps)))))

;;; keeping for posterity and comparison with loop-based version
(defun read-passports-recursive (s &optional (partial nil) (passports nil))
  (let ((line (read-line s nil)))
    (cond ((null line) passports)
          ((null (peek-char nil s nil))
           (cons (apply #'make-passport (append partial (parse-fields line)))
                 passports))
          ((char= (peek-char nil s nil) #\newline)
           (read-char s)
           (read-passport s nil
                          (cons (apply #'make-passport (append partial (parse-fields line)))
                                passports)))
          (:otherwise
           (read-passport s (append partial (parse-fields line)) passports)))))

(defvar *testdata* "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")
