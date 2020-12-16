
(defparameter *testinput*  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defun to-binary (n)
  (format nil "~36,'0B" n))

(defun from-binary (str)
  (parse-integer str :radix 2))

(defun masked-value (mask value)
  (concatenate
   'string
   (loop for nb across (to-binary value)
         for mb across mask
         collect (if (char= mb #\X) nb mb))))

(defun set-masked-value (mem mask addr value)
  (setf (gethash addr mem) (from-binary (masked-value mask value))))

(defun split (str &key (chr #\Space))
  "Split STR into tokens by character CHR"
  (loop with tok-start = 0
        for i from 0 below (length str)
        when (char= (aref str i) chr)
          collect (subseq str tok-start i)
          and do (setq tok-start (1+ i))
        when (= (1+ i) (length str))
          collect (subseq str tok-start (1+ i))))

(defun parse-address (str)
  (let ((start (position-if #'digit-char-p str)))
    (parse-integer (subseq str start (position-if-not #'digit-char-p str :start start)))))

(defun readinput (s maskfn)
  (loop with mem = (make-hash-table)
        with mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        for line = (read-line s nil)
        until (null line)
        do (let ((tok (split line)))
             (if (string-equal (first tok) "mask")
                 (setq mask (nth 2 tok))
                 (funcall maskfn mem mask
                          (parse-address line)
                          (parse-integer (nth 2 tok)))))
        finally (return mem)))

(defun read-and-sum (fn)
    (with-open-file (s "input/day14.txt")
      (loop for v being the hash-values of (readinput s fn)
            sum v)))

(defun part1 ()
  (print (read-and-sum #'set-masked-value)))
