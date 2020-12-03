#lang racket

(require rackunit)

(define (parseline line)
  (let ([m (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)" line)])
    (list (string->number (second m))
          (string->number (third m))
          (first (string->list (fourth m)))
          (fifth m))))
(module+ test
  (check-equal? (parseline "1-3 a: abcde") '(1 3 #\a "abcde")))

(define (isvalid1 b e c p)
  (<= b (count (λ (x) (eq? x c)) (string->list p)) e))
(module+ test
  (check-true (apply isvalid1 (parseline "1-3 a: abcde")))
  (check-false (apply isvalid1 (parseline "1-3 b: cdefg")))
  (check-true (apply isvalid1 (parseline "2-9 c: ccccccccc"))))

(define (isvalid2 b e c p)
  (= 1 (length (set-intersect
                (list (- b 1) (- e 1))
                (indexes-of (string->list p) c)))))
(module+ test
  (check-true (apply isvalid2 (parseline "1-3 a: abcde")))
  (check-false (apply isvalid2 (parseline "1-3 b: cdefg")))
  (check-false (apply isvalid2 (parseline "2-9 c: ccccccccc"))))

(define (count-valid-passwords in fn)
  (let ([ip (cond [(port? in) in]
                  [(string? in) (open-input-string in)])])
    (count (λ (x) (apply fn (parseline x)))
           (port->list read-line ip))))
(module+ test
  (define inp "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")
  (check-eq? (count-valid-passwords inp isvalid1) 2)
  (check-eq? (count-valid-passwords inp isvalid2) 1))

(module+ main
  (displayln (count-valid-passwords
              (open-input-file "input/day02.txt")
              isvalid1))
  (displayln (count-valid-passwords
              (open-input-file "input/day02.txt")
              isvalid2)))
