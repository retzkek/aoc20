#lang racket

(require rackunit)

(define (read-report in)
  (define ip (cond [(port? in) in]
                   [(string? in) (open-input-string in)]))
  (map string->number (port->list read-line ip)))

(module+ test
  (check-equal? (read-report "123\n456") '(123 456))
  (check-equal? (read-report (open-input-string "12\n34")) '(12 34)))

(define (sum-and-product ns)
  (cons (foldl + 0 ns) (foldl * 1 ns)))

(module+ test
  (check-equal? (sum-and-product '(1 2 3 4)) '(10 . 24)))

(define (product-where-sum-is r n sum)
  (sequence->list
   (sequence-map cdr
                 (sequence-filter (λ (x) (equal? (car x) sum))
                                  (sequence-map sum-and-product
                                                (in-combinations r n))))))

(module+ test
  (define r (read-report
             "1721
979
366
299
675
1456"))
  (check-equal? (product-where-sum-is r 2 2020) '(514579))
  (check-equal? (product-where-sum-is r 3 2020) '(241861950)))

(module+ main
  (define r (read-report (open-input-file "input/day01.txt")))
  (displayln (product-where-sum-is r 2 2020))
  (displayln (product-where-sum-is r 3 2020)))

