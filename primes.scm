(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n)
  (divides? 2 n))

(define (odd? n)
  (not (even? n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes k)
  (start-prime-search k (runtime)))

(define (start-prime-search k start-time)
  (cond ((= k 0) (report-search (- (runtime) start-time)))
        ((odd? k)
         (timed-prime-test k)
         (start-prime-search (- k 1) start-time))))

(define (report-search elapsed-time)
  (display " *** ")
  (display elapsed-time))

(search-for-primes 100)

(timed-prime-test )
