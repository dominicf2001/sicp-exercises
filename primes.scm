(define (even? n)
  (divides? 2 n))

(define (odd? n)
  (not (even? n)))

(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0)) 
  
  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (define (next a)
      (if (= a 2)
          3
          (+ a 2)))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (= n (smallest-divisor n)))

(define (miller-rabin-test n)
  (define (expmod base exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (check-non-trivial-sqrt (square (expmod base (/ exp 2))))
                      n))
          (else
           (remainder (* base (expmod base (- exp 1)))
                      n))))

  (define (check-non-trivial-sqrt square)
    (if (and (= square (remainder 1 n))
             (not (= (sqrt square) 1))
             (not (= (sqrt square) (- n 1))))
        0
        square))

  (define (try-it a)
    (= (expmod a (- n 1)) 1))

  (try-it (+ 1 (random (- n 1)))))

(miller-rabin-test 12)

(define (fermat-test n)
  (define (expmod base exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2)))
                      n))
          (else
           (remainder (* base (expmod base (- exp 1)))
                      n))))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 50)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes itr end)
  (start-prime-search itr end (runtime)))

(define (start-prime-search itr end start-time)
  (cond ((<= itr end)
         (if (odd? itr)
             (timed-prime-test itr))
         (start-prime-search (+ itr 1) end start-time))))


(define (expt x e)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  
  (expt-iter 1 x e))

(define (congruent a b m)
  (= (remainder a m) (remainder b m)))

(define (likely-prime? n)
  (define (likely-prime-iter n a)
    (if (>= a 0)
        (if (congruent (expt a n) a n)
            (likely-prime-iter n (- a 1))
            false)
        true))
  (likely-prime-iter n (- n 1)))

(likely-prime? 1729)

(expt 2 2)

(search-for-primes 1000000000 1000000100) ; 10^9
(search-for-primes 10000000000 10000000100) ; 10^10
(search-for-primes 100000000000 100000000100) ; 10^11
(search-for-primes 1000000000000 1000000000100) ; 10^12

(search-for-primes 1000000000 1000000100) ; 10^9
(search-for-primes 1000000000000000000 1000000000000000100) ; 10^18
