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

(define (identity a)
  a)

(define (inc a)
  (+ a 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner a result))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) 
                    (filtered-accumulate combiner null-value term (next a) next b filter))
          (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (product term a next b)
  (accumulate-i * 1 term a next b))

(define (filtered-sum term a next b filter)
  (filtered-accumulate + 0 term a next b filter))

(define (filtered-product term a next b filter)
  (filtered-accumulate * 1 term a next b filter))


(define (sum-of-square-primes a b)
  (filtered-sum square a inc b prime?))

(define (product-of-relative-primes n)
  (define (relatively-prime? i)
    (= (gcd i n) 1))

  (filtered-product identity 0 inc n relatively-prime?))

(product-of-relative-primes 10)
