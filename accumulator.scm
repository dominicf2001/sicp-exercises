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

(define (product term a next b)
  (accumulate-i * 1 term a next b))

(product identity 1 inc 10)
