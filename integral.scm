(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity a)
  a)

(define (inc a)
  (+ 1 a))

(define (sum-integers a b)
  (sum identity a inc b))
