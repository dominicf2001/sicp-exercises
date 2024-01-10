(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity a)
  a)

(define (inc a)
  (+ 1 a))

(define (factorial a)
  (product identity 1 inc a))

(define (approximate-pi a)
 (define (pi-term n) 
   (if (even? n) 
       (/ (+ n 2) (+ n 1)) 
       (/ (+ n 1) (+ n 2)))) 
  
 (* 4 (product pi-term 1 inc a)))

(product identity 1 inc 10)
(product-i identity 1 inc 10)
