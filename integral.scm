(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity a)
  a)

(define (inc a)
  (+ 1 a))

(define (even? a)
  (= (remainder a 2)))

(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons f a b n)
  (define h
    (/ (- b a) n))
  
  (define (y k)
    (* (f (+ a
             (* h k)))
          (cond ((or (= k 0) (= k n)) 1)
                ((even? k) 2)
                (else 4))))
  
  (* (sum y 0 inc n) (/ h 3)))
