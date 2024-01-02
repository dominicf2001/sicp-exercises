(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (square x)
  (* x x))

(define (even? x)
  (= (modulo x 2) 0))

(define (expt-r x e)
  (if (= e 0)
      1
      (* x (expt-r x (- e 1)))))

(define (expt-i x e)
  (define (expt-iter product counter)
    (if (= counter e)
        product
        (expt-iter (* product x) (+ counter 1))))
  
  (expt-iter 1 0))

(define (expt-fast-r x e)
  (cond ((= e 0) 1)
        ((even? e) (square (expt-fast x (/ e 2))))
        (else (* x (expt-fast x (- e 1))))))

(define (expt-fast-i x e)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  
  (expt-iter 1 x e))
