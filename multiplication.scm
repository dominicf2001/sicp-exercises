(define (even? x)
  (= (modulo x 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mult-r a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

(define (mult-i a b)
  (define (mult-iter c x y)
    (cond ((= y 0) c)
          ((even? y) (mult-iter c (double x) (halve y)))
          (else (mult-iter (+ c x) x (- y 1)))))

    (mult-iter 0 a b))

(mult-i 55 100)
