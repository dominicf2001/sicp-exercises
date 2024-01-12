(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (inc x)
  (+ x 1))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (compose f f))

(define (repeated f n)
  (define (iter x n)
    (if (= n 0)
        x
        (iter (f x) (- n 1))))
  
  (lambda (x) (iter x n)))

(define (repeat f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

(define (repeat f n)
  (define (iter n result)
    (if (<= n 1)
        result
        (iter (- n 1) (compose f result))))
  
  (iter n f))

((repeat square 1) 5)
