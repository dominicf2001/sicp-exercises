(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (good-enough? guess prev-guess)
  (> (abs (- guess prev-guess)) (* guess .0001))) 

(define (sqrt-iter prev-guess guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter guess (improve-guess guess x) x)))

(define (sqrt x)
  (sqrt-iter 1 1 x))

(sqrt 9)

(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1))
         (pascal (- r 1) c))))

(pascal 3 2)
