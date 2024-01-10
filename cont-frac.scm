(define (cont-frac n d k)
  (define (recurse n d k i)
    (/ (n i) (+ (d i) (if (= k 0)
                        (d i)
                        (recurse n d (- k 1) (+ 1 i))))))
  (recurse n d k 0))

(define (cont-frac-i n d k)
  (define (iter n d k result)
    (if (= k 0)
        (/ (n k) result)
        (iter n d (- k 1) (+ (d (- 1 k))
                             (/ (n k) result)))))
  
  (iter n d k (d k)))

(cont-frac-i (lambda (i) 1.0)
             (lambda (i) 1.0)
             9)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 0)
                        x
                        (- (square x))))
             (lambda (i) (+ 1 (* 2 i)))
             k))

(tan-cf 5 10)
