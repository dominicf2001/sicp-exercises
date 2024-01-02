(define (count-change amount)
  (define (first-denomination num-of-coins)
    (cond ((= num-of-coins 1) 1)
          ((= num-of-coins 2) 5)
          ((= num-of-coins 3) 10)
          ((= num-of-coins 4) 25)
          ((= num-of-coins 5) 50)))
  
  (define (cc amount num-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= num-of-coins 0)) 0)
          (else (+ (cc amount
                       (- num-of-coins 1))
                   (cc (- amount
                          (first-denomination num-of-coins))
                       num-of-coins)))))
  
  (cc amount 5))

(count-change 100)

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (f-iter count a b c)
    (cond ((<= count 0) a)
          ((< n 3) n)
          (else (f-iter (- count 1)
                        (+ a (* 2 b) (* 3 c))
                        a
                        b))))

  (f-iter (- n 2) 2 1 0))

(f-iterative 5)
(f-recursive 5)

