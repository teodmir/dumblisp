
(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

(+ (* (factorial 5) 3) 17)
