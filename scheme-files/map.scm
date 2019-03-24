(define (null? x) (equal? '() x))

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (inc x) (+ x 1))

(map inc '(5 3 2))
