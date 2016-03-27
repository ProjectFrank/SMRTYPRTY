(require sicp)

(define (n x)
  (car x))

(define (d x)
  (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; ex 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (if (positive? d)
          (cons n d)
          (cons (* n -1) (* d -1))))))

(define (print-rat x)
  (display (n x))
  (display "/")
  (display (d x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (n x) (d y))
               (* (n y) (d x)))
            (* (d x) (d y))))

(define (sub-rat x y)
  (make-rat (- (* (n x) (d y))
               (* (n y) (d x)))
            (* (d x) (d y))))

(define (mul-rat x y)
  (make-rat (* (n x) (n y))
            (* (d x) (d y))))

(define (div-rat x y)
  (make-rat (* (n x) (d y))
            (* (d x) (n y))))

(print-rat (make-rat -2 -4))
(print-rat (make-rat 2 -4))

;; ex 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2))
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((start-x (x-point start))
          (start-y (y-point start))
          (end-x (x-point end))
          (end-y (y-point end)))
      (make-point (average start-x end-x)
                  (average start-y end-y)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define midpoint
  (let ((start (make-point -1 1))
        (end (make-point 1 -3)))
    (midpoint-segment (make-segment start end))))

(print-point midpoint)

;; ex 2.3

(define make-rect make-segment)

(define (rect-dimension f)
  (lambda (rect)
    (abs (- (f (start-segment rect))
            (f (end-segment rect))))))

(define rect-width (rect-dimension x-point))

(define rect-height (rect-dimension y-point))

(define (perimeter rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (area rect)
  (* (rect-width rect)
     (rect-height rect)))

;; alternative will work as long as rect-width and rect-height is reimplemented
;; currently lacking creativity to actually make an alternative

;; ex 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define cons
  (lambda (x y)
    (lambda (m) (m x y))))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; ex 2.5

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (log-ish base n)
  (define (iter dividend result)
    (let ((q (quotient dividend base)))
      (let ((r (- dividend (* q base))))
        (if (= 0 r) 
            (iter q (inc result))
            result))))
  (iter n 0))

(define (car x)
  (log-ish 2 x))

(define (cdr x)
  (log-ish 3 x))

(cons 2 2) ;; 36
(car (cons 2 2)) ;; 2
(cdr (cons 2 2)) ;; 2

(cons 13 17) ;; 1057916215296
(car (cons 13 17)) ;; 13
(cdr (cons 13 17)) ;; 17

(cons 0 0) ;; 1
(car (cons 0 0)) ;; 0
(cdr (cons 0 0)) ;; 0

;; ex 2.6

(define zero (lambda (a) (lambda (b) b)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f)
  (lambda (x) (f ((zero f) x))))
(lambda (f)
  (lambda (x) (f (((lambda (a) (lambda (b) b)) f) x))))
(lambda (f)
  (lambda (x) (f ((lambda (b) b) x))))

;; with renamed params
(define one
  (lambda (a) (lambda (b) (a b))))

(define one
  (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (a)
                               (lambda (b) (a b))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (b) (f b)) x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

((two inc) 0) 

(define (add x y)
  (x y))
