;; a vector is a pair, (cons x y)
;; interpretation. a vector in 2 dimensions. the vector (cons 2 3)
;; specifies that it's moving 2 in the horizontal dimension and 3 in
;; the vertical dimension.


;; an expression is either:
;;    - A vector; or
;;    - (list expression expression)

;; expression -> expression
;; op-vector adds and subtracts vectors; and multiplies a vector by a scalar.
;; (op-vector (+ (cons 2 3) (cons 3 4))) -> (cons 5 7)
;; (op-vector (- (cons 3 4) (cons 2 3))) -> (cons 1 1)
;; (op-vector (* 3 (cons 3 4))) -> (cons 9 12)
(define (op-vector x)
  (cond ((sum? x)
	 (print-vector (make-vector
			(+ (horizontal-dimension (addend x))
			   (horizontal-dimension (augend x)))
			(+ (vertical-dimension (addend x))
			   (vertical-dimension (augend x))))))
	((sub? x)
	 (print-vector (make-vector
			(- (horizontal-dimension (subtrahend x))
			   (horizontal-dimension (minuend x)))
			(- (vertical-dimension (subtrahend x))
			   (vertical-dimension (minuend x))))))
	((mul? x)
	 (if (scalar? (multiplier x))
	     (print-vector (make-vector
			    (* (multiplier x) (horizontal-dimension (multiplicand x)))
			    (* (multiplier x) (vertical-dimension (multiplicand x)))))
	     (error "Scalar cannot be the multiplicand -- OP-VECTOR" (multiplicand x))))
	(else (error "Multipliying vectors is invalid --OP-VECTOR" x))))
	     
	
;;-----
;; data representation

;; dimension dimension -> vector
(define (make-vector h v) (cons h v))
;; vector -> dimension
(define (horizontal-dimension v) (car v))
;; vector -> dimension
(define (vertical-dimension v) (cdr v))

(define (scalar? x) (and (not (pair? x)) (number? x)))
(define (scalar x) (cadr x))

;; representation of sums of vectors
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (make-sum e1 e2) (list '+ e1 e2))
(define (addend x) (cadr x))
(define (augend x) (caddr x))

;; representation of sub
(define (sub? x) (and (pair? x) (eq? (car x) '-)))
(define (make-sub e1 e2) (list '- e1 e2))
(define (subtrahend x) (cadr x))
(define (minuend x) (caddr x))

;; representation of products
(define (mul? x) (and (pair? x) (eq? (car x) '*)))
(define (make-product e1 e2) (list '* e1 e2))
(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))
;;-----
;; printing vectors
(define (print-vector vector)
  (newline)
  (display "v = ")
  (display "(")
  (display (horizontal-dimension vector))
  (display ",")
  (display (vertical-dimension vector))
  (display ")"))
;;-----
;; examples/testing

(define vector1 (make-vector 2 3))
(define vector2 (make-vector 3 4))

;; ex1
(define sum1 (make-sum vector1 vector2))
(op-vector sum1)
;;value: v = (5,7)

;; ex2
(define sub1 (make-sub vector2 vector1))
(op-vector sub1)
;;value: v = (1,1)

;; ex3
(define (product1 (make-product 3 vector2)))
(op-vector product1)
;;value: v = (9,12)
