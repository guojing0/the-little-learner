#lang racket
(require malt)

(define line-xs #(2 1 4 3))
(define line-ys #(1.8 1.2 4.2 3.3))

;;; Chapter 1

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

((line 10) '(20 30))

;;; Chapter 2

(define rank-draft
  (lambda (t)
    (cond ((scalar? t) 0)
          (else (add1 (rank (tref t 0)))))))

(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond ((scalar? t) a)
          (else (ranked (tref t 0) (add1 a))))))

(rank #(#(#(8) #(9)) #(#(4) #(7))))

(define shape
  (lambda (t)
    (cond ((scalar? t) (list))
          (else (cons (tlen t) (shape (tref t 0)))))))

(shape #(#(#(8) #(9)) #(#(4) #(7))))

;;; Interlude 1

(define sum-1
  (lambda (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (lambda (t i a)
    (cond ((zero? i) (+ (tref t 0) a))
          (else (summed t (sub1 i) (+ (tref t i) a))))))

(sum-1 #(10 12 14))

;;; Chapter 3

(define l2-loss
  (lambda (target)
   (lambda (xs ys)
    (lambda (theta)
      (let ((pred-ys ((target xs) theta)))
        (sum (sqr (- ys pred-ys))))))))

;;; Chapter 4

(define obj ((l2-loss line) line-xs line-ys))

(obj (list 3 0)) ; theta_0 and theta_1, slope and weight

(gradient-of obj (list 0 0))

(define revise
  (lambda (f revs theta)
    (cond ((zero? revs) theta)
          (else (revise f (sub1 revs) (f theta))))))

(let ((alpha 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (lambda (theta)
             (let ((gs (gradient-of obj theta)))
               (list (- (ref theta 0) (* alpha (ref gs 0)))
                     (- (ref theta 1) (* alpha (ref gs 1))))))))
    (revise f 1000 (list 0 0))))
