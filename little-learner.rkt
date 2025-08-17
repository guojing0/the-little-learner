#lang racket
(require malt)

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

(((l2-loss line)
  #(2 1 4 3)
  #(1.8 1.2 4.2 3.3))
 (list 0 0)) ; theta_0 and theta_1, slope and weight
