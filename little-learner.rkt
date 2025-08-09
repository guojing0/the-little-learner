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
