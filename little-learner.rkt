#lang racket
(require malt)

;;; Chapter 1

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

; ((line 10) '(20 30))

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

; (rank #(#(#(8) #(9)) #(#(4) #(7))))

(define shape
  (lambda (t)
    (cond ((scalar? t) (list))
          (else (cons (tlen t) (shape (tref t 0)))))))

; (shape #(#(#(8) #(9)) #(#(4) #(7))))

;;; Interlude 1

(define sum-1
  (lambda (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (lambda (t i a)
    (cond ((zero? i) (+ (tref t 0) a))
          (else (summed t (sub1 i) (+ (tref t i) a))))))

; (sum-1 #(10 12 14))

;;; Chapter 3

(define l2-loss
  (lambda (target)
   (lambda (xs ys)
    (lambda (theta)
      (let ((pred-ys ((target xs) theta)))
        (sum (sqr (- ys pred-ys))))))))

;;; Chapter 4

(define line-xs #(2 1 4 3))
(define line-ys #(1.8 1.2 4.2 3.3))

(define obj ((l2-loss line) line-xs line-ys))

; (obj (list 3 0)) ; theta_0 and theta_1, slope and weight

; (gradient-of obj (list 0 0))

(define revise
  (lambda (f revs theta)
    (cond ((zero? revs) theta)
          (else (revise f (sub1 revs) (f theta))))))

;;; Chapter 5

(declare-hyper revs)
(declare-hyper alpha)

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
               (map (lambda (p g) (- p (* alpha g)))
                    big-theta
                    (gradient-of obj big-theta)))))
      (revise f revs theta))))

(with-hypers
    ((revs 1000)
     (alpha 0.01))
  (gradient-descent obj (list 0 0)))
; '(1.0499993623489503 1.8747718457656533e-6)

(define quad-xs #(-1 0 1 2 3))
(define quad-ys #(2.55 2.1 4.35 10.2 18.25))

(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t)
            (ref theta 2))))))

(with-hypers
    ((revs 1000)
     (alpha 0.001))
  (gradient-descent ((l2-loss quad) quad-xs quad-ys)
                    (list 0 0 0)))
; '(1.4787394427094362 0.9928606519360353 2.0546423148479684)

(define plane-xs
  (tensor (tensor 1 2.05)
          (tensor 1 3)
          (tensor 2 2)
          (tensor 2 3.91)
          (tensor 3 6.13)
          (tensor 4 8.09)))

(define plane-ys
  (tensor 13.99
          15.99
          18
          22.4
          30.2
          37.94))

(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t)
         (ref theta 1)))))

(with-hypers
    ((revs 10000)
     (alpha 0.001))
  (gradient-descent ((l2-loss plane) plane-xs plane-ys)
                    (list (tensor 0 0) 0)))
; '((tensor 3.9796455320046946 1.976454889887173) 6.1695789842820945)

(map (lambda (x)
       ((plane x) (list (tensor 3.98 1.98) 6.17)))
     (list (tensor 1 2.05)
           (tensor 1 3)
           (tensor 2 2)
           (tensor 2 3.91)
           (tensor 3 6.13)
           (tensor 4 8.09)))
; '(14.209 16.09 18.09 21.8718 30.2474 38.108200000000004)

;;; Chapter 6

(declare-hyper batch-size)

(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
      (lambda (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b)
                      (trefs ys b))
                      theta))))))

(with-hypers
    ((revs 500)
     (alpha 0.01)
     (batch-size 3))
  (gradient-descent (sampling-obj (l2-loss line) line-xs line-ys)
                    (list 0 0)))

(with-hypers
    ((revs 2000)
     (alpha 0.001)
     (batch-size 5))
  (gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                    (list (tensor 0 0) 0)))

;;; Chapter 7



