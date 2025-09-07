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

;(with-hypers
;    ((revs 1000)
;     (alpha 0.01))
;  (gradient-descent obj (list 0 0)))
; '(1.0499993623489503 1.8747718457656533e-6)

(define quad-xs #(-1 0 1 2 3))
(define quad-ys #(2.55 2.1 4.35 10.2 18.25))

(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t)
            (ref theta 2))))))

;; (with-hypers
;;     ((revs 1000)
;;      (alpha 0.001))
;;   (gradient-descent ((l2-loss quad) quad-xs quad-ys)
;;                     (list 0 0 0)))
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

;; (with-hypers
;;     ((revs 10000)
;;      (alpha 0.001))
;;   (gradient-descent ((l2-loss plane) plane-xs plane-ys)
;;                     (list (tensor 0 0) 0)))
; '((tensor 3.9796455320046946 1.976454889887173) 6.1695789842820945)

;; (map (lambda (x)
;;        ((plane x) (list (tensor 3.98 1.98) 6.17)))
;;      (list (tensor 1 2.05)
;;            (tensor 1 3)
;;            (tensor 2 2)
;;            (tensor 2 3.91)
;;            (tensor 3 6.13)
;;            (tensor 4 8.09)))
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

;; (with-hypers
;;     ((revs 500)
;;      (alpha 0.01)
;;      (batch-size 3))
;;   (gradient-descent (sampling-obj (l2-loss line) line-xs line-ys)
;;                     (list 0 0)))
;; '(1.0643197330294396 0.0110968108351973)

;; (with-hypers
;;     ((revs 2000)
;;      (alpha 0.001)
;;      (batch-size 5))
;;   (gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
;;                     (list (tensor 0 0) 0)))
;; '((tensor 4.010846297062351 1.9841311991926505) 6.094113832424458)

;;; Chapter 7

;; theta -> big-theta
(define lonely-i ; i is for inflate
  (lambda (p) (list p)))

;; big-theta -> theta
(define lonely-d ; d for deflate
  (lambda (P) (ref P 0)))

;; update
(define lonely-u
  (lambda (P g)
    (list (- (ref P 0) (* alpha g)))))

(define new-gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (big-theta)
                 (map update
                      big-theta
                      (gradient-of obj (map deflate big-theta))))))
        (map deflate (revise f revs (map inflate theta)))))))

(define lonely-gradient-descent
  (new-gradient-descent lonely-i lonely-d lonely-u))

(define try-plane
  (lambda (a-gradient-descent)
    (with-hypers
        ((revs 15000)
         (alpha 0.001)
         (batch-size 4))
      (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                          (list (tensor 0 0) 0)))))

;; (try-plane lonely-gradient-descent)
;; '((tensor 3.978802005223656 1.967861445812276) 6.163704752304487)

(define naked-i
  (lambda (p)
    (let ((P p))
      P)))

(define naked-d
  (lambda (P)
    (let ((p P))
      p)))

(define naked-u
  (lambda (P g)
    (- P (* alpha g))))

(define naked-gradient-descent
  (new-gradient-descent naked-i naked-d naked-u))

;; (try-plane naked-gradient-descent)
;; '((tensor 3.978802005223656 1.967861445812276) 6.163704752304487)

(declare-hyper mu)

(define velocity-i
  (lambda (p) (list p (zeroes p))))

(define velocity-d
  (lambda (P) (ref P 0)))

(define velocity-u
  (lambda (P g)
    (let ((v (- (* mu (ref P 1)) (* alpha g))))
      (list (+ (ref P 0) v) v))))

(define velocity-gradient-descent
  (new-gradient-descent velocity-i velocity-d velocity-u))

(define second-try-plane
  (lambda (a-gradient-descent a-revs)
    (with-hypers
        ((revs a-revs)
         (alpha 0.001)
         (batch-size 4))
      (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                          (list (tensor 0 0) 0)))))

(with-hypers
    ((mu 0.9))
  (second-try-plane velocity-gradient-descent 2000))
;; '((tensor 3.9640144427797903 1.9448860283069427) 6.183501161299013)

;;; Interlude 4

(define smooth
  (lambda (decay-rate average g)
    (+ (* decay-rate average)
       (* (- 1 decay-rate) g))))

;; Chapter 8

(declare-hyper beta)
(define epsilon 1e-08)

(define rms-u
  (lambda (P g)
    (let ((r (smooth beta (ref P 1) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon))))
        (list (- (ref P 0) (* alpha-hat g)) r)))))

(define rms-i
  (lambda (p) (list p (zeroes p))))

(define rms-d
  (lambda (P) (ref P 0)))

(define rms-gradient-descent
  (new-gradient-descent rms-i rms-d rms-u))

(define third-try-plane
  (lambda (a-gradient-descent a-revs an-alpha)
    (with-hypers
        ((revs a-revs)
         (alpha an-alpha)
         (batch-size 4))
      (a-gradient-descent (sampling-obj (l2-loss plane) plane-xs plane-ys)
                          (list (tensor 0 0) 0)))))

(with-hypers
    ((beta 0.9))
  (third-try-plane rms-gradient-descent 2000 0.01))

(define adam-u
  (lambda (P g)
    (let ((r (smooth beta (ref P 2) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon)))
            (v (smooth mu (ref P 1) g)))
        (list (- (ref P 0) (* alpha-hat v)) v r)))))

(define adam-i
  (lambda (p)
    (let ((v (zeroes p)))
      (let ((r v))
        (list p v r)))))

(define adam-d
  (lambda (P)
    (ref P 0)))

(define adam-gradient-descent
  (new-gradient-descent adam-i adam-d adam-u))

(with-hypers
    ((mu 0.85)
     (beta 0.9))
  (third-try-plane adam-gradient-descent 1500 0.001))
