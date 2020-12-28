

(defun c:3darc ( / p1 p2 p3)
    (and
	(setq p1 (getpoint "\nSpecify first point <exit>: "))
	(setq p2 (getpoint "\nSpecify second point <exit>: "))
	(setq p3 (getpoint "\nSpecify third point <exit>: "))
	(3DArc p1 p2 p3)
	)
    (princ)
    )

;; 3DArc --> Jonathan Handojo
;; Draws a 3D arc that passes through points p1, p2, and p3.
;; Returns the arc entity (ename)

(defun 3DArc (p1 p2 p3 / cen n1 m1 m2)
    (setq n1 (3DArc:vx1 (3DArc:v^v (mapcar '- p1 p2) (mapcar '- p3 p2))))
    (mapcar
	'(lambda (x)
	     (set x
		  (mapcar
		      '(lambda (y)
			   (if (< (abs y) 1e-8) 0.0 y)
			   )
		      (trans (eval x) 0 n1)
		      )
		  )
	     )
	'(p1 p2 p3)
	)
    (setq m1 (mapcar '(lambda (x y) (/ (+ x y) 2.0)) p1 p2)
	  m2 (mapcar '(lambda (x y) (/ (+ x y) 2.0)) p2 p3)
	  cen
	     (inters
		 m1 (polar m1 (+ (* 0.5 pi) (angle p1 p2)) 1)
		 m2 (polar m2 (+ (* 0.5 pi) (angle p2 p3)) 1)
		 nil
		 )
	  )
    (entmakex
	(list
	    '(0 . "ARC")
	    (cons 10 cen)
	    (cons 40 (distance cen p1))
	    (cons 50 (angle cen p3))
	    (cons 51 (angle cen p1))
	    (cons 210 n1)
	    )
	)
    )

;;; Online References ;;;

;; Vector Cross Product  -  Lee Mac
;; Args: u,v - vectors in R^3

(defun 3DArc:v^v ( u v )
    (list
        (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
        (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
        (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
    )
)

;; Unit Vector  -  Lee Mac
;; Args: v - vector in R^2 or R^3

(defun 3DArc:vx1 ( v )
    (   (lambda ( n ) (if (equal 0.0 n 1e-10) nil (mapcar '/ v (list n n n))))
        (distance '(0.0 0.0 0.0) v)
    )
)