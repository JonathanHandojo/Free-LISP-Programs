;;;                                                                                              ;;;
;;;  GrPolar --> Jonathan Handojo                                                                ;;;
;;;  Constructs a polar vector and the cross denoting the snap point to the polar whilst using   ;;;
;;;  grread.                                                                                     ;;;
;;;                                                                                              ;;;
;;;  If the functions LM:GrText and LM:DisplayGrText are present, the relevant polar distance    ;;;
;;;  and angle will be displayed next to the cursor.                                             ;;;
;;;                                                                                              ;;;
;;;  This function is called first without any arguments, which will then return an enhanced     ;;;
;;;  function accepting three arguments:                                                         ;;;
;;;                                                                                              ;;;
;;;  bpt - the base point for polar tracking in UCS                                              ;;;
;;;  ppt - the relative point for polar tracking in UCS                                          ;;;
;;;  rb - 'T' to display rubber-band line from 'bpt' to 'ppt'... 'nil' otherwise                 ;;;
;;;                                                                                              ;;;
;;;  If successful, the returned function returns the snapped point to the polar tracking in     ;;;
;;;  UCS whilst also displaying the polar tracking vector. Otherwise it will return the          ;;;
;;;  supplied relative point. Regardless, if 'rb' is set to T, the rubber-band line will be      ;;;
;;;  displayed from the base point to the returned point.                                        ;;;
;;;                                                                                              ;;;
;;;  Polar snap is also accounted for in this function. Competent LISP users may toggle the      ;;;
;;;  SNAPMODE variables whilst using GRREAD and see visual changes.                              ;;;
;;;                                                                                              ;;;
;;;  This will work under all UCS and View settings. Credits to Lee Mac for color conversion     ;;;
;;;  functions used in polar tracking vector, snap, and rubber-band line.                        ;;;
;;;                                                                                              ;;;

(defun JH:GrPolar ( / 45rad 90rad coltrack colsnap rubcol snp snpds)
    (setq 90rad (* 0.5 pi)
	  45rad (* (/ pi 180) 45)
	  coltrack (LM:OLE->ACI (atoi (cond ((getenv (if (= (getvar 'cvport) 1) "Layout ATrack vector" "Model ATrack vector"))) ("104193"))))
	  colsnap (LM:OLE->ACI (atoi (cond ((getenv (if (= (getvar 'cvport) 1) "LayoutXhairPickboxEtc" "XhairPickboxEtc"))) ("16777215"))))
	  rubcol (LM:OLE->ACI (atoi (cond ((getenv (if (= (getvar 'cvport) 1) "Layout Rubber-band Line color" "Model Rubber-band Line Color"))) ("2012400"))))
	  snp (JH:GrPolar:anglefix (+ (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans (getvar 'viewdir) 1 0 t) t)) (getvar 'viewtwist)))
	  )
    (if (zerop (setq snpds (getvar 'polardist)))
	(setq snpds (car (getvar 'snapunit)))
	)
    (eval
	(list
	    'lambda '(bpt ppt rb / ang bds dis gds pds s scr snaps vs)
	    '(setq vs (getvar 'viewsize)
		  ang (getvar 'polarang)
		  dis (* 0.0115 vs)
		  scr (getvar 'screensize)
		  s (- ang)
		  bpt (trans bpt 1 2)
		  ppt (trans ppt 1 2)
		  bds (* (/ vs (cadr scr)) 15)
		  gds (* (/ vs (cadr scr)) 10)
		  pds (* (/ vs (cadr scr)) 4)
		  )
	    
	    '(repeat (fix (/ (* 2 pi) ang))
		(setq snaps (cons (setq s (+ ang s)) snaps))
		)
	    
	    (list 'if '(= 8 (logand 8 (getvar 'autosnap)))
		  (list 'cond
			(list
			    (list 'vl-some
				  (list 'function
					(list 'lambda '(x / catch)
					      (list 'if
						    (list 'and
							  (list 'equal
								'ppt
								(list 'setq 'catch
								      (list 'inters 'bpt
									    (list 'polar 'bpt (list '+ snp 'x) 100)
									    'ppt
									    (list 'polar 'ppt
										  (list '+ 90rad snp 'x)
										  100
										  )
									    nil
									    )
								      )
								'dis
								)
							  (list 'equal '(JH:GrPolar:anglefix (angle bpt catch)) (list 'JH:GrPolar:anglefix (list '+ snp 'x)) 1e-7)
							  )
						    (list 'progn
							  (list 'if
								'(and
								    (= (getvar 'snapmode) 1)
								    (= (getvar 'snaptype) 1)
								    )
								(list 'progn
								      (list 'setq 'catch
									    (list 'polar 'bpt
										  '(angle bpt catch)
										  (list '* snpds
											(list '+
											      (list 'if
												    (list '>
													  (list 'rem '(distance bpt catch) snpds)
													  (* 0.5 snpds)
													  )
												    1 0
												    )
											      (list 'fix
												    (list '/ '(distance bpt catch) snpds)
												    )
											      )
											)
										  )
									    )
								      )
								)
							  (list 'grvecs
								(list 'list
								      colsnap
								      (list 'polar 'catch 45rad 'dis) 		(list 'polar 'catch (+ 45rad pi) 'dis)
								      (list 'polar 'catch (+ 45rad 90rad) 'dis)	(list 'polar 'catch (+ 45rad 90rad pi) 'dis)
								      )
								(list 'quote
								      '(
									(1.0 0.0 0.0 0.0)
									(0.0 1.0 0.0 0.0)
									(0.0 0.0 1.0 0.0)
									(0.0 0.0 0.0 1.0)
									)
								      )
								)
							  (list 'JH:GrPolar:dashed 'bpt '(polar bpt (angle bpt catch) (* 5 vs)) 'pds 'pds coltrack)
							  (list 'if 'rb (list 'JH:GrPolar:dashed 'bpt 'catch 'gds 'bds rubcol))
							  (if
							      (and LM:GrText LM:DisplayGrText)
							      (list 'LM:DisplayGrText '(trans ppt 2 1)
								    '(LM:GrText (strcat "Polar: " (rtos (distance bpt catch)) " < " (rtos (* (/ 180 pi) x)) "°")) colsnap
								    15 -30
								    )
							      )
							  '(trans catch 2 1)
							  )
						    )
					      )
					)
				  'snaps
				  )
			    )
			(list (list 'progn (list 'if 'rb (list 'JH:GrPolar:dashed 'bpt 'ppt 'gds 'bds rubcol)) '(trans ppt 2 1)))
			)
		  (list 'progn (list 'if 'rb (list 'JH:GrPolar:dashed 'bpt 'ppt 'gds 'bds rubcol)) '(trans ppt 2 1))
		  )
	    )
	)
    )

;; JH:GrPolar:dashed --> Jonathan Handojo
;; Displays a dashed line upon using grread
;; bpt - UCS first point
;; ppt - UCS second point
;; gap - the spacing distance
;; dis - the solid line distance
;; col - integer denoting Autocad Color Index of line color to display

(defun JH:GrPolar:dashed (bpt ppt gap dis col / pt rba rbn rbp)
    (setq rba (angle bpt ppt)
	  pt  bpt
	  )
    (repeat (fix (/ (distance bpt ppt) (+ dis gap)))
	(setq rbp (cons pt rbp)
	      pt  (polar pt rba (+ dis gap))
	      )
	)
    (grvecs
	(cons col
	      (append
		  (apply
		      (quote append)
		      (mapcar (quote (lambda (x) (list x (polar x rba dis))))
			      (reverse rbp)
			      )
		      )
		  (list pt ppt)
		  )
	      )
	'((1.0 0.0 0.0 0.0)
	  (0.0 1.0 0.0 0.0)
	  (0.0 0.0 1.0 0.0)
	  (0.0 0.0 0.0 1.0)
	  )
	)
    )


;; JH:GrPolar:anglefix --> Jonathan Handojo
;; Fixes the value 'ang' so that the value is not less than 0 and
;; not greater than or equal to 2*pi.
;; ang - angle in radians

(defun JH:GrPolar:anglefix (ang)
    (cond
	((minusp ang) (GrPolar:anglefix (+ ang pi pi)))
	((>= ang (+ pi pi)) (GrPolar:anglefix (- ang pi pi)))
	((if (or (equal ang 0.0 1e-6) (equal ang (+ pi pi) 1e-6)) 0.0 ang))
	)
    )

;; JH:GrPolar:pointfromstring --> Jonathan Handojo
;; Returns the reference point supplied from a string (ex. @10,20 or @10<135)
;; pt - base point
;; str - reference string

(defun JH:GrPolar:pointfromstring (pt str / lst)
    (cond
	((wcmatch str "`@*")
	 (setq str (substr str 2))
	 (cond
	     ((and
		  (<= 2 (length (setq lst (fl:str->lst str ","))) 3)
		  (vl-every 'numberp (setq lst (mapcar 'distof lst)))
	      )
	      (mapcar '+ pt lst)
	     )
	     ((and
		  (= 2 (length (setq lst (fl:str->lst str "<"))))
		  (vl-every 'numberp (setq lst (mapcar 'distof lst)))
	      )
	      (polar pt (* (cadr lst) (/ pi 180)) (car lst))
	     )
	 )
	)
	((and
	     (<= 2 (length (setq lst (fl:str->lst str ","))) 3)
	     (vl-every 'numberp (setq lst (mapcar 'distof lst)))
	 )
	 lst
	)
    )
)


;; OLE -> ACI  -  Lee Mac
;; Args: c - [int] OLE Colour
 
(defun LM:OLE->ACI ( c )
    (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)
 
;; OLE -> RGB  -  Lee Mac
;; Args: c - [int] OLE Colour
 
(defun LM:OLE->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)
 
;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values
 
(defun LM:RGB->ACI ( r g b / c o )
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
        )
    )
)

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)
