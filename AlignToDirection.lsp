;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                      Align To Direction                                      ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This routine allows users to align any objects to a certain direction that contains any     ;;;
;;;  one of the following properties:                                                            ;;;
;;;                                                                                              ;;;
;;;  1. Insertion Point                                                                          ;;;
;;;  2. Text Position                                                                            ;;;
;;;  3. Text Alignment Position                                                                  ;;;
;;;                                                                                              ;;;
;;;  Upon issuing the command, the user will be prompted a selection of objects without any      ;;;
;;;  filters. This will then be filtered after the selection set, highlighting the list of       ;;;
;;;  objects to be aligned. The user will be prompted the direction by clicking two point, or    ;;;
;;;  one of the following options:                                                               ;;;
;;;                                                                                              ;;;
;;;  1. X-direction,                                                                             ;;;
;;;  2. Y-direction (relative to the UCS)                                                        ;;;
;;;  3. "PerpendicularToCurve" - the user clicks a point on any curve objects and the            ;;;
;;;  direction will be perpendicular to the curve at that specified point.                       ;;;
;;;  4. "AlongCurve" – similar to step 3, but as opposed to perpendicular, it will be the        ;;;
;;;  direction of the curve at that specified point.                                             ;;;
;;;                                                                                              ;;;
;;;  The user can next specify the point where all the objects will be aligned to that           ;;;
;;;  direction. Simply put, it's like aligning marbles on a table using a ruler. Except unlike   ;;;
;;;  marbles, AutoCAD entities do not keep rolling and stick to your cursor.                     ;;;
;;;                                                                                              ;;;
;;;  Objects that I have successfully worked with this program include:                          ;;;
;;;                                                                                              ;;;
;;;  1. Tables                                                                                   ;;;
;;;  2. Dimensions                                                                               ;;;
;;;  3. Blocks                                                                                   ;;;
;;;  4. OLE Images                                                                               ;;;
;;;  5. Texts and MTexts (regardless of justification)                                           ;;;
;;;                                                                                              ;;;
;;;  This program will work under any UCS if the objects to translate resides on the same XY-    ;;;
;;;  plane as the UCS. I am not particularly good at coordinate transformations, so this is the  ;;;
;;;  least I can do.                                                                             ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Versions and updates                                     ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 – First release (27/04/20)                                                      ;;;
;;;                                                                                              ;;;
;;;  Version 1.1 (23/11/2020) – Included "Perpendicular to Curve" and "Along Curve" option.      ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:atd (/	*error*	acadobj	acrv	activeundo	adoc	alpts	ang	ang1
		ang2	blk	gr	grp	grv	halfpi	movang	movdis	movpt
		msp	p1	p2	p3	snapang	snp	snpang	ss	sse
		x	y
	       )
    (defun *error* ( msg )
	(setvar 'snapang snp)
	(if sse (mapcar '(lambda (x) (redraw x 4)) sse))
	(vla-EndUndoMark adoc)
	(if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
	    (princ (strcat "Error: " msg))
	    )
	)
    
    (setq acadobj (vlax-get-acad-object)
	  adoc (vla-get-ActiveDocument acadobj)
	  blk (vla-get-blocks adoc)
	  msp (vla-get-ModelSpace adoc)
	  activeundo nil)
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))
    
    (setq snp (getvar 'snapang))
    (if (setq ss (vl-remove-if-not '(lambda (x)
					(and
					    (vlax-write-enabled-p x)
					    (or
						(vlax-property-available-p x 'insertionpoint)
						(and
						    (eq (strcase (vla-get-ObjectName x)) "ACDBBLOCKREFERENCE")
						    (eq (vla-get-IsXRef (vla-item blk (vla-get-EffectiveName x))) :vlax-false)
						    )
						(vlax-property-available-p x 'textposition)
						(vlax-property-available-p x 'textalignmentpoint)
						(vlax-property-available-p x 'center)
						)
					    )
					)
		     (atd:selset-to-list-vla (ssget "_:L"))
		     )
	      )
	(progn
	    (setq sse (mapcar 'vlax-vla-object->ename ss))
	    (mapcar '(lambda (x) (redraw x 3)) sse)
	    (setq ang (progn (initget 1 "X Y PErpendicularToCurve ALongCurve") (getangle "\nSpecify alignment direction [X/Y/PErpendicularToCurve/ALongCurve]: "))
		  ang (+ (setq halfpi (* 0.5 pi))
			 (cond
			     ((numberp ang) ang)
			     ((eq ang "X") 0)
			     ((eq ang "Y") halfpi)
			     ((wcmatch ang "PErpendicularToCurve,ALongCurve")
			      (setq acrv (atd:GetCurve "\nSpecify point at any curve: "))
			      (+
				  (angle
				      (trans '(0.0 0.0 0.0) 0 1)
				      (trans
					  (vlax-curve-getfirstderiv (car acrv)
					      (vlax-curve-getparamatpoint (car acrv) (trans (cadr acrv) 1 0))
					      ) 0 1
					  )
				      )
				  (if (eq ang "PErpendicularToCurve") halfpi 0)
				  )
			      )
			     )
			 )
		  p1 (trans (vlax-get (car ss) (atd:objprop (car ss))) 0 1)
		  p2 (polar p1 ang 1)
		  alpts (mapcar '(lambda (x y / p3)
				     (list x
					   (inters p1 p2
						   (setq p3 (trans (vlax-get x (atd:objprop x)) 0 1))
						   (polar p3 (+ halfpi ang) 1)
						   nil
						   )
					   )
				     )
				ss
				sse
				)
		  )
	    (setvar 'snapang ang)
	    (while
		(progn
		    (setq gr (grread T 15 0)
			  grp (last gr)
			  grv (car gr)
			  )
		    (cond
			((= grv 5)
			 (setq movpt (inters grp (polar grp ang 1)
					     p1 (polar p1 (- ang halfpi) 1) nil)
			       movang (angle p1 movpt)
			       movdis (distance p1 movpt)
			       )
			 (mapcar '(lambda (x z)
				      (vlax-put (car x) (atd:objprop (car x)) (trans (polar (last x) movang movdis) 1 0))
				      )
				 alpts
				 sse
				 )
			 (mapcar '(lambda (x) (redraw x 3)) sse)
			 T
			 )
			((or (and (= grv 2) (vl-position grp '(13 32))) (= grv 3)) nil)
			(T)
			)
		    )
		)
	    )
	)
    
    (mapcar '(lambda (x) (redraw x 4)) sse)
    (setvar 'snapang snp)
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
    )

(defun atd:objprop (obj)
    (cond
	((vlax-property-available-p obj 'insertionpoint)
	 (if
	     (and
		 (vlax-property-available-p obj 'textalignmentpoint)
		 (null (equal (vlax-get obj 'textalignmentpoint) '(0 0 0) 1e-8))
		 )
	     'textalignmentpoint
	     'insertionpoint
	     )
	 )
	((vlax-property-available-p obj 'textalignmentpoint) 'textalignmentpoint)
	((vlax-property-available-p obj 'textposition) 'textposition)
	((vlax-property-available-p obj 'center) 'center)
	)
    )

(defun atd:GetCurve (msg / crv pt)
    (while
	(progn
	    (setq pt (getpoint msg))
	    (cond
		((not pt) nil)
		((not (setq crv (ssget "_C" pt pt '((0 . "LINE,POLYLINE,LWPOLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE")))))
		 (princ "\nNo curves detected")
		 )
		)
	    )
	)
    (if crv (list (ssname crv 0) pt))
    )

(defun atd:selset-to-list-vla (ss / rtn)
    (if ss
	(repeat (setq i (sslength ss))
	    (setq rtn (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) rtn))
	    )
	)
    rtn
    )