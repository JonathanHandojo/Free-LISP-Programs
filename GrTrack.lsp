;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                         GrTrack.lsp                                          ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;             (with thanks to Lee Mac's GrSnap to aid in displaying snap symbols)              ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This LISP routine aims to generate all polar tracking vectors from a supplied base point    ;;;
;;;  and all marked snaps whilst using grread. The motivation for such function lies in          ;;;
;;;  accuracy of returning snapped points to all polar tracking whilst using grread. Snap and    ;;;
;;;  polar tracking are unavailable in this mode, and thus this routine will aim to deliver      ;;;
;;;  just that.                                                                                  ;;;
;;;                                                                                              ;;;
;;;  This routine consists of the main function 'JH:GrTrack' which is first called without any   ;;;
;;;  arguments. Following this, it returns an enhanced function accepting four arguments:        ;;;
;;;                                                                                              ;;;
;;;  bpt – base point in UCS used for polar tracking, 'nil' if none                              ;;;
;;;  ppt – the point to try and snap to in UCS.                                                  ;;;
;;;  rb – if 'bpt' if a point, set this to 'T' to display rubber-band line from 'bpt' to         ;;;
;;;  'ppt', set to 'nil' otherwise                                                               ;;;
;;;  zm – 'T' to clear all marked snap references on mouse scroll/zoom, 'nil' otherwise          ;;;
;;;                                                                                              ;;;
;;;  If the snap is successful, the returned function returns the snapped point to any polar     ;;;
;;;  tracking and snapped points whilst also displaying any relevant snap symbols and tracking   ;;;
;;;  vectors. If all snaps fail, the returned function will return the supplied point in UCS.    ;;;
;;;  Regardless, if 'bpt' is a point and 'rb' is set to 'T', the rubber-band line will be        ;;;
;;;  displayed from 'bpt' to the returned point of the function.                                 ;;;
;;;                                                                                              ;;;
;;;  ------------------------------------------------------------------------------------------  ;;;
;;;                                                                                              ;;;
;;;  Polar snap is also taken into account in this routine. Competent LISP users may toggle      ;;;
;;;  the SNAPMODE variable whilst using grread to see visual changes.                            ;;;
;;;                                                                                              ;;;
;;;  ------------------------------------------------------------------------------------------  ;;;
;;;                                                                                              ;;;
;;;  Because the GrSnap function only displays the snap symbol and does not return the type of   ;;;
;;;  snap (midpoint, endpoint, etc…), this function will only display the relevant polar         ;;;
;;;  tracking distance and angle next to the snap symbol (e.g. 125.15 < 30°). (However, I        ;;;
;;;  figured that I could write my own function to return the snap type alongside the snapped    ;;;
;;;  point, but this would completely overwrite the already existing amazing GrSnap function. I  ;;;
;;;  certainly don't want to fully take all credits. All the great LISP developers have          ;;;
;;;  contributed to finally make this possible.)                                                 ;;;
;;;                                                                                              ;;;
;;;  Should the functions LM:GrText and LM:DisplayGrText not be found or present, the polar      ;;;
;;;  tracking details and angles will be displayed in the 'Drawing Coordinates' status bar (the  ;;;
;;;  bar that displays the cursor's coordinates).                                                ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                       Snap References                                        ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Snap references are stored under the global variable 'JH:GrTrack|ReferenceList' on using    ;;;
;;;  this track. This is used to keep track of any snaps that have been marked. To clear these   ;;;
;;;  snaps, simply call the function (JH:GrTrack:Refresh) to clear all snap references. Like     ;;;
;;;  all similar commands, it's a good idea to clear all references on mouse click               ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                         Limitations                                          ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  1. This routine is not compatible with the Extension and Nearest snaps.                     ;;;
;;;  2. This routine is unable to find perpendicular snaps from a given base point to an         ;;;
;;;     Autocad Entity.                                                                          ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun JH:GrTrack ( / 45rad 90rad apt colcross colrub colsnap coltrack cv pmd sf snp snpds)
    (setq 90rad (* 0.5 pi)
	  45rad (* 0.25 pi)
	  cv (= (getvar 'cvport) 1)
	  coltrack (LM:OLE->ACI (atoi (cond ((getenv (if cv "Layout ATrack vector" "Model ATrack vector"))) ("104193"))))
	  colcross (LM:OLE->ACI (atoi (cond ((getenv (if cv "LayoutXhairPickboxEtc" "XhairPickboxEtc"))) ("16777215"))))
	  colsnap (LM:OLE->ACI (atoi (cond ((getenv (if cv "Layout AutoSnap Color" "Model AutoSnap Color"))) ("104193"))))
	  colrub (LM:OLE->ACI (atoi (cond ((getenv (if cv "Layout Rubber-band Line color" "Model Rubber-band Line Color"))) ("2012400"))))
	  snp (JH:GrTrack:anglefix (+ (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans (getvar 'viewdir) 1 0 t) t)) (getvar 'viewtwist)))
	  pmd (= 2 (logand 2 (getvar 'polarmode)))
	  apt (getvar 'aperture)
	  sf (LM:grsnap:snapfunction)
	  )
    (if (zerop (setq snpds (getvar 'polardist)))
	(setq snpds (car (getvar 'snapunit)))
	)

    (JH:GrTrack:Refresh)
    (eval
	(list 'lambda '(bpt ppt rb zm / 05snpa ang apt bds dis gds in osm pds pmd s scr snaps snpa snps snpt vs)
	    (list 'if '(= 16 (logand 16 (getvar 'autosnap)))
		(list 'progn
		      (list 'setq
			    'osm '(getvar 'osmode)
			    'vs '(getvar 'viewsize)
			    'ang (if pmd '(getvar 'polarang) 90rad)
			    'dis '(* 0.0115 vs)
			    'scr '(getvar 'screensize)
			    's '(- ang)
			    'ppt '(trans ppt 1 2)
			    'bds '(* (/ vs (cadr scr)) 15)
			    'gds '(* (/ vs (cadr scr)) 10)
			    'pds '(* (/ vs (cadr scr)) 4)
			    'snpa (list '* '(/ vs (cadr scr)) apt)
			    '05snpa '(* 0.5 snpa)
			    )

		      '(if bpt (setq bpt (trans bpt 1 2)))
		      '(if (= 512 (logand 512 osm)) (setq osm (- osm 512)))
		      '(if (zerop (logand 16384 osm))
			  (setq snps
				   (LM:lst->str
				       (mapcar 'cdr
					       (vl-remove-if-not
						   '(lambda (x)
							(not (zerop (logand (car x) (getvar 'osmode))))
							)
						   '(
						     (1 . "_end")
						     (2 . "_mid")
						     (4 . "_cen")
						     (8 . "_nod")
						     (16 . "_qua")
						     (32 . "_int")
						     (64 . "_ins")
						     (128 . "_per")
						     (256 . "_tan")
						     (2048 . "_app")
						     (8192 . "_par")
						     )
						   )
					       )
				       ","
				       )
				)
			  )
		    '(repeat (fix (/ (* 2 pi) ang))
			(setq snaps (cons (setq s (+ ang s)) snaps))
			)
		      (list 'and 'snps
			    '(osnap (trans ppt 2 1) snps)
			    (list 'if
				  '(not (vl-some '(lambda (f) (equal f ppt snpa)) JH:GrTrack|Intersections))
				  (list 'setq 'snpt (list 'trans (list sf '(trans ppt 2 1) 'osm) 1 2))
				  )
			    '(cond
				((and JH:GrTrack|Release (<= (distance ppt snpt) snpa))
				 (setq JH:GrTrack|Release nil)
				 (if
				     (vl-some '(lambda (x) (equal (car x) snpt snpa)) JH:GrTrack|ReferenceList)
				     (setq JH:GrTrack|ReferenceList
					      (vl-remove-if
						  '(lambda (a) (equal (car a) snpt snpa))
						  JH:GrTrack|ReferenceList
						  )
					   )
				     (setq JH:GrTrack|ReferenceList (cons (list (JH:GrTrack:PointFix snpt)) JH:GrTrack|ReferenceList))
				     )
				 )
				((> (distance ppt snpt) snpa) (setq JH:GrTrack|Release t))
				)
			    )
		      (list 'grvecs
			    (list 'cons colsnap
				  (list 'apply
					(list 'quote 'append)
					(list 'mapcar
					      (list 'function
						    (list 'lambda '(a)
							  (list 'list
								(list 'polar '(car a) 0 '05snpa)
								(list 'polar '(car a) pi '05snpa)
								(list 'polar '(car a) 90rad '05snpa)
								(list 'polar '(car a) (+ 90rad pi) '05snpa)
								)
							  )
						    )
					      '(vl-remove-if 'cdr JH:GrTrack|ReferenceList)
					      )
					)
				  )
			    (list 'quote
				  '((1.0 0.0 0.0 0.0)
				    (0.0 1.0 0.0 0.0)
				    (0.0 0.0 1.0 0.0)
				    (0.0 0.0 0.0 1.0)
				    )
				  )
			    )

		      '(and zm
			  (not (equal vs JH:GrTrack|ViewSize 1e-7))
			  (setq JH:GrTrack|ViewSize vs)
			  (JH:GrTrack:Refresh)
			  )
		      '(and bpt
			   (if
			       (= 8 (logand 8 (getvar 'autosnap)))
			       (if (not (vl-remove-if-not 'cdr JH:GrTrack|ReferenceList))
				   (setq JH:GrTrack|ReferenceList
					    (cons
						(cons (JH:GrTrack:PointFix bpt) t)
						JH:GrTrack|ReferenceList
						)
					 )
				   )
			       (setq JH:GrTrack|ReferenceList (vl-remove-if 'cdr JH:GrTrack|ReferenceList))
			       )
			   )
		      (list 'cond
			    (list
				(list 'vl-some
				      (list 'function
					    (list 'lambda '(x / tmp1 snpt)
						  '(setq tmp1 (car x))
						  (list 'vl-some
							(list 'function
							      (list 'lambda '(y / catch ss)
								    (list 'if
									  (list 'and
										'(not (equal ppt tmp1 snpa))
										(list 'equal
										      'ppt
										      (list 'setq 'catch
											    (list 'inters
												  'ppt (list 'polar 'ppt (list '+ snp 90rad 'y) 10)
												  'tmp1 (list 'polar 'tmp1 (list '+ snp 'y) 10)
												  nil
												  )
											    )
										      'dis
										      )
										(list 'equal (list 'JH:GrTrack:anglefix (list '+ snp 'y)) '(JH:GrTrack:anglefix (angle tmp1 catch)) 1e-7)
										)
									  (list 'progn
										(list sf '(trans tmp1 2 1) 'osm)	; <-- display snap symbol to tracking
										'(and (not JH:GrTrack|Fence)
										     (= 32 (logand 32 osm))
										     (setq JH:GrTrack|Fence t ss (ssget "_F" (list (trans tmp1 2 1) (polar (trans tmp1 2 1) y (* vs 5)))))
										     (setq JH:GrTrack|Intersections
											      (mapcar
												  '(lambda (f) (JH:GrTrack:PointFix (trans f 0 2)))
												  (apply 'append
													 (mapcar
													     '(lambda (e)
														  (apply 'append
															 (mapcar 'cdr (cdddr e))
															 )
														  )
													     (ssnamex ss)
													     )
													 )
												  )
											   )
										     )
										(list 'JH:GrTrack:dashed
										      (list 'if '(cdr x) 'bpt (list 'polar 'tmp1 (list '+ snp 'y pi) '(* 5 vs)))
										      (list 'polar 'tmp1 (list '+ snp 'y) '(* 5 vs))
										      'pds 'pds coltrack
										      )
										(list 'cond
										      (list
											  (list 'vl-some
												(list 'quote
												      (list 'lambda '(q)
													    (list 'if
														  '(equal q catch snpa)
														  (list 'progn
															(list 'LM:grsnap:displaysnap '(trans q 2 1)
															      (list 'cdr (list 'assoc 32 (list 'LM:grsnap:snapsymbols (atoi (cond ((getenv "AutoSnapSize")) ("5"))))))
															      colsnap
															      )
															(if
															    (and LM:GrText LM:DisplayGrText)
															    (list 'LM:DisplayGrText '(trans q 2 1)
																'(LM:GrText "Intersection") colcross
																15 15
																)
															    (grtext -2 "Intersection")
															    )
															'(trans q 2 1)	; Intersection return value
															)
														  )
													    )
												      )
												'JH:GrTrack|Intersections
												)
											  )
										      (list
											  (list 'vl-some
												(list 'function
												      (list 'lambda '(a  / tmp2)
													    '(setq tmp2 (car a))
													    (list 'vl-some
														  (list 'quote
															(list 'lambda '(b / fixed)
															      (list 'if
																    (list 'and
																	  '(not (equal ppt tmp2 snpa))
																	  (list 'equal
																		'catch
																		(list 'setq 'fixed
																		      (list 'inters
																			    'tmp1 (list 'polar 'tmp1 (list '+ snp 'y) 10)
																			    'tmp2 (list 'polar 'tmp2 (list '+ snp 'b) 10)
																			    nil
																			    )
																		      )
																		'dis
																		)
																	  (list 'equal (list 'JH:GrTrack:anglefix (list '+ snp 'b)) '(JH:GrTrack:anglefix (angle tmp2 fixed)) 1e-7)
																	  )
																    (list 'progn
																	  (list sf '(trans tmp2 2 1) 'osm)	; <-- display snap symbol to tracking
																	  (list 'JH:GrTrack:dashed
																		(list 'if '(cdr a) 'bpt (list 'polar 'tmp2 (list '+ snp 'b pi) '(* 5 vs)))
																		(list 'polar 'tmp2 (list '+ snp 'b) '(* 5 vs))
																		'pds
																		'pds
																		coltrack
																		)
																	  (list 'if 'bpt (list 'JH:GrTrack:dashed 'bpt 'fixed 'gds 'bds colrub))
																	  (if
																	      (and LM:grtext LM:DisplayGrText)
																	      (list 'progn
																		    (list 'LM:DisplayGrText
																			  '(trans (if (cdr a) ppt tmp2) 2 1)
																			  '(LM:grtext
																			      (strcat
																				  (if (cdr a) "Polar: " "")
																				  (rtos (distance tmp2 fixed))
																				  " < "
																				  (rtos (JH:GrTrack:RadToDeg b))
																				  "°"
																				  )
																			      )
																			  colcross 15 '(if (cdr a) -30 15)
																			  )
																		    (list 'LM:DisplayGrText
																			  '(trans (if (cdr x) ppt tmp1) 2 1)
																			  '(LM:grtext
																			      (strcat
																				  (if (cdr x) "Polar: " "")
																				  (rtos (distance tmp1 fixed))
																				  " < "
																				  (rtos (JH:GrTrack:RadToDeg y))
																				  "°"
																				  )
																			      )
																			  colcross 15 '(if (cdr x) -30 15)
																			  )
																		    )
																	      )
																	  (list 'grvecs
																		(list 'list colcross
																		      (list 'polar 'fixed 45rad 'dis) (list 'polar 'fixed (+ 45rad pi) 'dis)
																		      (list 'polar 'fixed (+ 45rad 90rad) 'dis) (list 'polar 'fixed (+ pi 45rad 90rad) 'dis)
																		      )
																		(list 'quote
																		      '((1.0 0.0 0.0 0.0)
																			(0.0 1.0 0.0 0.0)
																			(0.0 0.0 1.0 0.0)
																			(0.0 0.0 0.0 1.0)
																			)
																		      )
																		)
																	  '(trans fixed 2 1)	; return value
																	  )
																    )
															      )
															)
														  'snaps
														  )
													    )
												      )
												'(vl-remove-if '(lambda (d) (equal (car d) tmp1 1e-6)) JH:GrTrack|ReferenceList)
												)
											  )
										      (list t
											  (list 'progn
												(list 'if
												      '(and
													  (= (getvar 'snapmode) 1)
													  (= (getvar 'snaptype) 1)
													  )
												      (list 'progn
													    (list 'setq 'catch
														  (list 'polar 'tmp1
															'(angle tmp1 catch)
															(list '* snpds
															      (list '+
																    (list 'if
																	  (list '>
																		(list 'rem '(distance tmp1 catch) snpds)
																		(* 0.5 snpds)
																		)
																	  1 0
																	  )
																    (list 'fix
																	  (list '/ '(distance tmp1 catch) snpds)
																	  )
																    )
															      )
															)
														  )
													    )
												      )
												(if
												    (and LM:grtext LM:DisplayGrText)
												    (list 'LM:DisplayGrText
													  '(trans (if (cdr x) ppt tmp1) 2 1)
													  '(LM:grtext
													      (strcat
														  (if (cdr x) "Polar: " "")
														  (rtos (distance tmp1 catch))
														  " < "
														  (rtos (JH:GrTrack:RadToDeg y))
														  "°"
														  )
													      )
													  colcross 15 '(if (cdr x) -30 15)
													  )
												    '(grtext -2 (strcat (if (cdr x) "Polar: " "") (rtos (distance tmp1 catch)) " < " (rtos (JH:GrTrack:RadToDeg y)) "°"))
												    )
												(list 'if 'bpt (list 'JH:GrTrack:dashed 'bpt 'catch 'gds 'bds colrub))
												(list 'grvecs
												      (list 'list colcross
													    (list 'polar 'catch 45rad 'dis) (list 'polar 'catch (+ 45rad pi) 'dis)
													    (list 'polar 'catch (+ 45rad 90rad) 'dis) (list 'polar 'catch (+ pi 45rad 90rad) 'dis)
													    )
												      (list 'quote
													    '((1.0 0.0 0.0 0.0)
													      (0.0 1.0 0.0 0.0)
													      (0.0 0.0 1.0 0.0)
													      (0.0 0.0 0.0 1.0)
													      )
													    )
												      )
												'(trans catch 2 1)	; return value
												)
											  )
										      )
										)
									  )
								    )
							      )
							'snaps
							)
						  )
					    )
				      'JH:GrTrack|ReferenceList
				      )
				)
			    (list
				(list 'progn
				      (list 'if 'bpt (list 'JH:GrTrack:dashed 'bpt '(cond (snpt) (ppt)) 'gds 'bds colrub))
				      '(setq JH:GrTrack|Intersections nil JH:GrTrack|Fence nil)
				      '(trans (cond (snpt) (ppt)) 2 1)	; return value
				      )
				)
			    )
		    )
		  (list 'progn
			(list 'if 'bpt (list 'JH:GrTrack:dashed 'bpt '(cond (snpt) (ppt)) 'gds 'bds colrub))
			'(setq JH:GrTrack|Intersections nil JH:GrTrack|Fence nil)
			'(trans (cond (snpt) (ppt)) 2 1)	; return value
			)
		)
	    )
	)
    )

;; JH:GrTrack:PointFix --> Jonathan Handojo
;; Sets x,y,z coordinates of pt to 0 if their values are zero to a tolerance of 0.0000001
;; pt - Point (list of 3 numbers)
(defun JH:GrTrack:PointFix (pt)
    (mapcar '(lambda (x) (if (< (abs x) 1e-7) 0.0 x)) pt)
    )

;; JH:GrTrack:anglefix --> Jonathan Handojo
;; Fixes the value 'ang' so that the value is not less than 0 and
;; not greater than or equal to 2*pi.
;; ang - angle in radians

(defun JH:GrTrack:anglefix (ang)
    (cond
	((minusp ang) (JH:GrTrack:anglefix (+ ang pi pi)))
	((>= ang (+ pi pi)) (JH:GrTrack:anglefix (- ang pi pi)))
	((if (or (equal ang 0.0 1e-6) (equal ang (+ pi pi) 1e-6)) 0.0 ang))
	)
    )

;; JH:GrTrack:dashed --> Jonathan Handojo
;; Displays a dashed line upon using grread
;; bpt - UCS first point
;; ppt - UCS second point
;; gap - the spacing distance
;; dis - the solid line distance
;; col - integer denoting Autocad Color Index of line color to display

(defun JH:GrTrack:dashed (bpt ppt gap dis col / pt rba rbn rbp)
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

;; JH:GrTrack:Refresh --> Jonathan Handojo
;; Removes all snap references.

(defun JH:GrTrack:Refresh nil
    (foreach x
	     '(
	       JH:GrTrack|ReferenceList
	       JH:GrTrack|Release
	       JH:GrTrack|Intersections
	       JH:GrTrack|Fence
	       )
	(set x nil)
	)
    )

;; JH:GrTrack:RadToDeg --> Jonathan Handojo
;; Converts an angle in radians to degrees.

(defun JH:GrTrack:RadToDeg (ang)
    (* ang (/ 180 pi))
    )

;;; -------------------- GrSnap (By Lee Mac) -------------------- ;;;

;; Object Snap for grread: Snap Function  -  Lee Mac
;; Returns: [fun] A function requiring two arguments:
;; p - [lst] UCS Point to be snapped
;; o - [int] Object Snap bit code
;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
;; or the supplied point if the snap failed for the given Object Snap bit code.
 
(defun LM:grsnap:snapfunction ( )
    (eval
        (list 'lambda '( p o / q )
            (list 'if '(zerop (logand 16384 o))
                (list 'if
                   '(setq q
                        (cdar
                            (vl-sort
                                (vl-remove-if 'null
                                    (mapcar
                                        (function
                                            (lambda ( a / b )
                                                (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                                                    (list (distance p b) b (car a))
                                                )
                                            )
                                        )
                                       '(
                                            (0001 . "_end")
                                            (0002 . "_mid")
                                            (0004 . "_cen")
                                            (0008 . "_nod")
                                            (0016 . "_qua")
                                            (0032 . "_int")
                                            (0064 . "_ins")
                                            (0128 . "_per")
                                            (0256 . "_tan")
                                            (0512 . "_nea")
                                            (2048 . "_app")
                                            (8192 . "_par")
                                        )
                                    )
                                )
                               '(lambda ( a b ) (< (car a) (car b)))
                            )
                        )
                    )
                    (list 'LM:grsnap:displaysnap '(car q)
                        (list 'cdr
                            (list 'assoc '(cadr q)
                                (list 'quote
                                    (LM:grsnap:snapsymbols
                                        (atoi (cond ((getenv "AutoSnapSize")) ("5")))
                                    )
                                )
                            )
                        )
                        (LM:OLE->ACI
                            (if (= 1 (getvar 'cvport))
                                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
                            )
                        )
                    )
                )
            )
           '(cond ((car q)) (p))
        )
    )
)
 
;; Object Snap for grread: Display Snap  -  Lee Mac
;; pnt - [lst] UCS point at which to display the symbol
;; lst - [lst] grvecs vector list
;; col - [int] ACI colour for displayed symbol
;; Returns nil
 
(defun LM:grsnap:displaysnap ( pnt lst col / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col lst)
        (list
            (list scl 0.0 0.0 (car  pnt))
            (list 0.0 scl 0.0 (cadr pnt))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)
 
;; Object Snap for grread: Snap Symbols  -  Lee Mac
;; p - [int] Size of snap symbol in pixels
;; Returns: [lst] List of vector lists describing each Object Snap symbol
 
(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
    (setq -p (- p) q (1+  p)
          -q (- q) r (+ 2 p)
          -r (- r) i (/ pi 6.0)
           a 0.0
    )
    (repeat 12
        (setq l (cons (list (* r (cos a)) (* r (sin a))) l)
              a (- a i)
        )
    )
    (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
    (list
        (list 1
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 2
            (list -r -q) (list 0  r) (list 0  r) (list r -q)
            (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
        )
        (cons 4 c)
        (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
        (list 16
            (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
            (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
            (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
        )
        (list 32
            (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
            (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
        )
        (list 64
            '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
            '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
            '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
            '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
        )
        (list 128
            (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
            (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
            (list -p q) (list -p -p) (list -p -p) (list q -p)
            (list -q q) (list -q -q) (list -q -q) (list q -q)
        )
        (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
        (list 512
            (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
            (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
        )
        (list 2048
            (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
            (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
            (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
            (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
    )
)
 
;; Object Snap for grread: Parse Point  -  Lee Mac
;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
;; str - [str] String representing point input
;; Returns: [lst] Point represented by the given string, else nil
 
(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
 
    (defun str->lst ( str / pos )
        (if (setq pos (vl-string-position 44 str))
            (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
            (list str)
        )
    )
 
    (if (wcmatch str "`@*")
        (setq str (substr str 2))
        (setq bpt '(0.0 0.0 0.0))
    )           
 
    (if
        (and
            (setq lst (mapcar 'distof (str->lst str)))
            (vl-every 'numberp lst)
            (< 1 (length lst) 4)
        )
        (mapcar '+ bpt lst)
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