;*********************************************************************
;                             OM-Geste                               *
;     (c) 2011-2016 Marlon Schumacher (CIRMMT/McGill University)     *
;               https://github.com/marleynoe/OM-Geste                *
;                                                                    *
;      Representation and Processing of Gesture Data in OpenMusic    *
;*********************************************************************
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: M. Schumacher

(in-package :om)


; for now list method works only on scalars, not on vectors
(defmethod! differentiate ((self list) (order integer))
            :icon '(631) 
            :initvals '(nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "order of derivative")
            :numouts 1
            :doc "Calculates the nth-order first finite difference."           
            (let ((thelist self))
              (if (numberp (car self))
                  (loop for i from 1 to order do
                        (let ((diff (x->dx thelist)))
                          (setf thelist (x-append (car diff) diff))
                          ))
                (setf thelist (mapcar (lambda (x) (differentiate x order)) thelist)))
              thelist))

(defmethod! differentiate ((self bpf) (order integer))
            (let ((thelist (y-points self)))
              (loop for i from 1 to order do
                    (let ((diff (x->dx thelist)))
                      (setf thelist (x-append (car diff) diff))
                      ))
              (simple-bpf-from-list (x-points self) thelist 'bpf (decimals self))
              ))

(defmethod! differentiate ((self bpc) (order integer))
            (let ((ylist (y-points self))
                  (xlist (x-points self)))
              (loop for i from 1 to order do
                    (let ((xdiff (x->dx xlist))
                          (ydiff (x->dx ylist)))
                      (setf xlist (x-append (car xdiff) xdiff))
                      (setf ylist (x-append (car ydiff) ydiff))
                      ))
              (simple-bpf-from-list xlist ylist 'bpc (decimals self))
              ))

(defmethod! differentiate ((self 3dc) (order integer))
            (let ((ylist (y-points self))
                  (xlist (x-points self))
                  (zlist (z-points self)))
              (loop for i from 1 to order do
                    (let ((xdiff (x->dx xlist))
                          (ydiff (x->dx ylist))
                          (zdiff (x->dx zlist)))
                      (setf xlist (x-append (car xdiff) xdiff))
                      (setf ylist (x-append (car ydiff) ydiff))
                      (setf zlist (x-append (car zdiff) zdiff))
                      ))
              (3dc-from-list xlist ylist zlist '3dc (decimals self))
              ))

(defmethod! differentiate ((self 3d-trajectory) (order integer))
            (let ((ylist (y-points self))
                  (xlist (x-points self))
                  (zlist (z-points self)))
              (loop for i from 1 to order do
                    (let ((xdiff (x->dx xlist))
                          (ydiff (x->dx ylist))
                          (zdiff (x->dx zlist)))
                      (setf xlist (x-append (car xdiff) xdiff))
                      (setf ylist (x-append (car ydiff) ydiff))
                      (setf zlist (x-append (car zdiff) zdiff))
                      ))
              (traject-from-list xlist ylist zlist (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))
              

; ******* INTEGRATION *********
;this needs an optional start-value - if nil use (car thelist)
(defmethod! integrate ((self list) (order integer) &optional startval)
            :icon '(631) 
            :initvals '(nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "order of integration")
            :numouts 1
            :doc "Integrates values in <self>"    
            (let ((thelist self))
              (if (numberp (car self))
                  (loop for i from 1 to order do
                        (let ((diff (dx->x (or startval (car thelist)) thelist)))
                          (setf thelist diff)
                          ))
                (setf thelist (mapcar (lambda (x) (integrate x order startval)) thelist)))
              thelist))

(defmethod! integrate ((self bpf) (order integer) &optional startval)          
            (simple-bpf-from-list (x-points self) (integrate (cdr (y-points self)) order startval) 'bpf (decimals self))       
            )

(defmethod! integrate ((self bpc) (order integer) &optional startval)
            (let ((xlist (cdr (x-points self)))
                  (ylist (cdr (y-points self)))) ; ???
              (loop for i from 1 to order do                 
                      (setf xlist (dx->x 0 xlist))
                      (setf ylist (dx->x 0 ylist))
                      )
              (simple-bpf-from-list xlist ylist 'bpc (decimals self))
              ))

(defmethod! integrate ((self 3dc) (order integer) &optional startval)
            (let ((xlist (cdr (x-points self)))
                  (ylist (cdr (y-points self)))
                  (zlist (cdr (z-points self))))
              (loop for i from 1 to order do                 
                      (setf xlist (dx->x 0 xlist))
                      (setf ylist (dx->x 0 ylist))
                      (setf zlist (dx->x 0 zlist))
                      )
              (3dc-from-list xlist ylist zlist '3dc (decimals self))
              ))

(defmethod! integrate ((self 3d-trajectory) (order integer) &optional startval)
            (let ((xlist (cdr (x-points self)))
                  (ylist (cdr (y-points self)))
                  (zlist (cdr (z-points self))))
              (loop for i from 1 to order do                 
                      (setf xlist (dx->x 0 xlist))
                      (setf ylist (dx->x 0 ylist))
                      (setf zlist (dx->x 0 zlist))
                      )
              (traject-from-list xlist ylist zlist (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))

; ----------------------------- 

; function to determine the extremas in a curve (= wendepunkte)

(defmethod! extrema ((self bpf))
            :icon '(631) 
            :initvals '(nil)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof")
            :numouts 1
            :doc "Finds extrema in <self>"  
            (let ((thexvals (y-transfer (differentiate self 1) 0)))
              (simple-bpf-from-list thexvals (x-transfer self thexvals) 'bpf (decimals self))
              )
            )

; root-mean-square
(defun rootmeansquare (self)
            (sqrt (om* (om/ 1 (length self)) (om-sum^2 self)))
            )

(defmethod! rms ((self list) &optional (windowsize nil) (hopsize 1) (padding 1)) ; padding 0 = no, 1 = first element, 2 = last element, 3 = circular
            :icon '(631) 
            :initvals '(nil 5)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "windowsize in samples" "hopsize" "padding")
            :numouts 1
            :doc "calculates the absolute magnitude of an n-dimensional vector."     
            (let ((thelist self))
              (if (numberp (car thelist))
                  (if (numberp windowsize)
                      (let ;((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist))) ; padding - only if I need the same number of values
                          ((windowedlist (x-append (car thelist) thelist)))
                        (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                            (rootmeansquare (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize))))
                        thelist)
                    (rootmeansquare thelist))
                (mapcar (lambda (x) (rms x windowsize hopsize padding)) thelist))
                    ))

(defmethod! rms ((self bpf) &optional (windowsize nil) (hopsize 1) (padding 1)) ; padding 0 = no, 1 = first element, 2 = last element, 3 = circular
            ;(print "here")
            (let* ((thexpoints (if (= hopsize 1) (x-points self) '(1)))
                   (thelist (y-points self))
                   (therootmeansquarelist (if (numberp windowsize)
                                              (let ;((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist))) ; padding - only if I need the same number of values
                                                  ((windowedlist (x-append (car thelist) thelist)))
                                                (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                                                    (rootmeansquare (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize))))
                                                thelist)
                                            (rootmeansquare thelist))
                                          ))
              (if (numberp windowsize)
                  (simple-bpf-from-list thexpoints therootmeansquarelist 'bpf (decimals self))
                therootmeansquarelist)
              ))

(defmethod! rms ((self 3dc) &optional (windowsize nil) (hopsize 1) (padding 1))
            (let ((xpoints (rms (x-points self) windowsize hopsize padding))
                  (ypoints (rms (y-points self) windowsize hopsize padding))
                  (zpoints (rms (z-points self) windowsize hopsize padding)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! rms ((self 3d-trajectory) &optional (windowsize nil) (hopsize 1) (padding 1))
            (let ((xpoints (list! (rms (x-points self) windowsize hopsize padding)))
                  (ypoints (list! (rms (y-points self) windowsize hopsize padding)))
                  (zpoints (list! (rms (z-points self) windowsize hopsize padding))))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))


; square function
(defun ^2 (n) (* n n))


(defun euc-distance (vector1 vector2)
  (let ((accum 0.0))
    (mapc #'(lambda (x y) (incf accum (^2 (- x y)))) (list! vector1) (list! vector2))
    (sqrt accum)))


; this assumes that items are lists
(defmethod! euclidean-distance ((variable1 list) (variable2 list))
            (mapcar (lambda (x y) (euc-distance x y )) variable1 variable2)
            )

(defmethod! euclidean-distance ((variable1 list) (variable2 number))
            (mapcar (lambda (x) (euc-distance x (repeat-n variable2 (length (list! (car variable1)))) )) variable1)
            )


; magnitude vector
(defmethod! magnitude ((self list) &optional (offset 0))
            :icon '(631) 
            :initvals '(nil 0)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "defines the origin (default: 0)")
            :numouts 1
            :doc "calculates the absolute magnitude of an n-dimensional vector."
            (if (numberp (car (list! (car self)))) 
                (euclidean-distance self offset)
              (flat (mapcar (lambda (x) (magnitude x offset)) self))
            ))

(defmethod! magnitude ((self bpf) &optional (offset 0))              
            (simple-bpf-from-list (x-points self) (euclidean-distance (y-points self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self bpc) &optional (offset 0)) ;when times is not known use 'normalized' stepsize of 1
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) offset) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3dc) &optional (offset 0))              
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) offset) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3d-trajectory) &optional (offset 0))
            (if (> (length (point-pairs self)) 1)
                (simple-bpf-from-list (times self) (euclidean-distance (point-pairs self) offset) 'bpf (decimals self))
              (euclidean-distance (point-pairs self) offset))
            )

;get minmax
(defmethod! minmax ((self list))
  :initvals '(nil)
  :icon '(209) 
  :indoc '("list")
  :doc "Returns min and max values in list or bpf."
  :numouts 2
  (values (list-min self) (list-max self))
  )

(defmethod! minmax ((self bpf))
            (let ((ypoints (y-points self)))
              (values (list-min ypoints) (list-max ypoints))
              ))

; summing function
(defmethod! om-sum ((self list))
  :initvals '(nil)
  :icon '(209) 
  :indoc '("list or bpf or 3DC or 3D-trajectory")
  :doc "Returns the sum of elements in list."
  (if (numberp (car self))
      (loop for item in self
            sum item)
    (mapcar (lambda (x) (om-sum x)) self)
    ))

(defmethod! om-sum ((self bpf))
            (om-sum (y-points self))
            )

(defmethod! om-sum ((self 3dc))
            (om-sum (y-points (magnitude self)))
            )

(defmethod! om-sum ((self 3d-trajectory))
            (om-sum (y-points (magnitude self)))
            )


; squared sum
(defmethod! om-sum^2 ((self list))
            :initvals '(nil)
            :icon '(209) 
            :indoc '("list")
            :doc "Returns the squared sum of elements in list."
            (loop for item in self
                  sum (* item item))
            )

(defmethod! om-sum^2 ((self bpf))
            (om-sum^2 (y-points self))
            )

(defmethod! om-sum^2 ((self 3dc))
            (om-sum^2 (y-points (magnitude self)))
            )

(defmethod! om-sum^2 ((self 3d-trajectory))
            (om-sum^2 (y-points (magnitude self)))
            )

; Need to make the average / arithmetic mean (SimpleArithmeticMean) for vectors.
; a defmethod! for lists-of-lists (each sublist being a vector)

(defmethod! arith-mean ((self list))
            :icon '(631)
            :initvals '(nil nil nil 1)
            :indoc '("a list, bpf, 3dc, 3d-trajectory")
            :numouts 1
            :doc "calculates the average"            
            (/ (om-sum self) (length self))
            )

(defmethod! arith-mean ((self bpf))
            (arith-mean (y-points self))
            )

;compat
(defmethod! mean (self)
            (arith-mean self)
            )

; variance / sample variance
(defmethod! variance ((self list) &optional (bessel nil))
            :icon '(631)
            :initvals '(nil nil)
            :indoc '("a list" "Bessel's correction")
            :numouts 1
            :doc "Calculates the variance of a series of samples."
            (if (and bessel (> (length self) 2))
                (/ (om-sum^2 (om- self (mean self))) (- (length self) 1))
              (/ (om-sum^2 (om- self (mean self))) (length self))
            ))


; standard deviation / sample standard deviation
(defun standev (self bessel)
  (if (and bessel (> (length self) 2)) ;prevent division by zero
      (sqrt (/ (om-sum^2 (om- self (mean self))) (- (length self) 1)))
    (sqrt (/ (om-sum^2 (om- self (mean self))) (length self))))
  )


(defmethod! stdev ((self list) &optional (bessel nil) (windowsize nil) (hopsize 1))
            :icon '(631)
            :initvals '(nil nil nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "Bessel's correction" "windowsize in samples" "hopsize in samples")
            :numouts 1
            :doc "calculates the standard deviation or sample standard deviation."
            (let ((thelist self))
              (if (numberp (car thelist))
                  (if (numberp windowsize)
                      (let ((windowedlist (x-append (car thelist) thelist)))
                        (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                            (let ((thewindow (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize)))                     
                                              (standev thewindow bessel)
                                              ))))
                    (setf thelist (standev thelist bessel))
                    )
                (setf thelist (mapcar (lambda (vals) (stdev vals bessel windowsize hopsize)) thelist)))
              thelist)
            )

(defmethod! stdev ((self bpf) &optional (bessel nil) (windowsize nil) (hopsize 1))
            (let ((xpoints (if (= hopsize 1) (x-points self) '(1)))
                  (ypoints (stdev (y-points self) bessel windowsize hopsize)))
              (if windowsize
                  (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
                ypoints)
              ))

(defmethod! stdev ((self 3dc) &optional (bessel nil) (windowsize nil) (hopsize 1))
            (let ((xpoints (stdev (x-points self) bessel windowsize hopsize))
                  (ypoints (stdev (y-points self) bessel windowsize hopsize))
                  (zpoints (stdev (z-points self) bessel windowsize hopsize)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! stdev ((self 3d-trajectory) &optional (bessel nil) (windowsize nil) (hopsize 1))
            (let ((xpoints (list! (stdev (x-points self) bessel windowsize hopsize)))
                  (ypoints (list! (stdev (y-points self) bessel windowsize hopsize)))
                  (zpoints (list! (stdev (z-points self) bessel windowsize hopsize))))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))


(defmethod! dot-product ((firstlist list) (secondlist list))
            (om-sum (om* firstlist secondlist))
            )




;dot.jab is interesting! 
;dot.region can also be interesting!

;; **** NOT IMPLEMENTED YET *****
;; Covariance and Correlation

#|
(defmethod! covariance ((self list))
  (sqrt (/ (om-sum^2 self) (length self)))
  )


; it's the mean of the product of the two vectors and subtracting from this the product of the means
(defmethod! covariance ((variable1 list) (variable2 list) (windowsize number)) ;if nothing provided for 'windowsize' take the entire list
            (let ((windowedlist1 (x-append (repeat-n (car variable1) windowsize) variable1))
                  (windowedlist2 (x-append (repeat-n (car variable2) windowsize) variable2)))   
              ;shall I uappend the last element instead?
                ;((windowedlist1 (print (x-append variable1 (repeat-n (car (reverse variable1)) windowsize))))
                ; (windowedlist2 (print (x-append variable2 (repeat-n (car (reverse variable2)) windowsize)))))
                 ;((windowedlist1 variable1)
                  ;(windowedlist2 variable2))
              (loop for window1 in variable1
                    for window2 in variable2 collect
                    ;(progn
                      ;(setf windowedlist1 (cdr windowedlist1))
                      ;(setf windowedlist2 (cdr windowedlist2))
                    (let ((thewindow1 (first-n (setf windowedlist1 (cdr windowedlist1)) windowsize))
                          (thewindow2 (first-n (setf windowedlist2 (cdr windowedlist2)) windowsize)))
                    (om- (om-mean (om* thewindow1 thewindow2)) (* (om-mean thewindow1) (om-mean thewindow2)))
                    ;(om-mean (om* (om- thewindow1 (om-mean thewindow1)) (om- thewindow2 (om-mean thewindow2))))
                    ))
              )
            )


(defmethod! covariance ((variable1 list) (variable2 list) (windowsize number)) ;if nothing provided for 'windowsize' take the entire list
            (let ((windowedlist1 (print (x-append (repeat-n (car variable1) windowsize) variable1)))
                  (windowedlist2 (print (x-append (repeat-n (car variable2) windowsize) variable2))))    
                
           ;  (let ((windowedlist self)) 
              (loop for window1 in variable1
                    for window2 in variable2 collect
                    (om-round (om- (om-mean (om* windowedlist1 windowedlist2)) (om* (om-mean windowedlist1) (om-mean windowedlist2))) 5)
                    )
  ))
|#

; ***** Correlation ****
; It is obtained by dividing the covariance of the two variables by the product of their standard deviations

#|

;no windowing  
(defmethod! correlation ((variable1 list) (variable2 list) (windowsize number))
            (om/ (covariance variable1 variable2 windowsize) (* (sstdev variable1) (sstdev variable2)))
            )
;----

(defmethod! correlation ((variable1 list) (variable2 list) (windowsize number))
            (let* ((windowedlist1 (x-append (repeat-n (car variable1) windowsize) variable1))
                   (windowedlist2 (x-append (repeat-n (car variable2) windowsize) variable2))
                   (thecovariance (covariance variable1 variable2 windowsize))
                   (thestdev 
                   (loop for window1 in variable1
                         for window2 in variable2 collect
                         (let ((thewindow1 (first-n (setf windowedlist1 (cdr windowedlist1)) windowsize))
                               (thewindow2 (first-n (setf windowedlist2 (cdr windowedlist2)) windowsize)))
                           (* (om-mean thewindow1) (om-mean thewindow2))))
                   ))
              (om/ thecovariance thestdev)
              )
            )

|#

(defmethod! centroid ((tuples list))
            :icon '(631)
            :initvals '(nil nil 1)
            :indoc '("a list")
            :numouts 1
            :doc "Calculates the center-of-gravity (centroid) of a series of samples."
            (let* ((translist (mat-trans tuples))
                   (xpoints (first translist))
                   (ypoints (om-abs (second translist))))
              (/
               (loop for x in xpoints 
                     for y in ypoints
                     finally
                     sum (* x y)
                     )
               (+ (om-sum ypoints) 0.000001) ;avoid division by zero
               )))

(defmethod! centroid ((self bpf))
            (centroid (point-pairs self))
            )

; ****** PEAK FINDING FUNCTION *****

(defmethod! find-peaks ((self list) (mode t) &key (numpeaks nil) (test '>) (decimals 10)) ; instead of numpeaks could be a slope threshold or similar
            :icon '(233)
            :indoc '("a bpf or point-list" "mode (Peak or Trough)" "Number of peaks to find" "Sorting function for result" "Decimals for calculation")
            :initvals '(((0 1) (5 10) (10 1)) peak nil > 10) ; no quote needed because it is already quoted
            :menuins '((1 (("peak" 'peak) ("trough" 'trough))) (3 ((">" >) ("<" <))))
            :doc "Attempts to find the n highest/lowest peaks or troughs in a bpf or point-list."
            (if (listp (car self))
                (let* ((transpoints (mat-trans self))
                       (thederivativepoints (list (first transpoints) (differentiate (second transpoints) 1)))
                       (zerox (remove-duplicates (y-transfer (mat-trans thederivativepoints) 0.0 decimals))) 
                       (secondderivative (list (first transpoints) (differentiate (second thederivativepoints) 1)))
                       (signs (x-transfer (mat-trans secondderivative) zerox))
                       (peakpos nil)
                       (troughpos nil))
              ;find maxima and minima and write into peakpos and troughpos
                  (loop for item in signs for x from 0 to (length signs) do ; also, this structure? for j = 0 then (+ j 1)
                        (unless (= item 0)
                          (if (< item 0) 
                              (push x peakpos)   ;probably better use append instead of push
                            (push x troughpos)))
                        finally (progn
                                  (setf peakpos (reverse peakpos))
                                  (setf troughpos (reverse troughpos))))
              ;look up x-values for peakpos and throughpos and write into x-peaks and x-troughs, respectively
                  (let* ((x-peaks (posn-match zerox peakpos))
                         (x-troughs (posn-match zerox troughpos))
                         (peakpoints (mat-trans (list x-peaks (x-transfer self x-peaks))))
                         (troughpoints (mat-trans (list x-troughs (x-transfer self x-troughs))))
                         )
                    (if numpeaks
                        (let* ((peaksandtroughs (sort-list (x-append peakpoints troughpoints) :test '< :key 'first))
                               (euc-distance (om^ (euclidean-distance (x-append (list (car peaksandtroughs)) peaksandtroughs) peaksandtroughs) 2)) ;squared distance
                               (summed-distances (loop for 1st in euc-distance
                                                       for 2nd in (cdr (x-append euc-distance (car (last euc-distance)))) collect
                                                       (+ 1st 2nd)))
                               (dummy (list (length peaksandtroughs) (length summed-distances) (length zerox)))
                               (points-and-distance (loop for pat in peaksandtroughs 
                                                          for sdt in summed-distances collect
                                                          (x-append sdt pat)
                                                          )))
                          (if (equal mode 'peak)
                              (let* ((thepeaks (sort-list (posn-match points-and-distance peakpos) :test test :key 'first)) ; change 'test'
                                     (cleanpeaks (loop for point in thepeaks collect
                                                       (last-n point 2)
                                                       )))
                                (first-n cleanpeaks numpeaks))
                                 ;cleanpeaks)
                            
                            (let* ((thetroughs (sort-list (posn-match points-and-distance troughpos) :test test :key 'first))
                                   (cleantroughs (loop for point in thetroughs collect
                                                       (last-n point 2)
                                                       )))
                              (first-n cleantroughs numpeaks)))
                          )
                      (if (equal mode 'peak)
                          peakpoints
                        troughpoints))))
              (mapcar (lambda (x) (find-peaks x mode :numpeaks numpeaks :test test :decimals decimals)) self))
            )
              

              
(defmethod! find-peaks ((self bpf) (mode t) &key (numpeaks nil) (test '>) (decimals 10))
            (let ((point-list (mat-trans (find-peaks (point-pairs self) mode :numpeaks numpeaks :test test :decimals decimals))))
              (simple-bpf-from-list (first point-list) (second point-list) 'bpf decimals)
              ))


(defun min-x-const-q-distance (list distance-factor)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first a) (first b)) (* (first a) distance-factor))
                                 )
                       ))

(defun min-x-distance (list distance)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first a) (first b)) distance)
                                 )
                       ))

(defun min-x-distance-reverse (list distance)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first b) (first a)) distance)
                                 )
                       ))


