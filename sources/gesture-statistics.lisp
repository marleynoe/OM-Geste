(in-package :om)
;---------------


;dot.jab is interesting! 
;dot.region can also be interesting!

(defmethod! differentiate ((self list) (order integer))
            :icon '(02) 
            :initvals '(nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "if t calculates sample standard deviation")
            :numouts 1
            :doc "Calculates the nth-order finite difference."           
            (let ((thelist self))
              (loop for i from 1 to order do
                    (let ((diff (x->dx thelist)))
                      (setf thelist (x-append (car diff) diff))
                      ))
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
              

;doesn't this need an optional 'start' value?
(defmethod! integrate ((self list) (order integer))
            (let ((thelist self))
              (loop for i from 1 to order do
                    (let ((diff (dx->x (car thelist) thelist)))
                      (setf thelist diff)
                      ))
              thelist))

(defmethod! integrate ((self list) (order integer))
            (let ((thelist (cdr self)))
              (loop for i from 1 to order do                 
                      (setf thelist (dx->x 0 thelist))
                      )
              thelist))

(defmethod! integrate ((self bpf) (order integer))
            (let ((thelist (cdr (y-points self)))) ; ???
              (loop for i from 1 to order do                 
                      (setf thelist (dx->x 0 thelist))
                      )
              (simple-bpf-from-list (x-points self) thelist 'bpf (decimals self))
              ))

(defmethod! integrate ((self bpc) (order integer))
            (let ((xlist (cdr (x-points self)))
                  (ylist (cdr (y-points self)))) ; ???
              (loop for i from 1 to order do                 
                      (setf xlist (dx->x 0 xlist))
                      (setf ylist (dx->x 0 ylist))
                      )
              (simple-bpf-from-list xlist ylist 'bpc (decimals self))
              ))

(defmethod! integrate ((self 3dc) (order integer))
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

(defmethod! integrate ((self 3d-trajectory) (order integer))
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

; root-mean-square
(defun rootmeansquare (self)
            (sqrt (om* (om/ 1 (length self)) (om-sum^2 self)))
            )


(defmethod! rms ((self list) &optional (windowsize nil) (hopsize 1) (padding 1)) ; padding 0 = no, 1 = first element, 2 = last element, 3 = circular
             :icon '(02) 
            :initvals '(nil 0)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "if t calculates sample standard deviation")
            :numouts 1
            :doc "calculates the absolute magnitude of an n-dimensional vector."     
            (let ((thelist self))
              (if (numberp windowsize)
                  (let ;((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist))) ; padding - only if I need the same number of values
                      ((windowedlist (x-append (car thelist) thelist)))
                    (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                        (rootmeansquare (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize))))
                    thelist)
                (rootmeansquare thelist)
                )))

(defmethod! rms ((self bpf) &optional (windowsize nil) (hopsize 1) (padding 1)) ; padding 0 = no, 1 = first element, 2 = last element, 3 = circular
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
              (simple-bpf-from-list thexpoints therootmeansquarelist 'bpf (decimals self))
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
            :icon '(02) 
            :initvals '(nil 0)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "if t calculates sample standard deviation")
            :numouts 1
            :doc "calculates the absolute magnitude of an n-dimensional vector."           
            (euclidean-distance self offset)
            )

(defmethod! magnitude ((self bpf) &optional (offset 0))              
            (simple-bpf-from-list (x-points self) (euclidean-distance (y-points self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self bpc) &optional (offset 0)) ;when times is not known use 'normalized' stepsize of 1
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3dc) &optional (offset 0))              
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3d-trajectory) &optional (offset 0))              
            (simple-bpf-from-list (times self) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

; summing function
(defmethod! om-sum ((self list))
                  (loop for item in self
                  sum item)
                  )

; squared sum
(defmethod! om-sum^2 ((self list))
                  (loop for item in self
                  sum (* item item))
                  )

; Need to make the average / arithmetic mean (SimpleArithmeticMean) for vectors.
; a defmethod! for lists-of-lists (each sublist being a vector)

(defun themean (list)
  (/ (om-sum list) (length list))
  )


; variance / sample variance
(defmethod! variance ((self list) &optional (bessel nil))
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
            :initvals '(nil nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "calculates sample standard deviation")
            :numouts 1
            :doc "calculates the standard deviation or sample standard deviation."
            (let ((thelist self))
              (if (numberp windowsize)
                  (let ((windowedlist (x-append (car thelist) thelist)))
                    (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                        (let ((thewindow (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize)))                     
                                          (standev thewindow bessel)
                                          ))))
                (setf thelist (standev thelist bessel))
                )
              thelist)
            )

(defmethod! stdev ((self bpf) &optional (bessel nil) (windowsize nil) (hopsize 1))
            (let ((xpoints (if (= hopsize 1) (x-points self) '(1)))
                  (ypoints (stdev (y-points self) bessel windowsize hopsize)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! stdev ((self 3dc) &optional (bessel nil) (windowsize nil) (hopsize 1))
            (let ((xpoints (stdev (x-points self) bessel windowsize hopsize))
                  (ypoints (stdev (y-points self) bessel windowsize hopsize))
                  (zpoints (stdev (z-points self) bessel windowsize hopsize)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))


(defmethod! dot-product ((firstlist list) (secondlist list))
            (om-sum (om* firstlist secondlist))
            )

; this is probably not correct.

#|
(defmethod! covariance ((self list))
  (sqrt (/ (om-sum^2 self) (length self)))
  )
|#

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

#|
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

; correlation

; It is obtained by dividing the covariance of the two variables by the product of their standard deviations

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

(defmethod! centroid ((tuples list))
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


(defmethod! find-peaks ((self list) (mode t) &key (numpeaks nil) (test '>) (decimals 10)) ; instead of numpeaks could be a slope threshold or similar
            :icon '(233)
            :indoc '("a bpf or point-list" "mode (Peak or Trough)" "Number of Peaks to find" "sorting function for result" "decimals for calculation" "delta step to determine peak or trough")
            :initvals '(((0 1) (5 10) (10 1)) peak nil > 10) ; no quote needed because it is already quoted
            :menuins '((1 (("peak" 'peak) ("trough" 'trough))) (3 ((">" >) ("<" <))))
            :doc "finds the n highest/lowest peaks or troughs in a bpf or point-list"
            (let* ((transpoints (mat-trans self))
                   (thederivativepoints (list (first transpoints) (differentiate (second transpoints) 1)))

                   (zerox (remove-duplicates (y-transfer (mat-trans thederivativepoints) 0.0 decimals))) ; give a list of "X" for zero crosses
                   ;(thecrossingfrequencies (x-transfer thedxpoints (om- thezerocrossings deltaparam) decimals))
                   (secondderivative (list (first transpoints) (differentiate (second thederivativepoints) 1)))
                   (signs (x-transfer (mat-trans secondderivative) zerox))
                   (peakpos nil)
                   (troughpos nil))
              ;find maxima and minima and write into peakpos and throughpos
              (loop for item in signs for x from 0 to (length signs) do
                    (unless (= item 0)
                      (if (< item 0) 
                          (push x peakpos)
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
                           (euc-distance (om^ (euclidean-distance (x-append (list (car peaksandtroughs)) peaksandtroughs) peaksandtroughs) 2)) ;maybe squared?
                           (summed-distances (loop for 1st in euc-distance
                                                   for 2nd in (cdr (x-append euc-distance (car (last euc-distance)))) collect
                                                   (+ 1st 2nd)))
                           (dummy (print (list (length peaksandtroughs) (length summed-distances) (length zerox))))
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
                troughpoints)
                  ))))
              

; peaks and troughs should always switch otherwise it's a mistake! 
              
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


