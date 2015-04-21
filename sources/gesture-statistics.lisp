(in-package :om)
;---------------


;dot.jab is interesting! 
;dot.region can also be interesting!

#|
(defmethod! differentiate ((self list) (order number))
            (let* ((thelist (x-append (car self) self))
                   (1stdiff (x->dx thelist)))
              (loop for i from 1 to order do
                    (setf thelist (x->dx thelist))
                    )
              thelist))
|#

(defmethod! differentiate ((self list) (order number))
            (let ((thelist self))
              (loop for i from 1 to order do
                    (let ((diff (x->dx thelist)))
                      (setf thelist (x-append (car diff) diff))
                      ))
              thelist))
              

(defmethod! integrate ((self list) (order number))
            (let ((thelist self))
              (loop for i from 1 to order do
                    (let ((diff (dx->x (car thelist) thelist)))
                      (setf thelist diff)
                      ))
              thelist))

(defmethod! integrate ((self list) (order number))
            (let ((thelist (cdr self)))
              (loop for i from 1 to order do
                    ;(let ((diff (dx->x (car thelist) thelist)))
                      (setf thelist (dx->x 0 thelist))
                      )
              thelist))

; euclidean distance
(defun ^2 (n) (* n n))

; not sure whether the 
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
; maybe this should explicitly state that if the second list is nil it becomes "0"?
(defmethod! magnitude ((self list))
            :icon '(02) 
            :initvals '(nil)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "if t calculates sample standard deviation")
            :numouts 1
            :doc "calculates the standard deviation or sample standard deviation."
            (
            (euclidean-distance self 0)
            )

(defmethod! magnitude ((self bpf))              
            (simple-bpf-from-list (x-points self) (euclidean-distance (y-points self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self bpc))              
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3dc))              
            (simple-bpf-from-list '(1) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

(defmethod! magnitude ((self 3d-trajectory))              
            (simple-bpf-from-list (times self) (euclidean-distance (point-pairs self) 0) 'bpf (decimals self))
            )

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

(defun mean (list)
  (/ (om-sum list) (length list))
  )


;could distinguish these 2 cases with an optional input menu for "Bessel" or "uncorrected"

(defmethod! variance ((self list))
            (/ (om-sum^2 (om- self (mean self))) (length self))
            )

(defmethod! svariance ((self list))
            (/ (om-sum^2 (om- self (mean self))) (- (length self) 1))
            )

; standard deviation / sample standard deviation
(defmethod! stdev ((self list) &optional (Bessel nil))
            :initvals '(nil t)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "if t calculates sample standard deviation")
            :numouts 1
            :doc "calculates the standard deviation or sample standard deviation."
            (if Bessel
                (sqrt (/ (om-sum^2 (om- self (mean self))) (- (length self) 1)))
            (sqrt (/ (om-sum^2 (om- self (mean self))) (length self)))
            ))





;*** sine/cosine functions for lists

(defmethod! om-sin ((arg1 number))  
  :initvals '(nil)
  :icon '(209) 
  :indoc '("list")
  :doc "Sine for every item in list."
  (sin arg1))

(defmethod! om-sin ((arg1 list))   
  (mapcar #'(lambda (input)
              (sin input)) arg1))

;*** cosine function for lists

(defmethod! om-cos ((arg1 number))  
  :initvals '(nil) 
  :icon '(209) 
  :indoc '("list")
  :doc "Cosine for every item in list."
  (cos arg1))

(defmethod! om-cos ((arg1 list))   
  (mapcar #'(lambda (input)
              (cos input)) arg1))

(defmethod! om-scale-exp ((self t) (minout number) (maxout number) (exponent number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 1 1) 
  :indoc '("number or list"  "a number" "a number" "an exponent")
  :icon '(209)
  :doc 
  "Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.

Ex. (om-scale 5 0 100 0 10)  => 50
Ex. (om-scale '(0 2 5) 0 100 0 10)  => (0 20 50)
Ex. (om-scale '(0 2 5) 0 100)  => (0 40 100)
 "
  (om-scale (om^ (om-scale self 0. 1. minin maxin) exponent) minout maxout 0. 1.)
  )

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

(defmethod! centroid ((freqs list) (amps list))
           (/
            (loop for x in freqs 
                  for y in amps
                  finally
                  sum (* x y)
                  )
            (+ (om-sum amps) 0.000001) ;avoid division by zero
           ))

#|
(defmethod! rms ((amps list) (windowsize number))
            (sqrt (mean (om-sum^2 amps)))
           )
|#