(in-package :om)



; This file  contains filtering functions for low-/high-pass filtering for conditioning and other purposes



; %%%%%%%%%%%%%%%%%%%%%%
; MOVING AVERAGE FILTERS

; note: moving average filters introduce lag which should be compensated for by shifting by half the window length


; *************************************
; **** Simple Moving Average (sma) ****
         
(defmethod! sma ((self list) (windowsize number) &key (recursion 1) mode)
            :icon '(631)  
            :initvals '(nil 5 1 lowpass)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :menuins '((3 (( "lowpass" lowpass ) ("highpass" highpass)))) 
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((thelist self))

              (if (numberp (car thelist)) ; means it's a list of numbers
                  
                  (loop for i from 1 to recursion do
                        (let*  ((begin (ceiling (* 0.5 (1- windowsize)))) 
                                (end (floor (* 0.5 (1- windowsize))))
                                ;; this is padding to have the same number of data points as the orig list.
                                (windowedlist (x-append (repeat-n (car thelist) begin) thelist (repeat-n (car (last thelist)) end))))
                                ;(windowedlist (x-append (repeat-n (car thelist) windowsize) thelist)))
                                ;(windowedlist (x-append (car thelist) thelist)))
                          (setf thelist (if (eq mode 'highpass)
                                            (om- self (loop for window in thelist while (> (length windowedlist) 1) collect
                                                            (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize))))
                                          (loop for window in thelist while (> (length windowedlist) 1) collect
                                                (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize)))))
                         ; (setf thelist (if (print (eq mode 'highpass))
                         ;                   (om- self thelist)
                         ;                 thelist))
                          ))
                ; else it's a list of something else
                (setf thelist (mapcar (lambda (x) (sma x windowsize :recursion recursion :mode mode)) thelist)))
              thelist)
            )

(defmethod! sma ((self bpf) (windowsize number) &key (recursion 1) mode)
            (let ((xpoints (x-points self))
                  (ypoints (sma (y-points self) windowsize :recursion recursion :mode mode)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! sma ((self 3dc) (windowsize number) &key (recursion 1) mode)
            (let ((xpoints (sma (x-points self) :recursion recursion :mode mode))
                  (ypoints (sma (y-points self) :recursion recursion :mode mode))
                  (zpoints (sma (z-points self) :recursion recursion :mode mode)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! sma ((self 3d-trajectory) (windowsize number) &key (recursion 1) mode)
            (let ((xpoints (sma (x-points self) windowsize :recursion recursion :mode mode))
                  (ypoints (sma (y-points self) windowsize :recursion recursion :mode mode))
                  (zpoints (sma (z-points self) windowsize :recursion recursion :mode mode)))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))


; ***************************************
; **** Weighted Moving Average (wma) ****

; Uses linear weighting

(defmethod! wma ((self list) (windowsize number) &optional (recursion 1))
            :icon '(631)   
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Calculates the linear-weighted-moving-average of a list of numbers in a sliding window"
            (let ((thelist self))

               (if (numberp (car thelist)) 
             
                  (loop for i from 1 to recursion do
                        (let*  ((begin (ceiling (* 1 (1- windowsize)))) 
                                (end (floor (* 0.5 (1- windowsize))))
                                (windowedlist (x-append (repeat-n (car thelist) begin) thelist (repeat-n (car (last thelist)) end))))

                          (setf thelist (loop for window in thelist collect
                                              (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize) (om-scale/sum (arithm-ser 1 10 1) 1.0))))))

                  (setf thelist (mapcar (lambda (x) (wma x windowsize recursion)) thelist)))
                      thelist))

(defmethod! wma ((self bpf) (windowsize number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (wma (y-points self) windowsize recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! wma ((self 3dc) (windowsize number) &optional (recursion 1))
            (let ((xpoints (wma (x-points self) windowsize recursion))
                  (ypoints (wma (y-points self) windowsize recursion))
                  (zpoints (wma (z-points self) windowsize recursion)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! wma ((self 3d-trajectory) (windowsize number) &optional (recursion 1))
            (let ((xpoints (wma (x-points self) windowsize recursion))
                  (ypoints (wma (y-points self) windowsize recursion))
                  (zpoints (wma (z-points self) windowsize recursion)))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))




; ******************************************
; **** Exponential Moving Average (ema) ****
; Note,  this is an IIR filter (thus, now windowsize)

(defmethod! ema ((self list) (alpha number) &key (recursion 1) mode)
            :icon '(631)  
            :initvals '(nil 10 1 lowpass)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :menuins '((3 (( "lowpass" lowpass) ("highpass" highpass)))) 
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((lastsample (car self))
                  (thelist self)
                  (recialpha (/ 1 alpha)))

              (if (numberp (car thelist))
              
                  (loop for i from 1 to recursion do
                        (setf thelist (if (eq mode 'highpass)
                                          (om- self (loop for sample in thelist collect
                                                          (setf lastsample (+ (* recialpha sample) (* (- 1 recialpha) lastsample)))))
                                        (loop for sample in thelist collect
                                              (setf lastsample (+ (* recialpha sample) (* (- 1 recialpha) lastsample))))
                                        ))
                    )
                (setf thelist (mapcar (lambda (x) (ema x alpha :recursion recursion :mode mode)) thelist)))
              thelist))

(defmethod! ema ((self bpf) (alpha number) &key (recursion 1) mode)
            (let ((xpoints (x-points self))
                  (ypoints (ema (y-points self) alpha :recursion recursion :mode mode)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))              

(defmethod! ema ((self 3dc) (alpha number) &key (recursion 1) mode)
            (let ((xpoints (ema (x-points self) alpha :recursion recursion :mode mode))
                  (ypoints (ema (y-points self) alpha :recursion recursion :mode mode))
                  (zpoints (ema (z-points self) alpha :recursion recursion :mode mode)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! ema ((self 3dc) (alpha number) &key (recursion 1) mode)
            (let ((xpoints (ema (x-points self) alpha :recursion recursion :mode mode))
                  (ypoints (ema (y-points self) alpha :recursion recursion :mode mode))
                  (zpoints (ema (z-points self) alpha :recursion recursion :mode mode)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! ema ((self 3d-trajectory) (alpha number) &key (recursion 1) mode)
            (let ((xpoints (ema (x-points self) alpha :recursion recursion :mode mode))
                  (ypoints (ema (y-points self) alpha :recursion recursion :mode mode))
                  (zpoints (ema (z-points self) alpha :recursion recursion :mode mode)))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))


; %%%%%%%%%%%%%%%%%%%%%%
; MEDIAN FILTER

; ************************************
; **** Simple Moving Median (smm) ****

(defmethod! smm ((self list) (windowsize number) &optional (recursion 1))
            :icon '(631)  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements a simple-moving-median: the median of a list of numbers in a sliding window."
            (let ((thelist self)) 
              (loop for i from 1 to recursion do
                    (let ((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist)))
                      (setf thelist (loop for window in self collect
                                          (nth (ceiling (* 0.5 windowsize))
                                               (sort-list (first-n (setf windowedlist (cdr windowedlist)) windowsize))
                                               )))
                      ))
              thelist))

(defmethod! smm ((self bpf) (windowsize number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (smm (y-points self) windowsize recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! smm ((self 3dc) (windowsize number) &optional (recursion 1))
            (let ((xpoints (smm (x-points self) windowsize recursion))
                  (ypoints (smm (y-points self) windowsize recursion))
                  (zpoints (smm (z-points self) windowsize recursion)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

(defmethod! smm ((self 3d-trajectory) (windowsize number) &optional (recursion 1))
            (let ((xpoints (smm (x-points self) windowsize recursion))
                  (ypoints (smm (y-points self) windowsize recursion))
                  (zpoints (smm (z-points self) windowsize recursion)))
              (traject-from-list xpoints ypoints zpoints (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
              ))


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; INTERPOLATION / CURVE FITTING

; **********************************************
; **** B-Splines (Polynomial Interpolation) ****

(defmethod! b-spline ((self bpf) &key (order 3) resample mode)
            :icon '(631)  
            :initvals '(nil 3 100 lowpass)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "order of polynomial function (integer)" "defines resampling of curve (integer=points, decimal=factor")
            :numouts 1
            :menuins '((3 (( "lowpass" lowpass ) ("highpass" highpass)))) 
            :doc "Calculates a b-spline curve (piecewise polynomial function) of order <order> over the points in <self>. 

<resample> defines how to sample this curve: 
if <resample> is an integer it corresponds to number of samples 
if <resample> is a decimal it is a factor of the points in the original curve."
            (let ((thesplined
                   (cond ((integerp resample) (om-spline self resample order))
                         ((floatp resample) (om-spline self (round (* resample (length (point-pairs self)))) order))
                         (t (om-spline self (length (point-pairs self)) order))
                         )))
              (if (eq mode 'highpass)
                  (let* ((splinedtranspoints (mat-trans (point-pairs thesplined)))
                         (origtranspoints (mat-trans (point-pairs (om-sample self (length (first splinedtranspoints))))))
                         )
                    (simple-bpf-from-list (first splinedtranspoints) (om- (second origtranspoints) (second splinedtranspoints)) 'bpf (decimals self)))
                thesplined)
              ))


(defmethod! b-spline ((self list) &key (order 3) resample mode)
  (mapcar #'(lambda (theobject) (b-spline theobject :order order :resample resample :mode mode)) 
                    self))

; temporary method/trick to be able to have b-spline work with 3d-trajectories
(defmethod! b-spline ((self 3d-trajectory) &key (order 3) resample (mode 'lowpass))
            (let* (;(the3DC (3dc-from-list (x-points self) (y-points self) (print (z-points self)) '3dc (decimals self))) ; probbaly not required 
                   (numpoints (cond ((integerp resample) resample)
                                    ((floatp resample) (round (* resample (length (point-pairs self)))))
                                    (t (length (point-pairs self)))))
                   
                   (nutraject (clone self))
                   (thetimes (times nutraject))
                   (transpoints (mat-trans (point-pairs nutraject)))
                   (xbpf (b-spline (simple-bpf-from-list thetimes (first transpoints) 'bpf (decimals self)) :order order :resample numpoints :mode mode))
                   (ybpf (b-spline (simple-bpf-from-list thetimes (second transpoints) 'bpf (decimals self)):order order :resample numpoints :mode mode))
                   (zbpf (b-spline (simple-bpf-from-list thetimes (third transpoints) 'bpf (decimals self)) :order order :resample numpoints :mode mode))
                   
                   ;(numpoints (length (first thesplinedpoints)))
                   ;(nutimes (nth 2 (multiple-value-list (om-sample thetimes numpoints))))
                   ;(timedtraject (setf (times nutraject) nutimes)))
                   (timedtraject (traject-from-list (y-points xbpf) 
                                                    (y-points ybpf) 
                                                    (y-points zbpf) (x-points xbpf) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))))
                   timedtraject))


; exponential moving deviation   
(defmethod! emd ((self list) (alpha number))
            (ema (om-abs (om- self (ema self alpha))) alpha)
            )



#|
; the subseq is interesting.

(om::defmethod! low-pass  ((data list) (window number)) 
  :initvals '('(1 2 3 4 5 6)   100 )
  :indoc '("list of data"  "window size in samples data" )
  :icon '(213) 
  :numouts 1
  :doc   " traditional Low pass filter, where <list> is the data flow to filter and <window> 
is the parameter to calculate the window delay. The <window delay> will be (2*window + 1)"
  
  (om::x-append (om::first-n data (1- window))
                (loop for x in data
                      for i from window to (length data)
                      collect (om::om-mean (subseq data (- i window) i)))))
|#
                      
