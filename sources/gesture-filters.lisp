(in-package :om)

; some filtering functions
; I should make these filters recursive! And the default windowsize should be over the entire list? the statistics should do the recursion and windowsize, too


; Max's "slide filter" (IIR)

; this one doesn't have a 'windowsize' as it is a IIR filter.
(defmethod! slide-filter ((self list) (slide number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the filter function y (n) = y (n-1) + ((x (n) - y (n-1))/slide"
            (let ((lastsample (car self))
                  (thelist self))
                  ;(thelist (x-append (cdr self) (car (last self))))) ; to compensate for lag
              (loop for i from 1 to recursion do
                    (setf thelist (loop for sample in thelist collect
                                        (setf lastsample (+ lastsample (/ (- sample lastsample) slide))
                                        ))
                          ))
                    thelist))


(defmethod! slide-filter ((self bpf) (slide number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (slide-f(thelist (x-append (cdr self) (car (last self))))ilter (y-points self) slide recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

;probably not the proper way of doing this (with dimensionality >1)
(defmethod! slide-filter ((self bpc) (slide number) &optional (recursion 1))
            (let ((xpoints (slide-filter (x-points self) slide recursion))
                  (ypoints (slide-filter (y-points self) slide recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpc (decimals self))
              ))

(defmethod! slide-filter ((self 3DC) (slide number) &optional (recursion 1))
            (let ((xpoints (slide-filter (x-points self) slide recursion))
                  (ypoints (slide-filter (y-points self) slide recursion))
                  (zpoints (slide-filter (z-points self) slide recursion)))
              (3DC-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))


; I could have simple-moving-average, weighted-moving-average and exponential-moving-average filters

; simple-moving-average (FIR)

; this one is with hopsize - what can this be useful for?
(defmethod! sma2 ((self list) (windowsize number) (hopsize number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1 1 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((thelist self))
             
              (loop for i from 1 to recursion do
                    (let ;((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist))) ; padding - only if I need the same number of values
                        ((windowedlist thelist))
                      (setf thelist (loop for window in thelist while (> (length windowedlist) hopsize) collect
                                          (om-mean (first-n (setf windowedlist (last-n windowedlist (- (length windowedlist) hopsize))) windowsize))))
                      ))       
              thelist))

; 3 options: padding y/n, hop-size y/n, 

#| ;no padding
(defmethod! sma3 ((self list) (windowsize number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((thelist self))
             
                  (loop for i from 1 to recursion do
                        (let ((windowedlist thelist))
                          (setf thelist (loop for window in thelist while (> (length windowedlist) 1) collect
                                              (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize))))
                          ))
                  thelist))
|#

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

; moving average filters introduce lag which should be compensated for by shifting by half the window length
; I need to keep the same number of sample points because of the x-values in the bpf (time) - otherwise need to resample the bpf

(defmethod! sma ((self list) (windowsize number) &optional (recursion 1))
            :icon '(631)  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
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
                          (setf thelist (loop for window in thelist while (> (length windowedlist) 1) collect
                                              (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize))))
                          ))
                ; else it's a list of something else
                (setf thelist (mapcar (lambda (x) (sma x windowsize recursion)) thelist)))
                      thelist))


(defmethod! sma ((self bpf) (windowsize number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (sma (y-points self) windowsize recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! sma ((self 3dc) (windowsize number) &optional (recursion 1))
            (let ((xpoints (sma (x-points self) windowsize recursion))
                  (ypoints (sma (y-points self) windowsize recursion))
                  (zpoints (sma (z-points self) windowsize recursion)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))


; weighted moving average

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

; exponential moving average
(defmethod! ema ((self list) (alpha number) &optional (recursion 1))
            :icon '(631)  
            :initvals '(nil 0.1 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((lastsample (car self))
                  (thelist self))
              
              (loop for i from 1 to recursion do
                    
                    (setf thelist (loop for sample in thelist collect
                                        (setf lastsample (+ (* alpha sample) (* (- 1 alpha) lastsample)))
                                        ))
                    )
              thelist))

(defmethod! ema ((self bpf) (alpha number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (ema (y-points self) alpha recursion)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))              

(defmethod! ema ((self 3dc) (alpha number) &optional (recursion 1))
            (let ((xpoints (ema (x-points self) alpha recursion))
                  (ypoints (ema (y-points self) alpha recursion))
                  (zpoints (ema (z-points self) alpha recursion)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))


; exponential moving deviation   
(defmethod! emd ((self list) (alpha number))
            (ema (om-abs (om- self (ema self alpha))) alpha)
            )

; simple-moving-median
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

                
#|
(defmethod! slide-filter-rec ((self list) (slide number))
            (loop for sample in self collect
                  (+ (slide-filter-rec (- sample (/ lastsample slide) ))
                        )
                  ))
|#



                      
