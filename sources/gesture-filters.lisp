(in-package :om)

; some filtering functions
; I should make these filters recursive! And the default windowsize should be over the entire list? the statistics should do the recursion and windowsize, too


; Max's "slide filter" (IIR)

(defmethod! slide-filter ((self list) (slide number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the filter function y (n) = y (n-1) + ((x (n) - y (n-1))/slide"
            (let ((lastsample (car self))
                  (thelist self))
              (loop for i from 1 to recursion do
                    (setf thelist (loop for sample in thelist collect
                                        (setf lastsample (+ lastsample (/ (- sample lastsample) slide))
                                        ))
                          ))
                    thelist))

(defmethod! slide-filter ((self bpf) (slide number) &optional (recursion 1))
            (let ((xpoints (x-points self))
                  (ypoints (slide-filter (y-points self) slide recursion)))
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



; simple-moving-average (FIR)
(defmethod! sma ((self list) (windowsize number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((thelist self))
             
                  (loop for i from 1 to recursion do
                        (let ((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist)))
                          (setf thelist (loop for window in thelist collect
                                              (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize))))
                          ))

                  thelist))

; this should be done for all the filter function 
(defmethod! sma ((self list) (windowsize number) &optional (recursion 1))
            :icon 631  
            :initvals '(nil 5 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the arithmetic mean of a list of numbers in a sliding window"
            (let ((thelist self))

              (if (numberp (car thelist)) ; means it's a list of numbers
               
                  (loop for i from 1 to recursion do
                        (let ((windowedlist (x-append (repeat-n (car thelist) windowsize) thelist)))
                          (setf thelist (loop for window in thelist collect
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

; for lists of objs (incl lists)

; simple-moving-median
(defmethod! smm ((self list) (windowsize number) &optional (recursion 1))
            :icon 631  
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

; write recursively?
                
#|
(defmethod! slide-filter-rec ((self list) (slide number))
            (loop for sample in self collect
                  (+ (slide-filter-rec (- sample (/ lastsample slide) ))
                        )
                  ))
|#


; exponential moving average
(defmethod! ema ((self list) (alpha number))
            (let ((lastsample (car self)))
            (loop for sample in self collect
                  (setf lastsample (+ (* alpha sample) (* (- 1 alpha) lastsample)))
                        )
                  ))
                   
; exponential moving deviation   
(defmethod! emd ((self list) (alpha number))
            (ema (om-abs (om- self (ema self alpha))) alpha)
            )
                      
