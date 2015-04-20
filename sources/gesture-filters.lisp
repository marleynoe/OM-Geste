(in-package :om)

; some filtering functions
; I should make these filters recursive!

; Max's "slide filter"
(defmethod! slide-filter ((self list) (slide number))
            (let ((lastsample (car self)))             
              (loop for sample in self collect
                    (setf lastsample (+ lastsample (/ (- sample lastsample) slide)))
                    )
              ))

(defmethod! slide-filter ((self list) (slide number) &optional recursion)
            :icon 631  
            :initvals '(nil nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the filter function y (n) = y (n-1) + ((x (n) - y (n-1))/slide"
            (let ((lastsample (car self))
                  (thelist self))
              (loop for i from 1 to recursion do
                    (setf thelist (print (loop for sample in thelist collect
                                        (setf lastsample (+ lastsample (/ (- sample lastsample) slide)))
                                        ))
                          ))
                    thelist))

(defmethod! slide-filter ((self bpf) (windowsize number))
            (let ((xpoints (x-points self))
                  (ypoints (slide-filter (y-points self) windowsize)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

;probably not the proper way of doing this (with dimensionality >1)
(defmethod! slide-filter ((self bpc) (windowsize number))
            (let ((xpoints (slide-filter (x-points self) windowsize))
                  (ypoints (slide-filter (y-points self) windowsize)))
              (simple-bpf-from-list xpoints ypoints 'bpc (decimals self))
              ))

(defmethod! slide-filter ((self 3DC) (windowsize number))
            (let ((xpoints (slide-filter (x-points self) windowsize))
                  (ypoints (slide-filter (y-points self) windowsize))
                  (zpoints (slide-filter (z-points self) windowsize)))
              (3DC-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

; simple-moving-average 
(defmethod! sma ((self list) (windowsize number) &optional recursion)
            :icon 631  
            :initvals '(nil nil 1)
            :indoc '("a list, bpf, bpc, 3dc, 3d-trajectory or libs thereof" "a number" "a number")
            :numouts 1
            :doc "Implements the simple-moving-average: the average of a list of numbers in a sliding winwod."
            (let ((windowedlist (x-append (repeat-n (car self) windowsize) self))
                  (thelist self))
              (loop for i from 1 to recursion do
                    (setf thelist (loop for window in thelist collect
                                        (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize)))
                          ))
              thelist))

(defmethod! sma ((self bpf) (windowsize number))
            (let ((xpoints (x-points self))
                  (ypoints (sma (y-points self) windowsize)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! sma ((self 3dc) (windowsize number))
            (let ((xpoints (sma (x-points self) windowsize))
                  (ypoints (sma (y-points self) windowsize))
                  (zpoints (sma (z-points self) windowsize)))
              (3dc-from-list xpoints ypoints zpoints '3dc (decimals self))
              ))

; simple-moving-median
(defmethod! smm ((self list) (windowsize number))
            (let ((windowedlist (x-append (repeat-n (car self) windowsize) self)))    
           ;  (let ((windowedlist self)) 
              (loop for window in self collect
                    (nth (ceiling (* 0.5 windowsize))
                      (sort-list (first-n (setf windowedlist (cdr windowedlist)) windowsize))
                        ))
                  ))

(defmethod! smm ((self bpf) (windowsize number))
            (let ((xpoints (x-points self))
                  (ypoints (smm (y-points self) windowsize)))
              (simple-bpf-from-list xpoints ypoints 'bpf (decimals self))
              ))

(defmethod! smm ((self 3dc) (windowsize number))
            (let ((xpoints (smm (x-points self) windowsize))
                  (ypoints (smm (y-points self) windowsize))
                  (zpoints (smm (z-points self) windowsize)))
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
                      
