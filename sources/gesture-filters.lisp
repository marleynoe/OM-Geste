(in-package :om)

; some filtering functions

; Max's "slide filter"
(defmethod! slide-filter ((self list) (slide number))
            (print self)
            (let ((lastsample (car self)))
              (print self)
              (loop for sample in self collect
                    (setf lastsample (+ lastsample (/ (- sample lastsample) slide)))
                    )
              ))


; simple-moving-average
(defmethod! sma ((self list) (windowsize number))
            (let ((windowedlist (print (x-append (repeat-n (car self) windowsize) self))))    
           ;  (let ((windowedlist self)) 
              (loop for window in self collect
                      (om-mean (first-n (setf windowedlist (cdr windowedlist)) windowsize))
                        )
                  ))

; simple-moving-median
(defmethod! smm ((self list) (windowsize number))
            (let ((windowedlist (print (x-append (repeat-n (car self) windowsize) self))))    
           ;  (let ((windowedlist self)) 
              (loop for window in self collect
                    (nth (print (ceiling (* 0.5 windowsize)))
                      (sort-list (first-n (setf windowedlist (cdr windowedlist)) windowsize))
                        ))
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
                      
