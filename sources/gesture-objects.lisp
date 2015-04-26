
(in-package :om)
 
; this function adds an extra component
(defmethod segment-gstr ((self gesture-array) times)
  (let ((descriptors (loop for str in (streams self) collect (second (sdif-info str))))
        (segment-data (print (loop for segment on times
                             while (cdr segment) collect
                             (let ((t1 (print (car segment)))
                                   (t2 (print (cadr segment))))
                               (loop for str in (streams self) append
                                     (loop for substr in (substreams str) 
                                           collect (make-segmented-object 
                                                    (valuelists substr) 
                                                    (mapcar #'(lambda (x) (coerce x 'single-float)) (timelist str))
                                                    t1 t2)
                                           ))
                               )))))
    
    (let ((timearray 
           (cons-array (make-instance 'time-array :times times) 
                       (list nil times)
                       (loop for row in (mat-trans segment-data)
                             for j = 0 then (+ j 1)
                             append (list (internk (nth j descriptors)) row)))))
      
      (set-data timearray)
      ; this function adds an extra component - need to remove it here again
      ;(remove-comp (car (last-n (get-components timearray) 2)))
      timearray
      )))


; I had an idea to have a more specialized class than simply a time-array...  it needs to be a time-array which segments audio! and score objects.
; Need to find a way to make temporal selections (extraction from the gesture model)
; a) means to re-segment the model and extract a component
; b) could have a function that merges the data together and then segments it and returns the object, for example. (A bit like the gesture-array - time range) 

;datalists is a row of the matrix (instance) of lists (parameters)

; here we choose to create objects solely based on the dimensionality of the data. This assumption is probably not always a good choice. 
; Instead, could be based on the matrix type inside the SDIF (implicit)? Or in an NVT an entry for "separable" or "integral"?
; Or a combination: First check for SDIF type, THEN check for dimensionality. Meaning: in the SDIF multiple fields (columns) are integral, rows are separable
; 
; here I should add methods for chord-seq (using the 'select' function), audio (using 'sound-cut') etc.

(defmethod make-segmented-object (datalists timelist t1 t2 &optional (decimals 10))
  (let ((pos1 (position t1 timelist :test '<))
        (pos2 (position t2 timelist :from-end t :test '>)))
    (when (and pos1 pos2)
      (let ((times (append (list t1) 
                           (range-filter timelist 
                                         (list (list pos1 pos2)) 'pass)
                           (list t2)))
            (data (mapcar #'(lambda (d) 
                               (append (list (x-transfer (mat-trans (list timelist d)) t1)) 
                                       (range-filter d (list (list pos1 pos2)) 'pass)
                                       (list (x-transfer (mat-trans (list timelist d)) t2))))
                           datalists)))
 
    ;(print (length datalists))
    (cond (
           (= (length datalists) 1)
           (simple-bpf-from-list times (first data) 'bpf decimals))
          ((= (length datalists) 2)
           (traject-from-list (first data) (second data) nil times '3D-trajectory decimals))
          ((= (length datalists) 3) 
           (traject-from-list (first data) (second data) (third data) times '3D-trajectory decimals))
          )
    ))))

