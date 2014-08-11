(in-package :om)


; a selection of streams form an SDIF/GDIF file
(defclass! gesture-array ()
  ((datasrc :accessor datasrc :initarg :datasrc :initform nil)
   (streams :accessor streams :initarg :streams :initform nil)
   ;(fields :accessor fields :initarg :fields :initform nil)
   ;(rows :accessor rows :initarg :rows :initform nil)
   (timerange :accessor timerange :initarg :timerange :initform nil))
  (:icon 654))

;a gesture-stream represents a stream from the gesture array (SDIF) (a GDIF descriptor)
(defclass! gesture-stream ()
  ((timelist :accessor timelist :initarg :timelist :initform nil)
   (substreams :accessor substreams :initarg :substreams :initform nil)
   (sdif-info :accessor sdif-info :initarg :sdif-info :initform nil)
   ))

;a gesture substream represents a value (one DOF) in a matrix  
(defclass! gesture-substream ()
   ((valuelists :accessor valuelists :initarg :valuelists :initform nil)))

; I should use the same for the gesture model
(defmethod initialize-instance :after ((self gesture-array) &rest initargs) 
   (declare (ignore initargs)) 
   (when (datasrc self)
     (let* ((stream-preinfo (sdifinfo (datasrc self) nil))
            ;(stream-info (table-filter (lambda member something) (streams self) 0 pass)) ;this would be more elegant than the loop-in-a-loop
            (stream-info (flat (loop 
                                for tstr in (streams self) collect
                                (remove nil (loop
                                             for pstr in stream-preinfo collect
                                             (if (equal (car pstr) tstr) pstr)
                                             ))) 1)
                         ))
       (setf (streams self) (loop for str in stream-info collect
 
                                   (multiple-value-bind (data times) 
                                      ;             SDIFFILE       StreamID frametype  Matrixtype  field row1 row2   time1                    time2
                                      (getsdifdata (datasrc self) (car str) (cadr str) (caddr str) nil   nil  nil    (first (timerange self)) (second (timerange self)))
                                    
                                    ;(print (car str))
                                    ;(print (streams self))
                                    ;this is a gesture stream ------------------
                                    (make-instance 'gesture-stream
                                                   :sdif-info (list (car str) (cadr str) (caddr str))
                                                   :timelist times
                                                   :substreams (loop for sbstr in (mapcar #'mat-trans (mat-trans data)) collect
                                                                      (make-instance 'gesture-substream 
                                                                                    :valuelists sbstr)))
                                    ;this is a gesture stream ------------------
                                    )
                                   (print data)
)))
       ))
   self)


;(fields self) (first (rows self)) (second (rows self))

(defclass! gesture-model (time-array)
           (;(gesture-data :accessor gesture-data :initarg :gesture-data :initform nil)
            )
           (:icon 02)
           )

#|
(defmethod* objFromObjs ((self gesture-array) (type gesture-model))
            (print (times type))
            (when (times type)
              (segment-gstr self (times type))
              ))
|#



(defmethod initialize-instance :after ((self gesture-model) &rest initargs) 
   ;(declare (ignore initargs)) 
   ;(when (and (gesture-data self) (times self))
     ;(make-instance 'gesture-model :self (segment-gstr (gesture-data self) (times self)))
     ))


 
(defmethod segment-gstr ((self gesture-array) times)
  (let ((timearray (make-instance 'time-array :times times))  ; this was making a gesture-model directly.. but don't know why it didn't work
        (descriptors (loop for str in (streams self) collect
                           (second (sdif-info str))))
        (segment-data (loop for segment on times
                             while (cdr segment) collect
                             (let ((t1 (car segment))
                                   (t2 (cadr segment)))
                               (loop for str in (streams self) append
                                     (loop for substr in (substreams str) 
                                           collect (make-segmented-object (valuelists substr) (timelist str) t1 t2)
                                           ))
                               ))))
    ;(print segment-data)
    (cons-array timearray 
                (list nil (butlast times))
                (loop for row in (mat-trans segment-data)
                      for j = 0 then (+ j 1)
                      append (list ; in streams i can collect what is in :sdif-info (caddr (sdif-info stream))
                                    ;(format nil "Field ~D" j)
                                    (format nil (nth j descriptors))
                                   row)))
    ))


;datalists is a row of the matrix (instance) of lists (parameters)

; here I should add methods for chord-seq and audio etc.
(defmethod make-segmented-object (datalists timelist t1 t2 &optional (decimals 10))
  (let* ((pos1 (position t1 timelist :test '<))
        (pos2 (position t2 timelist :from-end t :test '>))
        (times (append (list t1) 
                       (range-filter timelist 
                                     (list (list pos1 pos2)) 'pass)
                       (list t2)))
        (data (mapcar #'(lambda (d) 
                          (append (list (x-transfer (mat-trans (list timelist d)) t1)) 
                                  (range-filter d (list (list pos1 pos2)) 'pass)
                                  (list (x-transfer (mat-trans (list timelist d)) t2))))
                               datalists))
        )
    ;(print (length datalists))
    (cond (
           (= (length datalists) 1)
           (simple-bpf-from-list times (first data) 'bpf decimals))
          ((= (length datalists) 2)
           (traject-from-list (first data) (second data) nil times '3D-trajectory decimals))
          ((= (length datalists) 3) 
           (traject-from-list (first data) (second data) (third data) times '3D-trajectory decimals))
           )
    ))

#|
(defmethod make-segmented-object (datalists timelist t1 t2 &optional (decimals 10))
  (let* ((pos1 (position t1 timelist :test '<))
        (pos2 (position t2 timelist :from-end t :test '>))
        (times (append (list t1) 
                       (range-filter timelist 
                                     (list (list pos1 pos2)) 'pass)
                       (list t2)))
        (data (mapcar #'(lambda (d) 
                          (append (list (x-transfer (mat-trans (list timelist d)) t1)) 
                                  (range-filter d (list (list pos1 pos2)) 'pass)
                                  (list (x-transfer (mat-trans (list timelist d)) t2))))
                               datalists))
        )
    (traject-from-list (first data) (second data) (third data) times '3D-trajectory decimals)
    ))
|#
                