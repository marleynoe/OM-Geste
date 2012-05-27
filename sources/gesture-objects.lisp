(in-package :om)

(defclass! gesture-array ()
  ((datasrc :accessor datasrc :initarg :datasrc :initform nil)
   (streams :accessor streams :initarg :streams :initform nil))
  (:icon 654))

(defclass! gesture-stream ()
  ((timelist :accessor timelist :initarg :timelist :initform nil)
   (substreams :accessor substreams :initarg :substreams :initform nil)
   (sdif-info :accessor sdif-info :initarg :sdif-info :initform nil)
   ))
  
(defclass! gesture-substream ()
   ((valuelists :accessor valuelists :initarg :valuelists :initform nil)))

(defmethod initialize-instance :after ((self gesture-array) &rest initargs) 
   (declare (ignore initargs)) 
   (when (datasrc self)
     (let ((stream-info (sdifinfo (datasrc self) nil)))
       (setf (streams self) (loop for str in stream-info collect
                                  (multiple-value-bind (data times) 
                                      (getsdifdata (datasrc self) (car str) (cadr str) (caddr str) nil nil nil nil nil)
                                    (make-instance 'gesture-stream
                                                   :sdif-info (list (car str) (cadr str) (caddr str))
                                                   :timelist times
                                                   :substreams (loop for sbstr in (mapcar #'mat-trans (mat-trans data)) collect
                                                                     (make-instance 'gesture-substream 
                                                                                    :valuelists sbstr)))
                                    )))
       ))
   self)

(defclass! gesture-model (time-array)
           ()
           (:icon 15)
           )

#|
(defmethod! objfromobjs ((self time-array) (type gesture-model))
            (let* ((
|#

 

(defmethod segment-gstr ((self gesture-array) times)
  (let ((timearray (make-instance 'time-array :times times))  ; this was making a gesture-model directly.. but don't know why it didn't work
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
                      append (list ;(intern "fifi") ; in streams i can collect what is in :sdif-info (third (sdif-info stream))
                                    (format nil "Field ~D" j)
                                   row)))
    ))

;datalists is a row of the matrix (instance) of lists (parameters)


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

                