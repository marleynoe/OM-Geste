(in-package :om)


; a selection of streams form an SDIF/GDIF file
(defclass! gesture-array ()
  ((datasrc :accessor datasrc :initarg :datasrc :initform nil)
   (streams :accessor streams :initarg :streams :initform nil)
   ;(fields :accessor fields :initarg :fields :initform nil)
   ;(rows :accessor rows :initarg :rows :initform nil)
   (timerange :accessor timerange :initarg :timerange :initform nil))
  (:icon 02))

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
                                      ;             SDIFFILE       StreamID Frametype  Matrixtype  field row1 row2   time1                    time2
                                      (getsdifdata (datasrc self) (car str) (cadr str) (caddr str) nil   nil  nil    (first (timerange self)) (second (timerange self)))

                                    ;this is a gesture stream ------------------
                                    ;(print (length (mat-trans data)))
                                    (make-instance 'gesture-stream
                                                                    ;StreamID Frametype  Matrixtype
                                                   :sdif-info (list (car str) (cadr str) (caddr str))
                                                   :timelist times
                                                   :substreams (loop for sbstr in (mapcar #'mat-trans (mat-trans data)) collect
                                                                      (make-instance 'gesture-substream 
                                                                                    :valuelists sbstr)))
                                    ;this is a gesture stream ------------------
                                    ))
             )
       ))
   self)


;(fields self) (first (rows self)) (second (rows self))

(defclass! gesture-model (time-array)
           (;(gesture-data :accessor gesture-data :initarg :gesture-data :initform nil)
            )
           (:icon 02)
           )


;Yes, this is absolutely required!
#|
(defmethod* objFromObjs ((self gesture-array) (type gesture-model))
            (print (times type))
            (when (times type)
              (segment-gstr self (times type))
              ))
|#


#|
(defmethod initialize-instance :after ((self gesture-model) &rest initargs) 
   (declare (ignore initargs)) 
   (when (and (gesture-data self) (times self))
     (make-instance 'gesture-model :self (segment-gstr (gesture-data self) (times self)))
     )
   self)
|#