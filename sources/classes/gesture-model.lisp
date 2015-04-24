(in-package :om)


;(fields self) (first (rows self)) (second (rows self))

(defclass! gesture-model (time-array)
           (;(gesture-data :accessor gesture-data :initarg :gesture-data :initform nil)
            )
           (:icon 02)
           )

(defclass! gesture-bpf (bpf)
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



(defmethod initialize-instance :after ((self gesture-model) &rest initargs) 
   (declare (ignore initargs)) 
   ;(when (times self)
    ;(make-instance 'gesture-model :self (segment-gstr (gesture-data self) (times self)))
     ;)
   self)


