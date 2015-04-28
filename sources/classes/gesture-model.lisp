(in-package :om)


;(fields self) (first (rows self)) (second (rows self))

(defclass! gesture-model (time-array)
           ()
           (:icon 02)
           )

(defclass! gesture-model-test2 (time-array)
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
            ;(when (print (times type))
            ;(print "here")
             ; (let ((thetimearray (print (segment-gstr self (print (times type)))))
              ;      (thegesture-model (make-instance 'gesture-model)))
                ;(make-instance 'gesture-model :self thetimearray))
               ; (objFromObjs thetimearray thegesture-model))
            (print (segment-gstr self '(20 21 22 23)))
              )
|#

#|
(defmethod* objFromObjs ((self gesture-model) (type gesture-array))       
            ;(when (print (times type))
            (print "here")
              ;(let ((thetimearray (print (segment-gstr self (print (times type)))))
                  ;  (thegesture-model (make-instance 'gesture-model)))
                ;(make-instance 'gesture-model :self thetimearray))
               ; (objFromObjs thetimearray thegesture-model))
              )
|#


#|
(defmethod initialize-instance :after ((self gesture-model) &rest initargs) 
#|
   ;(declare (ignore initargs)) 
   (when (print (times self))
    (make-instance 'gesture-model :self (segment-gstr (gesture-data self) (times self)))
     )
|#
   self)
|#

; check how gesture-array is doing this...

(defmethod initialize-instance :after ((self gesture-model-test2) &rest initargs) 
  (declare (ignore initargs))
  ;(print "initializing")
  (print (times self))
   self)
