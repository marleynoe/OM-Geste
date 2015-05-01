;*********************************************************************
;                             OM-Geste                               *
;     (c) 2011-2015 Marlon Schumacher (CIRMMT/McGill University)     *
;               https://github.com/marleynoe/OM-Geste                *
;                                                                    *
;      Representation and Processing of Gesture Data in OpenMusic    *
;*********************************************************************
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: M. Schumacher

(in-package :om)


(defclass! gesture-model (time-array)
           ()
           (:icon 270)
           (:documentation "Gesture model is a tabulated representation of Gesture Streams.")
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

(defclass! gesture-init ()
           ((times :accessor times :initarg :times :initform nil))
           (:icon 02)
           (:documentation "Gesture model is a tabulated representation of Gesture Streams.")
           )

(defmethod initialize-instance :after ((self gesture-init) &rest initargs) 

   ;(declare (ignore initargs)) 
  (when (print sled))
    (print "bingo")
    (make-instance 'gesture-model :times (times self)))
     )

#|
(defmethod initialize-instance :after ((self gesture-init) &rest initargs) 
   ;(declare (ignore initargs)) 
  (when (print self)
    (print "bingo")
    (make-instance 'gesture-model :times (times self)))
     )
|#

|#




#|
(defmethod initialize-instance :after ((self gesture-model-test2) &rest initargs) 
  (declare (ignore initargs))
  ;(print "initializing")
  (print (times self))
   self)
|#