; Utilities functions for Om-Geste

(in-package :om)
;---------------

;*** sine/cosine functions for lists

(defmethod! om-sin ((arg1 number))  
  :initvals '(nil)
  :icon '(209) 
  :indoc '("list")
  :doc "Sine for every item in list."
  (sin arg1))

(defmethod! om-sin ((arg1 list))   
  (mapcar #'(lambda (input)
              (sin input)) arg1))

;*** cosine function for lists

(defmethod! om-cos ((arg1 number))  
  :initvals '(nil) 
  :icon '(209) 
  :indoc '("list")
  :doc "Cosine for every item in list."
  (cos arg1))

(defmethod! om-cos ((arg1 list))   
  (mapcar #'(lambda (input)
              (cos input)) arg1))

(defmethod! om-scale-exp ((self t) (minout number) (maxout number) (exponent number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 1 1) 
  :indoc '("number or list"  "a number" "a number" "an exponent")
  :icon '(209)
  :doc 
  "Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.

Ex. (om-scale 5 0 100 0 10)  => 50
Ex. (om-scale '(0 2 5) 0 100 0 10)  => (0 20 50)
Ex. (om-scale '(0 2 5) 0 100)  => (0 40 100)
 "
  (om-scale (om^ (om-scale self 0. 1. minin maxin) exponent) minout maxout 0. 1.)
  )

