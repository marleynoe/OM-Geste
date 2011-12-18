(in-package :om)

(defclass* mynote-array (class-array)
           (
            (note :type t :initform nil :accessor note  :initarg :note)
           ))

(defmethod! process-array ((process t) (array class-array))
            :icon '(264)
            (let ((thearray (clone array))
                  )
              (loop for index from 0 to (1- (numcols thearray)) do
                    (apply process (list thearray index)))
            thearray
            ))