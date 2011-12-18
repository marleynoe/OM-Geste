
(in-package :om)


(defclass gesture-editor (editorview) 
  ((streampanels :initform nil :accessor streampanels))
  (:default-initargs :scrollbars t))

(defclass stream-panel (om-view) 
  ((mystream :initform nil :accessor mystream :initarg :mystream)
   (mainarray :initform nil :accessor mainarray :initarg :mainarray)
   (substreampanels :initform nil :accessor substreampanels)))

(defclass substream-panel (om-view) 
  ((mysubstream :initform nil :accessor mysubstream :initarg :mysubstream)
   (mainarray :initform nil :accessor mainarray :initarg :mainarray)
   (vmin :initform -1 :accessor vmin :initarg :vmin)
   (vmax :initform 1 :accessor vmax :initarg :vmax)))

(defmethod class-has-editor-p ((self gesture-array)) t)
(defmethod get-editor-class ((self gesture-array)) 'gesture-editor)

(defmethod initialize-instance :after ((self gesture-editor) &rest initargs)
  (declare (ignore initargs))
  (om-set-bg-color self *om-gray-color*)
  (setf (streampanels self)
        (loop for stream in (streams (object self)) 
              for i = 0 then (+ i 1) collect
              (om-make-view 'stream-panel
                            :position (om-make-point 10 (+ 10 (* i 190)))
                            :size (om-make-point 500 180)
                            :bg-color *om-light-gray-color*
                            :owner self
                            :mainarray (object self)
                            :mystream stream)))
  self)

(defmethod initialize-instance :after ((self stream-panel) &rest initargs)
  (declare (ignore initargs))
  (let ((subviews-h (round (- (h self) 30) (length (substreams (mystream self))))))
    (setf (substreampanels self)
          (loop for substream in (substreams (mystream self)) 
                for i = 0 then (+ i 1) collect
                (om-make-view 'substream-panel
                              :position (om-make-point 10 (+ 15 (* i subviews-h)))
                              :size (om-make-point 480 subviews-h)
                              :bg-color *om-white-color*
                              :owner self
                              :mainarray (mainarray self)
                              :mysubstream substream)))
    self))

(defmethod initialize-instance :after ((self substream-panel) &rest initargs)
  (declare (ignore initargs))
  (setf (vmin self) (mapcar #'list-min (valuelists (mysubstream self))))
  (setf (vmax self) (mapcar #'list-max (valuelists (mysubstream self))))
  self)

(defmethod om-draw-contents ((self stream-panel))
     (om-with-focused-view self
       (om-draw-string 10 10 (format nil "Stream ~d - ~a/~a" 
                                     (car (sdif-info (mystream self)))
                                     (cadr (sdif-info (mystream self)))
                                     (caddr (sdif-info (mystream self)))
                                     ))))


(defmethod om-draw-contents ((self substream-panel))
  (let ((line-h (h self)) ;(round (h self) (length (valuelists (mysubstream self)))))
        ;(xfact (/ (w self) (length (car (valuelists (mysubstream self))))))
        )
    (om-with-focused-view self
     ; (om-with-fg-color self (om-make-color (om-random 0.2 0.9) (om-random 0.2 0.9) (om-random 0.2 0.9))
        (loop for datalist in (valuelists (mysubstream self)) 
              for mi in (vmin self) for ma in (vmax self) 
              for n = 0 then (1+ 1) do
              (let ((vals (caddr (multiple-value-list (om-sample datalist (w self))))))
                (loop for v on vals for i = 0 then (+ i 1) 
                      when (cdr v) do
                      ;(om-draw-line (* xfact i) (om-scale (car v) line-h 0 mi ma)
                      ;              (* xfact (1+ i)) (om-scale (cadr v) line-h 0 mi ma))
                      (om-draw-line i (om-scale (car v) line-h 0 mi ma)
                                    (1+ i) (om-scale (cadr v) line-h 0 mi ma))
                      )))
      ;  )
    )))


(defmethod update-subviews ((self gesture-editor))
  (mapcar #'(lambda (view)
              (om-set-view-size view (om-make-point (- (w self) 20) (h view)))
              (update-subviews view))
          (streampanels self)))

(defmethod update-subviews ((self stream-panel))
  (mapcar #'(lambda (view)
              (om-set-view-size view (om-make-point (- (w self) 20) (h view))))
          (substreampanels self)))



