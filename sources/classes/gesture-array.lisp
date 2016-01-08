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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


; a selection of streams form an SDIF/GDIF file
(defclass! gesture-array ()
  ((datasrc :accessor datasrc :initarg :datasrc :initform nil)
   (streams :accessor streams :initarg :streams :initform nil)
   ;(fields :accessor fields :initarg :fields :initform nil)
   ;(rows :accessor rows :initarg :rows :initform nil)
   (timerange :accessor timerange :initarg :timerange :initform nil))
  (:icon 0261)
  (:documentation "Gesture array is a selector and container of Gesture Streams.")
  )

;a gesture-stream represents a stream from the gesture array (SDIF) (a GDIF descriptor = a matrix over time)
(defclass! gesture-stream ()
  ((timelist :accessor timelist :initarg :timelist :initform nil)
   (substreams :accessor substreams :initarg :substreams :initform nil)
   (sdif-info :accessor sdif-info :initarg :sdif-info :initform nil)
   ))

;a gesture substream represents a value (one DOF) in a matrix  
(defclass! gesture-substream ()
   ((valuelists :accessor valuelists :initarg :valuelists :initform nil)))


(defmethod initialize-instance :after ((self gesture-array) &rest initargs) 
   (declare (ignore initargs)) 
   (when (datasrc self)
     (let* ((stream-preinfo (sdifinfo (datasrc self) nil))
            (streamsandtime (when (or (not (timerange self)) (not (streams self))) (SDIFstreams+timerange (datasrc self))))
            (timerange (or (timerange self) (list (second streamsandtime) (third streamsandtime))))
            (streams (or (streams self) (first streamsandtime)))
;(stream-info (table-filter (lambda member something) (streams self) 0 pass)) ;this would be more elegant than the loop-in-a-loop
            (stream-info (flat (loop 
                                for tstr in streams collect
                                (remove nil (loop
                                             for pstr in stream-preinfo collect
                                             (if (equal (car pstr) tstr) pstr)
                                             ))) 1)
                         ))
       (when (not (timerange self)) (setf (timerange self) (list (second streamsandtime) (third streamsandtime))))
       (setf (streams self) (loop for str in stream-info collect
 
                                  (multiple-value-bind (data times) 
                                      ;             SDIFFILE       StreamID Frametype  Matrixtype  field row1 row2   time1                    time2
                                      (getsdifdata (datasrc self) (car str) (cadr str) (caddr str) nil   nil  nil    (first timerange) (second timerange))

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





;SDIFtimeandstreams
(defmethod! SDIFstreams+timerange ((self sdifFile) &optional (print t))
   :icon 639
   :doc "Prints information about the SDIF data in <self>.
Returns a list of lists in which the first is the stream numbers in the file, the second the min and max time of any frame.
"
   :indoc '("SDIF file")
   (when print 
     (format *om-stream* "----------------------------------------------------------~%")
     (format *om-stream*  "Creating gesture streams for file: ~D~%"  (pathname-name (filepathname self)))
     (format *om-stream* "----------------------------------------------------------~%"))
   (let ((streams nil)
         (minmaxtimes nil)
         (streamlist nil))
     (loop for fr in (framesdesc self) do
           (let ((pos (position fr streams :test #'(lambda (frame1 frame2) (and (string-equal (car frame1) (car frame2))
                                                                         (= (third frame1) (third frame2))))
                                :key 'car)))
             (if pos (setf (nth pos streams) (append (nth pos streams) (list fr)))
               (setf streams (append streams (list (list fr)))))))
   
   (when print 
     (format *om-stream*  "NUMBER OF SDIF STREAMS: ~D~%~%" (length streams)))
   (loop for st in streams do
      (let* ((times (mapcar 'cadr st))
             (min-time (list-min times))
             (max-time (list-max times))             
             ;(matrices (remove-duplicates (mapcar 'car (flat (mapcar 'fifth st) 1)) :test 'string-equal)))
             (streams nil))
        (when print 
          (format *om-stream*  "      Matrices :  ")
          (format *om-stream*  "   STREAM ID ~D - ~D Frames type = ~A ~%"  (third (car st)) (length st) (car (car st)))
          (format *om-stream*  "      Tmin= ~D   -   Tmax= ~D~%"  min-time max-time)
          (setf minmaxtimes (push (list min-time max-time) minmaxtimes))
          (setf streamlist (push (third (car st)) streamlist))
          )
         ))
   (let* ((transtimes (mat-trans minmaxtimes))
          (global-tmin (list-min (first transtimes)))
          (global-tmax (list-max (second transtimes))))
     (format *om-stream*  "~%Global Tmin= ~D" global-tmin)
     (format *om-stream*  "~%Global Tmax= ~D~%~%"  global-tmax)
   (list (sort-list streamlist) global-tmin global-tmax)
   )
   ))

; Functions for accessing data in a gesture array

; get-stream is used in mappings (i.e. for gesture-models), too
(defmethod! get-stream ((matrix-data gesture-array) (stream symbol))
            :icon 013
            :initvals '(nil nil)
            :indoc '("gesture-data" "stream name")
            (let* ((thestream (find stream (streams matrix-data) :test 'string-equal :key #'(lambda (x) (second (sdif-info x))))))
              thestream
              )
            )

(defmethod! get-substreams ((matrix-data gesture-array) (stream symbol))
            :icon 013
            :initvals '(nil nil)
            :indoc '("gesture-data" "stream name")
            :numouts 2         
            (let* ((thestream (find stream (streams matrix-data) :test 'string-equal :key #'(lambda (x) (second (sdif-info x)))))
                   (timepoints (timelist thestream))
                   )
              (values timepoints (substreams thestream))
              )
            )

(defmethod! get-valuelists ((matrix-data gesture-array) (stream symbol))
            :icon 013
            :initvals '(nil nil)
            :indoc '("gesture-data" "stream name")
            :numouts 2
            ;(cadr (find stream matrix-data :test 'string-equal :key 'car))
            (let* ((thestream (find stream (streams matrix-data) :test 'string-equal :key #'(lambda (x) (second (sdif-info x)))))
                   (timepoints (timelist thestream))
                   )
              (values timepoints (valuelists (car (substreams thestream))))
              )
            )

(defmethod! get-values-bpf ((matrix-data gesture-array) (stream symbol) (dimension number))
            :icon 013
            :initvals '(nil nil 0)
            :indoc '("gesture-data" "stream name" "dimension")
            (let* ((thestream (find stream (streams matrix-data) :test 'string-equal :key #'(lambda (x) (second (sdif-info x)))))
                   (timepoints (timelist thestream))
                   (ypoints (nth dimension (valuelists (car (substreams thestream)))))
                   )
              (simple-bpf-from-list timepoints ypoints 'bpf 10)
              )
            )