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
;This program is distributed in the hope thnameat it will be useful,
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
 

(defmethod! segment-gesture ((self gesture-array) (times list))
            :icon 262
            :initvals '(nil nil)
            :indoc '("a gesture-array or gesture-model" "a list of times (in seconds) defining temporal segments")
            :numouts 1    
            (print (format nil "Starting segmentation of ~a" self))
            (let* ((descriptors (loop for str in (streams self) collect (second (sdif-info str))))
                   (thetimes (or times (timerange self)))
                   (segment-data (loop for segment on thetimes
                                      for i = 1  then (+ i 1)
                                      while (cdr segment) collect
                                      (let ((t1 (car segment))
                                            (t2 (cadr segment))
                                            (t3 (print (format nil "Processing segment ~D: sec ~D to sec ~D." i (car segment) (cadr segment)))))
                                        (loop for str in (streams self) 
                                              for i = 1 then (+ i 1) append
                                              (loop for substr in (substreams str) collect
                                                    (progn
                                                      (print (format nil "segment-gesture: processing stream # ~D: ~a" i (nth (1- i) descriptors)))
                                                      (make-segmented-object 
                                                       (valuelists substr)
                                                       (mapcar #'(lambda (x) (coerce x 'single-float)) (timelist str))
                                                       t1 t2))
                                                    ))
                                        ))
                                ))            
              (let ((timearray 
                     (cons-array (make-instance 'time-array :times thetimes) 
                                 (list nil thetimes)
                                 (loop for row in (mat-trans segment-data)
                                       for j = 0 then (+ j 1)
                                       append (list (internk (nth j descriptors)) row)))))
                
                (set-data timearray)
                (print (format nil "Segmentation of ~a DONE." self))
                timearray
                )))

(defmethod! segment-gesture ((self gesture-model) (times list))
            (print (format nil "Starting segmentation of ~a" self))
            (let* ((gesture-data (data self))
                   (descriptors (loop for row from 0 to (1- (length gesture-data)) collect (index2label self row)))
                   (concat-data (loop for row in gesture-data collect
                                      (concat-valuelists row))) ; (  ( ((xxx) (yyy) (zzz)) times) ( ((xxx) (yyy) (zzz)) times)
                   (times (or times (list (first (times self)) (car (last (times self))))))
                   (segment-data (loop for segment on times
                                       for i = 1  then (+ i 1)
                                       while (cdr segment) collect
                                       (let ((t1 (car segment))
                                             (t2 (cadr segment))
                                             (t3 (print (format nil "processing segment ~D: sec ~D to sec ~D." i (car segment) (cadr segment)))))
                                         (loop for row in concat-data 
                                               for i = 1 then (+ i 1) collect; each str is ((datalist) timelist)                                           
                                               (progn
                                                 (print (format nil "processing row # ~D: ~a" i (nth (1- i) descriptors)))
                                                 (make-segmented-object (first row) (second row) t1 t2)))
                                         ))))
              
              (let ((timearray 
                     (cons-array (make-instance 'gesture-model :times times) 
                                 (list nil times)
                                 (loop for row in (mat-trans segment-data)
                                       for j = 0 then (+ j 1)
                                       append (list (internk (nth j descriptors)) row)))))
                
                (set-data timearray)
                (print (format nil "Segmentation of ~a DONE." self))
                timearray
                )))

(defmethod make-segmented-object ((self list) timelist t1 t2 &optional (decimals 10))
  ;self is a list of lists values ((xxx) (yyy) (zzz))
  ;timelist is a list of values
  ;(print (format nil "Length of datalist: ~D" (length self)))
  ;(print (format nil "Length of datalist sublist: ~D" (length (car self))))
  ;(print (format nil "Length of timelist: ~D" (length timelist)))  
  (let ((pos1 (position t1 timelist :test '<))
        (pos2 (position t2 timelist :from-end t :test '>)))
    (when (and pos1 pos2)

      (let ((times (append (list t1) ;first timepoint of segment
                           (range-filter timelist 
                                         (list (list pos1 pos2)) 'pass)
                           (list t2))) ;last timepoint of segment

            (data (mapcar #'(lambda (d) ; do on every sublist in self:
                               (append  
                                (list (x-transfer (mat-trans (list timelist d)) t1))   ; get first timepoint of segment via x-transfer
                                (range-filter d (list (list pos1 pos2)) 'pass)         ; grab elements of the sublist which are within pos1 pos2
                                (list (x-transfer (mat-trans (list timelist d)) t2)))) ; git last timepoint of segment via x-transfer 
                           self))) 
 
    ;(print (length self))
        ; HERE OUR ASSUMPTIONS: 1-dimension = bpf, 2-dimensional = trajectory with z=0, 3-dimensional = trajectory, 
    (cond (
           (= (length self) 1)
           (simple-bpf-from-list times (first data) 'bpf decimals))
          ((= (length self) 2)
           (traject-from-list (first data) (second data) nil times '3D-trajectory decimals))
          ((= (length self) 3) 
           (traject-from-list (first data) (second data) (third data) times '3D-trajectory decimals))
          )
    ))))

(defmethod make-segmented-object ((self sound) timelist t1 t2 &optional (decimals 10))
  (sox-process self (sox-trim t1 t2) :output "new file")                                                                         
  )

(defmethod make-segmented-object ((self chord-seq) timelist t1 t2 &optional (decimals 10))
  (select self (round (* t1 1000)) (round (* t2 1000)))                                                                         
  )

; NEEDS A METHOD for (gesture-model): re-segmenting 
; --> requires concatenating bpfs, 3DCs etc. and segmenting them again. 


; *** HELPER FUNCTIONS ****

(defmethod! get-valuelists ((self bpf))
            (let ((timelist (x-points self))
                  (datalist (list (y-points self))))
              (list datalist timelist)
              ))

(defmethod! get-valuelists ((self 3d-trajectory))
            (let ((timelist (times self))
                  (datalist (list (x-points self) (y-points self) (z-points self))))
              (list datalist timelist)
              ))

(defmethod! get-valuelists ((self number))
            (let ((timelist '(1)) ; would need the time of the segment here.
                  (datalist (list (list self))))
              (list datalist timelist)
              ))


;(defmethod! get-valuelists ((self sound))
;            (list self nil)
;              )

(defmethod! concat-valuelists ((self list))         
            (if (soundp (car self))
                (list (sox-process (make-instance 'sox-concatenate :sound self)) nil)
              (let ((valueslist)
                    (timeslist))
                (loop for item in self do
                      (let ((data (get-valuelists item))) ;data is a list containing '(((xxx) (yyy) (zzz)) times)
                        (cond ((eql (type-of item) 'bpf) ; could add as another case audio files and score files (to concatenate them)
                               (progn                               
                                 (setf valueslist (list (append (first valueslist) (car (first data)))))
                                 (setf timeslist (append timeslist (second data)))
                               ;(print (format nil "length of bpf valueslist: ~D" (length valueslist))))
                                 ))
                              ((eql (type-of item) '3d-trajectory)
                               (progn
                               ;(print (format nil "length of valueslist: ~D" (length valueslist)))
                                 (setf valueslist (list (x-append (first valueslist) (first (car data)))
                                                        (x-append (second valueslist) (second (car data)))
                                                        (x-append (third valueslist) (third (car data)))))                               
                                 (setf timeslist (append timeslist (second data)))))
                              ((eql (type-of item) 'single-float)
                               (progn
                               ;(print (format nil "length of valueslist: ~D" (length valueslist)))
                                 (setf valueslist (print (list (append (first valueslist) (car (first data))))))                               
                                 (setf timeslist (print (append timeslist (second data))))))
                              )
                        ))
                (list valueslist timeslist)))
            )
             

; here we choose to create objects solely based on the dimensionality of the data. This assumption is probably not always a good choice. 
; Instead, could be based on the matrix type inside the SDIF (implicit)? Or in an NVT an entry for "separable" or "integral"?
; Or a combination: First check for SDIF type, THEN check for dimensionality. Meaning: in the SDIF multiple fields (columns) are integral, rows are separable
; 
; here I should add methods for chord-seq (using the 'select' function), audio (using 'sound-cut') etc.

; this function works only for temporal objects (i.e. objects that have a timelist)
; this is for a gesture stream 