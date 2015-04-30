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
 

(defmethod! segment-gesture ((self gesture-array) (times list))

            :icon '(104) ;02
            :initvals '(nil nil)
            :indoc '("a gesture-array or gesture-model" "a list of times (in seconds) defining temporal segments")
            :numouts 1
           
            (let ((descriptors (loop for str in (streams self) collect (second (sdif-info str))))
                  (segment-data (loop for segment on times
                                      for i = 1  then (+ i 1)
                                      while (cdr segment) collect
                                      (let ((t1 (car segment)) ;(format nil "Processing segment ~D sec to ~D sec." t1 t2)
                                            (t2 (cadr segment))
                                            (t3 (print (format nil "Processing segment ~D: sec ~D to sec ~D." i (car segment) (cadr segment)))))
                                        (loop for str in (streams self) append
                                              (loop for substr in (substreams str) 
                                                    collect (make-segmented-object 
                                                             (valuelists substr) 
                                                             (mapcar #'(lambda (x) (coerce x 'single-float)) (timelist str))
                                                             t1 t2)
                                                    ))
                                        ))))
              
              (let ((timearray 
                     (cons-array (make-instance 'time-array :times times) 
                                 (list nil times)
                                 (loop for row in (mat-trans segment-data)
                                       for j = 0 then (+ j 1)
                                       append (list (internk (nth j descriptors)) row)))))
                
                (set-data timearray)
                timearray
                )))


; method for re-segmenting a gesture-model


; *** HELPER FUNCTIONS ****

;(defmethod! concat



; I had an idea to have a more specialized class than simply a time-array...  it needs to be a time-array which segments audio! and score objects.
; Need to find a way to make temporal selections (extraction from the gesture model)
; a) means to re-segment the model and extract a component
; b) could have a function that merges the data together and then segments it and returns the object, for example. (A bit like the gesture-array - time range) 

;datalists is a row of the matrix (instance) of lists (parameters)

; here we choose to create objects solely based on the dimensionality of the data. This assumption is probably not always a good choice. 
; Instead, could be based on the matrix type inside the SDIF (implicit)? Or in an NVT an entry for "separable" or "integral"?
; Or a combination: First check for SDIF type, THEN check for dimensionality. Meaning: in the SDIF multiple fields (columns) are integral, rows are separable
; 
; here I should add methods for chord-seq (using the 'select' function), audio (using 'sound-cut') etc.

; this function works only for temporal objects (i.e. objects that have a timelist)
(defmethod make-segmented-object (datalists timelist t1 t2 &optional (decimals 10))
  (let ((pos1 (position t1 timelist :test '<))
        (pos2 (position t2 timelist :from-end t :test '>)))
    (when (and pos1 pos2)
      (let ((times (append (list t1) 
                           (range-filter timelist 
                                         (list (list pos1 pos2)) 'pass)
                           (list t2)))
            (data (mapcar #'(lambda (d) 
                               (append (list (x-transfer (mat-trans (list timelist d)) t1)) 
                                       (range-filter d (list (list pos1 pos2)) 'pass)
                                       (list (x-transfer (mat-trans (list timelist d)) t2))))
                           datalists)))
 
    ;(print (length datalists))
        ; HERE OUR ASSUMPTIONS: 1-dimension = bpf, 2-dimensional = trajectory with z=0, 3-dimensional = trajectory, 
    (cond (
           (= (length datalists) 1)
           (simple-bpf-from-list times (first data) 'bpf decimals))
          ((= (length datalists) 2)
           (traject-from-list (first data) (second data) nil times '3D-trajectory decimals))
          ((= (length datalists) 3) 
           (traject-from-list (first data) (second data) (third data) times '3D-trajectory decimals))
          )
    ))))

