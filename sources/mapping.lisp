;OM-geste, 2012-2015 McGill University
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

;%%%%%%%%%% PROCESS ARRAY %%%%%%%%%%%%%%

;;make methods for lists of processes!

(defmethod! mapping ((self gesture-model) matching-fun)
            :icon '(333)
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance 'temporalbox))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om)
                                    (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                          (list (name slot)
                                                (get-array-val self (name slot) col))))))
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (mat-trans (list names vals))))
                    
                    (setf (free-store box) vals)
                    
                    (loop for item in slots do
                          (if (is-om-slot? (type-of box) (car item))
                              
                              (set-slot box  (car item) (if (floatp (cadr item))
                                                            (om-round (cadr item))
                                                          (cadr item)))
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class TemporalBox !" (car item))))
                          )
                    box
                    )))

#|
(defmethod! mapping ((self gesture-model) matching-fun theclass)
            :icon '(333)
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance 'temporalbox)) ;here "theclass" should be the 'type-of
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om)
                                    (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                          (list (name slot)
                                                (get-array-val self (name slot) col))))))
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (mat-trans (list names vals))))
                    
                    (setf (free-store box) vals)
                    
                    (loop for item in slots do
                          (if (is-om-slot? (type-of box) (car item))
                              
                              (set-slot box  (car item) (if (floatp (cadr item))
                                                            (om-round (cadr item))
                                                          (cadr item)))
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class TemporalBox !" (car item))))
                          )
                    box
                    )))
|#
