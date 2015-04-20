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

#|
(defmethod! mapping ((self gesture-model) matching-fun theclass)
            :icon '(333)
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance (type-of theclass)))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om)
                                    (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                          (list (name slot)
                                                (get-array-val self (name slot) col))))))`
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (mat-trans (list names vals))))
                    
                    (setf (free-store box) vals) ; what is this doing?
                    
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

; To do: 1) needs to work with other classes (not only temporalboxes), 2) need a map-column, map-row? function (where the lambda patch can see the entire picture)
; if I want to have dynamic mappins I need to be able to see the entire model.
 
(defmethod! mapping ((self gesture-model) matching-fun)
            :icon '(333)
            ;(print "it's me")
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance 'temporalbox))
                         (thecontrols (lcontrols self))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om) ;matching fun = patch in lambda mode

                                   ; (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                   ;       (print (list (name slot)
                                   ;             (get-array-val self (name slot) col)))))))
                                   ; a list of ( (("name1" val@col1) ("name2" val@col1)) (("name1" val@col2) ("name2" val@col2)))                                   
                                   ; (lcontrols self))))

                                    (loop for slot in thecontrols collect
                                          (list (string (first slot)) (nth col (second slot)))))))
                                                 ;slot))))
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


; think about arrays: maybe in that case I should rather set components than making different instances.
; also, the slots of the instances actually DO get set, but it's not visible in the editor. -> Jean??
(defmethod! mapping2 ((self gesture-model) matching-fun theclass)
            :icon '(333)
            ;(print "it's me")
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance (type-of theclass)))
                         (thecontrols (lcontrols self))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om) ;matching fun = patch in lambda mode

                                   ; (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                   ;       (print (list (name slot)
                                   ;             (get-array-val self (name slot) col)))))))
                                   ; a list of ( (("name1" val@col1) ("name2" val@col1)) (("name1" val@col2) ("name2" val@col2)))                                   
                                   ; (lcontrols self))))

                                    (loop for slot in thecontrols collect
                                          (list (string (first slot)) (nth col (second slot)))))))
                                                 ;slot))))
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (print (mat-trans (list names vals)))))
                    
                    ;(setf (free-store box) vals)
                    
                    (loop for item in slots do
                          (if (is-om-slot? (type-of box) (car item))
                              
                              (set-slot box  (car item) (if (floatp (cadr item))
                                                            (om-round (cadr item))
                                                          (cadr item)))
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class TemporalBox !" (car item))))
                          )
                    box
                    )))


(defmethod! clearmaq ((self ommaquette))
            :icon 327
            :initvals '(nil)
            :indoc '("a maquette")
            :doc "Removes all TemporalBoxes in <self>."
           (removealltemporalboxes self)
           )

(defmethod! gesture-slot (descriptor name)
            :icon '(335) ;'(333)
            
            :menuins '((1 (("onset" "onset") ("duration" "duration") ("magnitude" "magnitude") 
                           ("norm" "norm") ("corpus-index" "corpus-index") ("file-index" "file-index") ("filepath" "filepath"))))
            ;(cadr (find name descriptor :test 'string-equal :key 'car)))
            (cadr (find name descriptor :test 'string-equal :key 'car)))

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
