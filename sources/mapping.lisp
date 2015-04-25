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


;;make methods for lists of processes!


; To do: 1) needs to work with other classes (not only temporalboxes), 2) need a map-column, map-row? function (where the lambda patch can see the entire picture)
; if I want to have dynamic mappins I need to be able to see the entire model or at least slot. 


(defmethod! map-gesture ((self gesture-model) mapping-fun object)
            :icon 02        
            (compile-patch mapping-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((obj-instance (make-instance (type-of object)))
                         ;(print (slot-definition-name (class-slots (class-of box))))
                         (thecontrols (lcontrols self))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code mapping-fun)) :om) ;mapping fun = patch in lambda mode

                                    (loop for slot in thecontrols collect
                                          (list (string (first slot)) (nth col (second slot)))))))
                                                
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out)))
                                        (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                        ; (names (mapcar #'(lambda (out) 
                         ;                   (intern (frame-name out) :om))
                          ;              (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                         
                         ;(slots (print (mat-trans (list names vals)))))

                         ; slots2 and slots seem to be THE SAME - yet one throws an error the other doesn't!

                         (slots2 (print (list names vals)))                        
                         (slots (print '((lmidic loffset) ((6943 5380 6860 6642 5287) 1.3)))))
                    (print (type-of (caar slots2)))
                    (print (type-of (caar slots)))
                           

                    ; for now make it work with chords / notes
                  ;  (if (subtypep (type-of object) class-array)
                  ;      (set-array (type-of object) 1 (flat slots))
                  ;    (
                   ;(loop for item in (print slots) do
                    ;(print (car slots))
                    (thelooper obj-instance (car slots2) (cadr slots2))
                    ;)
                    #|
                    (loop for item in (print slots) do                       
                            
                            (if (is-om-slot? (type-of obj-instance) (car item))
                    ;          
                              ;(set-slot box (car item) (cadr item));;
                               ; (lambda (x) (setf (x box) (cadr item)) (car item))
                              ;(setf (funcall (function theslotname) box) (cadr item))
                              ;(setf (slot-value box (car item)) (cadr item))
                              ;(setf ((symbol-function theslotname) box) (cadr item))
                                ;(progn (print theslotname)
                                ;  (setf ((string theslotname) box) (cadr item)))
                              ;(funcall (function setf) theslotname box (card item))
                              ;(setf (theslotname box) (cadr item))

                                (set-class-slot (print obj-instance) (print (car item)) (print (cadr item)))

                              ;(setf (funcall (symbol-function theslotname) box) (cadr item));; --> this doesn't work
                                ;(print (get-all-slots-of-class (type-of box)))
                              (om-beep-msg (format nil "Error: slot ~A does not exist in class ~A !" (car item) (type-of object)))
                              ))
                    |#
                    ;)
                    obj-instance
                    )))

#|
(setf thetestlist '((lmidic loffset) ((6943 5380 6860 6642 5287) 1.3)))
(setf thetestclass (make-instance 'chord))


(thelooper thetestclass (car thetestlist) (cadr thetestlist))
|#

(defun thelooper (class slotlist valuelist)
  (loop for slot in slotlist
        for value in valuelist do
        (set-class-slot class (print slot) (print value))
        )
  class)


#|
(setf thetestclass (make-instance 'chord))

(set-class-slot thetestclass 'loffset '(2300 2400))

(loffset thetestclass)
|#

(defun set-class-slot (class slot value)
  (funcall (fdefinition `(setf ,slot)) value class))


; syntactic sugar for "removealltemporalboxes"
(defmethod! clearmaq ((self ommaquette))
            :icon 327
            :initvals '(nil)
            :indoc '("a maquette")
            :doc "Removes all TemporalBoxes in <self>."
           (removealltemporalboxes self)
           )

(defmethod! gesture-slot (descriptor name)
            :icon '(335) ;'(333)
            
           ; :menuins '((1 (("onset" "onset") ("duration" "duration") ("magnitude" "magnitude") 
           ;                ("norm" "norm") ("corpus-index" "corpus-index") ("file-index" "file-index") ("filepath" "filepath"))))
            ;(cadr (find name descriptor :test 'string-equal :key 'car)))
            (cadr (find name descriptor :test 'string-equal :key 'car)))


;;; trying different mapping function
#|
(defmethod! mapping-class ((self gesture-model) matching-fun theclass)
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
                          ;(let ((theslotname (car item)))
                          (if (subtypep (type-of box) class-array)
                              
                              (set-array (type-of box) finaldata)
                            
                            (if (is-om-slot? (type-of box) (car printitem))
                              
                              (set-slot box (car item) (cadr item))

                              ;(setf (theslotname box) (cadr item)) --> this doesn't work
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class ~A !" (car item) (type-of theclass))))
                          );)
                    box
                    )))
|#

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
