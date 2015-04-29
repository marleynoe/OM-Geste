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
;Authors: M. Schumacher, J. Bresson

(in-package :om)


;;make methods for lists of processes!


; To do: 1) needs to work with other classes (not only temporalboxes), 2) need a map-column, map-row? function (where the lambda patch can see the entire picture)
; if I want to have dynamic mappins I need to be able to see the entire model or at least slot. -> not if this is already introduced in the model
; should have also the inputs named

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
                         
                         ;(input-names (print (mapcar #'(lambda (out) 
                         ;                   (intern (frame-name out) :om))
                         ;               (sort (find-class-boxes (boxes mapping-fun) 'omin) '< :key 'indice))))     
                         ;(names (mapcar #'(lambda (out) 
                         ;                   (frame-name out))
                         ;               (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                         
                         (names (mapcar #'(lambda (out) 
                                            (intern (string-upcase (frame-name out)) :om))
                                        (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                        ; (names (mapcar #'(lambda (out) 
                         ;                   (intern (frame-name out) :om))
                          ;              (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                         (slots (list names vals))
                         (transslots (mat-trans (list names vals))))
                    

                        ; (slots2 (print (list names vals)))                        
                        ; (slots (print '((lmidic loffset) ((6943 5380 6860 6642 5287) 1.3)))))
                    
                    ;(print (type-of (caar slots2)))
                    ;(print (type-of (caar slots)))                       

                    ; for now make it work with chords / notes
                  #|       
                    (if (subtypep (type-of object) 'class-array)
                        ;(let ((prep-slots (values (flat (loop for item in slots collect
                        ;                        (x-append (print (intern (string-upcase (car item)) :keyword)
                        ;                                         (cdr item))))))))
                       ; (let ((testlist (cons :numcols 15 :e-dels '(1 2 3 4 5))))

                        ; it can be solved by sorting out from lists etc. - but it would be better to find a solution for make-instance.

                        (let ((filteredslots (loop for item in transslots unless (string-equal (string-upcase (print (car item))) "numcols") collect 
                                                   item))
                              (numcols (loop for item in transslots when (string-equal (string-upcase (print (car item))) "numcols") collect 
                                                   item)));

                                                   ;this should be done by appending into a variable

                                                   ;(table-filter  #'(lambda (x) (eql (string-upcase x) 'numcols)) (print item) 0 'reject))))
                          (print filteredslots)
                          (print residue)
                          ))
|#

                    (let* ((actiontimes nil)
                           (numcols nil)
                           (filteredslots nil))
                      (loop for item in transslots do
                            (cond ((string-equal  (car item) "numcols") (setf numcols (append numcols item)))
                                  ((string-equal  (car item) "action-time") (setf actiontimes (append actiontimes item)))
                                  (t (setf filteredslots (append filteredslots item)))
                                  ))
                      ;(print actiontimes)
                      ;(print numcols)
                      ;(print filteredslots)

                    (if (subtypep (type-of object) 'class-array)
                        (set-array-2 (type-of object) (second numcols) (second actiontimes) filteredslots)
                      (thelooper obj-instance (car slots) (cadr slots))
                      )))))






                       ; (set-array2 (type-of object) 10 0 (print (flat transslots 1))))
                       ;   (make-instance (type-of object) (print testlist))
                       ; ))
                        ;(cons-array (first slots) (second slots))
                      ;)

                        ;(let ((thearray (make-instance (type-of object) (intern (string-upcase "numcols") :keyword) 15)))
                          ;(setf (slot-value thearray 'numcols) 15)
                         ; thearray
                          ;))

                   ; (if (subtypep (type-of object) 'class-array)
                   ;     (let ((theinstance (make-instance (type-of object))))
                    ;      (loop for item in (flat slots
                        ;(set-array (type-of object) 10 (flat slots))(eql 'this 'this)


                   ;     (make-instance (type-of object) (flat slots))
                   ;   )
                  ;    (
                   ;(loop for item in (print slots) do
                    ;(print (car slots))

                    ; Try this function "thelooper" with 'slots' and 'slots2' : one works the other doesn't (symbols are different??)
                    ; (thelooper obj-instance (car slots2) (cadr slots2))
                    
                    #|
                    (loop for item in (print slots) do                       
                          
                          (if (is-om-slot? (type-of obj-instance) (car item))
                              (om-beep-msg (format nil "Error: slot ~A does not exist in class ~A !" (car item) (type-of object)))
                              ))
                    |#
                    ;)
                    ;obj-instance
                    ;))


#|
(setf testlist '((initarg1 1) ('initarg2 2) ('initarg3 '(a b))))

(type-of (caar '((initarg1 1) ('initarg2 2) ('initarg3 '(a b)))))

(string-equal (string-upcase 'lmidic) "LMIDIC")

(print (values-list '(:numcols 15 :e-dels '(1 2 3 4 5))))

(print (intern (string-upcase "numcols") :keyword) 15)

(multiple-value-bind (:numcols 15 :e-dels '(1 2 3 4 5)))

(intern (string-upcase "lmdidc") :keyword)

;(make-instance 'chord (intern (string-upcase "lmdidc") :keyword) '(200 400 600))

;(make-instance (type-of object) (intern (string-upcase slotname) :keyword)

|#
    

#|
;some tests
; how can I set the slots of score-objects (chord, chord-seq, voice, etc.) upon instantiation?
; "do-initialize" might be a possibility

(setf thetestlist '((lmidic loffset) ((6943 5380 6860 6642 5287) 1.3)))
(setf thetestclass (make-instance 'chord))
(thelooper thetestclass (car thetestlist) (cadr thetestlist))
|#

(defun thelooper (class slotlist valuelist)
  (loop for slot in slotlist
        for value in valuelist do
        (set-class-slot class slot value)
        ;(setf (slot class) value)
        )
  class)

#|
(defun set-class-slot (class slot value)
  (funcall (fdefinition `(setf ,slot)) value class))

(defun set-class-slot (object slot value)
  (funcall (fdefinition `(setf ,slot ,object)) value)
  )
|#

;(defun set-class-slot (object slot value)
;  (eval `(setf (,slot ,object) value))
;  )

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
            :icon '(335)
            (cadr (find name descriptor :test 'string-equal :key 'car)))

