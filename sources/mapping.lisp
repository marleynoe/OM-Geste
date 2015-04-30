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


; Notes: 
; 1) Needs a map-column, map-row function (where the lambda patch can see the entire picture)?
; 2) if I want to have dynamic mappings I need to be able to see the entire model or at least slot
; 3) -> not if this is already introduced in the model
; 4) should have also the inputs named

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
                                          (list (string (first slot)) (nth col (second slot))))))) ;the function uses this
                         
                         (input-names (print (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes mapping-fun) 'omin) '< :key 'indice))))     
                         
                         (names (mapcar #'(lambda (out) 
                                            (intern (string-upcase (frame-name out)) :om))
                                        (sort (find-class-boxes (boxes mapping-fun) 'omout) '< :key 'indice)))
                         (slots (list names vals))
                         (transslots (mat-trans (list names vals))))
                    
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
                      (set-class-slots obj-instance (car slots) (cadr slots))
                      )))))


; **** HELPER FUNCTIONS ****

(defun set-class-slots (class slotlist valuelist)
  (loop for slot in slotlist
        for value in valuelist do
        (set-one-slot class slot value)
        )
  class)

(defun set-one-slot (class slot value)
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

