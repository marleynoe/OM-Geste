;OM-Geste, 2011-2015 McGill University
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
; I think for the chroma user-fun it is possible to use lists of processes

(defmethod! build-array ((components list))
            :icon '(264)
            (let* ((newarray (make-instance (type-of (comp-array (first components)))
                                            :numcols 0
                                            ))) ;(or actiontime 0)
              (mapcar (lambda (thecomps)
                        (add-comp newarray thecomps)) components)
              newarray
              ))

(defmethod! build-model ((components list) (times list))
            :icon '(264)
            (let* ((newarray (make-instance 'gesture-model
                                            :times times
                                            ))) ;(or actiontime 0)
              (mapcar (lambda (thecomps)
                        (add-comp newarray thecomps)) components)
              newarray
              ))

(defmethod! merge-models ((self gesture-model) &rest arrays)
            :icon '(264)
            (let* ((arraylist (flat (x-append self arrays)))
                   (complist 
                    (loop for array in arraylist collect
                          (get-components array)))
                    ;(mapcar (lambda (theindex)
                    ;          (get-comp array theindex)) (arithm-ser 0 (1- (numcols array)) 1))))
                   (thenewarray (build-model (flat complist) (times self)))
              thenewarray)))

(defmethod! get-components ((array class-array))
            :icon '(323)
            (mapcar (lambda (theindex)
                      (get-comp array theindex)) (arithm-ser 0 (1- (numcols array)) 1)))

(defmethod! get-components ((array list))
  (flat (mapcar (lambda (thearrays)
            (get-components thearrays)) array)))


; slotvals should be called 'data'
; **** MATRIX MANIPULATIONS (adding and removing rows and columns) ********
(defmethod! add-row ((self class-array) (slotname string) (slotvals t) &optional index)
            :icon '(264)
            (let* ((arraydata (data self))
                   (timesdata (times self))
                   (labeldata
                    (loop for slot in arraydata
                          for i from 0 to (1- (length arraydata))
                          collect
                          (list (index2label self i) slot)
                          ))
                   (newdata (print (list slotname (list slotvals))))
                   (finaldata (print (if (integerp index)
                                  (flat (interlock labeldata (list newdata) (list! index)) 1)
                              (x-append (flat labeldata 1) slotname (list slotvals))))))
              ;(print labeldata)
              ;(print timesdata)
              ;(print finaldata)
              (set-array (type-of self) (times self) finaldata)
              ))

(defmethod! add-row ((self class-array) (slotname list) (slotvals list) &optional index)
            (let ((themodel self))
              (mapc (lambda (slot val i) (setf themodel (add-row themodel slot val i))) slotname slotvals index)
              themodel)
            )

(defmethod! remove-row ((self class-array) (slotname string))
            :icon '(264)
            (let* ((arraydata (data self))
                   (timesdata (times self))
                   (labeldata
                    (loop for slot in arraydata
                          for i from 0 to (1- (length arraydata))
                          collect
                          (unless (string-equal (print (index2label self i)) slotname)
                            (list (index2label self i) slot)
                          )))
                   (finaldata (flat labeldata 1)))
              ;(print labeldata)
              ;(print timesdata)
              ;(print finaldata)
              (set-array (type-of self) (times self) finaldata)
              ))

(defmethod! remove-row ((self class-array) (slotname list))
            (let ((themodel self))
              (mapc (lambda (slot) (setf themodel (remove-row themodel slot))) slotname)
              themodel)
            )


; ***** getting data out of the model *****
; ToDo: catch error when index for row/column is beyond matrix dimensions

(defmethod! get-column ((array class-array) (column number))
            :icon 322
            (comp-list (get-comp array column))
            )

(defmethod! get-column ((array class-array) (column list))
            (mapcar (lambda (x) (get-column array x)) column)
            )


(defmethod! get-row ((array class-array) (row number))
            :icon 323
            (nth row (data array))
            )

(defmethod! get-row ((array class-array) (row list))
            (mapcar (lambda (x) (get-row array x)) row)
            )

(defmethod! get-field ((array class-array) (column number) (row number))
            :icon 321
            (comp-field (get-comp array column) row)
            )

(defmethod! get-field ((array class-array) (column list) (row list))
            (mapcar (lambda (thecolumn therow)
                      (get-field array thecolumn therow)) column row)
            )

(defmethod! get-field ((array class-array) (column list) (row number))
            (get-field array column (repeat-n row (length column)))
            )

(defmethod! get-field ((array class-array) (column number) (row list))
            (get-field array (repeat-n row (length row)) row)
            )


#|
(defmethod! set-array-slot ((self class-array) (slotname t) (slotvals t))
            :icon '(264)
            (let* ((arraydata (data self))
                   (slotname (list! slotname))
                   (slotvals (if (equal 1 (length slotname))
                                 (list slotvals)
                               (list! slotvals)))
                   (labeldata
                    (loop for slot in slotname
                          for vallist in slotvals
                          collect
                          (list slot vallist)
                          )))
              ;(print (flat labeldata 1))
              (set-array (type-of self) (numcols self) (flat labeldata 1))
              ))
|#

(defmethod! set-array-slot ((array class-array) (slotname string) (slotvals t))
            :icon '(264)
            (let ((newarray array))
           ; (setf ((label2index newarray slotname) newarray) slotvals)
            ;  (label2index newarray slotname)
              ;(setf (#'slotname newarray) slotvals)
              (setf #'(lambda (theslotname)
                      (theslotname newarray) slotname) slotvals)
            (set-data newarray)
            newarray         
            ))



(defun set-array (type numcols params)
  (let ((array (cons-array (make-instance type) (list nil numcols 0 nil) params)))
    (set-data array)
    array)
  )



(defmethod! process-array ((process t) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  (complist (loop for i from 0 to (1- (numcols thearray))
                                   collect (get-comp thearray i))))
                    (funcall process (list thearray complist))
            thearray
            ))

#|
(defmethod! process-array ((process t) (array class-array))
            :icon '(264)
            (let ((thearray (clone array))
                  )
              (loop for index from 0 to (1- (numcols thearray)) do
                    (apply process (list thearray index)))
            thearray
            ))
|#

; process-rows should process all the rows, i.e. supply the rows one by one and have an optional input to choose the row i.e. descriptor. If nil returns all rows one by one
; or the name of the row (slot) is provided, too, so it can be chosen inside the function
; 

(defmethod! process-rows ((process t) (array class-array))
            :icon '(264)
            (let ((thearray (clone array)))
                    (apply process (list thearray)) ;what's the difference to 'funcall' ?
            thearray
            ))

(defmethod! process-row ((process t) (slotname string) (array class-array))
            :icon '(264)
            (let ((thearray (clone array)))
                    (apply process (list thearray)) ;what's the difference to 'funcall' ?
            thearray
            ))

(defmethod! process-model ((process t) (array class-array))
            :icon '(264)
            (let ((thearray (clone array)))
                    (apply process (list thearray)) ;what's the difference to 'funcall' ?
            thearray
            ))



(defmethod! process-columns ((process t) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                   (complist (loop for i from 0 to (1- (numcols thearray))
                                   collect (get-comp thearray i))))
              (loop for comp in complist do
                    (apply process (list comp)))
              thearray
              ))

;;this doesn't work yet... not so easy with lists of processes
(defmethod! process-array-comp ((process list) (array class-array))
            (mapcar (lambda (theprocess)
                      (process-array-comp theprocess array)) process)
            )

(defmethod! process-array-slot ((process t) (slotname string) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  ;(theslotvalues (symbol-function slotname)) ;later I should use symbol-function... but no time for now
                  (theslotvalues (array-field array slotname))
                  (thenewvalues 
                   (loop for value in theslotvalues collect 
                         (funcall process value))))
              (array-field thearray slotname thenewvalues)
              thearray
              ))

; old
(defmethod! process-array-slot ((process t) (slotname string) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  ;(theslotvalues (symbol-function slotname)) ;later I should use symbol-function... but no time for now
                  (theslotvalues (array-field array slotname))
                  (thenewvalues (funcall process theslotvalues)))
            (array-field thearray slotname thenewvalues)
            thearray
            ))


(defmethod! user-process-model ((process t) (slotname string) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  ;(theslotvalues (symbol-function slotname)) ;later I should use symbol-function... but no time for now
                  (theslotvalues (array-field array slotname))
                  (thenewvalues (funcall process theslotvalues)))
            (array-field thearray slotname thenewvalues)
            thearray
            ))


;%%%%%%%%%%%%%% USER FUNS %%%%%%%%%%%%%%%%%%%%%%

; a number of pre-determined processing funs for arrays

;; component methods %%%%%%%%%%%%%%


(defmethod! ran-env ((self component) (slotname string) (min number) (max number))
            :icon 04
            :initvals '(nil nil nil nil)
            (comp-field self slotname (gen-window (om-random min max)))
            )

(defmethod! comp-bandfilter ((self component) (slotname string) (minval number) (maxval number))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue minval) (> thevalue maxval))
                (remove-comp self)
              thevalue)
            ))

(defmethod! comp-bandfilter ((self component) (slotname string) (minval list) (maxval list))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue (nth  (index self) minval)) 
                    (> thevalue (nth  (index self) maxval)))
                (remove-comp self)
              thevalue)
            ))

(defmethod! comp-bandfilter ((self component) (slotname string) (minval string) (maxval string))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue (comp-field self minval)) 
                    (> thevalue (comp-field self maxval)))
                (remove-comp self)
              thevalue)
            ))

(defmethod! field-quantize ((self component) (slotname string) (interval number))
            :icon 04
            (comp-field self slotname (quantize (comp-field self slotname) interval))
            )

(defmethod! comp-quantize ((self component) (interval list))
            :icon 02
            (comp-list self (quantize (comp-list self) interval))
            )

(defmethod! field-perturbation ((self component) (slotname string) (amount number))
            :icon 04
            (comp-field self slotname (perturbation (comp-field self slotname) (* 0.01 amount)))
            )

(defmethod! comp-perturbation ((self component) (amount number))
            :icon 04
            (comp-list self (perturbation (comp-list self) (* 0.01 amount)))
            )

(defmethod! comp-perturbation ((self component) (amount list))
            :icon 04
            (comp-list self (print (perturbation (comp-list self) (om* 0.01 amount))))
            )


;catch exceptions
(defmethod! perturbation ((self t) number)
            self)
  
(defmethod! perturbation ((self 3D-trajectory) number)
            (traject-from-list
             (perturbation (x-points self) number)
             (perturbation (y-points self) number)
             (perturbation (z-points self) number)
             (times self)
             '3D-trajectory
             (decimals self)
             (sample-params self)
             (interpol-mode self)
             ))

;;; array-field methods %%%%%%%%%%%%%%%%%%%%%%%


(defmethod! field-reduce ((self class-array) (slotname string) (points integer) &key slotname-x (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (xvals (if slotname-x
                              (array-field self slotname-x)
                            (arithm-ser 0 (1- numcomps) 1)))
                   (yvals (array-field self slotname))
                   (curve (mat-trans (list xvals yvals))))
              (array-field self slotname (third (multiple-value-list (om-sample (second (mat-trans (reduce-n-points curve points precision))) numcomps))))
              ))

(defmethod! field-reduce ((self class-array) (slotname string) (points float) &key slotname-x (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (xvals (if slotname-x
                              (array-field self slotname-x)
                            (arithm-ser 0 (1- numcomps) 1)))
                   (yvals (array-field self slotname))
                   (curve (mat-trans (list xvals yvals))))
              (array-field self slotname (third (multiple-value-list (om-sample (second (mat-trans (reduce-points curve (* 0.01 (- 100 points))))) numcomps))))
              ))

(defmethod! slope-reduce ((self class-array) (slotname string) (points integer) &key slotname-x (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (xvals (if slotname-x
                              (array-field self slotname-x)
                            (arithm-ser 0 (1- numcomps) 1)))
                   (yvals (array-field self slotname))
                   (curve (mat-trans (list xvals yvals))))
              (array-field self slotname (third (multiple-value-list (om-sample (second (mat-trans (reduce-n-points curve points precision))) numcomps))))
              ))

#|
(defmethod! reduce-n-points ((self 3D-trajectory) n &optional (precision 10))
     (let ((reduced-points (print (reduce-n-points (point-pairs self) n precision))))
       (traject-from-list (mapcar 'car reduced-points)
                          (mapcar 'cadr reduced-points)
                          (mapcar 'caadr reduced-points)
                          (times self)
                          (type-of self)
                          (decimals self)
                          (sample-params self)
                          (interpol-mode self)))
     )
     |#


(defmethod! noncausal-filter ((self class-array) (slotname string) (points integer) &key slotname-x (precision 10))
            :icon 02
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (thebpfs (array-field self slotname))
                   )
              (array-field self slotname (loop for bpf in thebpfs collect
                                               (reduce-n-points bpf points precision))) 
              ))


;Need a Smoothing+Downsample! ->this can be constructed
;Maybe the 'stream-resample' should be simply a 'resample' that works on slots or components..

(defmethod! stream-resample ((self class-array) (slotname string) (downsampling number))
            :icon 04
           ; :initvals '(nilnil "lowpass" 3 1)
           ; :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))
              ;(loop for item in thedata do
             (array-field self slotname (om-sample thedata downsampling))
                    ;)
              )
            )

(defmethod! quantize ((self 3D-trajectory) interval)
            ;(when (and self interval)
                (let* ((xpoints (om* (om-round (om/ (x-points self) interval)) interval))
                       (ypoints (om* (om-round (om/ (y-points self) interval)) interval))
                       (zpoints (om* (om-round (om/ (z-points self) interval)) interval))
                       (theinstance 
                        (traject-from-list xpoints ypoints zpoints (times self) (type-of self) (decimals self) (sample-params self) (interpol-mode self)
                                       )))
            theinstance))


(defmethod! field-scale ((self class-array) (slotname string) &key minval maxval exp)
            :icon 04
            :initvals '(nil nil nil nil)
            (let ((thedata (array-field self slotname)))           
            (unless minval
              (setf minval (list-min thedata)))
            (unless maxval
              (setf maxval (list-max thedata)))
            (unless exp
              (setf exp 1.0))
            (array-field self slotname (om-scale-exp thedata minval maxval exp))
            ))


; what is the functionality of field-sort? 
(defmethod! field-sort ((self class-array) (slotname string) (test symbol) &key key)
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thedata (data self))
                   (trans-data (mat-trans thedata)) 
                   (thesorteddata (sort-list trans-data
                                             :test test 
                                             :key #'(lambda (trans-data) (nth (label2index self slotname) trans-data))))
                   (slot-data (mat-trans thesorteddata)))
              (loop for slot in slot-data
                    for i from 0 to (1- (length slot-data))
                    do
                   (array-field self (index2label self i) slot))
              self
              ))


; destructive manipulation
(defmethod! array-rep-filter ((self list) (slotname string))
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thearrayvals (multiple-value-list (array-vals self)))
                   (thearray (first thearrayvals))
                   (thecomplist (second thearrayvals))
                   (thevallist (array-field thearray slotname))
                   (replist (rep-p thevallist)))
              (loop for rep in replist
                    for comp in thecomplist
                    do
                    (if rep
                        (remove-comp comp)
                      comp)
                    ))
            )

(defmethod! array-delta-filter ((self list) (slotname string) (maxdelta number) &optional (mode '<))
            :icon 04
            :initvals '(nil nil nil '<)
            (let* ((thearrayvals (multiple-value-list (array-vals self)))
                   (thearray (first thearrayvals))
                   (thecomplist (second thearrayvals))
                   (thevallist (array-field thearray slotname))
                   (deltalist (print (x->dx thevallist))))
              (if (equal mode '<)
                  (loop for delta in deltalist
                        for comp in thecomplist
                    do
                    (if (> (abs delta) maxdelta)
                        (remove-comp comp)
                      comp)
                    )
                (loop for delta in deltalist
                      for comp in thecomplist
                      do
                      (if (< (abs delta) maxdelta)
                          (remove-comp comp)
                        comp)
                      )))
            )
#|
(defmethod! array-delta-filter ((self list) (slotname string) (maxdelta number))
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thearrayvals (multiple-value-list (array-vals self)))
                   (thearray (first thearrayvals))
                   (thecomplist (second thearrayvals))
                   (thevallist (array-field thearray slotname))
                   (deltalist (print (x->dx thevallist))))
              (loop for delta in deltalist
                    for comp in thecomplist
                    do
                    (if (> (abs delta) maxdelta)
                        (remove-comp comp)
                      comp)
                    ))
            )
|#

; careful... the slope filter works only on sorted lists.
#|
(defmethod! array-slope-filter ((self list) (slotname string) (maxslope number) (timeslist list))
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thearrayvals (multiple-value-list (array-vals self)))
                   (thearray (first thearrayvals))
                   (thecomplist (second thearrayvals))
                   (thevallist (array-field thearray slotname))
                   (deltalist (print (x->dx thevallist))))
                   ;(onsetlist (second thearray))
              (loop for delta in deltalist
                    for time in timeslist
                    for comp in thecomplist
                    do
                    (if (> (/ (abs delta) time) maxslope)
                        (remove-comp comp)
                      comp)
                    ))
            )
|#

; %%%% slot-methods %%%%%%%%                                


; if I add a method for bpfs, etc. to om-scale-exp it should work on components or rows depending on the processing function

(defmethod! slot-scale ((thedata list) &key minval maxval exp)
            :icon 04
            :initvals '(nil nil nil nil)
            (unless minval
              (setf minval (list-min thedata)))
            (unless maxval
              (setf maxval (list-max thedata)))
            (unless exp
              (setf exp 1.0))
            (om-scale-exp thedata minval maxval exp)
            )

(defmethod! slot-reduce ((thedata list) (points integer) &key (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length thedata))
                   (xvals (arithm-ser 0 (1- numcomps) 1))
                   (yvals thedata)
                   (curve (mat-trans (list xvals yvals))))
              (third (multiple-value-list (om-sample (second (mat-trans (reduce-n-points curve points precision))) numcomps)))
              ))

(defmethod! slot-reduce ((thedata list) (points float) &key (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length thedata))
                   (xvals (arithm-ser 0 (1- numcomps) 1))                            
                   (yvals thedata)
                   (curve (mat-trans (list xvals yvals))))
              (third (multiple-value-list (om-sample (second (mat-trans (reduce-points curve points))) numcomps)))
              ))



;;; HELPER FUNCTIONS ______________________

; I can use 'index' on the component here to save some ressources

(defmethod! array-field ((self class-array) (slotname string) &optional newvalues)
            :icon '(323)
            :initvals '(nil nil nil)
            :outdoc '("the slotvalues")
            (if  newvalues 
                (let ((newvalues (list! newvalues))
                      (indices (arithm-ser 0 (length newvalues) 1)))
                  (print newvalues)
                  (mapcar (lambda (thevalue theindex)
                            (comp-field (get-comp self theindex) slotname thevalue)) 
                          newvalues indices))
                (nth (label2index self slotname) (data self)))
            )

(defmethod! array-field ((self class-array) (slotname list) &optional newvalues)
            (flat (mapcar (lambda (theslotname thenewvalues)
                      (array-field self theslotname thenewvalues)) slotname newvalues)
            ))


(defmethod! get-comp-vals ((self component) (thefunction t) &rest slotnames)
            :icon '(323)
            (funcall thefunction 
                     (list self 
                           (mapcar (lambda (theslotname)
                                     (comp-field self theslotname)) 
                                   slotnames)
                           slotnames)
                     ))

(defmethod! comp-vals ((self list))
            :icon '(323)
            :numouts 3
            :outdoc '("compoonent" "list of lists of slot values" "list of lists of slotnames")
  (values (first self) (second self) (third self))
  )

(defmethod! array-vals ((self list))
            :icon '(323)
            :initvals '(nil nil)
            :numouts 2
            :outdoc '("array" "list of components")
            (values (first self) (second self))
            )

(defmethod! quantize ((self number) interval)
            (if (and self interval)
                (om* (om-round (om/ self interval)) interval)
              self
              ))

(defmethod! quantize ((self 3D-trajectory) interval)
            ;(when (and self interval)
                (let* ((xpoints (om* (om-round (om/ (x-points self) interval)) interval))
                       (ypoints (om* (om-round (om/ (y-points self) interval)) interval))
                       (zpoints (om* (om-round (om/ (z-points self) interval)) interval))
                       (theinstance 
                        (traject-from-list xpoints ypoints zpoints (times self) '3D-trajectory (decimals self) (sample-params self) (interpol-mode self)
                                       )))
            theinstance))

(defmethod! quantize ((self bpc) interval)
            ;(when (and self interval)
                (let* ((xpoints (om* (om-round (om/ (x-points self) interval)) interval))
                       (ypoints (om* (om-round (om/ (y-points self) interval)) interval))
                       (theinstance 
                        (simple-bpf-from-list xpoints ypoints 'bpc (decimals self)
                                       )))
            theinstance))

(defmethod! quantize ((self bpf) interval)
            ;(when (and self interval)
                (let* ((xpoints (om* (om-round (om/ (x-points self) interval)) interval))
                       (ypoints (om* (om-round (om/ (y-points self) interval)) interval))
                       (theinstance 
                        (simple-bpf-from-list xpoints ypoints 'bpf (decimals self)
                                       )))
            theinstance))


(defmethod! quantize ((self list) (interval list))
            (mapcar (lambda (thecomplist theintervals)
                      (quantize thecomplist theintervals))
                    self interval))


;; this would be cool with a 'depth' parameter... that is it will only return t after a number of repetitions...

(defun rep-p (thelist)
  (loop for item in thelist
        for otheritem in (x-append 'dummy (butlast thelist))
        collect
        (equal item otheritem)
        ))