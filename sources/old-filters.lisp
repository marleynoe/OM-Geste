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

; OLD FILTER FUNCTIONS

(in-package :om)

(defmethod! field-lowpass ((self class-array) (slotname string) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nilnil "lowpass" 3 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname (filtres::low-pass-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "median")
                     (array-field self slotname (filtres::median-filter-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "mean")
                     (array-field self slotname (filtres::mean-filter-rec thedata windowsize recursion-depth)))
                    ))
              )

(defmethod! smoothing-filter ((self class-array) (slotname string) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil nil "lowpass" 3 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname 
                                  (loop for item in thedata collect
                                  (traject-from-list (filtres::low-pass-rec (x-points item) windowsize recursion-depth)
                                                     (filtres::low-pass-rec (y-points item) windowsize recursion-depth) 
                                                     (filtres::low-pass-rec (z-points item) windowsize recursion-depth) 
                                                     (times item) '3D-trajectory (decimals item) (sample-params item) (interpol-mode item)
                                       ))))
                    ((equal filtertype "median")
                     (array-field self slotname 
                                  (loop for item in thedata collect
                                  (traject-from-list (filtres::median-filter-rec (x-points item) windowsize recursion-depth)
                                                     (filtres::median-filter-rec (y-points item) windowsize recursion-depth) 
                                                     (filtres::median-filter-rec (z-points item) windowsize recursion-depth) 
                                                     (times item) '3D-trajectory (decimals item) (sample-params item) (interpol-mode item)
                                       ))))
                    ((equal filtertype "mean")
                     (array-field self slotname 
                                  (loop for item in thedata collect
                                  (traject-from-list (filtres::mean-filter-rec (x-points item) windowsize recursion-depth)
                                                     (filtres::mean-filter-rec (y-points item) windowsize recursion-depth) 
                                                     (filtres::mean-filter-rec (z-points item) windowsize recursion-depth) 
                                                     (times item) '3D-trajectory (decimals item) (sample-params item) (interpol-mode item)
                                       ))))
                    )))

;maybe also for the high-pass and bandpass I should introduce scales (for rescaling the ranges of the data...

(defmethod! field-highpass ((self class-array) (slotname string) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil nil "lowpass" 3 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname (om- thedata (filtres::low-pass-rec thedata windowsize recursion-depth))))
                    ((equal filtertype "median")
                     (array-field self slotname (om- thedata (filtres::median-filter-rec thedata windowsize recursion-depth))))
                    ((equal filtertype "mean")
                     (array-field self slotname (om- thedata (filtres::mean-filter-rec thedata windowsize recursion-depth))))
                    ))
              )

(defmethod! field-bandpass ((self class-array) (slotname string) (filtertype string) (windowsize-h number) (recursion-depth-h number) (windowsize-l number) (recursion-depth-l number))
            :icon 04
            :initvals '(nil nil "lowpass" 3 1 2 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let* ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::low-pass-rec thedata windowsize-h recursion-depth-h)) 
                                       (om- thedata (filtres::low-pass-rec thedata windowsize-l recursion-depth-l)))))                                      
                    ((equal filtertype "median")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::median-filter-rec thedata windowsize-h recursion-depth-h))
                                       (om- thedata (filtres::median-filter-rec thedata windowsize-l recursion-depth-l)))))                
                    ((equal filtertype "mean")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::mean-filter-rec thedata windowsize-h recursion-depth-h)) 
                                       (om- thedata (filtres::mean-filter-rec thedata windowsize-l recursion-depth-l)))))
                    ))
              )

; %%%% slot-methods %%%%%%%%

(defmethod! slot-lowpass ((thedata list) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil "lowpass" 3 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))         
              (cond ((equal filtertype "lowpass")
                     (filtres::low-pass-rec thedata windowsize recursion-depth))
                    ((equal filtertype "median")
                     (filtres::median-filter-rec thedata windowsize recursion-depth))
                    ((equal filtertype "mean")
                     (filtres::mean-filter-rec thedata windowsize recursion-depth))
                    ))

(defmethod! slot-highpass ((thedata list) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil "lowpass" 3 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mea(defmethod! array-delta-filter ((self list) (slotname string) (maxdelta number))
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
            )n" "mean"))))         
              (cond ((equal filtertype "lowpass")
                     (om- thedata (filtres::low-pass-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "median")
                     (om- thedata (filtres::median-filter-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "mean")
                     (om- thedata (filtres::mean-filter-rec thedata windowsize recursion-depth)))
                    ))

(defmethod! slot-bandpass ((thedata list) (filtertype string) (windowsize-h number) (recursion-depth-h number) (windowsize-l number) (recursion-depth-l number))
            :icon 04
            :initvals '(nil "lowpass" 3 1 2 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))         
            (cond ((equal filtertype "lowpass")
                   (om- (om- thedata (filtres::low-pass-rec thedata windowsize-h recursion-depth-h)) 
                        (om- thedata (filtres::low-pass-rec thedata windowsize-l recursion-depth-l))))    
                  ((equal filtertype "median")
                   (om- (om- thedata (filtres::median-filter-rec thedata windowsize-h recursion-depth-h))
                        (om- thedata (filtres::median-filter-rec thedata windowsize-l recursion-depth-l))))
                  ((equal filtertype "mean")
                   (om- (om- thedata (filtres::mean-filter-rec thedata windowsize-h recursion-depth-h)) 
                        (om- thedata (filtres::mean-filter-rec thedata windowsize-l recursion-depth-l))))
                  ))



