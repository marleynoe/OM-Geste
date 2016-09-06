;OM-Geste, 2012-2016 McGill University
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

(defparameter *gdif-types* '(("XP30" ("x" "y" "z") "Position in three dimensions") 
                             ("XVA0" ("v-abs") "Velocity of an object independent from the direction of the movement.")
                             ("XA30" ("x" "y" "z") "3-dimensional velocity stream.")
                             ("XAA0" ("a-abs") "Acceleration of an object independent from direction.")
                             ("XP31" ("x-1" "y-1" "z-1")"Position in three dimensions, first finite difference.")
                             ;compat
                             ("XVEA" ("v-abs")"Velocity of an object independent from the direction of the movement.")
                             ("XVE3" ("x" "y" "z") "3-dimensional velocity stream.")
                             ("XEUL" ("x" "y" "z") "3-dimensional orientation in Euler format. E.g. an orientation described as three successive rotations along the cumulative Z, X and Z axes.")
                             ))

;import/Export

(defmethod! make-gdif-buffer ((self gesture-model) (streamlist list) &key nvts out options)
   :icon 639
   :indoc '("a gesture-model" "format options" "output pathname")
   :initvals '(nil nil nil nil t)
   :doc "Saves the contents of <self> as an SDIF file in <outpath>.

<self> is an SDIF-Buffer object or some other object having the SAVE-SDIF-FILE method implemented.
<options> are specific options depending on <self>.

If <outpath> is not specified, a pop-up dialog will open and allow to choose a destination pathname."
   ; little bit of a 'detour' here:
   (let* ((newmodel self);(segment-gesture self (list (first (times self)) (car (last (times self))))))
          (types (loop for element in *gdif-types* append
                       (list
                       (make-instance 'sdiftype :struct 'f :signature (first element) :description (print (list (list (makestring (first element)) (makestring (first element))))))
                       (make-instance 'sdiftype :struct 'm :signature (first element) :description (print (mapcar #'makestring (second element))))
                       )))
          (framelists
           (loop for data in (data newmodel)
                 for stream in streamlist
                 for i from 0 to (length streamlist)
                 collect
                 (data-to-frameslist (car data) i stream stream)))
          (thebuffer (make-instance 'sdif-buffer 
                                    :types (flat types)
                                    :nvts nvts
                                    :lframes framelists)))
     thebuffer
     )
   )
         

; better structure would be to have frametypes which contain these matrixtypes (this way it is synchronuous)
(defmethod! data-to-frameslist ((self 3d-trajectory) (streamid fixnum) (frametype string) (matrixtype string))
            ; should be done with "raw-sdifmatrix"
            (let ((timepoints (times self))
                  (xvals (x-points self))
                  (yvals (y-points self))
                  (zvals (z-points self)))
              (loop for time in timepoints 
                    for xval in xvals
                    for yval in yvals
                    for zval in zvals
                    collect
                    (let ((thematrix (make-instance 'sdifmatrix
                                                  :numcols 1
                                                  :signature matrixtype
                                                  )))
                      (setf (lcontrols thematrix)                                                    
                            (list (list 'x xval) (list 'y yval) (list 'z zval)))
                      (setf (data thematrix)                                                    
                            (list (list xval) (list yval) (list zval)))
                      (make-instance 'sdifframe
                                     :signature frametype
                                     :ftime time
                                     :streamid streamid
                                     :lmatrix thematrix
                                     ))
                    )
              )
            )

(defmethod! data-to-frameslist ((self bpf) (streamid fixnum) (frametype string) (matrixtype string))
            ; from the matrixtype the different fields should be deduced
            (let ((timepoints (x-points self))
                  (yvals (y-points self)))
              (loop for time in timepoints 
                    for yval in yvals
                    collect
                    (let ((thematrix (make-instance 'sdifmatrix
                                                  :numcols 1
                                                  :signature matrixtype
                                                  )))
                      (setf (lcontrols thematrix)                                                    
                            (list (list 'y yval)))
                      (setf (data thematrix)                                                    
                            (list (list yval)))
                      (make-instance 'sdifframe
                                     :signature frametype
                                     :ftime time
                                     :streamid streamid
                                     :lmatrix thematrix
                                     ))
                    )
              )
            )

; obsolete helper functions
(defmethod! get-begin-and-end ((self list))
            (list (get-begin (car self)) (get-end (car (last self))))
            )


(defmethod! get-begin ((self 3d-trajectory))
            (first (times self))
            )

(defmethod! get-begin ((self bpf))
            (first (x-points self))
            )

(defmethod! get-end ((self 3d-trajectory))
            (car (last (times self)))
            )

(defmethod! get-end ((self bpf))
            (car (last (x-points self)))
            )