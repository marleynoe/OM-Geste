;OM-Geste, 2010-2015 IDMIL/McGill University
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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



(defparameter *om-geste-lib-path* (make-pathname :directory (pathname-directory *load-pathname*)))
;(clean-sources *om-geste-lib-path*)

#|
(defparameter *geste-classes* '(
                        ;"gesture-model"
                        "gesture-array"
                        ))

(defun load-geste-classes (subdir &optional pack)
  (loop for item in *geste-classes* do  
        (let ((itempath (om-relative-path subdir item)))
          (compile&load itempath)
          (let ((classname (intern (string-upcase item))))
            (when (and pack (find-class classname nil))              
              (addclass2pack classname pack))))
        )
  )

(load-geste-classes  '("sources" "classes") (find-library "OM-geste"))
|#
                      
(defun recursive-load-classes (dir &optional pack)
  (loop for item in (om-directory dir) do
        (if (directoryp item) 
            (let ((thepackage 
                   (or (find (car (last (pathname-directory item)))
                             (subpackages pack) :key 'name :test 'string-equal)
                       pack)))
               (recursive-load-classes item thepackage))    
        (when (string-equal (pathname-type item) "lisp")
          (load item)
          (addclass2pack (intern (string-upcase (pathname-name item))) pack)))))
   
(recursive-load-classes (om-relative-path '("sources" "classes") nil) *current-lib*)

(mapcar #'(lambda (file) (compile&load (om-relative-path '("sources") file )))
        '(
          "gesture-objects"
          "gesture-editor"
          "gesture-tools"
          "gesture-filters"
          "gesture-statistics"
          "mapping"
          ))

; here add the menu entries

;(sub-pack-name subpack-lists class-list function-list class-alias-list)


(om::fill-library '(
                    ("Filters" nil nil (slide-filter sma wma ema smm) nil)
                    ("Statistics" nil nil (rms magnitude variance stdev covariance correlation centroid) nil)
                    ("Utilities" nil nil (integrate differentiate find-peaks) nil)
                    ("Matrices" nil nil (add-row remove-row) nil)
                    ()
                    )
)


(print "
OM-Geste 0.1.0
")