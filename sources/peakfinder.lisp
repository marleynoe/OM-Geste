;*********************************************************************
; OM-SoX, (c) 2011-2013 Marlon Schumacher (CIRMMT/McGill University) *
;             http://sourceforge.net/projects/omsox/                 *
;                                                                    *
;  Multichannel Audio Manipulation and Functional Batch Processing.  *
;        DSP based on SoX - (c) C.Bagwell and Contributors           *
;                  http://sox.sourceforge.net/                       *
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

(defmethod! mag->lin ((magnitude number) (windowsize number) (wcoef number))
  :icon 141
  :indoc '("a value or list of values" "windowsize" "window coefficient")
  :initvals '(100 4096 1)
  :doc "Converts magnitude values of a power spectrum to linear gain"
 (* 2 (/ (sqrt magnitude) windowsize) wcoef))
 
(defmethod! mag->lin ((magnitude list) (windowsize number) (wcoef number))
  (mapcar (lambda (themagnitude)
            (mag->lin themagnitude windowsize wcoef)) magnitude)
  )


(defmethod! find-n-peaks ((points list) (numpeaks integer) (mode t) &key (test '>) (decimals 10) (deltaparam 1) (mindistance-factor 0.01) (filter-window 5) (filter-recursions 2))
              :icon '(233)
              :indoc '("a bpf or point-list" "Number of Peaks to find" "mode (Peak or Trough)" "sorting function for result" "decimals for calculation" "delta step to determine peak or trough")
              :initvals '(((0 1) (5 10) (10 1)) 1 peak > 10 1 0.01 5 2) ; no quote needed because it is already quoted
              :menuins '((2 (("peak" 'peak) ("trough" 'trough))) (3 ((">" >) ("<" <))))
              :doc "finds the n highest/lowest peaks or troughs in a bpf or point-list"
              :numouts 2
              (let* ((transpoints (mat-trans points))
                     (filteredpoints (list (first transpoints) (sox-median-filter-rec (second transpoints) filter-window filter-recursions)))
                     (thedxpoints (mat-trans (list (first filteredpoints) (x-append 0 (x->dx (second filteredpoints))))))
                     (thezerocrossings (remove-duplicates (y-transfer thedxpoints 0 decimals)))
                     (thecrossingfrequencies (x-transfer thedxpoints (om- thezerocrossings deltaparam) decimals))
                     (thepeakfrequencies 
                      (remove nil
                              (if (equal mode 'peak)
                                  (loop for item in thecrossingfrequencies 
                                        for x from 0 to (length thecrossingfrequencies)
                                        collect
                                        (omif (> item 0) nil x)
                                      )
                                (loop for item in thecrossingfrequencies 
                                      for x from 0 to (length thecrossingfrequencies)
                                      collect
                                       (omif (< item 0) nil x)
                                      ))))
                   (thepeakfrequenciesfiltered (remove nil (subs-posn thezerocrossings thepeakfrequencies nil)))
                   (thepeakpointlist (mat-trans (list thepeakfrequenciesfiltered (x-transfer points thepeakfrequenciesfiltered decimals))))
                  ; (thefilteredpeakpointlist (min-x-const-q-distance thepeakpointlist mindistance-factor))
                   (thefilteredpeakpointlist thepeakpointlist)
                   (thesortedpointlist 
                    (sort-list thefilteredpeakpointlist :test test :key 'second))
                   (thetranspeakpoints (mat-trans (first-n thesortedpointlist numpeaks)))
                   )
              (values-list (list (first thetranspeakpoints) (second thetranspeakpoints)))
              ))

(defmethod! find-n-peaks ((points bpf) (numpeaks integer) (mode t) &key (test '>) (decimals 10) (deltaparam 1) (mindistance-factor 0.01) (filter-window 5) (filter-recursions 2))
            (find-n-peaks (point-pairs points) numpeaks mode 
                          :test test :decimals decimals :deltaparam deltaparam 
                          :mindistance-factor mindistance-factor 
                          :filter-window filter-window 
                          :filter-recursions filter-recursions)
            )

(defun min-x-const-q-distance (list distance-factor)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first a) (first b)) (* (first a) distance-factor))
                                 )
                       ))

(defun min-x-distance (list distance)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first a) (first b)) distance)
                                 )
                       ))

(defun min-x-distance-reverse (list distance)
    (remove-duplicates list 
                       :test #'(lambda (a b) 
                                 (< (- (first b) (first a)) distance)
                                 )
                       ))

;=== averages a bpf-lib into a single bpf

(defmethod! bpf-average ((self list))
            :icon '(233)
            (let* ((xpoints (x-points (first self)))
                   (transypointlist (mat-trans 
                                     (loop for bpf in self
                                           collect 
                                           (y-points bpf))))
                  (averagedypoints (mapcar #'om-mean transypointlist)))
              (simple-bpf-from-list xpoints averagedypoints 'bpf (decimals (first self)))
              ))

(defmethod! bpf-average ((self bpf-lib))
            (bpf-average (bpf-list self)))

;;=======================================================================
; filter functions borrowed from om-fil
;;=======================================================================

(defun sox-debut (list elem)
  (loop for x in elem
        collect (om-mean (first-n list (1+ (om* x 2))))))

(defun sox-fin (list elem)
  (loop for x in elem
        collect (om-mean (first-n (reverse list) (1+ (om* x 2))))))

(defun sox-median-point (list N)
  (nth N (sort. list)))


(defmethod! sox-median-filter  ((data list) (window number) ) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   " Traditional Median filter, where <list> is the data flow to filter and <window> 
is the parameter to calculate the window delay. The <window delay> will be (2*window + 1).
We will use the median point of the effective window delay"
  (let ((aux data)
        (modulo (om// (1+ (om* window 2)) 2)))
    (x-append (first data)
                  (sox-debut data (arithm-ser 1
                                              (1- modulo)
                                              1))
                  
                  (loop for i from modulo to (om- (1- (length data)) modulo)
                        collect (sox-median-point
                                 (subseq data
                                         (om- i modulo)
                                         (om+ i (1+ modulo))) window))
                  (sox-fin data (arithm-ser 1
                                            (1- modulo)
                                            1))
                  (last-elem data))))

(defmethod! sox-median-filter-rec  ((data list) (window number) (deep number)) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Recursive sox-median-filter"
  (let ((aux data))
    
    (dotimes  (x deep aux)
      (setf aux (sox-median-filter aux window)))))

