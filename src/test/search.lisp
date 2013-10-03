;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : 13.8 A CLOS Example: Searching Tools
;;;            from P. Norvig "Paradigms of Artificial Intelligence Programming"
;;;            Implementation einiger Suchstrategien mit dem objektorientierten
;;;            Ansatz.
;;;
;;; $Revision: 1.4 $
;;; $Log: search.lisp,v $
;;; Revision 1.4  1993/05/11  05:58:17  ft
;;; Anpassung der Around-Methode zu problem-combiner an die strikte
;;; Definition von SUBSEQ wie sie Lucid Common Lisp und CLICC verwenden.
;;;
;;; Revision 1.3  1993/05/11  05:47:48  ft
;;; Definition der Klasse trip-problem korrigiert.
;;;
;;; Revision 1.2  1993/05/04  12:12:13  ft
;;; Bösen Fehler beseitigt: Los Angeles falsch geschrieben.
;;;
;;; Revision 1.1  1993/03/25  10:05:36  ft
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; The basic class, PROBLEM, contains a single-instance variable to hold
;; unexplored states of the problem.
;;------------------------------------------------------------------------------
(defclass problem ()
  ((states :initarg :states :accessor problem-states)))

;;------------------------------------------------------------------------------
;; The function SEARCHER is similar tothe function TREE-SEARCH of section 6.4.
;;------------------------------------------------------------------------------
(defconstant fail nil "Indicates pat-match failure.")

(defmethod searcher ((prob problem))
  "Find a state that solves the search problem."
  (cond ((no-states-p prob) fail)
        ((goal-p prob) (current-state prob))
        (t (let ((current (pop-state prob)))
             (setf (problem-states prob)
                   (problem-combiner
                    prob
                    (problem-successors prob current)
                    (problem-states prob))))
           (searcher prob))))

(defmethod current-state ((prob problem))
  "The current state is the first of the possible states."
  (first (problem-states prob)))

(defmethod pop-state ((prob problem))
  "Remove and return the current state."
  (pop (problem-states prob)))

(defmethod no-states-p ((prob problem))
  "Are there any more unexplored states?"
  (null (problem-states prob)))

;; Print Debugging Information
(defmethod searcher :before ((prob problem))
           (dbg 'search "~&;; Search: ~a" (problem-states prob)))

(defvar *dbg-ids* nil "Identifiers used by dbg.")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line T)
    (apply #'format T format-string args)))

;; Umbenannt: debug -> n-debug
(defun n-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

;; Umbenannt: undebug -> n-undebug
(defun n-undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;; Enable Debug Mode
;; (n-debug 'search)

;;------------------------------------------------------------------------------
;; EQL-PROBLEM
;;------------------------------------------------------------------------------
(defclass eql-problem (problem)
  ((goal :initarg :goal :reader problem-goal)))

(defmethod goal-p ((prob eql-problem))
  (eql (current-state prob) (problem-goal prob)))

;;------------------------------------------------------------------------------
;; Basic searching strategies: depth-first and breadth-first
;;------------------------------------------------------------------------------
(defclass dfs-problem (problem) ()
   (:documentation "Depth-first search problem."))

(defclass bfs-problem (problem) ()
   (:documentation "Breadth-first search problem."))

(defmethod problem-combiner ((prob dfs-problem) new old)
  "Depth-first search looks at new states first."
  (append new old))

(defmethod problem-combiner ((prob bfs-problem) new old)
  "Breadth-first search looks at old states first."
  (append old new))

;;------------------------------------------------------------------------------
;; Problem domain
;;------------------------------------------------------------------------------
(defclass binary-tree-problem (problem) ())

(defmethod problem-successors ((prob binary-tree-problem) state)
  (let ((n (* 2 state)))
    (list n (+ n 1))))

;;------------------------------------------------------------------------------
;; Test: binary tree problem with breadth-first search;
;;       the expected result is 12
;;------------------------------------------------------------------------------
(defclass binary-tree-eql-bfs-problem
    (binary-tree-problem eql-problem bfs-problem) ())

(clicc-test "search1"
            ";;; Search a Solution for the Binary-Tree-Problem"
            (((searcher (make-instance 'binary-tree-eql-bfs-problem
                                 :states '(1) :goal 12))
              12)))

;;------------------------------------------------------------------------------
;; Best-First Search
;;------------------------------------------------------------------------------
(defclass best-problem (problem) ()
          (:documentation "A Best-first search problem."))

(defmethod problem-combiner ((prob best-problem) new old)
  "Best-first search sorts new and old according to cost-fn."
  (sort (append new old) #'<
        :key #'(lambda (state) (cost-fn prob state))))

(defmethod cost-fn ((prob eql-problem) state)
  (abs (- state (problem-goal prob))))

;;------------------------------------------------------------------------------
;; Beam Search
;;------------------------------------------------------------------------------
(defclass beam-problem (problem)
  ((beam-width :initarg :beam-width :initform nil
               :reader problem-beam-width)))

(defmethod problem-combiner :around ((prob beam-problem) new old)
   (let ((p-b-w (problem-beam-width prob))
         (primary-result (call-next-method)))
     (if (> (length primary-result) p-b-w)
         (subseq (call-next-method) 0 (problem-beam-width prob))
         primary-result)))

;;------------------------------------------------------------------------------
;; another Test: binary tree problem with beam search;
;;               the expected result is 12
;;------------------------------------------------------------------------------
(defclass binary-tree-eql-best-beam-problem
  (binary-tree-problem eql-problem best-problem beam-problem)
  ())

(clicc-test "search2"
            ";;; Search a Solution for the Binary-Tree-Problem"
            (((searcher (make-instance 'binary-tree-eql-best-beam-problem
                                       :states '(1) :goal 12 :beam-width 3))
              12)))

;;------------------------------------------------------------------------------
;; one more Test: trip problem with beam search;
;;                the expected result is (SAN-FRANCISCO 122.26 37.47)
;;------------------------------------------------------------------------------
(defclass trip-problem (binary-tree-eql-best-beam-problem) ())

(defmethod cost-fn ((prob trip-problem) city)
  (air-distance (problem-goal prob) city))

(defmethod problem-successors ((prob trip-problem) city)
  (neighbors city))

;; CLICC kennt die defstruct option :type nicht
;;(defstruct (city (:type list)) name long lat)
(defmacro city-name (city) `(first  ,city))
(defmacro city-long (city) `(second ,city))
(defmacro city-lat  (city) `(third  ,city))

(defparameter *cities*
  '((Atlanta      84.23 33.45) (Los-Angeles  118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)
    (Chicago      87.37 41.50) (New-York       73.58 40.47)
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27)
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (remove-if-not #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))
  
(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities.
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a spere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidian distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

;; umgeschrieben, da CLICC den Typ Bruch nicht kennt
(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem deg 1) (/ 100 60))) pi (/ 1 180)))

(clicc-test "search3"
            ";;; Search a Solution for the Trip-Problem"
            (((searcher (make-instance 'trip-problem
                                       :states (list (city 'new-york))
                                       :goal (city 'san-francisco)
                                       :beam-width 1))
              (SAN-FRANCISCO 122.26 37.47))))

;;------------------------------------------------------------------------------

(provide "search")
