;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (17. Arrays)                                
;;;
;;; $Revision: 1.21 $
;;; $Log: array.lisp,v $
;;; Revision 1.21  1994/06/08  16:29:04  hk
;;; In vector-push-extend wird array-total-size nur noch berechnet, wenn
;;; das Array vergr"o"sert wird.
;;;
;;; Revision 1.20  1994/04/21  12:42:53  sma
;;; array-rmi-internal entfernt. Durch Restlisten-Optimierung wird die
;;; dort explizit kodierte Optimierung ebenfalls erzielt.
;;;
;;; Revision 1.19  1994/02/02  09:39:17  hk
;;; Deklaration :simp-when-n-args bei aref und (setf aref) eingefügt,
;;; Definition von vref und (setf vref) nach vorn gezogen, da sie in den
;;; Deklarationen vorkommen.
;;;
;;; Revision 1.18  1994/01/18  08:27:32  ft
;;; Neue Version von ADJUST-ARRAY, die auf row-major-aref basiert.
;;;
;;; Revision 1.17  1994/01/05  12:36:08  sma
;;; rt::make-plain-vector gelöscht. Dafür gibt es jetzt eine
;;; rt::make-vector-Funktion für -t, -fixnum, -float und -bit. -internal
;;; bei rt::(set-)pvref gelöscht. Neue lokale Lisp-Funktion
;;; make-plain-vector.
;;;
;;; Revision 1.16  1993/12/14  12:37:32  sma
;;; * Einführung des plain-vector-Typs. Dies ist der (neue) Name für die
;;; einfachen Vektoren des Laufzeitsystems. Ein simple-vector ist ein
;;; plain-vector mit element-code 0, d.h. er kann daten des Typs T
;;; aufnehmen. Deswegen Namensänderung und Umstellung einiger Typtests.
;;; * svref optimiert, wird jetzt teilweise inline-compiliert.
;;; * row-major-aref optimiert.
;;; * Funktion vector-length (Aufruf einzig aus length aus seq.lisp)
;;; optimiert. seq.lisp enthält jetzt keine Aufrufe von (besser) internen
;;; Funktionen aus array.lisp mehr.
;;;
;;; Revision 1.15  1993/12/09  16:55:15  sma
;;; Neue array-Repräsentation. Mehr Lisp, weniger C.
;;;
;;; Revision 1.14  1993/09/03  14:15:12  hk
;;; Fehler in array-in-bounds-p behoben.
;;;
;;; Revision 1.13  1993/09/03  14:07:28  hk
;;; Fehler in array-in-bounds-p behoben.
;;;
;;; Revision 1.12  1993/08/20  10:13:21  hk
;;; array-element-type prüft auf array Typ
;;;
;;; Revision 1.11  1993/08/19  13:38:19  hk
;;; array-in-bounds-p optimiert, array-rmi-internal optimiert und einige
;;; Fehlerabfragen entfernt.
;;;
;;; Revision 1.10  1993/07/15  10:20:21  hk
;;; Neue Funktionen sub-type-code-p und type-code-p eingefuegt, die auf
;;; internen Typkodierungen arbeiten. typep und subtypep werden nicht mehr
;;; verwendet.
;;;
;;; Revision 1.9  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.8  1993/05/08  18:14:56  hk
;;; Aufrufe von set-row-major-aref-internal, set-svref-internal und
;;;  set-fill-pointer-internal geaendert.
;;;
;;; Revision 1.7  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.6  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.21 $ eingefuegt
;;;
;;; Revision 1.5  1993/01/19  12:11:03  hk
;;; string-char gestrichen in upgraded-array-element-type und
;;; to-internal-element-type.
;;;
;;; Revision 1.4  1992/07/29  13:12:24  hk
;;; Character ist als Element-Typ von Strings zulaessig.
;;;
;;; Revision 1.3  1992/07/06  09:11:43  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/07/06  08:25:24  hk
;;; Neue Syntax fuer declaim fun-spec.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(arrayp simple-array-p stringp vectorp bit-array-p bit-vector-p
   make-array vector aref row-major-aref svref array-element-type
   upgraded-array-element-type array-rank array-dimension array-dimensions 
   array-total-size array-in-bounds-p array-row-major-index
   adjustable-array-p bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand
   bit-nor bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not
   array-has-fill-pointer-p fill-pointer vector-push vector-push-extend 
   vector-pop adjust-array))

(export 
 '(rt::vref) "RT")

;;------------------------------------------------------------------------------
;; Datenstrukturen
;;------------------------------------------------------------------------------

(defstruct (complex-base-array)
  (data       nil  :type 'array)
  (displaced  -1   :type 'fixnum))

(defstruct (complex-array (:include complex-base-array))
  (dims       nil  :type 'list))

(defstruct (complex-vector (:include complex-base-array))
  (length      0   :type 'fixnum)
  (fillptr    -1   :type 'fixnum))


;; C-Funktinen aus array.c
;; -----------------------
;; simple-vector-p (inline)
;; rt::plain-vector-p (inline)
;; rt::plain-vector-element-code    -> 0=T,1=fixnum,2=float,3=character,4=bit
;; rt::plain-vector-length (inline)
;; rt::make-vector-t
;; rt::make-vector-fixnum
;; rt::make-vector-float
;; rt::make-vector-char
;; rt::make-vector-bit
;; rt::svref-internal (inline)
;; rt::set-svref-internal (inline)
;; rt::pvref
;; rt::set-pvref
;; simple-bit-vector-p (inline)
;; rt::sbvref
;; rt::set-sbvref
;; rt::shrink-smstr

;;------------------------------------------------------------------------------
;; Typtests
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ARRAYP object
;;------------------------------------------------------------------------------
(defun arrayp (object)
  (or (rt::plain-vector-p object) (complex-base-array-p object)))

;;------------------------------------------------------------------------------
;; SIMPLE-ARRAY-P object
;;------------------------------------------------------------------------------
(defun simple-array-p (object)
  ;; jedes complex-array ist adjustable, was diese arrays als simple-arrays
  ;; ausschliesst. Nur ein plain-vector ist nicht adjustable, displaced
  ;; und hat keinen fill-pointer.
  (rt::plain-vector-p object))

;;------------------------------------------------------------------------------
;; STRINGP object
;;------------------------------------------------------------------------------
(defun stringp (object)
  (or (simple-string-p object) 
      (and (complex-vector-p object)
           (simple-string-p (displaced-data object)))))

;;------------------------------------------------------------------------------
;; VECTORP object
;;------------------------------------------------------------------------------
(defun vectorp (object)
  (or (rt::plain-vector-p object) (complex-vector-p object)))

;;------------------------------------------------------------------------------
;; BIT-ARRAY-P object
;;------------------------------------------------------------------------------
(defun bit-array-p (object)
  (or (simple-bit-vector-p object)
      (and (complex-base-array-p object)
           (simple-bit-vector-p (displaced-data object)))))

;;------------------------------------------------------------------------------
;; BIT-VECTOR-P object
;;------------------------------------------------------------------------------
(defun bit-vector-p (object)
  (or (simple-bit-vector-p object)
      (and (complex-vector-p object)
           (simple-bit-vector-p (displaced-data object)))))

;;------------------------------------------------------------------------------
;; lokal: DISPLACED-DATA array
;;------------------------------------------------------------------------------
(defun displaced-data (array)
  (setq array (complex-base-array-data array))
  (if (rt::plain-vector-p array)
      array
      (displaced-data array)))

;;------------------------------------------------------------------------------
;; Fehlermeldungen
;;------------------------------------------------------------------------------
(defconstant DISPLACED_NOT_WITH ":displaced-to option may not be used with ~A.")
(defconstant NO_ARRAY "~S should be of type ARRAY")
(defconstant OUT_OF_RANGE "The index ~S is not in the range [0, ~S)")
(defconstant NO_FILL_PTR "The argument ~A is not a vector with a fill-pointer.")

;;------------------------------------------------------------------------------
;; 17.1
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; MAKE-ARRAY dimensions
;;     &KEY :element-type :initial-element :initial-contents                   
;;          :adjustable   :fill-pointer                                        
;;          :displaced-to :displaced-index-offset  
;;------------------------------------------------------------------------------
(defun make-array (dimensions
                   &key
                   (element-type t)
                   (initial-element  nil init-elem-sp)
                   (initial-contents nil init-cont-sp)
                   adjustable
                   fill-pointer
                   displaced-to
                   (displaced-index-offset 0 displ-io-sp))
  (when (atom dimensions) 
    (setq dimensions (cons dimensions nil)))

  (let ((rank (length dimensions))
        (size (check-dimensions dimensions))
        (type-code (to-type-code element-type)))

    (when (and init-elem-sp
               (not (type-code-p initial-element type-code)))
      (error "The :initial-element, ~A, is of the wrong type; ~
                     it should be of type ~A"
             initial-element (to-element-type type-code)))

    (when init-cont-sp
      (when init-elem-sp
        (error ":initial-element and :initial-contents are both supplied."))
      (labels ((chk-shape-of-init-cont (dims init-cont)
                 (cond
                   ((null dims))
                   ((/= (first dims) (length init-cont))
                    (error "The :initial-contents argument, ~A,
                             isn't the correct shape or size
                             for an array of rank ~A with dimensions ~A"
                           initial-contents rank dimensions))
                   (t (mapc #'(lambda (reduced-init-cont)
                                (chk-shape-of-init-cont
                                 (rest dims)
                                 reduced-init-cont))
                            init-cont)))))
        (chk-shape-of-init-cont dimensions initial-contents)))
    (when displaced-to
      (when init-elem-sp
        (error DISPLACED_NOT_WITH :initial-element))
      (when init-cont-sp
        (error DISPLACED_NOT_WITH :initial-contents))
      (unless (arrayp displaced-to)
        (error "The :displaced-to argument is not an array."))
      (unless (sub-type-code-p
               type-code
               (array-element-type-internal displaced-to))
        (error "Cannot displace the array, because it has a different type."))
      (when (> size (array-total-size displaced-to))
        (error "Cannot displace the array, ~
                because the total size of the to-array is too small.")))
    
    ;; Die Groesse des neuen Array's + DISPLACED-INDEX-OFFSET darf 
    ;;  nicht groesser als die Groesse des Displaced To Array's sein 
    ;;---------------------------------------------------------------
    (when displ-io-sp
      (unless displaced-to
        (error "A :displaced-index-offset argument is given to make-array, ~
                        but there is no :displaced-to array"))
      (let ((size-of-displaced-to (array-total-size displaced-to)))
        (when (> size size-of-displaced-to)
          (error "The displaced-to array is smaller than the displaced array"))
        (unless (check-integer displaced-index-offset
                               0 (- size-of-displaced-to size))
          (error "The :displaced-index-offset argument, ~A, ~
                  is not in the linearized range [0, ~A) ~
                  of the :displaced-to array"
                 displaced-index-offset
                 (- (array-total-size displaced-to) size)))))
    
    (if (= rank 1)

        (let (vector)
          (when (and fill-pointer
                     (not (check-integer fill-pointer 0 size)))
            (error "The fill-pointer ~A must be an integer in the range [0, ~A]"
                   fill-pointer size))
        
          (setq vector
                (if displaced-to
                    (make-complex-vector 
                     :length size
                     :displaced displaced-index-offset
                     :data displaced-to
                     :fillptr (if fill-pointer fill-pointer -1))

                    (if (and (not fill-pointer)
                             (not adjustable))
                        (make-plain-vector size type-code)

                        (make-complex-vector 
                         :length size
                         :data (make-plain-vector size type-code)
                         :fillptr (if fill-pointer fill-pointer -1)))))

          (when init-elem-sp
            (fill vector initial-element))
          (when init-cont-sp
            (replace vector initial-contents))
          vector)
      
        (let (array)
          (when fill-pointer
            (error "Fill-pointers are only for 1-dimensional arrays."))
          (setq array
                (if displaced-to
                    (make-complex-array :dims dimensions
                                        :displaced displaced-index-offset
                                        :data displaced-to)
                    (make-complex-array :dims dimensions
                                        :data (make-plain-vector 
                                               size 
                                               type-code))))
          (when init-elem-sp
            (dotimes (i (array-total-size array))
              (setf (row-major-aref array i) initial-element)))
          (when init-cont-sp
            (labels ((set-initial-contents (rm-index dims init-cont)
                       (if (null dims)
                           (progn
                             (setf (row-major-aref array rm-index) init-cont)
                             1)
                           (do ((first-dim (first dims))
                                (rest-dims (rest  dims))
                                (i 0 (1+ i))
                                (offset 0))
                               ((>= i first-dim) offset)
                             (incf offset
                                   (set-initial-contents
                                    (+ rm-index offset)
                                    rest-dims
                                    (elt init-cont i)))))))
              (set-initial-contents 0 dimensions initial-contents)))
          array))))

;;------------------------------------------------------------------------------
;; lokal: MAKE-PLAIN-VECTOR
;;------------------------------------------------------------------------------
(defun make-plain-vector (size type-code)
  (case type-code
    (0 (rt::make-vector-t size))
    (1 (rt::make-vector-fixnum size))
    (2 (rt::make-vector-float size))
    (3 (rt::make-vector-char size #\Space))
    (4 (rt::make-vector-bit size))
    (T (error "make-plain-vector: unknow type-code"))))

;;------------------------------------------------------------------------------
;; lokal: CHECK-DIMENSIONS dimensions
;;------------------------------------------------------------------------------
(defun check-dimensions (dimensions)
  (do (dimension
       (size 1 (* size dimension)))
      ((null dimensions) size)
    (setq dimension (pop dimensions))
    (unless (check-integer dimension 0 array-dimension-limit)
      (error "A dimension argument to MAKE-ARRAY, ~A, is unusable ~
              since it is not a positive fixnum ~
              less or equal than ~A." dimension array-dimension-limit))))

;;------------------------------------------------------------------------------
;; lokal: TO-TYPE-CODE element-type
;;------------------------------------------------------------------------------
(defun to-type-code (element-type)
  (case element-type
    (fixnum 1)
    ((float short-float single-float double-float long-float) 2)
    ((standard-char character) 3)
    (bit 4)
    (otherwise 0)))

;;------------------------------------------------------------------------------
;; lokal: SUB-TYPE-CODE-P code1 code2
;;------------------------------------------------------------------------------
(defun sub-type-code-p (t1 t2)
  (or (= 0 t2) (= t1 t2)))

;;------------------------------------------------------------------------------
;; lokal: TYPE-CODE-P x code
;;------------------------------------------------------------------------------
(defun type-code-p (x code)
  (case code
    (0 t)
    (1 (typep x 'fixnum))
    (2 (typep x 'float))
    (3 (typep x 'character))
    (4 (typep x 'bit))
    (T (error "unknow type code ~A" code))))

;;------------------------------------------------------------------------------
;; VECTOR &REST objects
;;------------------------------------------------------------------------------
(defun vector (&rest objects)
  (make-array (length objects) :element-type t :initial-contents objects))

;;------------------------------------------------------------------------------
;; 17.2.
;;------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; VREF vector index                                                          
;; Source-Level Optimierung: (aref vector index) -> (vref vector index)       
;; Hat den Vorteil, dass kein &REST-Parameter benoetigt wird.              
;;-----------------------------------------------------------------------------
(defun rt:vref (vector index)
  (unless (vectorp vector)
    (error WRONG_TYPE vector 'vector))
  (row-major-aref vector index))

;;-----------------------------------------------------------------------------
;; (SETF VREF) newvalue array index
;;-----------------------------------------------------------------------------
(defun (setf rt:vref) (newvalue vector index)
  (unless (vectorp vector)
    (error WRONG_TYPE vector 'vector))
  (setf (row-major-aref vector index) newvalue))

;;------------------------------------------------------------------------------
;; AREF array &REST subscripts
;;------------------------------------------------------------------------------
(defun aref (array &rest subscripts)
  (declare (:simp-when-n-args 2 rt:vref))
  (row-major-aref array (apply #'array-row-major-index array subscripts)))

;;------------------------------------------------------------------------------
;; (SETF AREF) newvalue array &REST subscripts
;;------------------------------------------------------------------------------
(defun (setf aref) (newvalue array &rest subscripts)
  (declare (:simp-when-n-args 3 (setf rt:vref)))
  (setf (row-major-aref array (apply #'array-row-major-index array subscripts))
        newvalue))

;;------------------------------------------------------------------------------
;; SVREF simple-vector index
;;------------------------------------------------------------------------------
(defun svref (simple-vector index)
  (unless (simple-vector-p simple-vector)
    (error WRONG_TYPE simple-vector 'simple-vector))
  (unless (check-integer index 0 (1- (rt::plain-vector-length simple-vector)))
    (error OUT_OF_RANGE index (rt::plain-vector-length simple-vector)))
  (rt::svref-internal simple-vector index))

;;------------------------------------------------------------------------------
;; lokal: PVREF simple-vector index
;; spart den Typtest auf plain-vector-p
;;------------------------------------------------------------------------------
(defun pvref (simple-vector index)
  (unless (check-integer index 0 (1- (rt::plain-vector-length simple-vector)))
    (error OUT_OF_RANGE index (rt::plain-vector-length simple-vector)))
  (rt::pvref simple-vector index))

;;------------------------------------------------------------------------------
;; (SETF SVREF) newvalue simple-vector index
;;------------------------------------------------------------------------------
(defun (setf svref) (newvalue simple-vector index)
  (unless (simple-vector-p simple-vector)
    (error WRONG_TYPE simple-vector 'simple-vector))
  (unless (check-integer index 0 (1- (rt::plain-vector-length simple-vector)))
    (error OUT_OF_RANGE index (rt::plain-vector-length simple-vector)))
  (rt::set-svref-internal newvalue simple-vector index))

;;------------------------------------------------------------------------------
;; lokal: (SETF PVREF) newvalue simple-vector index
;; spart den Typtest auf plain-vector-p
;;------------------------------------------------------------------------------
(defun (setf pvref) (newvalue simple-vector index)
  (unless (check-integer index 0 (1- (rt::plain-vector-length simple-vector)))
    (error OUT_OF_RANGE index (rt::plain-vector-length simple-vector)))
  (unless (type-code-p newvalue (rt::plain-vector-element-code simple-vector))
    (error "Can't store ~A in a vector of type ~A" 
           newvalue (plain-vector-element-type simple-vector)))
  (rt::set-pvref newvalue simple-vector index))

;;------------------------------------------------------------------------------
;; 17.3.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ARRAY-ELEMENT-TYPE array
;;------------------------------------------------------------------------------
(defun array-element-type (array)
  (to-element-type (array-element-type-internal array)))

(defun array-element-type-internal (array)
  (cond
    ((rt::plain-vector-p array) (rt::plain-vector-element-code array))
    ((complex-base-array-p array) (rt::plain-vector-element-code
                                   (displaced-data array)))
    (T (error NO_ARRAY array))))

(defun plain-vector-element-type (vector)
  (rt::plain-vector-element-code vector))

(defun to-element-type (element-code)
  (case element-code
    (0 T)
    (1 'fixnum)
    (2 'float)
    (3 'character)
    (4 'bit)
    (T (error "unknown type code ~A" element-code))))

;;------------------------------------------------------------------------------
;; UPGRADED-ARRAY-ELEMENT-TYPE type
;;------------------------------------------------------------------------------
(defun upgraded-array-element-type (type)
  (case type
    (bit 'bit)
    (fixnum 'fixnum)
    ((standard-char character) 'character)
    ((float short-float single-float double-float long-float) 'float)
    (otherwise t)))

;;------------------------------------------------------------------------------
;; ARRAY-RANK array
;;------------------------------------------------------------------------------
(defun array-rank (array)
  (cond 
    ((or (rt::plain-vector-p array) (complex-vector-p array)) 1)
    ((complex-array-p array) (length (complex-array-dims array)))
    (T (error NO_ARRAY array))))

;;------------------------------------------------------------------------------
;; ARRAY-DIMENSION array axis-number
;;------------------------------------------------------------------------------
(defun array-dimension (array axis-number)
  (unless (arrayp array)
    (error NO_ARRAY array))
  (unless (check-integer axis-number 0 (1- (array-rank array)))
    (error "The value ~A is not suitable as an axis-number ~
            for an array with rank ~A" axis-number (array-rank array)))
  (cond
    ((rt::plain-vector-p array) (rt::plain-vector-length array))
    ((complex-vector-p array) (complex-vector-length array))
    ((complex-array-p array) (elt (complex-array-dims array) axis-number))
    (T (error "Unexpected error"))))
  

;;------------------------------------------------------------------------------
;; ARRAY-DIMENSIONS array
;;------------------------------------------------------------------------------
(defun array-dimensions (array)
  (cond 
    ((rt::plain-vector-p array) (list (rt::plain-vector-length array)))
    ((complex-vector-p array) (list (complex-vector-length array)))
    ((complex-array-p array) (complex-array-dims array))
    (T (error NO_ARRAY array))))
        
;;------------------------------------------------------------------------------
;; ARRAY-TOTAL-SIZE array
;;------------------------------------------------------------------------------
(defun array-total-size (array)
  (cond 
    ((rt::plain-vector-p array) (rt::plain-vector-length array))
    ((complex-vector-p array) (complex-vector-length array))
    ((complex-array-p array) (if (< (complex-array-displaced array) 0)
                                 (rt::plain-vector-length 
                                  (complex-array-data array))
                                 (apply #'* (complex-array-dims array))))
    (T (error NO_ARRAY array))))

;;------------------------------------------------------------------------------
;; ARRAY-IN-BOUNDS-P array &REST subscripts
;;------------------------------------------------------------------------------
(defun array-in-bounds-p (array &rest subscripts)
  (unless (= (array-rank array) (length subscripts))
    (error "Wrong number of subscripts for array ~a" array))
  (let ((i 0))
    (dolist (index subscripts t)
      (unless (check-integer index 0 (1- (array-dimension array i)))
        (return nil))
      (incf i))))

;;------------------------------------------------------------------------------
;; ARRAY-ROW-MAJOR-INDEX array &REST subscripts
;;------------------------------------------------------------------------------
(defun array-row-major-index (array &rest subscripts)
  (unless (arrayp array)
    (error NO_ARRAY array))
  (let ((i 0)
        (row-major-index 0))
    (dolist (index subscripts)
      (let ((dim (array-dimension array i)))
        (incf i)
        (setq row-major-index (+ (* row-major-index dim) index))))
    row-major-index))

;;------------------------------------------------------------------------------
;; ROW-MAJOR-AREF array index
;;------------------------------------------------------------------------------
(defun row-major-aref (array index)
  (cond
    ((simple-vector-p array) (svref array index))
    ((rt::plain-vector-p array) (pvref array index))
    ((complex-base-array-p array)
     (if (< (complex-base-array-displaced array) 0)
         (pvref (complex-base-array-data array) index)
         (row-major-aref (complex-base-array-data array) 
                         (+ index (complex-base-array-displaced array)))))
    (T (error NO_ARRAY array))))

;;------------------------------------------------------------------------------
;; (SETF ROW-MAJOR-AREF) newvalue array index
;;------------------------------------------------------------------------------
(defun (setf row-major-aref) (newvalue array index)
  (cond
    ((simple-vector-p array) (setf (svref array index) newvalue))
    ((rt::plain-vector-p array) (setf (pvref array index) newvalue))
    ((complex-base-array-p array) 
     (if (< (complex-base-array-displaced array) 0)
         (setf (pvref (complex-base-array-data array) index)
               newvalue)
         (setf (row-major-aref (complex-base-array-data array)
                               (+ index (complex-base-array-displaced array)))
               newvalue)))
    (T (error NO_ARRAY array))))

;;------------------------------------------------------------------------------
;; ADJUSTABLE-ARRAY-P array
;;------------------------------------------------------------------------------
(defun adjustable-array-p (array)
  (cond
    ((rt::plain-vector-p array) nil)
    ((complex-base-array-p array) t)
    (T (error NO_ARRAY array))))

;;------------------------------------------------------------------------------
;; 17.4
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; BIT bit-array &REST subscripts
;;------------------------------------------------------------------------------
(defun bit (bit-array &rest subscripts)
  (unless (bit-array-p bit-array)
    (error WRONG_TYPE bit-array 'bit-array))
  (row-major-aref bit-array 
                  (apply #'array-row-major-index bit-array subscripts)))

;;------------------------------------------------------------------------------
;; (SETF BIT) newbit bit-array &REST subscripts
;;------------------------------------------------------------------------------
(defun (setf bit) (newbit bit-array &rest subscripts)
  (unless (bit-array-p bit-array)
    (error WRONG_TYPE bit-array 'bit-array))
  (setf (row-major-aref bit-array 
                        (apply #'array-row-major-index bit-array subscripts))
        newbit))

;;------------------------------------------------------------------------------
;; SBIT bit-array &REST subscripts
;;------------------------------------------------------------------------------
(defun sbit (bit-array &rest subscripts)
  (unless (bit-array-p bit-array)
    (error WRONG_TYPE bit-array 'bit-array))
  (row-major-aref bit-array 
                  (apply #'array-row-major-index bit-array subscripts)))

;;------------------------------------------------------------------------------
;; (SETF SBIT) newbit bit-array &REST subscripts
;;------------------------------------------------------------------------------
(defun (setf sbit) (newbit bit-array &rest subscripts)
  (unless (bit-array-p bit-array)
    (error WRONG_TYPE bit-array 'bit-array))
  (setf (row-major-aref bit-array 
                        (apply #'array-row-major-index bit-array subscripts)) 
        newbit))

;;------------------------------------------------------------------------------
;; BIT-... bit-array1 bit-array2 &OPTIONAL result-bit-array
;;------------------------------------------------------------------------------
(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 0 bit-array1 bit-array2 result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 1 bit-array1 bit-array2 result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 2 bit-array1 bit-array2 result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 3 bit-array1 bit-array2 result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 4 bit-array1 bit-array2 result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 5 bit-array1 bit-array2 result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 6 bit-array1 bit-array2 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 7 bit-array1 bit-array2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 8 bit-array1 bit-array2 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-op 9 bit-array1 bit-array2 result-bit-array))

;;------------------------------------------------------------------------------
;; lokal: BIT-OP opcode bit-array1 bit-array2 result-bit-array
;;------------------------------------------------------------------------------
(defun bit-op (opcode bit-array1 bit-array2 result-bit-array)
  (unless (bit-array-p bit-array1)
    (error WRONG_TYPE bit-array1 'bit-array))
  (unless (bit-array-p bit-array2)
    (error WRONG_TYPE bit-array2 'bit-array))
  (unless (equal (array-dimensions bit-array1) (array-dimensions bit-array2))
    (error "~A and ~A must have the same dimensions" bit-array1 bit-array2))

  (rt::bitop opcode bit-array1 bit-array2
             ;;ObdA wird bit-array1 als Referenzarray benutzt
             (cond
               ((null result-bit-array)
                (make-array (array-dimensions bit-array1) :element-type 'bit))
               ((eq result-bit-array t) 
                bit-array1)
               (T (unless (bit-array-p result-bit-array)
                    (error WRONG_TYPE result-bit-array 'bit-array))
                  (unless (equal (array-dimensions bit-array1)
                                 (array-dimensions result-bit-array))
                    (error "~A must have the same dimensions as the other ~
                              bit-array parameters" result-bit-array))
                  result-bit-array))))
   
;;------------------------------------------------------------------------------
;; BIT-NOT bit-array &OPTIONAL result-bit-array
;;------------------------------------------------------------------------------
(defun bit-not (bit-array &optional result-bit-array)
  (bit-op 10 bit-array bit-array result-bit-array))

;;------------------------------------------------------------------------------
;; 17.5
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ARRAY-HAS-FILL-POINTER-P array
;;------------------------------------------------------------------------------
(defun array-has-fill-pointer-p (array)
  (unless (arrayp array)
    (error NO_ARRAY array))
  (and (complex-vector-p array)
       (not (< (complex-vector-fillptr array) 0))))

;;------------------------------------------------------------------------------
;; FILL-POINTER vector
;;------------------------------------------------------------------------------
(defun fill-pointer (vector)
  (unless (array-has-fill-pointer-p vector)
    (error NO_FILL_PTR vector))
  (complex-vector-fillptr vector))

;;------------------------------------------------------------------------------
;; (SETF FILL-POINTER) new-fp vector
;;------------------------------------------------------------------------------
(defun (setf fill-pointer) (new-fp vector)
  (unless (array-has-fill-pointer-p vector)
    (error NO_FILL_PTR vector))
  (unless (check-integer new-fp 0 (array-total-size vector))
    (error "The arg ~S given to SETF of FILL-POINTER is not in ~
            range for an array of total size ~S."
           new-fp (array-total-size vector)))
  (setf (complex-vector-fillptr vector) new-fp))

;;------------------------------------------------------------------------------
;; VECTOR-PUSH new-element vector
;;------------------------------------------------------------------------------
(defun vector-push (new-element vector)
  (let ((fp (fill-pointer vector)))
    (cond
      ((< fp (array-dimension vector 0))
       (setf (aref vector fp) new-element)
       (setf (fill-pointer vector) (1+ fp))
       fp)
      (t nil))))

;;------------------------------------------------------------------------------
;; VECTOR-PUSH-EXTEND new-element vector &OPTIONAL extension
;;------------------------------------------------------------------------------
(defun vector-push-extend (new-element vector &optional extension)
  (let ((fp (fill-pointer vector)))
    (when (>= fp (array-total-size vector))
      (adjust-array vector (if extension
                               (+ (array-total-size vector) extension)
                               (* 2 (array-total-size vector)))
                    :fill-pointer fp))
    (setf (aref vector fp) new-element)
    (setf (fill-pointer vector) (1+ fp))
    fp))

;;------------------------------------------------------------------------------
;; VECTOR-POP vector
;;------------------------------------------------------------------------------
(defun vector-pop (vector)
  (let ((fp (fill-pointer vector)))
    (when (zerop fp)
      (error "The fill pointer of the vector ~A is zero." vector))
    (decf fp)
    (setf (fill-pointer vector) fp)
    (aref vector fp)))

;;------------------------------------------------------------------------------
;; 17.6
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ADJUST-ARRAY array new-dimensions
;;    &KEY :element-type :initial-element :initial-contents                   
;;         :adjustable   :fill-pointer                                        
;;         :displaced-to :displaced-index-offset 
;;------------------------------------------------------------------------------
(defun adjust-array (array new-dimensions
                           &rest rest-args
                           &key (element-type nil elem-type-sp)
                           initial-element
                           initial-contents
                           (fill-pointer nil fill-pointer-sp)
                           displaced-to
                           displaced-index-offset)
  (declare (ignore initial-element displaced-to displaced-index-offset))
  (let ((original-element-type (array-element-type array))
        (original-dimensions   (array-dimensions   array))
        new-array
        subscripts)

    (labels ((dim-to-skips (dim)          ; wandelt array-dimensions in
               (if (null (cdr dim))       ; skips fuer r.-m.-aref um
                   dim
                   (let ((result (dim-to-skips (cdr dim))))
                     (cons (* (car dim) (car result)) result))))
             
           (rmi-copy (min-dim new-skip old-skip new-offset old-offset)
             (if (null (cdr min-dim))
                 
                 (dotimes (i (car min-dim))
                   (setf (row-major-aref new-array new-offset)
                         (row-major-aref array old-offset))
                   (incf new-offset)
                   (incf old-offset))
                 
                 (dotimes (i (car min-dim))
                   (rmi-copy (cdr min-dim) (cdr new-skip) (cdr old-skip)
                             (+ new-offset (* i (car new-skip)))
                             (+ old-offset (* i (car old-skip))))))))
                 
      (unless (adjustable-array-p array)
      (error "The 'array' argument given to ADJUST-ARRAY, ~A,
                     isn't an adjustable array." array))
      (when (atom new-dimensions) (setq new-dimensions (list new-dimensions)))
      (when (/= (length new-dimensions) (array-rank array))
        (error "The 'new-dimensions' argument given to ADJUST-ARRAY, ~A, ~
              is not the same rank as the argument array." new-dimensions))
      (when elem-type-sp
        (setq element-type (upgraded-array-element-type element-type))
        (unless (eq element-type original-element-type)
          (error "The 'element-type' argument to ADJUST-ARRAY, ~A ~
                  is inconsistent with that of the array to be adjusted [~A]."
                 element-type original-element-type)))
      (cond
        (fill-pointer-sp
         (unless (array-has-fill-pointer-p array)
           (error "A :fill-pointer argument was given to ADJUST-ARRAY, ~
   		 but the array being adjusted, doesn't have one."))
         (when (null fill-pointer)
           (setq fill-pointer (fill-pointer array))))
        (t (when (array-has-fill-pointer-p array)
             (setq fill-pointer (fill-pointer array)))))
      
      (setq new-array
            (apply #'make-array new-dimensions
                   :element-type original-element-type
                   :adjustable   T
                   :fill-pointer fill-pointer
                   rest-args))
      (unless initial-contents
        (rmi-copy (mapcar #'min new-dimensions original-dimensions)
                  (cdr (dim-to-skips new-dimensions))
                  (cdr (dim-to-skips original-dimensions)) 0 0))
      (displace-array array new-array))))

;;------------------------------------------------------------------------------
;; lokal: DISPLACE-ARRAY array new-array
;;------------------------------------------------------------------------------
(defun displace-array (array new-array)
  (cond 
    ((complex-array-p array)
     (setf (complex-array-data array) (complex-array-data new-array))
     (setf (complex-array-displaced array) (complex-array-displaced new-array))
     (setf (complex-array-dims array) (complex-array-dims new-array)))
    ((complex-vector-p array)
     (setf (complex-vector-data array) (complex-vector-data new-array))
     (setf (complex-vector-displaced array)(complex-vector-displaced new-array))
     (setf (complex-vector-length array) (complex-vector-length new-array))
     (setf (complex-vector-fillptr array) (complex-vector-fillptr new-array)))
    (T (error "internal displace-error")))
  array)

;;------------------------------------------------------------------------------
;; SHRINK-SIMPLE-STRING vector newsize
;;------------------------------------------------------------------------------
(defun shrink-simple-string (simple-string new-size)
  (unless (simple-string-p simple-string)
    (error WRONG_TYPE simple-string 'simple-string))
  (unless (check-integer new-size 0 (length simple-string))
    (error "~A is not a positive fixnum or is too big." new-size))
  (rt::shrink-smstr simple-string new-size))

;;------------------------------------------------------------------------------
;; STRING-TO-SIMPLE-STRING string
;;------------------------------------------------------------------------------
(defun string-to-simple-string (string)
  (cond
    ((simple-string-p string) string)
    ((stringp string) 
     (if (and (< (complex-vector-displaced string) 0)
              (< (complex-vector-fillptr string) 0))
         (complex-vector-data string)
         (copy-seq string)))
    (T (error WRONG_TYPE string 'string))))

;;------------------------------------------------------------------------------
;; VECTOR-LENGTH vector (für length aus seq.lisp)
;;------------------------------------------------------------------------------
(defun vector-length (vector)
  (cond
    ((rt::plain-vector-p vector) (rt::plain-vector-length vector))
    ((complex-vector-p vector) (if (< (complex-vector-fillptr vector) 0)
                                   (complex-vector-length vector)
                                   (complex-vector-fillptr vector)))
    (T (error WRONG_TYPE vector 'SEQUENCE))))
