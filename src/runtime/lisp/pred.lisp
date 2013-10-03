;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Predicates
;;;            - TYPEP, SUBTYPEP in sehr eingeschraenkter Form.
;;;            - Praedikate zum Testen, ob ein Objekt eines bestimmten Typs
;;;              vorliegt. Die Definition dieser Funktionen scheinen zirkulaer
;;;              zu sein. Der Rumpf wird allerdings von CLICC inline
;;;              uebersetzt, so dass der richtige C-Code erzeugt wird.
;;;            - Funktionen zum Testen gewisser Eigenschaften von Fixnums und
;;;              Arrays.
;;;
;;; $Revision: 1.10 $
;;; $Log: pred.lisp,v $
;;; Revision 1.10  1994/01/24  16:21:13  sma
;;; Not und null in LISP implementiert. Beide Funktionen muessen T
;;; und nicht nur einen Wahrheitswert `true' zurueckliefern. Da das Symbol
;;; T nicht mehr im Codegenerator bekannt ist, muessen diese Funktionen
;;; zunaechst hier implementiert werden bis eine neue Optimierungsfunktion
;;; geschrieben wurde.
;;;
;;; Revision 1.9  1993/12/09  17:13:20  sma
;;; rt::simple-array-p -> simple-array-p.
;;;
;;; Revision 1.8  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.7  1993/04/22  10:44:14  hk
;;;  (in-package "RUNTIME") -> (in-package "LISP"),
;;; exports eingefuegt, commonp gestrichen, viele Praedikate nach inline.lisp
;;;
;;; Revision 1.6  1993/04/07  09:13:52  hk
;;; Viele Praedikate nach inline.lisp
;;;
;;; Revision 1.5  1993/04/06  10:59:49  hk
;;; check-integer-low, check-integer-high, check-string-size,
;;; check-vector-size gestrichen, check-integer vereinfacht.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.10 $ eingefuegt
;;;
;;; Revision 1.3  1993/01/19  12:53:54  ft
;;; Vorkommen von STRIN-CHAR in TYPEP gestrichen.
;;;
;;; Revision 1.2  1992/07/28  11:35:30  hk
;;; Neue Funktionen check-integer-{low,high}.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(typep subtypep not null))
(export '(rt::check-array rt::check-simple-array) "RT")

;;-----------------------------------------------------------------------------
;; TYPEP object type
;;-----------------------------------------------------------------------------
(defun typep (object type)
  (case type
    ((T) T)
    ((NIL) NIL)
    (fixnum
     (check-integer object most-negative-fixnum most-positive-fixnum))
    ((float short-float single-float double-float long-float) (floatp object))
    ((character standard-char) (characterp object))
    ((list) (listp object))
    ((null) (null object))
    ((cons) (consp object))
    (otherwise (error "(TYPEP ~S ~S) is not implemented" object type))))

;;-----------------------------------------------------------------------------
;; SUBTYPEP type1 type2
;;-----------------------------------------------------------------------------
(defun subtypep (type1 type2)
  (case type2
    ((T) t)
    (otherwise (error "(SUBTYPEP ~S ~S) is not implemented" type1 type2))))

;;------------------------------------------------------------------------------
;; Fuer weitere Praedikate siehe inline.lisp
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; NOT object / NULL object
;;------------------------------------------------------------------------------
(defun not (object)
  (if object nil t))

(defun null (object)
  (if object nil t))

;;-----------------------------------------------------------------------------
(defun check-integer (object low high)
  (and (integerp object)
       (<= low object high)))

;;-----------------------------------------------------------------------------
;; CHECK-ARRAY object element-type dimensions
;;-----------------------------------------------------------------------------
(defun rt:check-array (object element-type dimensions)
  (and (arrayp object)
       (check-array-internal object element-type dimensions)))

;;-----------------------------------------------------------------------------
;; CHECK-SIMPLE-ARRAY object element-type dimensions
;;-----------------------------------------------------------------------------
(defun rt:check-simple-array (object element-type dimensions)
  (and (simple-array-p object)
       (check-array-internal object element-type dimensions)))

;;------------------------------------------------------------------------------
(defun check-array-internal (object element-type dimensions)
  (and (or (eq element-type '*)
           (equal (array-element-type object)
                  (upgraded-array-element-type element-type)))
       (or (eq dimensions '*)
           (if (atom dimensions)
             (= (array-rank object) dimensions)
             (and (= (array-rank object) (length dimensions))
                  (let (dimension)
                    (dotimes (i (length dimensions) t)
                      (setq dimension (pop dimensions))
                      (unless (or (eq dimension '*)
                                  (= (array-dimension object i) dimension))
                        (return nil)))))))))
