;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Funktionen EQUAL, EQUALP
;;;
;;; $Revision: 1.8 $
;;; $Log: equal.lisp,v $
;;; Revision 1.8  1994/02/02  09:41:32  hk
;;; Deklaration simp-when-some-arg-not-cons/pathn/string/bitv für equal eingefügt.
;;;
;;; Revision 1.7  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.6  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.5  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.8 $ eingefuegt
;;;
;;; Revision 1.4  1993/01/11  15:28:18  hk
;;; structure -> struct
;;;
;;; Revision 1.3  1993/01/11  15:24:18  hk
;;; structure -> struct
;;;
;;; Revision 1.2  1992/07/06  15:28:09  hk
;;; 'runtime --> "RUNTIME"
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(equal equalp))

;;------------------------------------------------------------------------------
;; Einschraenkungen:
;; - BITVECTOR: entfaellt
;; - PATHNAME : entfaellt
;;------------------------------------------------------------------------------
(defun equal (x y)
  (declare (:simp-when-some-arg-not-cons/pathn/string/bitv eql))
  (typecase x
    (cons (and (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y))))
    (string (and (stringp y) (string= x y)))
    (T (eql x y))))

;;------------------------------------------------------------------------------
;; Einschraenkungen:
;; - HASHTABLE: entfaellt
;;------------------------------------------------------------------------------
(defun equalp (x y)
  (typecase x
    (character (and (characterp y) (char-equal x y)))
    (number (and (numberp y) (= x y)))
    (cons (and (consp y) (equalp (car x) (car y)) (equalp (cdr x) (cdr y))))
    (vector (and (vectorp y)
                 
                 ;; LENGTH beachtet Fill-Pointer
                 ;;-----------------------------
                 (eql (length x) (length y))
                 (dotimes (i (length x) t)
                   (unless (equalp (aref x i) (aref y i))
                     (return nil)))))
    (array (and (arrayp y) (eql (array-rank x) (array-rank y))
                (dotimes (i (array-rank x) t)
                  (unless (eql (array-dimension x i) (array-dimension y i))
                    (return nil)))
                (dotimes (i (array-total-size x) t)
                  (unless (equalp (row-major-aref x i) (row-major-aref y i))
                    (return nil)))))
;;  (hashtable ...)
    (rt:struct (and (rt::structp y)
                 (eq (rt::struct-type x) (rt::struct-type y))
                 (let ((type (rt::struct-type x)))
                   (dotimes (i (rt::struct-size x) t)
                     (unless (equalp (rt::struct-ref x i type)
                                     (rt::struct-ref y i type))
                       (return nil))))))

    (T (eq x y))))
