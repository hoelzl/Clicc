;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem (19. Structures)
;;;
;;; $Revision: 1.14 $
;;; $Log: struct.lisp,v $
;;; Revision 1.14  1994/06/09  09:25:09  hk
;;; rt:struct-ref anders kodiert
;;;
;;; Revision 1.13  1994/06/02  14:18:57  hk
;;; Neue Funktionen rt:struct-printer und (setf rt:struct-printer)
;;; definiert.
;;;
;;; Revision 1.12  1994/05/05  14:51:11  sma
;;; Wenn in make-struct do in dolist umgeschrieben wird, wird die Funktion
;;; restlisten-optimiert.
;;;
;;; Revision 1.11  1994/01/24  16:18:11  sma
;;; rt:struct-type in LISP implementiert.
;;;
;;; Revision 1.10  1994/01/13  16:46:31  sma
;;; setf-Methoden statt set-*. Sourcecode verschönert/einfacher
;;; programmiert.
;;;
;;; Revision 1.9  1993/10/14  13:09:36  sma
;;; rt:new-struct nach structure.c verschoben
;;;
;;; Revision 1.8  1993/06/25  12:38:23  wg
;;; Symbol rt::included-struct aus dem Package RT exportiert.
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
;;; $Revision: 1.14 $ eingefuegt
;;;
;;; Revision 1.4  1993/01/11  14:38:35  hk
;;; structure -> struct
;;;
;;; Revision 1.3  1992/07/06  09:22:42  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/07/06  08:18:10  hk
;;; Neue Sysntax fuer declaim fun-spec.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(rt::struct rt::struct-typep rt::struct-type rt::make-struct rt::copy-struct
   rt::struct-ref rt::struct-constructor rt::struct-printer
   rt::included-struct) "RT")

;;------------------------------------------------------------------------------
;; Für Fehlermeldungen
;;------------------------------------------------------------------------------
(defconstant NO_STRUCT "~S is not a structure of type ~S")

;;------------------------------------------------------------------------------
;; RT::STRUCT Datentyp
;;------------------------------------------------------------------------------
(deftype rt:struct () '(satisfies rt::structp))

;;-----------------------------------------------------------------------------
;; RT::STRUCT-TYPEP object type
;;-----------------------------------------------------------------------------
(defun rt:struct-typep (object type)
  (if (rt::structp object)
      (let ((struct-type (rt::struct-type object)))
        (loop
         (when (eq type struct-type)
           (return object))
         (unless (setq struct-type (get struct-type 'rt::included-struct))
           (return nil))))))

;;------------------------------------------------------------------------------
;; RT::STRUCT-TYPE structure
;;------------------------------------------------------------------------------
(defun rt:struct-type (structure)
  (rt::structure-ref structure -1))

;;-----------------------------------------------------------------------------
;; RT::MAKE-STRUCT type &rest slot-values
;;-----------------------------------------------------------------------------
(defun rt:make-struct (type &rest slot-values)
  (let ((new-structure (rt::new-struct (length slot-values))))
    (setf (rt::structure-ref new-structure -1) type)
    (let ((i 0))
      (dolist (slot slot-values new-structure)
        (setf (rt::structure-ref new-structure i) slot)
        (incf i)))))

;;-----------------------------------------------------------------------------
;; RT::COPY-STRUCT structure type
;;-----------------------------------------------------------------------------
(defun rt:copy-struct (structure type)
  (unless (rt:struct-typep structure type)
    (error NO_STRUCT structure type))
  (let* ((structure-size (rt::struct-size structure))
         (copy-of-structure (rt::new-struct structure-size)))
    (setf (rt::structure-ref copy-of-structure -1) type)
    (dotimes (index structure-size copy-of-structure)
      (setf (rt::structure-ref copy-of-structure index)
            (rt::structure-ref structure index)))))

;;-----------------------------------------------------------------------------
;; RT::STRUCT-REF structure index type
;;-----------------------------------------------------------------------------
(defun rt:struct-ref (structure index type)
  (if (rt:struct-typep structure type)
      (rt::structure-ref structure index)
      (error NO_STRUCT structure type)))

;;-----------------------------------------------------------------------------
;; (SETF RT::STRUCT-REF) newvalue structure index type
;;-----------------------------------------------------------------------------
(defun (setf rt:struct-ref) (newvalue structure index type)
  (if (rt:struct-typep structure type)
      (setf (rt::structure-ref structure index) newvalue)
      (error NO_STRUCT structure type)))

;;-----------------------------------------------------------------------------
;; RT::STRUCT-CONSTRUCTOR symbol
;;-----------------------------------------------------------------------------
(defun rt:struct-constructor (symbol)
  (get symbol 'rt::struct-constructor))

;;-----------------------------------------------------------------------------
;; (SETF RT::STRUCT-CONSTRUCTOR) constructor symbol
;;-----------------------------------------------------------------------------
(defun (setf rt:struct-constructor) (constructor symbol)
  (setf (get symbol 'rt::struct-constructor) constructor))

;;-----------------------------------------------------------------------------
;; RT::STRUCT-PRINTER symbol
;;-----------------------------------------------------------------------------
(defun rt:struct-printer (symbol)
  (get symbol 'rt::struct-printer))

;;-----------------------------------------------------------------------------
;; (SETF RT::STRUCT-PRINTER) print-function symbol
;;-----------------------------------------------------------------------------
(defun (setf rt:struct-printer) (print-function symbol)
  (setf (get symbol 'rt::struct-printer) print-function))

