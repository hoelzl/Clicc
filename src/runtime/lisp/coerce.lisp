;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Funktion COERCE
;;;
;;; $Revision: 1.6 $
;;; $Log: coerce.lisp,v $
;;; Revision 1.6  1993/12/14  12:38:39  sma
;;; coerce für simple-string und simple-vector erweitert.
;;;
;;; Revision 1.5  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.4  1993/05/14  13:33:13  hk
;;; Funktionalitaet in die Funktion character ausgelagert.
;;;
;;; Revision 1.3  1993/04/22  10:47:09  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Exports eingefuegt.
;;;
;;; Revision 1.2  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.6 $ eingefuegt
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(coerce))

;;------------------------------------------------------------------------------
;; Einschraenkungen:
;; - SEQUENCE : siehe CONCATENATE
;; - CHARACTER: keine Umwandlung INTEGER --> CHARACTER (CLtL, 2. Ed., S. 64)
;; - COMPLEX  : entfaellt
;; - FUNCTION : SYMBOL --> FUNCTION nicht implementiert
;;              (LAMBDA ...) --> FUNCTION entfaellt
;;------------------------------------------------------------------------------
(defun coerce (object type)
  (case type
    (list (if (listp object)
              object
              (concatenate 'list object)))
    (simple-string (if (simple-string-p object)
                       object
                       (concatenate 'simple-string object)))
    (string (if (stringp object)
                object
                (concatenate 'string object)))
    (simple-vector (if (simple-vector-p object)
                       object
                       (concatenate 'simple-vector object)))
    (vector (if (vectorp object)
                object
                (concatenate 'vector object)))
    (character (character object))
    ((t) object)
    (t (error "cannot coerce to ~S" type))))



