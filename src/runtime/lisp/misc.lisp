;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (25. Miscellaneous Features)                
;;;
;;; $Revision: 1.5 $
;;; $Log: misc.lisp,v $
;;; Revision 1.5  1993/12/17  14:33:13  hk
;;; Dummy-Funktionen DOCUMENTATION und (SETF DOCUMNTATION) definiert.
;;;
;;; Revision 1.4  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.3  1993/04/22  10:47:09  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Exports eingefuegt.
;;;
;;; Revision 1.2  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.5 $ eingefuegt
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export '(identity documentation))

;;-----------------------------------------------------------------------------
;; IDENTITY object
;;-----------------------------------------------------------------------------
(defun identity (object) object)

;;------------------------------------------------------------------------------
;; DOCUMENTATION symbol doc-type
;; a dummy function, nil means that no documentation exists.
;;------------------------------------------------------------------------------
(defun documentation (symbol doc-type)
  (declare (ignore symbol doc-type))
  nil)

;;------------------------------------------------------------------------------
;; (setf DOCUMENTATION) new-doc symbol doc-type
;; a dummy function, the new documentation is not used to update anything
;;------------------------------------------------------------------------------
(defun (setf documentation) (new-doc symbol doc-type)
  (declare (ignore symbol doc-type))
  new-doc)

