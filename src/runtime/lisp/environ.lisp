;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (UNIX, Systemressourcen)
;;;
;;; $Revision: 1.1 $
;;; $Log: environ.lisp,v $
;;; Revision 1.1  1993/12/09  17:31:09  sma
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(rt::c-getenv rt::c-system) "RT")

;;------------------------------------------------------------------------------
;; C-GETENV string
;;------------------------------------------------------------------------------
(defun rt:c-getenv (string)
  (rt::c-getenv-internal (string-to-simple-string string)))

;;------------------------------------------------------------------------------
;; C-SYSTEM string
;;------------------------------------------------------------------------------
(defun rt:c-system (string)
  (rt::c-system-internal (string-to-simple-string string)))
