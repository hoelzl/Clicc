;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Funktionen Y-OR-N-P, YES-OR-NO-P
;;;
;;; $Revision: 1.4 $
;;; $Log: yesno.lisp,v $
;;; Revision 1.4  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.3  1993/04/22  10:36:08  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; (export '(y-or-n-p yes-or-no-p))
;;;
;;; Revision 1.2  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.4 $ eingefuegt
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(y-or-n-p yes-or-no-p))

;;------------------------------------------------------------------------------
(defun y-or-n-p (&optional format-string &rest args &aux answer)
  (when format-string
    (format *query-io* "~&~?" format-string args) )
  (loop
    (princ "(y or n):" *query-io*)
    (setq answer (read-line *query-io*))
    (cond
      ((string-equal "y" answer) (return t))
      ((string-equal "n" answer) (return nil))
      (T (print "Please answer y or n." *query-io*)) )))

;;------------------------------------------------------------------------------
(defun yes-or-no-p (&optional format-string &rest args &aux answer)
  (when format-string
    (format *query-io* "~&~?" format-string args) )
  (loop
    (princ "(yes or no):" *query-io*)
    (setq answer (read-line *query-io*))
    (cond
      ((string-equal "yes" answer) (return t))
      ((string-equal "no" answer) (return nil))
      (T (print "Please answer yes or no." *query-io*)) )))
