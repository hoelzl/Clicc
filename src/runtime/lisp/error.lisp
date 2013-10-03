;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Funktionen ERROR, WARN, BREAK
;;;
;;; $Revision: 1.8 $
;;; $Log: error.lisp,v $
;;; Revision 1.8  1993/11/25  18:21:38  hk
;;; Vor Ausgabe der Fehlermeldung werden *print-length* und *print-level*
;;; gebunden.
;;;
;;; Revision 1.7  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.6  1993/04/22  10:47:09  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Exports eingefuegt.
;;;
;;; Revision 1.5  1993/03/23  14:39:25  ft
;;; Neue Funktion error-in die auch ausgibt, in welcher Funktion der Fehler
;;; auftrat.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.8 $ eingefuegt
;;;
;;; Revision 1.3  1992/12/18  13:16:32  ft
;;; Erweiterung um die Funktion break.
;;;
;;; Revision 1.2  1992/07/06  15:26:11  hk
;;; 'runtime --> "RUNTIME"
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(error warn break))

(defparameter *error-print-length* 6)
(defparameter *error-print-level* 6)

;;------------------------------------------------------------------------------
(defun error (format-string &rest args)
  (let ((*print-length* *error-print-length*)
        (*print-level* *error-print-level*))
  (format *error-output* "~&Error: ~?~%" format-string args))
  (throw 'ERROR-CATCHER -1))

;;------------------------------------------------------------------------------
(defun error-in (caller format-string &rest args)
  (error "~&Error in ~A: ~?~%" caller format-string args))

;;------------------------------------------------------------------------------
(defun warn (format-string &rest args)
  (let ((*print-length* *error-print-length*)
        (*print-level* *error-print-level*))
    (format *error-output* "~&Warning: ~?~%" format-string args)))

;;------------------------------------------------------------------------------
(defun break (&optional format-string &rest args)
  (format *error-output* "~&Break: call to the `break' function.~%~?~%" 
          format-string args)
  (if (y-or-n-p "Restart actions:~% y: continue~% n: abort~%")
      nil
    (throw 'ERROR-CATCHER -1)))
