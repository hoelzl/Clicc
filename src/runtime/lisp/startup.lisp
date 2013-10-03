;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem, Initialisierung
;;;
;;; $Revision: 1.12 $
;;; $Log: startup.lisp,v $
;;; Revision 1.12  1993/12/09  17:14:26  sma
;;; *package-array* und *keyword-package* werden jetzt in packg.lisp
;;; definiert.
;;;
;;; Revision 1.11  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.10  1993/04/22  10:41:32  hk
;;;  (in-package "RUNTIME") -> (in-package "LISP"),
;;; fast die gesamte Initialisierung findet jetzt in lisp.lisp statt,
;;; nur noch init-keysort, Error-Catcher und Aufruf des Hauptprogramms.
;;;
;;; Revision 1.9  1993/03/12  09:56:45  ft
;;; Anpassung an die geaenderte Repraesentation ungebundener Slot-Werte.
;;;
;;; Revision 1.8  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.12 $ eingefuegt
;;;
;;; Revision 1.7  1993/01/19  16:09:10  hk
;;; filesys-init
;;;
;;; Revision 1.6  1993/01/14  16:44:41  hk
;;; :par-spec fuer lisp-main eingefuegt.
;;;
;;; Revision 1.5  1992/12/16  12:14:05  ft
;;; Initialisierung von *unbound-slot*.
;;;
;;; Revision 1.4  1992/08/06  16:00:10  hk
;;; Aufruf von init-keysort eingefuegt.
;;;
;;; Revision 1.3  1992/07/29  14:17:32  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/07/06  11:38:27  hk
;;; string-char --> character.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(rt::startup) "RT")

;;------------------------------------------------------------------------------
;; Initialisierung und Aufruf der Toplevel forms des Hauptprogramms
;;------------------------------------------------------------------------------
(defun rt:startup ()
  (rt::init-keysort ':ALLOW-OTHER-KEYS)

  (catch 'ERROR-CATCHER
    (rt::init-main)
    0))


