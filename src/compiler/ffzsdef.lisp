;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Zwischensprachkonstrukte fuer das Foreign Function Interface
;;;
;;; $Revision: 1.12 $
;;; $Log: ffzsdef.lisp,v $
;;; Revision 1.12  1994/06/07  15:34:57  jh
;;; provide eingefuegt.
;;;
;;; Revision 1.11  1994/04/18  12:11:14  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Weitere Annotation fuer das Zwischensprachkonstrukt fuer
;;;   Call-Out-Functions.
;;;
;;; Revision 1.10  1993/12/16  16:36:29  pm
;;; Bezeichner bei foreign-fun umbenannt. Zur Vereinheitlichung.
;;;
;;; Revision 1.9  1993/11/03  11:46:27  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.8  1993/07/26  16:52:10  pm
;;; Erweiterungen um Strukturen
;;;
;;; Revision 1.7  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.6  1993/05/31  17:03:53  pm
;;; Struktur fuer call-in-Funktionen eingefuegt
;;;
;;; Revision 1.5  1993/03/10  12:48:47  pm
;;; Kleinigkeiten geaendert
;;;
;;; Revision 1.4  1993/02/16  15:23:12  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.3  1992/11/05  10:47:02  pm
;;; Ueberarbeitet
;;;
;;; Revision 1.2  1992/10/16  16:05:10  pm
;;; Zwischensprach-Konstrukt fuer Foreign-Functions
;;;
;;; Revision 1.1  1992/10/16  16:02:58  pm
;;; Initial revision
;;;-----------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; TODO-Liste
;;------------------------------------------------------------------------------
;; * in def-zws foreign-fun noch die Inforamtionen fuer die Seiteneffektanalyse
;;   eintragen.


;;------------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Fuer Foreign Functions wird ein eigenes Zwischensprachkonstrukt
;; eigerichtet, da keins der Vorhandenen geeignete Slots enthaelt.
;; Foreign-Functions sind naemlich keine Funktionen im Sinne von LISP.  Die
;; Slots im Einzelnen: 
;;
;; - arguments: Enthaelt die Typ-Signatur der Foreign-Function
;; - name: Name der zu generierenden FF
;; - callback: Die FF ruft selbst LISP-Funktionen auf.
;; - return-type: Der von der FF zurueckgelieferte Wert.
;;------------------------------------------------------------------------------
(defzws foreign-fun (form)
  (arguments :type list)
  (name :type string)
  (callback :initform nil :type bool)
  return-type
  ;; Annotation
  ;;-----------
  (par-spec :type integer)
  (symbol :type symbol)                 ; Name im Quelltext
  (other-sym :type symbol)
)


;;------------------------------------------------------------------------------
;; Call-In-Funktions sind LISP-Funktionen, die speziell behandelt werden
;; muessen. Da sie vorwaerts deklariert werden, koennen die Informationen
;; nicht ins Globale Environment unter operators abgelegt werden. Deshalb
;; muessen diese gemerkt werden. Dies geschieht durch folgendes lokales
;; Zwischensprachkonstrukt. Man haette auch eine Struktur dafuer verwenden
;; koennen, doch so geht es einfacher.
;; Fuer dieses Konstrukt muessen keine Methoden geschrieben werden.
;;------------------------------------------------------------------------------
(defzws call-in-fun ()
  (arguments :type list)
  (foreign-name :type string)
  return-type
)

(provide "ffzsdef")
