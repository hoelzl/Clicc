;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Traversieren von Zwischensprachkonstrukten
;;;
;;; $Revision: 1.21 $
;;; $Log: traverse.lisp,v $
;;; Revision 1.21  1993/07/16  09:50:24  uho
;;; Fuer Function als Specializer von #+(or CLISP CMU) auf #+PCL
;;; umgestellt, dito fuer #-(or CLISP CMU) auf #-PCL
;;;
;;; Revision 1.20  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.19  1993/05/22  11:29:13  kl
;;; Es wird nun auch die location-Komponente in setq-Ausdruecken traversiert.
;;;
;;; Revision 1.18  1993/05/14  12:34:15  kl
;;; Analyse der Parameter-Initialisierungsausdruecke eingebaut.
;;; Aufruf von (gensym analyse-mark-) in (gensym) geaendert.
;;;
;;; Revision 1.17  1993/05/13  13:38:48  hk
;;; Anpassung an CMU und CLISP.
;;;
;;; Revision 1.16  1993/05/03  10:35:27  jh
;;; ?fun-list in all-global-funs geaendert, damit die toplevel-forms ebenfalls
;;; traversiert werden.
;;;
;;; Revision 1.15  1993/04/16  10:56:25  jh
;;; Ueberfluessige Option entfernt.
;;;
;;; Revision 1.14  1993/04/15  13:34:08  jh
;;; *analyse-mark* mit Symbol vorbesetzt und Unsinn entfernt.
;;;
;;; Revision 1.13  1993/04/08  14:50:58  uho
;;; traverse-zs-Methode fuer functions eingefuehrt. Funktionen des Wirts-
;;; systems treten NUR in Macroexpansionsfunktionen auf!
;;;
;;; Revision 1.12  1993/03/16  16:54:24  jh
;;; Funktionen fuer die Handhabung der Analysemarken eingefuehrt.
;;;
;;; Revision 1.11  1993/02/24  14:14:16  jh
;;; Lokale Funktionen werden jetzt erst in der entsprechenden labels-form
;;; untersucht.
;;;
;;; Revision 1.10  1993/02/16  16:09:05  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.9  1993/02/11  13:27:07  jh
;;; mv-lambda eingebaut.
;;;
;;; Revision 1.8  1993/02/10  12:44:03  jh
;;; local-fun-list vom module wird jetzt benutzt.
;;;
;;; Revision 1.7  1993/01/26  11:08:32  jh
;;; Kommentar korrigiert.
;;;
;;; Revision 1.6  1993/01/21  14:13:33  jh
;;; Fehler bei tagged-forms beseitigt.
;;;
;;; Revision 1.5  1993/01/20  12:39:45  jh
;;; before- und after-funs eingebaut.
;;;
;;; Revision 1.4  1993/01/19  15:48:01  jh
;;; Fehler beseitigt.
;;;
;;; Revision 1.3  1993/01/19  13:04:37  jh
;;; Erweiterung der Traversiermoeglichkeiten.
;;;
;;; Revision 1.2  1993/01/12  15:11:55  kl
;;; Funktionsselektor eingebaut.
;;;
;;; Revision 1.1  1993/01/12  13:51:35  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Die folgenden Funktionen dienen zur Erzeugung und Benutzung von
;;  Analysemarken.
;;------------------------------------------------------------------------------

(defvar *analyse-mark* 'analyse-mark)

(defun new-analyse-mark ()
  (setq *analyse-mark* (gensym)))

(defun analysed-p (a-zws-object)
  (eq (?analysed a-zws-object) *analyse-mark*))

(defun mark-as-analysed (a-zws-object)
  (setf (?analysed a-zws-object) *analyse-mark*))

;;------------------------------------------------------------------------------
;; Globale Variablen fuer die Traversierung der Zwischensprache:
;;------------------------------------------------------------------------------

;; Analysepfad, um Rekursionen zu entdecken
(defvar *tr-analyzation-path*)


;; Listen der Funktionen, die beim Traversieren der Zwischensprache an jedem 
;; Knoten vor bzw. nach eintritt in die Teilausdruecke appliziert werden sollen.
(defvar *tr-before-funs* '())
(defvar *tr-after-funs* '())

;; Enthaelt das Praedikat, das bestimmt, ob der form-slot einer Applikation
;; traversiert werden soll.
(defvar *tr-app-form-p* #'(lambda (an-app) (declare (ignore an-app)) T))


(defmethod is-defined-fun-without-recursion-p ((a-fun fun))
  nil)

(defmethod is-defined-fun-without-recursion-p ((a-defined-fun defined-fun))
  (not (member a-defined-fun *tr-analyzation-path*)))


;; Enthaelt das Praedikat, das bestimmt, ob der body einer Funktion traversiert
;; werden soll.
(defvar *tr-fun-body-p* #'is-defined-fun-without-recursion-p)

;; Enthaelt das Praedikat, das bestimmt, welche Funktionen bereits an der Stelle
;; ihrer Definition traversiert werden sollen. (Von welchen Funktionen aus
;; startet die Traversierung?)
(defvar *tr-fun-selector* #'(lambda (a-fun) (declare (ignore a-fun)) T))

;;------------------------------------------------------------------------------
;; Traversiere das Modul und appliziere an jedem Knoten die in *tr-before-funs*
;; und *tr-after-funs* angegebenen Funktionen.
;;------------------------------------------------------------------------------
(defun traverse-module (a-module
                        &key
                        ((:before-funs *tr-before-funs*) *tr-before-funs*)
                        ((:after-funs *tr-after-funs*) *tr-after-funs*)
                        ((:fun-selector *tr-fun-selector*) *tr-fun-selector*)
                        ((:tr-app-form-p *tr-app-form-p*) *tr-app-form-p*)
                        ((:tr-fun-body-p *tr-fun-body-p*) *tr-fun-body-p*))

  (setf *tr-analyzation-path* '())

  (dolist (a-fun (?all-global-funs a-module))
    (when (funcall *tr-fun-selector* a-fun)
      (traverse-function a-fun))))

;;------------------------------------------------------------------------------
;; Es folgen die Methoden zu der generischen Funktion `traverse-zs', die
;; zu jedem Zwischensprachkonstrukt die entsprechenden Sprachteile analysiert.
;;------------------------------------------------------------------------------
(defgeneric traverse-zs (analysable-object))


;;------------------------------------------------------------------------------
;; Diese Methode wird fuer die verschiedenen Zwischensprachausdruecke geeignet
;; spezialisiert.
;;------------------------------------------------------------------------------
(defmethod traverse-zs ((anything T))
  #+PCL
  (when (functionp anything)
    (return-from traverse-zs anything))
  (internal-error 'traverse-zs "can't traverse ~S." anything))


;;------------------------------------------------------------------------------
;; In Makroexpansionsfunktionen und auch NUR dort treten auch Funktionen des
;; Wirtssystems auf. Beim Traversieren werden sie ignoriert. 
;;------------------------------------------------------------------------------
#-PCL                        ; siehe (traverse-zs T)
(defmethod traverse-zs ((a-function function)))

(defmethod traverse-zs ((a-zws-object zws-object)))


;;------------------------------------------------------------------------------
;; Wenn *tr-fun-body-p* erfuellt ist, wird der Rumpf der Funktion traversiert.
;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-fun fun))
  (when (funcall *tr-fun-body-p* a-fun)
    (traverse-function a-fun)))


;;------------------------------------------------------------------------------
;; An jedem Zwischensprachknoten werden die Funktionen aus *tr-before-funs*
;; und *tr-after-funs* auf den Knoten appliziert.
;;------------------------------------------------------------------------------
(defmethod traverse-zs :before ((a-zws-object zws-object))
           (dolist (a-function *tr-before-funs*)
             (funcall a-function a-zws-object)))

(defmethod traverse-zs :after ((a-zws-object zws-object))
           (dolist (a-function *tr-after-funs*)
             (funcall a-function a-zws-object)))


;;------------------------------------------------------------------------------
(defmethod traverse-function ((a-form form)))

;;------------------------------------------------------------------------------
;; Die Funktion a-fun wird in den Analysepfad eingetragen und ihr Rumpf
;; traversiert.
;;------------------------------------------------------------------------------
(defmethod traverse-function ((a-fun fun))
  (let ((*tr-analyzation-path*  (cons a-fun *tr-analyzation-path*))
        (params                 (?params a-fun)))
    (dolist (param (append (?opt-list params) (?key-list params)))
      (traverse-zs (?init param)))
    (traverse-zs (?body a-fun))))


;;------------------------------------------------------------------------------
;; Wenn *tr-app-form-p* erfuellt ist, wird die form der Applikation traversiert.
;;------------------------------------------------------------------------------
(defmethod traverse-zs ((an-app app))
  (mapc #'traverse-zs (?arg-list an-app))
  (when (funcall *tr-app-form-p* an-app)
    (traverse-zs (?form an-app))))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-setq-form setq-form))
  (traverse-zs (?location a-setq-form))
  (traverse-zs (?form a-setq-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-progn-form progn-form))
  (mapc #'traverse-zs (?form-list a-progn-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((an-if-form if-form))
  (traverse-zs (?pred an-if-form))
  (traverse-zs (?then an-if-form))
  (traverse-zs (?else an-if-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-switch-form switch-form))
  (traverse-zs (?form a-switch-form))
  (mapc #'traverse-zs (?case-list a-switch-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-labeled-form labeled-form))
  (traverse-zs (?value a-labeled-form))
  (traverse-zs (?form a-labeled-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-labels-form labels-form))
  (dolist (a-fun (?fun-list a-labels-form))
    (when (funcall *tr-fun-selector* a-fun)
      (traverse-function a-fun)))
  (traverse-zs (?body a-labels-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-let*-form let*-form))
  (mapc #'traverse-zs (?init-list a-let*-form))
  (traverse-zs (?body a-let*-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-let/cc-form let/cc-form))
  (traverse-zs (?body a-let/cc-form)))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-tagbody-form tagbody-form))
  (traverse-zs (?first-form a-tagbody-form))
  (mapc #'traverse-zs (mapcar #'?form (?tagged-form-list a-tagbody-form))))

;;------------------------------------------------------------------------------
(defmethod traverse-zs ((a-mv-lambda mv-lambda))
  (traverse-zs (?body a-mv-lambda))
  (traverse-zs (?arg a-mv-lambda)))

;;------------------------------------------------------------------------------

(provide "traverse")
