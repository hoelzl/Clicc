;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Makros und Funktionen zur Handhabung der Typdeklarationen
;;;
;;; $Revision: 1.37 $
;;; $Log: tidecl.lisp,v $
;;; Revision 1.37  1994/05/13  12:33:10  hk
;;; das Makro dec-type ver"andert, so da"s If-Ausdrcke bereits bei der
;;; Expansion berechnet werden.
;;;
;;; Revision 1.36  1993/12/09  10:32:22  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.35  1993/11/12  14:14:55  kl
;;; require-Aufrufe nach timain.lisp verlegt.
;;;
;;; Revision 1.34  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.33  1993/05/18  16:13:08  kl
;;; Typen der Typinferenz werden jetzt durch Strukturen repraesentiert.
;;;
;;; Revision 1.32  1993/05/14  15:04:10  kl
;;; Makros zur Typdeklaration vereinfacht und zusammengelegt.
;;;
;;; Revision 1.31  1993/05/09  16:54:42  kl
;;; Neues Makro declare-atypes eingefuehrt.
;;;
;;; Revision 1.30  1993/05/03  10:33:18  kl
;;; Typdeklarationen werden bei jedem Uebersetzungslauf gesetzt.
;;;
;;; Revision 1.29  1993/04/19  12:26:47  kl
;;; get-the-function verbessert.
;;;
;;; Revision 1.28  1993/04/15  08:25:43  kl
;;; Anpassung an den verkleinerten Typverband vorgenommen.
;;;
;;; Revision 1.27  1993/03/05  16:33:18  kl
;;; Im Makro get-the-function das Package CLICC-LISP durch LISP ersetzt.
;;;
;;; Revision 1.26  1993/03/04  09:08:41  kl
;;; Anpassung des Makros get-the-function fuer Allegro CL vorgenommen.
;;;
;;; Revision 1.25  1993/02/17  12:08:35  kl
;;; Umstellung auf neues Package-System.
;;;
;;; Revision 1.24  1993/02/16  16:11:23  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.23  1993/02/02  09:36:05  kl
;;; Weitere Funktionen nach tiimpdec verlegt.
;;;
;;; Revision 1.22  1993/01/24  16:40:54  kl
;;; declare-rtype eingefuehrt. Damit wird ein Resultattyp deklariert.
;;;
;;; Revision 1.21  1993/01/21  12:31:04  kl
;;; Anpassung an den ueberarbeiteten Typverband und an neue Typverbands-
;;; operatoren.
;;;
;;; Revision 1.20  1993/01/19  10:29:37  kl
;;; Typabstraktionsfunktionen nach tiimpdec.lisp verlegt. Deklarationen fuer
;;; die Typzusicherungen nach tiassert.lisp verlegt.
;;; Neue Variable *ti-type-declarations-are-initialized* verwendet.
;;;
;;; Revision 1.19  1993/01/12  12:51:14  kl
;;; Typdeklarationen um die besonders getypten Funktionen aus
;;; *special-typed-functions* erweitert.
;;;
;;; Revision 1.18  1993/01/08  19:54:52  kl
;;; Anpassung der Typdeklarationen an den fuer Listen erweiterten Typverband.
;;;
;;; Revision 1.17  1993/01/06  13:32:14  kl
;;; Die Predicate-assertions haengen nicht mehr an einem Funktionssymbol,
;;; sondern an der eigentlichen Funktion.
;;;
;;; Revision 1.16  1992/12/31  12:35:11  kl
;;; Typdeklarationen weiter verfeinert.
;;;
;;; Revision 1.15  1992/12/31  11:47:58  kl
;;; Typdeklarationen mit den neuen Typen t-t und truth-t verfeinert.
;;;
;;; Revision 1.14  1992/12/28  16:53:55  kl
;;; Typdeklarationen erweitert und geordnet.
;;;
;;; Revision 1.13  1992/12/21  08:57:31  kl
;;; Typdeklarationen anhand der neuen Typen weiter verfeinert.
;;;
;;; Revision 1.12  1992/12/10  10:15:04  kl
;;; Typdeklarationen erweitert. Typdeklarationen haengen jetzt in der ZS.
;;;
;;; Revision 1.11  1992/12/09  11:05:38  kl
;;; Neue Typzusicherungen bei Typpraedikaten deklariert.
;;;
;;; Revision 1.10  1992/12/08  14:33:43  kl
;;; Typdeklarationen verfeinert.
;;;
;;; Revision 1.9  1992/12/02  15:49:38  kl
;;; Typdeklarationen verbessert und neue Typabstraktionsfunktionen erstellt.
;;;
;;; Revision 1.8  1992/12/02  15:15:16  kl
;;; Typdeklarationern erweitert.
;;;
;;; Revision 1.7  1992/12/02  09:34:39  kl
;;; An neues tidef.lisp und timisc.lisp angepasst.
;;;
;;; Revision 1.6  1992/12/01  15:35:54  kl
;;; Typdeklarationen erweitert und Header geaendert.
;;;
;;; Revision 1.5  1992/11/26  11:21:54  kl
;;; Typabstraktionsfunktionen der Typpredikate verfeinert und einige
;;; Typdeklarationen zugefuegt.
;;;
;;; Revision 1.4  1992/11/24  15:25:39  kl
;;; Funktion list-type fuer Listenkonstruktoren verwendet.
;;;
;;; Revision 1.3  1992/11/11  12:58:16  kl
;;; Typabstraktionsfunktionen verbessert. make-type-abstraction... verkuerzt.
;;;
;;; Revision 1.2  1992/11/05  14:20:55  kl
;;; Typbeschreibungen der Mappings eingefuehrt.
;;;
;;; Revision 1.1  1992/11/04  13:27:36  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "titypes")
(require "tidef")
(require "timisc")
 
;;------------------------------------------------------------------------------
;; Beim jedem Laden dieser Datei wird dieser Schalter zurueckgesetzt, weil sich
;; Funktionsbeschreibungen geaendert haben koennen.
;;------------------------------------------------------------------------------
(setf *ti-type-declarations-are-initialized* nil)


;;------------------------------------------------------------------------------
;; Liefert zu einem Funktionsnamen die entsprechende Funktion.
;;------------------------------------------------------------------------------
(defun get-the-function (function-name)
  (if (atom function-name) 
    (get-global-fun function-name)
    (cdr (get-global-setf-fun-def function-name))))


;;------------------------------------------------------------------------------
;; Makro zur Deklaration der Funktionstypen von importierten Funktionen.
;; Auf den Funktionsnamen folgen die Argumenttypen, ein Pfeil, der Ergebnistyp
;; und eine optionale Typabstraktionsfunktion.
;;------------------------------------------------------------------------------
(defmacro dec-type (function-name argument-types arrow result-type 
                                  &optional type-abstraction-function)
  (declare (ignore arrow))
  `(progn
    (let ((function  (get-the-function ',function-name)))
      (setf (?argument-types function) (list ,@argument-types))
      (setf (?result-type    function) ,result-type)
      ,@(when type-abstraction-function
             `((setf (?type-abstraction-function function) 
               ,type-abstraction-function))))))


;;------------------------------------------------------------------------------
;; Makro fuer Typabstraktionsfunktion der Typtests:
;;------------------------------------------------------------------------------
(defmacro type-test-type-abstraction-function (type)
  `#'(lambda (argument-type)
       (cond ((is-bottom-t argument-type)                    bottom-t)
             ((zs-subtypep argument-type ,type)             t-symbol-t)
             ((not (types-are-conform ,type argument-type)) null-t)
             (T bool-t))))


;;------------------------------------------------------------------------------
;; Makros zum Deklarieren der Typbeschreibungen der Typpraedikate:
;;------------------------------------------------------------------------------
(defmacro declare-type-predicate (type-predicate type)
  `(dec-type ,type-predicate () -> bool-t
             (type-test-type-abstraction-function ,type)))


;;------------------------------------------------------------------------------
(defmacro declare-type-predicates (type-predicate-type-pairs)
  `(progn . 
    ,(mapcar #'(lambda (type-predicate-type-pair) 
                 (let ((type-predicate (first  type-predicate-type-pair))
                       (type           (second type-predicate-type-pair)))  
                   `(declare-type-predicate ,type-predicate ,type)))
              type-predicate-type-pairs)))


;;------------------------------------------------------------------------------
;; Initialisiere die Typabstraktionsfunktionen der importierten Funktionen und
;; die Typzusicherungen.
;;------------------------------------------------------------------------------
(defun initialize-type-declarations ()
  (when (> *ti-verbosity* 0)
    (clicc-message "Initialize type declarations"))

  ;; Deklarationen aus tiimpdec.lisp
  (initialize-function-descriptions-part1)
  (initialize-function-descriptions-part2)
  (initialize-function-descriptions-part3)
  (initialize-function-descriptions-part4)

  ;; Deklarationen aus tiassert.lisp
  (initialize-type-assertion-functions)       
)

;;------------------------------------------------------------------------------
(provide "tidecl")


