;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Definitionen der glob. Variablen und Konstanten der Typinferenz
;;;
;;; $Revision: 1.40 $
;;; $Log: tidef.lisp,v $
;;; Revision 1.40  1993/12/09  10:31:24  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.39  1993/11/21  22:07:54  kl
;;; Die Typinferenzfeatures werden nun nicht mehr über den Typinferenzlevel,
;;; sondern über neu angelegte Prädikate abgefragt.
;;;
;;; Revision 1.38  1993/09/13  12:31:52  hk
;;; *ti-level* auf 2 gesetzt, weil schnell und gut.
;;;
;;; Revision 1.37  1993/09/12  16:06:32  kl
;;; Kommentare an die nochmals umgestellten Typinferenz-Level angepasst.
;;;
;;; Revision 1.36  1993/09/12  11:47:44  kl
;;; Typinferenz-Level 2 gestrichen. Es gibt nun die Level 0 -- 3.
;;;
;;; Revision 1.35  1993/09/04  14:02:03  kl
;;; Hilfsvariable *successor-workset* zur Beschleunigung der Fixpunktiteration
;;; eingefuehrt. Kommentare zu den Leveln 1 und 2 erweitert.
;;;
;;; Revision 1.34  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.33  1993/06/08  11:53:57  kl
;;; *ti-level* und *ti-verbosity* erhoeht.
;;;
;;; Revision 1.32  1993/05/17  06:39:02  kl
;;; Vorbereitung auf einen weiteren ti-level.
;;;
;;; Revision 1.31  1993/05/06  06:49:21  kl
;;; Variablen *ti-errors* und *ti-warnings* gestrichen. Es wird jetzt nur
;;; noch auf *NWarnings* zugegriffen.
;;;
;;; Revision 1.30  1993/04/20  15:04:00  kl
;;; *ti-level* auf 3 erhoeht und unnoetige Variablen entfernt.
;;;
;;; Revision 1.29  1993/04/18  16:00:21  kl
;;; Debugschalter entfernt.
;;;
;;; Revision 1.28  1993/03/18  13:27:53  kl
;;; Konstanten fuer die Verwendung des analysed-Slots entfernt.
;;;
;;; Revision 1.27  1993/03/10  08:48:49  kl
;;; Vorbesetzung der Variablen *ti-verbosity* gesenkt.
;;;
;;; Revision 1.26  1993/03/05  15:35:31  kl
;;; Neue Konstanten eingefuehrt.
;;;
;;; Revision 1.25  1993/03/04  10:43:33  kl
;;; *ti-level* eingefuehrt und unnoetige Variablen entfernt.
;;;
;;; Revision 1.24  1993/02/16  16:11:15  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.23  1993/02/02  09:02:51  kl
;;; Makrodefinitionen zs-typecase und update-type-f nach titypes verlegt.
;;;
;;; Revision 1.22  1993/01/27  13:02:47  kl
;;; Makrodefinition von update-type-f aus timisc.lisp hierhin verlegt.
;;;
;;; Revision 1.21  1993/01/26  18:39:35  kl
;;; Globale Variablen aus den Typinferenzdurchlaeufen hierhin verlegt.
;;;
;;; Revision 1.20  1993/01/25  13:16:29  kl
;;; *ti-current-function* eingefuehrt. Klassendefinitionen der Typinferenz
;;; entfernt, weil die entsprechenden Angaben jetzt direkt in Annotationen
;;; der jeweiligen Zwischensprachkonstrukte gehalten werden.
;;;
;;; Revision 1.19  1993/01/21  12:28:45  kl
;;; Makro zs-typecase, dass einem typecase ueber zs-typen entspricht eingebaut.
;;;
;;; Revision 1.18  1993/01/20  17:49:17  kl
;;; Zur Klassendefinition wird nun defclass1 anstatt defclass verwendet.
;;;
;;; Revision 1.17  1993/01/19  10:32:27  kl
;;; Neue globale Variable *ti-type-declarations-are-initialized* eingefuehrt.
;;;
;;; Revision 1.16  1993/01/10  18:08:24  kl
;;; *ti-get-type-assertions-from-predicate-positions* eingefuehrt.
;;;
;;; Revision 1.15  1992/12/10  10:14:19  kl
;;; Unnoetige Konstantendeklarationen und Klassendefinitionen entfernt.
;;;
;;; Revision 1.14  1992/12/08  14:14:43  kl
;;; Variablenvorbesetzungen der globalen Variablen geaendert.
;;;
;;; Revision 1.13  1992/12/02  09:36:44  kl
;;; Unnoetige globale Variablen entfernt. get-fun-descr nach timisc verlegt.
;;;
;;; Revision 1.12  1992/12/01  15:39:30  kl
;;; Klassendefinitionen geaendert und neue globale Variablen eingefuehrt.
;;;
;;; Revision 1.11  1992/11/26  11:14:42  kl
;;; Klassendefinitionen um Typschemata erweitert. Neue Variablen fuer die
;;; Anzeige der nicht getypten importierten Funktionen und fuer zwei
;;; Anzeigeschalter eingefuehrt.
;;;
;;; Revision 1.10  1992/11/04  13:26:35  kl
;;; Typdeklarationen nach tidecl.lisp verlegt. Kommentare erweitert.
;;;
;;; Revision 1.9  1992/11/02  12:12:30  kl
;;; Typdeklarationen erweitert.
;;;
;;; Revision 1.8  1992/10/27  12:08:38  kl
;;; An neuen Typverband angepasst.
;;;
;;; Revision 1.7  1992/10/02  14:24:21  kl
;;; Typschemata werden waehrend der Uebersetzungzeit in Funktionen umgewandelt.
;;;
;;; Revision 1.6  1992/10/01  17:02:41  kl
;;; Makro declare-type auf neue Art der Typabstraktionsfunktionen umgestellt.
;;;
;;; Revision 1.5  1992/09/25  16:40:37  kl
;;; Alle Definitionen des Typverbandes nach titypes.lisp verlegt.
;;;
;;; Revision 1.4  1992/09/15  14:33:23  kl
;;; Repraesentation der Funktionsbeschreibungen verbessert.
;;;
;;; Revision 1.3  1992/09/14  14:07:05  kl
;;; Liste der Systemfunktionsbeschreibungen erweitert.
;;;
;;; Revision 1.2  1992/09/12  19:50:56  kl
;;; Zu einigen Systemfunktionen sind jetzt Typen deklariert.
;;;
;;; Revision 1.1  1992/09/12  18:06:25  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Natuerliche Zahl, die angibt, wie intensiv die Typinferenz arbeiten soll. Je 
;; hoeher der angegebenen Wert ist, desto `globaler' arbeitet die Typinferenz.  
;; Mit niedrigerem Grad sinkt die fuer die Typinferenz benoetigte Rechenzeit, 
;; allerdings werden dafuer in der Regel die inferierten Ergebnisse schwacher.
;;
;; Die genaue Einteilung ist wie folgt:
;;
;;  0 : Es werden keine Typen inferiert. Die Typannotationen der Literale 
;;      werden auf die entsprechenden Typen, alle anderen Typannotationen
;;      werden auf das Top-Element des Typverbandes gesetzt.
;;
;;  1 : Es werden keine Aufrufkontexte beachtet. Das angewendete Verfahren 
;;      entspricht in diesem Fall einer intraprozeduralen Typinferenz.
;;      Diese Stufe wird realisiert, indem die Eintrittsumgebungen aller
;;      Funktionen mit Top-Elementen vorbesetzt werden und der Ergebnistyp
;;      von Funktionen auf Top gesetzt wird.
;;
;;  2 : In dieser Stufe werden alle Aufrufkontexte beachtet.
;;
;;  3 : In dieser Stufe werden die Typen globaler Variablen intensiver als in
;;      den anderen Stufen behandelt.
;;      Die Ausfuehrung dieser Analysestufe benoetigt in der Regel deutlich 
;;      mehr Speicher als die anderen Stufen. Sie ermittelt dafuer die im 
;;      Vergleich zu den anderen Stufen praezisesten Typinformationen.
;;------------------------------------------------------------------------------
(defvar *ti-level* 2)


;;------------------------------------------------------------------------------
;; Prädikate für einzelne Typinferenzfeatures:
;;------------------------------------------------------------------------------


;; Soll eine Typinferenz auch auf den Nichtliteralen durchgeführt werden?
;;------------------------------------------------------------------------------
(defun do-type-inference-on-non-literals ()
  (> *ti-level* 0))


;; Soll eine interprozedurale Analyse durchgeführt werden? 
;;------------------------------------------------------------------------------
(defun do-interprocedural-type-inference ()
  (> *ti-level* 1))


;; Sollen für dynamisch gebundene Variablen präzise Typen ermittelt werden?
;;------------------------------------------------------------------------------
(defun use-bindings-of-dynamic-variables ()
  (> *ti-level* 2))


;; Soll die (aufwendigere) Typsemantik für Funktionen verwendet werden?
;;------------------------------------------------------------------------------
(defun use-precise-function-type-semantics ()
  (> *ti-level* 2))


;;------------------------------------------------------------------------------
;; Natuerliche Zahl fuer den Gespraechigkeitsgrad der Typinferenz. Je hoeher der
;; angegebenen Wert ist, desto mehr Ausgaben und Meldungen werden waehrend des
;; Durchlaufs erzeugt. Ist *ti-verbosity*=0, werden keine Ausgaben geschrieben.
;;------------------------------------------------------------------------------
(defvar *ti-verbosity* 3)


;;------------------------------------------------------------------------------
;; Diese Variable wird zum Propagieren von Typbindungen verwendet. Sie enthaelt
;; die Typumgebung fuer locations in Form einer Assoziationsliste. Die Liste
;; besteht aus Paaren (<location> . <zugeordneter Typ>).
;;------------------------------------------------------------------------------
(defvar *type-environment*)        


;;------------------------------------------------------------------------------
;; Menge der noch zu analysierenden Funktionen und Klassen. Diese Variable wird
;; im zweiten Pass der Typinferenz (tipass2) verwendet. 
;;------------------------------------------------------------------------------
(defvar *ti-workset*)  


;;------------------------------------------------------------------------------
;; *successor-workset* enthaelt die Menge der zu analysierenden Nachfolger der 
;; aktuell analysierten Funktion. Sie enthaelt somit einen kleinen Ausschnitt
;; der Menge aller zu bearbeitenden Funktionen. 
;; Die Einfuehrung dieser Menge beschleunigt die Fixpunktiteration, weil nicht 
;; alle Mengenoperation auf der evtl. sehr grossen *ti-workset* geschehen.
;;------------------------------------------------------------------------------
(defvar *successor-workset*)


;;------------------------------------------------------------------------------
;; Sind die Typdeklarationen initialisiert worden? Typdeklarationen sind die
;; Typabstraktionsfunktionen der importierten Funktionen und Funktionen zur 
;; Typzusicherungen fuer Ausdruecke an Praedikatsposition eines Konditionals.
;;------------------------------------------------------------------------------
(defvar *ti-type-declarations-are-initialized*)


;;------------------------------------------------------------------------------
;; Umgebung, in der Funktionen fuer Typzusicherungen in if-Konstrukten in Form
;; einer Assoziationsliste vorliegen. Sie enthaelt Paare der Gestalt:
;; (<Praedikat> . <Typzusicherungsfunktion>)
;;------------------------------------------------------------------------------
(defvar *ti-predicate-assertion-environment* ())


;;------------------------------------------------------------------------------
;; Angabe, wie haeufig die Groesse der *ti-workset* ausgeben werden soll.
;;------------------------------------------------------------------------------
(defconstant *ti-write-size-of-workset-interval* 300)


;;------------------------------------------------------------------------------
(provide "tidef")



