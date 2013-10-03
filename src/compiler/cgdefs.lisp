;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung: Struktur- und Variablen-Deklarationen
;;;
;;; $Revision: 1.17 $
;;; $Log: cgdefs.lisp,v $
;;; Revision 1.17  1994/06/08  10:37:34  hk
;;; *rest-optimization* mit nil initialisiert, damit opt-args weiterhin
;;; von weight aufgerufen werden kann.
;;;
;;; Revision 1.16  1994/02/16  16:44:14  hk
;;; (defvar *if-counter*) hinzugef"ugt.
;;;
;;; Revision 1.15  1994/02/08  13:55:23  sma
;;; Neue Variablen *rest-optimization*, *rest-var-count*,
;;; *rlo-statistics-rest-{funs, opt, usage}*
;;;
;;; Revision 1.14  1994/01/26  13:35:55  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.13  1994/01/21  16:47:52  ft
;;; Notlösung für *SECRET-UNBOUND-SLOT-VALUE*.
;;;
;;; Revision 1.12  1994/01/21  08:23:55  ft
;;; Neue Variable: *SECRET-UNBOUND-SLOT-VALUE* zur Unboundkennzeichnung
;;; von Slots in der Zwischensprache.
;;;
;;; Revision 1.11  1994/01/07  10:17:52  hk
;;; Definition von closure-result entfernt, da nicht benutzt.
;;;
;;; Revision 1.10  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.9  1993/06/04  14:15:03  hk
;;; (defvar *C-line-count*)
;;;
;;; Revision 1.8  1993/02/16  16:07:54  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.7  1992/10/08  14:23:43  hk
;;; Typdklaration fuer *result-spec* korrigiert.
;;;
;;; Revision 1.6  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.5  1992/08/10  11:56:23  hk
;;; *mv-spec* und *mv-produced* gestrichen, da Analyse komplett in Pass3.
;;;
;;; Revision 1.4  1992/08/07  11:59:10  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.3  1992/07/08  15:36:39  hk
;;; Variable *C-NAME-PREFIX* aus clcdef.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")     

;;------------------------------------------------------------------------------
;; Klassen und Subklassen der Knoten der Zwischensprache, die nur waehrend der
;; Codegenerierung verwendet werden.  Instanzen dieser Klassen werden in
;; *result-spec* eingetragen, um Spezialfaelle zu beschreiben.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Das Resultat soll auf der Spitze des LISP-Stacks erzeugt werden.
;;------------------------------------------------------------------------------
(defzws stacktop-result (local-static))

;;------------------------------------------------------------------------------
;; Ein Wert im Multiple-Value-Buffer soll erzeugt werden.
;;------------------------------------------------------------------------------
(defzws mv-buf ()
  (offset :type integer))               ; Offset im Multiple-Value-Buffer

;;------------------------------------------------------------------------------
;; Globale Variablen
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; die naechste freie Position im LISP-Stack
;;------------------------------------------------------------------------------
(defvar *stack-top* 0)

;;------------------------------------------------------------------------------
;; Ein LISP-Ausdruck, der beschreibt, 
;; - ob ein Resultat benoetigt wird oder nicht.
;; - ob als Resultat ein LISP-Wert oder ein Boolescher C-Wert benoetigt wird.

;; - wo der LISP-Wert erzeugt werden soll.
;;------------------------------------------------------------------------------
(defvar *result-spec*)
(proclaim '(type (or (member nil C-bool) static dynamic mv-buf) *result-spec* ))

;;------------------------------------------------------------------------------
;; Ein String, der in C-Syntax einen Booleschen Ausdruck beschreibt;
;; das Boolesche Resultat eines LISP-Ausdrucks.
;;------------------------------------------------------------------------------
(defvar *C-bool*)

;;------------------------------------------------------------------------------
;; Anzahl gebundener Special Variablen.
;;------------------------------------------------------------------------------
(defvar *special-count*)

;;------------------------------------------------------------------------------
;; Jeder special Variablen wird der aktuelle Wert von *special-count*
;; zugeordnet. Diese Zuordnung ist zwar nicht eindeutig innerhalb
;; eines Programms, genuegt aber, um waehrend der Uebersetzung die special
;; Variablen eindeutig zu identifizieren.
;; 
;; Eine Liste von Nummern, die die special Variablen bezeichnen,
;; die beim Verlassen des Lambda-Ausdrucks NICHT restauriert werden duerfen,
;; da den special Variablen schon ein neuer Wert zugewiesen wurde.
;;------------------------------------------------------------------------------
(defvar *do-not-restore*)

;;------------------------------------------------------------------------------
;; Das statische Niveau der gerade uebersetzten Funktion
;;------------------------------------------------------------------------------
;;Schon in p3main definiert
;;(defvar *level*)

;;------------------------------------------------------------------------------
;; Die Closure mit dem hoechsten Level, innerhalb der der gerade uebersetze
;; Ausdruck liegt, (Repraesentiert durch die zugehoerige local-fun Struktur).
;; NIL, falls der Ausdruck innerhalb keiner Closure liegt.
;;------------------------------------------------------------------------------
(defvar *closure*)

;;------------------------------------------------------------------------------
;; Das statische Niveau der Closure in *closure* oder
;; das statische Niveau einer globalisierten lokalen Funktion oder
;; 0, wenn gerade keine Closure uebersetzt wird.
;;------------------------------------------------------------------------------
(defvar *cl-level*)

;;------------------------------------------------------------------------------
;; Anzahl der neu angelegten DOWNFUN-Parameter einer Funktion
;;------------------------------------------------------------------------------
(defvar *downfun-count*)

;;------------------------------------------------------------------------------
;; Zaehler zum Generieren eindeutiger marken fuer Tagbody bzw. Continuations
;;------------------------------------------------------------------------------
(defvar *tagbody-counter*)
(defvar *cont-counter*)
(defvar *if-counter*)

;;------------------------------------------------------------------------------
;; Beschreibung von Quelle und Ziel des letzten Kopierbefehls fuer Lisp-Daten.
;; Diese Information wird benutzt, um redundante Kopierbefehle zu vermeiden.
;; Wenn kein Kopierbefehl ausgefuehrt wurde, hat *copy-source* den wert nil.
;;------------------------------------------------------------------------------
(defvar *copy-source*)
(defvar *copy-dest*)

;;------------------------------------------------------------------------------
;; Anzahl der Tabs, die am Anfang der naechsten mit C-Ln erzeugten Zeile 
;; erzeugt werden soll. Wird durch C-blockstart und C-blockend veraendert
;;------------------------------------------------------------------------------
(defvar *C-indent*)

;;------------------------------------------------------------------------------
;; Flag, das anzeigt, ob gerade ein C-Block durch '{' geoeffnet wurde
;;------------------------------------------------------------------------------
(defvar *block-open*)

;;------------------------------------------------------------------------------
;; Liste der Werte von *C-indent* fuer die Aufrufe der Funktion 'C-blockstart',
;; die keinen Code erzeugt haben, weil dann zwei offnende '{' direkt aufeinander
;; gefolgt waeren.
;;------------------------------------------------------------------------------
(defvar *not-opened*)

;;------------------------------------------------------------------------------
;; Zaehler fuer die Anzahl der Zeilen in der gerade generierten C Datei
;;------------------------------------------------------------------------------
(defvar *C-line-count*)

;;------------------------------------------------------------------------------
;; Zaehler fuer Praefix fuer Namen von C-Funktionen
;;------------------------------------------------------------------------------
(defvar *C-NAME-PREFIX*)   

;;------------------------------------------------------------------------------
;; Die Datei, in die der C-Code geschrieben wird
;;------------------------------------------------------------------------------
(defvar *C-file*)

;;------------------------------------------------------------------------------
;; Flag, ob eine Optimierung eines Restlistenparamters durchgeführt werden
;; soll bzw kann. Dieser wird dann direkt als Zeiger auf die im Stack liegende
;; Restliste direkt in C verwaltet und das erzeugen der Restliste mit Flist
;; wird unterdrückt.
;;------------------------------------------------------------------------------
(defvar *rest-optimization* nil)

;;------------------------------------------------------------------------------
;; Zähler für eindeutige Namen von Restvariablen
;;------------------------------------------------------------------------------
(defvar *rest-var-count*)

;;------------------------------------------------------------------------------
;; Für die Statistik
;;------------------------------------------------------------------------------
(defvar *rlo-statistics-rest-funs*)
(defvar *rlo-statistics-rest-opt*)
(defvar *rlo-statistics-rest-usage*)

;;------------------------------------------------------------------------------
(provide "cgdefs")
