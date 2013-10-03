;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Methoden zum Traversieren der Zwischensprache, die die 
;;;            Zwischensprachausdruecke zaehlen und Statistiken ueber 
;;;            in den Annotationen liegenden Informationen erstellen.
;;;
;;; $Revision: 1.36 $
;;; $Log: statistics.lisp,v $
;;; Revision 1.36  1994/02/08  11:13:21  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.35  1993/10/08  21:28:01  kl
;;; Debugmeldung entfernt.
;;;
;;; Revision 1.34  1993/10/08  21:20:29  kl
;;; Zaehlweise noch weiter umgestellt.
;;;
;;; Revision 1.33  1993/10/08  20:32:50  kl
;;; tagged-forms und Continuations werden nicht mehr gezaehlt.
;;;
;;; Revision 1.32  1993/09/20  06:21:23  kl
;;; Typstatistik beschleunigt.
;;;
;;; Revision 1.31  1993/09/17  14:03:49  jh
;;; Test #'eq bei assoc angegeben.
;;;
;;; Revision 1.30  1993/07/15  12:28:56  hk
;;; ~:*~[s~; ~:;s~] durch ~:p ersetzt
;;;
;;; Revision 1.29  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.28  1993/05/15  13:39:46  kl
;;; Berechnung der mit CONS getypten Ausdruecke geaendert.
;;;
;;; Revision 1.27  1993/05/14  16:00:29  kl
;;; Ausgabe der mit CONS getypten Ausdruecke eingefuehrt.
;;;
;;; Revision 1.26  1993/05/14  15:05:30  kl
;;; Unnoetige Zaehllaeufe entfernt. Ausgabe erweitert.
;;;
;;; Revision 1.25  1993/04/19  15:48:01  hk
;;; Kein Aufruf von write-statistics, falls (zerop *number-of-forms*).
;;;
;;; Revision 1.24  1993/04/19  15:42:30  hk
;;; DIVISION-BY-ZERO, falls *number-of-forms* = 0.
;;;
;;; Revision 1.23  1993/03/24  07:56:40  kl
;;; Vorbereitung auf die neue vereinfachte Version von traverse.
;;;
;;; Revision 1.22  1993/02/26  11:13:42  jh
;;; Funktionsselektor ist jetzt natuerlich (not (zerop ...)).
;;;
;;; Revision 1.21  1993/02/24  14:12:25  jh
;;; Funktionsselektor zu (zerop (?used a-fun)) korrigiert.
;;;
;;; Revision 1.20  1993/02/16  16:11:59  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.19  1993/01/28  11:51:50  kl
;;; Der Funktionsselektor ist jetzt die Funktion ?used.
;;;
;;; Revision 1.18  1993/01/27  13:05:24  kl
;;; Makrodefinitionen vor deren Anwendungen gestellt. Anwendungen des
;;; loop-Makros, die falsch expandiert worden sind umgeschrieben.
;;;
;;; Revision 1.17  1993/01/25  13:17:55  kl
;;; Die Sprungstellen der tagbody- bzw. let/cc-Ausdruecke werden nun
;;; nicht mehr gezaehlt. Schwellwert fuer die Ausgabe von Typen erhoeht.
;;;
;;; Revision 1.16  1993/01/24  16:38:10  kl
;;; Gebrauch von traverse-module umgestellt.
;;;
;;; Revision 1.15  1993/01/21  12:26:00  kl
;;; Bei der Ausgabe von Typen wird wieder output-type aufgerufen.
;;; Bei der Ueberpruefung auf die Eigenschaft optimal getypt zu sein wird
;;; nun das neue Praedikat simple-type-p aus titypes aufgerufen.
;;;
;;; Revision 1.14  1993/01/20  15:34:01  kl
;;; tr-app-form-p ueberprueft nun fun-p auf (?form a-form) nicht mehr a-form.
;;;
;;; Revision 1.13  1993/01/20  12:41:59  jh
;;; An before- und after-funs aus traverse.lisp angepasst.
;;;
;;; Revision 1.12  1993/01/19  15:14:09  jh
;;; Anpassung an die neue Version von traverse.lisp.
;;;
;;; Revision 1.11  1993/01/19  11:33:35  kl
;;; Zaehlweise korrigiert und unnoetigen Code entfernt.
;;;
;;; Revision 1.10  1993/01/12  15:14:21  kl
;;; Zwischensprachtraversierer nach traverse.lisp verlegt.
;;;
;;; Revision 1.9  1993/01/06  13:31:21  kl
;;; Ausgabe der mit bottom getypten Ausdruecke geaendert.
;;;
;;; Revision 1.8  1993/01/06  13:12:10  kl
;;; Zwischensprachtraversierer geaendert.
;;;
;;; Revision 1.7  1993/01/02  16:08:36  kl
;;; Schwellwert fuer die Ausgabe von Typen erhoeht.
;;;
;;; Revision 1.6  1992/12/31  12:35:49  kl
;;; Statistikausgaben ergaenzt.
;;;
;;; Revision 1.5  1992/12/29  16:15:24  kl
;;; Neue Statistiken eingefuehrt und generische Funktion zum Traversieren
;;; der Zwischensprachausdruecke eingebaut und benutzt.
;;;
;;; Revision 1.4  1992/12/16  14:40:53  jh
;;; Kleine Aenderungen an den Formatstrings in write-statistics.
;;;
;;; Revision 1.3  1992/12/08  14:12:57  kl
;;; Bei einfachen Funktionsapplikationen werden die Funktionen nicht gezaehlt.
;;;
;;; Revision 1.2  1992/12/02  16:01:22  kl
;;; Literale werden gezaehlt und Doppelanalysen werden vermieden.
;;;
;;; Revision 1.1  1992/12/02  13:36:23  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(require "titypes")
(require "traverse")

;;------------------------------------------------------------------------------
;; Globale Variablen fuer den Statistikanalyse:
;;------------------------------------------------------------------------------
(defvar *number-of-forms*)
(defvar *number-of-literals*)
(defvar *number-of-simple-typed-forms*)


;; Tabelle zur Typstatistik mit Eintraegen der Art (Typ . Anzahl Annotierungen) 
(defvar *type-statistic*)


;; Assoziationsliste fuer applizierte Funktionen. Schluessel ist die Funktion
;; und Datum ist die Anzahl der Applikationen dieser Funktion.
(defvar *application-statistic*)


;; Schwellwert fuer die Ausgabe von applizierten Funktionen in Prozent.
(defconstant *st-write-application-threshold* 0.3)


;; Schwellwert fuer die Ausgabe von Typvorkommen.
(defconstant *st-write-type-threshold* 0.2)


;;------------------------------------------------------------------------------
;; Sammle Statistiken zu dem Modul und gib sie aus. Es werden die Ausdruecke 
;; gezaehlt und Statistiken mit folgenden Informationen erstellt:
;; 
;; - Anzahl der literalen und nicht-literalen Ausdruecke,
;; - Haeufigkeitsstatistik der in Applikationen applizierten Funktionen,
;; - Haeufigkeitsstatistik zu den Typannotationen der nicht-literalen Ausdruecke
;;------------------------------------------------------------------------------
(defun do-statistics (&key ((:module    *module*      ) *module*))
  (setf *number-of-forms*              0
        *number-of-literals*           0
        *number-of-simple-typed-forms* 0
        *type-statistic*               '()
        *application-statistic*        '())
    
  (clicc-message "Collecting type statistics")
  (traverse-module *module* 
                   :after-funs
                   (if (> *ti-verbosity* 3)
                       (list #'count-applications
                             #'collect-type-statistics)
                       (list #'collect-type-statistics))
                   :tr-fun-body-p #'(lambda (a-fun)
                                      (declare (ignore a-fun)) nil))
  
  (unless (zerop *number-of-forms*)
    (write-statistics)))


;;------------------------------------------------------------------------------
;; Die generische Funktion `count-applications' erstellt eine Statistik ueber
;; die Applikationen im analysierten Modul. 
;; Zu jeder mittels eines Applikationsausdrucks applizierten Funktion wird die
;; Anzahl der Applikationen aufgefuehrt.
;;------------------------------------------------------------------------------
(defgeneric count-applications (object))

(defmethod count-applications ((a-zws-object zws-object)))

(defmethod count-applications ((an-app app))
  (let ((form (?form an-app)))
    (when (fun-p form)
      (let ((assoc (assoc form *application-statistic* :test #'eq)))
        (if assoc 
            (incf (rest assoc))
            (push (cons form 1) *application-statistic*))))))


;;------------------------------------------------------------------------------
;; Es folgen die Methoden der generischen Funktion zum Erstellen der Statstik
;; ueber die Typannotationen.
;;------------------------------------------------------------------------------
(defun collect-types (a-form)
  (incf *number-of-forms*)
  (when (simple-type-p (?type a-form))
    (incf *number-of-simple-typed-forms*))
  (add-type-statistics a-form))


(defgeneric collect-type-statistics (object))

(defmethod collect-type-statistics ((a-zws-object zws-object)))

(defmethod collect-type-statistics ((a-form form)) 
  (collect-types a-form))

(defmethod collect-type-statistics ((a-cont cont)) 
  (collect-types a-cont))

(defmethod collect-type-statistics ((an-app app))
  (unless (cont-p (?form an-app))
    (collect-types an-app)))

(defmethod collect-type-statistics ((a-tagged-form tagged-form)))

(defmethod collect-type-statistics ((a-literal literal))
  (incf *number-of-forms*) 
  (incf *number-of-literals*)
  (add-type-statistics a-literal))


;;------------------------------------------------------------------------------
;; Fuege einen neuen Eintrag in eine der Statistiktabellen.
;;------------------------------------------------------------------------------
(defmacro add-to-table (entry table test)
  `(let ((assoc (assoc ,entry ,table :test ,test)))
    (if assoc
        (incf (rest assoc))
        (push (cons ,entry 1) ,table))))


;;------------------------------------------------------------------------------
(defun add-type-statistics  (a-form)
  (add-to-table (?type a-form) *type-statistic* #'type-eq))


;;------------------------------------------------------------------------------
;; Ausgabe der gewonnenen Statistikinformation ueber den Zwischensprachencode.
;;------------------------------------------------------------------------------
(defun write-statistics ()

  (when (> *ti-verbosity* 2)
    (write-type-statistics)
    (write-count-statistic))

  (when (> *ti-verbosity* 3)
    (write-application-statistic)))



;;------------------------------------------------------------------------------
;; Ausgabe der Typstatistiken.
;;------------------------------------------------------------------------------
(defun write-type-statistics ()
  (clicc-message-line)
  (clicc-message "The module contains ~D forms, typed as follows:"
                 *number-of-forms*)
  
  ;; Sortiere die Eintraege der Typtabelle nach Haeufigkeit der Vorkommen.
  ;;----------------------------------------------------------------------
  (setf *type-statistic* (sort *type-statistic* #'(lambda (e1 e2)
                                                    (> (rest e1) (rest e2)))))

  (let ((*print-length* 4))

    (dolist (entry *type-statistic*)
      (let* ((type    (first entry))
             (number  (rest entry))
             (percent (* 100 (/ number *number-of-forms*))))
        
        (when (> percent *st-write-type-threshold*)
          (clicc-message
           "~5d form~:p (~4,1F%) ~2:*~[are~; is~:;are~] typed ~*~A."
           number
           percent
           (output-type type))))))

  (clicc-message-line))


;;------------------------------------------------------------------------------
;; Ausgabe der Anzahl der literalen und nichtliteralen Ausdruecke.
;;------------------------------------------------------------------------------
(defun write-count-statistic ()
  (let ((optimally-typed-forms (+ *number-of-literals* 
                                  *number-of-simple-typed-forms*))
        (cons-typed-forms      0))

    (dolist (entry *type-statistic*)
      (let ((type   (first entry))
            (number (rest  entry)))
        (when (and (not (is-bottom-t type))
                   (zs-subtypep type cons-t))
          (incf cons-typed-forms number))))

    (clicc-message 
     "~5D forms (~4,1F%) are proven to be typed optimally."
     optimally-typed-forms
     (* 100 (/ optimally-typed-forms *number-of-forms*))) 

    (clicc-message 
     "~5D forms (~4,1F%) are literals."
     *number-of-literals*
     (* 100 (/ *number-of-literals* *number-of-forms*)))

    ;; (clicc-message 
    ;;  "~5D forms (~4,1F%) are typed ~A."
    ;;  cons-typed-forms
    ;;  (* 100 (/ cons-typed-forms *number-of-forms*))
    ;;  (output-type cons-t))

    )

  (clicc-message-line))


;;------------------------------------------------------------------------------
;; Ausgabe der Haeufigkeitsstatistik zu Funktionsapplikationen.
;;------------------------------------------------------------------------------
(defun write-application-statistic ()
  (let ((number-of-applications 
         (let ((sum 0))
           (dolist (entry *application-statistic* sum)
             (incf sum (rest entry))))))
  
    (clicc-message "There are ~D application~:p as follows:"
                   number-of-applications)

    ;; Sortiere die Funktionen nach der Anzahl ihrer Vorkommen.
    (setf *application-statistic* 
          (sort *application-statistic*
                #'(lambda (entry1 entry2)
                    (> (rest entry1) (rest entry2)))))

    (dolist (entry *application-statistic*)
      (let* ((number  (rest entry))
             (percent (* 100 (/ number number-of-applications)))
             (symbol  (?symbol (first entry))))
        
        (when (> percent *st-write-application-threshold*)
          
          (clicc-message "~5D application~:p (~4,1F%) to ~S"
                         number
                         percent
                         symbol)))))

  (clicc-message-line))


;;------------------------------------------------------------------------------
(provide "statistics")



