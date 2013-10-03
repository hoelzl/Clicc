;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Ein Typinferenzalgorithmus fuer die Zwischensprache LZS
;;;            Zuerst wird die Zwischensprache traversiert, dabei nach
;;;            Typen analysiert und mit Typinformationen versehen.
;;;            Auf Wunsch werden in einem zweiten Schritt Statistiken 
;;;            erstellt und ausgegeben.
;;;
;;; $Revision: 1.43 $
;;; $Log: timain.lisp,v $
;;; Revision 1.43  1994/01/05  10:12:28  kl
;;; Funktionen types-on und types-off nach printzs verlegt.
;;;
;;; Revision 1.42  1993/12/09  10:30:40  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.41  1993/11/21  20:01:05  kl
;;; Beim Start der Typinferenz wird nun die Typinfernzstufe ausgegeben.
;;;
;;; Revision 1.40  1993/11/12  14:14:16  kl
;;; Reihenfolge der require-Aufrufe geändert.
;;;
;;; Revision 1.39  1993/09/12  12:08:49  kl
;;; Startreihenfolge der Funktionen in der Typinferenz-Workset verbessert.
;;;
;;; Revision 1.38  1993/06/24  14:33:55  kl
;;; Debug-Funktionen hierhin verlegt.
;;;
;;; Revision 1.37  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.36  1993/05/06  13:32:59  kl
;;; Aufruf der Suche nach Typfehlern nach clcmain verlegt.
;;;
;;; Revision 1.35  1993/05/03  10:32:54  kl
;;; Typdeklarationen werden bei jedem Uebersetzungslauf gesetzt.
;;;
;;; Revision 1.34  1993/04/30  09:23:08  kl
;;; Hauptfunktion etwas umgestellt.
;;;
;;; Revision 1.33  1993/04/19  13:32:15  kl
;;; Verwendung von appfuns in write-function-type eingebaut.
;;;
;;; Revision 1.32  1993/04/19  12:24:15  kl
;;; Unnoetigen Code entfernt.
;;;
;;; Revision 1.31  1993/03/25  09:47:25  kl
;;; Hauptfunktion etwas verkleinert.
;;;
;;; Revision 1.30  1993/03/04  10:46:26  kl
;;; Typinferenzlevel eingefuehrt und Hauptfunktion verjuengt.
;;;
;;; Revision 1.29  1993/02/16  16:10:46  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.28  1993/02/11  15:54:51  kl
;;; Seiteneffektanalyse eingebunden.
;;;
;;; Revision 1.27  1993/02/02  10:11:29  kl
;;; Dritten Durchlauf der Typinferenz (tipass3) eingebunden.
;;;
;;; Revision 1.26  1993/01/26  18:35:22  kl
;;; Umstellung auf die neue Einteilung der Dateien zur Typinferenz.
;;;
;;; Revision 1.25  1993/01/24  16:41:25  kl
;;; Anpassung an die neuen Typannotationen.
;;;
;;; Revision 1.24  1993/01/21  14:25:15  kl
;;; Die Typfehler und -warnungen werden nun ausgegeben.
;;;
;;; Revision 1.23  1993/01/21  12:24:26  kl
;;; Bei der Ausgabe von Typen wird output-type aus titypes aufgerufen.
;;;
;;; Revision 1.22  1993/01/20  17:48:21  kl
;;; Funktion compile-ti entfernt.
;;;
;;; Revision 1.21  1993/01/19  10:31:18  kl
;;; Funktion zum Auflisten ALLER Funktionstypen eingebaut. Die neue Variable
;;; *ti-type-declarations-are-initialized* wird nun verwendet.
;;;
;;; Revision 1.20  1993/01/12  15:13:26  kl
;;; Die Typannotation der Variablen wird nach (types-on) mit ausgegeben.
;;;
;;; Revision 1.19  1992/12/29  16:15:55  kl
;;; Anpassung an den geaenderten Statistikteil.
;;;
;;; Revision 1.18  1992/12/28  16:54:33  kl
;;; Die ungetypten Funktionen werden nach der Anzahl der Vorkommen sortiert.
;;;
;;; Revision 1.17  1992/12/10  10:21:23  kl
;;; Unnoetigen Code entfernt.
;;;
;;; Revision 1.16  1992/12/10  10:16:54  kl
;;; Umstellung auf die neue Art der Funktionsbeschreibungen.
;;;
;;; Revision 1.15  1992/12/09  11:03:21  kl
;;; Der Resultatstyp der Typanalyse wird nicht mehr ausgegeben.
;;;
;;; Revision 1.14  1992/12/08  14:16:45  kl
;;; Funktionalitaet der Hauptfunktion erweitert.
;;;
;;; Revision 1.13  1992/12/02  16:00:19  kl
;;; Einbindung des Statistikteils verbessert. output-type nach titypes verlegt.
;;;
;;; Revision 1.12  1992/12/02  13:37:27  kl
;;; Statistikteil eingebaut und zahlreiche Verbesserungen vorgenommen.
;;;
;;; Revision 1.11  1992/11/26  11:17:59  kl
;;; Schaltervariablen fuer die Anzeige der inferierten Listentypen und die
;;; Anzeige der ungetypten importierten Funktionen eingebaut.
;;;
;;; Revision 1.10  1992/11/05  14:32:33  kl
;;; Neues compile-clicc verwendet.
;;;
;;; Revision 1.9  1992/11/04  13:21:35  kl
;;; tidecl.lisp eingebunden und Kommentare verbessert und erweitert.
;;;
;;; Revision 1.8  1992/11/02  09:01:20  kl
;;; list-function-types verbessert.
;;;
;;; Revision 1.7  1992/10/27  12:10:30  kl
;;; Funktionen zur Handhabung und Ueberpruefung der Typanalyse zugefuegt.
;;;
;;; Revision 1.6  1992/10/15  19:04:54  kl
;;; Klammerungsfehler behoben.
;;;
;;; Revision 1.5  1992/09/25  16:39:19  kl
;;; Implementation des Typverbandes nach titypes.lisp und die eigentliche
;;; Analyse nach tipass1.lisp verlegt.
;;;
;;; Revision 1.4  1992/09/15  14:31:18  kl
;;; Repraesentation des Typverbandes und der Typumgebungen geaendert.
;;;
;;; Revision 1.3  1992/09/12  20:24:30  kl
;;; Typverbandsoperationen erweitert.
;;;
;;; Revision 1.2  1992/09/12  19:44:59  kl
;;; Fixpunktiteration fuer (gegenseitige) Rekursion implementiert.
;;;
;;; Revision 1.1  1992/09/12  18:06:30  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "tidef")
(require "timisc")
(require "tidecl")
(require "tiimpdec")
(require "tiassert")
(require "tipass1")
(require "tipass2")
(require "tipass3")
(require "statistics")


;;------------------------------------------------------------------------------
;; Hauptfunktion des Typinferenzalgorithmus. 
;;
;; Vorbedingungen:
;;   Das uebergebene Modul muss ein Zwischensprach-Modul sein, und die 
;;   Seiteneffekt- (static-effect) und Kontrollflussinformationen (appfuns) 
;;   zu allen Applikationen muessen vorliegen.
;;
;; Nachbedingungen:
;;   In allen Typannotationen der Zwischensprachkonstrukte des uebergebenen 
;;   Moduls liegen in einem gewissen Sinne `korrekte' Typen.
;;
;; Erl"auterung:
;;   Es wird das in *module* enthaltene Zwischensprachenmodul analysiert. 
;;   In Abhaengigkeit des Intensitaetsgrades *ti-level* wird eine mehr oder
;;   weniger intensive Typinferenz durchgefuehrt.
;;   *ti-verbosity* ist der Gespraechigkeitsgrad der Typinferenz. 
;;------------------------------------------------------------------------------
(defun do-ti (&KEY ((:module    *module*      ) *module*      )
                   ((:level     *ti-level*    ) *ti-level*    )
                   ((:verbosity *ti-verbosity*) *ti-verbosity*))

  (unless *ti-type-declarations-are-initialized*
    (initialize-type-declarations)
    ;; (setf *ti-type-declarations-are-initialized* T)
    )
    
  (when (> *ti-verbosity* 0)
    (clicc-message "Type inference (level ~D)" *ti-level*))
  
  (prepare-type-inference *module*) 

  (when (do-type-inference-on-non-literals)
    (analyse-objects (append (?class-def-list *module*) (?all-funs *module*))))
                                              
  (when (> *ti-verbosity* 2)
    (do-statistics)))
  

    
;;------------------------------------------------------------------------------
;; Funktion zur Ausgabe des Typs einer definierten Funktion.
;;------------------------------------------------------------------------------
(defun write-function-type (a-defined-function)
  (let ((function-name (?symbol a-defined-function)))

    (cond ((zerop (?used a-defined-function))
           (clicc-message "~S is never used." function-name))
          (T
           
           (if (use-call-contexts a-defined-function)
               (clicc-message "The type of ~S is: " function-name)  
               (clicc-message "~S is used as: " function-name))

           (let* ((all-vars 
                   (?all-vars (?params a-defined-function)))
                  (parameter-types 
                   (mapcar #'output-type (mapcar #'?type all-vars)))
                  (result-type     
                   (output-type (?result-type a-defined-function))))

             (unless (endp parameter-types)
               (format t "~S" (first parameter-types))
               
               (dolist (type (rest parameter-types))
                 (format t " x ~S" type)))
             
             (format t " -> ~S~%" result-type))))

    nil))


;;------------------------------------------------------------------------------
;; Gibt die Funktionsbeschreibungen der in `a-function-list' enthaltenen 
;; Funktionen aus. Die Vorbesetzung dieser Liste ist die fun-list des *module*.
;;------------------------------------------------------------------------------
(defun list-function-types (&optional (a-function-list (?fun-list *module*)))
  (dolist (a-function a-function-list)
    (write-function-type a-function))
  (terpri))


;;------------------------------------------------------------------------------
;; Gibt die Funktionsbeschreibungen aller im Modul *module* definierten 
;; Funktionen aus.
;;------------------------------------------------------------------------------
(defun list-all-function-types ()
  (list-function-types (?all-funs *module*)))


;;------------------------------------------------------------------------------
;; Liefert die entsprechende Funktion aus der functions-liste
;;------------------------------------------------------------------------------
(defun get-function (name)
  (find name (?all-funs *module*) :key #'?symbol))

;;------------------------------------------------------------------------------
(provide "timain")
