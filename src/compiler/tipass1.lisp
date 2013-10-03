;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Vorbereitungen zur Typinferenz. Dazu gehoeren:
;;;             o Typisierung der Literale,
;;;             o Feststellen, ob eine Funktion als funktionales 
;;;               Objekt verwendet wird,
;;;             o Setzen der `called-by'-Komponenten.
;;;
;;; $Revision: 1.67 $
;;; $Log: tipass1.lisp,v $
;;; Revision 1.67  1994/03/03  13:54:36  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.66  1994/01/27  19:08:55  kl
;;; Anpassung an den erweiterten Typverband.
;;;
;;; Revision 1.65  1993/12/09  10:34:44  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.64  1993/11/21  22:08:54  kl
;;; Referenzen auf den Typinferenzlevel entfernt.
;;;
;;; Revision 1.63  1993/11/07  14:21:00  kl
;;; Setzen der Read- und Write-Listen aufgeräumt.
;;;
;;; Revision 1.62  1993/10/08  20:43:37  kl
;;; Nicht verwendeten Code entfernt.
;;;
;;; Revision 1.61  1993/10/06  16:53:01  hk
;;; In set-all-predecessor-and-result-type-envs wird ?pred-type-env nun
;;; anhand des Werts in ?free-lex-vars und nicht mehr anhand der falschen
;;; Analyseergebnisse von get/set-lex-vars berechnet.
;;;
;;; Revision 1.60  1993/09/12  16:08:39  kl
;;; Bei den Typinferenz-Leveln 0,1 und 2 werden globale Variablen mit TOP
;;; getypt. Dadurch werden diese Stufen sehr viel schneller.
;;;
;;; Revision 1.59  1993/09/12  11:53:00  kl
;;; Typinferenz-Level 2 gestrichen und Spezialisierungen ueber T entfernt.
;;;
;;; Revision 1.58  1993/09/04  14:03:01  kl
;;; Bei Level 1 und 2 laeuft nun tatsaechlich nur noch eine intraprozedurale
;;; und damit wesentlich schnellere Analyse ab.
;;;
;;; Revision 1.57  1993/07/26  14:35:06  wg
;;; Lokale Funktionen in Methoden fuer CMU global definiert.
;;;
;;; Revision 1.56  1993/07/22  09:51:41  jh
;;; prepare-type-inference fuer *ti-level* 0 vereinfacht.
;;;
;;; Revision 1.55  1993/07/19  09:50:28  jh
;;; Fehler bei named-const behoben.
;;;
;;; Revision 1.54  1993/07/11  13:33:32  kl
;;; Ruecksetzen der Typannotationen implementiert.
;;;
;;; Revision 1.53  1993/06/24  14:02:54  kl
;;; In der Komponente local-funs von Funktionen werden Elemente nun hoechstens
;;; einmal abgelegt.
;;;
;;; Revision 1.52  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.51  1993/06/10  10:34:32  kl
;;; Lokale Funktionen und freie lexikalische Variablen werden jetzt
;;; eigenstaendig berechnet und nicht mehr aus der Annotation fuer die
;;; Codegenerierung entnommen.
;;;
;;; Revision 1.50  1993/06/07  10:08:20  kl
;;; set-parameter-types und target-fixnump eingebaut.
;;;
;;; Revision 1.49  1993/06/02  08:55:59  kl
;;; Behandlung der dyn-var-list verbessert.
;;;
;;; Revision 1.48  1993/05/23  15:59:21  kl
;;; Anpassung an den neuen Typverband.
;;;
;;; Revision 1.47  1993/05/22  11:27:15  kl
;;; Typisierung bei ti-level=0 geaendert.
;;;
;;; Revision 1.46  1993/05/18  16:17:18  kl
;;; Vorbereitung auf einen neuen ti-level getroffen.
;;;
;;; Revision 1.45  1993/05/09  16:55:25  kl
;;; Behandlung der constant-values korrigiert.
;;;
;;; Revision 1.44  1993/04/30  09:19:30  kl
;;; Aufruf von search-fun-calls entfernt.
;;;
;;; Revision 1.43  1993/04/20  15:05:36  kl
;;; Der called-by-Slot wird nun in appfuns.lisp gesetzt.
;;;
;;; Revision 1.42  1993/04/19  12:29:42  kl
;;; Noch Anpassungen an den verkleinerten Typverband.
;;;
;;; Revision 1.41  1993/04/15  08:25:01  kl
;;; Anpassung an den verkleinerten Typverband vorgenommen.
;;;
;;; Revision 1.40  1993/04/02  10:15:25  kl
;;; Joergs Analyse zu Funktionsaufrufen eingebunden.
;;;
;;; Revision 1.39  1993/03/25  09:39:36  kl
;;; Ausgabemeldungen erweitert und Behandlung von Funktionen geaendert.
;;;
;;; Revision 1.38  1993/03/18  13:46:31  kl
;;; Unterschiedliche Meldungen fuer unterschiedliche TI-Level eingefuehrt.
;;;
;;; Revision 1.37  1993/03/05  15:50:13  kl
;;; Used-Annotation wird nicht mehr verwendet.
;;;
;;; Revision 1.36  1993/03/04  10:45:22  kl
;;; Anpassung an die eingefuehrten Typinferenzlevel.
;;;
;;; Revision 1.35  1993/02/26  11:11:26  jh
;;; traverse-keyword function-selector in fun-selector geaendert.
;;;
;;; Revision 1.34  1993/02/16  16:10:31  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.33  1993/02/15  14:44:35  kl
;;; Durch die Verwendung des Slots read-list bleiben die Vorgaenger-
;;; typumgebungen jetzt wesentlich kleiner.
;;;
;;; Revision 1.32  1993/02/02  09:51:54  kl
;;; Die Vorgaengertypumgebungen werden jetzt in diesem Pass gesetzt.
;;; In der Typvorbereitung werden nur noch benutzte Funktionen behandelt.
;;;
;;; Revision 1.31  1993/01/29  13:07:21  kl
;;; Applikation von type-of entfernt.
;;;
;;; Revision 1.30  1993/01/27  13:04:12  kl
;;; Typecase in zs-type-of so umgestellt, dass der Typtest auf bignum
;;; unnoetig geworden ist.
;;;
;;; Revision 1.29  1993/01/26  18:37:22  kl
;;; Die eigentliche Typinferenz findet nun in tipass2.lisp statt.
;;; Hier werden jetzt nur noch Vorbereitungen fuer die Typinferenz getroffen.
;;;
;;; Revision 1.28  1993/01/25  13:12:48  kl
;;; Globale Fixpunktiteration naeher an die Fixpunktiteration von Jens Knoop
;;; angepasst. Fixpunktiteration fuer tagbody-Konstrukte fuer einen haeufigen
;;; Spezialfall optimiert. Typumgebungen wachsen jetzt etwas langsamer.
;;;
;;; Revision 1.27  1993/01/21  14:23:43  kl
;;; Die Variablen fuer Typfehler und -warnungen werden jetzt jeweils mit der 
;;; leeren Liste initialisiert.
;;;
;;; Revision 1.26  1993/01/20  12:43:53  jh
;;; An before- und after-funs aus traverse.lisp angepasst.
;;;
;;; Revision 1.25  1993/01/19  15:13:23  jh
;;; Anpassung an die neue Version von traverse.lisp.
;;;
;;; Revision 1.24  1993/01/19  11:31:03  kl
;;; get-type-assertions-from-predicate-position nach tiassert.lisp verlegt.
;;;
;;; Revision 1.23  1993/01/14  10:38:11  kl
;;; Analyse der Repraesentation globaler Variablen verbessert.
;;;
;;; Revision 1.22  1993/01/12  12:47:54  kl
;;; Globale Fixpunktiteration geaendert.
;;;
;;; Revision 1.21  1993/01/07  09:41:53  kl
;;; Dokumentation zur Fixpunktiteration auf Tagbody-Konstrukten erweitert.
;;;
;;; Revision 1.20  1993/01/06  13:30:41  kl
;;; Analyse der Continuations verbessert und kommentiert. Fixpunktiteration
;;; auf tagbody-Konstrukten korrigiert.
;;;
;;; Revision 1.19  1992/12/31  12:37:05  kl
;;; Neuen Typ t-t eingebaut.
;;;
;;; Revision 1.18  1992/12/28  16:55:24  kl
;;; Bei den ungetypten importierten Funktionen wird die Anzahl der
;;; Applikationen mitgezaehlt.
;;;
;;; Revision 1.17  1992/12/21  09:02:40  kl
;;; Kleine Fehler behoben.
;;;
;;; Revision 1.16  1992/12/10  10:12:15  kl
;;; Die Funktionsbeschreibungen an die Zwischensprachelemente gehaengt.
;;;
;;; Revision 1.15  1992/12/09  10:56:56  kl
;;; Falsche Abfrage auf symbol in if-Konstrukten wird abgefangen.
;;;
;;; Revision 1.14  1992/12/08  14:11:07  kl
;;; Argumenttypen werden bei Applikation definierter Funktionen verwendet.
;;; Fehler mit falschem Rueckgabewert in analyse-types (setq-form) behoben.
;;;
;;; Revision 1.13  1992/12/02  13:27:19  kl
;;; Fehler in der Typbindung in let*-Konstrukten behoben.
;;;
;;; Revision 1.12  1992/12/01  16:04:05  kl
;;; Fixpunktiteration umgestellt und zahlreiche Verbesserungen eingebaut.
;;;
;;; Revision 1.11  1992/11/26  11:41:29  kl
;;; Anpassung an neue Variablen und Typzugriffsfunktionen vorgenommen.
;;;
;;; Revision 1.10  1992/11/24  16:34:44  kl
;;; Continuations beruecksichtigt und Analyse fuer tagbodys verbessert.
;;;
;;; Revision 1.9  1992/11/23  13:35:21  kl
;;; Typen globaler Variablen werden nicht mehr an das zugehoerige Symbol
;;; sondern an die Variable selber gebunden. Neues bind-parameter-types benutzt.
;;;
;;; Revision 1.8  1992/11/05  14:40:27  kl
;;; Die initialen Typannotationen werden waehrend der Analyse gesetzt.
;;;
;;; Revision 1.7  1992/11/04  13:47:42  kl
;;; Kommentare verbessert und internal-error verwendet.
;;;
;;; Revision 1.6  1992/11/02  12:14:26  kl
;;; Dynamische Variablen haben jetzt eine eigene Typumgebung.
;;;
;;; Revision 1.5  1992/10/27  12:09:23  kl
;;; Umstellung auf den neuen Typverband durchgefuehrt. Teile korrigiert.
;;;
;;; Revision 1.4  1992/10/15  19:03:19  kl
;;; Umstellung auf neue Hilfsfunktionen. Einigee Analysemethoden korrigiert.
;;;
;;; Revision 1.3  1992/10/02  14:23:36  kl
;;; Bei Applikationen werden eventuelle Seiteneffekte beruecksichtigt.
;;;
;;; Revision 1.2  1992/10/01  17:01:15  kl
;;; Umstellung auf neue Repraesentation der einfachen Literale.
;;;
;;; Revision 1.1  1992/09/25  16:42:45  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "titypes")
(require "tidef")
(require "traverse")

;;------------------------------------------------------------------------------
;; zs-type-of liefert den Typ eines Literals. Diese Funktion ist nicht 
;; generisch implementiert, weil die zu unterscheidenden Typen im allgemeinen
;; nicht als Klassen zur Verfuegung stehen.
;;------------------------------------------------------------------------------
(defun zs-type-of (anything) 
  (labels ((proper-list-p (a-list)
             (null (rest (last a-list))))

           (ti-bytep (an-integer)
             (< (abs an-integer) 256))

           (ti-wordp (an-integer)
             (< (abs an-integer) 65536)))
  
    (typecase anything
      (null-form        null-t)
      (null             null-t)
      (symbol           (if (eq anything T)
                            t-symbol-t
                            other-symbol-t))
      (sym              (if (eq (?symbol anything) T)
                            t-symbol-t
                            other-symbol-t))
      (cons             (if (proper-list-p anything)
                            (apply #'list-cons-of 
                                   (mapcar #'zs-type-of anything))
                            non-list-cons-t))
      (character        character-t)
      (string           string-t)
      (vector           non-string-vector-t)
      (array            non-vector-array-t)
      (fun              function-t)
      (function         function-t)
      (literal-instance class-t)
      (number           (typecase anything
                          (integer (cond ((ti-bytep anything)      
                                          byte-t)
                                         ((ti-wordp anything)       
                                          non-byte-word-t)
                                         ((target-fixnump anything) 
                                          non-word-fixnum-t)
                                         (T bignum-t)))
                          (float   float-t)
                          (otherwise 
                           (internal-error 'zs-type-of 
                                           "~S has unknown number type." 
                                           anything)
                           number-t)))
      (package          package-t)
      (stream           stream-t)
      (otherwise        (internal-error 'zs-type-of
                                        "~S has unknown type." anything)
                        top-t))))


;;------------------------------------------------------------------------------
;; Setze die Typkomponenten der Literale. Wenn die Typen der Nichtliterale 
;; nicht inferiert werden sollen, dann werden die Typannotationen der 
;; Nichtliterale auf das Typ-Topelement gesetzt.
;;------------------------------------------------------------------------------
(defgeneric preset-types (object))

(defmethod preset-types ((a-form form))
  (unless (do-type-inference-on-non-literals)
    (setf (?type a-form) top-t)))

(defmethod preset-types ((a-var var))
  (unless (do-type-inference-on-non-literals)
    (setf (?type a-var) top-t)))

(defmethod preset-types ((a-let* let*-form))
  (unless (do-type-inference-on-non-literals)
    (setf (?type a-let*) top-t)
    (dolist (var (?var-list a-let*))
      (setf (?type var) top-t))))
  
(defmethod preset-types ((the-empty-list null-form))
  (setf (?type the-empty-list) 
        null-t))

(defmethod preset-types ((an-instance literal-instance)) 
  (setf (?type an-instance) 
        class-t))

(defmethod preset-types ((a-simple-literal simple-literal))
  (setf (?type a-simple-literal) 
        (zs-type-of (?value a-simple-literal))))

(defmethod preset-types ((a-structured-literal structured-literal))
  (setf (?type a-structured-literal) 
        (zs-type-of (?value a-structured-literal))))

(defmethod preset-types ((a-sym sym))
  (setf (?type a-sym) 
        (zs-type-of a-sym)))

(defmethod preset-types ((a-fun fun))
  (setf (?type a-fun) 
        (zs-type-of a-fun)))

(defmethod preset-types :after ((a-var-ref var-ref))
  (preset-types (?var a-var-ref)))

(defmethod preset-types :after ((a-dynamic dynamic))
  (let* ((sym (?sym a-dynamic))
         (constant-value (?constant-value sym)))
    (when (constant-value-p sym)
      (preset-types constant-value))))

(defmethod preset-types :after ((a-defined-named-const defined-named-const))
  (let ((value (?value a-defined-named-const)))
    (case value
      (:forward (internal-error 'preset-types
                                ":forward in defined-named-const ~S."
                                a-defined-named-const))
      (:unknown)
      (otherwise (preset-types value)
                 (setf (?type a-defined-named-const) (?type value))))))

(defmethod preset-types :after ((an-imported-named-const imported-named-const))
   (setf (?type an-imported-named-const)
         (case (?value-zs-type an-imported-named-const)
           (:cons cons-t)
           (:string string-t)
           (:vector vector-t)
           (:array array-t)
           (:literal-instance class-t))))

;;------------------------------------------------------------------------------
;; Setze die Typkomponenten der Nicht-Literale auf das Bottom-Element des 
;; Typverbands.
;;------------------------------------------------------------------------------
(defgeneric reset-type-annotation (object))

(defmethod reset-type-annotation ((a-form form))
  (setf (?type a-form) bottom-t))

(defmethod reset-type-annotation ((a-var var))
  (setf (?type a-var) bottom-t))

(defmethod reset-type-annotation ((a-let* let*-form))
  (setf (?type a-let*) bottom-t)
  (dolist (var (?var-list a-let*))
    (setf (?type var) bottom-t)))

(defmethod reset-type-annotation ((a-defined-named-const defined-named-const))
  (let ((value (?value a-defined-named-const)))
    (when (or (eq value :unknown) (eq value :forward))
      (setf (?type a-defined-named-const) bottom-t))))

(defmethod reset-type-annotation ((a-literal literal)))

(defmethod reset-type-annotation ((a-fun fun)))

(defmethod reset-type-annotation ((a-defined-fun defined-fun))
  (setf (?result-type a-defined-fun) bottom-t))

(defmethod reset-type-annotation ((a-sym sym)))

(defmethod reset-type-annotation :after ((a-var-ref var-ref))
  (reset-type-annotation (?var a-var-ref)))


(defun reset-type-annotations ()
  (traverse-module *module*
                   :before-funs (list #'reset-type-annotation)
                   :tr-fun-body-p #'(lambda (a-fun)
                                      (declare (ignore a-fun)) nil)))

  
;;------------------------------------------------------------------------------
;; Sollen/koennen die Aufrufkontexte der Funktion beachtet werden?
;;------------------------------------------------------------------------------
(defun use-call-contexts (fun)
  (and (not (?unknown-caller fun))
       (do-interprocedural-type-inference)))


;;------------------------------------------------------------------------------
;; Funktion zum Ermitteln der potentiell gelesenen/geschriebenen Variablen
;;------------------------------------------------------------------------------
(defun get-read/write-list (read/write-list fun dynamic-variables)
  (let ((result (if (listp read/write-list)
                    read/write-list
                    (append (if (local-fun-p fun)
                                (?free-lex-vars fun)
                                nil)
                            dynamic-variables))))
    (if (use-bindings-of-dynamic-variables)
        result
        (remove-if #'dynamic-p result))))


;;------------------------------------------------------------------------------
;; Setzt die Eintritts- und Ergebnistypumgebungen aller Funktionen auf ihren 
;; jeweiligen Startwert.
;;------------------------------------------------------------------------------
(defun set-all-predecessor-and-result-type-envs (a-module)
  (let* ((dynamic-variables 
          (remove-if-not #'(lambda (var) 
                             (eq (?constant-value (?sym var)) :no-const))
                         (?dyn-var-list a-module)))
         (imported-constants 
          (remove-if-not #'(lambda (var)
                             (and (imported-sym-p (?sym var))
                                  (eq (?constant-value (?sym var)) :unknown)))
                         (?dyn-var-list a-module)))
         (used-dynamic-variables
          (if (use-bindings-of-dynamic-variables)
              dynamic-variables
              nil)))

    (when (and (> *ti-verbosity* 1)
               (use-bindings-of-dynamic-variables)
               (> (length dynamic-variables) 0))
      (clicc-message "The module contains ~D dynamic variables."
                     (length dynamic-variables)))

    (setf *type-environment* (mapcar #'(lambda (var) (cons var top-t))
                                     used-dynamic-variables))

    (labels ((set-pred-and-result-type-env (fun)
               (let ((read-list  (get-read/write-list (?read-list fun) fun
                                                      used-dynamic-variables))
                     (write-list (get-read/write-list (?write-list fun) fun
                                                      used-dynamic-variables))
                     (init-type  (if (use-call-contexts fun) bottom-t top-t)))

                 ;; Setze die Eintrittstypumgebung.                 
                 ;;--------------------------------
                 (setf (?pred-type-env fun)
                       (mapcar #'(lambda (var)
                                   (cons var init-type))
                               (union read-list write-list)))

                 
                 ;; Setze zur intraprozeduralen Inferenz Ergebnistypen auf Top.
                 ;;------------------------------------------------------------
                 (unless (do-interprocedural-type-inference)
                   (setf (?result-type fun) top-t))
                 

                 ;; Setze die Ergebnistypumgebung.
                 ;;-------------------------------
                 (setf (?result-type-env fun)
                       (mapcar #'(lambda (var) (cons var bottom-t)) 
                               write-list)))
               
               
               ;; Binde bei allen Funktionen, deren Aufrufkontexte nicht
               ;; verwendet werden die Parametertypen an das Typ-Topelement.
               ;;-----------------------------------------------------------
               (unless (use-call-contexts fun)
                 (simple-set-parameter-types (?params fun)))))
       
      (mapc #'set-pred-and-result-type-env (?all-funs a-module)))


    (let ((dynamic-variable-type (if (use-bindings-of-dynamic-variables)
                                     bottom-t
                                     top-t)))
          (dolist (a-dynamic dynamic-variables)
            (setf (?type a-dynamic) dynamic-variable-type)))

    (dolist (an-imported-constant imported-constants)
      (setf (?type an-imported-constant) top-t))))


;;------------------------------------------------------------------------------
;; Traversiert das uebergebene Modul und markiert dabei diejenigen Funktionen
;; und Continuations, die als funktionales Objekt verwendet werden.
;;------------------------------------------------------------------------------
(defun prepare-type-inference (a-module)
  (traverse-module a-module
                   :after-funs (list #'preset-types)
                   :tr-fun-body-p #'(lambda (a-fun)
                                      (declare (ignore a-fun)) nil))

  (set-all-predecessor-and-result-type-envs a-module))


;;------------------------------------------------------------------------------
(provide "tipass1") 



