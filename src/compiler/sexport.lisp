;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Syntaktischer Export und Import
;;;
;;; $Revision: 1.35 $
;;; $Log: sexport.lisp,v $
;;; Revision 1.35  1994/01/24  16:15:29  sma
;;; Fehler in write-syntax-export korrigiert.
;;;
;;; Revision 1.34  1993/12/18  11:08:58  hk
;;; Noch einmal korrigiert.
;;;
;;; Revision 1.33  1993/12/18  09:48:17  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.32  1993/12/18  09:29:38  hk
;;; package-name nur auf Package, nicht auf Namen anwenden (wg. AKCL).
;;;
;;; Revision 1.31  1993/12/09  17:05:52  uho
;;; Meldung wird nicht mehr hier sondern zentral ausgegeben.
;;;
;;; Packages werden beim Einlesen der .def-Files angelegt. In
;;; 'syntax-import' wird dann die Package-Use-List und die Nicknames
;;; gestetzt.
;;;
;;; Revision 1.30  1993/12/09  10:04:58  uho
;;; Beim Importieren syntaktisch und als Programmobjekt exportierter
;;; Funktionen wird die zugehoerige Zwischensprach-Funktion in den neuen
;;; slot 'syntax' der 'imported-fun'-Zwischensprachobjekte eingetragen.
;;;
;;; Revision 1.29  1993/12/07  11:13:34  uho
;;; Achja. Ausserdem wird nun der Slot 'source' an den Funktionen des Moduls
;;; zurueckgesetzt, nachdem der syntaktische Export vorgenommen wurde.
;;; (Funktion: 'write-syntax-export')
;;;
;;; Revision 1.28  1993/12/07  11:06:56  uho
;;; Syntaktisch exportierte Funktionsdefinitionen werden nun nicht mehr
;;; durch 'p1-defun' importiert. Sie werden nun zur Interpretation nach
;;; LZS ueberfuehrt und in das globale Environment eingetragen
;;; (Funktion 'syntax-import-defun').
;;;
;;; Fuer ein IMPORT, das keine Package-Angabe enthaelt, wird eine
;;; IMPORT-Spezifikation in das .syntax-File geschrieben, die das aktuelle
;;; Package enthaelt. (Funktion 'write-syntax-export').
;;;
;;; Statt DEFUN wird das Schluesselwort SYNTAX-DEFUN verwendet, um die
;;; Qualitaet des 'syntaktischen' Exports zu verdeutlichen.
;;;
;;; Ein Syntax-Import findet nun auch dann statt, wenn (?package *module*)
;;; noch ungebunden ist (Im Quellcode stand dann noch keine entsprechende
;;; Package Operation). (Funktion 'source-code-looks-like-a-new-module')
;;;
;;; Revision 1.27  1993/09/07  09:51:43  ft
;;; Der Aufruf von p1-def-built-in in syntax-import liefert jetzt einen
;;; Wert fuer die Annotation order einer built-in-class.
;;;
;;; Revision 1.26  1993/07/27  16:53:06  uho
;;; IMPORT-Direktiven in Syntax-Headerfiles werden nun mit ausgewerteten
;;; Argumenten generiert.
;;; Beim Einlesen der Syntax-Headerfiles muessen IMPORT und EXPORT-
;;; Direktiven konstante Parameter besitzen.
;;;
;;;
;;; Revision 1.25  1993/07/19  14:48:05  uho
;;; Generierung des Aufrufs der Initialisierungsfunktion und des
;;; Zuruecksetzens von *PACKAGE* nach p1-load verschoben.
;;;
;;; Revision 1.24  1993/07/13  11:30:10  uho
;;; Beim Importieren von Modulen ('syntax-import') wird jetzt Code fuer
;;; den Aufruf der Initialisierungsfunktion des importierten Modul und das
;;; richtige Ruecksetzen auf das aktuelle Package generiert.
;;;
;;; Revision 1.23  1993/07/05  17:48:53  kl
;;; Schreibfehler behoben.
;;;
;;; Revision 1.22  1993/07/05  13:18:54  uho
;;; Neues Verfahren zum Schreiben und Lesen der Syntax-Headerfiles
;;; eingebaut.
;;;
;;; Revision 1.21  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.20  1993/05/19  16:17:28  uho
;;; Debugging-Funktionen auskommentiert.
;;;
;;; Revision 1.19  1993/05/14  13:18:55  hk
;;; L:: eingefuegt, Lisp Package wird nicht ge-un-used.
;;;
;;; Revision 1.18  1993/05/14  09:10:13  hk
;;; find-package nicht auf Package anwenden.
;;;
;;; Revision 1.17  1993/05/07  08:35:22  hk
;;; In syntax-import: Aufruf von p1-def-built-in.
;;;
;;; Revision 1.16  1993/05/06  14:51:52  uho
;;; Keine Ausgaben mehr waehrend des Syntax-Imports
;;;
;;; Revision 1.15  1993/05/03  15:24:23  uho
;;; Einlesen von DEFTYPEs eingefuegt.
;;;
;;; Revision 1.14  1993/04/21  10:11:35  hk
;;; write-syntax-export nach *OUT-FILENAME*.
;;;
;;; Revision 1.13  1993/04/21  08:53:11  ft
;;; Anpassung an 'def-built-in'.
;;;
;;; Revision 1.12  1993/04/20  09:17:08  uho
;;; Print-Parameter fuer write-syntax-export gesetzt.
;;;
;;; Revision 1.11  1993/04/20  09:07:57  uho
;;; Behandlung von DEFVAR beim Importieren eingefuegt.
;;;
;;; Revision 1.10  1993/04/20  08:40:53  uho
;;; DEFPARAMETER in write-syntax-export behandelt
;;;
;;; Revision 1.9  1993/04/16  11:02:49  kl
;;; require an den Anfang der Datei verlegt.
;;;
;;; Revision 1.8  1993/04/16  08:36:26  uho
;;; Syntax von PROCLAIM SPECIAL in DEFVAR umgeaendert
;;;
;;; Revision 1.7  1993/04/15  11:53:59  uho
;;; Verfahren zum Funktionsmarkieren umgestellt;
;;; Vergleich in  is-module-import  korrigiert.
;;;
;;; Revision 1.6  1993/04/15  08:31:53  hk
;;; mark-all-exported-syntax-functions kennt nun :COMPILER-MACRO.
;;;
;;; Revision 1.5  1993/04/15  07:52:34  hk
;;; Wert von *tr-fun-selector* korrigiert.
;;;
;;; Revision 1.4  1993/04/14  11:59:40  hk
;;; Syntax von Special Deklarationen korrigiert.
;;;
;;; Revision 1.3  1993/04/14  10:25:19  hk
;;; is-module-import: Neues Modul, wenn angegebenes Package in
;;; (IN-PACKAGE <Package-name> ...) nicht mit dem Package des gerade
;;; uebersetzten Modul uebereinstimmt.
;;;
;;; Revision 1.2  1993/04/14  07:57:02  kl
;;; Ignore-Deklarationen eingefuegt.
;;;
;;; Revision 1.1  1993/04/08  14:55:56  uho
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------
(in-package "CLICC")

(require "traverse")


(defvar *SYNTAX-EXTENSION* ".syntax")

;;------------------------------------------------------------------------------
;; Grundlegende Funktionen zur Manipulation der Liste der syntaktischen Exporte
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Erweitert die Liste der syntaktischen Exporte um das uebergebene Argument
;;------------------------------------------------------------------------------
(defun extend-syntax-export (construct)
  (setq *SYNTAX-EXPORT* (cons construct *SYNTAX-EXPORT*)))

;;------------------------------------------------------------------------------
;; Kuerzt die Liste der syntaktischen Exporte um das zuletzt erweiterte eine
;; Element
;;------------------------------------------------------------------------------
(defun retract-syntax-export ()
  (setq *SYNTAX-EXPORT* (cdr *SYNTAX-EXPORT*)))

;;------------------------------------------------------------------------------
;; Liefert die Liste der aufgesammelten Syntaxexporte in der Reihenfolge ihrer
;; Eintragung
;;------------------------------------------------------------------------------
(defun retrieve-syntax-export ()
  (reverse *SYNTAX-EXPORT*))



;;------------------------------------------------------------------------------
;; Ausgehend von den exportierten Macrodefinitionen muessen alle zur Expansion
;; benoetigten Funktionen syntaktisch mit exportiert werden, da sie bei
;; spaeterer Verwendung der Macros in anderen Modulen bekannt sein muessen, um
;; die Expansion vorzunehmen. Zu diesem Zweck werden die
;; Macroexpansionsfunktionen durchlaufen und alle von ihnen aufrufbaren
;; Funktionen zum exportieren markiert.
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Laufe durch die angegebene Liste the-list, die die Struktur 
;; (SYMBOL . DEFINITION), wie sie im globalen Environment verwendet wird,
;; besitzt. Binde das angegebene Element the-element an die DEFINITION und
;; fuehre den Rumpf the-body aus, falls SYMBOL aus dem aktuellen Package
;; exportiert wurde.
;;------------------------------------------------------------------------------
(defmacro examine-export-list ((the-element the-list) the-body)
  `(dolist (entry ,the-list)
    (let ((symbol (car entry))
          (,the-element (cdr entry)))
      (multiple-value-bind (symbol status)
          (find-symbol (symbol-name symbol) *PACKAGE*)
        (when (eq :EXTERNAL status)
          ,the-body)))))

;;------------------------------------------------------------------------------
;; Laufe durch die Definitionen des globalen Environments und markiere alle
;; syntaktisch zu exportierenden Programmobjekte
;;------------------------------------------------------------------------------
(defun mark-all-exported-syntax-functions ()

  ;; Betrachte die Exporte aus dem zum Modul gehoerigen Package:
  ;;------------------------------------------------------------
  (let ((*PACKAGE* (?package *MODULE*)))
  
    ;; Alle OPERATOREN untersuchen
    ;;----------------------------
    (examine-export-list
     (operator-def (?operators *GLOBAL-ENVIRONMENT*))
     (case (first operator-def)
       (:SPECIAL-FORM nil)
       (:USER-MACRO (mark-all-function-called-funs (rest operator-def)))
       (:SYS-MACRO nil)
       (:IMPORTED-FUN  nil)
       (:GLOBAL-FUN nil)
       (:COMPILER-MACRO nil)
       (:FORWARD nil)
       (otherwise
        (internal-error
         "mark-all-exported-syntax-functions"
         "Unbekannte Operator Definition der Art ~A fuer Symbol ~A~%"
         (first operator-def)
         symbol))))

    ;; Alle SETF-Methoden untersuchen
    ;;-------------------------------
    (examine-export-list
     (setf-method-def (?setf-methods *GLOBAL-ENVIRONMENT*))
     (case (first setf-method-def)
       ((:SIMPLE-SETF-METHOD :COMPLEX-SETF-METHOD)
        (mark-all-function-called-funs (rest setf-method-def)))
       (otherwise
        (internal-error
         "mark-all-exported-syntax-functions"
         "Unbekannte Setf-Definition der Art ~A fuer Symbol ~A~%"
         (first setf-method-def)
         symbol))))

    ;; Alle DEFTYPES untersuchen
    ;;--------------------------
    (examine-export-list
     (type-def (?types *GLOBAL-ENVIRONMENT*))
     (case (first type-def)
       (:TYPE 
        (mark-all-function-called-funs (rest type-def)))
       (:BUILT-IN
        (mark-all-function-called-funs (?type-expander (rest type-def))))
       (:CLASS  nil)
       (otherwise
        (internal-error
         "mark-all-exported-syntax-functions"
         "Unbekannte Typ-Definition der Art ~A fuer Symbol ~A~%"
         (first type-def)
         symbol))))))
  
        
;;------------------------------------------------------------------------------
;; Zur Ausgabe der Symbole im Syntax-Headerfile
;; --------------------------------------------
;;
;; Es ist moeglich die Ausgabe der Symbole im Syntax-Headerfile grundsaetzlich
;; vollstaendig qualifiziert vorzunehmen. Das macht die Headerfiles nur schwer
;; lesbar. Um die Ausgabe uebersichtlicher zu halten, sollen die Symbol nach
;; Moeglichkeit unqualifiziert auftreteten. Dazu wird folgende Verfahren
;; verwendet. Zu Anfang des Moduls stehende Veraenderungen des Package-Systems
;; werden nachvollzogen, spaetere Aenderungen werden nicht nachvollzogen,
;; sondern statt dessen qualifizierte Namen erzeugt. Dieses Vorgehen fuehrt
;; nicht immer zu einer maximalen Anzahl von unqualifizierten Symbolen. In den
;; meisten Faellen wird aber vermutlich das Packagesystem am Anfang eines
;; Moduls in den richtigen Zustand gebracht und dann waehrend des Moduls nicht
;; mehr weiter veraendert. Dann liefert das Verfahren vernuenftige Ergebnisse.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Vollziehe die Veraenderung des Package-Systems durch IN-PACKAGE fuer den
;; Syntax-Export nach.
;;------------------------------------------------------------------------------
(defun syntax-import-in-package (name &key nicknames use)
  (declare (ignore nicknames))
  ;; nicknames werden ignoriert, da es sich hier um Wiederholungen schon
  ;; einmal in p1-in-package bearbeiteter Formen handelt.
  (let ((new-package (find-package name)))
    (syntax-import-use-package use new-package)
    (setq *PACKAGE* new-package)))

;;------------------------------------------------------------------------------
;; Vollziehe die Veraenderung des Package-Systems durch USE-PACKAGE fuer den
;; Syntax-Export nach.
;;------------------------------------------------------------------------------
(defun syntax-import-use-package (packages-to-use &OPTIONAL package)
  (when packages-to-use                 ; NIL bedeutet explizit kein Package
    (let (current-package)

      (if (null package)
          (setq current-package *PACKAGE*)
          (setq current-package (if (packagep package)
                                    package
                                    (find-package package))))

      (when (null current-package)
        (clicc-error "syntax-import-use-package: package undefined!"))
                 
      ;; Aus einem Package eine einelementige
      ;; Liste von Packages machen
      ;; ------------------------------------
      (when (atom packages-to-use)
        (setq packages-to-use (list packages-to-use)))

      (when (set-difference packages-to-use
                            (mapcar #'package-name
                                    (package-use-list current-package))
                            :test #'string=)

        ;; use-package nachvollziehen
        ;; --------------------------
        (use-package packages-to-use current-package)))))


;;------------------------------------------------------------------------------
;; Importiere Symbole in das aktuelle Modul.
;;------------------------------------------------------------------------------
(defun syntax-import-import (symbols &OPTIONAL package)
  (let ((current-package
         (if (null package) *PACKAGE* (find-package package))))

    (when (null current-package)
      (clicc-error "syntax-import-import: package ~s is undefined" package))

    (unless (eq (car symbols) 'L::QUOTE)
      (internal-error
       "syntax-import-import"
       "Argumentliste ~A muss mit QUOTE beginnen!~%"
       symbols))
  
    (dolist (symbol (cdr symbols))
      (import symbol current-package))))

;;------------------------------------------------------------------------------
;; Exportiert Symbole
;;------------------------------------------------------------------------------
(defun syntax-import-export (symbols &OPTIONAL package)
  (let ((current-package
         (if (null package) *PACKAGE* (find-package package))))

    (when (null current-package)
      (clicc-error "syntax-import-export: package ~s is undefined" package))

    (unless (eq (car symbols) 'L::QUOTE)
      (internal-error
       "syntax-import-export"
       "Argumentliste ~A muss mit QUOTE beginnen!~%"
       symbols))
    
    (dolist (symbol (cdr symbols))
      (export symbol current-package))))

;;------------------------------------------------------------------------------
;; Stellt sicher, das ein Package im Wirtssystem existiert
;;------------------------------------------------------------------------------
(defun syntax-import-make-package (package &KEY
                                           ((:USE uselist) '("LISP"))
                                           ((:NICKNAMES nicklist) '()))
  (let ((the-package (find-package package)))
    (cond
      (the-package                      ; existiert mach's so wie frueher
       (let ((name (package-name the-package)))
       
         (unuse-package (package-use-list the-package) the-package)
         (use-package uselist the-package)

         (rename-package the-package (find-secret-package-name))
         (rename-package the-package name nicklist)))

      (T
     (make-package package :USE uselist :NICKNAMES nicklist)))))


;;------------------------------------------------------------------------------
;; Importiere (syntaktisch) eine Funktionsdefinition.
;; Syntaktische Funktiondefinitionen werden fuer die Interpretation zur
;; Uebersetzungszeit nach LZS ueberfuehrt und in das globale Environment
;; eingetragen. Sie werden aber nicht in die Funktionsliste des aktuellen
;; Moduls eingetragen, da fuer sie kein Code generiert werden soll.
;;------------------------------------------------------------------------------
(defun syntax-import-defun (name_lambda-list_body)
  (multiple-value-bind (name lambda-list_body)
      (parse-named-lambda 'L::SYNTAX-DEFUN name_lambda-list_body)

    (let* ((SETF-FUN (consp name))
           (operator-def (if SETF-FUN
                             (get-setf-fun-def name)
                             (get-operator-def name)))
           (fun (make-instance 'global-fun :symbol name)))

      (case (car operator-def)
        ((nil)
         (if SETF-FUN
             (set-global-setf-fun-def name :GLOBAL-FUN fun)
             (set-global-fun name fun)))
        (:FORWARD
         (setf (?value (cdr operator-def)) fun)
         (setf (car operator-def) :GLOBAL-FUN)
         (setf (cdr operator-def) fun))
        (:IMPORTED-FUN
         (setf (?syntax (cdr operator-def)) fun))
        (otherwise (internal-error
                    "syntax-import-defun"
                    "Unexpected operator-def: ~A" (car operator-def))))
      
      ;; fun jetzt mit Leben fuellen.
      (in-compile-time-env
       (p1-named-lambda fun
                        name
                        (if SETF-FUN (second name) name)
                        lambda-list_body)))))

;;------------------------------------------------------------------------------
;; Schreibe fuer das aktuelle Modul ein Syntax-Headerfile unter Ausnutzung der
;; in Pass1 aufgesammelten Informationen in *SYNTAX-EXPORT*, dem Environment
;; und an den Zwischensprachobjekten (insbesoner fun, fuer deren Quellcode).
;; Die Syntax-Headerfiles werden geschrieben waehrend das LISP-Package aktiv
;; ist. Dadurch werden alle Symbole, die nicht im LISP-Package (|clicc-lisp|)
;; sind, qualifiziert mit ihrem Home-Package herausgeschrieben. Symbole des
;; LISP-Packages werden ls einzige nicht qualifiziert. Dadurch ist es
;; gleichgueltig, welches in ihr Home-Package ist (|original-lisp| oder
;; |clicc-lisp|).  In einem zweiten Teil werden Veraenderungen auf das Package
;; System und IMPORTe vorgenommen.
;;------------------------------------------------------------------------------
(defun write-syntax-export ()

  (clicc-message "Writing syntax header file ...")

  ;; Alle syntaktisch benoetigten Funktionen markieren
  ;; -------------------------------------------------
  (mark-all-exported-syntax-functions)

  ;; Headerfile schreiben
  ;; --------------------
  (with-open-file
      (syntax-header-file
       (concatenate 'string *OUT-FILENAME* *SYNTAX-EXTENSION*)
       :direction :output
       :if-exists :supersede)
    
    (let* ((*package* (find-package "LISP"))
           (this-package (?package *module*))
           (this-package-name (package-name this-package))
           (top-level-forms (retrieve-syntax-export))
           (*print-level* NIL)
           (*print-length* NIL)
           (*print-circle* NIL)
           (exported-symbols '()))
      (labels ((haeh (form)
                   (internal-error
                    "write-syntax-export"
                    "Unbekannte Form ~A in *SYNTAX-EXPORT*~%"
                    form)))

      ;; Kopf des Header-Files schreiben
      ;; -------------------------------
      (format syntax-header-file
              ";;; This is a CLICC-generated syntax header file.~%")

      (format syntax-header-file
              ";;; It contains syntactic exports of module ~A~%~%"
              this-package-name)


      ;; Erstmal klarstellen, wo wir sind
      ;; --------------------------------
      (format syntax-header-file "(IN-PACKAGE \"LISP\")~%")
      
      ;; Dann das Package erzeugen
      ;;--------------------------
      (format syntax-header-file "~S~%"
              `(L::MAKE-PACKAGE ,this-package-name :USE NIL))
      
      ;; Die IMPORTs angeben
      ;; -------------------
      (dolist (form top-level-forms)
        (when (null form) (haeh form))
        (when (eq (car form) 'L::IMPORT)
          (let* ((symbols-form (cadr form))
                 (package-form (caddr form))
                 (symbols (p1-eval symbols-form))
                 (package (when package-form (p1-eval package-form))))
            (when (atom symbols)
              (setq symbols (list symbols)))
            (format
             syntax-header-file
             "~S~%"
             `(L::IMPORT
               (L::QUOTE ,symbols)
               ,(if package
                    (package-name (p1-eval package-form))
                    (if (slot-boundp *module* 'package)
                        (package-name (?package *module*))
                        "USER")))))))

      ;; Die EXPORTs angeben
      ;; -------------------
      (do-external-symbols (sym this-package)
          (unexport sym this-package)
          (push sym exported-symbols))
      (format syntax-header-file "~S~%"
              `(L::EXPORT (L::QUOTE ,exported-symbols) ,this-package-name))
      
      ;; Die USEs darstellen
      (when (package-use-list this-package)
        (format syntax-header-file "~%~S~%"
                `(L::USE-PACKAGE (,@(mapcar
                                    #'package-name
                                    (package-use-list this-package)))
                  ,this-package-name)))

      ;; Direkt syntaktisch exportierte Programmobjekte schreiben
      ;; --------------------------------------------------------
      (dolist (form top-level-forms)
        (labels ((write? (form)
                   (let ((sym (get-symbol-bind (cadr form))))
                     (and sym (?exported sym))))
                 (print-only-exported (form)
                   (when (write? form)
                     (print form syntax-header-file))))
                 
          (when (null form) (haeh form))
          (case (car form)             
            (L::DEFMACRO (print-only-exported form))
            (L::DEFSETF  (print-only-exported form))
            (L::DEFTYPE  (print-only-exported form))
            (L::DEF-BUILT-IN (print-only-exported form))
            ((L::DEFPARAMETER L::DEFVAR L::DEFCONSTANT)
             (print-only-exported `(L::DEFVAR ,(cadr form))))
            ((L::IN-PACKAGE L::USE-PACKAGE) nil)
            (L::IMPORT nil)
            (otherwise (haeh form))))))

      (terpri syntax-header-file)
      ;; Markierte Funktionen schreiben
      ;; ------------------------------
      (dolist (fun (?fun-list *module*))
        (when (and (defined-fun-P fun) (?syntactically-exported fun))
          (print `(L::SYNTAX-DEFUN
                   ,(?symbol fun)
                   ,@(?source fun)) syntax-header-file)
          (slot-makunbound fun 'source))) 

      ;; Die Symbole wieder exportieren
      ;; ------------------------------
      (dolist (symbol exported-symbols)
        (export symbol this-package))))

  (values))

;;------------------------------------------------------------------------------
;; Markiere Funktionen, die durch die Funktion fun (idR. eine Makroexpansions-
;; funktionen) aufgerufen werden. Hier wird das allgemeine Durchlaufschema
;; fuer die Zwischensprache verwendert.
;;------------------------------------------------------------------------------
(defun mark-all-function-called-funs (fun)
  (let ((*tr-analyzation-path* ())
        (*tr-before-funs* (list #'mark-syntax-export)) 
        (*tr-after-funs* ())
        (*tr-fun-selector* #'(lambda (a-zws-object) 
                               (declare (ignore a-zws-object)) NIL))         
        (*tr-app-form-p* #'(lambda (a-zws-object) 
                             (declare (ignore a-zws-object)) T))   
        (*tr-fun-body-p* #'is-defined-fun-without-recursion-p))
    (traverse-zs fun)))

;;------------------------------------------------------------------------------
;; Im allgemeinen hat das Markieren auf Zwischensprachkonstrukte keine Wirkung
;;------------------------------------------------------------------------------
(defmethod mark-syntax-export ((a-zws-object zws-object)))

;;------------------------------------------------------------------------------
;; Aber fuer Funktionen hat das schon eine Wirkung, naemlich das Setzen des
;; synactically-exported slots.
;;------------------------------------------------------------------------------
(defmethod mark-syntax-export ((a-fun defined-fun))
  (setf (?syntactically-exported a-fun) T))

;;------------------------------------------------------------------------------
;; Und fuer Funktionen mir Vorwaertsreferenz, die als named-constants
;; behandelt werden, muss die zugehoerige Funktion markiert werden.
;;------------------------------------------------------------------------------
(defmethod mark-syntax-export ((a-const defined-named-const))
  (when (defined-fun-P (?value a-const))
    (traverse-zs (?value a-const))))

;;------------------------------------------------------------------------------
;; Ueberpruefe, ob in dem angegebenen File ein Modul definiert wird.
;; Erste Top-Level-Form ist ein (IN-PACKAGE <Package-name> ...) und das
;; angegebene Package stimmt nicht mit dem Package des gerade uebersetzten
;; Moduls ueberein.
;; Liefere ein Flag: T = ein neues Modul, NIL = Bestandteil des aktuellen Moduls
;;------------------------------------------------------------------------------
(defun is-module-import (filename &optional ext)
  (let ((*CLICC-FILENAME* (clc-probe-file filename ext))
        (syntax-filename  (clc-probe-file filename *SYNTAX-EXTENSION*))
        (*PACKAGE* (find-package "LISP")))
    
    (labels ((source-code-looks-like-a-new-module ()
               (block fun
                 (if *CLICC-FILENAME*
                     ;; Quellcode auf Import-Syntax testen
                     ;; ----------------------------------
                     (with-open-file
                         (sourcecode *CLICC-FILENAME* :direction :input)
                       (loop
                        (let ((form (clicc-read sourcecode)))
                          (cond
                            ((atom form) (return-from fun NIL))
                            ((eq (car form) 'L::IN-PACKAGE)
                             (return-from fun
                               (and
                                (>= (length form) 2)
                                (not
                                 (and
                                  (slot-boundp *module* 'package)
                                  (eq (find-package (p1-eval (second form)))
                                      (?package *module*)))))))
                            (T (return-from fun NIL))))))
                     ;; kein Quellcode gefunden: nimm an er gehoere zu einem
                     ;; Modul
                     (return-from fun T)))))
      
      (and (source-code-looks-like-a-new-module)
           (if (null syntax-filename)
               (if *module-compiler*
                   (clicc-error "Cannot INCLUDE another module from file ~A"
                                filename)
                   NIL)
               T)))))

;;------------------------------------------------------------------------------
;; Importiere die im angegebenen File vorhandenenen Syntax Exporte
;;------------------------------------------------------------------------------
(defun syntax-import (filename)
  (let* ((*PACKAGE* (find-package "LISP"))
         (*CLICC-FILENAME* (clc-probe-file filename *SYNTAX-EXTENSION*)))
    (cond
      ((null *CLICC-FILENAME*)
       (clc-error "File ~A~A does not exist.~%" filename *SYNTAX-EXTENSION*))
      (T
       (block read-syntax-forms
         (with-open-file (headerfile *CLICC-FILENAME* :direction :input)
           (let ((*CLICC-PRINT* NIL)
                 (built-in-class-order 1))
             (loop
              (let* ((form (clicc-read headerfile)))
                (if (eq '|eof| form)
                    (return-from read-syntax-forms))
                (let* ((name (car form))
                       (args (cdr form)))
                  (case name
                    (L::IN-PACKAGE
                     (apply #'syntax-import-in-package args))
                    (L::USE-PACKAGE
                     (apply #'syntax-import-use-package args))
                    (L::IMPORT
                     (apply #'syntax-import-import args))
                    (L::EXPORT
                     (apply #'syntax-import-export args))
                    (L::MAKE-PACKAGE
                     (apply #'syntax-import-make-package args))  
                    (L::SYNTAX-DEFUN    (syntax-import-defun args))
                    (L::DEFMACRO (p1-defmacro args))
                    (L::DEFSETF  (p1-defsetf args))
                    (L::DEFTYPE  (p1-deftype args))
                    (L::DEF-BUILT-IN
                        (p1-def-built-in
                         (append args
                                 `(:order ,built-in-class-order)))
                        (incf built-in-class-order))
                    (L::DEFVAR  (p1-defvar args))
                    (otherwise (internal-error
                                "syntax-import"
                                "Undefined keyword: ~A" name)))))))))))))

;;------------------------------------------------------------------------------
(provide "sexport")

