;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Start und Abschluss von Pass 1
;;;
;;; $Revision: 1.84 $
;;; $Log: p1main.lisp,v $
;;; Revision 1.84  1994/05/22  16:16:39  sma
;;; Erzeugen einer leeren '*-ffi.h'-Datei verhindert.
;;;
;;; Revision 1.83  1994/04/18  12:17:01  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Finalisieren der FFtypes eingebaut.
;;;
;;; Revision 1.82  1994/02/21  09:59:37  ft
;;; Aufruf von export-classes in p1-end hinzugefügt.
;;;
;;; Revision 1.81  1994/02/09  14:01:49  hk
;;; Zusätzliche Packages RT und FFI für das *lisp-moduke* werden erst in
;;; p1-end und nicht schon in init-pass1 in den Slot package-list des
;;; Moduls eingetragen, damit die Initialisierungsfunktion des Lisp Moduls
;;; *package* auf das Lisp-Package setzt.
;;;
;;; Revision 1.80  1994/02/04  08:16:30  hk
;;; Fehler im Formatstring der Fehlermeldung in pass_1-of-file behoben.
;;;
;;; Revision 1.79  1994/02/02  12:37:48  hk
;;; (require "p1compmac") entfernt, da nicht mehr benötigt.
;;;
;;; Revision 1.78  1994/02/01  11:26:30  hk
;;; Fehlermeldung in pass_1-of-file verschönert.
;;; make-instance 'global-fun -> make-global-fun, wg. raw slots
;;;
;;; Revision 1.77  1993/12/22  12:04:12  hk
;;; Packages müssen auch initailiert werden, wenn das Modul keine Symbole
;;; definiert.
;;;
;;; Revision 1.76  1993/11/30  16:51:25  sma
;;; Funktionen mit einem uninterned symbol werden nicht exportiert
;;;
;;; Revision 1.75  1993/11/05  14:20:50  hk
;;; require an den Dateianfang verschoben, um Probleme mit akcl zu vermeiden.
;;;
;;; Revision 1.74  1993/10/18  10:14:14  hk
;;; generate-init-fun generiert keinen Code zur Initialisierung des
;;; Package-Systems, wenn ein kleines Laufzeitsystem ohne Packagefunktoinen
;;; verwendet wird.
;;;
;;; Revision 1.73  1993/10/14  17:21:34  hk
;;; Die Funktion in ?toplevel-forms eines Moduls bekommt als Rumpf ein
;;; empty-list, wenn keine Toplevel Forms vorliegen.
;;;
;;; Revision 1.72  1993/08/26  12:18:13  hk
;;; Initialisierungsfunktion eines importierten Moduls bekommt unbekannten
;;; write-effect, damit ihr Aufruf nicht wegoptimiert wird.
;;;
;;; Revision 1.71  1993/08/26  09:58:05  uho
;;; Es wird ggf. fuer die Initialisierung Code generiert, damit das
;;; USER-Package das LISP-Package used (CLtL2, 11.6. Builtin Packages, p.
;;; 258).
;;;
;;; Revision 1.70  1993/08/19  16:48:53  hk
;;; In sym-list eines Moduls wird nun schon von p1-make-new-sym
;;; geschrieben.
;;;
;;; Revision 1.69  1993/07/20  15:48:31  hk
;;; init-fun-name und symbol-base eines Moduls und der Slot adr der
;;; Initialisierungsfunktion werden erst in codegen gesetzt
;;;
;;; Revision 1.68  1993/07/19  14:58:18  uho
;;; Beim Erzeugen der Modul-Zwischensprachkomponente jetzt auch den neuen Slot
;;; 'init-fun-name' und auch schon 'symbol-base' initialisiert. Alle
;;; Aufrufe von 'calc-init-fun-name' durch Lesen des slots ersetzt.
;;;
;;; Revision 1.67  1993/07/13  11:22:08  uho
;;; Die Aufrufe der Initialisierungsfunktionen von Modulen werden jetzt an
;;; der Stelle generiert, an der das importierende 'load' steht. Das
;;; entspricht eher dem Verhalten des Komplett-Compilers.
;;; 'calc-init-module-forms' geloescht und 'init-lisp' eingefuegt.
;;; In 'calc-init-fun-name' wird jetzt 'calc-C-name' aufgerufen um immer
;;; einen gueltigen C-Identifikator zu generieren.
;;;
;;; Revision 1.66  1993/07/09  15:30:44  hk
;;; Das Lisp Modul generiert die Packages RT und FFI zur Laufzeit.
;;;
;;; Revision 1.65  1993/07/02  14:52:11  wg
;;; Nach (load ...) ein (setq *package* (find-package old-package))
;;; anstatt (in-package old-package) generiert.
;;;
;;; Revision 1.64  1993/06/23  10:00:08  hk
;;; *features* an '(:CLiCC) gebunden, Fehler in check-undef-funs behoben.
;;;
;;; Revision 1.63  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.62  1993/06/10  09:11:01  pm
;;; require fftypes entfernt
;;;
;;; Revision 1.61  1993/05/31  17:06:55  pm
;;; Einbindung von finalize-cal-in-funs in p1-end eingtragen
;;;
;;; Revision 1.60  1993/05/19  14:59:51  hk
;;; check-undef-funs korrigiert.
;;;
;;; Revision 1.59  1993/05/19  10:22:10  hk
;;; *CLICC-INPUT* gestrichen.
;;;
;;; Revision 1.58  1993/05/17  08:01:42  hk
;;; IN-PACKAGE -> L::IN-PACKAGE.
;;;
;;; Revision 1.57  1993/05/12  14:46:46  hk
;;; fuer undefinierte Funktionen wird automatisch eine Definition generiert,
;;; die einen Laufzeitfehler bewirkt.
;;;
;;; Revision 1.56  1993/04/30  13:15:46  hk
;;; Symbol Array wird nur initialisiert, wenn im Modul Symbole
;;; definiert werden.
;;;
;;; Revision 1.55  1993/04/22  11:34:25  hk
;;; Sonderbehandlung bei *HEADER-FILE*, *CLICC-LISP-PROGRAM*, *CLICC-MODULE*,
;;; *SYSTEM-HEADER-FILES* gestrichen. *package* wird bei *lisp-module* auf
;;; Lisp Package statt User package gesetzt. (?package *module*) nach
;;; Bearbeitung eines Moduls auf User package setzen, falls kein in-package
;;; im Programm vorkam. Neue Funktion (generate-init-fun) in p1-end.
;;; Symbole des rt Package werden vor syntax export unexported.
;;; generate-init-fun: Aufruf der Initialisierungsfunktionen der
;;; importierten Module, Initialisierung der Symbole des Moduls,
;;; Initialisierung von *package*. Sonderbehandlung des Lisp Moduls:
;;; Initialisierung der Symbole erst nach Ausfuehrung der Modulinitialisierung.
;;; Initialisierungsfnktion heisst I<modul name>.
;;;
;;; Revision 1.54  1993/04/14  10:38:34  hk
;;; export-funs beachtet (setf f), (?package *module*) wird in p1-end
;;; mit dem User Package initialisiert, wenn kein in-package im Programm vorkam.
;;;
;;; Revision 1.53  1993/04/13  10:05:17  hk
;;; export-funs:  Setzen des exported Flags von Funktionen.
;;;
;;; Revision 1.52  1993/04/08  15:01:39  uho
;;; Aenderungen fuer Syntax-Export ge-merged
;;;
;;; Revision 1.51  1993/04/08  07:53:27  hk
;;; (require p1compmac)
;;;
;;; Revision 1.50  1993/04/05  09:12:44  hk
;;; p0-init-special-funs p1 gestrichen, da compiler-macros
;;;
;;; Revision 1.49  1993/03/30  12:31:29  ft
;;; Aufruf der zweiten Phase der Klassenverarbeitung in p1-end.
;;;
;;; Revision 1.48  1993/03/22  17:32:08  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.47  1993/03/22  11:19:53  hk
;;; ?package vom Modul bekommt Defaultwert User-Package,
;;; Default-Toplevel-Form (in-package USER) gestrichen.
;;;
;;; Revision 1.46  1993/03/19  11:17:44  hk
;;; Slot name in *module* wird initialisiert mit dem Namen der Datei.
;;;
;;; Revision 1.45  1993/03/18  14:52:09  uho
;;; Initialisierung von *SYNTAX-EXPORT* eingefuegt.
;;;
;;; Revision 1.44  1993/03/16  13:45:40  ft
;;; ffzsdef wird nicht mehr hier sondern in zsdef required.
;;;
;;; Revision 1.43  1993/03/12  09:49:28  ft
;;; make-instance als expandierbar (Pass 1) deklariert.
;;;
;;; Revision 1.42  1993/02/16  16:42:38  hk
;;; Revision Keyword eingefuegt, Symbole des zu uebersetzenden Programms
;;; durch clicc-lisp:: gekennzeichnet, *CLICC-PACKAGE* durch *PACKAGE* ersetzt.
;;;
;;; Revision 1.41  1993/01/08  15:46:11  hk
;;; clicc-error -> clcerror
;;;
;;; Revision 1.40  1993/01/07  10:00:20  hk
;;; Fehler mit special-sys-fun behoben.
;;;
;;; Revision 1.39  1993/01/07  08:29:50  hk
;;; Fehler in macrolet von p1-special-funs behoben.
;;;
;;; Revision 1.38  1993/01/06  13:03:40  hk
;;; Funktionen {p1,p2,p3,cg}-special-funs vereinheitlicht.
;;;
;;; Revision 1.37  1993/01/02  13:02:05  kl
;;; Wenn die Codegenerierung mittels *NO-CODEGEN* abgeschaltet ist, dann
;;; wird check-forward-refs aufgerufen. Kommentare geaendert.
;;;
;;; Revision 1.36  1992/12/22  16:49:19  hk
;;; check-forward-refs wird nicht aufgerufen, weil ein Fehler auch
;;; waehrend Codegen entdeckt wird und man die Meldung nicht 2x haben will.
;;;
;;; Revision 1.35  1992/12/07  14:21:03  hk
;;; Message in pass_1-of-file veraendert.
;;;
;;; Revision 1.34  1992/12/03  16:59:52  hk
;;; Fehlermeldung, wenn Toplevel Forms ausserhalb des Haupt-Moduls auftreten.
;;;
;;; Revision 1.33  1992/11/23  13:31:39  kl
;;; Annotation dyn-var-list in module wird gesetzt.
;;;
;;; Revision 1.32  1992/11/20  13:54:40  ft
;;; Aufruf der Init.fkt. fuer die Hash-Table fuer zw-sym-fun eingefuegt.
;;;
;;; Revision 1.31  1992/11/19  13:22:51  ft
;;; Erweiterung von p0-init-pass1 um Aufruf von init-macro-error-funs.
;;;
;;; Revision 1.30  1992/11/17  12:59:18  ft
;;; Aufbau von rt::*package-info* erfolgt jetzt direkt
;;; statt mit p1-make-constant.
;;;
;;; Revision 1.29  1992/11/17  12:12:39  ft
;;; (require p1env) verschoben.
;;;
;;; Revision 1.28  1992/11/06  09:09:28  ft
;;; Einbindung von finalize-generic-funs in p1-end.
;;;
;;; Revision 1.27  1992/11/05  10:48:37  pm
;;; requires fuer FFI eingehaengt
;;;
;;; Revision 1.26  1992/10/16  11:29:34  ft
;;; Aufruf von finalize-classes in p1-end eingetragen.
;;;
;;; Revision 1.25  1992/09/29  21:05:47  hk
;;; pass_1-of-file bricht nicht mehr ab, wenn die Datei nicht existiert
;;; sondern gibt nur eine Fehlermeldung aus und faehrt fort. Vorkommen von
;;; (throw 'ABORT nil) entfernt, Fehlerzaehlen in clcmain.
;;;
;;; Revision 1.24  1992/09/29  14:07:34  ft
;;; Kommentierung von p1-end verschoenert.
;;;
;;; Revision 1.23  1992/09/03  19:34:54  ft
;;; Aufteilung von 'p1-end' in einzelne Funktionen
;;;
;;; Revision 1.22  1992/08/31  08:40:00  ft
;;; p1class eingebunden
;;;
;;; Revision 1.21  1992/08/05  13:34:31  hk
;;; Leere Paramterliste fuer lisp-main explizit angeben.
;;;
;;; Revision 1.20  1992/08/03  08:08:19  hk
;;; Slots in module belegt.
;;;
;;; Revision 1.19  1992/07/30  13:07:08  hk
;;; RUNTIME-Package wird zur Package-List des Moduls hinzugefuegt.
;;;
;;; Revision 1.18  1992/07/28  10:51:50  hk
;;; Name des Hauptprogramms ist nun rt::LISP-MAIN.
;;;
;;; Revision 1.17  1992/07/27  16:42:37  hk
;;; Header-Dateien werden nun von p0-init mit Prozedur read-sys-header-files
;;; eingelesen, Umbenennungen, Toplevel-Forms in *GLOBAL-ENVIRONMENT*, etc. .
;;;
;;; Revision 1.16  1992/07/22  18:10:50  hk
;;; (setq *HEADER-FILE* (strip-path name)) gestrichen, da nicht benutzt.
;;;
;;; Revision 1.15  1992/07/22  17:24:03  hk
;;; get-global-operator --> get-global-fun.
;;;
;;; Revision 1.14  1992/07/22  15:13:14  hk
;;; *CLICC-PACKAGE* in p0-init-pass1 gesetzt.
;;;
;;; Revision 1.13  1992/07/09  11:25:48  hk
;;; Schreibfehler.
;;;
;;; Revision 1.12  1992/07/09  11:22:45  hk
;;; *GLOBAL-ENVIRONMENT* zunaechst doch nicht auf nil setzen.
;;;
;;; Revision 1.11  1992/07/07  12:41:04  ft
;;; Erweiterung um (require "p1generic")
;;;
;;; Revision 1.10  1992/07/07  11:21:59  hk
;;; Nach Pass1 *GLOBAL-ENVIRONMENT* auf nil gesetzt.
;;;
;;; Revision 1.9  1992/07/07  09:49:13  hk
;;; sym-list von *module* wird in Pass1 und nicht erst in Pass3 gesetzt.
;;;
;;; Revision 1.8  1992/07/06  08:11:45  hk
;;; Package-List des Moduls mit Keywor Lisp und User Package initalisiert.
;;;
;;; Revision 1.7  1992/07/06  07:54:47  hk
;;; In Warning Package-Name statt Package verwendet.
;;;
;;; Revision 1.6  1992/07/02  14:01:57  hk
;;; Warnun fuer nicht definierte Forwaertsreferenzen.
;;;
;;; Revision 1.5  1992/06/05  09:53:08  hk
;;; Schreibfehler.
;;;
;;; Revision 1.4  1992/06/04  15:23:00  hk
;;; Explizites (in-package "USER") eingefuegt.
;;;
;;; Revision 1.3  1992/06/04  14:15:54  hk
;;; *clicc-print* wird beim lesen von Header-Files auf nil gesetzt.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;; 
;;;               1991/12/11
;;; Oeffnen der .def-Files mit :if-exists :supersede
;;; 
;;;               1991/07/22
;;; Nach dem Laden einer Datei wird *package* ggf. wieder auf das Package der
;;; ladenden Datei zurueckgesetzt.
;;;
;;;               1991/07/18
;;; PASS_1-of-DATA: Default-Package ist das Package der ladenden Datei.
;;; 
;;;               1990/12/14
;;; Created
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "p1macro")   ; muss vor "p1tlf" stehen !!
(require "p1foreign")
(require "ffload")
(require "p1tlf")
(require "p1lambda")
(require "p1spform")
(require "p1type")
(require "p1decls")
(require "p1struct")
(require "p1eval")
(require "p1generic")
(require "p1class")
(require "sexport")

;;------------------------------------------------------------------------------
;; Initialisierung fuer das Frontend
;;------------------------------------------------------------------------------
(defun init-pass1 ()  
  (setq *CLICC-REQUIRED*  ())           ; Liste der uebersetzten Module
  (setq *PACKAGE* (find-package (if *lisp-module* "LISP" "USER")))
  (let ((name (if *module-compiler*
                  (strip-path *FILENAME*)
                  "main")))
    (setq *module* (make-module
                    :fun-list (empty-queue)
                    :class-def-list (empty-queue)
                    :named-const-list (empty-queue)
                    :var-list (empty-queue)
                    :sym-list (empty-queue)
                    :toplevel-forms nil
                    :package-list (empty-queue)
                    :loaded-modules (empty-queue)
                    :name name)))
  
  (addnew-q (find-package "KEYWORD") (?package-list *module*))

  (setq *SYNTAX-EXPORT* ()))            ; Liste der syntakt. exp. Konstrukte

;;------------------------------------------------------------------------------
;; Pass 1 des COMMON-LISP nach C Compilers.
;;------------------------------------------------------------------------------
(defun pass_1 ()
  
  ;; Alten Dispatch Makro fuer Strukturen retten.
  ;;---------------------------------------------
  (let ((original-s-reader (get-dispatch-macro-character #\# #\S))
        (*features* '(:CLiCC)))
    
    ;; Neuen Dispatch Makro fuer Strukturen setzen.  Durch die Angabe eines
    ;; eigenen #S-Readers kann der Compiler konstante Strukturen einlesen und
    ;; verarbeiten.
    ;;-----------------------------------------------------------------------
    (set-dispatch-macro-character #\# #\S #'p1-s-reader)
    
    (unwind-protect
         (pass_1-of-file *FILENAME* *EXTENSION*)

      ;; Protected: Ruecksetzen des Dispatch Makros
      ;;-------------------------------------------
      (set-dispatch-macro-character #\# #\S original-s-reader)))

  (p1-end))

;;------------------------------------------------------------------------------
;; Liest einen Lisp Ausdruck aus 'lisp-stream'.
;;------------------------------------------------------------------------------
(defun clicc-read (lisp-stream)
  (read lisp-stream nil '|eof|))

;;------------------------------------------------------------------------------
;; Wendet Pass 1 auf die Daten an, die CLICC-READ liefert.
;;------------------------------------------------------------------------------
(defun pass_1-of-data (&optional lisp-stream)
  (let (form
        (old-package *PACKAGE*))

    (setq *FUN-NAME* "top-level-forms")
    
    (loop
     (setq form (clicc-read lisp-stream))
     (if (eq '|eof| form)
         (return)
         (p1-top-level-form form)))
    
    (unless (or (eq *PACKAGE* old-package) *MODULE-COMPILER*)
      (save-toplevel-form
       (p1-form
        `(L::SETQ L::*PACKAGE* (L::FIND-PACKAGE ,(package-name old-package)))))
      (setq *PACKAGE* old-package))))

;;------------------------------------------------------------------------------
;; Wendet Pass 1 auf die Daten aus der spezifizierten Datei an.
;;------------------------------------------------------------------------------
(defun pass_1-of-file (filename &optional ext)
  (let ((*CLICC-FILENAME* (clc-probe-file filename ext)))
    (if (null *CLICC-FILENAME*)

        (clc-error "File \"~A~@[.~A~]\" does not exist.~%" filename ext)

        (with-open-file (lisp-stream *CLICC-FILENAME* :direction :input)
          (clicc-message "File ~S" *CLICC-FILENAME*)      
          (pass_1-of-data lisp-stream)))))

;;------------------------------------------------------------------------------
;; Beendet Pass 1 des Compilers
;;------------------------------------------------------------------------------
(defun p1-end ()
  (unless (slot-boundp *module* 'package)
    (setf (?package *module*) (find-package "USER"))
    (add-q (find-package "USER") (?package-list *module*)))
  (when *lisp-module*
    (addnew-q (find-package "RT") (?package-list *module*))
    (addnew-q (find-package "FFI") (?package-list *module*)))
  (finalize-fftypes)
  
  (finalize-classes)
  (finalize-generic-funs)
  (symbolize-class-names)
  (check-undef-funs)
  (dequeue-module)
  (generate-init-fun)
  (collect-packages)
  (export-classes)
  (export-funs)
  (finalize-call-in-funs)

  ;; Symbole aus Package RT unexporten, damit write-syntax-export
  ;; rt:: schreibt.
  ;;---------------
  (when *lisp-module*
    (mapc #'(lambda (sym)
              (when (and (?exported sym)
                         (eq *runtime-package* (symbol-package (?symbol sym))))
                (unexport (?symbol sym) (symbol-package (?symbol sym)))))
          (?sym-list *module*)))
  (when (and *MODULE-COMPILER* (not *inline-module*)) (write-syntax-export)))

;;------------------------------------------------------------------------------
;; Die Top-Level Forms zu der Initialisierungsfunktion des Moduls
;; zusammenfassen
;;------------------------------------------------------------------------------
(defun generate-init-fun ()
  (labels
      ((init-lisp ()
         (list (make-app 
                :form (make-imported-fun
                       :symbol (make-symbol "INIT-LISP")
                       :adr "Ilisp"
                       :par-spec 0
                       :write-list -1)   ; has side effect
                :arg-list ())))
       
       (init-sym/packg ()
         (p1-form
          (if (null (?sym-list *module*))

              ;; Nur Packages generieren, keine Symbole vorhanden
              ;;-------------------------------------------------
              `(L::PROGN
                ,@ (mapcar #'(lambda (p)
                               `(rt::ensure-package ,(package-name p)))
                    (?package-list *module*)))

              ;; Packages generieren und in die Package-Cell der Symbole
              ;; eintragen
              ;;----------
              `(rt::setup-symbols-iterator
                (L::QUOTE ,(?symbol (first (?sym-list *module*))))
                (L::vector
                 ,@ (mapcar #'(lambda (p)
                                `(rt::ensure-package ,(package-name p)))
                     (?package-list *module*)))))))
       
       (calc-setup-sym/packg ()
         (if (or *inline-module*

                 ;; kleines Laufzeitsystem ohne Package-Operationen ?
                 ;;--------------------------------------------------
                 (null (get-global-fun 'L::find-package)))
             ()
             (let ((set-*package*
                    (p1-form
                     `(L::setq L::*package*
                       (L::find-package
                        ,(package-name (second (?package-list *module*))))))))

               (if (member (find-package "USER") (?package-list *module*))
                   (list (init-sym/packg)
                         (p1-form `(L:use-package "LISP" "USER"))
                         set-*package*)
                   (list (init-sym/packg)
                         set-*package*))))))
    
    (setf (?toplevel-forms *module*)
          (make-global-fun
           :symbol 'toplevel-forms
           :params (make-params :var-list nil
                                :opt-list nil
                                :rest nil
                                :key-list nil
                                :allow-other-keys
                                nil
                                :all-vars nil)
           :par-spec 0
           :mv-spec 1
           :body (let ((forms (if *lisp-module*
                                  (append
                                   (get-toplevel-form-list)
                                   (calc-setup-sym/packg))
                                  (append
                                   (unless *module-compiler* (init-lisp))
                                   (calc-setup-sym/packg)
                                   (get-toplevel-form-list)))))
                   (if forms
                       (make-progn-form :form-list forms)
                       empty-list))))))

;;------------------------------------------------------------------------------
;; Ablegen aller Package-Informationen
;;------------------------------------------------------------------------------
(defun collect-packages ()
  
  ;; Im Zwischencode treten nur noch Package-Names auf.
  ;;---------------------------------------------------
  (setf (?package-list *module*)
        (mapcar #'package-name (?package-list *module*))))

;;------------------------------------------------------------------------------
;; Umwandeln der Slots von *module* von einer Queue in eine Liste
;;------------------------------------------------------------------------------
(defun dequeue-module ()
  (setf (?package-list *module*) (queue2list (?package-list *module*)))
  (setf (?fun-list *module*) (queue2list (?fun-list *module*)))
  (setf (?class-def-list *module*) (queue2list (?class-def-list *module*)))
  (setf (?named-const-list *module*) (queue2list (?named-const-list *module*)))
  (setf (?var-list *module*) (queue2list (?var-list *module*)))
  (setf (?sym-list *module*) (queue2list (?sym-list *module*)))
  (setf (?loaded-modules *module*) (queue2list (?loaded-modules *module*)))
  (setf (?dyn-var-list *module*) 
        (mapcar #'cdr (?dyn-vars *GLOBAL-ENVIRONMENT*))))

;;------------------------------------------------------------------------------
;; Setzen des exported Flags von Funktionen
;;------------------------------------------------------------------------------
(defun export-funs ()
  (dolist (fun (?fun-list *module*))
    (let ((symbol (?symbol fun)))
      (when (consp symbol) (setq symbol (second symbol)))
      (when (symbol-package symbol)
        (multiple-value-bind (s status)
            (find-symbol (symbol-name symbol) (symbol-package symbol))
          (declare (ignore s))
          (when (eq :external status)
            (setf (?exported fun) t)))))))

;;------------------------------------------------------------------------------
;; Pruefen auf referenzierte aber nicht definierte Funktionen.
;; Fehlende Funktionen werden automatisch um eine Definition ergaenzt,
;; die einen Laufzeitfehler verursacht.
;;------------------------------------------------------------------------------
(defun check-undef-funs () 
  (dolist (nc (queue2list (?named-const-list *module*)))
    (when (eq (?value nc) :forward)
      (clicc-warning "Function ~s is referenced but not defined." (?symbol nc))
      (let ((*SDF* T))
        (p1-top-level-form
         `(L::defun ,(?symbol nc) (L::&rest args)
           (L::error
            "Function ~s is not defined. It has been called with arguments ~%~s"
            ,(format nil "~a" (?symbol nc))
            args)))))))

;;------------------------------------------------------------------------------
(provide "p1main")
