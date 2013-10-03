;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung
;;;            - Initialisierung globaler Variablen
;;;            - Fehlerbehandlung
;;;            - Start der Codegenerierung
;;;
;;; $Revision: 1.77 $
;;; $Log: cgmain.lisp,v $
;;; Revision 1.77  1994/06/23  10:17:44  hk
;;; Im Fall von *split-files*: sch"onere Dateinamen f"ur Konstanten,
;;; Symbole und Hauptprogramm.
;;; CodeGen gibt Meldungen "uber die generierten Dateien aus.
;;;
;;; Revision 1.76  1994/05/24  11:07:16  sma
;;; Automatische Definition der Obrep-Nummer eingefügt.
;;;
;;; Revision 1.75  1994/05/22  15:04:32  sma
;;; Jedes C-Files erhält jetzt eine Kennung aus der Compiler-Version und
;;; Datenrepräsenataion hervorgehen als Kommentar in der 1. Zeile.
;;;
;;; Revision 1.74  1994/04/22  14:09:07  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Erzeugte Include-Datei heisst jetzt <datei-name>-ffi.h
;;;
;;; Revision 1.73  1994/04/18  12:03:40  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Include-Dateien nur einbinden, wenn ueberhaupt eine erzeugt wurde.
;;;
;;; Revision 1.72  1994/02/16  16:46:37  hk
;;; Initialisierung von *cont-counter* und *tagbody-counter* geschieht
;;; jetzt in cgfuns.
;;;
;;; Revision 1.71  1994/02/08  13:57:54  sma
;;; Statistik für Rest-Paramter-Optimierung.
;;;
;;; Revision 1.70  1994/01/10  08:33:20  hk
;;; write-C-file: with-open-file darf nicht mit return-from verlassen
;;; werden, weil dann die Datei wieder gelöscht werden würde.
;;;
;;; Revision 1.69  1993/12/09  09:58:55  ft
;;; Slot-Initforms die strukturierte Literale sind werden jezt durch ihren
;;; Wert in den Laufzeit-Slot-Informationen ersetzt.
;;;
;;; Revision 1.68  1993/11/22  11:54:06  pm
;;; Eigenen Fehler behoben in codegen.
;;;
;;; Revision 1.67  1993/11/22  09:24:02  hk
;;; Neuer C-Code ONLY_ONCE in Initialisierungsfunktionen, der bewirkt,
;;; da_ diese Funktionen hvchstens 1x ausgef|hrt werden.
;;;
;;; Revision 1.66  1993/11/05  14:20:50  hk
;;; require an den Dateianfang verschoben, um Probleme mit akcl zu vermeiden.
;;;
;;; Revision 1.65  1993/09/13  13:43:06  jh
;;; get-rt-slot-info global definiert, da sie auch von der weight-Funktion
;;; verwendet wird.
;;;
;;; Revision 1.64  1993/09/10  15:12:48  hk
;;; Fehler-Methode cg-form (t) entfernt.
;;;
;;; Revision 1.63  1993/09/10  14:42:49  hk
;;; *C-bool* mit "0" initialisiert. Dieser Wert wird verwendet, wenn ein
;;; Prädikat abbricht und somit keinen Wert als Resultat generiert.
;;;
;;; Revision 1.62  1993/07/26  16:48:52  pm
;;; weitere #include's für das FFI eintragen lassen.
;;;
;;; Revision 1.61  1993/07/23  10:46:13  hk
;;; Fehler in letzter Aenderung behoben.
;;;
;;; Revision 1.60  1993/07/23  08:39:21  hk
;;; In codegen: Extern Deklaration fuer named-const-base.
;;;
;;; Revision 1.59  1993/07/20  15:52:12  hk
;;; calc-init-fun-name von p1main nach hier. symbol-base, named-const-base
;;; und init-fun-name eines Moduls werden in godegen gesetzt, Aufruf von
;;; cg-gen-named-constants.
;;;
;;; Revision 1.58  1993/07/20  13:22:23  uho
;;; Iterationen ueber 'loaded-modules' auf 'imported-module'-
;;; Zwischensprachkonstrukte umgestellt.
;;;
;;; Revision 1.57  1993/07/19  15:11:38  uho
;;; Der Slot 'loaded-modules' enthaelt jetzt eine Liste dreielementiger
;;; Listen der Form (Modulname, Initfunctionname, Symbolbase).  Beim
;;; Iterieren ueber 'loaded-modules' Aufrufe von 'calc-symbol-base' durch
;;; das Lesen der entsprechenden Komponente aus der 'loaded-modules'-Elemente
;;; ersetzt.
;;; Die Initialisierung des Slots 'symbol-base' des LZS-Moduls
;;; entfernt; sie wird jetzt beim Anlegen des Moduls vorgenommen.
;;;
;;; Revision 1.56  1993/07/07  15:33:19  uho
;;; Initialisierung von *C-NAME-PREFIX* aus der Codegenerierung nach
;;; p0-init verlegt.
;;;
;;; Revision 1.55  1993/06/22  08:10:08  uho
;;; Expliziten BLOCK um write-C-file gelegt (CLISP)
;;;
;;; Revision 1.54  1993/06/19  21:49:25  hk
;;; Zaehler fuer Dateinamen bei Split-Files wurde versehentlich nicht
;;; erhoeht.
;;;
;;; Revision 1.53  1993/06/17  12:48:13  ft
;;; Codeerzeugung für Klassen ohne Slots korrigiert.
;;;
;;; Revision 1.52  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.51  1993/06/04  14:09:22  hk
;;; Funktion codegen so geaendert, dass die Zeilenzahl in der generierten
;;; C Datei beschraenkt werden kann.
;;;
;;; Revision 1.50  1993/05/13  14:12:38  hk
;;; cg-funs-in-file geloescht, da unbenutzt.
;;;
;;; Revision 1.49  1993/05/08  18:46:05  hk
;;; cg-form fuer sym, class-def, simple-literal nach cgconst.
;;;
;;; Revision 1.48  1993/05/06  10:58:17  hk
;;; Splitting von Files wieder ermoeglicht.
;;;
;;; Revision 1.47  1993/04/22  11:17:56  hk
;;; Bearbeitung von *clicc-module* und *USER-INCLUDE*gestrichen.
;;; Bearbeitung von *module-compiler* und *inline-module* eingebaut.
;;; Symbole werden relativ zum Symbol-Array des Moduls indiziert.
;;;
;;; Revision 1.46  1993/04/15  13:03:49  ft
;;; Die Laufzeitinformationen eines Slots beinhalten jetzt auf seinem Namen,
;;; dies wird u.a. von SLOT-Value benoetigt.
;;;
;;; Revision 1.45  1993/04/07  16:41:57  hk
;;; cg-error sagt nun in welcher Funktion der Fehler stattfand.
;;;
;;; Revision 1.44  1993/04/06  14:55:58  ft
;;; Es wird nur noch Code fuer Klassen erzeugt, falls auch welche
;;; definiert wurden.
;;;
;;; Revision 1.43  1993/03/12  09:40:25  ft
;;; Codeerzeugung fuer Klassen.
;;;
;;; Revision 1.42  1993/02/16  15:49:12  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.41  1992/11/26  16:00:45  hk
;;; cg-init nach cginline.
;;;
;;; Revision 1.40  1992/11/25  17:51:27  hk
;;; Inline Compilation von %car, %cdr, %rplaca, %rplacd, %cons und
;;; einige Umbenennungen: check-integer-low -> fixnum-low-p ...
;;;
;;; Revision 1.39  1992/11/25  13:57:54  pm
;;; cgforeign eingehaengt
;;;
;;; Revision 1.38  1992/11/23  14:33:10  ft
;;; Zwei (require ...) verschoben.
;;;
;;; Revision 1.37  1992/11/05  08:34:17  kl
;;; internal-error eingebaut.
;;;
;;; Revision 1.36  1992/10/07  10:47:47  hk
;;; Fehlermeldung verschoenert.
;;;
;;; Revision 1.35  1992/10/06  09:04:41  hk
;;; C-badcode wurde aufgerufen, nachden Stream geschlossen ist.
;;;
;;; Revision 1.34  1992/10/05  16:01:14  uho
;;; Erzeugen der Groessenkonstanten fuer den Stack und die Heapbereiche
;;; eingefuehrt.
;;;
;;; Revision 1.33  1992/10/02  14:07:22  hk
;;; Fehlerbehandlung jetzt lokal
;;;
;;; Revision 1.32  1992/10/01  11:44:39  hk
;;; In error0: terpri.
;;;
;;; Revision 1.31  1992/09/29  21:11:10  hk
;;; Message und Fehlerzaehlen nach clcmain.
;;;
;;; Revision 1.30  1992/09/29  11:32:51  hk
;;; (require cgthrow) entfernt, da leer.
;;;
;;; Revision 1.29  1992/09/28  17:23:16  hk
;;; (CC-bool form) -> (CC-bool (not (null-form-p form))),
;;; (CC-bool form) -> C-TRUE.
;;;
;;; Revision 1.28  1992/09/25  17:23:25  kl
;;; Auf die neue Repraesentation der einfachen Literale umgestellt und
;;; Fehlermeldungen verbessert.
;;;
;;; Revision 1.27  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.26  1992/09/02  09:17:02  hk
;;; CC-bool in (cg-form sym) verwendet.
;;;
;;; Revision 1.25  1992/09/02  08:50:01  hk
;;; CC-bool in (cg-form simple-literal) verwendet.
;;;
;;; Revision 1.24  1992/08/31  08:56:57  hk
;;; Schreibfehler in 1. Zeile.
;;;
;;; Revision 1.23  1992/08/26  09:49:11  kl
;;; Fehler behoben: fehlende Kommata werden jetzt geschrieben.
;;;
;;; Revision 1.22  1992/08/11  12:44:17  hk
;;; C-Ln --> C-Decl, fuer Deklarationen.
;;;
;;; Revision 1.21  1992/08/10  12:00:26  hk
;;; *mv-spec* und *mv-produced* gestrichen, da Analyse komplett in Pass3.
;;;
;;; Revision 1.20  1992/08/10  10:21:48  hk
;;; In (cg-form progn-form) wird nun mapc-progn-form-list benutzt.
;;;
;;; Revision 1.19  1992/08/07  11:54:17  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.18  1992/08/07  11:28:10  hk
;;; Schreibfehler.
;;;
;;; Revision 1.17  1992/08/07  11:03:45  hk
;;; Schoenere Fehlermeldung.
;;;
;;; Revision 1.16  1992/08/06  15:22:56  hk
;;; Include fuer generierte Header-Datei ohne Pfad.
;;;
;;; Revision 1.15  1992/08/06  11:55:38  hk
;;; Neue Funktion cg-error0.
;;;
;;; Revision 1.14  1992/07/28  12:29:06  hk
;;; simple-array-p --> rt::simple-array-p.
;;;
;;; Revision 1.13  1992/07/28  08:11:15  hk
;;; Aufruf von cg-gen-symbols geaendert.
;;;
;;; Revision 1.12  1992/07/22  18:18:22  hk
;;; *CLICC-NEW-MODULE* gestrichen, kommt nur in Pass1 vor.
;;;
;;; Revision 1.11  1992/07/22  17:15:35  hk
;;; get-global-operator --> get-global-fun.
;;;
;;; Revision 1.10  1992/07/09  16:40:48  hk
;;; Definierte Symbole im Hauptprog. werden relativ zu Header definierten
;;; Symbolen adressiert.
;;;
;;; Revision 1.9  1992/07/08  14:49:50  hk
;;; *c-name-prefix* aus p0init in cg-reset-vars eingefuegt.
;;;
;;; Revision 1.8  1992/07/07  09:54:22  hk
;;; Adressen fuer Symbole werden nun in cg-main vergeben. Ueberpruefung auf
;;; nicht erlaubte Symbole in Modulen erfolgt erst in cg-main.
;;;
;;; Revision 1.7  1992/06/11  11:19:27  hk
;;; cg-error -> error
;;;
;;; Revision 1.6  1992/06/11  10:01:36  hk
;;; Schreibfehler.
;;;
;;; Revision 1.5  1992/06/11  09:56:15  hk
;;; In codegen werden C-Namen auch fuer Fkt. in Modulen generiert.
;;;
;;; Revision 1.4  1992/06/05  11:19:59  hk
;;; Methode cg-form (sym) hinzugefuegt.
;;;
;;; Revision 1.3  1992/06/04  12:23:47  hk
;;; Zu Beginn von CodeGen werden die C-Namen der glob. Fkt. generiert.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;;               1992/01/29            hk
;;; Der Code fuer Global Closures wird lokal vor der zugehoerigen Funktion
;;; generiert.
;;;
;;;               1992/01/17            hk
;;; Deklarationen fuer globale Funktionen werden an den Anfang der Datei
;;; geschrieben
;;;-----------------------------------------------------------------------------

(in-package "CLICC")     

(require "cgdefs")
(require "cgcode")
(require "cgconst")
(require "cgif")
(require "cgblock")
(require "cgvalues")
(require "cginline")
(require "cgvars")
(require "cgfuns")
(require "cgforeign")

;;------------------------------------------------------------------------------
;; Initialisierung globaler Variablen fuer die Codeerzeugung
;;------------------------------------------------------------------------------
(defun cg-reset-vars ()
  (setq *copy-source* nil)
  (setq *C-indent* 0)
  (setq *not-opened* nil)
  (setq *block-open* nil)
  (setq *special-count* 0)
  (setq *do-not-restore* ())
  (setq *C-bool* "0")                   ; notwendig, für Prädikat ohne Resultat
  (setq *cl-level* 0)
  (setq *closure* nil)
  (setq *C-line-count* 0)
  (setq *rlo-statistics-rest-funs* 0)
  (setq *rlo-statistics-rest-opt* 0)
  (setq *rlo-statistics-rest-usage* 0))

;;------------------------------------------------------------------------------
;; Berechnet aus dem Modulnamen den Namen der Initialisierungsfunktion
;;------------------------------------------------------------------------------
(defun calc-init-fun-name (module-name)
  (calc-C-name "I" module-name))

;;------------------------------------------------------------------------------
;; Die Funktion zum Starten der Codeerzeugung Der uebersetzte Code wird in
;; einer Datei <*OUT-FILENAME*>.c abgelegt.  Fuer die Funktionsdeklarationen
;; wird eine Datei mit dem Namen <*OUT-FILENAME*>.h angelegt.
;;------------------------------------------------------------------------------
(defun codegen (&aux header-file-name current-file-name)
  (cg-reset-vars)

  (let ((name (?name *module*)))
    (setf (?symbol-base *module*) (calc-symbol-base name))
    (setf (?named-const-base *module*) (calc-named-const-base name))
    (setf (?init-fun-name *module*) (calc-init-fun-name name)))
  
  ;; Relativadressen der Symbole bestimmen
  ;;--------------------------------------
  (let ((adr 0))
    (dolist (sym (?sym-list *module*))
      (setf (?adr sym) adr)
      (incf adr)))

  ;; Relativadressen der Klassen bestimmen und Slot Deskriptoren modifizieren
  ;;-------------------------------------------------------------------------
  (let ((adr 0))
    (dolist (class (?class-def-list *module*))
      (setf (?adr class) adr)
      (incf adr)
      (setf (?slot-descr-list class)
            (if (?slot-descr-list class)
                (make-instance 'structured-literal
                               :value (mapcar #'get-rt-slot-info 
                                              (?slot-descr-list class))
                               :needs-fixnum-array nil
                               :needs-float-array nil)
                empty-list))))
  
  ;; Namen der entsprechenden C-Funktionen generieren.
  ;;--------------------------------------------------
  (dolist (fun (?fun-list *module*))
    (cg-set-C-name fun))
  (unless *inline-module*
    (setf (?adr (?toplevel-forms *module*)) (?init-fun-name *module*)))

  (labels
      ((c-file-message (name)
         (clicc-message "Writing file ~a" name))
       (cg-include ()
         (C-Ln "/* This file was generated by CLiCC " *CLICC-Version*
               " [obrep " *OBREP* "] */")
         (C-Ln "#define __OBREP " *OBREP*)
         (C-sysinclude "c_decl.h")
         (C-include "sys.h")
         (unless (equal *interface-file-queue* '(nil))
           (C-include
            (concatenate 'string (pathname-name *FILENAME*) "-ffi.h")))
         (unless *inline-module*
           (dolist (imported-module (?loaded-modules *module*))
             (C-include (calc-include-file-name (?name imported-module)))))
         (C-include (strip-path header-file-name)))
       (cg-mem-sizes ()
         (C-empty-Ln)
         (C-DefMemSizes)
         (C-empty-Ln)))


    (setq header-file-name (concatenate 'string *OUT-FILENAME* ".h"))
    (with-open-file (*C-file* header-file-name
                              :direction :output :if-exists :supersede)
      (c-file-message header-file-name)
      (C-empty-Ln)
      (unless *inline-module*
        (C-ExternArrayDecl "CL_INIT"
                           (CC-NameConc "K" (?named-const-base *module*)))
        (C-ExternArrayDecl "CL_INIT" (?symbol-base *module*)))

      ;; Deklarationen fuer die zu erzeugenden Funktionen generieren
      ;;------------------------------------------------------------
      (dolist (fun (?fun-list *module*))
        (C-fun-decl fun)
        (when (?closure fun)
          (C-ExternVarDecl "GLOBAL_FUNARG" (CC-NameConc "C" (?adr fun))))))


    (if *split-files*

        ;; Jede Lisp Funktion in eine eigene Datei.
        ;;-----------------------------------------
        (let ((base-name (strip-path *OUT-FILENAME*))
              (path (get-path *OUT-FILENAME*))
              (count 0))
          (when (?named-const-list *module*)
            (setq current-file-name
                  (calc-fun-filename (?named-const-base *module*)
                                     base-name path count))
            (with-open-file (*C-file* current-file-name
                                      :direction :output :if-exists :supersede)
              (c-file-message current-file-name)
              (cg-include)
              (cg-gen-named-constants)))
          (dolist (fun (?fun-list *module*))
            (incf count)
            (setq current-file-name
                  (calc-fun-filename (?adr fun) base-name path count))
            (with-open-file (*C-file* current-file-name
                                      :direction :output :if-exists :supersede)
              (c-file-message current-file-name)
              (cg-include)
              (cg-defun fun)))

          (unless *inline-module*
            (let ((fun (?toplevel-forms *module*)))
              (incf count)
              (setq current-file-name
                    (calc-fun-filename (?adr fun) base-name path count))
              (with-open-file (*C-file* current-file-name
                                        :direction :output
                                        :if-exists :supersede)
                (c-file-message current-file-name)
                (cg-include)
                (unless *module-compiler*
                  (cg-mem-sizes))
                (cg-defun fun :only-once *module-compiler*)
                (unless *module-compiler*
                  (cg-GC-function))
                (when (> *NERRORS* 0) (C-badcode))))
            
            (when (?class-def-list *module*)
              (clc-error "Split-option has not been implemented for classes."))

            (incf count)
            (setq current-file-name
                  (calc-fun-filename (?symbol-base *module*)
                                     base-name path count))
            (with-open-file (*C-file* current-file-name
                                      :direction :output :if-exists :supersede)
              (c-file-message current-file-name)
              (cg-include)
              (cg-gen-symbols))))

        ;; Funktionen in eine Datei bzw. in mehrere Dateien, deren Laenge
        ;; *C-max-line-count*  nicht ueberschreitet.
        ;;------------------------------------------
        (labels ((write-C-file (fun-list count)
                   (block write-C-file
                     (setq current-file-name
                           (calc-next-C-filename *OUT-FILENAME* count))
                     (with-open-file
                         (*C-file* current-file-name
                                   :direction :output :if-exists :supersede)
                       (c-file-message current-file-name)
                       (setq *C-line-count* 0)
                       (cg-include)
                       (when (and (not *module-compiler*) (zerop count))
                         (cg-mem-sizes))
                       (when (zerop count)
                         (cg-gen-named-constants))
                       (loop
                        (when (null fun-list)
                          (unless *inline-module*
                            (cg-defun (?toplevel-forms *module*)
                                      :only-once *module-compiler*)
                            (unless *module-compiler*
                              (cg-GC-function))
                            (cg-gen-symbols)
                            (unless (not (?class-def-list *module*))
                              (cg-gen-classes)))
                          (when (> *NERRORS* 0) (C-badcode))
                          (return :ready))
                        (cg-defun (pop fun-list))
                        (when (and *C-max-line-count*
                                   (> *C-line-count* *C-max-line-count*))
                          (return fun-list)))))))
          
          (let ((count 0)               ; Zaehler fuer Dateinamen
                (fun-list (?fun-list *module*)))
            (loop
             (setq fun-list (write-C-file fun-list count))
             (when (eq :ready fun-list) (return))
             (incf count))))))

  ;; Statistik für Rest-Optimierung
  ;;-------------------------------
  (when (and *optimize* (plusp *rlo-statistics-rest-funs*))
    (clicc-message "~A rest-list optimizations in ~A of ~A functions."
                   *rlo-statistics-rest-usage*
                   *rlo-statistics-rest-opt*
                   *rlo-statistics-rest-funs*)
    (clicc-message-line)))

;;------------------------------------------------------------------------------
(defun get-rt-slot-info (slot-desc)
  (list (first (?initargs slot-desc)) 
        (if (and (structured-literal-p (?initform slot-desc))
                 (not (equalp (?value (?initform slot-desc))
                              "SECRET-UNBOUND-SLOT-VALUE")))
            (?value (?initform slot-desc))
            (?initform slot-desc))
        (?symbol slot-desc)))

;;------------------------------------------------------------------------------
;; Code fuer die Funktion, die bei einer Garbage Collection die Symbole des
;; Moduls traversiert.
;;------------------------------------------------------------------------------
(defun cg-GC-function ()
  (C-Ln)
  (C-Ln "void gc_main ()")
  (C-Blockstart)
  (dolist (imported-module (?loaded-modules *module*))
    (C-Call "gc_symbols" (?symbol-base imported-module)))
  (C-Call "gc_symbols" (?symbol-base *module*))
  (C-Blockend))

;;------------------------------------------------------------------------------
;; Berechnet aus dem Modulnamen den Namen der Include Datei
;;------------------------------------------------------------------------------
(defun calc-include-file-name (module-name)
  (concatenate 'string module-name ".h"))

;;------------------------------------------------------------------------------
;; Schreibt Fehlermeldung, erhoeht *NERRORS*
;;------------------------------------------------------------------------------
(defun cg-error (string &rest args)
  (incf *NERRORS*)
  (format *error-output* "Error in ~a: " *fun-name*)
  (apply #'format *error-output* string args)
  (terpri *error-output*)
  (terpri *error-output*))

;;------------------------------------------------------------------------------
(defmethod cg-form ((form progn-form))
  (mapc-progn-form-list (?form-list form)
                        #'(lambda (form)
                            (let ((*result-spec* nil))
                              (cg-form form)))
                        #'cg-form))

;;------------------------------------------------------------------------------
(provide "cgmain")



