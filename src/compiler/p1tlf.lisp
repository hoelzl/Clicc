;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Top-Level-Forms
;;;
;;; $Revision: 1.105 $
;;; $Log: p1tlf.lisp,v $
;;; Revision 1.105  1994/04/18  12:18:03  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - UNEXPANDED-FOREIGN-FUN als Operator im Global-Environment
;;;
;;; Revision 1.104  1994/02/18  13:55:25  hk
;;; (funcall f . args) wird direkt in zu einem app-Knoten mit form f und
;;; arg-list args.
;;;
;;; Revision 1.103  1993/12/16  12:49:07  hk
;;; In p1-export eine Queue verwendet, um das Umrehen des 1. Arguments zu
;;; verhindern.
;;;
;;; Revision 1.102  1993/12/09  14:34:36  hk
;;; Meldung "Importing definitions from module ~A" erfolgt in p1-load vor
;;; dem Lesen des .def Files und nicht erst beim Lesen des .syntax Files
;;;
;;; Revision 1.101  1993/12/07  10:20:31  uho
;;; Der Quellcode von Funktionsdefinitionen wird nur dann gemerkt, wenn
;;; der Modulkompiler aktiv ist.
;;;
;;; Revision 1.100  1993/11/23  16:05:13  hk
;;; In p1-defun: Test auf (setf f) durch consp statt durch listp.
;;;
;;; Revision 1.99  1993/11/15  13:35:45  pm
;;; selbst fabrizierten Fehler behoben in p1-call.
;;;
;;; Revision 1.98  1993/11/03  11:47:02  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.97  1993/10/06  11:15:23  hk
;;; Bei Applikationen lokaler Funktionen wird nun geprüft, ob die
;;; aktuellen zu den formalen Parametern passen.
;;;
;;; Revision 1.96  1993/08/26  12:18:24  hk
;;; Initialisierungsfunktion eines importierten Moduls bekommt unbekannten
;;; write-effect, damit ihr Aufruf nicht wegoptimiert wird.
;;;
;;; Revision 1.95  1993/08/20  11:50:18  ft
;;; p1-defvar beachtet jetzt das gesetzte *SDF*-flag, d.h. die Meldung
;;; in so einem Fall unterdrueckt.
;;;
;;; Revision 1.94  1993/08/19  16:50:20  hk
;;; Verwendung von lex-var-name-p, in p1-make-new-symbol schon in sym-list
;;; des Moduls schreiben.
;;;
;;; Revision 1.93  1993/07/30  08:54:52  hk
;;; Schreibfehler in p1-export behoben.
;;;
;;; Revision 1.92  1993/07/28  11:29:41  hk
;;; Auch nil wird zur Laufzeit exportiert, wenn es in einer
;;; Exportanweisung vorkam.
;;;
;;; Revision 1.91  1993/07/26  09:45:07  hk
;;; p1-export: wirklich nur importierte Symbole zur Laufzeit exportieren
;;;
;;; Revision 1.90  1993/07/22  15:45:12  wg
;;; export exportiert alle importierten Symbole einer Symbolliste auf
;;; einmal.
;;;
;;; Revision 1.89  1993/07/22  14:57:46  hk
;;; p1-export exportiert imported syms zur Laufzeit
;;;
;;; Revision 1.88  1993/07/20  13:24:56  uho
;;; Generierung des Aufrufs der Initialisierungsfunktion auf
;;; 'imported-module'-Zwischensprachkonstrukt umgestellt.
;;;
;;; Revision 1.87  1993/07/19  14:46:05  uho
;;; In 'p1-load' bei Import die Generierung a) des Aufrufs der
;;; Initialisierungsfunktion des importierten Moduls und b) des
;;; Zuruecksetzen von *PACKAGE* eingefuegt. Das .def-File muss vor dem .syntax-File
;;; geladen werden, damit die Symbole des importierten Moduls beim Laden
;;; des .syntax-Files schon im globalen Environment definiert sind.
;;;
;;; Revision 1.86  1993/07/13  11:25:26  uho
;;; Behandlung des Slots 'package' in 'p1-in-package' geaendert, um niemals
;;; auf den uninitialisierten Slot zuzugreifen.
;;;
;;; Revision 1.85  1993/07/02  13:44:38  ft
;;; Name des impliziten Blocks als optionaler Parameter von p1-defun
;;; eingefuegt.
;;;
;;; Revision 1.84  1993/07/02  11:41:31  ft
;;; Tippfehler beseitigt.
;;;
;;; Revision 1.83  1993/07/02  11:31:30  ft
;;; Anpassung an die geaenderte Definition von p1-named-lambda.
;;;
;;; Revision 1.82  1993/07/02  11:24:25  uho
;;; Wirkung von *MODULE-COMPILER* auf p1-load geandert.
;;;
;;; Revision 1.81  1993/06/28  17:32:40  hk
;;; Wenn APP Zwischensprachkonstrukte generiert werden, und der Slot form
;;; eine fun enthaelt, dann wird gewaehrleistet, dass aktuelle und formale
;;; Parameterliste zusammenpassen, (das geschah bisher in Pass3).
;;;
;;; Revision 1.80  1993/06/22  11:14:13  hk
;;; p1-import definiert.
;;;
;;; Revision 1.79  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.78  1993/05/17  08:10:48  pm
;;; Fehler beseitigt im Aufruf von p1-foreign-fun-call
;;;
;;; Revision 1.77  1993/05/08  17:50:34  hk
;;; (quote <symbol>) in p1-proclaim gestrichen, clicc-lisp: -> L:,
;;; p1-top-level-form umsortiert bzgl. extend-syntax-export.
;;;
;;; Revision 1.76  1993/05/06  16:42:40  hk
;;; Im *lisp-module* darf aus beliebigen Packages, auch ffi, exportiert
;;; werden.
;;;
;;; Revision 1.75  1993/05/04  11:56:10  uho
;;; DEFCONSTANTs werden nicht mehr automatisch exportiert (p1-make-constant)
;;;
;;; Revision 1.74  1993/04/30  10:16:20  hk
;;; Keywords werden automatisch aus Modulen exportiert.
;;;
;;; Revision 1.73  1993/04/22  12:56:31  pm
;;; aufruf von p1-foreign-fun-call verbessert
;;;
;;; Revision 1.72  1993/04/22  11:26:33  hk
;;; Sonderbehandlung bei *HEADER-FILE*, *CLICC-LISP-PROGRAM*, *FORCE-COMPILE*
;;; gestrichen. Voraeufige Sonderbehandlung des Symbols T.
;;; Funktionen in Modulen werden nicht mehr automatisch exportiert.
;;; Sonderbehandlung bei *CLICC-MODULE* gestrichen. Sonderbehandlung fuer
;;; *inline-module* um die eigenartigen Definitionen in inline.lisp
;;; verarbeien zu koennen. p1-write-to-header gestrichen.
;;;
;;; Revision 1.71  1993/04/21  11:57:42  kl
;;; Unnoetige Variable fct in p1-call entfernt.
;;;
;;; Revision 1.70  1993/04/21  08:59:52  ft
;;; Aufruf von p1-def-built-in erfolgt jetzt nicht mehr mit apply.
;;;
;;; Revision 1.69  1993/04/20  14:38:37  ft
;;; Eintragen der neuen Top-Level-Form 'def-built-in'.
;;;
;;; Revision 1.68  1993/04/16  08:21:30  hk
;;; Aufrufe von Lisp Funktionen in Makroexpansionsfunktionen werden
;;; immer durch Lisp-Funktionen dargestellt, sonst Warning.
;;; In p1-export Sonderfall fuer *lisp-module*: auch aus RT darf exportiert werden.
;;;
;;; Revision 1.67  1993/04/14  10:23:45  hk
;;; p1-in-package: Test auf (slot-boundp *module* 'package).
;;;
;;; Revision 1.66  1993/04/14  07:57:21  kl
;;; Weitere Klammerungsfehler behoben.
;;;
;;; Revision 1.65  1993/04/13  10:16:52  uho
;;; Klammerfehler beseitigt, vielen Dank an kl
;;;
;;; Revision 1.64  1993/04/08  14:59:55  uho
;;; Aenderungen fuer syntaktischen Export ge-merged
;;;
;;; Revision 1.63  1993/04/08  09:12:33  pm
;;; p1-call-foreign-fun in p1-foreign-fun-call umbenannt
;;;
;;; Revision 1.62  1993/04/08  08:12:47  hk
;;; Aufrufe von Lisp Systemfunktionen in Makroexpansionsfunktionen
;;; werden gesondert behandelt.
;;;
;;; Revision 1.61  1993/04/06  14:06:58  ft
;;; p1-constant braucht sich jetzt nicht mehr um Klassen zu kuemmern.
;;;
;;; Revision 1.60  1993/04/03  10:00:40  hk
;;; p1-in-package an Modulkompilation angepasst und aufgeraeumt,
;;; Bearbeitung von Compiler-Macros eingebaut, dazu p1-call und p1-defun
;;; angepasst.
;;;
;;; Revision 1.59  1993/03/23  07:37:13  ft
;;; Multiple Werte fuer die pass1-fn's eingefuehrt um die Probleme mit
;;; make-instance zu beseitigen.
;;;
;;; Revision 1.58  1993/03/22  17:33:39  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.57  1993/03/12  09:50:28  ft
;;; p1-constant an Klassen angepasst.
;;;
;;; Revision 1.56  1993/02/25  13:15:41  jh
;;; Beim Uebersetzen von CLICC-Modulen wird der exported-slot der definierten Funktionen gesetzt.
;;;
;;; Revision 1.55  1993/02/23  08:27:32  ft
;;; p1-defun liefert jetzt erzeugte Funktion als Resultat, um
;;;  bei der Finalisierung generischer Funktionen anwendbar zu sein.
;;;
;;; Revision 1.54  1993/02/19  13:59:18  hk
;;; Fehler beim Test auf Lisp Package beseitigt.
;;;
;;; Revision 1.53  1993/02/17  11:34:07  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.52  1993/02/16  16:06:48  hk
;;; Revision Keyword eingefuegt, Test auf Lisp-Package in p1-make-new-symbol
;;; erweitert, Symbole des zu uebersetzenden Programms mit clicc-lisp::
;;; gekennzeichnet, *CLICC-PACKAGE* durch *PACKAGE* ersetzt.
;;;
;;; Revision 1.51  1993/01/29  06:54:40  ft
;;; *SETF-FUN* in SETF-FUN geaendert.
;;;
;;; Revision 1.50  1993/01/22  15:04:10  ft
;;; Aenderungen fuer die Verarbeitung von erweiterten Funktionsnamen.
;;;
;;; Revision 1.49  1993/01/07  13:42:51  hk
;;; In p1-top-level-form Aufruf von clc-error, wenn ein Ausdruck ignoriert wird.
;;;
;;; Revision 1.48  1992/12/17  07:21:50  kl
;;; Einrueckung geaendert.
;;;
;;; Revision 1.47  1992/12/17  07:19:31  kl
;;; (when (not A) B) durch (unless A B) ersetzt.
;;;
;;; Revision 1.46  1992/12/02  10:56:07  hk
;;; Aufrufe von clicc-message neu formatiert.
;;;
;;; Revision 1.45  1992/12/01  16:23:48  ft
;;; (nil) statt otherwise im case von p1-call eingesetzt.
;;;
;;; Revision 1.44  1992/11/27  09:22:04  ft
;;; Erweiterung von p1-call um die Bahandlung gen. Funktionen.
;;;
;;; Revision 1.43  1992/11/26  12:22:29  hk
;;; ?write von dynamischen Variablen wird erhoeht, wenn diese beschrieben oder
;;; gebunden werden, damit bestimmt werden kann, ob spaeter ein illegales
;;; defconstant erfolgt.
;;;
;;; Revision 1.42  1992/11/23  15:01:24  hk
;;; let um find-secret-package-name in defvar umbenannt.
;;;
;;; Revision 1.41  1992/11/20  13:52:10  ft
;;; Aufruf von p1-eval in p1-make-constant enabled.
;;;
;;; Revision 1.40  1992/11/13  11:46:56  ft
;;; Aufruf von p1-eval in p1-make-constant disabled.
;;;
;;; Revision 1.39  1992/11/05  10:50:20  pm
;;; Tippfehler korrigiert
;;;
;;; Revision 1.38  1992/11/04  12:44:14  pm
;;; special-form load-foreign eingebaut.
;;;
;;; Revision 1.37  1992/11/02  14:50:06  pm
;;; p1-call um Aufrufe von Foreign-Functions erweitert
;;;
;;; Revision 1.36  1992/10/19  14:31:53  ft
;;; p1-defun verschoenert und Aufruf von redef-op-error eingefuegt.
;;;
;;; Revision 1.35  1992/09/25  16:59:32  kl
;;; Einfache Literale (die leere Liste, Zeichen und Zahlen) werden jetzt
;;; durch die entsprechenden Zwischensprachkonstrukte repraesentiert.
;;; Innerhalb von strukturierten Literalen werden die leere Liste, Zeichen
;;; und Zahlen weiterhin durch sich selbst dargestellt.
;;; p1-make-symbol warnt nun, wenn NIL als Symbol verwendet wird.
;;;
;;; Revision 1.34  1992/09/25  16:48:31  kl
;;; In p1-in-package und p1-use-package werden die Mengendifferenzen
;;; jetzt richtig berechnet.
;;;
;;; Revision 1.33  1992/09/02  12:08:06  hk
;;; Fehlermeldung eingefuegt, wenn Funktionsobjekt als Konstante verwendet wird.
;;;
;;; Revision 1.32  1992/08/31  08:39:15  ft
;;; p1-defclass in p1-top-level-form eingebunden
;;;
;;; Revision 1.31  1992/08/31  08:24:27  hk
;;; Indentation hinter p1-use-package korrigiert.
;;;
;;; Revision 1.30  1992/08/28  10:44:38  uho
;;; p1-use-package definiert, um top-level USE-PACKAGES zu behandeln.
;;;
;;; Revision 1.29  1992/08/16  14:45:30  kl
;;; Eingefuegte clicc-message wieder entfernt.
;;;
;;; Revision 1.28  1992/08/15  13:08:16  kl
;;; In p1-top-level-form (first form) durch name ersetzt.
;;;
;;; Revision 1.27  1992/08/11  16:22:41  hk
;;; Ignore Declaration in p1-make-new-symbol.
;;;
;;; Revision 1.26  1992/08/07  11:35:54  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.25  1992/08/07  09:57:58  hk
;;; Aufrufe von lokal definierten Makros werden erkannt.
;;;
;;; Revision 1.24  1992/08/05  13:43:13  hk
;;; Syntaktische Aenderungen.
;;;
;;; Revision 1.23  1992/08/04  18:22:28  hk
;;; Symbole des Lisp-Package werden ggf. automatisch exportiert,
;;; Symbole des Keyword-Package bekommen sich selbst als Wert.
;;;
;;; Revision 1.22  1992/07/29  16:53:30  hk
;;; p1-mv-lambda und p1-lambda-app nach p1lambda.lisp .
;;;
;;; Revision 1.21  1992/07/29  14:12:14  hk
;;; Exported Symbole des LISP-Package werden automatisch exportiert.
;;;
;;; Revision 1.20  1992/07/29  09:58:40  hk
;;; check-use-of-params in p1-mv-lambda gestrichen, Schreibfehler beseitigt.
;;;
;;; Revision 1.19  1992/07/27  16:47:22  hk
;;; In p1-in-package: get-symbol-bind --> p1-make-symbol, Umbenennungen,
;;; wenn Zwischensprachrepraesentationen von Symbolen erzeugt werden,
;;; die nur zum
;;; evaluieren waehrend der Ubersetzungszeit benoetigt werden, dann werden diese
;;; nicht in *global-environment* eingetragen.
;;;
;;; Revision 1.18  1992/07/22  13:08:14  hk
;;; In p1-constant array mit update-array bearbeitet.
;;;
;;; Revision 1.17  1992/07/22  11:52:21  hk
;;; In p1-export: :exported --> :external, Package bei find-symbol angegeben.
;;;
;;; Revision 1.16  1992/07/09  09:07:35  hk
;;; Fuer Symbole aus Header-Dateien werden imported-syms generiert, falls
;;; nicht gerade das Hauptprogramm uebersetzt wird.
;;;
;;; Revision 1.15  1992/07/08  12:59:19  hk
;;; p1-export: Nur im aktuellen Package exportieren.
;;;
;;; Revision 1.14  1992/07/08  12:48:51  hk
;;; Schreibfehler.
;;;
;;; Revision 1.13  1992/07/08  12:44:23  hk
;;; Das Symbol nil wird immer exportiert.
;;;
;;; Revision 1.12  1992/07/08  12:33:08  hk
;;; p1-make-new-symbol generiert fuer importierte Symbole imported-sym
;;; ausser bei der Uebersetzung des Hauptprogramms,
;;; in dem alle Symbole (scheinbar) definiert werden.
;;;
;;; Revision 1.11  1992/07/08  12:23:09  hk
;;; Toplevel Form export wird gesondert uebersetzt.
;;;
;;; Revision 1.10  1992/07/07  12:26:31  ft
;;; Erweiterung von p1-top-level-form um defgeneric & defmethod
;;;
;;; Revision 1.9  1992/07/07  09:52:27  hk
;;; Erst in CodeGen wird geprueft, ob in einem Modul neue Symbole definiert
;;; wurden. Neue im Modul definierte Symbole werden in sym-list eingetragen.
;;;
;;; Revision 1.8  1992/07/02  14:02:44  hk
;;; Named-const haben Wert :forward, solange sie nicht definiert sind.
;;;
;;; Revision 1.7  1992/06/11  09:47:05  hk
;;; definiert, aber angwandte Vorkommen weiterhin durch imported-fun.
;;;
;;; Revision 1.6  1992/06/11  09:43:21  hk
;;; Bei der Uebersetzung von Modulen werden definierte Fkt. durch global-fun
;;;
;;; Revision 1.5  1992/06/04  13:26:01  hk
;;; Schreiben von DECLAIM sollte nicht in Pass1 erfolgen.
;;;
;;; Revision 1.4  1992/06/04  12:43:13  hk
;;; ?const in ?value bzw. ?constant-value umbenannt.
;;;
;;; Revision 1.3  1992/06/04  09:47:54  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;;               1991/07/18            hk
;;; P1-IN-PACKAGE neu geschrieben, benutzt nicht mehr die Funktion IN-PACKAGE,
;;; um Kompatibilitaet mit X3J13 zu erreichen.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(defun p1-top-level-form (form)
  (catch 'CLICC-ERROR
    (let ((*CURRENT-FORM* form))        ; Aktuelle Form fuer Fehlermeldungen
      (if (atom form)
          (save-toplevel-form (p1-form form))
          (let* ((name (first form))
                 (args (rest form)) 
                 (operator-def (get-operator-def name)))
            (cond
              ((eq (car operator-def) :USER-MACRO)
               (p1-top-level-form
                (p1-expand-user-macro (cdr operator-def) form)))

              (t (case name
                   (L::defun       (p1-defun      args))
                   (L::progn       (mapc #'p1-top-level-form args))
                   (L::load        (p1-load       args))
                   (ffi:load-foreign (p1-load-foreign args))
                   (L::provide     (p1-provide    args))
                   (L::require     (p1-require    args))
                   (L::declaim     (p1-declaim    args))
                   (L::proclaim    (p1-proclaim   args))
                   (L::export      (p1-export     args))
                   (L::defgeneric  (p1-defgeneric args))
                   (L::defmethod   (p1-defmethod  args))
                   (L::defclass    (p1-defclass   args))
                   (L::defstruct   (p1-defstruct    args))
                   (t
                    (when *MODULE-COMPILER* (extend-syntax-export form))
                    (case name
                      (L::in-package   (p1-in-package   args))
                      (L::use-package  (p1-use-package  args))
                      (L::import       (p1-import       args))
                      (L::defmacro     (p1-defmacro     args))
                      (L::deftype      (p1-deftype      args))
                      (L::def-built-in (p1-def-built-in args))
                      (L::defconstant  (p1-defconstant  args))
                      (L::defparameter (p1-defparameter args))
                      (L::defvar       (p1-defvar       args))
                      (L::defsetf      (p1-defsetf      args))
                      (t (when *MODULE-COMPILER* (retract-syntax-export))
                         (save-toplevel-form (p1-form form)))))))))))))

;;------------------------------------------------------------------------------
;; Analysiert ein Atom oder einen Aufruf einer Special-Form, eines Makros
;; oder einer Funktion.
;;------------------------------------------------------------------------------
(defun p1-form (form)
  (let ((*CURRENT-FORM* form))
    (if (atom form)
      (p1-atom form)
      (p1-call form))))

;;------------------------------------------------------------------------------
;; Analysiert einen atomaren Ausdruck.
;;------------------------------------------------------------------------------
(defun p1-atom (object)
  (if (and (symbolp object) (not (null object)) (not (eq t object)))
    (p1-read-access-of-var object)
    (p1-constant object)))

;;------------------------------------------------------------------------------
;; Analysiert einen Aufruf
;;------------------------------------------------------------------------------
(defun p1-call (form)
  (let ((name (first form))
        (args (rest form)))
    (cond

      ;; Aufrufe von FUNCALL k"onnen in der
      ;; Zwischensprache direkt dargestellt werden
      ;;------------------------------------------
      ((eq 'L:FUNCALL name)
       (make-app :form (p1-form (pop args)) :arg-list (p1-args args)))
      
      ((symbolp name)
       (let* ((operator-def (get-operator-def name))
              (operator     (cdr operator-def))
              cm-operator)

         ;; Compiler-Macros vorher expandieren und mit p1-call fortfahren,
         ;; wenn keine Expansion stattfand.
         ;; Eintrag im global Environment:
         ;; (:COMPILER-MACRO . (expander . (:???-FUN . fun)))
         ;;--------------------------------------------------
         (when (eq :COMPILER-MACRO (car operator-def))
           (let ((new-form (funcall (car operator) form)))
             (unless (eq form new-form)
               (return-from p1-call (p1-form new-form)))
             (setq cm-operator operator)
             (setq operator-def (cdr cm-operator))
             (setq operator (cdr operator-def))))             
               
         (case (car operator-def)
           (:SPECIAL-FORM
            (funcall operator args))
           (:SYS-MACRO
            (p1-form (p1-expand-system-macro operator form)))
           ((:USER-MACRO :LOCAL-MACRO)
            (p1-form (p1-expand-user-macro operator form)))
           (:UNEXPANDED-FOREIGN-FUN
            (p1-form (p1-foreign-fun-call operator form)))
           ((:LOCAL-FUN :FOREIGN-FUN)
            (clc-check-nparams
             (?par-spec operator) (length args) (?symbol operator))
            (make-instance 'app :form operator :arg-list (p1-args args)))
           (T
            (make-instance
             'app
             :form
             (or (and *compiler-eval*
                      (eq name (find-symbol (symbol-name name) *lisp-package*))
                      (or (zw-symbol-fun name)
                          (progn
                            (clicc-warning
                             "Function ~s may not be used in macro function"
                             name)
                            nil)))
                                  
                 (ecase (car operator-def)
                   ((:GLOBAL-FUN :IMPORTED-FUN)
                    (clc-check-nparams
                     (?par-spec operator) (length args) (?symbol operator))
                    operator)
                   (:FORWARD operator)
                   (:GENERIC-FUN
                    (let ((fun (?fun operator)))
                      (clc-check-nparams (?par-spec fun)
                                         (length args)
                                         (?symbol fun))
                      fun))
                   ((nil) (bind-forward-ref-fun name cm-operator))))
             :arg-list (p1-args args))))))
      
      ;; Lambda-Applikation
      ;;-------------------
      ((and (consp name) (eq (first name) 'L::LAMBDA))
       (p1-lambda-app (rest name) args))
      ((and (consp name) (eq (first name) 'MV-LAMBDA))
       (p1-mv-lambda (rest name) args))
      (t (clicc-error NO_OPERATOR name)))))

;;------------------------------------------------------------------------------
;; defun name lambda-list {declaration || doc-string}* {form}*
;;------------------------------------------------------------------------------
(defun p1-defun (name_lambda-expr &optional fun-block-name)
  (multiple-value-bind (name lambda-list_body)
      (parse-named-lambda 'L::DEFUN name_lambda-expr)
    
    ;; Bei automatisch generierten Funktionen keine Meldung ausgeben.
    ;;---------------------------------------------------------------
    (unless *SDF* (clicc-message "Analyse DEFUN        ~A" name))
    
    (let* ((SETF-FUN (consp name))
           (operator-def (if SETF-FUN
                            (get-setf-fun-def name)
                            (get-operator-def name)))
           cm-operator
          fun)

      (when (eq :COMPILER-MACRO (car operator-def))
        (setq cm-operator (cdr operator-def))
        (setq operator-def (cdr cm-operator)))
      
      (case (car operator-def)
        ((nil)
         (setq fun (make-instance 'global-fun :symbol name))
         (if cm-operator
             (setf (cdr cm-operator) (cons :GLOBAL-FUN fun))
             (if SETF-FUN
                 (set-global-setf-fun-def name :GLOBAL-FUN fun)
                 (set-global-fun name fun))))
        (:FORWARD
         (setq fun (make-instance 'global-fun :symbol name))
         (setf (?value (cdr operator-def)) fun)
         (setf (car operator-def) :GLOBAL-FUN)
         (setf (cdr operator-def) fun))
        (:IMPORTED-FUN
         (if (and *inline-module* (?c-inline (cdr operator-def)))
             (setq fun (make-instance 'global-fun :symbol name :exported T))
             (clicc-error REDEF_FUN name)))
        (t (redef-op-error (car operator-def) name)))

      (unless fun-block-name
        (setf fun-block-name (if (consp name) (second name) name)))

      (setq fun (p1-named-lambda fun name fun-block-name
                                 lambda-list_body))
      (add-q fun (?fun-list *module*))
      (when *module-compiler* (setf (?source fun) lambda-list_body))

      ;; Liefert das generierte Zwischensprachkonstrukt als Wert
      ;;--------------------------------------------------------
      fun)))

;;------------------------------------------------------------------------------
;; Forwaerts Referenzen auf Funktionen werden durch eine Indirektion mittels
;; einer named-constant ausgedrueckt. Die Named-Constant bekommt als Wert
;; :forward und entspricht damit der der Spezifikation der Zwischensprache. Am
;; Ende von Pass1 werden alle Named-Constants ueberprueft, ob sie einen
;; gueltigen Wert haben.
;;------------------------------------------------------------------------------
(defun bind-forward-ref-fun (name comp-macro-operator)
  (let ((const (make-instance 'defined-named-const
                              :symbol name
                              :value :forward)))
    (if comp-macro-operator
        (setf (cdr comp-macro-operator) (cons :FORWARD const))
        (if (consp name)
            (set-forward-setf-fun name const)
            (set-forward-fun name const)))
    (add-q const (?named-const-list *module*))
    const))

;;------------------------------------------------------------------------------
;; Analysiert die Argumente eines Funktionsaufrufes.
;;------------------------------------------------------------------------------
(defun p1-args (args)
  (mapcar #'p1-form args))

;;------------------------------------------------------------------------------
;; Analysiert einen lesenden Variablenzugriff
;; Resultat : Darstellung der Variablen in der abstrakten Syntax
;;------------------------------------------------------------------------------
(defun p1-read-access-of-var (var)
  (if (keywordp var)
     (p1-form `(L::QUOTE ,var))
     (let ((var-bind (get-var-bind var)))
          (make-instance 'var-ref :var var-bind))))

;;------------------------------------------------------------------------------
;; Ermittelt die abstrakte Syntax des Symbols. Erzeugt, falls noetig,
;; einen neuen Eintrag in der Symboltabelle.
;;------------------------------------------------------------------------------
(defun p1-make-symbol (symbol)
  (cond ((null symbol)
         (clicc-warning "NIL is used as symbol.")
         empty-list)
        (T (let ((sym (get-symbol-bind symbol)))
             (if sym
                 sym
                 (p1-make-new-symbol symbol))))))

;;------------------------------------------------------------------------------
;; Ermittelt die abstrakte Syntax des Symbols.
;; Erzeugt einen neuen Eintrag in der Symboltabelle.
;;------------------------------------------------------------------------------
(defun p1-make-new-symbol (symbol)
  (cond
    (*compiler-eval* (make-instance 'sym :symbol symbol))
    (t  
     (let ((sym
            (make-instance
             'defined-sym
             :name (symbol-name symbol)
             :package
             (if (not (symbol-package symbol))
                 :uninterned
                 (if (or (eq (symbol-package symbol) *lisp-package*)
                      
                         ;; Auch Symbole, die aus dem Wirst-Lisp-Package in das
                         ;; CLICC-Lisp Package importiert wurden, werden so
                         ;; behandelt, als waere ihr Home Package das
                         ;; CLICC-LISP-Package
                         ;;-------------------                      
                         (eq symbol (find-symbol (symbol-name symbol)
                                                 *lisp-package*)))
                     "LISP"
                     (package-name (symbol-package symbol))))
             :symbol symbol)))
       (when (eq (symbol-package symbol) *keyword-package*)
         (setf (?exported sym) t)
         (setf (?constant-value sym) sym))
       (bind-symbol sym)
       (add-q sym (?sym-list *module*))
       sym))))

;;------------------------------------------------------------------------------
;; Analysiert einen konstanten Ausdruck und stellt strukturierte
;; konstante Ausdruecke durch eine Struktur vom Typ structured-literal dar.
;;------------------------------------------------------------------------------
(defun p1-constant (object)
  (let ((array-in-const nil)
        (float-in-const nil))
    (labels
        ((in-constant (object)
           (typecase object
    
             (cons (cons (in-constant (car object))
                         (in-constant (cdr object))))
    
             (null   nil)

             (symbol (p1-make-symbol object))
    
             (number (typecase object
                       (integer object)
                       (float   (setq float-in-const t) object)
                       (t       (clicc-error TYPE_NOT_IMPL object))))

             (character object)

             (string object)

             (vector (dotimes (i (length object))
                       (setf (elt object i)
                             (in-constant (elt object i))))
                     object)
             (array (update-array #'in-constant object)
                    (setq array-in-const object)
                    object)
             
             (literal-instance (setf (?class object)
                                     (p1-make-symbol (?class object)))
                               (setf (?value-list object)
                                     (mapcar #'in-constant
                                             (?value-list object)))
                               object)

             (function (clicc-error "Function object not allowed as constant"))
             (t (error "Unknown atom ~A" object)))))
    
      (typecase object
        (null   empty-list)
        (number (typecase object
                  (integer (make-instance 'int :value object))
                  (float   (make-instance 'float-form :value object))
                  (t       (clicc-error TYPE_NOT_IMPL object))))
        (character (make-instance 'character-form :value object))
          
        ((or cons array literal-instance)
         (make-instance 'structured-literal
                        :value (in-constant object)
                        :needs-fixnum-array array-in-const
                        :needs-float-array float-in-const))
        (t (in-constant object))))))


;;------------------------------------------------------------------------------
;; DEFCONSTANT name initial-value [documentation]
;;------------------------------------------------------------------------------
(defun p1-defconstant (name_initial-value_documentation)
  (let (name initial-value documentation)
    (tagbody
       (setq name
             (if (atom name_initial-value_documentation)
               (go no-match)
               (pop name_initial-value_documentation)))
       (clicc-message "Analyse DEFCONSTANT  ~A" name)
       (setq initial-value
             (if (atom name_initial-value_documentation)
               (go no-match)
               (pop name_initial-value_documentation)))
       (setq documentation
             (if (atom name_initial-value_documentation)
               ""
               (pop name_initial-value_documentation)))
       (when (null name_initial-value_documentation) (go end))
       
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "(NAME INITIAL-VALUE &OPTIONAL (DOC-STRING NIL DOCP))"
                    "DEFCONSTANT")
     END)
    (unless (symbolp name)          (clicc-error NO_SYMBOL name))
    (unless (stringp documentation) (clicc-error NO_STRING documentation))
    (when (lex-var-name-p name)
      (clicc-error
       "defconstant: ~a has been used as lexical variable before" name))
    (when (let ((dyn-bind (find-global-dynamic name)))
            (and dyn-bind (plusp (?write (cdr dyn-bind)))))
      (clicc-error
       "defconstant: ~a has been bound or assigned a value to before" name))
    (p1-make-constant name initial-value)))

;;------------------------------------------------------------------------------
;; Deklariert eine Konstante 'name' mit Wert 'initial-value'.
;; Der Wert kann evtl. schon zur Uebersetzungszeit bestimmt werden und wird dann
;; dem Symbol zugeordnet. Ansonsten wird das Symbol im Attribut ?constant-value
;; mit dem ausgezeichneten Wert :unknown gekennzeichnet, der ausdrueckt,
;; dass der Wert des Symbols nach seiner Initialisierung zur Laufzeit nicht
;; veraendert werden darf.
;;------------------------------------------------------------------------------
(defun p1-make-constant (name initial-value)
  (let ((sym (p1-make-symbol name)))
    
    ;; Wurde das Symbol bereits mit DEFCONSTANT deklariert ?
    ;;-----------------------------------------------------
    (unless (eq :no-const (?constant-value sym))
      (clicc-warning "~S, being defined as constant ~S, has global value ~S"
                     name initial-value (?constant-value sym)))
    
    ;; Konstanten werden als SPECIAL proklamiert.  Warum ist das notwendig,
    ;; wenn sowieso keine Bindung vorgenommen werden darf ?
    ;;-----------------------------------------------------
    (p1-top-level-form
     `(L::PROCLAIM (L::QUOTE (L::SPECIAL ,name))))
    
    ;; Es wird versucht, 'initial-value' auszuwerten.
    ;;----------------------------------------------
    (multiple-value-bind (evaluated-initial-value evaluated-p)
        (p1-eval initial-value)
      (cond
        (evaluated-p (setf (?constant-value sym)
                           (p1-constant evaluated-initial-value)))
        
        ;; Hier muss zuerst die Zuweisung analysiert werden,
        ;; bevor das Symbol als Konstante markiert wird.
        ;; Sonst handelt es sich um eine nicht erlaubte
        ;; Zuweisung an eine Konstante.
        ;;--------------------------------------------------
        (T (p1-top-level-form `(L::SETQ ,name ,initial-value))
           (setf (?constant-value sym) :unknown))))))

;;------------------------------------------------------------------------------
(defun p1-defconstant-p (symbol)
  (let ((sym (get-symbol-bind symbol)))
    (and sym (not (eq :no-const (?constant-value sym))))))

;;------------------------------------------------------------------------------
;; DEFPARAMETER name initial-value [documentation]
;;------------------------------------------------------------------------------
(defun p1-defparameter (name_initial-value_documentation)
  (let (name initial-value documentation)
    (tagbody
       (setq name
             (if (atom name_initial-value_documentation)
               (go no-match)
               (pop name_initial-value_documentation)))
       (clicc-message "Analyse DEFPARAMETER ~A" name)
       (setq initial-value
             (if (atom name_initial-value_documentation)
               (go no-match)
               (pop name_initial-value_documentation)))
       (setq documentation
             (if (atom name_initial-value_documentation)
               ""
               (pop name_initial-value_documentation)))
       (when (null name_initial-value_documentation) (go end))
       
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "(NAME INITIAL-VALUE &OPTIONAL (DOC-STRING NIL DOCP))"
                    "DEFPARAMETER")
     END)
    (unless (symbolp name)          (clicc-error NO_SYMBOL name))
    (unless (stringp documentation) (clicc-error NO_STRING documentation))
    
    (p1-top-level-form
     `(L::PROGN
       (L::PROCLAIM (L::QUOTE (L::SPECIAL ,name)))

       ;; SET setzt auch dann die globale Variable, wenn das DEFPARAMETER
       ;; in einer nicht leeren lexikalischen Umgebung aufgerufen wird.
       ;;--------------------------------------------------------------
       (L::SET (L::QUOTE ,name) ,initial-value)))))

;;------------------------------------------------------------------------------
;; DEFVAR name [initial-value [documentation]]
;;------------------------------------------------------------------------------
(defun p1-defvar (name_initial-value_documentation)
  (let (name initial-value documentation init-val-sp)
    (tagbody
       (setq name
             (if (atom name_initial-value_documentation)
               (go NO-MATCH)
               (pop name_initial-value_documentation)))
       (unless *SDF*
         (clicc-message "Analyse DEFVAR       ~A" name))
       (setq initial-value
             (if (atom name_initial-value_documentation)
               NIL
               (progn
                 (setq init-val-sp t)
                 (pop name_initial-value_documentation))))
       (setq documentation
             (if (atom name_initial-value_documentation)
               ""
               (pop name_initial-value_documentation)))
       (when (null name_initial-value_documentation) (go END))
       
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "(NAME &OPTIONAL INITIAL-VALUE (DOC-STRING NIL DOCP))"
                    "DEFVAR")
     END)
    (unless (symbolp name)          (clicc-error NO_SYMBOL name))
    (unless (stringp documentation) (clicc-error NO_STRING documentation))
    
    (p1-top-level-form
     `(L::PROGN
       (L::PROCLAIM (L::QUOTE (L::SPECIAL ,name)))
       ,@(when init-val-sp
           `((L::UNLESS (L::BOUNDP (L::QUOTE ,name))

               ;; SET setzt auch dann die globale Variable, wenn das DEFVAR
               ;; in einer nicht leeren lexikalischen Umgebung aufgerufen wird.
               ;;--------------------------------------------------------------
               (L::SET (L::QUOTE ,name) ,initial-value))))))))

;;------------------------------------------------------------------------------
(defvar *secret-package-name* "1")
(defvar *counter* 1)

(defun find-secret-package-name ()
  (loop
   (if (find-package *secret-package-name*)
       (setq *secret-package-name* (format nil "$%~a%$" (incf *counter*)))
       (return))))

;;------------------------------------------------------------------------------
;; Fuegt Nicknames zu einem Package hinzu.
;;------------------------------------------------------------------------------
(defun add-nicknames (package new-nicknames)
  (let ((name (package-name package))
        (nicknames (union (package-nicknames package) new-nicknames)))
    (rename-package package (find-secret-package-name))
    (rename-package package name nicknames)))

;;------------------------------------------------------------------------------
;; IN-PACKAGE package-name &KEY :nicknames :use
;;------------------------------------------------------------------------------
(defun p1-in-package (name_rest)
  (apply #'(lambda (name &key nicknames use)
             (when (and nicknames (atom nicknames))
               (setq nicknames (list nicknames)))
             (when (and use (atom use))
               (setq use (list use)))
               
             (let* ((package (or (find-package name)
                                (make-package name :use '())))
                   (new-nicknames
                    (set-difference nicknames
                                    (package-nicknames package)
                                    :test #'string=))
                   (new-use
                    (set-difference use 
                                    (mapcar #'package-name
                                            (package-use-list package))
                                    :test #'string=)))
               (when new-nicknames (add-nicknames package new-nicknames))
               (when new-use (use-package use package))
    
               ;; Die IN-PACKAGE Anweisung muss vor der ersten Anweisung
               ;; erfolgen. Innerhalb eines Moduls darf wiederholt IN-PACKAGE
               ;; auf das gleiche Modul angewendet werden.
               ;;-----------------------------------------
               (cond
                 ((not (slot-boundp *module* 'package))
                  (setf (?package *module*) package))
                 ((eq (?package *module*) package) nil)
                 (t
                  (when (and (?toplevel-forms *GLOBAL-ENVIRONMENT*)
                             *MODULE-COMPILER*)
                    (clc-error "changing package to ~a is not allowed while ~
                                compiling Module ~a"
                               (package-name package)
                               (package-name (?package *module*))))
                  (setf (?package *module*) package)))

               ;; Der Aufruf von IN-PACKAGE muss evtl. auch im 
               ;; uebersetzten Programm vorkommen.
               ;; Nur Code erzeugen, wenn sich gegenueber dem letzten
               ;; IN-PACKAGE etwas geaendert hat.
               ;;--------------------------------
               (when (or (not (eq package *PACKAGE*)) new-nicknames new-use)
                 (save-toplevel-form
                  (p1-form `(L::IN-PACKAGE . ,name_rest))))

               ;; Die folgenden READ Operationen des Compilers erfolgen in dem
               ;; angegebenen Package.
               ;;---------------------
               (setq *PACKAGE* package)
               
               ;; Eintragen des evtl. neuen Packages in die globale Umgebung
               ;;-----------------------------------------------------------
               (addnew-q *PACKAGE* (?package-list *module*))))
               
         (mapcar #'p1-eval name_rest)))

;;------------------------------------------------------------------------------
;; USE-PACKAGE packages-to-use &OPTIONAL package 
;;------------------------------------------------------------------------------
(defun p1-use-package (args)
  (apply #'(lambda (packages-to-use &OPTIONAL package)
             (let ((current-package
                    (if (null package) *PACKAGE* (find-package package))))

               (when (null current-package)
                 (clicc-error "use-package: package ~s is undefined" package))
               (when (atom packages-to-use)
                 (setq packages-to-use (list packages-to-use)))

               (when (set-difference packages-to-use
                                     (mapcar #'package-name
                                             (package-use-list current-package))
                                     :test #'string=)
                 (use-package packages-to-use current-package))

               (save-toplevel-form (p1-form `(L::USE-PACKAGE . ,args)))))

         (mapcar #'p1-eval args)))

;;------------------------------------------------------------------------------
;; IMPORT symbols &OPTIONAL package
;;------------------------------------------------------------------------------
(defun p1-import (args)
  (apply #'(lambda (symbols &OPTIONAL package)
             (let ((current-package
                    (if (null package) *PACKAGE* (find-package package))))

               (when (null current-package)
                 (clicc-error "import: package ~s is undefined" package))
               (import symbols current-package)
               (save-toplevel-form (p1-form `(L::IMPORT . ,args)))))

         (mapcar #'p1-eval args)))

;;------------------------------------------------------------------------------
;; EXPORT {symbol || symbol-list} &OPTIONAL package
;; Bearbeitet Toplevel Aufrufe von export.
;; Traegt in die zugehoerigen Zwischensprachkonstrukte im Slot exported T ein.
;; Exportiert das Symbol im Lisp-Wirtssystem, wenn es noch nicht exportiert war.
;; Das verhindert, dass auf Symbole des Lisp-Packages export angewendet wird,
;; da das in neueren LISP-Systemen nicht erlaubt sein kann.
;;------------------------------------------------------------------------------
(defun p1-export (args)
  (let ((evaluated-args (mapcar #'p1-eval args)))
    (when (atom evaluated-args)
      (clicc-error ILLEGAL_CALL
                   "export"
                   "(symbols &optional package)"))
    (let ((symbols (pop evaluated-args))
          (package (if (p1-endp evaluated-args)
                       *PACKAGE*
                       (prog1 (pop evaluated-args)
                         (unless (p1-endp evaluated-args)
                           (clicc-error ILLEGAL_CALL
                                        "export"
                                        "(symbols &optional package)"))))))
      (unless (packagep package) (setq package (find-package package)))
      (unless (eq package *PACKAGE*)
        (unless *lisp-module*
          (clicc-error "Only Symbols in current package may be exported")))
      
      (unless (listp symbols)
        (setq symbols (list symbols)))
      (let ((symstoexport (empty-queue)))
        (dolist (symbol symbols)
          (if (symbolp symbol)
              (multiple-value-bind (symbol status)
                  (find-symbol (symbol-name symbol) package)
                (unless (eq :external status)
                  (export (if (null symbol) (list symbol) symbol) package))
                (if (not (null symbol))
                    (let ((sym (p1-make-symbol symbol)))
                      (setf (?exported sym) t)
                      (when (imported-sym-p sym)
                        (add-q symbol symstoexport)))
                    (add-q symbol symstoexport)))
              (clc-error "~s can not be exported" symbol)))
        (unless (empty-queue-p symstoexport)
          (save-toplevel-form
           (p1-form `(L::EXPORT  ',(queue2list symstoexport)))))))))

;;------------------------------------------------------------------------------
;; LOAD filename &KEY :verbose :print :if-does-not-exist
;;------------------------------------------------------------------------------
(defun p1-load (args)
  (labels ((gen-init-call (imported-module)
             (save-toplevel-form
              (make-app 
               :form (make-imported-fun
                      :symbol (make-symbol (?name imported-module))
                      :adr (?init-fun-name imported-module)
                      :par-spec 0
                      :write-list -1)   ; has side effect
               :arg-list ())))

           (gen-reset-package ()
             (save-toplevel-form
              (p1-form
               `(L::SETQ L::*PACKAGE*
                 (L::FIND-PACKAGE ,(package-name *PACKAGE*)))))))

    (let ((evaluated-args (mapcar #'p1-eval args)))
      (when (atom evaluated-args)
        (clicc-error ILLEGAL_CALL
                     "LOAD"
                     "(FILENAME &KEY :VERBOSE :PRINT :IF-DOES-NOT-EXIST)"))
      (let* ((filename (pop evaluated-args))) ; Schluesselworte ignorieren
        (cond
          ((is-module-import filename)  ; import
           (clicc-message "Importing definitions from module ~A"
                          (strip-path filename))
           (let ((imported-module (import-read filename)))
             (syntax-import filename) 
             (unless (string= filename *LISP-DEF*)
               (gen-init-call imported-module))
           (gen-reset-package)))
          (T                            ; include
           (pass_1-of-file filename))))))) 

;;------------------------------------------------------------------------------
;; PROVIDE module-name
;;------------------------------------------------------------------------------
(defun p1-provide (args)
  (let ((evaluated-args (mapcar #'p1-eval args)))
    (when (/= (length evaluated-args) 1)
      (clicc-error ILLEGAL_CALL "PROVIDE" "(MODULE-NAME)"))
    (let ((module-name (string (pop evaluated-args))))
      (pushnew module-name *CLICC-REQUIRED* :test #'string=)))) 

;;------------------------------------------------------------------------------
;; REQUIRE module-name &OPTIONAL pathname
;;------------------------------------------------------------------------------
(defun p1-require (args)
  (let ((evaluated-args (mapcar #'p1-eval args)))
    (when (atom evaluated-args)
      (clicc-error ILLEGAL_CALL
                   "REQUIRE" "(MODULE-NAME &OPTIONAL PATHNAME)"))
    (let ((module-name (string (pop evaluated-args))))
      (unless (member module-name *CLICC-REQUIRED* :test #'string=)
        (p1-top-level-form `(L::LOAD ,module-name))))))

;;------------------------------------------------------------------------------
(provide "p1tlf")
