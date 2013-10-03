;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung
;;;            - lokale, globale Funktionsdefinitionen
;;;            - Closures, Downfuns
;;;            - Aufrufe von importierten und definierten Funktionen
;;;
;;; $Revision: 1.58 $
;;; $Log: cgfuns.lisp,v $
;;; Revision 1.58  1994/06/08  09:54:33  hk
;;; opt-args: Bei Rest-Listen-Optimierung kann das Problem auftreten, da"s
;;; eine Variable LOCAL(x) den gleichen Offset hat als die
;;; Argument-Position eines Arguments an ARG(x).  Eine COPY-Anweisung
;;; zwischen diesen Positionen w"urde f"alschlich wegoptimiert werden.
;;;
;;; Revision 1.57  1994/06/07  16:15:24  hk
;;; Bug in CMU17 umgangen. Wieder vern"unftig einger"uckt.
;;;
;;; Revision 1.56  1994/05/25  14:05:17  sma
;;; Aufruf der Restlistenoptimierung aus cg-params herausgezogen.
;;;
;;; Revision 1.55  1994/05/02  14:20:55  sma
;;; opt-args korrigiert: Keine Optimierung, wenn ein Parameter eine
;;; Restvariable ist.
;;;
;;; Revision 1.54  1994/04/28  09:59:52  sma
;;; Erzeugung eines DOWN_FUNARGS durch Makroaufruf abstrahiert.
;;;
;;; Revision 1.53  1994/04/14  17:05:36  sma
;;; REST_NOT_EMPTY Makro wird statt (rest_X != local) benutzt.
;;;
;;; Revision 1.52  1994/02/16  16:45:29  hk
;;; *if-counter* *cont-counter* *tagbody-counter* werden f"ur jede
;;; Funktion neu mit 0 initialisiert.
;;;
;;; Revision 1.51  1994/02/10  16:01:00  sma
;;; cdr-rest-funcall-p -> rlo-rest-form.
;;;
;;; Revision 1.50  1994/02/08  13:56:53  sma
;;; Diverse Änderungen für rest-Parameter-Optimierungen.
;;;
;;; Revision 1.49  1994/02/03  17:31:23  sma
;;; Änderungen für Optimierung von &rest-Paramtern.
;;;
;;; Revision 1.48  1994/01/07  15:17:42  hk
;;; Nach der letzten Änderung wird opt-args in mehr Fällen aufgerufen, so
;;; daß ein Fehler bei der Ausnutzung von ?need-no-stack auftreten kann:
;;; man darf eine Funktion nur direkt auf die Variablenposition anwenden,
;;; wenn diese stackalloziert ist und im aktuellen Activation Record
;;; liegt.
;;;
;;; Revision 1.47  1994/01/07  13:40:06  hk
;;; In opt-args die Abschätzung für den Fall verbessert, daß die aktuelle
;;; Funktion lokale Funktionen hat.
;;;
;;; Revision 1.46  1994/01/07  10:18:52  hk
;;; closure-result-p entfernt, weil Daten vom Typ closure-result
;;; nicht mehr vorkommen.
;;;
;;; Revision 1.45  1994/01/06  17:28:34  sma
;;; opt-args verbessert, um mehr COPY-Befehle einzusparen. Die
;;; imported-funs aus sys.def haben eine neue Annotation need-no-stack
;;; bekommen, die den Wert T enthält, wenn die Funktion einstellig ist und
;;; keinen weiteren LISP-Stack benötigt. So kann die optimierung, daß das
;;; Funktionsergebnis eines Funktionsaufrufs an der Stelle erzeugt wird,
;;; wo es benötigt wird, öfter greifen.
;;; Wurde bei
;;; 	(let (a b)
;;; 	  (setq a (1+ a)))
;;; vorher
;;; 	COPY(ARG(0),ARG(2));
;;; 	F1plus(ARG(2));
;;; 	COPY(ARG(2),ARG(0));
;;; erzeugt, ist es jetzt nur
;;; 	F1plus(ARG(0));
;;;
;;; Revision 1.44  1994/01/05  12:28:24  sma
;;; cg-set-C-name so geändert, daß bei Funktionen aus dem rt-package ein
;;; Präfix "rt_" generiert wird, wenn die Funktionsnamen weder mit "c_"
;;; noch mit "unix_" beginnen.
;;;
;;; Revision 1.43  1993/12/23  12:02:25  hk
;;; Fehler in opt-args behoben.
;;;
;;; Revision 1.42  1993/12/18  09:11:12  hk
;;; In cg-downfuns #+CMU17(declare (notinline CC-NameConc)) eingefügt, um
;;; einen Fehler in CMU-CL 17.c zu umgehen
;;;
;;; Revision 1.41  1993/11/22  09:24:02  hk
;;; Neuer C-Code ONLY_ONCE in Initialisierungsfunktionen, der bewirkt,
;;; da_ diese Funktionen hvchstens 1x ausgef|hrt werden.
;;;
;;; Revision 1.40  1993/10/14  16:09:32  sma
;;; Code für die Erstellung einer Closure in das Makro GEN_CLOSURE
;;; verschoben.
;;;
;;; Revision 1.39  1993/07/13  11:14:14  uho
;;; Funktion 'C-ify' und 'cg-unique-prefix' (als 'unique-prefix') nach clcmisc verschoben.
;;;
;;; Revision 1.38  1993/07/07  15:49:00  uho
;;; Umwandlung von Lisp-Namen in C-Namen aus der Funktion cg-set-C-name
;;; herausgeloest und verfeinert. Wird jetzt auch CALC-SYMBOL-BASE verwendet.
;;;
;;; Revision 1.37  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.36  1993/06/04  13:45:08  pm
;;; Aufruf von cg-call-in-interface
;;;
;;; Revision 1.35  1993/05/19  08:54:16  hk
;;; Funktionen im *ffi-package* bekommen Praefix FFI_.
;;;
;;; Revision 1.34  1993/05/06  10:56:47  hk
;;; cg-set-C-name erweitert + korrigiert.
;;;
;;; Revision 1.33  1993/04/07  16:39:41  hk
;;; cg-fun-def bindet dynamische Variable *fun-name* fuer Fehlermeldungen.
;;;
;;; Revision 1.32  1993/04/07  16:11:09  hk
;;; In cg-set-C-name: cg-error -> clc-error.
;;;
;;; Revision 1.31  1993/04/06  17:28:21  hk
;;; ?codegen -> ?c-inline.
;;;
;;; Revision 1.30  1993/03/24  13:41:53  hk
;;; Symbole 'jump, 'closure, 'downfun in Slots heissen jetzt :jump,
;;; :closure und :downfun.
;;;
;;; Revision 1.29  1993/03/22  15:30:05  hk
;;; lower-case in Lisp Funktionsnamen -> upcase in C Funktionsnamen.
;;;
;;; Revision 1.28  1993/02/19  13:59:11  hk
;;; Fehler beim Test auf Lisp Package beseitigt.
;;;
;;; Revision 1.27  1993/02/17  11:33:58  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.26  1993/02/16  16:03:06  hk
;;; Revision Keyword eingefuegt, Test auf Lisp-Package in cg-set-C-name
;;; erweitert.
;;;
;;; Revision 1.25  1993/01/22  14:52:46  ft
;;; cg-set-C-name an erweiterte Funktionsnamen angepasst.
;;;
;;; Revision 1.24  1993/01/14  14:31:27  hk
;;; cg-set-C-name neu
;;;
;;; Revision 1.23  1992/09/28  17:22:30  hk
;;; (CC-bool fun) -> C-TRUE.
;;;
;;; Revision 1.22  1992/09/23  09:47:29  hk
;;; Fehler in C-global-closure beseitigt.
;;;
;;; Revision 1.21  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.20  1992/08/11  12:42:45  hk
;;; C-Ln --> C-Decl, fuer Deklarationen.
;;;
;;; Revision 1.19  1992/08/10  16:54:53  hk
;;; Variablen *mv-spec* und *mv-produced* werden nicht mehr benutzt.
;;;
;;; Revision 1.18  1992/08/05  09:13:21  hk
;;; opt-args wusste noch nichts von var-ref.
;;;
;;; Revision 1.17  1992/08/04  17:59:06  hk
;;; ?num-free-lex-vars wurde erst nach der ersten Benutzung initialisiert.
;;;
;;; Revision 1.16  1992/08/04  17:32:22  hk
;;; Fehler in opt-args behoben.
;;;
;;; Revision 1.15  1992/07/30  12:28:05  hk
;;; *downfun-count* wurde nicht beruecksichtigt.
;;;
;;; Revision 1.14  1992/07/30  10:18:49  hk
;;; Code fuer Downfuns umgestellt.
;;;
;;; Revision 1.13  1992/07/29  15:08:57  hk
;;; Berechnung von num-fee-lex-vars nicht mehr in Pass3, neue Konstante
;;; CLOSURE-EXTRA-SPACE.
;;;
;;; Revision 1.12  1992/07/28  10:52:44  hk
;;; cg-set-C-name aufgeraeumt.
;;;
;;; Revision 1.11  1992/07/27  13:08:36  hk
;;; * in Lisp-Namen wird zu X in C-Namen.
;;;
;;; Revision 1.10  1992/07/21  13:54:40  hk
;;; Fuer Standardfunktionen des LISP-Package werden C-Namen generiert,
;;; indem = nach E, < nach L , > nach G und / nach N abgebildet werden.
;;; string/= wird somit zu FstringNE.
;;;
;;; Revision 1.9  1992/07/21  13:16:57  hk
;;; C-Namen fuer Funktionen des Laufzeitsystems deren Symbol im LISP-Package
;;; liegt, beginnen mit dem Zeichen "F".
;;;
;;; Revision 1.8  1992/07/21  10:54:54  hk
;;; Slot closure-offset ist unbound und nicht nil, wenn noch nicht initialisiert
;;;
;;; Revision 1.7  1992/06/11  11:18:05  hk
;;; cg-error
;;;
;;; Revision 1.6  1992/06/11  11:11:31  hk
;;; C-Namen von lokalen Funktionen werden zu Beginn der Uebersetzung der
;;; umfassenden Funktion generiert. Schreibfehler: var -> local static.
;;; Rumpf von labels-form heisst ?body. Fuer die Uebesetzung lokaler Funktionen
;;; in gen-fun wird nur noch eine einfache dolist Schleife benoetigt.
;;;
;;; Revision 1.5  1992/06/04  16:07:13  hk
;;; Leerstring in CC-nargs eingefuegt.
;;;
;;; Revision 1.4  1992/06/04  12:39:53  hk
;;; ?const -> ?const-list
;;;
;;; Revision 1.3  1992/06/04  12:24:54  hk
;;; Zu Beginn der Bearbeitung von labels-form werden die C-Namen fuer lokale
;;; Funktionen generiert.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;;               1992/02/03            hk
;;; Neue Funktion cg-defun; Generierung von Konstanten erfolgt jeweils vor der
;;; Uebersetzung einer Funktion.
;;;
;;;               1992/01/17            hk
;;; Neue Funktion C-gen-decl. Uebersetzte Funktionen haben nun den Typ void.
;;; Neue Komponente in der Struktur fun, die die lokalen Funktionen enthaelt,
;;; damit auch lokale Funktionen deklariert werden koennen. Globale Funktionen
;;; werden am Anfang der Datei deklariert (siehe cgmain). Lokale Funktionen
;;; werden vor der Funktion deklariert, in deren Rumpf sie definiert sind.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Closures werden wie Simple-Vektoren dargestellt. Die ersten 3 Felder werden
;; fuer die Laengenangabe + Zeiger auf den Code + par_spec benoetigt.
;;------------------------------------------------------------------------------
(defconstant CLOSURE-EXTRA-SPACE 3)

;;------------------------------------------------------------------------------
(defun cg-defun (fun &key only-once)
  
  ;; Ein funktionales Objekt generieren, wenn die Funktion entsprechend
  ;; benutzt wird.
  ;;----------------------------------------------------------------
  (when (?closure fun)
    (C-empty-Ln)
    (C-global-closure fun))

  ;; Die strukturierten Konstanten generieren
  ;;-----------------------------------------
  (gen-literals (?const-list fun) (?adr fun)) 
  
  (cg-fun-def fun :only-once only-once))
          
;;------------------------------------------------------------------------------
(defun cg-fun-def (fun &key only-once)
  (let* ((*fun-name* (?symbol fun))     ; fuer Fehlermeldungen
         (*current-fun* fun)
         (global (or (global-fun-p fun)
                     (?as-global-fun fun)))
         (closure (and (not global)
                       (eq :CLOSURE (?closure fun))))
         (*stack-top* 0)
         (*level* (if (local-fun-p fun) (?level fun) 0))
         (old-special *special-count*)
         (*result-spec* (cond
                          ((eq :JUMP (?mv-spec fun)) nil)
                          (closure (stacktop-location))
                          (t (stacktop-result-location)))))
    (setq *if-counter* (setq *cont-counter* (setq *tagbody-counter* 0)))
    (flet 
        ((gen-fun ()
           (when (?local-funs fun)
             (C-empty-Ln)
             (dolist (local (?local-funs fun))

               ;; C-Namen der lokalen Funktionen generieren
               ;;------------------------------------------
               (cg-set-C-name local)

               ;; Deklarationen fuer lokale Funktionen und ggf. Closures
               ;;-------------------------------------------------------
               (C-fun-decl local)
               (when (and (?as-global-fun local)
                          (?closure local))
                 (C-global-closure local))))

           (C-fun-entry fun)
           (C-blockstart)
           (when (and (> (?max-level fun) 0) (or global closure))

             ;; Platz fuer das Environment lokaler Funktionen bereitstellen,
             ;; falls dies eine globale Funktion oder eine Closure ist.
             ;;--------------------------------------------------------
             (C-PtrArrayDecl "CL_FORM" C-display (?max-level fun)))

           (when closure
             ;; 1. aktueller Parameter, die Closure-Datenstruktur,
             ;; nicht bearbeiten
             ;;-----------------------------------------------------
             (incf *stack-top*))

           (cg-params (?params fun) (>= (?par-spec fun) 0)
                      (rest-optimization-p (?params fun) (?body fun)
                                           (?local-funs fun)))

           ;; Initialisiserungsfunktionen dürfen nur einmal aufgerufen werden.
           ;;-----------------------------------------------------------------
           (when only-once
             (C-only-once))

           ;; Rumpf der Funktion
           ;;-------------------
           (cg-form (?body fun))

           ;; Ggf. die dynamischen Variablen restaurieren,
           ;; die in cg-formal-params gebunden wurden.
           ;;-----------------------------------------------
           (C-restore-special old-special)
           (C-blockend)
           
           ;; Die im Rumpf definierten lokalen Funktionen uebersetzen
           ;;--------------------------------------------------------
           (dolist (local (?local-funs fun))
             (cg-fun-def local))))
     
      (cond
        (closure (let ((*closure* fun)
                       (*cl-level* *level*))
                   (gen-fun)))
        
        (global (let ((*cl-level* *level*))
                  (gen-fun)
                  (when (and (global-fun-p fun)
                             (?call-in fun))
                    (cg-call-in-interface fun))))
        
        (t (gen-fun))))))

;;------------------------------------------------------------------------------
(defun C-global-closure (fun)
  (if (local-fun-p fun)
      (C-static-GLOBAL_FUNARG-Init (CC-NameConc "C" (?adr fun))
                                   (?adr fun)
                                   (?par-spec fun))
      (C-GLOBAL_FUNARG-Init (CC-NameConc "C" (?adr fun))
                            (?adr fun)
                            (?par-spec fun))))

;;------------------------------------------------------------------------------
;; Erzeugt den Kopf der C-Prozedur, in die die Funktion, die durch 'fun'
;; repraesentiert wird, uebersetzt werden soll.
;;------------------------------------------------------------------------------
(defun C-fun-entry (fun)
  (let ((need-display (and (local-fun-p fun)
                           (not (?as-global-fun fun))
                           (not (eq :CLOSURE (?closure fun)))))
        (need-nargs (< (?par-spec fun) 0)))

    (C-empty-Ln)

    (cond
      (*ANSI-C*
       (C-Ln (if (local-fun-p fun) "static " "")
             "void " (?adr fun)
             "(CL_FORM *base"
             (if need-display ", CL_FORM *display[]" "")
             (if need-nargs ", int nargs" "")
             ")"))
      (T
       (C-Ln (if (local-fun-p fun) "static " "")
             "void " (?adr fun)
             "(base"
             (if need-display ", display" "")
             (if need-nargs ", nargs" "")
             ")")  
       (C-Ln "CL_FORM *base;")
       (when need-display (C-Ln "CL_FORM *display[];"))
       (when need-nargs (C-Ln "int nargs;"))))))

;;------------------------------------------------------------------------------
;; Deklaration bzw. Prototype fuer eine Funktion erzeugen
;;------------------------------------------------------------------------------
(defun C-fun-decl (fun)
  (let ((need-display (and (local-fun-p fun)
                           (not (?as-global-fun fun))
                           (not (eq :CLOSURE (?closure fun)))))
        (need-nargs (< (?par-spec fun) 0)))

    (cond
      (*ANSI-C*
       (C-Ln (if (local-fun-p fun) "static " "")
             "void " (?adr fun)
             "(CL_FORM *base"
             (if need-display ", CL_FORM *display[]" "")
             (if need-nargs ", int nargs" "")
             ");"))
      (T
       (C-Ln (if (local-fun-p fun) "static " "")
             "void " (?adr fun)
             "();")))))

;;------------------------------------------------------------------------------
;; Generiert aus einem Symbol einen String, der als C-Identifikator benutzt
;; werden soll. Symbole koennen Zeichen enthalten, die in C-Identifikatoren
;; nicht verwendet werden duerfen.
;; 
;; Die Konvertierungsregeln sind in der Hilfsfunktion C-ify festgelegt.
;;
;; Die Konvertierung ist nicht injektiv, daher wird dem generierten String im
;; allgemeinen ein eindeutiger Prefix vorangestellt.  Fuer Symbole des Lisp
;; und Runtime Package wird sichergestellt, dass keine 'weiteren Zeichen' in
;; den Symbolen vorkommen. Allen Funktionen, die weder mit "c-" noch mit 
;; "unix-" beginnen, wird der Praefix "rt_" vorangestellt, so dass aus
;; (rt::make_string) ein rt_make_string wird.
;; Die Indentifikatoren fuer Symbole des Lisp-Package werden nur deshalb mit
;; dem Praefix "F" versehen, um sie sofort als Standard-Lisp-Funktionen
;; identifizieren zu k"onnen.  Symbole des User-package werden nur dann mit
;; einem eindeutigen Prefix versehen, wenn sie 'weitere Zeichen' enthalten,
;; sonst wird der Prefix "U" verwendet.  Die Form der eindeutigen Prefixe ist
;; durch die Hilfsfunktion unique-prefix festgelegt.
;;------------------------------------------------------------------------------
(defun cg-set-C-name (fun)
  (labels ((unique-name (symbol)
             (unique-prefix (symbol2ident symbol :allow-illegal t)))
           (symbol2ident (symbol &key (allow-illegal nil))
             (C-ify (string symbol) :allow-illegal allow-illegal))
           (rt-symbol2ident (symbol)
             (let ((str (symbol2ident symbol)))
               (if (and (> (length str) 5)
                        (or (string= (subseq str 0 2) "c_")
                            (string= (subseq str 0 5) "unix_")))
                   str
                   (concatenate 'string "rt_" str)))))
    
    (setf (?adr fun)
          (let ((symbol (?symbol fun)))
            (etypecase symbol
              (cons
               (let* ((real-symbol (second symbol))
                      (package (symbol-package real-symbol))
                      (string (concatenate 'string
                                           "SET-" (string real-symbol))))
                 (cond
                   ((local-fun-p fun) (unique-name string))
                   ((eq package *runtime-package*) 
                    (rt-symbol2ident string))
                   ((eq package *ffi-package*)
                    (concatenate 'string "FFI_" (symbol2ident string)))
                   (T (multiple-value-bind (s status)
                          (find-symbol (symbol-name real-symbol) *lisp-package*)
                        (if (eq s real-symbol)
                            (if (eq :external status)
                                (concatenate 'string "F" (symbol2ident string))
                                (symbol2ident string))
                            (unique-name string)))))))
              (symbol
               (let ((package (symbol-package symbol)))
                 (cond
                   ((local-fun-p fun) (unique-name symbol))
                   ((eq package *runtime-package*) 
                    (rt-symbol2ident symbol))
                   ((eq package *ffi-package*)
                    (concatenate 'string "FFI_" (symbol2ident symbol)))
                   ((eq package *user-package*)
                    (multiple-value-bind (id illegal)
                        (symbol2ident symbol :allow-illegal t)
                      (if illegal
                          (unique-prefix id)
                          (concatenate 'string "U" id))))
                   (T 
                    ;; Auch Symbole, die aus dem Wirts-Lisp-Package in das
                    ;; CLICC-Lisp Package importiert wurden, werden so
                    ;; behandelt, als waere ihr Home Package das
                    ;; CLICC-LISP-Package
                    ;;-------------------
                    (multiple-value-bind (s status)
                        (find-symbol (symbol-name symbol)
                                     *lisp-package*)
                      (if (eq s symbol)
                          (if (eq :external status)
                              (concatenate 'string "F" (symbol2ident symbol))
                              (symbol2ident symbol))
                          (unique-name symbol))))))))))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((form labels-form))
  (let ((old-stack *stack-top*)
        (funs (?fun-list form)))
    
    ;; 3 Passes ueber die lokalen Funktions-Definitionen:

    ;; 1. Pass:
    ;;---------
    ;; Closure-Datenstrukturen anlegen.
    ;; Sie enthalten noch nicht die Zeiger auf die Closure-Datenstrukturen
    ;; der Funktionen, die im gleichen LABELS spaeter definiert werden.
    ;;------------------------------------------------------------------
    (dolist (fun funs)
      (unless (?as-global-fun fun)
        (when (eq :CLOSURE (?closure fun))
          
          ;; Dieser Wert wir im folgenden haeufiger benoetigt.
          ;;--------------------------------------------------
          (setf (?num-free-lex-vars fun) (length (?free-lex-vars fun)))
            
          ;; Closure-Datenstruktur bilden.
          ;; Sie wird im Activation-Record der umfassenden Funktion abgelegt.
          ;;-----------------------------------------------------------------
          (C-gen-closure fun (CC-StackTop))
          (setf (?closure-offset fun) *stack-top*)
          (incf *stack-top*))))

    ;; 2. Pass:
    ;;---------
    ;; In die im 1. Pass gebildeten Closure-Datenstrukturen noch die Zeiger
    ;; auf spaeter gebildete Closure-Datenstrukturen eintragen.
    ;;---------------------------------------------------------
    (dolist (fun funs)
      (let (i)
        (unless (?as-global-fun fun)
          (when (eq :CLOSURE (?closure fun))
            (setq i (+ CLOSURE-EXTRA-SPACE (?num-free-lex-vars fun)))
            (dolist (free-fun (?free-local-funs fun))
              (when (and (= (?level free-fun)
                            (?level fun))
                         (> (?closure-offset free-fun)
                            (?closure-offset fun)))
                (C-copy (CC-closure free-fun)
                        (CC-closure-access (CC-closure fun) i)))
              (incf i))))))
  

    ;; Die Codeerzeugung fuer die lokalen Funktionen erfolgt erst,
    ;; wenn die Uebersetzung der aktuellen Funktion beendet ist.
    ;;---------------------------------------------------------
    
    ;; in 'forms' werden die lokalen Funktionen angewandt.
    ;;----------------------------------------------------
    (cg-form (?body form))
    (setq *stack-top* old-stack)))

;;------------------------------------------------------------------------------
;; Erzeugt in 'dest' einen Zeiger auf eine CLOSURE-Datenstruktur,
;; die im Heap angelegt wird.
;; 0.       Eintrag: Typ CL_INT + Laenge der Datenstruktur
;; 1.       Eintrag: Typ CL_CODE + Zeiger auf die Funktion
;; 2.       Eintrag: Typ CL_INT + Parameter Spezifikation
;; 3. - n.  Eintrag: Typ CL_IND + Zeiger auf die freien Variablen im Heap
;; n+1. - 
;; n+m.     Eintrag: Typ CL_CLOSURE + Zeiger auf die Closures der freien 
;;                      lokalen Funktionen.
;;                   (Evtl. zunaechst NIL, s.u.)
;;------------------------------------------------------------------------------
(defun C-gen-closure (fun dest)

  ;; In der Komponente ?free-local-funs duerfen keine
  ;; globalisierten lokalen Funktionen vorkommen !
  ;;----------------------------------------------
  (setf (?free-local-funs fun)
        (delete-if #'?as-global-fun
                   (?free-local-funs fun)))

  (let ((heap-size (+ CLOSURE-EXTRA-SPACE (?num-free-lex-vars fun)
                      (length (?free-local-funs fun))))
        (i CLOSURE-EXTRA-SPACE)
        (C-array "array"))

    (C-blockstart)

    ;; Erzeugt Code für den Kopf einer Closure-Struktur. Entspricht im Prinzip
    ;; einem Simple-Vektor, dessen zwei erste Einträge ein Zeiger auf den CODE
    ;; der Funktion und deren Parameter-Spezifikation ist
    ;;---------------------------------------------------
    (C-MacroCall "GEN_CLOSURE" C-array (CC-StackTop) heap-size 
                 (?adr fun) 
                 (?par-spec fun))
    
    ;; freie Variablen
    ;;----------------
    (dolist (var (?free-lex-vars fun))
      
      ;; Der Zeiger (Typ CL_IND) auf die Variable im Heap wird kopiert.
      ;; Evtl. liegt der Zeiger selbst schon im Heap, wenn die Closure
      ;; innerhalb einer Closure gebildet wird.
      ;;---------------------------------------
      (C-copy (if (< (?level var) *cl-level*)
                  (CC-heapenv (closure-var-offset var))
                  (CC-frame-access (?offset var) (?level var)))
              (CC-arrayptr C-array i))
      (incf i))

    ;; freie Funktionen
    ;;-----------------
    (dolist (fun (?free-local-funs fun))
      
      ;; Der LISP-Zeiger (Typ CL_CLOSURE) auf die Closure wird kopiert.
      ;; Er liegt entweder im Stack und wurde zur Definitionszeit
      ;; der Funktion dort abgelegt oder in der Closure-Datenstruktur einer 
      ;; umfassenden Closure.
      ;; Wenn die Funktion erst spaeter definiert wird (in einem LABELS),
      ;; dann wird zunaechst NIL eingetragen.
      ;;--------------------------------------
      (if (slot-boundp fun 'closure-offset)
          (C-copy (CC-closure fun) (CC-arrayptr C-array i))
          (C-nil (CC-arrayptr C-array i)))
      (incf i))
    (C-MacroCall "LOAD_CLOSURE" C-array dest)
    (C-blockend)))

;;------------------------------------------------------------------------------
;; Berechnung des Offsets einer Variablenadresse in der aktuellen Closure
;;------------------------------------------------------------------------------
(defun closure-var-offset (static)
  (+ CLOSURE-EXTRA-SPACE
     (position static (?free-lex-vars *closure*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Berechnung des Offsets einer Closureadresse in der aktuellen Closure
;;------------------------------------------------------------------------------
(defun closure-fun-offset (fun)
  (+ CLOSURE-EXTRA-SPACE
     (?num-free-lex-vars *closure*)
     (position fun (?free-local-funs *closure*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String), fuer den Zugriff auf die CLOSURE-Datenstruktur
;; der lokalen Funktion, die durch 'fun' beschrieben wird.
;;------------------------------------------------------------------------------
(defun CC-closure (fun)
  (cond
    ((eq fun *closure*)
     
     ;; rekursiver Aufruf
     ;; Zeiger auf die CLOSURE-Datenstruktur wurde als 1. Parameter uebergeben.
     ;;------------------------------------------------------------------------
     (CC-Stack 0))

    ((<= (?level fun) *cl-level*)
     
     ;; Die lokale Funktion ist  frei innerhalb der aktuellen Closure.
     ;; Der Zeiger auf ihre CLOSURE-Datenstruktur liegt in
     ;; der CLOSURE-Datenstruktur der aktuellen Closure.
     ;;-------------------------------------------------
     (CC-heapenv (closure-fun-offset fun)))
    (t
     ;; ein Zeiger auf die CLOSURE-Datenstruktur der lokalen Funktion liegt 
     ;; im Activation-Record der umfassenden Funktion.
     ;;-------------------------------------------------------------------
     (CC-frame-access (?closure-offset fun) (1- (?level fun))))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((fun imported-fun))
  (case *result-spec*
    ((NIL))
    (C-BOOL (setq *C-bool* C-TRUE))
    (T (let ((dest (CC-dest *result-spec*))
             (C-extern_closure "extern_closure"))
         (C-blockstart)
         (C-Static-GLOBAL_FUNARG-Init C-extern_closure
                                      (?adr fun)
                                      (?par-spec fun))
         (C-MacroCall "LOAD_GLOBFUN" (CC-Address C-extern_closure) dest)
         (C-Blockend)))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((fun defined-fun))
  (case *result-spec*
    ((NIL))
    (C-BOOL (setq *C-bool* C-TRUE))
    (T (let ((dest (CC-dest *result-spec*)))
         (if (or (global-fun-p fun)
                 (?as-global-fun fun))
               
             ;; Closure wurde einmal beim Laden des Programms erzeugt.
             ;;-------------------------------------------------------
             (C-MacroCall "LOAD_GLOBFUN"
                          (CC-Address (CC-NameConc "C" (?adr fun)))
                          dest)

             (case (?closure fun)
               (:closure (C-copy (CC-closure fun) dest))
               (:downfun
                (C-MacroCall "LOAD_DOWNFUN"
                             (CC-Address (CC-NameConc "downfun"
                                                      (incf *downfun-count*)))
                             dest))
               (T (error "illegal closure-type ~A" (?closure fun)))))))))

;;------------------------------------------------------------------------------
;; Optimierung:
;; Funktionsaufruf evtl. so uebersetzen, dass das Resultat gleich an der
;; gewuenschten Stelle erzeugt wird.
;; z.B:
;; (let ((x))
;;    ...
;;    (setq x (car x))
;;    ...
;; )
;; (defun f (x)
;;    (car (car x))
;; )
;; (f 1 (let ((x 1)) (+ x x)) 3)
;;------------------------------------------------------------------------------
(defun opt-args (args &optional fun)
  (when (or (stacktop-result-p *result-spec*)
            (and (imported-fun-p fun)
                 (?need-no-stack fun)
                 (local-static-p *result-spec*)
                 (eql (?level *result-spec*) *level*)
                 (not (?closure *result-spec*))))
    (let* ((new-stack-top (?offset *result-spec*))
           (deleted-stack (1- new-stack-top)))
      (when
          (and
           (> *stack-top* new-stack-top)

           ;; Bei Rest-Listen-Optimierung kann das Problem auftreten, da"s
           ;; eine Variable LOCAL(x) den gleichen Offset hat als die
           ;; Argument-Position eines Arguments an ARG(x).
           ;; Eine COPY-Anweisung zwischen diesen Positionen w"urde
           ;; f"alschlich wegoptimiert werden.
           ;;---------------------------------
           (or (not *rest-optimization*) (>= new-stack-top *rest-optimization*))

           ;; Prüfen, ob die aktuelle Funktion lokale Funktionsdefinitionen
           ;; enthält. Wenn diese Funktionen freie Variablen der aktuellen
           ;; Funktion enthalten und diese im Stack liegen, was nur der Fall
           ;; ist, wenn es Downward Functions sind, und wenn sie eine
           ;; Continuation der aktuellen Funktion frei enthalten, dann können
           ;; sie evtl. noch auf Variablen im zu optimierenden Stackabschnitt
           ;; zugreifen. Die Optimierung muß dann unterbleiben.
           ;;-----------------------------------------------------
           (not (find-if #'(lambda (local-fun)
                             (and (eq :DOWNFUN (?closure local-fun))
                                  
                                  ;; Abschätzung: Continuation ODER Variable
                                  ;;----------------------------------------
                                  (find *level* (?free-lex-vars local-fun)
                                        :key #'?level)))
                         (?local-funs *current-fun*)))

           ;; pruefen, ob die ueberschriebenen Parameter und lokalen Variablen
           ;; als Argumente der aufzurufenden Funktion benutzt werden.
           ;;---------------------------------------------------------
           (labels
               ((check-args (args last-arg)
                  (setq deleted-stack (max last-arg deleted-stack))
                  (dolist (arg args t)
                    (cond
                      ((var-ref-p arg)
                       (setq arg (?var arg))
                       (cond
                         ((static-p arg)
                          ;; Wenn Variablen-Referenz eine Rest-Variable in
                          ;; einer Funktion mit Restlisten-Optimierung ist
                          ;; oder wenn Variable in dem überschriebenen 
                          ;; Bereich liegt...
                          (when (or (minusp (?offset arg))
                                    (and (eql *level* (?level arg))
                                     (<= new-stack-top
                                         (?offset arg)
                                         deleted-stack)))
                            (return nil)))
                         ((dynamic-p arg))
                         (t (return nil))))
                      ((sym-p arg))
                      ((named-const-p arg))
                      ((simple-literal-p arg))
                      ((structured-literal-p arg))
                      ((fun-p arg))
                      ((class-def-p arg))
                      ((cont-p arg))
                      ((app-p arg)
                       (let ((fun (?form arg)))
                         (cond

                           ;; zusaetzlichen Parameter von Closures beachten
                           ;;----------------------------------------------
                           ((and (local-fun-p fun)
                                 (eq :CLOSURE (?closure fun))
                                 (not (?as-global-fun fun)))
                            (unless (check-args (?arg-list arg)
                                                (1+ last-arg))
                              (return nil))
                            (setq deleted-stack most-positive-fixnum))
                      
                           ((simple-fun-p fun)
                            (unless (check-args (?arg-list arg)
                                                last-arg)
                              (return nil))
                            (setq deleted-stack most-positive-fixnum))
                           (T (return nil)))))
                      (T (return nil)))
                    (incf last-arg)
                    (setq deleted-stack (max last-arg deleted-stack)))))
             (check-args args deleted-stack))
      
           (or
            (not (local-fun-p fun))

            (labels ((free-vars-ok (fun)
             
                       ;; pruefen, ob die ueberschriebenen Parameter und
                       ;; lokalen Variablen als freie Variablen der Funktion
                       ;; benutzt werden.
                       ;;---------------------------------------------------
                       (dolist (var (?free-lex-vars fun) t)
                         (when (and (eql *level* (?level var))
                                    (>= (?offset var) new-stack-top))
                           (return nil)))))
              (and
               (free-vars-ok fun)

               ;; Eine Variable kann auch noch dann benutzt werden, wenn sie
               ;; nicht in der aufzurufenden, sondern in einer von dieser
               ;; Funktion aufgerufenen Funktion frei vorkommt.
               ;; z.B:
               ;; (let ((x 7))
               ;;   (labels ((g () x)
               ;;            (f () (g)))
               ;;     (f)))
               ;;----------------------------------------------
               (dolist (free-fun (?free-local-funs fun) t)
                 (when (> (?level free-fun) *level*)
                   (unless (free-vars-ok free-fun)
                     (return nil))))))))

        (setq *stack-top* new-stack-top)))))

;;------------------------------------------------------------------------------
;; Die Parameter evaluieren und die Resultate auf den Stack legen.
;;-Resultat:
;; Anzahl der Argumente, oder NIL, wenn kein Parameter 'nargs' benoetigt wird.
;;------------------------------------------------------------------------------
(defun cg-args (args par-spec)
  (let ((*result-spec* (stacktop-result-location))
        (old-stack *stack-top*))
    
    (dolist (arg args)
      (cg-form arg)
      (incf *stack-top*)
      (incf (?offset *result-spec*)))

    (prog1 (if (>= par-spec 0) nil (- *stack-top* old-stack))
      (setq *stack-top* old-stack))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((app app))
  (cg-app (?form app) (?arg-list app) app))

;;------------------------------------------------------------------------------
(defmethod cg-app ((fun special-sys-fun-with-mv) args app)
  (if (?c-inline fun)
      (let ((*stack-top* *stack-top*))
        (funcall (?c-inline fun) args app))
      (call-next-method)))

;;------------------------------------------------------------------------------
(defmethod cg-app ((fun special-sys-fun) args app)
  (if (and *rest-optimization* 
           (?my-last-arg-may-be-rest-var fun)
           (rlo-rest-form (car (last args))))
      (rlo-dispatch fun args app)
      ;;else
      (if (?c-inline fun)
          (let ((*stack-top* *stack-top*))
            (apply (?c-inline fun) args))
          (call-next-method))))
  
;;------------------------------------------------------------------------------
;; Traegt die Basisadresse des aktuellen Activation-Records in den fuer
;; die aktuelle Funktion bereitgestellten Eintrag im Display-Array ein.
;;------------------------------------------------------------------------------
(defun C-save-base ()
  (C-Assign (CC-arraykomp C-display (- *level* *cl-level*)) (CC-Stack 0)))

;;------------------------------------------------------------------------------
(defmethod cg-app ((fun imported-fun) args app)
  (if (and *rest-optimization* 
           (?my-last-arg-may-be-rest-var fun)
           (rlo-rest-form (car (last args))))
      (rlo-dispatch fun args app)
      ;;else
      (let* (nargs
             new-base
             (old-stack *stack-top*)
             (*downfun-count* 0)
             (save-base (cg-downfuns app)))
        
        (opt-args args fun)             ; veraendert *stack-top* !
        (setq new-base (CC-StackTop))
        (setq nargs (cg-args args (?par-spec fun)))
        (when save-base (C-save-base))
        
        ;; Aufruf einer globalen Funktion.
        ;;--------------------------------
        (C-Lispcall (?adr fun) new-base nargs) 
        
        (unless (eq (?mv-spec fun) :JUMP)
          
          ;; mv_count auf 1 ruecksetzen, falls die Funktion nicht benoetigte
          ;; Values erzeugt.
          ;;---------------------------------
          (unless (or (eql (?mv-spec fun) 1) (?mv-used app))
            (C-ResetMV))
          
          (stacktop-to-result-loc))
        
        (setq *stack-top* old-stack)
        
        (when (> *downfun-count* 0)
          (C-blockend)))))

;;------------------------------------------------------------------------------
(defmethod cg-app ((fun defined-fun) args app)
  (if (and *rest-optimization* 
           (?my-last-arg-may-be-rest-var fun)
           (rlo-rest-form (car (last args))))
      (rlo-dispatch fun args app)
      ;;else
      (let* (nargs
             new-base
             level
             (adr (?adr fun))
             (old-stack *stack-top*)
             (closure (and (local-fun-p fun)
                           (eq :CLOSURE (?closure fun))
                           (not (?as-global-fun fun))))
             (*downfun-count* 0)
             (save-base (cg-downfuns app)))
        
        (opt-args args fun)             ; veraendert evtl. *stack-top* !
        (setq new-base (CC-StackTop))
        
        (when closure
          
          ;; Aufruf einer Closure.
          ;; Closure-Datensruktur wird als 1. Parameter uebergeben
          ;;------------------------------------------------------
          (C-Copy (CC-closure fun) (CC-StackTop))
          (incf *stack-top*))
        
        (setq nargs (cg-args args (?par-spec fun)))
        (when save-base (C-save-base))
        
        (cond
          
          ;; Aufruf einer globalen Funktion oder einer Closure.
          ;;---------------------------------------------------
          ((or (not (local-fun-p fun))
               (?as-global-fun fun)
               closure)
           
           (C-Lispcall adr new-base nargs) 
           
           (when closure
             (decf *stack-top*)))
          
          ;; Aufruf einer im Rumpf der aktuellen Funktion lokal definierten
          ;; Funktion, vorher Display um aktuelle Basisadresse erweitern.
          ;;-------------------------------------------------------------------
          ((> (setq level (?level fun)) *level*)
           (C-save-base)
           (C-Lispcall adr new-base nargs C-display))
          
          ;; Aufruf einer lokalen Funktion mit gleichem statischen Niveau
          ;; oder einer lokalen Funktion f, mit niedrigerem statischen Niveau,
          ;; wenn gilt maxlevel(f) = 0.
          ;;------------------------------------------------------------------
          ((or (= level *level*)
               (and (eql 0 (?max-level fun)) (< level *level*)))
           (C-Lispcall adr new-base nargs C-display))
          
          ;; Aufruf einer lokalen Funktion f mit niedrigerem statischen Niveau.
          ;; Vorher neues Display anlegen und dorthin alle Eintraege kopieren,
          ;; die die aufgerufene Funktion benoetigt.
          ;;-------------------------------------------------------------------
          ((< level *level*)
           (C-blockstart)
           (C-PtrArrayDecl "CL_FORM" C-new_display (+ (- level *cl-level*)
                                                      (?max-level fun)))
           (dotimes (i (- level *cl-level*))
             (C-Assign (CC-arraykomp C-new_display i) 
                       (CC-arraykomp C-display i)))
           (C-Lispcall adr new-base nargs C-new_display)
           (C-blockend)))
        
        (unless (eq (?mv-spec fun) :JUMP)
          
          ;; mv_count auf 1 ruecksetzen, falls die Funktion nicht benoetigte
          ;; Values erzeugt.
          ;;---------------------------------
          (unless (or (eql (?mv-spec fun) 1) (?mv-used app))
            (C-ResetMV))
          
          (stacktop-to-result-loc))
        
        (setq *stack-top* old-stack)
        
        (when (> *downfun-count* 0)
          (C-blockend)))))

;;------------------------------------------------------------------------------
;; Funktion wird erst zur Laufzeit berechnet.
;;------------------------------------------------------------------------------
(defmethod cg-app (exp args app)
  (let* (nargs
         new-base
         (old-stack *stack-top*)
         (*downfun-count* 0)
         (save-base (cg-downfuns app)))

    (push exp args)
    (opt-args args)
    (setq new-base (CC-StackTop))
    (setq nargs (cg-args args -2))
    (when save-base (C-save-base))
    (C-Lispcall "Ffuncall" new-base nargs)

    ;; mv_count auf 1 ruecksetzen, falls die Funktion nicht benoetigte
    ;; Values erzeugt.
    ;;---------------------------------
    (unless (?mv-used app)
      (C-ResetMV))

    (stacktop-to-result-loc)
    (setq *stack-top* old-stack)

    (when (> *downfun-count* 0)
      (C-blockend))))

;;------------------------------------------------------------------------------
;; Generiert Code fuer die funktionalen Argumente unter den Argumenten der
;; Applikation, die nur downward verwendet werden.
;;------------------------------------------------------------------------------
(defun cg-downfuns (app)
  #+CMU17(declare (notinline CC-NameConc)) ; by-pass a bug
  (let ((downfuns (?downfun-list app))
        (save-base nil))
  
    ;; Bestimmen, ob bzw. wieviel Platz im C-Stack fuer 
    ;; DOWNFUN-Strukturen bereitgestellt werden muss. 
    ;;------------------------------------------------
    (when downfuns

      ;; feststellen, wieviele Downward-Funargs uebergeben werden
      ;;---------------------------------------------------------
      (dolist (downfun downfuns)
        (when (and (not (?as-global-fun downfun))
                   (eq :DOWNFUN (?closure downfun)))
          (incf *downfun-count*)))
      (when (> *downfun-count* 0)

        ;; Datenstrukturen fuer CL_DOWNFUNs im C-Stack anlegen.
        ;;-----------------------------------------------------
        (C-blockstart)
        (setq *downfun-count* 0)
        (dolist (downfun downfuns)
          (when (and (not (?as-global-fun downfun))
                     (eq :DOWNFUN (?closure downfun)))
            (let ((level (?level downfun))
                  (df-name (CC-NameConc "downfun" (incf *downfun-count*))))
              (C-VarDecl "DOWN_FUNARG" df-name)
              (cond
                ;; vor Aufruf 'base' in 'display' sichern
                ;;---------------------------------------
                ((> level *level*) nil)

                ;; 'display' kann direkt fuer das Downward-Funarg
                ;; verwendet werden.
                ;;------------------
                ((or (= level *level*)
                     (and (eql 0 (?max-level downfun))
                          (< level *level*))) nil)
                  
                ;; neues 'display' anlegen.
                ;;-------------------------
                ((< level *level*)
                 (C-PtrArrayDecl "CL_FORM"
                                 (CC-NameConc C-new_display *downfun-count*) 
                                 (+ (- level *cl-level*)
                                    (?max-level downfun))))))))

        ;; Datenstrukturen fuer Downfuns initialisieren.
        ;;----------------------------------------------
        (setq *downfun-count* 0)
        (dolist (downfun downfuns)
          (when (and (not (?as-global-fun downfun))
                     (eq :DOWNFUN (?closure downfun)))
            (let ((level (?level downfun))
                  (df-name (CC-NameConc "downfun" (incf *downfun-count*))))
              (C-MacroCall 
               "INIT_DOWN_FUNARG" df-name
               (?adr downfun)
               (?par-spec downfun)
               (cond
                   
                 ;; vor Aufruf 'base' in 'display' sichern
                 ;;---------------------------------------
                 ((> level *level*)
                  (setq save-base t)
                  C-display)
                   
                 ;; 'display' kann direkt fuer das Downward-Funarg
                 ;; verwendet werden.
                 ;;------------------
                 ((or (= level *level*)
                      (and (eql 0 (?max-level downfun))
                           (< level *level*)))
                  C-display)
                  
                 ;; 'display' muss kopiert werden.
                 ;;-------------------------------
                 ((< level *level*)
                  (dotimes (i (- level *cl-level*))
                    (C-Assign
                     (CC-arraykomp (CC-NameConc C-new_display 
                                                *downfun-count*) i)
                     (CC-arraykomp C-display i)))
                  C-new_display))))))

        (setq *downfun-count* 0)))
    save-base ))

;;------------------------------------------------------------------------------
;; Erzeugt C-Code für `rest', `(L:CDR rest)' oder `(L::CDR (L:CDR .. rest)..)'
;;------------------------------------------------------------------------------
(defun cg-rest-cdr (form)
  #+CMU17(declare (notinline C-PtrDecl)) ; by-pass a bug
  (labels
      ((rest-cdr (form)
         (if (var-ref-p form)
             (CC-restvar (?offset (?var form)))
             (progn 
               (C-Assign C-rest (CC-MacroCall 
                                 "REST_CDR" 
                                 (rest-cdr (car (?arg-list form)))))
               C-rest))))
    (unless (var-ref-p form)
      (C-blockstart)
      (C-PtrDecl "CL_FORM" C-rest))
    (rest-cdr form)))

;;------------------------------------------------------------------------------
;; Generiert Code für speziell zu behandelnde Funktionen bei rest-optimierung
;;------------------------------------------------------------------------------
(defun rlo-dispatch (fun args app)
  (let ((rest (cg-rest-cdr (car (last args)))))
    (incf *rlo-statistics-rest-usage*)
    (ecase (?my-last-arg-may-be-rest-var fun)
      (:length
       (case *result-spec*
         ((NIL))
         (C-bool (setq *C-bool* C-TRUE))
         (T (C-MacroCall "REST_LENGTH" rest (CC-dest *result-spec*)))))
      (:car
       (case *result-spec*
         ((NIL))
         (C-bool (setq *C-bool* (CC-op&& (CC-op!= rest C-local) 
                                         (CC-make-bool rest))))
         (T (C-MacroCall "REST_CAR" rest (CC-dest *result-spec*)))))
      (:cdr
       (case *result-spec*
         ((NIL))
         (C-bool (setq *C-bool* (CC-op!= (CC-MacroCall "REST_CDR" rest) 
                                         C-local)))
         (T (C-Assign (CC-dest *result-spec*)
                      (CC-MacroCall "REST_CDR" rest)))))
      (:apply
       (let* ((new-base (CC-StackTop))
              (nargs (cg-args (butlast (?arg-list app)) (?par-spec fun))))
         (C-MacroCall "REST_APPLY" new-base nargs rest)
         (unless (eq (?mv-spec fun) :JUMP)
           (unless (or (eql (?mv-spec fun) 1) (?mv-used app))
             (C-ResetMV))
           (stacktop-to-result-loc))))
      (:atom (pred-result (CC-MacroCall 
                           "NOT" (CC-MacroCall "REST_NOT_EMPTY" rest))))
      (:consp (pred-result (CC-MacroCall "REST_NOT_EMPTY" rest)))))
  (unless (var-ref-p (car (last args)))
    (C-blockend)))

;;------------------------------------------------------------------------------
(provide "cgfuns")

