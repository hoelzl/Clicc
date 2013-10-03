;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Attributierung des Zwischencodes fuer die Codeerzeugung
;;;
;;; $Revision: 1.73 $
;;; $Log: p3main.lisp,v $
;;; Revision 1.73  1994/06/10  09:44:09  hk
;;; ?mv-used wird ggf. auch wieder auf nil zur"uckgesetzt, da durch
;;; fr"uhre Aufrufe von pass3 und inlining ein falsches T in diesen Slot
;;; gelangt sein kann.
;;;
;;; Revision 1.72  1994/02/02  09:13:00  hk
;;; (defvar *current-fun*) nach clcdef
;;;
;;; Revision 1.71  1994/01/21  14:03:46  hk
;;; *CURRENT-FORM* wurde unoetig gebunden
;;;
;;; Revision 1.70  1994/01/10  09:08:59  hk
;;; apply #'max (mapcar  --> reduce
;;;
;;; Revision 1.69  1994/01/05  12:32:10  sma
;;; Namensänderung im Laufzeitsystem: -INTERNAL bei rt::CATCH-INTERNAL,
;;; rt::UNWIND-PROTECT-INTERNAL, rt::THROW-INTERNAL und rt::PROGV-INTERNAL
;;; gestrichen.
;;;
;;; Revision 1.68  1993/12/09  08:52:07  hk
;;; Fehler behoben: Auswertung einer Klasse liefert EINEN Wert.
;;; p3-form spezialisiert nun über literal statt über simple literal,
;;; dadurch kann die Methode für sym entfallen und die Methode für
;;; structured-literal verwendet call-next-method.
;;;
;;; Revision 1.67  1993/11/02  14:01:03  hk
;;; Fehler in p3-analyse-mv-spec behoben: ?toplevel-forms und dessen
;;; lokale Funktionen wurden vergessen zu analysieren.
;;;
;;; Revision 1.66  1993/10/15  12:57:41  hk
;;; Initialisierung der Slots zum großen Teil in die Bearbeitung von
;;; labels-form oder p3-fun-def verschoben, um die Benutzung des Slots
;;; all-funs zu vermeiden.
;;;
;;; Revision 1.65  1993/10/13  10:20:17  hk
;;; Methode (p3-form T) entfernt.
;;;
;;; Revision 1.64  1993/10/13  10:19:20  hk
;;; In p3-app den Spezialisierer T durch form ersetzt.
;;;
;;; Revision 1.63  1993/09/20  14:12:50  jh
;;; pass_3 fuer wiederholte Anwendung geaendert.
;;;
;;; Revision 1.62  1993/09/13  14:18:14  hk
;;; Aufrufe von (setf ?used) für Symbole, p3-syms-in-const und
;;; traverse-const entfernt, weil used Slot von syms im weiteren Verlauf
;;; nicht benutzt wird.
;;;
;;; Revision 1.61  1993/09/09  10:37:12  hk
;;; Fehler in der Multiple-Value Analyse behoben. Trat auf, wenn funcall,
;;; apply etc. auf Funktionsposition in einem funcall, apply etc. standen.
;;;
;;; Revision 1.60  1993/07/26  14:34:12  wg
;;; Lokale Funktionen in Methoden fuer CMU global definiert.
;;;
;;; Revision 1.59  1993/07/19  12:40:09  uho
;;; in 'p3-app (defined-fun)' Declare-Ignore-Direktive fuer 'args' eingefuegt.
;;;
;;; Revision 1.58  1993/06/28  17:22:59  hk
;;; Ueberpruefung der Parameteranzahl jetzt in Pass1,
;;; p3-syms-in-const spezialisiert nicht mehr ueber T, sondern simple-literal,
;;; kein :forward mehr als Wert von named-const, kein Abbruch bei named-const.
;;;
;;; Revision 1.57  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.56  1993/05/22  10:31:10  ft
;;; Erweiterung um das Traversieren von Slot-Init-Funktionen.
;;;
;;; Revision 1.55  1993/05/04  08:28:41  hk
;;; Revision Keyword korrigiert.
;;;
;;; Revision 1.54  1993/04/22  11:21:50  hk
;;; Bearbeitung von (?toplevel-forms *module*) eingebaut.
;;;
;;; Revision 1.53  1993/03/24  13:41:53  hk
;;; Symbole 'jump, 'closure, 'downfun in Slots heissen jetzt :jump,
;;; :closure und :downfun.
;;;
;;; Revision 1.52  1993/03/22  17:33:27  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.51  1993/03/12  09:54:30  ft
;;; Neue Methode fuer p3-form auf Klassen.
;;;
;;; Revision 1.50  1993/02/16  16:27:45  hk
;;; Revision Keyword eingefuegt, apply und values mit clicc-lisp::
;;; gekennzeichnet, (search-and-delete-unused-objects) entfernt.
;;;
;;; Revision 1.49  1993/02/11  13:59:13  ft
;;; Aufruf von search-and-delete-unused-objects in p3-end eingebaut.
;;;
;;; Revision 1.48  1993/01/14  14:15:55  hk
;;; falsche Ignore Deklaration in (p3-app foreign-fun t t) entfernt.
;;;
;;; Revision 1.47  1993/01/12  13:23:29  pm
;;; p3-app fuer Foreign Functions
;;;
;;; Revision 1.46  1993/01/07  10:00:20  hk
;;; Fehler mit special-sys-fun behoben.
;;;
;;; Revision 1.45  1993/01/07  08:28:08  hk
;;; Fehler in macrolet von p3-special-funs behoben.
;;;
;;; Revision 1.44  1993/01/06  13:03:40  hk
;;; Funktionen {p1,p2,p3,cg}-special-funs vereinheitlicht.
;;;
;;; Revision 1.43  1992/12/02  13:35:48  hk
;;; error durch internal-error ersetzt.
;;;
;;; Revision 1.42  1992/11/22  17:45:33  kl
;;; internal-error verwendet.
;;;
;;; Revision 1.41  1992/09/29  21:09:57  hk
;;; Message nach clcmain.
;;;
;;; Revision 1.40  1992/09/29  13:02:57  hk
;;; catch 'clicc-error entfernt.
;;;
;;; Revision 1.39  1992/09/26  15:06:42  hk
;;; Schreibfehler in (p3-form simple-literal): form -> a-simple-literal.
;;;
;;; Revision 1.38  1992/09/25  17:18:50  kl
;;; Umstellung auf die neue Repraesentation der einfachen Literale.
;;; Zusaetzlich p3-form und die Fehlermeldungen verbessert.
;;;
;;; Revision 1.37  1992/08/11  16:26:46  hk
;;; Schreibfehler.
;;;
;;; Revision 1.36  1992/08/10  16:49:16  hk
;;; Keine Sonderbehandlung mehr fuer funcall. MV Info wird in app und
;;; mv-lambda eingetragen, p3-combine-mv-spec neu, labels aus Methoden
;;; herausgezogen fuer CMU-Lisp.
;;;
;;; Revision 1.35  1992/08/06  09:04:38  hk
;;; Schreibfehler.
;;;
;;; Revision 1.34  1992/08/05  15:32:15  hk
;;; Schreibfehler.
;;;
;;; Revision 1.33  1992/08/05  15:27:57  hk
;;; Schreibfehler.
;;;
;;; Revision 1.32  1992/08/05  15:19:14  hk
;;; In p3-values nicht p3-args, sondern p3-down-args.
;;;
;;; Revision 1.31  1992/08/05  15:10:45  hk
;;; Ignore Decl. fuer app gestrichen.
;;;
;;; Revision 1.30  1992/08/05  15:02:06  hk
;;; ?downfun-list in Applikationen von Continuations auf nil.
;;;
;;; Revision 1.29  1992/08/05  14:58:11  hk
;;; ?downfun-list in Applikationen mit berechneter Funktion auf nil.
;;;
;;; Revision 1.28  1992/08/05  14:48:10  hk
;;; Schreibfehler.
;;;
;;; Revision 1.27  1992/08/05  14:44:50  hk
;;; Downward-funargs werden jetzt auch in Aufrufen benutzerdefinierter
;;; Funktionen gesucht.
;;;
;;; Revision 1.26  1992/08/05  13:08:27  hk
;;; Syntaktische Aenderungen.
;;;
;;; Revision 1.25  1992/08/04  18:25:54  hk
;;; Loesen der Gleichungssysteme zum Bestimmen von globalisierbaren lokalen
;;; Funktionen, Closures, Maxlevel und Multiplen Werten zum Teil vollstaendig
;;; ueberarbeitet.
;;;
;;; Revision 1.24  1992/07/29  15:04:40  hk
;;; Berechnung von num-fee-lex-vars nach cg-fun.
;;;
;;; Revision 1.23  1992/07/29  10:22:19  hk
;;; Nicht benutzte Symbole werden zunmaechst nicht wegoptimiert, um
;;; Konsistenz zwischen Modulen zu wahren.
;;;
;;; Revision 1.22  1992/07/23  08:31:41  hk
;;; Schreibfehler.
;;;
;;; Revision 1.21  1992/07/23  08:28:38  hk
;;; Schreibfehler.
;;;
;;; Revision 1.20  1992/07/22  17:25:55  hk
;;; get-global-operator --> get-global-fun.
;;;
;;; Revision 1.19  1992/07/22  12:26:28  hk
;;; (p3-form structured-literal): Fehler beim Aufruf von p3-syms-in-const.
;;;
;;; Revision 1.18  1992/07/22  12:07:32  hk
;;; Globalisieren von Fkt., propagieren von Closures und Bestimmung von
;;; max-level neu kodiert und an das Ende von p3-defun plaziert.
;;;
;;; Revision 1.17  1992/07/22  07:22:29  hk
;;; Keywords in Paramterlisten als used markiert.
;;;
;;; Revision 1.16  1992/07/21  10:41:38  hk
;;; In p3-analyse-mv-spec wurden lokale Funktionen mit Level > 1 bisher
;;; nicht bearbeitet.
;;;
;;; Revision 1.15  1992/07/20  16:26:12  hk
;;; Schreibfehler.
;;;
;;; Revision 1.14  1992/07/09  17:02:35  hk
;;; Referenzen auf Symbole werden nun auch in dyn. Variablen, strukturierten
;;; Konstanten und Werten von Symbolen und benannten Konstanten gefunden.
;;;
;;; Revision 1.13  1992/07/08  13:00:15  hk
;;; Schreibfehler.
;;;
;;; Revision 1.12  1992/07/08  12:43:04  hk
;;; Nicht referenzierte nicht exportierte Symbole eliminieren.
;;;
;;; Revision 1.11  1992/07/07  09:00:58  hk
;;; Bearbeitung der Sym auf Pass1 und CodeGen verlagert.
;;;
;;; Revision 1.10  1992/07/06  09:36:08  hk
;;; In let* Level von Variablen vor der Bearbeitung der Initforms setzen.
;;;
;;; Revision 1.9  1992/07/02  14:58:14  hk
;;; Referenzen von named-const mit unbekanntem Wert werden bearbeitet.
;;;
;;; Revision 1.8  1992/06/11  14:59:09  hk
;;; Schreibfehler.
;;;
;;; Revision 1.7  1992/06/11  14:55:10  hk
;;; Fehler in p3-params, nur der Seiteneffekt war relevant.
;;;
;;; Revision 1.6  1992/06/10  16:01:50  hk
;;; Fehler in p3-apply/funcall beseitigt.
;;;
;;; Revision 1.5  1992/06/04  14:41:47  hk
;;; Bearbeitung von downward-args fuer imported-fun korrigiert.
;;;
;;; Revision 1.4  1992/06/04  13:51:08  hk
;;; *CONSTANTS* in *CONST-LIST* umbenannt.
;;;
;;; Revision 1.3  1992/06/04  12:14:56  hk
;;; Fehler bei der Ueberpruefung der Anzahl der Arg. des Aufrufs einer Contin.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;;               1992/02/05            hk
;;; Strukturierte Konstanten werden in fun eingetragen.
;;;
;;;               1992/01/30            hk
;;; Bug Fixed: Bei der Uebersetzung von Argumenten von Multiple-Value-Calls
;;; ist MV-Information relevant.
;;;
;;;               1992/01/29            hk
;;; Bearbeitung von GLOBAL-CLOSURES erfolgt vollstaendig waehrend der
;;; Codegenerierung.
;;;
;;;               1992/01/17            hk
;;; Bearbeitung von *LOCAL-FUNS* geaendert.  Neue Komponente local-funs in fun
;;; eingefuegt.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(defvar *LEVEL*)                        ; Aktuelles statisches Niveau
(defvar *LOCAL-FUNS*)                   ; Direkt im Rumpf definierte lokale Fkt.
(defvar *CONST-LIST*)                   ; In einer globalen Fkt. enthaltene
                                        ; strukturierte Konstanten
(defvar *DOWNWARD-FUNARG* NIL)          ; Das funktionale Argument wird an
                                        ; dieser Stelle nur als
                                        ; 'downward function' verwendet.
(defvar *DOWNFUNS*)                     ; Queue der Downward-Funargs
(defvar *MV-SPEC*    NIL)               ; Anzahl der Rueckgabewerte
                                        ; einer Funktion
(defvar *MV-CALLS*   ())                ; Liste der Funktionen, die fuer die
                                        ; Ermittlung von mv-spec benoetigt
                                        ; werden
(defvar *MV-POSITION* NIL)              ; Wird die aktuelle Form fuer die
                                        ; Ermittlung von mv-spec benoetigt ?
(defvar *FN-ON-MV-POS* NIL)             ; Steht die Funktionsanwendung auf
                                        ; Ergebnispos. ?
(defvar *CONTINUATIONS*)                ; Liste aller Continuations des Prog.
(defvar *MV-LAMBDAS*)                   ; Liste aller Vorkommen von mv-lambda

(defconstant CONST_AS_FN "A constant is used as a functional argument.")

;;------------------------------------------------------------------------------
;; Funktionen, die in Pass3 in besonderer Weise analysiert werden
;;------------------------------------------------------------------------------
(p0-special-funs
 (?pass3 "P3")
 rt::progv
 rt::catch
 rt::unwind-protect
 L::apply
 L::values)

;;------------------------------------------------------------------------------
;; PASS_3
;;------------------------------------------------------------------------------
(defun pass_3 ()
  (dolist (fun (?fun-list *module*))
    (setf (?mv-called-by fun) ()))
  (setq *CONTINUATIONS* ())
  (setq *MV-LAMBDAS* ())
  (mapc #'p3-defun (?fun-list *module*))
  (p3-defun (?toplevel-forms *module*))
  (p3-analyse-mv-spec))

;;------------------------------------------------------------------------------
(defun p3-defun (fun)

  ;; Globale Funktionen haben das statische Niveau 0.
  ;;-------------------------------------------------
  (let ((*LEVEL* 0)
        (*CONST-LIST* (empty-queue)))

    (p3-fun-def fun)
    (setf (?const-list fun) (queue2list *CONST-LIST*)))

  ;; Bestimmen, welche lokalen Funktionen NICHT globalisiert werden
  ;; koennen.  Eine lokale Funktion kann nicht globalisiert werden, wenn
  ;; sie freie lexikalisch gebundene Variablen enthaelt oder wenn eine
  ;; ihrer freien lokalen Funktionen nicht globalisiert werden kann.  Es
  ;; wird zunaechst davon ausgegangen, dass alle lokalen Funktionen
  ;; globalisierbar sind.  Wenn eine Funktion freie lexikalisch gebundene
  ;; Variablen enthaelt, wird sie als nicht globalisierbar gekennzeichnet.
  ;; Wenn eine Funktion f als nicht globalisierbar gekennzeichnet wurde,
  ;; muessen alle lokalen Funktionen, in denen f direkt oder indirekt frei
  ;; vorkommt, auch als nicht globalisierbar gekennzeichnet werden.
  ;;---------------------------------------------------------------------
  (process-local-funs fun
                      #'(lambda (fun)
                          (when (and (?free-lex-vars fun)
                                     (?as-global-fun fun))
                            (labels ((propagate (fun)
                                       (setf (?as-global-fun fun) nil)
                                       (dolist (fun (?free-in fun))
                                         (when (?as-global-fun fun)
                                           (propagate fun)))))
                              (propagate fun)))))

  ;; Lokale Funktionen, die als funktionales Objekt verwendet werden und
  ;; von denen nicht gezeigt werden konnte, dass sie nur 'downward'
  ;; verwendet werden, sind als Closure gekennzeichnet.  Die
  ;; Closure-Eigenschaft muss an alle nicht globalisierten lokalen
  ;; Funktionen, die in Closures frei vorkommen, propagiert werden.  Die
  ;; lokal freien lex. Variablen von Closures kennzeichnen.
  ;;---------------------------------------------------------------------
  (process-local-funs fun
                      #'(lambda (fun)
                          (when (and (eq (?closure fun) :CLOSURE)
                                     (not (?as-global-fun fun)))
                            (labels ((propagate (fun)
                                       (dolist (var (?free-lex-vars fun))
                                         (setf (?closure var) t))
                                       (dolist (fun (?free-local-funs fun))
                                         (unless
                                             (or (eq (?closure fun)
                                                     :CLOSURE)
                                                 (?as-global-fun fun))
                                           (setf (?closure fun) :CLOSURE)
                                           (propagate fun)))))
                              (propagate fun)))))

  ;; Ermitteln der max. statischen Aufruftiefe von Funktionen.  Die
  ;; Berechnung erfolgt 'depth first', d.h. der Wert fuer eine Funktion
  ;; haengt von den Werten der in dieser Funktion lokal definierten
  ;; Funktionen ab. Bei lokalen Funktionen haengt max-level zudem noch von
  ;; den auf gleichem Niveau aufgerufenen Funktionen ab.  Hier wird ein
  ;; 'free-in' benutzt, das eine Funktion f auch dann angibt, wenn in ihr
  ;; eine Funktion g nur indirekt frei vorkommt. Es wuerde reichen, nur
  ;; die direkte Relation zu betrachten, die indirekte Relation schadet
  ;; aber nicht.
  ;;---------------------------------------------------------
  (labels ((setf-deeper-max-level (fun)

             ;; Das Maximum der max-level aus DEEPER-LEVEL-CALLS berechnen.
             ;; Funktionen, die kein 'display' benoetigen, werden dabei
             ;; nicht beruecksichtigt.
             ;;----------------------------------------------------------
             (setf (?max-level fun)
                   (reduce #'(lambda (x deeper-level)
                               (if (needs-display deeper-level)
                                   (max x (1+ (?max-level deeper-level)))
                                   x))
                           (?deeper-level-calls fun)
                           :initial-value 0)))

           (process (funs)
             (dolist (fun funs)
               (process (?local-funs fun)) ; depth first !
               (setf-deeper-max-level fun))
             (let ((sorted (sort (collect-if #'needs-display funs)
                                 #'>
                                 :key #'?max-level)))
               (dolist (fun sorted)
                 (dolist (free-in (?free-in fun))
                   (when (and (= (?level fun) (?level free-in))
                              (> (?max-level fun) (?max-level free-in)))
                     (setf (?max-level free-in) (?max-level fun)))))))

           (needs-display (local-fun)
             (not (or (eq (?closure local-fun) :CLOSURE)
                      (?as-global-fun local-fun)))))

    ;; lokale Funktionen bearbeiten
    ;;-----------------------------
    (process (?local-funs fun))

    ;; globale Funktionen bearbeiten
    ;;------------------------------
    (setf-deeper-max-level fun)))

;;------------------------------------------------------------------------------
;; Die Funktion f wird auf alle lokalen Funktionen von fun angewendet.
;; Es wird zugesichert, dass vor der Bearbeitung einer Funktion g alle in g
;; lokal definierten Funktionen bearbeitet wurden.
;;------------------------------------------------------------------------------
(defun process-local-funs (fun f)
  (labels ((process (funs)
             (dolist (fun funs)
               (process (?local-funs fun))
               (funcall f fun))))
    (process (?local-funs fun))))

;;------------------------------------------------------------------------------
(defun p3-fun-def (fun)
  (let ((*DOWNWARD-FUNARG* nil)
        (*CURRENT-FUN* fun)
        (*LOCAL-FUNS* ()))

    (setf (?deeper-level-calls fun) ())

    (p3-params (?params fun))

    (let ((*MV-POSITION*  t)
          (*FN-ON-MV-POS* nil)
          (*MV-SPEC*      nil)
          (*MV-CALLS*     ()))
      (p3-form (?body fun))

      (setf (?mv-spec fun) *MV-SPEC*)
      (setf (?mv-calls fun) *MV-CALLS*)

      (dolist (fun-which-is-called *MV-CALLS*)
        (pushnew fun (?mv-called-by fun-which-is-called))))

    (setf (?local-funs fun) *LOCAL-FUNS*)

    ;; Propagieren der FREIEN lokalen Funktionen und lexikalischen Variablen.
    ;;-----------------------------------------------------------------------
    (dolist (local-fun *LOCAL-FUNS*)
      (mapc #'p3-check-if-free-local-fun (?free-local-funs local-fun))
      (mapc #'p3-check-if-free-lex-var   (?free-lex-vars   local-fun)))))

;;------------------------------------------------------------------------------
(defun p3-params (params)
  (p3-set-var-level (?all-vars params))
  (dolist (opt (?opt-list params))
    (p3-arg (?init opt)))
  (dolist (key (?key-list params))
    (p3-arg (?init key))))

;;------------------------------------------------------------------------------
;; Funktionsaufruf
;;------------------------------------------------------------------------------
(defmethod p3-form ((app app))
  (setf (?mv-used app) *MV-POSITION*)
  (p3-app (?form app) (?arg-list app) app))

;;------------------------------------------------------------------------------
(defmethod p3-app ((fun defined-fun) args app)
  (declare (ignore args))

  ;; Merken, wenn der Aufruf fuer die Ermittlung von MV-SPEC benoetigt wird.
  ;;------------------------------------------------------------------------
  (when *MV-POSITION*
    (pushnew fun *MV-CALLS*))
  (p3-set-fn-pos-mv-spec t)

  (when (local-fun-p fun)

    ;; Handelt es sich um ein freies Vorkommen der lokalen Funktion ?
    ;;---------------------------------------------------------------
    (p3-check-if-free-local-fun fun)

    (p3-check-level-of-call fun))

  (p3-down-args app))

;;------------------------------------------------------------------------------
(defmethod p3-app ((fun special-sys-fun) args app)
  (if (?pass3 fun)
      (funcall (?pass3 fun) args app)
      (call-next-method)))

;;------------------------------------------------------------------------------
(defmethod p3-app ((fun imported-fun) args app)
  (declare (ignore args))
  (p3-set-mv-spec-if-necessary (?mv-spec fun))
  (p3-set-fn-pos-mv-spec t)
  (p3-down-args app))

;;------------------------------------------------------------------------------
;; Aufruf einer Foreign-Function
;;------------------------------------------------------------------------------
(defmethod p3-app ((fun foreign-fun) args app)
  (p3-set-mv-spec-if-necessary 1) ;oder nil fuer ffi:void
  (p3-set-fn-pos-mv-spec t)
  (p3-args args)
  (setf (?downfun-list app) ()))


;;------------------------------------------------------------------------------
;; Aufruf einer berechneten Funktion
;;------------------------------------------------------------------------------
(defmethod p3-app ((fun form) args app)
  (p3-set-fn-pos-mv-spec t)
  (let ((*MV-POSITION*  NIL)
        (*FN-ON-MV-POS* *MV-POSITION*))
    (p3-form fun))
  (p3-args args)
  (setf (?downfun-list app) ()))

;;------------------------------------------------------------------------------
;; funktionales Argument
;;------------------------------------------------------------------------------
(defmethod p3-form ((fun global-fun))
  (setf (?closure fun) T)

  (p3-set-mv-spec-if-necessary 1)       ; liefert EINEN Wert
  (when *FN-ON-MV-POS* (pushnew fun *MV-CALLS*)))

;;------------------------------------------------------------------------------
(defmethod p3-form ((fun imported-fun))
  (setf (?closure fun) T)

  (p3-set-mv-spec-if-necessary 1)       ; liefert EINEN Wert
  (p3-set-fn-pos-mv-spec (?mv-spec fun)))

;;------------------------------------------------------------------------------
(defmethod p3-form ((fun local-fun))

  (p3-set-mv-spec-if-necessary 1)       ; liefert EINEN Wert
  (when *FN-ON-MV-POS* (pushnew fun *MV-CALLS*))

  (p3-check-if-free-local-fun fun)
  (p3-check-level-of-call fun)

  (cond

    ;; Wird das Ergebnis als Downward-Funarg benutzt ?
    ;;------------------------------------------------
    (*DOWNWARD-FUNARG*

     ;; Falls von der lokalen Funktion noch keine Closure gebildet
     ;; wurde, Funktion als DOWNFUN kennzeichnen.
     ;;------------------------------------------
     (when (null (?closure fun))
       (setf (?closure fun) :DOWNFUN))

     ;; Fun in die Funktionsapplikation eintragen.
     ;;------------------------------------------------
     (add-q fun *DOWNFUNS*))

    ;; Falls es kein Downward-Funarg ist, als CLOSURE kennzeichnen
    ;;------------------------------------------------------------
    (T (setf (?closure fun) :CLOSURE))))

;;------------------------------------------------------------------------------
;; progn
;;------------------------------------------------------------------------------
(defmethod p3-form ((form progn-form))
  (mapc-progn-form-list (?form-list form) #'p3-arg #'p3-form))

;;------------------------------------------------------------------------------
;; Literale, also (), numbers, characters, symbole
;;------------------------------------------------------------------------------
(defmethod p3-form ((an-literal literal))
  (p3-atom an-literal))

;;------------------------------------------------------------------------------
;; Strukturierte Literale
;;------------------------------------------------------------------------------
(defmethod p3-form ((obj structured-literal))
  (call-next-method)
  (add-q obj *CONST-LIST*))

;;------------------------------------------------------------------------------
;; Klasse
;;------------------------------------------------------------------------------
(defmethod p3-form ((form class-def))
  (dolist (slot (?slot-descr-list form)) 
    (when (global-fun-p (?initform slot)) (p3-form (?initform slot))))
  (p3-atom form))

;;------------------------------------------------------------------------------
(defun p3-atom (form)
  (declare (ignore form))
  (p3-set-mv-spec-if-necessary 1)
  (when *FN-ON-MV-POS* (clicc-warning CONST_AS_FN)))

;;------------------------------------------------------------------------------
;; let*
;;------------------------------------------------------------------------------
(defmethod p3-form ((form let*-form))
  (p3-set-var-level (?var-list form))
  (p3-args (?init-list form))

  ;; Da der Ausdruck eine neue Umgebung anlegt,
  ;; kann das Ergebnis kein DOWNWARD-FUNARG sein.
  ;;---------------------------------------------
  (let ((*DOWNWARD-FUNARG* NIL))
    (p3-form (?body form))))

;;------------------------------------------------------------------------------
;; mv-lambda
;;------------------------------------------------------------------------------
(defmethod p3-form ((form mv-lambda))
  (let ((*MV-POSITION*     t)
        (*FN-ON-MV-POS*    nil)
        (*MV-SPEC*         nil)
        (*MV-CALLS*        ())
        (*DOWNWARD-FUNARG* NIL))
    (p3-form (?arg form))
    (setf (?mv-spec form) *MV-SPEC*)
    (setf (?mv-calls form) *MV-CALLS*)
    (push form *MV-LAMBDAS*))

  (p3-params (?params form))

  ;; Da der Ausdruck eine neue Umgebung anlegt,
  ;; kann das Ergebnis kein DOWNWARD-FUNARG sein.
  ;;---------------------------------------------
  (let ((*DOWNWARD-FUNARG* NIL))
    (p3-form (?body form))))

;;------------------------------------------------------------------------------
;; Variablen Referenz
;;------------------------------------------------------------------------------
(defmethod p3-form ((form var-ref))
  (p3-set-mv-spec-if-necessary 1)
  (p3-set-fn-pos-mv-spec t)

  (setq form (?var form))
  (if (local-static-p form)
    (p3-check-if-free-lex-var form)))

;;------------------------------------------------------------------------------
;; Referenz einer Named Constant
;;------------------------------------------------------------------------------
(defmethod p3-form ((form named-const))
  (p3-set-mv-spec-if-necessary 1)
  (p3-set-fn-pos-mv-spec t))

;;------------------------------------------------------------------------------
;; setq
;;------------------------------------------------------------------------------
(defmethod p3-form ((form setq-form))
  (p3-set-mv-spec-if-necessary 1)       ; liefert EINEN Wert

  (let ((var (?var (?location form))))
    (when (local-static-p var)
      (p3-check-if-free-lex-var var))

    ;; *FN-ON-MV-POS* wird ggf. von (?form form) bestimmt, also
    ;; *FN-ON-MV-POS* nicht an nil binden !
    ;;------------------------------------------------------------
    (let ((*MV-POSITION* NIL)

          ;; 'form' darf nicht mehr als DOWNWARD-FUNARG
          ;; gekennzeichnet werden, da durch die Zuweisung
          ;; die Closure aus der aktuellen Umgebung
          ;; herausgereicht werden koennte.
          ;;-------------------------------
          (*DOWNWARD-FUNARG* NIL))

      (p3-form (?form form)))))

;;------------------------------------------------------------------------------
;; labels
;;------------------------------------------------------------------------------
(defmethod p3-form ((form labels-form))
  (let ((fun-list (?fun-list form))
         (body (?body form)))
    
    (dolist (fun fun-list)
      (setf (?as-global-fun fun) t)
      (setf (?free-local-funs fun) ())
      (setf (?free-lex-vars   fun) ())
      (setf (?mv-called-by fun) ())
      (setf (?free-in fun) ()))
    
    (setq *LOCAL-FUNS* (append fun-list *LOCAL-FUNS*))
    (let ((*LEVEL* (1+ *LEVEL*)))

      ;; Jeder lokalen Funktion wird ein statisches Niveau von N + 1 zugeordnet.
      ;;------------------------------------------------------------------------
      (mapc #'(lambda (fun)
                (setf (?level fun) *LEVEL*))
            fun-list)

      ;; Analysieren der lokalen Funktionen
      ;;-----------------------------------
      (mapc #'p3-fun-def fun-list))

    ;; Das Ergebnis von labels wird aus dem Bindungsbereich der hierdrin
    ;; definierten Funktionen herausgereicht. Hier waere eine weitergehende
    ;; Analyse notwendig, um festzustellen, dass ein Abschluss, der herausge-
    ;; reicht wird, trotzdem ein DOWNWARD-FUNARG ist.
    ;;-----------------------------------------------
    (let ((*DOWNWARD-FUNARG* NIL))
      (p3-form body))))

;;------------------------------------------------------------------------------
;; if
;;------------------------------------------------------------------------------
(defmethod p3-form ((form if-form))
  (p3-arg  (?pred form))
  (p3-form (?then form))
  (p3-form (?else form)))

;;------------------------------------------------------------------------------
;; let/cc
;;------------------------------------------------------------------------------
(defmethod p3-form ((form let/cc-form))
  (let ((cont (?cont form)))

    (push cont *CONTINUATIONS*)
    (setf (?level cont) *LEVEL*)
    (setf (?only-local cont) t)
    (setf (?mv-position  cont) *MV-POSITION*)
    (setf (?fn-on-mv-pos cont) *FN-ON-MV-POS*)
    (setf (?mv-spec cont) nil)
    (setf (?mv-calls cont) ())

    (let ((*DOWNWARD-FUNARG* NIL)       ; Anmerkung siehe LABELS/FLET
          (*MV-SPEC*         nil)
          (*MV-CALLS*         ()))
      (p3-form (?body form))

      ;; Die Information ueber den Rumpf mit den kombinierten Informationen
      ;; der Anwendungen der Continuation kombinieren.
      ;;-----------------------------------------------
      (setf (?mv-spec cont) (p3-combine-mv-spec (?mv-spec cont) *MV-SPEC*))
      (setf (?mv-calls cont) (union *MV-CALLS* (?mv-calls cont))))

    (p3-set-mv-spec-if-necessary (?mv-spec cont))
    (p3-set-fn-pos-mv-spec (?mv-spec cont))
    (setq *MV-CALLS* (union *MV-CALLS* (?mv-calls cont)))))

;;------------------------------------------------------------------------------
;; Angewandtes Vorkommen einer Continuation
;;------------------------------------------------------------------------------
(defmethod p3-form ((form cont))
  (setf (?mv-spec form) t)
  (p3-atom form))

;;------------------------------------------------------------------------------
;; Aufruf einer Continuation
;;------------------------------------------------------------------------------
(defmethod p3-app ((cont cont) args app)
  (unless (eql (length args) 1)
    (internal-error 'p3-main
                    "continuation should be called with exactly one argument"))

  ;; Sprung aus einer Funktion heraus ?
  ;;-----------------------------------
  (when (> *LEVEL* (?level cont))
    (setf (?only-local cont) nil)

    ;; Bei Closurebildung muss die statisch gebundene Cont. beachtet werden
    ;;---------------------------------------------------------------------
    (p3-check-if-free-lex-var cont))

  ;; Die MV-Spezifikation des Sprungs ist relevant fuer die MV-Spezifikation
  ;; des Let/cc.
  ;;---------------------------------
  (let ((*DOWNWARD-FUNARG* nil)         ; siehe auch LABELS/FLET
        (*MV-POSITION*     (?mv-position  cont))
        (*FN-ON-MV-POS*    (?fn-on-mv-pos cont))
        (*MV-SPEC*         nil)
        (*MV-CALLS*        ()))
    (p3-form (first args))
    (setf (?downfun-list app) ())

    ;; Die Continuation wurde evtl. schon an anderer Stelle angewendet.
    ;; Deshalb die hier gewonnene Information mit der schon vorhandenen
    ;; kombinieren.
    ;;-----------------------------------------------------------------
    (setf (?mv-spec cont) (p3-combine-mv-spec (?mv-spec cont) *MV-SPEC*))
    (setf (?mv-calls cont) (union (?mv-calls cont) *MV-CALLS*)))

  ;; Sprung kann auf Ergebnisposition einer lokalen Funktion stehen.
  ;; In diesem Fall wird als MV-Spezifikation :JUMP eingetragen.
  ;;------------------------------------------------------------
  (p3-set-mv-spec-if-necessary :JUMP))

;;------------------------------------------------------------------------------
;; tagbody
;;------------------------------------------------------------------------------
(defmethod p3-form ((form tagbody-form))
  (when *FN-ON-MV-POS*
    (clicc-warning "TAGBODY is used as a functional argument."))

  (setf (?level form) *LEVEL*)
  (p3-arg (?first-form form))
  (mapc-progn-form-list (?tagged-form-list form)
                        #'(lambda (tagged-form) (p3-arg (?form tagged-form)))
                        #'(lambda (tagged-form) (p3-form (?form tagged-form)))))

;;------------------------------------------------------------------------------
;; go (d.h. angewandtes Vorkommen von tagged-form)
;;------------------------------------------------------------------------------
(defmethod p3-form ((form tagged-form))

  ;; GO kann auf Ergebnisposition einer lokalen Funktion stehen.
  ;; In diesem Fall wird als MV-Spezifikation :JUMP eingetragen.
  ;;------------------------------------------------------------
  (p3-set-mv-spec-if-necessary :JUMP)
  (p3-set-fn-pos-mv-spec :JUMP))

;;------------------------------------------------------------------------------
;; Spezielle Optimierungen von Systemfunktionen
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; PROGV-INTERNAL symbols values body
;;------------------------------------------------------------------------------
(defun p3-progv (args app)
  (let ((symbols (pop args))
        (values (pop args))
        (body (pop args)))

    (p3-arg symbols)
    (p3-arg values)
    (p3-set-fn-pos-mv-spec t)
    (let ((*DOWNWARD-FUNARG* T)
          (*DOWNFUNS* (empty-queue))
          (*MV-POSITION*     NIL)
          (*FN-ON-MV-POS* *MV-POSITION*))
      (p3-form body)
      (setf (?downfun-list app) (queue2list *DOWNFUNS*)))))

;;------------------------------------------------------------------------------
;; CATCH-INTERNAL tag body
;;------------------------------------------------------------------------------
(defun p3-catch (args app)

  ;; Da das CATCH durch ein THROW dynamisch verlassen werden kann,
  ;; ist die MV-Spezifikation unbekannt.
  ;;-----------------------------------------
  (p3-set-mv-spec-if-necessary T)
  (p3-set-fn-pos-mv-spec t)

  (let ((tag (pop args))
        (funarg (pop args)))

    (p3-arg tag)
    (let ((*DOWNWARD-FUNARG*  T)
          (*DOWNFUNS* (empty-queue))
          (*MV-POSITION*     NIL)       ; keine MV, sondern 1 Funktion
          (*FN-ON-MV-POS*    NIL))
      (p3-form funarg)
      (setf (?downfun-list app) (queue2list *DOWNFUNS*)))))

;;------------------------------------------------------------------------------
;; UNWIND-PROTECT-INTERNAL protected-form cleanup-form
;;------------------------------------------------------------------------------
(defun p3-unwind-protect (args app)
  (let ((protected-form (pop args))
        (cleanup-form (pop args))
        (*DOWNWARD-FUNARG*  T)
        (*DOWNFUNS* (empty-queue)))     ; Liste der Downward Funargs

    ;; Die MV-Spezifikation fuer UNWIND-PROTECT ergibt sich
    ;; aus der MV-Spezifikation der Protected-Form.
    ;;---------------------------------------------
    (p3-set-fn-pos-mv-spec t)
    (let ((*MV-POSITION*  NIL)
          (*FN-ON-MV-POS* *MV-POSITION*))
      (p3-form protected-form))
    (let ((*MV-POSITION*  NIL)
          (*FN-ON-MV-POS* NIL))
      (p3-form cleanup-form))           ; nicht p3-arg, da evtl. downfun
    (setf (?downfun-list app) (queue2list *DOWNFUNS*))))

;;------------------------------------------------------------------------------
;; APPLY fn args
;;------------------------------------------------------------------------------
(defun p3-apply (args app)
  (p3-set-fn-pos-mv-spec t)
  (let ((*DOWNWARD-FUNARG*  T)
        (*DOWNFUNS* (empty-queue))
        (*MV-POSITION*  NIL)
        (*FN-ON-MV-POS* *MV-POSITION*))
    (p3-form (first args))
    (setf (?downfun-list app) (queue2list *DOWNFUNS*)))
  (p3-args (rest args)))

;;------------------------------------------------------------------------------
;; VALUES args
;;------------------------------------------------------------------------------
(defun p3-values (args app)
  (p3-set-mv-spec-if-necessary (length args))
  (p3-down-args app))


;;------------------------------------------------------------------------------
;; Ermittelt die kleinste obere Schranke zweier Elemente aus dem unten
;; angegbenen Verband.
;;
;;      t     = Anzahl der Werte kann erst zur Laufzeit bestimmt werden
;;   /    \
;;  0 1 2 ... = Anzahl der Werte ist n
;;   \    /
;;    JUMP    = Ausdruck wird durch einen Sprung verlassen oder terminiert nicht
;;      |
;;     nil    = Anzahl der Werte ist noch nicht bestimmt
;;
;;------------------------------------------------------------------------------
(defun p3-combine-mv-spec (old-mv-spec new-mv-spec)
  (cond
    ((eq old-mv-spec nil) new-mv-spec)
    ((eq new-mv-spec nil) old-mv-spec)
    ((eq old-mv-spec :JUMP) new-mv-spec)
    ((eq new-mv-spec :JUMP) old-mv-spec)
    ((eql old-mv-spec new-mv-spec) old-mv-spec)
    (t t)))
    
;;------------------------------------------------------------------------------
;; Setzen die MV-Spezifikation (falls noetig)
;;------------------------------------------------------------------------------
(defun p3-set-fn-pos-mv-spec (mv-spec)
  (when *FN-ON-MV-POS*
    (setq *MV-SPEC* (p3-combine-mv-spec *MV-SPEC* mv-spec))))

;;------------------------------------------------------------------------------
(defun p3-set-mv-spec-if-necessary (mv-spec)
  (when *MV-POSITION*
    (setq *MV-SPEC* (p3-combine-mv-spec *MV-SPEC* mv-spec))))

;;------------------------------------------------------------------------------
;; Analysiert die multiplen Werte der definierten Funktionen
;;------------------------------------------------------------------------------
(defun p3-analyse-mv-spec ()
  (let ((all-funs (?all-global-funs *module*)))

    (labels ((add-local-funs (funs)
               (dolist (fun funs)
                 (push fun all-funs)
                 (add-local-funs (?local-funs fun)))))
      (dolist (fun all-funs)
        (add-local-funs (?local-funs fun))))

    (labels ((combine-mv-calls (fun)
               (let ((mv-spec (?mv-spec fun)))
                 (dolist (called (?mv-calls fun))
                   (setq mv-spec
                         (p3-combine-mv-spec mv-spec (?mv-spec called))))
                 (setf (?mv-spec fun) mv-spec))))

      ;; Loesen des Gleichungssystems fuer mv-spec
      ;;------------------------------------------
      (do ((work-set all-funs)
           fun
           old-mv-spec)
          ((null work-set))
        (setq fun (pop work-set))
        (setq old-mv-spec (?mv-spec fun))
        (unless (eql old-mv-spec (combine-mv-calls fun))
          (setq work-set (union work-set (?mv-called-by fun)))))

      ;; Konnte fuer eine Funktion keine MV-Spezifikation ermittelt werden ?
      ;;--------------------------------------------------------------------
      (let ((no-mv-attr-p nil))
        (dolist (fun all-funs)
          (when (null (?mv-spec fun))
            (unless no-mv-attr-p
              (setq no-mv-attr-p t)
              (terpri)
              (clicc-warning "No Multiple Value Attribute for"))
            (setf (?mv-spec fun) 1)
            (format t ";;; Function ~A~%" (?symbol fun)))))

      ;; Eintragen der MV-Spezifikationen in die Continuations
      ;;------------------------------------------------------
      (dolist (cont *CONTINUATIONS*)
        (combine-mv-calls cont))

      ;; Eintragen der MV-Spezifikationen in die mv-lambda Ausdruecke
      ;;-------------------------------------------------------------
      (dolist (form *MV-LAMBDAS*)
        (combine-mv-calls form)))))

;;------------------------------------------------------------------------------
;; Hilfsfunktionen
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Analysiert die Argumente eines Funktionsaufrufes.
;;------------------------------------------------------------------------------
(defun p3-arg (arg)
  (let ((*MV-POSITION*     NIL)         ; Argumente sind NICHT ergebnisrelevant.
        (*FN-ON-MV-POS*    NIL)
        (*DOWNWARD-FUNARG* NIL))        ; Argumente koennen hier nicht als
                                        ; DOWNWARD-FUNARG erkannt werden.
    (p3-form arg)))

;;------------------------------------------------------------------------------
(defun p3-args (args)
  (mapc #'p3-arg args))

;;------------------------------------------------------------------------------
;; Analysiert Argumente einer Applikation und traegt die Downward Funargs in
;; die downfun-list der Applikation ein.
;;------------------------------------------------------------------------------
(defun p3-down-args (app)
  (let* ((fun (?form app))
         (args (?arg-list app))
         (*DOWNFUNS* (empty-queue))
         (downward-args (?downward-args fun))
         (i 0))
    (dolist (arg args)
      (if (eql i (car downward-args))
          (let ((*MV-POSITION*  NIL)
                (*FN-ON-MV-POS* NIL)
                (*DOWNWARD-FUNARG*  T))
            (pop downward-args)
            (p3-form arg))
          (p3-arg arg))
      (incf i))
    (setf (?downfun-list app) (queue2list *DOWNFUNS*))))

;;------------------------------------------------------------------------------
;; Setzt bei statisch gebundenen Variablen das statische Niveau.
;;------------------------------------------------------------------------------
(defun p3-set-var-level (var-list)
  (mapc #'(lambda (var)
            (when (local-static-p var)
              (setf (?level var) *LEVEL*)))
        var-list))

;;------------------------------------------------------------------------------
;; Wenn 'var' frei in der momentan uebersetzten Funktion vorkommt, wird 'var'
;; unter 'free-lex-vars' vermerkt.
;;------------------------------------------------------------------------------
(defun p3-check-if-free-lex-var (var)
  (when (< (?level var) *LEVEL*)
    (pushnew var (?free-lex-vars *CURRENT-FUN*))))

;;------------------------------------------------------------------------------
;; Prueft, ob es sich um einen DEEPER-LEVEL-CALL handelt.
;; (ein Funktionsaufruf einer im Rumpf der aktuellen Funktion
;; lokal definierten Funktion)
;;------------------------------------------------------------------------------
(defun p3-check-level-of-call (fun)
  (when (> (?level fun) *LEVEL*)
      (pushnew fun (?deeper-level-calls *CURRENT-FUN*))))

;;------------------------------------------------------------------------------
;; Wenn 'local-fun' frei in der momentan uebersetzten Funktion vorkommt, wird
;; 'local-fun' unter 'free-local-funs' vermerkt.  In 'local-fun' wird die
;; aktuelle Funktion in der Annotation 'free-in' vermerkt.  Eine (lokale)
;; Funktion gilt als nicht frei in sich selbst.
;;------------------------------------------------------------------------------
(defun p3-check-if-free-local-fun (local-fun)
  (when (and (<= (?level local-fun) *LEVEL*)
             (not (eq local-fun *CURRENT-FUN*)))
    (pushnew local-fun (?free-local-funs *CURRENT-FUN*))
    (pushnew *CURRENT-FUN* (?free-in local-fun))))

;;------------------------------------------------------------------------------
(provide "p3main")
