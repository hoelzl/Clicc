;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Methoden zum Traversieren der Zwischensprache, die die 
;;;            Zwischensprachausdruecke analysieren, Typen inferieren 
;;;            und die entsprechenden Ausdruecke mit Typinformationen 
;;;            versehen.
;;;
;;; $Revision: 1.48 $
;;; $Log: tipass2.lisp,v $
;;; Revision 1.48  1994/05/16  12:03:06  hk
;;; Schreibfehler behoben
;;;
;;; Revision 1.47  1994/05/16  11:12:43  hk
;;; SC-mapcar vereinfacht. SC-mapcan korrigiert, der Resultattyp ist keine
;;; Liste von Funktionsresultaten wie bei mapcar sondern das
;;; Funktionsresultat oder NULL.
;;;
;;; Revision 1.46  1994/02/21  10:24:57  kl
;;; Bei Applikationen mit falscher Argumentanzahl kann es nun nicht mehr
;;; zum Abbruch der Typinferenz kommen.
;;;
;;; Revision 1.45  1994/01/26  14:44:30  ft
;;; Letzter Feinschliff an der eben gemachten Änderung.
;;;
;;; Revision 1.44  1994/01/26  13:37:43  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.43  1994/01/15  20:28:42  kl
;;; Destruktives Bearbeiten der Variablen *ti-workset* korrigiert.
;;;
;;; Revision 1.42  1993/12/09  10:35:16  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.41  1993/11/21  22:16:42  kl
;;; In den Typumgebungen tauchen jetzt keine doppelten Elemente mehr auf..
;;;
;;; Revision 1.40  1993/10/28  07:49:17  kl
;;; An Applikation werden jetzt zum Teil schaerfere Typen inferiert.
;;;
;;; Revision 1.39  1993/10/13  10:22:22  hk
;;; Methode (analyse-types T) entfernt.
;;;
;;; Revision 1.38  1993/10/12  19:53:35  kl
;;; Nun wird die Funktion not-destructive von Anouar benutzt.
;;;
;;; Revision 1.37  1993/10/09  17:55:30  atr
;;; Die Funktion has-destruktiv-effects an die neue Analyse angepasst.
;;; Jeztz werden auch Spr"unge als Seiteneffekt betrachtet, deswegen gibt
;;; es jetzt einen Effekt bei DATA-EFFECTS zu betrachten :jump.
;;;
;;; Revision 1.36  1993/10/08  22:37:27  kl
;;; Praezisere Behandlung von Seiteneffekten eingebaut.
;;;
;;; Revision 1.35  1993/10/08  15:57:57  kl
;;; Variablenreferenzen in setq-Ausdruecken erhalten jetzt den Typ des
;;; zugewiesenen Wertes.
;;;
;;; Revision 1.34  1993/10/04  14:13:13  hk
;;; Fehler in (defmethod analyse-types (tagbody-form)) behoben.
;;;
;;; Revision 1.33  1993/09/12  16:09:41  kl
;;; Bei den Stufen 0,1 und 2 werden globale Variablen mit TOP getypt.
;;; Dadurch werden diese Stufen sehr viel schneller.
;;;
;;; Revision 1.32  1993/09/12  15:21:39  kl
;;; Ruecksetzen dynamisch gebundener Variablen in let*-Ausdruecken korrigiert.
;;;
;;; Revision 1.31  1993/09/12  11:52:23  kl
;;; Typinferenz-Level 2 gestrichen.
;;;
;;; Revision 1.30  1993/09/04  14:13:13  kl
;;; Fixpunktiteration beschleunigt. In der Variablen *successor-workset*
;;; wird nun ein Teil der zu analysierenden Objekte vorgehalten. Dadurch
;;; muessen nicht mehr alle Mengenoperationen auf der evtl. sehr grossen
;;; *ti-workset* vorgenommen werden.
;;; Bei einem Level > 2 werden vor der eigentlichen Fixpunktiteration alle
;;; Elemente einmal vorweg analysiert. Das fuehrt waehrend der Iteration zu
;;; einer in der Regel schnelleren Abnahme der Anzahl noch zu analysierender
;;; Funktionen.
;;;
;;; Revision 1.29  1993/06/24  14:35:06  kl
;;; Verhalten bei destruktiven Seiteneffekten verbessert.
;;;
;;; Revision 1.28  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.27  1993/06/16  07:51:14  kl
;;; Bei Applikationen wird nun die Komponente data-effects beachtet.
;;;
;;; Revision 1.26  1993/06/10  10:32:53  kl
;;; Neuen ti-level eingefuehrt. Binden von Funktionsparameter vereinheitlicht.
;;;
;;; Revision 1.25  1993/06/07  10:09:52  kl
;;; Binden der aktuellen Parameter vereinheitlicht.
;;;
;;; Revision 1.24  1993/06/05  21:46:27  hk
;;; Bei mv-lambda suppliedp Parameter an bool-t gebunden.
;;;
;;; Revision 1.23  1993/05/27  13:41:01  kl
;;; Weil cont keine LZS-form ist, wurde eine besondere around-Methode
;;; der gen. Funktion analyse-type fuer Continuations noetig.
;;;
;;; Revision 1.22  1993/05/27  13:17:33  kl
;;; Typisierung der Continuations auf tomain.lisp abgestimmt.
;;;
;;; Revision 1.21  1993/05/23  15:59:45  kl
;;; Anpassungen an den neuen Typverband.
;;;
;;; Revision 1.20  1993/05/21  12:27:01  kl
;;; Fehler im Umgang mit Typumgebungen behoben.
;;;
;;; Revision 1.19  1993/05/18  16:16:04  kl
;;; Umstellung auf die neue Implementierung des Typverbands. Fehler in der
;;; Behandlung von tagbody-Konstrukten behoben. Analyse-Methode fuer
;;; foreign-funs eingefuehrt.
;;;
;;; Revision 1.18  1993/05/15  14:07:52  kl
;;; Behandlung der Initialisierungsausdruecke umgestellt.
;;;
;;; Revision 1.17  1993/05/09  16:57:27  kl
;;; Argumenttypen der importierten Funktionen koennen jetzt genutzt werden.
;;;
;;; Revision 1.16  1993/04/30  09:23:34  kl
;;; Hauptfunktion nach timain.lisp verlagert.
;;;
;;; Revision 1.15  1993/04/20  15:06:57  kl
;;; Es werden jetzt auch die Klassendefinitionen analysiert.
;;;
;;; Revision 1.14  1993/04/20  11:24:55  kl
;;; Fehler in der Seiteneffektanalyse werden besser aufgefangen.
;;; Typisierung der mapping-Funktionen und der Funktion coerce verbessert.
;;;
;;; Revision 1.13  1993/04/19  12:25:59  kl
;;; Die durch appfuns gelieferten Informationen werden jetzt besser genutzt.
;;;
;;; Revision 1.12  1993/04/15  08:29:12  kl
;;; nunion durch union ersetzt und appfuns benutzt.
;;;
;;; Revision 1.11  1993/03/22  17:32:32  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.10  1993/03/18  13:46:49  kl
;;; Umstellung auf die neue Bedeutung des analysed-Slots.
;;;
;;; Revision 1.9  1993/03/10  08:50:01  kl
;;; Bei fehlender Hauptfunkion wird kein Fehler mehr erzeugt.
;;;
;;; Revision 1.8  1993/03/09  07:21:25  ft
;;; Erweiterung von analyse-types um eine Methode fuer Klassen.
;;;
;;; Revision 1.7  1993/03/05  15:50:48  kl
;;; Die used-Annotation wird nicht mehr verwendet.
;;;
;;; Revision 1.6  1993/03/04  10:44:59  kl
;;; Anpassung an die eingefuehrten Typinferenzlevel.
;;;
;;; Revision 1.5  1993/02/16  16:10:20  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.4  1993/02/15  14:46:19  kl
;;; Ist eine Variable nicht (Typ-)gebunden, dann hat sie Typ bottom-t.
;;; Das Ruecksetzen der Typumgebungen auf bottom-t geschieht
;;; durch Loeschen aller Eintraege.
;;;
;;; Revision 1.3  1993/02/02  09:58:54  kl
;;; Initialisierungen der Vorgaengertypumgebungen nach tipass1 verlegt.
;;;
;;; Revision 1.2  1993/01/26  18:34:51  kl
;;; Umbenennung der globalen Variablen vorgenommen. Einige Variablen
;;; nach tidef.lisp verlegt.
;;;
;;; Revision 1.1  1993/01/26  17:10:49  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "titypes")
(require "tidef")
(require "tidecl")
(require "timisc")


;;------------------------------------------------------------------------------
;; Es folgen die Methoden zu der generischen Funktion `analyse-types', die zu
;; jedem Zwischensprachkonstrukt die entsprechenden Sprachteile analysiert.
;; Waehrend des Traversierens werden die entsprechenden Typannotationen gesetzt.
;;------------------------------------------------------------------------------
(defgeneric analyse-types (analysable-object))

;;------------------------------------------------------------------------------
;; analyse-objects analysiert die Liste der angegebenen Funktionen und Klassen
;; bis zum Erreichen des Typfixpunktes.
;;------------------------------------------------------------------------------
(defun analyse-objects (list)

  ;; Bei einer Fixpunktiteration werden zunaechst alle Elemente einmal
  ;; analysiert. Dadurch wird bei grossen Programmen der Fixpunkt in 
  ;; der Regel zuegiger erreicht.
  ;;------------------------------------------------------------------
  (when (do-interprocedural-type-inference)
    (dolist (an-object list)
      (analyse-object an-object T)))

  (do ((*ti-workset*  list)
       (iterations 0 (1+ iterations)))
      ((endp *ti-workset*))
      
    (write-size-of-workset *ti-workset* iterations)
    (analyse-object (pop *ti-workset*))))


;;------------------------------------------------------------------------------
;; Die Typanalyse einer im Modul definierten Funktion bindet die Argumenttypen
;; an die Parameter und analysiert den Rumpf.
;;------------------------------------------------------------------------------
(defmethod analyse-object ((a-defined-fun defined-fun) &optional no-iteration)
  (let* ((*type-environment* (copy-type-env (?pred-type-env a-defined-fun)))
         (*successor-workset* nil))
    
    ;; Binden der Parameter:
    ;;----------------------
    (bind-parameter-types (?params a-defined-fun))

    ;; Analyse des Funktionsrumpfes
    ;;-----------------------------
    (let* ((old-result-type     (?result-type a-defined-fun))
           (old-result-type-env (?result-type-env a-defined-fun))
           (new-result-type     (analyse-types (?body a-defined-fun)))
           (types-are-unchanged (zs-subtypep new-result-type old-result-type)))
      
      (multiple-value-bind (new-result-type-env env-changed)
          (join-pred-type-env-with-type-env old-result-type-env 
                                            *type-environment*)
        (when env-changed
          (setf (?result-type-env a-defined-fun) new-result-type-env
                types-are-unchanged nil)))

      (cond (no-iteration
             (update-type-f (?result-type a-defined-fun) new-result-type))

            (types-are-unchanged
             (setf *ti-workset* (union *successor-workset*
                                        *ti-workset*)))
            (T
             (setf (?result-type a-defined-fun) new-result-type)
             (setf *ti-workset* (union (union *successor-workset*
                                              (union (?called-by a-defined-fun)
                                                     (list a-defined-fun)))
                                       *ti-workset* )))))))


;;------------------------------------------------------------------------------
(defmethod analyse-object ((a-class-def class-def) &optional no-iteration)
  (declare (ignore no-iteration))
  (dolist (a-slot-desc (?slot-descr-list a-class-def))
    (unless (or (null (?initform a-slot-desc))
                (eq (?initform a-slot-desc) :unbound))
      (analyse-types (?initform a-slot-desc)))))


;;------------------------------------------------------------------------------
;; Die Typanalyse von bennanten Konstanten liefert den Wert der bei der 
;; einmaligen Zuweisung gesetzten Typkomponente.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-named-const named-const))
  (?type a-named-const))


;;------------------------------------------------------------------------------
;; Bei Variablenreferenzen wird die zugehoerige Variable analysiert.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-var-ref var-ref))
  (analyse-types (?var a-var-ref)))


;;------------------------------------------------------------------------------
;; Bei statischen Variablen wird der Wert der Typbindung geliefert.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-static static))
  (get-type a-static *type-environment*))


;;------------------------------------------------------------------------------
;; Die Analyse dynamischer Variablen liefert entweder die Typbindung oder, falls
;; sie eine Common Lisp-Konstante mit bekanntem Wert repraesentiert, den Typ der
;; Konstanten.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-dynamic dynamic))
  (let ((constant-value (?constant-value (?sym a-dynamic))))
    (case constant-value
      (:no-const
       (if (use-bindings-of-dynamic-variables) 
           (get-type a-dynamic *type-environment*)
           top-t))
      (:unknown
       (?type a-dynamic))
      (otherwise
       (analyse-types constant-value)))))


;;------------------------------------------------------------------------------
;; Continuations als funktionales Objekt liefern den Funktionstyp.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-cont cont))
  function-t)


;;------------------------------------------------------------------------------
;; Typanalyse der Literale liefert den im tiPass1 ermittelten (konstanten) Typ.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-literal literal))
  (?type a-literal))

(defmethod analyse-types ((a-fun fun))
  function-t)


;;------------------------------------------------------------------------------
;; Wird eine Continuation appliziert, dann wird das Typ-Bottomelement geliefert.
;; Ausserdem wird in die Annotation `type' der Argumenttyp der Applikation mit
;; aufgenommen und die aktuelle Typumgebung wird auf die leere Umgebung gesetzt.
;;------------------------------------------------------------------------------
(defmethod analyse-function-app ((a-cont cont) argument-types)
  (update-type-f (?result-type a-cont) (first argument-types))
  (setf (?type-env a-cont) 
        (join-type-environments *type-environment* (?type-env a-cont))
        *type-environment*
        (empty-environment))
  bottom-t)


;;------------------------------------------------------------------------------
;; Die Applikation einer benutzerdefinierten Funktion liefert den Wert der
;; Komponente ?result-type.
;;------------------------------------------------------------------------------
(defun analyse-defined-function-app (a-fun argument-types &optional union-type)
  (let* ((old-type-env    
          *type-environment*)
         (old-pred-type-env   
          (?pred-type-env a-fun))
         (param-types-changed 
          (not (if union-type
                   (simple-set-parameter-types (?params a-fun) union-type)
                   (set-parameter-types (?params a-fun) argument-types)))))

    (multiple-value-bind (new-pred-type-env env-changed)
      (join-pred-type-env-with-type-env old-pred-type-env *type-environment*)

      (cond (env-changed 
             (setf (?pred-type-env a-fun) new-pred-type-env)
             (pushnew a-fun *successor-workset*))
            (param-types-changed
             (pushnew a-fun *successor-workset*))
            (T nil)))
      
    (setf *type-environment* old-type-env)
    (?result-type a-fun)))


;;------------------------------------------------------------------------------
(defmethod analyse-function-app ((a-fun fun) argument-types)
  (analyse-defined-function-app a-fun argument-types))


;;------------------------------------------------------------------------------
;; Die Typanalyse einer importierten Funktion liefert wenn die Funktion bekannt
;; ist den entsprechenden Resultattyp, das Top-Element des Typverbandes sonst.
;;------------------------------------------------------------------------------
(defmethod analyse-function-app ((an-imp-fun imported-fun) argument-types)
  (if (number-of-args-is-not-ok (?par-spec an-imp-fun) (length argument-types))
      bottom-t
      (let ((type-abstraction-function (?type-abstraction-function an-imp-fun)))
        
        (if (null type-abstraction-function)
            (?result-type an-imp-fun)
            (apply type-abstraction-function argument-types)))))


;;------------------------------------------------------------------------------
;; Der Typ einer Applikation ist der Ergebnistyp des applizierten Ausdrucks bei
;; Anwendung auf die Argumente.
;; Applikationen koennen Seiteneffekte haben. Deshalb ist die Typumgebung nach 
;; der Applikationen so zu aendern, dass die Bindungen der von Seiteneffekten
;; der applizierten Funktion abhaengenden Variablen geloescht werden.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((an-app app))
  (let ((form     (?form     an-app))
        (arg-list (?arg-list an-app)))
    (analyse-types form)
    (let* ((arg-types 
            (mapcar #'analyse-types arg-list))
           (called-funs
            (?called-funs an-app))
           (result-types 
            (mapcar (if (and (special-sys-fun-p form) (?special-caller form))
                        #'(lambda (a-fun)
                            (funcall (?special-caller form) a-fun arg-list))
                         
                        #'(lambda (a-fun)
                            (if (and (special-sys-fun-p a-fun) 
                                     (?tipass a-fun))
                                (apply (?tipass a-fun) arg-list)
                                (analyse-function-app a-fun arg-types))))
                                   
                    called-funs))
           (result
            (if (some #'is-bottom-t arg-types)
                bottom-t
                (apply #'multiple-type-join result-types))))

      ;; Wenn eine importierte Funktion appliziert wird und Typisierungen
      ;; fuer Parameter vorliegen, verwende diese, um:
      ;;  1. die Typbindung der an Argumentposition stehenden Variablen und
      ;;  2. den Ergebnistyp der Applikation
      ;; zu verbessern.
      ;;-------------------------------------------------------------------
      (when (and (imported-fun-p form)
                 (?argument-types form))
        (mapc #'(lambda (argument assertion)
                  (when (var-ref-p argument)
                    (assert-type (?var argument) assertion *type-environment*))
                  (when (is-bottom-t (type-meet assertion (?type argument)))
                    (setf result bottom-t)))
              arg-list (?argument-types form)))

      (if (?other-funs an-app)
          top-t
          result))))


;;------------------------------------------------------------------------------
;; Aendere die Typbindungen entsprechend der moeglichen Seiteneffekte der
;; Applikation.
;;------------------------------------------------------------------------------
(defmethod analyse-types :after ((an-app app))

  ;; Wenn eine definierte Funktion appliziert worden ist, dann aktualisiere 
  ;; die Typumgebung um die in der applizierten Funktion erfolgten 
  ;; Typaenderungen.
  ;; Die geaenderten Typbindungen liegen in der Komponente `result-type-env' 
  ;; der applizierten Funktion.
  ;;
  ;; Ansonsten setze die Typbindungen der von der Seiteneffektanalyse als
  ;; schreibend veraendert angezeigten Variablen auf das Topelement.
  ;; Wenn die Menge der schreibend veraenderten Variablen unbekannt ist, 
  ;; dann setze alle Typbindungen auf das Topelement. 
  ;; Wenn durch die vorliegende Funktionsanwendung destruktive Seiteneffekte 
  ;; auf Variablen ausgeloest worden sind, setze die Typbindungen geeignet
  ;; zurueck.
  ;;------------------------------------------------------------------------
  (cond ((defined-fun-p (?form an-app))
         (setf *type-environment* 
               (update-type-env *type-environment* 
                                (?result-type-env (?form an-app))))
         (reset-type-bindings '() (not-destructive an-app)))
        (T 
         (reset-type-bindings (?write-list an-app) (not-destructive an-app)))))


;;------------------------------------------------------------------------------
;; Die Applikationen der folgenden Funktionen werden in `appfuns',
;; `static-effect' und in der Typinferenz gesondert behandelt.
;;------------------------------------------------------------------------------
(p0-special-funs
 (?special-caller "SC")
 clicc-lisp::funcall
 clicc-lisp::apply
 clicc-lisp::mapcar
 clicc-lisp::maplist
 clicc-lisp::mapcon
 clicc-lisp::mapcan
 clicc-lisp::mapc
 clicc-lisp::mapl)
 

;;------------------------------------------------------------------------------
(defun SC-funcall (a-fun argument-list)
  (analyse-function-app a-fun (mapcar #'?type (rest argument-list))))

;;------------------------------------------------------------------------------
(defun SC-apply (a-fun argument-list)
  (if (imported-fun-p a-fun)
      (?result-type a-fun)
      (let* ((raw-args      (rest argument-list))
             (raw-arg-types (mapcar #'?type raw-args))
             (applied-types (if raw-arg-types
                                (cons (list-component 
                                       (first (last raw-arg-types)))
                                      (butlast raw-arg-types))
                                top-t))
             (union-type    (apply #'multiple-type-join applied-types)))
        (analyse-defined-function-app a-fun nil union-type))))
      

;;------------------------------------------------------------------------------
;; Hilfsfunktion f"ur mapcar, mapc und mapcan
;;------------------------------------------------------------------------------
(defun mapcxx (a-fun argument-list)
  (let* ((list-types (mapcar #'?type (rest argument-list)))
         (arg-types  (mapcar #'list-component list-types))
         (result     (analyse-function-app a-fun arg-types)))
    result))

;;------------------------------------------------------------------------------
;; Hilfsfunktion f"ur maplist, mapl und mapcon
;;------------------------------------------------------------------------------
(defun maplxx (a-fun argument-list)
  (let* ((list-types (mapcar #'?type (rest argument-list)))
         (arg-types  (mapcar #'(lambda (lt) (type-join null-t lt)) list-types))
         (result     (analyse-function-app a-fun arg-types)))
    result))

;;------------------------------------------------------------------------------
(defun SC-mapcar (a-fun argument-list)
  (list-of (mapcxx a-fun argument-list)))

;;------------------------------------------------------------------------------
(defun SC-maplist (a-fun argument-list)
  (list-of (maplxx a-fun argument-list)))


;;------------------------------------------------------------------------------
(defun SC-mapcan (a-fun argument-list)
  (type-join null-t (mapcxx a-fun argument-list)))

;;------------------------------------------------------------------------------
(defun SC-mapcon (a-fun argument-list)
  (type-join null-t (maplxx a-fun argument-list)))

;;------------------------------------------------------------------------------
(defun SC-mapc (a-fun argument-list)
  (mapcxx a-fun argument-list)
  (?type (second argument-list)))

;;------------------------------------------------------------------------------
(defun SC-mapl (a-fun argument-list)
  (maplxx a-fun argument-list)
  (?type (second argument-list)))

                 
;;------------------------------------------------------------------------------
;; Die Applikationen der folgenden Funktionen sehr speziell behandelt.
;;------------------------------------------------------------------------------
(p0-special-funs
 (?tipass "TI")
 clicc-lisp::coerce
 clicc-lisp::concatenate)

;;------------------------------------------------------------------------------
(defun ti-coerce (&rest arguments)
  (if (< (length arguments) 2)
      bottom-t
      (get-intern-type (second arguments))))

(defun ti-concatenate (&rest arguments)
  (if (rest arguments)
      (type-meet sequence-t (get-intern-type (first arguments)))
      bottom-t))
 

;;------------------------------------------------------------------------------
;; Der Typ eines setq ist der Typ der zugehoerigen Form. Als Seiteneffekt 
;; wird bei Zuweisungen an Variablen die entsprechende Typbindung aktualisiert.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-setq-form setq-form))
  (let ((location (?location a-setq-form))
        (result   (analyse-types (?form a-setq-form))))
    
    ;; Bei Variablenreferenzen wird noch deren Typbindung aktualisiert.
    ;;-----------------------------------------------------------------
    (unless (named-const-p location)
      (let ((var (?var location)))
        (setf (?type location) result)
        (when (or (static-p var)
                  (and (dynamic-p var) 
                       (eq (?constant-value (?sym var)) :no-const))))
        (bind-and-update-type var result)))
      
    result))


;;------------------------------------------------------------------------------
;; Alle Elemente der form-list werden analysiert. Analysierte Typbindungen 
;; werden von Ausdruck zu Nachfolgeausdruck propagiert. Geliefert wird der 
;; Typ des letzten Ausdrucks.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-progn-form progn-form))
  (mapc-progn-form-list (?form-list a-progn-form)
                        #'analyse-types
                        #'analyse-types))


;;------------------------------------------------------------------------------
;; Der Typ eines if-Konstruktes ist der Vereinigungstyp der Typen der then- und
;; der else-Ausdruecke.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((an-if-form if-form))

  (analyse-types (?pred an-if-form))
  
  ;; Merke die Typumgebung vor der Analyse der then- und else-Teile.
  ;;----------------------------------------------------------------
  (multiple-value-bind (then-entry-env else-entry-env)
      (get-type-assertions-from-predicate-position (?pred an-if-form) 
                                                   *type-environment*)

    (let (then-exit-type 
          then-exit-env 
          else-exit-type 
          else-exit-env)
      
      ;; Analyse des then-Teils:
      ;;------------------------
      (setf *type-environment* then-entry-env
            then-exit-type     (analyse-types (?then an-if-form))
            then-exit-env      *type-environment*)
      
      ;; Analyse des else-Teils:
      ;;------------------------
      (setf *type-environment* else-entry-env
            else-exit-type     (analyse-types (?else an-if-form))
            else-exit-env      *type-environment*)
      
      ;; Setzen der Ergebnisumgebung und Rueckgabe des Resultates:
      ;;----------------------------------------------------------
      (setf *type-environment* (join-type-environments then-exit-env
                                                       else-exit-env))
      (type-join then-exit-type else-exit-type))))


;;------------------------------------------------------------------------------
;; Die Typanalyse einer switch-Form analysiert die Form zur Bestimmung des
;; Schluessels und liefert dann den Vereinigungstypen der Analysen der
;; case-Liste.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-switch-form switch-form))
  (analyse-types (?form a-switch-form))
  
  ;; Merke die Typumgebung vor der Analyse des switch-Konstruktes.
  ;;-------------------------------------------------------------- 
  (let* ((old-environment    *type-environment*)
         (result-type        (analyse-types (?otherwise a-switch-form)))
         (result-environment *type-environment*))
    
    (dolist (a-case (?case-list a-switch-form))
      (setf *type-environment* (copy-type-env old-environment)
            result-type        (type-join (analyse-types a-case) result-type)
            result-environment (join-type-environments *type-environment*
                                                       result-environment)))
    
    (setf *type-environment* result-environment)
    result-type))


;;------------------------------------------------------------------------------
;; Analysiere den Wert und den Ausdruck der labeled-form.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-labeled-form labeled-form))
  (analyse-types (?value a-labeled-form))
  (analyse-types (?form a-labeled-form)))


;;------------------------------------------------------------------------------
;; Der Typ einer labels-form ist der Typ des Rumpfes.
;; Die Funktionsbeschreibungen zu den hier lokal definierten Funktionen sind
;; zu Beginn der Analyse bereits angelegt worden.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-labels-form labels-form))
  (analyse-types (?body a-labels-form)))


;;------------------------------------------------------------------------------
;; Die Typanalyse einer let*-Form bindet die Typen der Initialisierungsformen an
;; die zu bindenden Variablen. Als Ergebnis wird das Analyseergebnis des Rumpfes
;; geliefert.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-let*-form let*-form))
  (let ((old-type-env (copy-type-env *type-environment*))
        (bound-vars-env '()))
    
    (mapc #'(lambda (variable init-form)
              (push (cons variable (get-type variable *type-environment*))
                    bound-vars-env)
              (bind-and-update-type variable (analyse-types init-form)))
          (?var-list a-let*-form)
          (?init-list a-let*-form))
    
    (let ((result (analyse-types (?body a-let*-form))))
      
      ;; Ruecksetzen der Typumgebung
      ;;----------------------------
      (setf *type-environment* 
            (reset-let-type-env old-type-env 
                                *type-environment* 
                                bound-vars-env))
      
      result)))


;;------------------------------------------------------------------------------
;; Bei let/cc-Ausdruecken wird der Rumpf analysiert. Waehrend der Analyse des
;; Rumpfes werden jeweils die Typen und die aktuellen Typumgebungen der 
;; Ruecksprungstellen in Annotationen der Continuation abgelegt.
;; Das Analyseergebnis ist davon abhaengig, ob die Continuation (auch) als
;; funktionales Objekt verwendet wird oder nicht.
;; Wenn die Continuation als funktionales Objekt verwendet wird, dann kann mit
;; irgendeiner Typumgebung und einem unbekannten Typ zurueckgesprungen werden.
;; Deshalb wird die Typumgebung zurueckgesetzt und als Ergebnistyp das Top-
;; element geliefert.
;; Wenn die Continuation nicht als funktionales Objekt verwendet wird, dann
;; werden die Ergebnistypumgebung des Rumpfes und die an den Ruecksprungstellen
;; gesammelten Typumgebungen vereinigt. Der Ergebnistyp ist die Typvereinigung
;; aus dem Ergebnistyp des Rumpfes und den an den Ruecksprungstellen gesammelten
;; Typen.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-let/cc-form let/cc-form))
  (let ((cont (?cont a-let/cc-form)))

    (let* ((body-result-type (analyse-types (?body a-let/cc-form)))
           (body-result-env  *type-environment*))
      
      (cond ((?unknown-caller cont)
             (set-all-type-bindings-to top-t)
             top-t)
            (T 
             (setf *type-environment* 
                   (join-type-environments body-result-env (?type-env cont)))
             (type-join body-result-type (?result-type cont)))))))


;;------------------------------------------------------------------------------
;; Bei der Analyse der tagbody-Konstrukte wird das folgende Fixpunktverfahren
;; angewendet. Fuer alle tagged-forms werden die Typumgebungen saemtlicher 
;; Vorgaenger im Datenfluss gesammelt. In einem tagbody-Konstrukt mit den 
;; tagged-forms {T0, T1, ..., Tn} sind die Vorgaenger der tagged-form Ti
;; alle Ausdruecke der Form (GO Ti) und Ti-1.
;;------------------------------------------------------------------------------
(defvar *type-env-changed*)

(defmethod analyse-types ((a-tagbody-form tagbody-form))
  (let ((result (analyse-types (?first-form a-tagbody-form)))
        (tagged-form-list (?tagged-form-list a-tagbody-form)))

    (let ((entry-env *type-environment*))
           
      (loop
       (setq *type-environment* entry-env)
       (let* ((*type-env-changed* nil))
              
         (dolist (tagged-form tagged-form-list)

           ;; die letzte tagged-form bzw. die first-form ist Vorgänger
           ;; der nächsten tagged-form.
           ;;--------------------------
           (add-predecessor-type-env tagged-form *type-environment*)
           (setq *type-environment* 
                 (copy-type-env (?pred-type-env tagged-form)))
           (setq result (analyse-types (?form tagged-form))))
              
         (unless *type-env-changed*
           (return))))

      ;; Resultat des und Typumgebung nach dem letzten Element der
      ;; tagged-form-list bzw. der first-form, falls tagged-form-list leer ist.
      ;;-----------------------------------------------------------------------
      result)))


;;------------------------------------------------------------------------------
;; Wird eine tagged-form analysiert, dann handelt es sich um ein angewandtes
;; Vorkommen. Alle definierenden Vorkommen werden schon im zugehoerigen 
;; tagbody-Konstrukt analysiert.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-tagged-form tagged-form))
  (add-predecessor-type-env a-tagged-form *type-environment*)
  (setf *type-environment* (empty-environment))
  bottom-t)


;;------------------------------------------------------------------------------
;; add-predecessor-type-env vereinigt eine Typumgebung mit der bisherigen 
;; Vereinigungsumgebung aller Vorgaenger.
;;------------------------------------------------------------------------------
(defun add-predecessor-type-env (a-tagged-form type-env)
  (multiple-value-bind (result-env type-env-changed)
      (join-type-environments (?pred-type-env a-tagged-form) type-env)
    (setf (?pred-type-env a-tagged-form) result-env)
    (when type-env-changed
      (setf *type-env-changed* T))))


;;------------------------------------------------------------------------------
;; Bei mv-lambda wird zur Zeit noch an alle Parameter das Top-Element des 
;; Typverbandes gebunden.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((an-mv-lambda mv-lambda))
  ;; Analysiere zuerst den Ausdruck, der eventuell multiple Werte liefert.
  (analyse-types (?arg an-mv-lambda))
  
  (let* ((old-type-env (copy-type-env *type-environment*))
         (params       (?params an-mv-lambda)))
    
    ;; Setze alle Parametertypen auf das Topelement.
    ;;----------------------------------------------
    (simple-set-parameter-types params)
    
    ;; Binden der Parameter
    ;;---------------------
    (bind-parameter-types params)
    
    ;; Analyse des Rumpfs
    ;;-------------------
    (let ((result (analyse-types (?body an-mv-lambda))))
      
      ;; Ruecksetzen der Typumgebung
      ;;----------------------------
      (setf *type-environment* 
            (update-type-env old-type-env *type-environment*))
      
      result)))


;;------------------------------------------------------------------------------
;; Alle Klassen haben zunaechst den Typ class-t
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-class class-def))
  class-t)


;;------------------------------------------------------------------------------
;; Analyse der foreign-fun-Konstrukte liefert zur Zeit noch das Topelement des
;; Typverbands.
;;------------------------------------------------------------------------------
(defmethod analyse-types ((a-foreign-fun foreign-fun))
  top-t)


;;------------------------------------------------------------------------------
;; In der around-Methode zu analyse-types wird die Typkomponente der analysier-
;; ten Form gesetzt.
;;------------------------------------------------------------------------------
(defmethod analyse-types :around ((a-form form))
  (update-type-f (?type a-form) (call-next-method)))

(defmethod analyse-types :around ((a-cont cont))
  (update-type-f (?type a-cont) (call-next-method)))


;;------------------------------------------------------------------------------
(provide "tipass2") 
