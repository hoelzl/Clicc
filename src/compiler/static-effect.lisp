;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;--------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Seiteneffektanalyse
;;;            Implementierung der folgenden Funktion:
;;;            Analyse-form : ZWS * EFFECT  -----> EFFEKT
;;;            wobei 
;;;            ZWS    : die Menge der Zwischensprachobjekte
;;;            EFFEKT : {(read-list,write-list)/ 
;;;                       read-list , write-list Mengen von Variablen }
;;;
;;; $Revision: 1.75 $
;;; $Log: static-effect.lisp,v $
;;; Revision 1.75  1994/02/08  11:12:13  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.74  1994/01/28  13:19:35  ft
;;; Ausnahmetest in analyse-form auf nil erweitert.
;;;
;;; Revision 1.73  1994/01/26  13:37:17  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.72  1994/01/17  14:10:21  hk
;;; Fehler in combine-level-list behoben.
;;;
;;; Revision 1.71  1994/01/10  09:17:30  hk
;;; apply #'max (mapcar  --> reduce
;;;
;;; Revision 1.70  1993/12/22  11:16:27  atr
;;; Die Tail-Rekursion direkt in clcmain als erste Optimierung verschoben.
;;; Es werden jetzt zwei Iterationen der Voranalyse gemacht. (die zweite
;;; Iteration bringt noch einiges).
;;;
;;; Revision 1.69  1993/11/09  16:41:45  atr
;;; Die globalen Variablen stehen jetzt in se-init.lisp.
;;;
;;; Revision 1.68  1993/11/08  14:07:21  atr
;;; require an den Anfang der datei verschoben. Kommentare verbessert.
;;;
;;; Revision 1.67  1993/11/04  13:43:39  atr
;;; Die Funktion known-funs verbessert.
;;;
;;; Revision 1.66  1993/10/25  10:04:45  atr
;;; Die Funktion "known-funs" verbessert.
;;;
;;; Revision 1.65  1993/10/19  17:39:01  atr
;;; Die Analyse der Spr"unge korrigiert. Jetzt werden lokale und
;;; nicht lokale Spr"unge korrekt unterschieden.
;;; Dei Statistic um die Ausgabe der Funktionen, die "nicht lokale" Spr"unge
;;; machen k"onnen, erweitert.
;;;
;;; Revision 1.64  1993/10/18  22:22:23  atr
;;; Einen gro"sen Fehler bei analyse-form von Labels korrigiert.
;;; Die zwei Abfragen auf *visited-function* und *effect-all-functions*
;;; und die Zuweisung an *effect-work-set* entfernt. Die lokalen
;;; Funktionen werden schon vor der Fixpunktiteration an *work-set*
;;; hinzugef"ugt. Der Code etwas verbessert, globalen Variablen entfernt
;;; (*visited-functions* und *effect-all-functions*)
;;;
;;; Revision 1.63  1993/10/14  13:55:26  atr
;;; Dei Aufrufe von get-result-position-forms von get-arg-from-*-param
;;; entfernt, und in spec-vars-buond-to-vars eingef"ugt, weil
;;; get-arg-from-param bei der Tail-recursion ben"otigt wird.
;;; Dei Funktionen get-arg-from-*-param leicht ge"andert.
;;; Die Funktion has-top-effect korrigiert.
;;;
;;; Revision 1.62  1993/10/13  17:07:24  atr
;;; Kleine Verbesserungen an "known-funs" und "get-arg-from-coded-param"
;;; vorgenommen.
;;;
;;; Revision 1.61  1993/10/12  21:51:33  atr
;;; Noch ein Fehler bei Analyse-form f"ur APP korrigiert.
;;;
;;; Revision 1.60  1993/10/12  19:23:37  atr
;;; Falsche Ausklammerung bei analyse-form einer Setq-form korrigiert.
;;;
;;; Revision 1.59  1993/10/11  11:09:44  hk
;;; Tippfehler behoben
;;;
;;; Revision 1.58  1993/10/11  10:58:57  atr
;;; Der Slot Data-effects bei top-effect auf :allo-dest-jump gesetzt,
;;; anstatt :alloc-dest.
;;;
;;; Revision 1.57  1993/10/11  10:42:03  atr
;;; Tippfehler korrigiert (data-effect ---> data-effects ).
;;;
;;; Revision 1.56  1993/10/11  09:59:59  atr
;;; (defvar *map-functions* ) eingefügt.
;;;
;;; Revision 1.55  1993/10/11  09:52:51  atr
;;; Die Variable *map-functions* wieder eingef"ugt, weil sie noch bei der
;;; Pre-analyse gebraucht wird.
;;;
;;; Revision 1.54  1993/10/09  18:38:25  atr
;;; Jetzt werden auch Spr"unge als Seiteneffekt betrachtet. Sie werden im
;;; Slot Data-effects mit dem Wert :JUMP bezeichnet.
;;;
;;; Revision 1.53  1993/10/09  14:58:56  atr
;;; Ein Fehler bei der Behandlung der Aufrufe von Funktionen mit
;;; special-vars beseitigt. Nun werden alle solche Funktionen
;;; einheitlich behandelt, es gibt keine sonderbehandlung f"ur die
;;; MAP-Funktionen mehr.(Die Variable *map-functions* wurde entfernt.)
;;;
;;; Revision 1.52  1993/10/05  11:16:16  atr
;;; Ein Typfehler bei "analyse-form" von "dynamic" korrigiert.
;;;
;;; Revision 1.51  1993/09/14  18:01:52  atr
;;; Noch einen Fehler bei der Funktion get-arg-from-param korrigiert.
;;;
;;; Revision 1.50  1993/09/14  13:17:41  atr
;;; Die Funktion get-arg-from-param so veraendert, dass sie fuer
;;; die nicht gebundene optionale oder Keyword Parameter die Init-form
;;; nicht nil zurueckliefert.
;;;
;;; Revision 1.49  1993/09/07  08:16:00  kl
;;; Schreibfehler behoben. *current-function  -> *current-function*
;;;
;;; Revision 1.48  1993/09/06  15:09:15  atr
;;; Die globale Variable *effect-current-function* ist entfernt.
;;; Es wird nur die Variable *current-function* benutzt, die auch bei
;;; der Voranalyse, let-optimierung und Tail-rekursion benutzt wird.
;;;
;;; Revision 1.47  1993/09/06  12:03:36  atr
;;; Eine ignore Deklaration bei (analyse-form ((function fun))) eingefügt.
;;;
;;; Revision 1.46  1993/09/06  11:38:33  atr
;;; Angewandte Vorkommen von Funktionen, die nicht auf Funktionsposition
;;; sind machen keine Seiteneffekte.
;;;
;;; Revision 1.45  1993/08/25  13:55:42  atr
;;; Die Funktion has-top-effect leicht geaendert.
;;; Bei analyse-module werden die Funktionen, die schon den Top-effect haben
;;; nicht mehr re-analysiert.
;;;
;;; Revision 1.44  1993/08/04  16:04:48  hk
;;; optimize und tail erst nach (defvar *static-level*) laden, da die
;;; Variable dort gebunden wird.
;;;
;;; Revision 1.43  1993/08/04  10:33:41  atr
;;; Die Tail-rekursion eingebunden. Die kann man ueber
;;; den Schalter *no-tail-recursion* ausschalten.
;;;
;;; Revision 1.42  1993/07/27  13:58:40  atr
;;; Die unbekannte Seiteneffekte werden nicht mehr durch ein
;;; UNKNOWN bezeichnet, sondern durch eine Inetegerzahl:
;;; Das maximale statische Level auf dem unbekannte Seiteneffekte passieren.
;;;
;;; Revision 1.41  1993/07/21  12:31:11  atr
;;; Zwei Fehler korrigiert bei spec-vars-bound-to-funs und
;;; get-arg-from-param.
;;;
;;; Revision 1.40  1993/07/13  11:07:37  atr
;;; Das Setzen der Sots LEVEL passiert jetzt in der Voranalyse.
;;; Ein Aufruf von remove-duplicates durch NUNION ersetzt .
;;; Ein Test , der bis jetzt immer nil liefert, korrigiert .
;;;
;;; Revision 1.39  1993/07/06  14:31:50  atr
;;; Optimierungen wieder angeschaltet.
;;;
;;; Revision 1.38  1993/07/06  13:14:47  atr
;;; Fehler bei (Analyse-form (defined-fun)) korrigiert:
;;; union-only-global-effects durch union-all-effects ersetzt.
;;;
;;; Revision 1.37  1993/07/06  10:20:21  hk
;;; *no-let-optimizing* auf t, weil noch ein gemeiner Fehler vorliegt.
;;;
;;; Revision 1.36  1993/06/30  14:22:52  hk
;;; (require optimize) eingefuegt.
;;;
;;; Revision 1.35  1993/06/29  23:26:32  atr
;;; Kommentare korrigiert.
;;;
;;; Revision 1.34  1993/06/26  13:34:58  atr
;;; Bessere Kommentare geschrieben.
;;;
;;; Revision 1.33  1993/06/17  18:05:55  atr
;;; Kommentare verbessert.
;;;
;;; Revision 1.32  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.31  1993/06/16  18:30:47  atr
;;; Aufruf der Funktion UNION durch REMOVE-DUPLICATES und
;;; APPEND ersetzt . Jetzt entsteht keine endlose Schleife !!!!
;;;
;;; Revision 1.30  1993/06/14  20:26:13  atr
;;; Klammer zu viel entfernt.
;;;
;;; Revision 1.29  1993/06/14  19:57:31  atr
;;; In der Analyse der Applikationen wird union-only-global-effects
;;; durch union-all-effects ersetzt damit die Slots read-list write-list bei
;;; Applikationen richtig gesetzt wird also damit die lokale Seiteneffekte
;;; nicht ignoriert werden.
;;;
;;; Revision 1.28  1993/06/14  10:41:24  atr
;;; Fehler bei der Analyse von DEFCLASS korrigiert,
;;; und kleine verbesserungen an Funktionsdefinitionen gemacht.
;;;
;;; Revision 1.27  1993/06/11  17:54:01  atr
;;; Analyse der Seiteneffekten auf Daten eingebaut.
;;; In dem Slot DATA-EFFECTS bei Funktionen und bei Applikationen
;;; steht nun entweder nil oder :alloc und/oder :dest.
;;; Alloc fuer Alloziierung auf dem Heap , dest fuer destruktive Operation.
;;;
;;; Revision 1.26  1993/05/31  09:34:10  ft
;;; Tippfehler in analyse-form (class-def) beseitigt.
;;;
;;; Revision 1.25  1993/05/30  14:06:00  atr
;;; Die Analyse der Applikationen verfeinert.
;;; Jetzt werden die special-defined-functions besser analysiert.
;;;
;;; Revision 1.24  1993/05/13  16:25:28  hk
;;; *static-level* bei globalen Funktionen auf 0.
;;;
;;; Revision 1.23  1993/05/12  13:28:39  hk
;;; (clicc-message ... CALLED) entfernt
;;;
;;; Revision 1.22  1993/05/10  16:04:50  atr
;;; (require closure-analysis) wieder eingefuegt.
;;;
;;; Revision 1.21  1993/05/10  10:21:23  atr
;;; Der Aufruf von Get-closures-of-module entfernt .
;;; Zwei momentan neue Variablen *map-functions* und *error-function* eingefuegt,
;;; um die map-funktionen sowie die Funktion error auf einer sonder Art
;;; zu behandeln.
;;;
;;; Revision 1.20  1993/05/05  17:34:52  atr
;;; Eine Applikation einer Variablen hat den Effekt UNKNOWN.
;;; Das Lesen eines Funktionalen Objectes hat keine Seiteneffekte.
;;; Bei der Fixpunktiteration werden nur Funktionen reanalysiert, die noch nicht den Top-effekt haben.
;;;
;;; Revision 1.19  1993/04/27  10:55:43  atr
;;; Es wird jetzt ein 'closure-analysis' im Clicc-Baum required.
;;;
;;; Revision 1.18  1993/04/27  07:12:32  ft
;;; atr's require in sein Home-Verzeichnis verbogen.
;;;
;;; Revision 1.17  1993/04/26  12:16:05  atr
;;; Die Variable *closures* in *upward-fun-args* umbenannt.
;;;
;;; Revision 1.16  1993/04/24  16:42:11  atr
;;; Analyse der Applikationen vera?ndert , clicc-message anstatt format.
;;;
;;; Revision 1.15  1993/04/18  16:55:43  atr
;;; *side-effect-info-level* auf 1 gesetzt.
;;;
;;; Revision 1.14  1993/04/18  16:35:49  atr
;;; Fehler korrigiert bei analyse-form fuer defined-fun.
;;;
;;; Revision 1.13  1993/04/17  12:53:23  atr
;;; Messages verbessert.
;;;
;;; Revision 1.12  1993/04/15  19:06:53  atr
;;; *side-effect-info-level* Schalter fuer infos ueber die Analyse.
;;;
;;; Revision 1.11  1993/03/31  18:43:15  atr
;;; empty-message
;;;
;;; Revision 1.10  1993/03/11  02:16:39  atr
;;; Methode analyse-app-form fur
;;;
;;; Revision 1.9  1993/03/09  01:57:47  atr
;;; Applikationen der Iterationsfunktionen (map apply ...) korrigiert.
;;;
;;; Revision 1.8  1993/03/08  02:49:51  atr
;;; Fehler entfernt.
;;;
;;; Revision 1.7  1993/03/08  02:48:56  atr
;;; Fehler korrigiert.
;;;
;;; Revision 1.6  1993/03/06  04:41:28  atr
;;; kleine Verbesserungen...
;;;
;;; Revision 1.5  1993/02/16  16:14:00  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.4  1993/02/15  13:52:50  atr
;;; Fehler beseitigt.
;;;--------------------------------------------------------------------------

(in-package "CLICC")
(require "se-init")
(require "optimize")
(require "tail")
(require "closure-analysis")

;;;-------------------------------------------------------------------------
;;; Analyse eines Modules durch eine Fixpunktiteration.
;;;-------------------------------------------------------------------------
(defun analyse-a-module ()
  (let* ((all-functions           (append  (?fun-list *module*)
                                           (list (?toplevel-forms *module*))))
         (*effect-work-set*       nil)
         (*error-function*        (get-global-fun 'clicc-lisp::error)))
    (labels ((push-local-funs (function)
               (when (?local-funs function)
                 (dolist (one-local-fun (?local-funs function))
                   (push one-local-fun all-functions)
                   (push-local-funs one-local-fun)))))
      
            
      ;; Hier werden die Funktionen weiter attributiert um bessere 
      ;; Ergebnisse bei der Seiteneffektanalyse zu erzielen.
      ;;----------------------------------------------------------
      (unless *no-pre-analysis*
        (clicc-message "Preparing side effect analysis...")
        (clicc-message "---------------------------------")
        (pre-analyse-module)
        (pre-analyse-module))
      
      ;; Fixpunktiteration zur Ableitung der Seiteneffekte der Funktionen
      ;;-----------------------------------------------------------------
      (clicc-message "Side-effect analysis...")
      (clicc-message "-----------------------")
      
      ;; Nun werden alle lokalen Funktionen zu all-functions
      ;; hinzugef"ugt.
      ;;------------------------------------------------------------------
      (mapc #'push-local-funs all-functions)
      
      ;; Die Fixpunktiteration ...
      ;;--------------------------
      (do* ( (*effect-work-set*  all-functions)
             *current-function*
             old-read
             old-write
             old-data-effects)
           ((null *effect-work-set*))
        (setq *current-function*        (pop *effect-work-set*))
        (setq old-read         (?read-list     *current-function*))
        (setq old-write        (?write-list    *current-function*))
        (setq old-data-effects (?data-effects  *current-function*))
        (when (equal *Side-effect-info-level* 2)
          (clicc-message "Analysing the function ~s"
                         (?symbol *current-function*)))
        (analyse-function  *current-function*  (empty-effect))
        
        ;; Wenn der Seiteneffekt der Funktion sich geaendert hat 
        ;; dann wird jeder Aufrufer dieser Funktion , dessen
        ;; Seiteneffekt noch nicht der TOP-effekt ist, nochmal
        ;; analysiert.
        ;;------------------------------------------------------
        (unless (and (equal  old-read   
                             (?read-list  *current-function*))
                     (equal  old-write  
                             (?write-list *current-function*))
                     (eq   old-data-effects
                           (?data-effects  *current-function*)))
          
          (let ((to-reanalyse    (remove-if #'has-top-effect 
                                            (?mv-called-by *current-function*))))
            (setq *effect-work-set* (nunion to-reanalyse
                                            *effect-work-set* :test #'eq)))))
      
      (unless (zerop *Side-effect-info-level*)
        (statistics all-functions)))))

;;;------------------------------------------------------------------------
;;; Analyse eines Moduls, Let-optimierug und Tail-Rekursion.
;;;------------------------------------------------------------------------
(defun analyse-module ()
 
  (analyse-a-module)
  
  ;; Optimierungen der Let-Ausdr"ucke.
  ;; Abschalten : durch den Schalter
  ;; *no-let-optimizing*
  ;;---------------------------------------
  (unless *no-let-optimizing*
    (let-optimizing))
  
  (clicc-message-line))

;;;-----------------------------------------------------------------------
;;; Has-top-effect "uberpr"uft ob der Seiteneffekt einer Funktion schon 
;;; der Top-effect ist.
;;;-----------------------------------------------------------------------
(defun has-top-effect (function)
  (let ((r (?read-list function))
        (w (?write-list function))
        (level (if (global-fun-p function) 0 
                   (?level function))))
    (and (integerp r)
         (eql r level)
         (integerp w)
         (eql w level)
         (eql :alloc-dest-jump (?data-effects function)))))

;;;------------------------------------------------------------------------
;;; Zur Zeit nur diese einfache Ausgabe der Ergebnisse der Analyse.
;;;------------------------------------------------------------------------

(defun statistics (fun-list)
  (let ((se-free 0)
        (se-top  0)
        (read    0)
        (write   0)
        (alloc   0)
        (dest    0)
        (jump    0)
        (number-of-funs (length fun-list)))
    (dolist (fun fun-list)
      (let ((read-list    (?read-list fun))
            (write-list   (?write-list fun))
            (data-effects (?data-effects fun)))
        (when (and (integerp read-list)
                   (integerp write-list))
          (setq se-top (1+ se-top)))
        (when (and (null read-list )
                   (null write-list ))
          (setq se-free (1+ se-free)))
        (when (integerp read-list)
          (setq read (1+ read)))
        (when (integerp write-list )
          (setq write (1+ write)))
        (when (or (eql data-effects   :alloc)
                  (eql data-effects   :alloc-dest)
                  (eql data-effects   :alloc-jump)
                  (eql data-effects   :alloc-dest-jump))
          (setq alloc (1+ alloc)))
        (when (or (eql data-effects   :dest)
                  (eql data-effects   :alloc-dest)
                  (eql data-effects   :dest-jump)
                  (eql data-effects   :alloc-dest-jump))
          (setq dest (1+ dest)))
        (when (member data-effects '(:jump :alloc-jump :dest-jump 
                                     :alloc-dest-jump) :test #'eql)
          (incf jump))))
    
    
    (clicc-message "~s functions are analysed" number-of-funs)
    (clicc-message "~s functions are side effect free"
                   se-free )
    (clicc-message "~s functions have an unknown side effect"
                   se-top)
    (clicc-message "~s functions have an unknown read-effect"
                   read)
    (clicc-message "~s functions have an unknown write-effect"
                   write)
    (clicc-message "~s functions may use the heap" alloc)
    (clicc-message "~s functions may be destructive" dest)
    (clicc-message "~s functions may make non local jumps" jump)))

;;;-------------------------------------------------------------------------
;;; Simple-literal hat keine Effekte.
;;;-------------------------------------------------------------------------
(defmethod analyse-form ((a-simple-lit simple-literal)  effect)
  (declare (ignore effect)))

;;;-------------------------------------------------------------------------
;;; Methode zur Analyse von Structured-literal
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form structured-literal) effect)
  (declare (ignore effect)))

;;;--------------------------------------------------------------------------
;;; Ein Symbol hat keine Seiteneffekte.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form sym)   effect)
  (declare (ignore effect)))

;;;--------------------------------------------------------------------------
;;; Zur Zeit werden class-def nicht analysiert.
;;; -------------------------------------------------------------------------
(defmethod analyse-form ((one-class-def class-def) effect)
  (dolist (one-slot-descr (?slot-descr-list one-class-def))
    (unless (or (null (?initform one-slot-descr))
                (eq (?initform one-slot-descr) :unbound))
      (analyse-form (?initform one-slot-descr) effect))))

;;;--------------------------------------------------------------------------
;;; Analyse von Variablen:
;;; wenn das statische Niveau dieser statisch gebundenen  Variablen kleiner
;;; ist als das aktuelle statische Niveau der Analyse , wird sie in die 
;;; Read-list eingetragen, sonst wird nichts getan. 
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((static-var static) effect)
  (if  (listp (?read-list effect) )
       (pushnew static-var (?read-list effect) :test #'eq)
       (setf (?read-list effect) (max (?read-list effect) 
                                      (?level static-var)))))

;;;--------------------------------------------------------------------------
;;; Die dynamisch gebundene Variablen werden in die "read-list"
;;; eingetragen.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((dynamic-var dynamic) effect)
  (when (and (listp (?read-list effect))
             (eql  (?constant-value (?sym dynamic-var)) :no-const))
    (pushnew dynamic-var  (?read-list effect) :test #'eq)))

;;;--------------------------------------------------------------------------
;;; analyse-form (var-ref,r-w-list) = analyse-form ((?var var-ref),r-w-list)
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((ref var-ref) effect)
  (analyse-form (?var ref)  effect))

;;;--------------------------------------------------------------------------
;;; Seiteneffekte einer IF-Form ist das MAX ueber die Seiteneffekte
;;; der Pred- then- und else-Teile.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form if-form) effect)
  (analyse-form (?pred form)  effect )
  (analyse-form (?then form)  effect )
  (analyse-form (?else form)  effect ))

;;;--------------------------------------------------------------------------
;;; Analyse einer Labels-Form : hier wird der Rumpf analysiert.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((labels labels-form) effect)
  (analyse-form (?body labels) effect))

;;;--------------------------------------------------------------------------
;;; Analyse einer PROG-Form:
;;; analyse-form (progn , effect) = Max [ (analyse-form (form1  effect))
;;;                                       (analyse-form (form2  effect))
;;;                                                    ...
;;;                                       (analyse-form (formN  effect))]
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form progn-form) effect)
  (dolist (one-form  (?form-list form))
    (analyse-form one-form   effect )))

;;;--------------------------------------------------------------------------
;;; Analyse einer SETQ-form: (setq var form)
;;; Var wird in die WRITE-LIST eingetragen, soweit sie nicht eine dynamische 
;;; Variable mit konstantem Wert ist, dann wird FORM in den 
;;; neuen EFFECT analysiert.
;;;--------------------------------------------------------------------------

(defmethod analyse-form ((form setq-form) effect)
  (unless  (named-const-p (?location form)) 
    (let ((var (?var (?location form))))
      
      (unless  (and (dynamic-p var)
                    (constant-value-p  (?sym var)))
        (if  (listp (?write-list effect))
             (pushnew var (?write-list effect) :test #'eq)
             (when (static-p var)
               (setf (?write-list effect)
                     (max (?write-list effect) (?level var))))))))
  (analyse-form (?form form) effect))

;;;--------------------------------------------------------------------------
;;; Analyse einer Switch-Form :
;;; es werden die erste Form in der Switch, die Otherwise-form, die Form
;;; in jedem Case analysiert, und das Maximum ueber deren Seiteneffekte
;;; genommen. 
;;; -------------------------------------------------------------------------
(defmethod analyse-form ((form switch-form) effect)
  (analyse-form (?form form)      effect )
  (analyse-form (?otherwise form) effect )
  (let ((case-list (?case-list form)))
    (dolist (case case-list )
      (analyse-form  case  effect))))


(defmethod analyse-form ((form labeled-form) effect)
  (analyse-form (?value form)  effect )
  (analyse-form (?form  form)  effect ))

;;;--------------------------------------------------------------------------
;;; Analyse einer Tagbody-Form :
;;; Es wird das Maximum genommen ueber die Seiteneffekte der First-form
;;; in dem Tagbody und der Formen in jeder Tagged-form.
;;; Es werden alle tagged-formen aus der Tagged-form-list eines Tagbody
;;; analysiert.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form tagbody-form) effect)
  (analyse-form (?first-form  form) effect)
  (dolist (one-form (?tagged-form-list form))
    (analyse-form (?form one-form) effect )))

;;;--------------------------------------------------------------------------
;;; Eine GO-FORM zu einem Tag, der in der *current-function* definiert ist
;;; hat keinen Seiteneffekt, sonst verursacht er einen Sprung au"serhalb 
;;; der Funktion also :JUMP
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((form tagged-form) effect)
  (setf *jump-level* (min *jump-level* (?level (?tagbody form))))
  (setf (?data-effects effect) (max-data-effects (?data-effects effect)
                                                 :jump)))


;;;--------------------------------------------------------------------------
;;; Analyse einer LET*-Form 
;;; analyse-form ([let* ((var1 val1)  ...  (varN valN)) body ],effect1) 
;;;  = MAX [(analyse-form val1 effect1)
;;;         (analyse-form val2 effect2)
;;;                ...
;;;         (analyse-form body effectN+1)]
;;; wobei effectI+1 der erhatene effekt nach (analyse-form (valI effectI))
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((let*form let*-form) effect)
  
  ;; Zuerst werden alle init-formen  analysiert.
  ;;-----------------------------------------------------------------
  (dolist (one-form (?init-list let*form))
    (analyse-form one-form effect))
  
  ;; nun wird der Rumpf analysiert.
  ;;-----------------------------------------------------------------
  (analyse-form (?body let*form) effect))

;;;-------------------------------------------------------------------------
;;; Hier wird eine Funktionsdefinition in einen leeren Effekt analysiert.
;;; Aus dem  erhaltenen Effekt werden alle lokale Seiteneffekte 
;;; entfernt, und der globale Effekt wird in den Slots read-list 
;;; write-list und data-effects abgespeichert.
;;;-------------------------------------------------------------------------
(defun analyse-function (function effect)
  (let* (( *static-level*            (if (local-fun-p function)
                                         (?level function)
                                         0))
         (*jump-level*  *static-level*))
    (analyse-params (?params function) effect)
    (analyse-form   (?body   function) effect)
    
    ;; Die lokalen Effekte werden entfernt.
    ;;-------------------------------------
    (setf (?read-list function) 
          (remove-local-effect (?read-list effect)))
    (setf (?write-list function)
          (remove-local-effect (?write-list effect)))
    (if (  > *static-level*  *jump-level*)
        
        ;; Die Funktion spring evtl zum Au"sen.
        ;;-------------------------------------
        (setf (?data-effects function) (?data-effects effect))
        
        ;; Es gibt nur evtl lokale Spr"unge. Remove-local-jump 
        ;; wird diesen lokalen JUMP-effekt entfernen.
        (setf (?data-effects function) 
              (remove-local-jump (?data-effects effect))))))

;;;------------------------------------------------------------------------
;;; Remove-local-jump entfernt aus DATA-EFFECTS den EFFEKT :JUMP.
;;;------------------------------------------------------------------------
(defun remove-local-jump (data-effects)
  (cond 
    ((eq data-effects :alloc-jump ) :alloc)
    ((eq data-effects :alloc-dest-jump) :alloc-dest)
    ((eq data-effects :dest-jump) :dest)
    ((eq data-effects :jump ) nil)
    (t data-effects)))


;;;-------------------------------------------------------------------------
;;; remove-local-effect dient zum Verstecken lokaler Seiteneffekte.
;;;-------------------------------------------------------------------------
(defun remove-local-effect (list/level)
  (if (listp list/level)
      
      ;; In diesem Fall werden alle lokale Variablen entfernt.
      ;;------------------------------------------------------
      (remove-if-not  #'(lambda (s) 
                          (or (dynamic-p s)
                              (and (static-p s)
                                   (< (?level s) *static-level*))))
                      list/level)
      
      ;; sonst enth"alt list/level eine Zahl : Diese Zahle ist eine Kodierung
      ;; der Menge aller Variablen, die ein statisches Niveau <= list/level
      ;; haben.
      ;;---------------------------------------------------------------------
      (if (<= *static-level* list/level)
          
          ;; d.h es sind lokale Variablen in der Kodierung von list/level
          ;; enthalten, die entfernt werden m"ussen. 
          ;; (*static-level* -1) kodiert die gr"ste Teilmenge von list/level
          ;; die keine lokalen Variablen enth"alt.
          ;;-----------------------------------------------------------------
          (1- *static-level*)
          
          ;; sonst sind keine lokalen Variablen enthalten
          list/level)))


;;;--------------------------------------------------------------------------
;;; Analyse eines angewandten Vorkommens einer Funktion aber nicht 
;;; auf Funktionsposition (d.h als Funktionales Objekt).
;;; Diese macht keine Seiteneffekte.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((function fun) effect)
  (declare (ignore effect)))
    
;;;--------------------------------------------------------------------------
;;; Hilfsfunktionen ...
;;;--------------------------------------------------------------------------

(defun non-local-jump (effect)
  (member (?data-effects effect) 
          '(:jump :alloc-jump :dest-jump :alloc-dest-jump) :test #'eq))


;;;--------------------------------------------------------------------------
;;; (STORE-EFFECT effect  fun/app)
;;; speichert den Effect  in den Slot effect der Funktion 
;;; oder der applikation 'fun/app' .
;;;---------------------------------------------------------------------------
(defun store-effect (effect fun/app)
  (setf (?read-list     fun/app)  (?read-list    effect))
  (setf (?write-list    fun/app)  (?write-list   effect))
  (setf (?data-effects  fun/app)  (?data-effects effect)))

;;;--------------------------------------------------------------------------
;;; (GET-EFFECT fun )
;;;  kopiert den Effekt einer Funktion 'fun' in eine Instance von EFFECT
;;;--------------------------------------------------------------------------
(defun get-effect (function)
  (make-instance 'effect
                 :read-list    (?read-list    function)
                 :write-list   (?write-list   function)
                 :data-effects (?data-effects function)))

(defun empty-effect ()
  (make-instance 'effect))

(defun top-effect (level)
  (make-instance 'effect
                 :read-list    level
                 :write-list   level
                 :data-effects :alloc-dest-jump))

;;;--------------------------------------------------------------------------
;;; Max-effect wird benutzt um die sichtbaren Effekte einer Funktion 
;;; zu berechnen.
;;; Seiteneffekte auf die lokale Variablen der Funktion sind nach aussen
;;; nicht sichtbar.
;;; Beachte den Unterschied zu unify-lists unten !!!
;;;-------------------------------------------------------------------------- 
(defun max-effect (list1 list2)
  (remove-duplicates
   (remove-if-not  #'(lambda (s) 
                       (or (dynamic-p s)
                           (and (static-p s)
                                (< (?level s) *static-level*))))
                   (append list1 list2)) :test #'eq))


;;;--------------------------------------------------------------------------
;;; Die kleinste obere Schranke zweier Elemente aus dem Verband 
;;; {nil, :alloc, :dest, :jump, :alloc-dest, :alloc-jump, :dest-jump,
;;; :alloc-dest-jump}
;;;--------------------------------------------------------------------------

(defun max-data-effects (d1 d2)
  (cond 
    ((eq d1 d2) d1)
    ((null d1) d2)
    ((null d2) d1)
    ((or (eq d1 :alloc-dest-jump) 
         (eq d2 :alloc-dest-jump)) :alloc-dest-jump)
    ((and (member d1 '(:alloc :dest :alloc-dest) :test #'eql)
          (member d2 '(:alloc :dest :alloc-dest) :test #'eql)) :alloc-dest )
    ((and (member d1 '(:alloc :jump :alloc-jump) :test #'eql)
          (member d2 '(:alloc :jump :alloc-jump) :test #'eql)) :alloc-jump)
    ((and (member d1 '(:jump :dest :dest-jump) :test #'eql)
          (member d2 '(:jump :dest :dest-jump) :test #'eql)) :dest-jump)
    (t :alloc-dest-jump)))

;;;----------------------------------------------------------------------------
;;; Berechnet die kelinste obere Schranke zweier Effekte (also Tripel
;;; (read-list , write-list , data-effects) ) aus dem Verband der Seiteneffekte
;;; dann entfernt die lokalen Effekte (lokal bzgl dem Level der zu
;;; analysierenden Funktion *current-function*).
;;;-----------------------------------------------------------------------------
(defun union-only-global-effects (target-effect effect1 effect2)
  (let ((read1  (?read-list  effect1))
        (read2  (?read-list  effect2))
        (write1 (?write-list effect1))
        (write2 (?write-list effect2)))
    (setf (?read-list target-effect )
          (if (and (listp read1) (listp read2))
              (max-effect read1 read2)
              (if (and (integerp read1) (integerp read2))
                  (min (1- (?level *current-function*))
                       (max read1 read2))
                  (if (integerp read1)
                      (min (1- *static-level*)
                           (combine-level-list read1 read2))
                      (min (1- *static-level*)
                           (combine-level-list read2 read1))))))
    
    (setf (?write-list target-effect)
          (if (and (listp write1) (listp  write2))
              (max-effect write1 write2)
              (if (and (integerp write1) (integerp write2))
                  (min (1- *static-level*)
                       (max write1 write2))
                  (if (integerp write1)
                      (min (1- *static-level*)
                           (combine-level-list write1 write2))
                      (min (1- *static-level*)
                           (combine-level-list write2 write1))))))
    (setf (?data-effects target-effect)
          (max-data-effects (?data-effects effect1) (?data-effects effect2)))))

;;;----------------------------------------------------------------------------
;;; Die Vereinigung einer Menge von Variablen "LIST" und einer Menge, 
;;; die durch ein statische Niveau "LEVEL"kodiert wird, ist das Maximum 
;;; aller statischen Levels aller statischen Variablen aus "LIST" 
;;; und "LEVEL".
;;;----------------------------------------------------------------------------
(defun combine-level-list (level list)
  (reduce #'(lambda (x var) (if (static-p var) (max x (?level var)) x))
          list
          :initial-value level))

;;;--------------------------------------------------------------------------
;;; union-all-effects wird benutzt, um die Effekte einer Applikation 
;;; zu berechnen. Sie vereinigt die read- und die write-listen der 
;;; applizierten Form mit den der Argumenten ohne den *Static-level*
;;; abzufragen (im gegenteil zu union-only-global-effects).
;;; Denn bei einer Applikation einer lokalen Funktion muessen auch die 
;;; lokale Variablen stehen die durch den Aufruf gelesen oder veraendert
;;; werden.
;;;--------------------------------------------------------------------------
(defun union-all-effects (target-effect effect1 effect2)
  (setf (?read-list  target-effect) (unify-lists (?read-list  effect1) 
                                                 (?read-list  effect2)))
  (setf (?write-list target-effect) (unify-lists (?write-list effect1)
                                                 (?write-list effect2)))
  (setf (?data-effects target-effect) 
        (max-data-effects (?data-effects effect1)
                          (?data-effects effect2))))


(defun unify-lists (list1 list2)
  (if (and (listp list1) (listp list2))
      (remove-duplicates 
       (append list1  list2)  :test #'eq)
      (if (and (integerp list1) (integerp list2))
          (max list1 list2)
          (if (integerp list1)
              (combine-level-list list1 list2)
              (combine-level-list list2 list1)))))

;;;--------------------------------------------------------------------------
;;; spec-vars-bound-to-funs : wird eine Funktion an der Variablen gebunden,
;;; die im Slot HAS-FUNS-AS-ARGS steht ?
;;; " Die Variablen die im Slot HAS-FUNS-AS-ARGS  stehen , sind diejenigen
;;; die im Rumpf der Funktion oder der globaleren umfassenden Funktionen
;;; nur auf Funktionsposition stehen ".
;;;--------------------------------------------------------------------------
(defun  spec-vars-bound-to-funs (app)
  (let* ((function  (?form app))
         (spec-vars (if (defined-fun-p function)
                        (remove-if-not 
                         #'(lambda (var)
                             (member var 
                                     (?all-vars (?params function)) 
                                     :test #'eq))
                         (?has-funs-as-args function))
                        (?has-funs-as-args function)))
         one-spec-var)
    (if  (null spec-vars)
         T
         (loop
          (setq one-spec-var (pop spec-vars))
          
          (let ((arg (if (defined-fun-p function)
                         (get-arg-from-param one-spec-var app)
                         (get-arg-from-coded-param one-spec-var app ))))
            (if (known-funs (get-result-position-forms arg) app)
                (if (endp spec-vars)
                    (return  T)
                    nil)
                (return nil)))))))

;;;--------------------------------------------------------------------------
;;; get-arg-from-coded-param liefert das Argument aus der Argumentliste 
;;; einer Applikation einer imported-fun mit spec-vars.
;;; Bei imported-funs ist der Slot HAS-FUNS-AS-ARGS kodiert.
;;; Get-result-position-forms liefert alle Ergebnisformen.
;;;--------------------------------------------------------------------------
(defun get-arg-from-coded-param (spec-var app)
  (if (numberp spec-var)
      
      ;; Hier handelt es sich um die Kodierung eines required oder optionalen
      ;; Parameters durch seine Position in der Parameterliste.
      ;;---------------------------------------------------------------------
      (let (( arg (nth spec-var (?arg-list app))))
        (if  (null arg) 
             ;; in diesem Fall ist ein optionaler Parameter nicht gebunden,
             ;; empty-list wird zur"uckgegeben.
             ;;------------------------------------------------------------
             empty-list
             ;; sonst wird das Argument zurueckliefert.
             arg))
      
      ;; Hier handelt es sich um keyword (nur das Symbol).
      ;; cadr von member liefert dann den eventuell an das keyword 
      ;; gebundene Parameter
      ;;----------------------------------------------------------
      (let ((arg (find spec-var (?arg-list app) :test #'eq)))
        (if (null arg)
            empty-list
            arg ))))

;;;--------------------------------------------------------------------------
;;; get-arg-from-param ist wie die get-arg-from-coded-param, aber 
;;; wo eine defined-fun appliziert wird .
;;; Der Unterschied liegt darin, dass in diesem Fall Variablen im
;;; Slot HAS-FUNS-AS-ARGS stehen, also noch keine Kodierung .
;;;--------------------------------------------------------------------------
(defun get-arg-from-param (spec-var app)
  (let ((function (?form app)))
    
    ;; Die Ueberpruefung der Bindungen findet nur statt, wenn die "spec-var"
    ;; aus der Parameterliste der applizierten Funktion ist, aber nicht 
    ;; wenn die Variable aus der Parameterliste einer der 
    ;; naechstglobaleren Funktionen ist.
    ;;----------------------------------------------------------------------
    (when (member spec-var (?all-vars (?params function)) :test #'eq)
      
      (let* ((is-key-var (find spec-var 
                               (?key-list (?params function)) 
                               :key #'?var :test #'eq))
             (is-opt-var (find spec-var 
                               (?opt-list (?params function))
                               :key #'?var :test #'eq)))
        
        (if is-key-var
            ;; falls die Variable fuer einen Keyword Parameter steht,
            ;; wird in der Argumentliste nach dem Keyword (symbol) 
            ;; gesucht und dann die darauffolgende Form ist dann das 
            ;; entsprechende Argument.
            ;;-------------------------------------------------------
            (let ((value (cadr (member (?sym is-key-var) 
                                       (?arg-list app) :test #'eq))))
              ;; wenn der Key Parameter gebunden ist wird das entsprechende
              ;; Argument zurueckgegeben, sont der Init-wert dieses 
              ;; Parameters aus der Parameterliste der Funktion.
              ;;-----------------------------------------------------------
              (if value
                  value
                  (?init is-key-var)))
            
            ;; sonst geht es um einen required oder optional Parameter.
            ;; Hier wird zunaechst die Position des Parameters in der 
            ;; parameterliste berechnet, dann wird in der Argumentliste 
            ;; an der gleichen Position nach dem Argument gesucht.
            ;;---------------------------------------------------------
            
            (let ((value (nth  (position spec-var 
                                         (?all-vars (?params function))
                                         :test #'eq)
                               
                               (?arg-list app))))
              ;; analog wie oben, falls VALUE nil ist, dann geht es um 
              ;; eienen optionalen Parameter, der nicht gebunden wird.
              ;; in dem Fall wird die Init-form aus dem Argument 
              ;; zurueckgeliefert.
              ;;------------------------------------------------------
              (if value
                  value
                  (?init is-opt-var))))))))

;;;--------------------------------------------------------------------------
;;; Ueberprueft ob das gelieferte Argument, oder alle Ergebnisformen davon
;;; Funktionen sind .
;;;--------------------------------------------------------------------------
(defun known-funs (list-of-forms app-form)
  (let ((app-effect (get-effect app-form)))
    (if (endp list-of-forms)
        T
        (let ((one-form (car list-of-forms)))
          (if (fun-p  one-form)
              (progn 
                ;; Diese Funktion steht zwar nicht auf 
                ;; Funktionsposition, wird aber appliziert,  
                ;; weil eine Spec-var an sie gebunden ist.
                ;;-------------------------------------------------------
                (when (defined-fun-p one-form)
                  (pushnew *current-function* (?mv-called-by one-form)
                           :test #'eq))
                (let ((effect (get-effect one-form)))
                  (when (non-local-jump effect)
                    (setf *jump-level* -1))
                  (union-all-effects app-form app-effect effect)  
                  (known-funs (cdr list-of-forms) app-form)))
              (if (or (and (var-ref-p one-form)
                           (member (?var one-form) 
                                   (?has-funs-as-args *current-function*) 
                                   :test #'eq))
                      (eq empty-list one-form))
                  
                  ;; Wenn das Argument eine spec-var der gerade analysierten 
                  ;; Funktion ist, wird in Analyse-form die Variable zu der
                  ;; READ-LIST hinzugef"ugt. 
                  ;; Wenn das Argument die empty-list handelt es sich um einen
                  ;; optionalen oder Key-word-Parameter, der nicht gebunden 
                  ;;; war. Hier auch werden die anderen Argumente verarbeitet.
                  ;;--------------------------------------------------------
                  (known-funs (cdr list-of-forms) app-form)
                  nil))))))


;;;--------------------------------------------------------------------------
;;; Analyse einer APPLICATION:
;;; Zunaechst wird der Seiteneffekt der applizierten Form berechnet
;;; FUN-EFFEKT.
;;; Dann werden die Argumente analysiert,deren Seiteneffkte in  ARG-EFFECT
;;; abgespeichert werden.
;;; Der Effekt der Applikation ist dann FUN-EFFECT vereinigt mit 
;;; ARG-EFFECT --> APP-EFFECT.
;;; APP-EFFECT wird zu dem Aktuellen EFFECT addiert.
;;;--------------------------------------------------------------------------

(defmethod analyse-form ((app-form app) effect)
  
  (let ((functional      (?form app-form))
        (fun-effect      (empty-effect))
        (arg-effect      (empty-effect)))
    
    
    (block calculate-app-effect
      (typecase functional 
        (defined-fun 
            
            ;; FALL1 die applizierte Funktion ist eine benutzer-definierte 
            ;; Funktion : 
            ;;     1.1 Die Error-funktion hat zur Zeit keine Seiteneffekte.
            ;;         (weil sie einfach die Berechnug stoppt).
            ;;     1.2 Die applizierte Funktion hat spec-vars, die an 
            ;;         Funktionen gebunden sein m"ussen, sonst hat die 
            ;;         Applikation top-effect. 
            ;;         (die "Uberpr"ufung der Bindungen, und das Setzen der 
            ;;         Slots read- ,write-list, data-effects  der Applikation,
            ;;         und des Slots called-by bei jeder der an diese Variablen
            ;;         gebundenen Funktionen erfolgt mit der Funktion 
            ;;         spec-vars-bound-to-funs. 
            ;;     1.3 Die Applizierte Funktion hat keine spec-vars, dann ist
            ;;         der Effekt dieser Funktion bekannt. 
            ;;----------------------------------------------------------------
            (if (eq functional *error-function*)
                
                (setf fun-effect (empty-effect))
                (progn 
                  (pushnew *current-function* 
                           (?mv-called-by functional) :test #'eq)
                  (if (?has-funs-as-args functional)
                      (if (spec-vars-bound-to-funs app-form)
                          (setf fun-effect (get-effect functional))
                          (let ((level (get-top-effect-level 
                                        *current-function*)))
                            (setf fun-effect (top-effect level))))
                      (setf fun-effect (get-effect functional)))))
            (when (non-local-jump fun-effect)
              (setf *jump-level* -1)))
        
        (imported-fun 
         
         ;; Fall2 ist analog zu Fall1, aber f"ur imported-fun.
         ;;---------------------------------------------------
         (if (eq functional *error-function*)
             (setf fun-effect (empty-effect))
             (if (?has-funs-as-args functional)
                 (if (spec-vars-bound-to-funs app-form)
                     (setf fun-effect (get-effect functional))
                     (let ((level (get-top-effect-level 
                                   *current-function* )))
                       (setf fun-effect (top-effect level))))
                 (setf fun-effect (get-effect functional))))
         
         (when (non-local-jump fun-effect)
           (setf *jump-level* -1)))
        (var-ref 
         
         ;; Fall3 die applizierte Form ist eine Variable :
         ;; falls die Variable schon in dem Slot HAS-FUNS-AS-ARGS
         ;; der gerade analysierten Funktion vorhanden ist,
         ;; dann  wird einfach die Variablen in die READ-LIST
         ;; eingetragen. (denn das funktionale Objekt wird an der
         ;; Aufrufstelle analysiert).
         ;; sonst wird TOP-EFFEKT.
         ;;----------------------------------------------------------
         (if   (member (?var functional )
                       (?has-funs-as-args *current-function*) 
                       :test #'eq)
               
               ;; ACHTUNG !!
               ;; wenn die applizierte Variable im Slot has-funs-as-args 
               ;; enthalten ist, dann ist sie aus der Argument Liste dieser
               ;; Funktion oder der naechst globaleren Funktionen und 
               ;; steht auf Funktionsposition und wird ausserdem nur 
               ;; nur gelesen oder so veraendert dass der neue Wert
               ;; nur eine aehnliche Variable oder eine bekannte Funktion
               ;; ist.
               ;; In diesem Fall besteht der Effekt dieser Applikation aus 
               ;; dem Lesen dieser Variablen. Die ganze Applikation wird
               ;; dadurch markiert da"s, der Slot OTHER-FUNS auf T gesetzt ist.
               ;;----------------------------------------------------------
               (progn 
                 (pushnew (?var functional) (?read-list fun-effect) :test #'eq)
                 (dolist (one-arg (?arg-list app-form))
                   (analyse-form one-arg arg-effect))
                 
                 (union-all-effects app-form (get-effect app-form) fun-effect)
                 (union-all-effects app-form (get-effect app-form) arg-effect)
                 (union-all-effects effect effect (get-effect app-form))
                 (return-from calculate-app-effect))
               
               ;; sonst ist keine Information ueber die Applizierte 
               ;; Variable vorhanden, also wird der Effekt auf TOP
               ;; gesetzt.
               ;;--------------------------------------------------
               (setf fun-effect (top-effect (get-top-effect-level 
                                             *current-function* ))))
         (setf *jump-level* -1))
        
        
        (cont  
         (setf *jump-level* (min *jump-level* (?level functional)))
         (setf (?data-effects fun-effect) :jump))
        (t
         
         ;; SONST 
         ;; werden alle Formen auf Ergebnis-position berechnet ,
         ;; mit Hilfe von GET-RESULT-POSITION-FORMS, und das 
         ;; Maximum deren Seiteneffekte als der Effekt des Funktionals
         ;; genommen.
         ;;------------------------------------------------------------
         (let ((local-effect (empty-effect)))
           (analyse-form functional local-effect)
           (let ((applied-forms (get-result-position-forms functional)))
             (dolist (one-applied-form applied-forms)
               (if (fun-p one-applied-form)
                   (let (( effect-of-applied-form (get-effect one-applied-form)))
                     (when (non-local-jump effect-of-applied-form)
                       (setf *jump-level* -1))
                     (when (defined-fun-p one-applied-form)
                       (pushnew *current-function* 
                                (?mv-called-by one-applied-form) :test #'eq))
                     (union-all-effects fun-effect fun-effect 
                                        effect-of-applied-form))
                   (if (cont-p  one-applied-form)
                       (progn 
                         (setf (?data-effects fun-effect ) :jump)
                         (setf *jump-level* 
                               (min *jump-level* (?level functional))))
                       (progn
                         (setf *jump-level* -1)
                         (setf fun-effect (top-effect 
                                           (get-top-effect-level
                                            *current-function*)))))))))))
      
      ;; Nun wird der Effekt der Argumente berechnet, dann wird 
      ;; der Effekt der Applikation aus dem Effekt des Funktionals
      ;; und dem der Argumente berechnet und in der Applikation 
      ;; abgespeichert.
      ;; der Effekt der Applikation wird am Ende mit dem Aktuellen
      ;; Effekt der Analyse vereinigt.
      ;;----------------------------------------------------------
      (dolist (one-arg (?arg-list app-form))
        (analyse-form one-arg arg-effect))
      (union-all-effects app-form (get-effect app-form) fun-effect)
      (union-all-effects app-form (get-effect app-form) arg-effect)
      (union-all-effects effect effect (get-effect app-form)))))

(defun get-top-effect-level (function)
  (if  (generates-closures function)
       (if (global-fun-p function)
           0
           (?level function))
       (if (global-fun-p function)
           -1
           (1- (?level function)))))


;;;--------------------------------------------------------------------------
;;; Methode zur Analyse von LET/CC
;;; Der Effekt ist der Effekt der Rumpf des Let/cc-Konstruktes.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ( (let/cc let/cc-form) effect)
  (analyse-form (?body let/cc) effect))

;;;--------------------------------------------------------------------------
;;; Angewandte Vorkommen von Continuations haben keine Seiteneffkte
;;; in unserem Sinne . (Sie verursachen einen Sprung aber beeinflussen 
;;; keine Variablen).
;;;-------------------------------------------------------------------------
(defmethod analyse-form ((continuation cont) effect)
  (declare (ignore effect)))


;;;--------------------------------------------------------------------------
;;; Analyse der Parameter einer Funktion.
;;; Es werden alle Init-formen der Optionalen bzw key-Parameter analysiert.
;;;--------------------------------------------------------------------------
(defun  analyse-params (parameter  effect)
  (dolist (opt-param (?opt-list parameter))
    (analyse-form (?init opt-param)  effect))
  (dolist (key-param (?key-list parameter))
    (analyse-form (?init key-param) effect)))

;;;--------------------------------------------------------------------------
;;; Methode zur Analyse von mv-lambda.
;;;--------------------------------------------------------------------------
(defmethod analyse-form ((lambda mv-lambda) effect)
  (analyse-params (?params lambda) effect)
  (analyse-form   (?body   lambda) effect)
  (analyse-form   (?arg    lambda) effect))
;;;--------------------------------------------------------------------------

(defmethod analyse-form ((anything-else form) effect)
  (declare (ignore effect)))
;;;--------------------------------------------------------------------------
(provide "static-effect")



