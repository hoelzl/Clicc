;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Project    :CLICC ein Commmon LIsp to C Compiler
;;;             ------------------------------------
;;; Dateiname  : CLOSURE-ANALYSIS.LISP
;;; Funktion   : Die vorbereitung fuer die Seiteneffektanalyse, sowie fuer 
;;;              verschiedene Optimierungsverfahren.
;;;              1) Es wird eine Liste der im Module enstandenen Closures 
;;;                 erstellt.
;;;              2) Die  Closures werden analysiert, und attributiert, mit 
;;;                 Informationen die zur Codegenerierung sowie fuer die 
;;;                 Optimierung der Tail-rekursion nuetzlich sind.
;;; Autor      : Anouar Trigui
;;; $Revision: 1.44 $
;;; $Log: closure-analysis.lisp,v $
;;; Revision 1.44  1994/04/28  17:15:37  hk
;;; (null (?initform one-slot-descr)) erg"anzt
;;;
;;; Revision 1.43  1994/02/09  11:57:30  atr
;;; Die Variable *result-position-cont-list* wird nicht mehr bei
;;; get-result-position-form verwendet. Ein Schreibfehler bei
;;; (get-result-position-forms tagbody) behoben.
;;;
;;; Revision 1.42  1994/02/08  14:46:18  atr
;;; Die Methode get-result-position-forms verbessert. Jetzt werden auch
;;; die Formen die nicht auf Resultatsposition stehen untersucht, und alle
;;; Applikationen von Continuations untersucht.
;;;
;;; Revision 1.41  1994/02/04  10:48:09  hk
;;; get-fun-vars korrigiert und aufgeraeumt. Wenn Parameter einander
;;; zugewiesen werden, dann wird dies durch eine Alias-Relation
;;; ausgedrueckt. Damit wirken Aenderungen im Status eines Parameters
;;; automatisch auf den Status seiner Aliase.
;;;
;;; Revision 1.38  1994/02/03  12:30:37  hk
;;; Bei der Analyse für has-funs-as-args ist eine Zuweisung einer Funktion
;;; an einen Parameter nur dann unschädlich, wenn die Funktion
;;; seiteneffektfrei ist.
;;;
;;; Revision 1.37  1994/02/03  10:52:00  hk
;;; Fehlendes get-fun-vars-of-params in der Methode get-fun-vars für
;;; mv-lambda eingefügt.
;;;
;;; Revision 1.36  1994/02/03  10:21:47  ft
;;; Default-Methode zu find-closures spezialisiert jezt über form.
;;;
;;; Revision 1.35  1994/01/28  12:56:10  ft
;;; Ausnahmetest in get-fun-vars(class) auf nil erweitert.
;;;
;;; Revision 1.34  1994/01/26  13:36:17  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.33  1994/01/21  16:28:53  ft
;;; Default-Methode von find-closures zu einer Default-Methode gemacht.
;;;
;;; Revision 1.32  1994/01/10  12:43:38  atr
;;; Zwei Aufrufe (get-function 'clicc-lisp::error) durch *error-function*
;;; ersetzt.
;;;
;;; Revision 1.31  1994/01/03  11:55:24  atr
;;; Die Local-funs-Slots werden jetzt vor der tail-rekursion gesetzt, und
;;; nicht mehr während der Voranalyse.
;;;
;;; Revision 1.30  1993/12/17  10:12:46  hk
;;; Für CMU: Lokale Funktion all-vars-or-funs aus der Methode
;;; (get-fun-vars setq-form) herausgezogen.
;;;
;;; Revision 1.29  1993/11/09  16:39:55  atr
;;; Die globalen Variablen stehen jetzt in se-init-lisp.
;;;
;;; Revision 1.28  1993/10/25  10:03:28  atr
;;; Get-result-position-forms einer app ist die app selbst, es wird nicht
;;; weiter auf sonderf"alle geachtet.
;;;
;;; Revision 1.27  1993/10/18  22:23:38  atr
;;; Code etwas verbessert, so da"s die Voranalyse etwas schneller l"auft.
;;;
;;; Revision 1.26  1993/10/14  17:03:32  atr
;;; Die Variable *map-functions* wird hier nicht mehr benutzt, stattdessen
;;; wird der Slot ?special-cller bei special-defined-funs benutzt.
;;;
;;; Revision 1.25  1993/10/13  17:09:41  atr
;;; Eine Methode find-closures f"ur VAR eingef"ugt.
;;;
;;; Revision 1.24  1993/10/13  14:44:37  atr
;;; Dei Spezialisierung uber T durch Spezialisierung "uber Form ersetzt,
;;; und Methoden f"ur CONT eingef"ugt.
;;;
;;; Revision 1.23  1993/10/11  10:41:11  atr
;;; Die Funktion list-the-special-defined-funs und die Variable
;;; *special-defined-funs* entfernt.
;;;
;;; Revision 1.22  1993/10/08  10:11:14  hk
;;; Fehler behoben: 'get-fun-vars' wird nun auch auf die 1. Komponente
;;; einer Applikation angewendet, wenn dieses eine Variable ist.
;;; Aus der Methode für params eine eigene Funktion gemacht.
;;;
;;; Revision 1.21  1993/10/08  09:16:34  hk
;;; Fehler in der letzten Änderung behoben
;;;
;;; Revision 1.20  1993/10/07  15:33:16  hk
;;; Die Komponente ?free-lex-vars von lokalen Funktionen wird während der
;;;  Ausführung von 'get-fun-vars' für die Typinferenz gesetzt.
;;;
;;; Revision 1.19  1993/10/07  13:59:35  hk
;;; special Variablen aufgeräumt.
;;;
;;; Revision 1.18  1993/09/06  15:10:32  atr
;;; Die Deklaration der Variablen *current-function* steht jetzt in
;;; static-analysis.
;;;
;;; Revision 1.17  1993/08/25  14:05:10  atr
;;; Die Voranalyse ist jetzt besser. Die Variablen die auf
;;; Funktionsposition aber durch eine Setq modifiziert werden, werden jetzt
;;; auch in den Slot HAS-FUNS-AS-ARGS eingetragen, falls der Wert bei der Setq
;;; eine andere Variable, die im Slot schon vorhanden ist oder eine bekannte 
;;; Funktion ist.
;;;
;;; Revision 1.16  1993/07/27  14:01:30  atr
;;; Neue Funktion generates-closures implmentiert,die abfragt
;;; ob in einer Funktion closures erzeugt werden.
;;;
;;; Revision 1.15  1993/07/21  12:27:37  atr
;;; Funktionen besser kommentiert.
;;;
;;; Revision 1.14  1993/07/13  16:22:06  atr
;;; Level bei dynamischen Variablen wird nicht gesetzt.
;;;
;;; Revision 1.13  1993/07/13  14:40:06  hk
;;; Lokale Aenderung von Annouar, die auch den Slot level der Variablen
;;; eines mv-lambda setzt.
;;;
;;; Revision 1.12  1993/07/13  11:08:48  atr
;;; Nun werden die Slots LEVEL bei lokalen Funktionen
;;; und bei Variablen hier gesetzt.
;;;
;;; Revision 1.11  1993/07/06  13:17:44  atr
;;; Unnoetige Ausgabe entfernt.
;;;
;;; Revision 1.10  1993/06/30  16:49:57  atr
;;; Ausgabe -preparing side effect analysis- entfernt.
;;;
;;; Revision 1.9  1993/06/26  16:01:30  atr
;;; (require static-effect) entfernt .
;;;
;;; Revision 1.8  1993/06/26  13:33:33  atr
;;; Neue Methoden geschrieben zur Attributierung der SPEC-DEFINED-FUNS
;;; als Vorbereitung zur Seiteneffektanalyse.
;;;
;;; Revision 1.7  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.6  1993/06/14  10:54:16  atr
;;; Unnoetige Ausgabe entfernt.
;;;
;;; Revision 1.5  1993/06/14  10:47:15  atr
;;; Funktionsdefinitionen verbessert.
;;;
;;; Revision 1.4  1993/06/09  12:21:30  atr
;;; delete durch remove ersetzt, denn delete ist destruktiv.
;;;
;;; Revision 1.3  1993/05/30  14:04:24  atr
;;; Analyse fuer special-defined-functions (also Funktionen
;;; die immer ein oder mehrere Parameter haben , die nur auf Funktions-
;;; position stehen) Diese Funktionen werden anderes analysiert als normale
;;; Funktionen.
;;;
;;; Revision 1.2  1993/04/30  09:37:12  hk
;;; Revision-Keyword eingetragen.
;;;
;;; Revision 1.1  1993/04/27  10:53:56  atr
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(defvar *not-on-result-position* nil)  
;;----------------------------------------------------------------------------
;; Die zwei Variablen enthalten die Continuations, die in einem 
;; auf Resultatsposition  bzw in einem nicht auf Resultatsposition 
;; auftretenden Let/cc-Konstruk definiert worden sind.
;;----------------------------------------------------------------------------
(defvar *result-pos-cont-list* nil)    
(defvar *not-on-result-pos-cont-list* nil) 

;;----------------------------------------------------------------------------
;; Bei einer If-Konstrukt sind die THEN und ELSE_Zweige auf Resultatsposition.
;; Das Prädikat steht zwar nicht auf resultatsposition, kann aber eine 
;; Applikation einer Continuation enthalten. Diese Applikation kann dann zum
;; Verlassen des If-Konstruktes mit einem Resultat.
;;----------------------------------------------------------------------------
(defmethod get-result-position-forms ((form if-form))
  (let ((returned-forms 
         (let ((*not-on-result-position* T))
           (get-result-position-forms (?pred form)))))
    
    (append returned-forms
            (get-result-position-forms (?then form))
            (get-result-position-forms (?else form)))))

;;----------------------------------------------------------------------------
;; Der Resultatswert eines Progn-Konstruktes ist der Wert der letzten 
;; Form. Die anderen Formen werden nach Appliaktionen von Continuations,
;; die das Verlassen des Progn-Konstruktes verursachen können mit einem 
;; Rückgabewert.
;;----------------------------------------------------------------------------
(defmethod get-result-position-forms ((form progn-form))
  (let ((returned-forms nil))
    (let ((*not-on-result-position* T))
      (dolist (one-form (butlast (?form-list form)))
        (setq returned-forms 
              (append returned-forms
                (get-result-position-forms one-form)))))
    (append returned-forms
            (get-result-position-forms (car (last (?form-list form)))))))

;;------------------------------------------------------------------------------
;; Das Tagbody-Konstrukt liefert als Resultat den Wert der letzten 
;; Tagged-form. Andere Resultatswerte können explizit durch eine Applikation 
;; einer Continuation.
;;------------------------------------------------------------------------------
(defmethod get-result-position-forms ((form tagbody-form))
  (let ((returned-forms nil))
    (let ((*not-on-result-position* T))
      (setq returned-forms (get-result-position-forms (?first-form form)))
      (dolist (one-tagged-form (butlast (?tagged-form-list form)))
        (setq returned-forms 
              (append returned-forms 
                      (get-result-position-forms (?form one-tagged-form))))))
    (append returned-forms (get-result-position-forms 
                            (?form (car (last (?tagged-form-list form))))))))

(defmethod get-result-position-forms ((form tagged-form)))

;;-----------------------------------------------------------------------------
;; Die Init-formen eines Let*-Konstruktes stehen nicht auf Resultatsposition.
;; Diese werden nach Applikationen von Continuations, die das Verlassen 
;; des Let*-Konstruktes mit einem Rückgabewert verursachen.
;;----------------------------------------------------------------------------
(defmethod get-result-position-forms ((form let*-form))
  (let ((returned-forms nil))
    (let ((*not-on-result-position* T))
      (dolist (one-form (?init-list form))
        (setq returned-forms 
              (append returned-forms
                      (get-result-position-forms one-form)))))
    (append returned-forms 
            (get-result-position-forms (?body form)))))

;;--------------------------------------------------------------------------
;; Bei einer Switch-form werden alle Ergebnis-formen jeder labelled Form
;; zusammen mit der Ergebnis-form der Otherwise-form zurueckgegeben.
;;-------------------------------------------------------------------------- 
(defmethod get-result-position-forms ((form switch-form))
  (let ((app-nodes (get-result-position-forms (?otherwise form))))
    (dolist (one-case (?case-list form) app-nodes)
      (setq app-nodes 
            (append app-nodes 
                    (get-result-position-forms (?form one-case)) app-nodes)))))

;;-------------------------------------------------------------------------- 
;; Wenn ein Let/cc-Konstrukt nicht auf Resultatsposition steht, wird die Cont
;; zu der Liste *not-on-result-position-cont-list* hinzugefügt. 
;; Das Resultat einer Applikation dieser Continuation ist kein Resultatswert 
;; des analysierten Ausdrucks.
;;--------------------------------------------------------------------------
(defmethod get-result-position-forms ((form let/cc-form))
  (if *not-on-result-position* 
      (let ((*not-on-result-pos-cont-list* 
             (cons  (?cont form) *not-on-result-pos-cont-list*)))
        (get-result-position-forms (?body form)))
      (get-result-position-forms (?body form))))

;;--------------------------------------------------------------------------
;; Bei der Applikation einer Continuation wird das 
;; Argument zurueckgegeben, sonst die ganze Applikation.
;;--------------------------------------------------------------------------
(defmethod get-result-position-forms ((form app))
  (let* ((funcall-fun (get-global-fun 'funcall))
         (functional  (if (eq (?form form) funcall-fun)
                          (car (?arg-list form))
                          (?form form)))
         (arg-list    (if (eq (?form form) funcall-fun)
                          (cdr (?arg-list form))
                          (?arg-list form))))
    (if *not-on-result-position* 
        (cond 
          ((and (cont-p functional) 
                (not (member functional *not-on-result-pos-cont-list*)))
           arg-list)
          (T ()))
        (cond 
          ((and (cont-p functional) 
                (not  (member functional *not-on-result-pos-cont-list*)))
           arg-list)
          (T (list form))))))

;;--------------------------------------------------------------------------
;; Bei einem Labels Konstrukt wird der Rumpf traversiert.
;;--------------------------------------------------------------------------
(defmethod get-result-position-forms ((form labels-form))
  (get-result-position-forms (?body form)))

;;--------------------------------------------------------------------------
;; Hier geht es nicht um Applikationen von Funktionen, sondern um andere 
;; Angewandte Vorkommen von Funktionen, aber nicht auf Funktionsposition.
;;--------------------------------------------------------------------------
(defmethod get-result-position-forms ((form fun))
  (unless *not-on-result-position*
    (list form)))
  
(defmethod get-result-position-forms ((form mv-lambda))
  (get-result-position-forms (?body form)))

(defmethod get-result-position-forms ((form setq-form))
  (get-result-position-forms (?form form))) 

(defmethod get-result-position-forms ((a-cont cont))
  (unless *not-on-result-position*
    (list a-cont)))

(defmethod get-result-position-forms ((any-thing form))
  (unless *not-on-result-position*
    (list any-thing)))

;;-----------------------------------------------------------------------
;; get-fun-on-result-position liefert die Liste der Closures die bei 
;; der gegebenen Form auf Ergebnisposition stehen, wenn vorhanden, sonst
;; nil.
;;-----------------------------------------------------------------------
(defun get-fun-on-result-position (form)
  (remove-if-not #'fun-p (get-result-position-forms form)))

;;-----------------------------------------------------------------------
;; The-applied-function nimmt eine app und liefert die applizierte Form
;; zurueck.
;;-----------------------------------------------------------------------
(defun the-applied-function (app)
  (let ((functional (?form app))
        (arg-list   (?arg-list app)))
    (case functional 
      (defined-fun-p functional)
      
      ;; Bei einer Applikation durch FUNCALL oder APPLY oder bei einer 
      ;; Iteration durch MAP* wird die applizierte Form zurueckgegeben
      ;; und nicht die Funktion Funcall selbst.
      ;; sonst wird die applizierte Funktion zurueckgegeben.
      ;;----------------------------------------------------------------
      (imported-fun-p functional)
      (special-sys-fun-p 
       (if  (?special-caller functional )
            (the-applied-function (make-instance 'app
                                                 :form (first arg-list)
                                                 :arg-list 
                                                 (cdr (?arg-list app))))
            functional))
      
      ;; Bei einer cont-app wird nil zurueckgegeben.
      ;;---------------------------------------------
      (cont-p nil)
      (app-p  (the-applied-function (?form functional)))
      
      ;; anderenfalls werden alle Formen gesammelt, die auf 
      ;; Resultatsposition stehen.
      ;;---------------------------------------------------
      (t (get-result-position-forms functional)))))

;;------------------------------------------------------------------------
;; Get-applied-functions traversiert eine Form, und liefert alle 
;; applizierten Formen bei Applikationen, die auf Ergebnisposition stehen.
;;------------------------------------------------------------------------
(defun get-applied-functions (form)
  (let ((applied-functions nil))
    (dolist (one-res-form (get-result-position-forms form))
      (when (app-p one-res-form)
        (setq applied-functions (append applied-functions
                                        (the-applied-function one-res-form)))))
    applied-functions))
 
;;------------------------------------------------------------------------------
;; Analyse der Closures in einem Module.
;;------------------------------------------------------------------------------
(defun get-closures-of-module ()
  (let ((*se-var-env*        nil)
        (*se-vars-to-funs*   nil))
    (mapcar #'get-closures-of-function (?fun-list *module*))))

(defun get-closures-of-function (function)
  (find-closures (?body function)))

(defun generates-closures (function)
  (let ((*upward-fun-args* nil)
        (*down-fun-args*   nil))
    (dolist (one-opt-par (?opt-list (?params function)))
      (find-closures (?init one-opt-par)))
    (dolist (one-key-par (?key-list (?params function)))
      (find-closures (?init one-key-par)))
    (find-closures (?body function))
    (if *upward-fun-args* 
        T
        nil)))

(defmethod find-closures ((ref var-ref)))
  
(defmethod find-closures ((form if-form))
  (find-closures (?pred form))
  (find-closures (?then form))
  (find-closures (?else form)))

(defmethod find-closures ((form progn-form))
  (dolist (one-form (?form-list form))
    (find-closures one-form)))
                
(defmethod find-closures ((form tagbody-form))
  (let ((tagged-form-list (?tagged-form-list form)))
    (dolist (one-tagged-form tagged-form-list)
      (find-closures (?form one-tagged-form)))
    (find-closures (?first-form form))))


(defmethod find-closures ((form tagged-form)))


(defmethod find-closures ((let*form let*-form))
  (find-closures (?body let*form))
  (dolist (one-init-form (?init-list let*form))
    (find-closures one-init-form)))

(defmethod find-closures ((one-class-def class-def) )
  (dolist (one-slot-descr (?slot-descr-list one-class-def))
    (unless (or (null (?initform one-slot-descr))
                (eq (?initform one-slot-descr) :unbound))
      (find-closures (?initform one-slot-descr )))))

(defmethod find-closures ((switch switch-form))
  (find-closures (?otherwise switch))
  (let ( (case-list    (?case-list switch)))
    (dolist (one-case case-list)
      (find-closures (?form one-case)))))

(defmethod find-closures ((let/cc let/cc-form))
  (find-closures (?body let/cc)))

(defmethod find-closures ((labels labels-form))
  (dolist (one-local-fun (?fun-list labels))
    (pushnew one-local-fun (?local-funs *current-function*))
    (find-closures (?body one-local-fun)))
  (find-closures (?body labels)))


(defmethod find-closures ((an-app app))
  (let* ((functional      (?form     an-app))
         (arg-list        (?arg-list an-app)))
    
    (when  (and (special-sys-fun-p  functional)
                (?special-caller functional))
      (setq functional (first arg-list))
      (setq arg-list (cdr arg-list)))
    (unless  (or (var-ref-p functional)
                 (fun-p functional))
      (let ((down-fun-list (remove-if-not 
                            #'defined-fun-p
                            (get-result-position-forms functional))))
        (setq *down-fun-args* (remove-duplicates 
                               (append 
                                *down-fun-args* 
                                down-fun-list)))))
    (dolist (one-arg arg-list)
      (find-closures one-arg))))

(defmethod find-closures ((function defined-fun))
  (pushnew function *upward-fun-args*))
  
(defmethod find-closures ((function local-fun))
  (pushnew  function *upward-fun-args*)) 

(defmethod find-closures ((a-mv-lambda mv-lambda))
  (find-closures (?body a-mv-lambda))
  (find-closures (?arg  a-mv-lambda)))

(defmethod find-closures ((a-setq setq-form))
  (find-closures (?form a-setq)))

(defmethod find-closures ((a-cont cont)))

(defmethod find-closures ((a-form form)))

;;------------------------------------------------------------------------------
;; Die vorbereitung zur Seiteneffektanalyse. Es wird untersucht, ob die
;; Parameter einer funktion die Eigenschaft E haben. Ein Parameter p der
;; Funktion f hat die Eigenschaft E genau dann wenn:
;; p mindestens einmal als Funktion angewendet wird oder einer Funktion als
;; Argument übergeben wird, wo der zugehörige Parameter die Eigenschaft E hat
;; und wenn p nur Werte zugewiesen werden, die Literale oder seiteneffektfreie
;; Funktionen oder Werte anderer Parameter mit der Eigenschaft E sind.
;;
;; Die Eigenschaft E eines Parameters p der Funktion f wird in der
;; nachfolgenden Seiteneffektanalyse verwendet: Aufrufe von p als Funktion
;; werden zunächst als seiteffektfrei klassifiziert und als Ausgleich werden
;; an den Aufrufstellen von f die Seiteneffekte der an p gebundenen Funktionen
;; verwendet. Ohne diese Analyse würde für alle Aufrufe von p der größte
;; Seiteneffekt angenommen werden.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Eine A-Liste, die für die Variablen der Paramterliste der aktuellen und der
;; umfassenden Funktionen angibt, welchen Status sie hat (nil, :FUN-ARG-CALL,
;; :UNKNOWN-CALL) oder mit welcher anderen Variablen sie in Alias-Relation
;; steht.
;;------------------------------------------------------------------------------
(defvar *fun-vars-info*)

;;------------------------------------------------------------------------------
;; Info für die Typinferenz bereitstellen:
;; Wenn 'var' frei in der momentan uebersetzten Funktion vorkommt, wird 'var'
;; unter 'free-lex-vars' vermerkt.
;;------------------------------------------------------------------------------
(defun se-check-if-free-lex-var (var)
  (when (< (?level var) *static-level*)
    (pushnew var (?free-lex-vars *current-function*))))

;;------------------------------------------------------------------------------
;; Setzt den Level der statisch gebundenen Variablen in var-list
;;------------------------------------------------------------------------------
(defun set-levels (var-list)
  (dolist (var var-list)
    (when (static-p var)
      (setf (?level var) *static-level*))))

;;------------------------------------------------------------------------------
;; Funktionen zur Verwaltung der Alias Beziehung von Variablen
;;------------------------------------------------------------------------------
(defun get-alias (var.value)
  (if (consp (cdr var.value))
      (get-alias (cdr var.value))
      var.value))

(defun get-var-alias (var)
  (get-alias (assoc var *fun-vars-info*)))

;;------------------------------------------------------------------------------
;; Diese Methode traversiert jede Funktion, und sammelt alle Variablen die
;; nur auf Funktionsposition stehen. Diese Variablen werden in dem Slot
;; HAS-FUNS-AS-ARGS abgespeichert.
;;------------------------------------------------------------------------------
(defun pre-analyse-module ()
  (let ((*se-fun-counter* 0)
        (*static-level* 0))
    
    (pre-analyse-defun (?toplevel-forms *module*))
    (mapc #'pre-analyse-defun (?fun-list *module*))
    (clicc-message "~s special functions found " *se-fun-counter*)))

;;------------------------------------------------------------------------------
;; Analyse globaler Funktionen
;;------------------------------------------------------------------------------
(defun pre-analyse-defun (fun)
  (let* ((*current-function* fun)
         (*fun-vars-info* ()))
    (get-fun-vars-of-fun fun)))

;;-----------------------------------------------------------------------------
;; Bei einem Labels-Ausdruck werden zunaechst die lokale Funktionen
;; analysiert und attributiert und dann wird die gerade analysierte 
;; Funktion weiter analysiert.
;;-----------------------------------------------------------------------------
(defmethod get-fun-vars ((labels labels-form))
  (let ((*static-level* (1+ *static-level*)))
    (dolist (fun (?fun-list labels))
      (setf (?free-lex-vars fun) ())
      (let* ((*current-function* fun))
        (get-fun-vars-of-fun fun)
        
        ;; Propagieren der FREIEN lokalen lexikalischen Var. für Typinferenz
        ;;------------------------------------------------------------------
        (dolist (local-fun (?local-funs fun))
          (mapc #'se-check-if-free-lex-var (?free-lex-vars local-fun))))))
  
  (get-fun-vars (?body labels)))

;;------------------------------------------------------------------------------
;; Analyse von Funktionen. In den Slot has-funs-as-args werden die Parameter
;; eingetragen, die als Funktion aufgerufen werden.
;;------------------------------------------------------------------------------
(defun get-fun-vars-of-fun (fun)
  (let ((*fun-vars-info* *fun-vars-info*))
    (dolist (param (?all-vars (?params fun)))
      (when (static-p param)
        (push (cons param nil) *fun-vars-info*)))
  
    (get-fun-vars-of-params (?params fun))
    (get-fun-vars (?body fun))

    (setf (?has-funs-as-args fun) ())
    (dolist (arg-info *fun-vars-info*)
      (when (eq :FUN-AS-ARG-CALL (cdr (get-alias arg-info)))
        (push (car arg-info) (?has-funs-as-args fun))))
    (when (?has-funs-as-args fun)
      (incf *se-fun-counter*))))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((an-app app))
  (let* ((form (?form an-app))
         (arg-list (?arg-list an-app)))
    (labels (
             ;;----------------------------------------------------------------
             ;; Prüft, ob eine Variable ein Parameter ist und damit potentiell
             ;; eine Funktion enthält und trägt für die Variable und ihre
             ;; Aliase den Wert :FUN-AS-ARG-CALL ein, wenn noch kein anderer
             ;; Wert eingetragen worden ist.
             ;;----------------------------------------------------------------
             (set-param-call (var-ref )
               (let ((pair (get-var-alias (?var var-ref))))
                 (when (and pair (null (cdr pair)))
                   (setf (cdr pair) :FUN-AS-ARG-CALL)))))
  
      (typecase form
        (defined-fun
            (when (?has-funs-as-args form)
              (dolist (var (?has-funs-as-args form))
                (let ((arg (get-arg-from-param var an-app)))
                  (when (var-ref-p arg)
                    (set-param-call arg))))))

        (imported-fun
         (when (?has-funs-as-args form)
           (dolist (one-spec-var (?has-funs-as-args form))
             (let ((arg (get-arg-from-coded-param one-spec-var an-app)))
               (when (var-ref-p arg)
                 (set-param-call arg))))))
      
        (var-ref (set-param-call form)))
    
      (get-fun-vars form)
      (dolist (one-arg arg-list)
        (get-fun-vars one-arg)))))

;;------------------------------------------------------------------------------
;; Bei einer Zuweisung an einen Parameter können folgende Situationen auftreten:
;; - Der neue Wert ist ein Literal, eine seiteneffektfreie Funktion oder ein
;;   Aufruf der Funktion ERROR. Dann bleibt der Status des Parameters
;;   unverändert.
;; - Der neue Wert ist der Wert eines anderen Parameters. Dann werden die beiden
;;   Parameter als Aliase betrachtet und jede Änderung im Status des einen
;;   Parameters verändert den Staus des anderen Parameters auf gleiche Weise.
;; - Sonst wird der Staus zu :UNKNOWN-CALL, d.h. für den Parameter erfolgt keine
;;   Sonderbehandlung.
;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((a-setq setq-form))
  (labels ((max-fun-vars-info (v1 v2)
             (cond
               ((null v1) v2)
               ((null v2) v1)
               ((eq :UNKNOWN-CALL v1) v1)
               ((eq :UNKNOWN-CALL v2) v2)
               (T v1))))                ; v1 = v2 = :FUN-AS-ARG-CALL
                     
    (let* ((var (?var (?location a-setq)))
           (pair (get-var-alias var)))
      
      (when (local-static-p var)
        (se-check-if-free-lex-var var))

      (when pair
        (unless (eq (cdr pair) :UNKNOWN-CALL)

          ;; Zuweisung an einen Parameter, der entweder als Funktion aufgerufen
          ;; wird oder für den noch nichts bekannt ist.
          ;;-------------------------------------------
          (dolist (one-form (get-result-position-forms (?form a-setq)))
            (cond
              ((var-ref-p one-form)

               ;; Bei Zuweisungen von Variablen (setq v1 v2) müssen die
               ;; statischen Niveaus betrachtet werden.
               ;; Falls level(v1) <= level(v2), dann kann Aliasing erfolgen,
               ;; sonst bekommt v1 den Status :UNKNOWN-CALL, da bis zum Ende
               ;; der Analyse der aktuellen lokalen Funktion der Status von v2
               ;; noch nicht endgültig feststeht.
               ;;--------------------------------
               (let ((pair2 (get-var-alias (?var one-form))))
                 (cond                   
                   ((and pair2 (<= (?level var) (?level (?var one-form))))

                    (unless (eq pair pair2)
                      
                      ;; Aliasing der Variablen auf der rechten und linken
                      ;; Seite der Zuweisung 
                      ;;---------------------
                      (let ((value (cdr pair2)))
                        (setf (cdr pair2) pair)
                        (setf (cdr pair)
                              (max-fun-vars-info value (cdr pair))))))
               
                   (t (setf (cdr pair) :UNKNOWN-CALL)))
                 (when (eq (cdr pair) :UNKNOWN-CALL)
                   (return))))
          
              ((or (literal-p one-form)

                   ;; Zu diesem Zeitpunkt wissen wir nur von importierten
                   ;; Funktionen, ob sie Seiteneffektfrei sind.
                   ;;------------------------------------------
                   (and (imported-fun-p one-form)
                        (not (or (?read-list    one-form)
                                 (?write-list   one-form)
                                 (?data-effects one-form))))
                                   
                   (and (app-p one-form)
                        (eq (?form one-form) *error-function*)))
               nil)
          
              (T (setf (cdr pair) :UNKNOWN-CALL)
                 (return)))))))
  
    (get-fun-vars (?form a-setq))))

;;------------------------------------------------------------------------------
;; Analyse der Parameterliste von Funktionen und mv-lambda
;;------------------------------------------------------------------------------
(defun get-fun-vars-of-params (params)
  (set-levels (?all-vars params))
  (dolist (one-opt (?opt-list params))
    (get-fun-vars (?init one-opt)))
  (dolist (one-key (?key-list params))
    (get-fun-vars (?init one-key))))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((fun fun)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((ref var-ref))
  (when (local-static-p (?var ref))
    (se-check-if-free-lex-var (?var ref))))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((form if-form))
  (get-fun-vars (?pred form))
  (get-fun-vars (?then form))
  (get-fun-vars (?else form)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((form progn-form))
  (dolist (one-form (?form-list form))
    (get-fun-vars one-form)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((form tagbody-form))
  (get-fun-vars (?first-form form))
  (dolist (tagged-form (?tagged-form-list form))
    (get-fun-vars (?form tagged-form))))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((form tagged-form)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((let*-form let*-form))
  (set-levels (?var-list let*-form))
  (dolist (one-init-form (?init-list let*-form))
    (get-fun-vars one-init-form))
  (get-fun-vars (?body let*-form)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((one-class-def class-def) )
  (dolist (one-slot-descr (?slot-descr-list one-class-def))
    (unless (or (null (?initform one-slot-descr))
                (eq (?initform one-slot-descr) :UNBOUND))
      (get-fun-vars (?initform one-slot-descr)))))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((switch switch-form))
  (get-fun-vars (?form switch))
  (dolist (one-case (?case-list switch))
    (get-fun-vars (?form one-case)))
  (get-fun-vars (?otherwise switch)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((let/cc let/cc-form))
  (get-fun-vars (?body let/cc)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((a-cont cont)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((mv-lambda mv-lambda))
  (get-fun-vars-of-params (?params mv-lambda))
  (get-fun-vars (?body mv-lambda))
  (get-fun-vars (?arg  mv-lambda)))

;;------------------------------------------------------------------------------
(defmethod get-fun-vars ((any-thing-else form)))

;;--------------------------------------------------------------------------
(provide "closure-analysis")    
