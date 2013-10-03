;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Special forms
;;;
;;; $Revision: 1.45 $
;;; $Log: p1spform.lisp,v $
;;; Revision 1.45  1994/02/18  13:42:55  hk
;;; let*-form wird nur generiert, wenn var-list nicht leer ist.
;;; let/cc-form wird nur generiert, wenn es mindestens ein angewandtes
;;; Vorkommen gibt.
;;;
;;; Revision 1.44  1994/02/18  10:47:45  hk
;;; p1-progn: Kein progn-form generieren, wenn nur ein Ausdruck.
;;; p1-tagbody: Kein Abschlie"sendes NIL, wenn davor ein (GO ..) steht.
;;;
;;; Revision 1.43  1994/01/07  13:46:41  hk
;;; Unbenannte Funktionen bekommen nun den Namen LAMBDA statt Gxxx.
;;;
;;; Revision 1.42  1994/01/05  12:31:17  sma
;;; Namensänderung im Laufzeitsystem: _INTERNAL bei rt::CATCH-INTERNAL,
;;; rt::UNWIND-PROTECT-INTERNAL, rt::THROW-INTERNAL und rt::PROGV-INTERNAL
;;; gestrichen.
;;;
;;; Revision 1.41  1993/10/15  10:21:09  hk
;;; p1-progn optimiert
;;;
;;; Revision 1.40  1993/10/06  17:33:23  hk
;;; In p1-labels/flet :par-spec früher vorläufig setzen, damit eine
;;; Überprüfung auch bei wechselseitigen Aufrufen erfogen kann.
;;;
;;; Revision 1.39  1993/09/14  13:36:15  ft
;;; rt::type-error in rt::the-type-error umbenannt, da der Name in CLtL2
;;; schon vergeben ist.
;;;
;;; Revision 1.38  1993/09/13  15:10:58  ft
;;; Der von P1-THE erzeugte Code ruft jetzt im Fehlerfall die
;;; Laufzeitunktion TYPE-ERROR auf.
;;;
;;; Revision 1.37  1993/09/12  16:42:19  kl
;;; Fehler in p1-setq behoben. Null-form -> empty-list und anderes.
;;;
;;; Revision 1.36  1993/09/10  14:44:51  hk
;;; (setq) -> null-form, (setq x v) -> setq-form, (setq x v y w) ->
;;; progn-form
;;;
;;; Revision 1.35  1993/09/10  13:20:17  hk
;;; p1-progn generiert keine progn-forms mit leerer form-list mehr.
;;;
;;; Revision 1.34  1993/07/23  05:27:48  ft
;;; p1-multiple-value-prog1 mit nur einer form als Rumpf expandiert jetzt
;;; zu dieser form.
;;;
;;; Revision 1.33  1993/07/02  11:42:57  ft
;;; Und noch ein Tippfehler...
;;;
;;; Revision 1.32  1993/07/02  11:31:14  ft
;;; Anpassung an die geaenderte Definition von p1-named-lambda.
;;;
;;; Revision 1.31  1993/07/02  11:26:04  hk
;;; Auch (setf f) in Makroexpansionsfunktionen im Wirts-Lispsystem
;;; ausfuehren
;;;
;;; Revision 1.30  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.29  1993/05/10  11:57:00  hk
;;; Fehlerhaftes when durch if ersetzt.
;;;
;;; Revision 1.28  1993/04/16  08:18:32  hk
;;; Aufrufe von Lisp Funktionen in Makroexpansionsfunktionen werden
;;; immer durch Lisp-Funktionen dargestellt, sonst Warning.
;;;
;;; Revision 1.27  1993/04/08  08:13:12  hk
;;; Vorkommen von Lisp Systemfunktionen in Makroexpansionsfunktionen
;;; werden gesondert behandelt.
;;;
;;; Revision 1.26  1993/04/03  10:03:17  hk
;;; p1-function an Compiler-Macros angepasst.
;;;
;;; Revision 1.25  1993/02/16  16:40:29  hk
;;; Revision Keyword eingefuegt, Symbole des zu uebersetzenden Programms
;;; durch clicc-lisp:: gekennzeichnet.
;;;
;;; Revision 1.24  1993/01/22  15:07:05  ft
;;; Aenderungen fuer die Verarbeitung von erweiterten Funktionsnamen.
;;;
;;; Revision 1.23  1993/01/08  17:11:20  kl
;;; (the <Typ> <Ausdruck>) erzeugt nun einen entsprechenden Typtest
;;; um <Ausdruck>.
;;;
;;; Revision 1.22  1992/12/03  16:59:25  ft
;;; Fehler in p1-function beseitigt.
;;;
;;; Revision 1.21  1992/12/03  16:39:13  ft
;;; Erweiterung von p1-function um die Bahandlung gen. Funktionen.
;;;
;;; Revision 1.20  1992/11/26  12:22:29  hk
;;; ?write von dynamischen Variablen wird erhoeht, wenn diese beschrieben oder
;;; gebunden werden, damit bestimmt werden kann, ob spaeter ein illegales
;;; defconstant erfolgt.
;;;
;;; Revision 1.19  1992/10/09  13:39:48  hk
;;; Fehler in p1-let/let* behoben.
;;;
;;; Revision 1.18  1992/10/08  16:56:00  hk
;;; Prueft auf Mehrfachdefinition in p1-labels/flet.
;;;
;;; Revision 1.17  1992/10/02  09:36:33  kl
;;; In p1-let/let* werden jetzt auch globale special-Variablen gesondert
;;; behandelt.
;;;
;;; Revision 1.16  1992/09/08  15:24:38  kl
;;; In p1-let/let* werden Deklarationen jetzt anders behandelt.
;;;
;;; Revision 1.15  1992/08/07  09:59:55  hk
;;; Keine Closures von lokalen Makros, Definition von p1-macrolet nach p1macro.
;;;
;;; Revision 1.14  1992/08/06  13:04:45  hk
;;; clicc-error fuer nicht implementierte Konstrukte.
;;;
;;; Revision 1.13  1992/08/05  13:21:43  hk
;;; Syntaktische Aenderungen.
;;;
;;; Revision 1.12  1992/07/29  16:16:59  hk
;;; Vereinfachung in p1-multiple-value-call.
;;;
;;; Revision 1.11  1992/07/29  12:08:50  hk
;;; Kleine Fehler.
;;;
;;; Revision 1.10  1992/07/23  10:06:28  hk
;;; :SYSTEM-MACRO --> :SYS-MACRO.
;;;
;;; Revision 1.9  1992/07/06  09:05:58  hk
;;; Schreibfehler.
;;;
;;; Revision 1.8  1992/07/06  08:47:23  hk
;;; Schreibfehler.
;;;
;;; Revision 1.7  1992/06/05  14:49:41  hk
;;; progv-internal etc. mit rt:: versehen.
;;;
;;; Revision 1.6  1992/06/05  13:51:06  hk
;;; Fehler in p1-multiple-value-call.
;;;
;;; Revision 1.5  1992/06/05  13:08:14  hk
;;; Slot fuer den Rumpf einer Labels-form heisst :body und nicht :form.
;;;
;;; Revision 1.4  1992/06/04  11:57:53  hk
;;; p1-let/let* korrigiert (mapcan -> mapcar).
;;;
;;; Revision 1.3  1992/06/04  10:19:01  hk
;;; Fuer Form von setq hatte ich den Aufruf von p1-form vergessen.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;; Revision 1.0  1991/05/11  16:54:56  ob
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(defun parse-quote (object_rest)
  (when (or (atom object_rest)
            (rest object_rest))
    (clicc-error NO_MATCH_SF "(OBJECT)" "QUOTE"))
  (first object_rest))

;;------------------------------------------------------------------------------
;; quote object 
;;------------------------------------------------------------------------------
(defun p1-quote (object_rest)
  (p1-constant (parse-quote object_rest)))

;;------------------------------------------------------------------------------
;; function fn
;;------------------------------------------------------------------------------
(defun p1-function (name_rest)
  
  (when (or (atom name_rest)
            (rest name_rest))
    (clicc-error NO_MATCH_SF "(FUN)" "FUNCTION"))
  
  (let ((name (first name_rest)))
    (cond
      ((or (symbolp name) (and (consp name) (eq (car name) 'L::SETF)))
       (let* ((operator-def (if (consp name)
                                (get-setf-fun-def name)
                                (get-operator-def name)))
              cm-operator)

         (when (eq :COMPILER-MACRO (car operator-def))
           (setq cm-operator (cdr operator-def))
           (setq operator-def (cdr cm-operator)))

         (cond
           ((eq :LOCAL-FUN (car operator-def))
            (cdr operator-def))
           ((and *compiler-eval*
                 (eq name
                     (find-symbol (symbol-name (if (consp name)
                                                   (second name)
                                                   name))
                                  *lisp-package*))
                 (or (zw-symbol-fun name)
                     (progn
                       (clicc-warning
                        "Function ~s may not be used in macro function" name)
                       nil))))          ; Result: 3. Arg. v. AND 
           (T (case (car operator-def)
                ((:IMPORTED-FUN :FORWARD :GLOBAL-FUN) (cdr operator-def))
                (:GENERIC-FUN (?fun (cdr operator-def)))
                ((nil) (bind-forward-ref-fun name cm-operator))
                (t (op-as-fun-error (car operator-def) name)))))))

      ;; (function (lambda (..) ...))
      ;; -->
      ;; (labels ((<new-name> (..) ...))
      ;;   (function <new-name>))
      ;;--------------------------
      ((and (consp name) (eq (car name) 'L::LAMBDA))
       (p1-form
        (let ((symbol 'L::LAMBDA))
          `(L::LABELS ((,symbol ,@ (cdr name))) (function ,symbol)))))
      (t (clicc-error NO_OPERATOR name)))))

;;------------------------------------------------------------------------------
;; setq {var form}*
;;------------------------------------------------------------------------------
(defun p1-setq (var_form-list)
  (do (var
       form
       (setq-list (empty-queue)))
        
      ((p1-endp var_form-list)
       (progn 
         (setq setq-list (queue2list setq-list))
         (cond
           ((null setq-list) empty-list)
           ((null (cdr setq-list)) (car setq-list))
           ((make-instance 'progn-form :form-list setq-list)))))
    
    (setq var (pop var_form-list))
    (setq form
          (if (atom var_form-list)
            (clicc-error ODD_NARGS "SETQ")
            (pop var_form-list)))
    (add-q (make-instance 'setq-form
                          :location (p1-write-access-of-var var form)
                          :form (p1-form form))
           setq-list)))

;;------------------------------------------------------------------------------
;; Funktion     : P1-WRITE-ACCESS-OF-VAR
;; Beschreibung : Prueft, ob der Variablen ein Wert zugewiesen werden kann.
;; Rueckgabewert: Eine Struktur vom Typ var-ref
;;------------------------------------------------------------------------------
(defun p1-write-access-of-var (var form)
  (cond
    ((not (symbolp var))
     (clicc-error NO_SYMBOL var))
    ((or (p1-defconstant-p var) (keywordp var))
     (clicc-error CONST_VAL var form))
    (t (let ((var (get-var-bind var)))
         (when (dynamic-p var) (incf (?write var)))
         (make-instance 'var-ref :var var)))))

;;------------------------------------------------------------------------------
;; progn {form}*
;;------------------------------------------------------------------------------
(defun p1-progn (forms)
  (cond
    ((p1-endp forms) (p1-form forms))
    ((p1-endp (cdr forms)) (p1-form (car forms)))
    (t (make-instance 'progn-form :form-list (mapcar #'p1-form forms)))))

;;------------------------------------------------------------------------------
;; let ( {var || (var [value])}*) {declaration}* {form}* 
;;------------------------------------------------------------------------------
(defun p1-let (bindings_body)
  (p1-let/let* 'LET bindings_body))

;;------------------------------------------------------------------------------
;; let* ( {var || (var value)}*) {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-let* (bindings_body)
  (p1-let/let* 'LET* bindings_body))

;;------------------------------------------------------------------------------
(defun p1-let/let* (special-form-name bindings_body)
  (when (atom bindings_body)
    (clicc-error NO_MATCH_SF "(BINDINGS &REST BODY)" special-form-name))
  
  ;; Aufsplitten des Rumpfes in Deklarationen und Ausdruecke
  ;;--------------------------------------------------------
  (multiple-value-bind (decl body)
      (p1-get-decl/forms (rest  bindings_body))

    (let ((bindings (first bindings_body))
          (vars   (empty-queue))
          (values (empty-queue))
          (*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*)))

      ;; Analysieren der Bindungen
      ;; -------------------------
      (do (binding var value)
          
          ((p1-endp bindings))
        
        (setq binding (car bindings))
        (cond
          ((atom binding)
           (setq var   binding)
           (setq value nil))
          (t
           (setq var (pop binding))
           (setq value
                 (if (p1-endp binding)
                     nil                ; (var) wird auch akzeptiert
                     (pop binding)))
           (unless (null binding) 
             (clicc-error IFD_BINDING (car bindings)))))
        (add-q var   vars)
        (add-q value values) 
        (pop bindings))

      (setq vars (queue2list vars))
      (setq values (queue2list values))

      ;; Bekanntgeben der Deklarationen VOR dem Analysieren der Bindungsliste
      ;; wenn die Bindungsbereiche von Deklarationen nach [Ste84] zugrunde
      ;; gelegt werden. 
      ;; Dort ist der Gueltigkeitsbereich von special-Deklarationen auf die
      ;; Initialisierungsformen ausgeweitet.
      ;;---------------------------------------------------------------------
      (p1-declare decl)

      (let* ((local-special-vars 
              (p1-get-special-declared-vars (rest decl)))
             (global-special-vars 
              (intersection vars (?special-decls *GLOBAL-ENVIRONMENT*)))
             (special-vars 
              (append local-special-vars global-special-vars)))
        (cond
          ((eq 'LET special-form-name)

           ;; Zuerst alle Werte berechnen und danach die Variablen binden.
           ;;--------------------------------------------------------------
           ;; Die Werte, die an special-Variablen gebunden werden sollen, 
           ;; werden zuerst an temporaere Variablen gebunden. Deren Werte 
           ;; werden nach der Auswertung aller Initialisierungsformen an 
           ;; die special-Variablen gebunden.
           ;;
           ;; Beispiel:
           ;;---------
           ;; (let ((a (f1 b))
           ;;       (b (f2 a))
           ;;       (c (f3 a)))
           ;;   (declare (special a c))
           ;;   (f3 a b)))                    --->
           ;;
           ;; (let ((tmp1 (f1 b))
           ;;       (b    (f2 a))
           ;;       (tmp2 (f3 a))
           ;;       ------------
           ;;       (a    tmp1)           
           ;;       (c    tmp2))
           ;;  ...
           ;; Unterhalb der mit Strichen gekennzeichneten Linie ist die
           ;; Reihenfolge der Bindungen egal, weil keine Seiteneffekte mehr
           ;; auftreten.
           ;;--------------------------------------------------------------
           (let ((tmp-specials (empty-queue))
                 (tmp-vars (empty-queue)))
             (setq values (mapcar #'p1-form values))
             (setq vars
                   (mapcar #'(lambda (param) 
                               (when (member param special-vars)
                                 (add-q param tmp-specials)
                                 (add-q (setq param (gensym)) tmp-vars)) 
                               
                               (p1-bind-param param local-special-vars))
                           vars))

             (when special-vars  
               (setf values      
                     (append values                   
                             (mapcar #'p1-form (queue2list tmp-vars))))
               (setf vars                  
                     (append vars           
                             (mapcar #'(lambda (param)      
                                         (p1-bind-param param 
                                                        local-special-vars))
                                     (queue2list tmp-specials)))))))               

          (t (let ((vars2 vars) (values2 values))

               ;; Werte auswerten in einer Umgebung, in der die links stehenden
               ;; Variablen schon gebunden sind.
               ;;-------------------------------
               (loop
                (unless vars2 (return))
                (setf (car values2) (p1-form (car values2)))
                (setf (car vars2) (p1-bind-param (car vars2) 
                                                 local-special-vars))
                (pop vars2)
                (pop values2))))))

      ;; Bekanntgeben der Deklarationen NACH dem Analysieren der Lambda-Liste,
      ;; wenn die Bindungsbereiche von Deklarationen nach [Ste90] zugrunde
      ;; gelegt werden.
      ;;-------------------
      ;;   (p1-declare decl)
      ;;-------------------
      ;; VORSICHT! Bei den Bindungen in let*-Formen ist unklar, ob der 
      ;; Gueltigkeitsbereich der Deklarationen auch die Initialisierungsformen
      ;; enthalten soll oder nicht. 
      ;; Zur Zeit werden in der let*-Schleife, die special-Variablen gebunden
      ;; und als special deklariert.

      (if vars
          (make-instance 'let*-form
                         :var-list vars :init-list values :body (p1-progn body))
          (p1-progn body)))))


;;------------------------------------------------------------------------------
;; labels ( { (name lambda-list
;;               {declaration || doc-string}* {form}*) }*)
;;        {declaration}* {form}* 
;;------------------------------------------------------------------------------
(defun p1-labels (functions_body)
  (p1-labels/flet 'LABELS functions_body))

;;------------------------------------------------------------------------------
;; flet ( { (name lambda-list
;;               {declaration || doc-string}* {form}*) }*)
;;        {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-flet (functions_body)
   (p1-labels/flet 'FLET functions_body))

;;------------------------------------------------------------------------------
(defun p1-labels/flet (special-form-name functions_body)
  (when (atom functions_body)
    (clicc-error NO_MATCH_SF "(FUNCTIONS &REST BODY)" special-form-name))
  
  (let ((functions (first functions_body))
        (body      (rest  functions_body))
        p1Body
        (local-fun-names (empty-queue))
        (local-fun-list (empty-queue))

        ;; LABELS bzw. FLET fuehren fuer die spezifizierten Funktionen neue,
        ;; lokale Bindungen ein. Deshalb muss hier eine neue,
        ;; lokale Umgebung angelegt werden.
        ;;---------------------------------
        (*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*)))

    ;; ZWS-Strukturen fuer die lokalen Funktionen erzeugen und vorläufig
    ;; :par-spec setzen, damit Aufrufe schon geprüft werden können.
    ;;----------------------------------------------------
    (dolist (name_lambda-expr functions)
      (multiple-value-bind (name lambda-list_body)
          (parse-named-lambda 'LOCAL-FUNCTION name_lambda-expr)
        (multiple-value-bind (lambda-list doc-string decl body)
            (parse-lambda-expr lambda-list_body)
          (declare (ignore doc-string doc-string decl body))
          (add-q name local-fun-names)
          (add-q (make-instance 'local-fun
                                :symbol name
                                :par-spec (ll-par-spec lambda-list))
                 local-fun-list))))
    (setq local-fun-names (queue2list local-fun-names))
    (setq local-fun-list (queue2list local-fun-list))

    ;; Pruefen auf Mehrfachdefinition
    ;;-------------------------------
    (let ((dupl (duplicates-in-list local-fun-names)))
      (when dupl
        (clicc-warning "multiple functions with same name defined in ~a: ~s"
                       special-form-name
                       (if (> (length dupl) 1) dupl (first dupl)))))

    ;; Bei LABELS die lokalen Funktionsdefinitionen bekanntgeben,
    ;; BEVOR die Funktionen analysiert werden.
    ;;----------------------------------------------
    (when (eq special-form-name 'LABELS)
      (mapc #'bind-local-fun local-fun-names local-fun-list))

    ;; Die lokalen Funktionen uebersetzen
    ;;-----------------------------------
    (mapc #'(lambda (fun name_lambda-expr)
              (let ((name (first name_lambda-expr))
                    (lambda-expr (rest  name_lambda-expr)))
                
                (p1-named-lambda fun name (if (consp name)
                                              (second name) name)
                                 lambda-expr)))

          local-fun-list 
          functions)

    ;; Bei FLET die lokalen Funktionsdefinitionen bekanntgeben,
    ;; NACHDEM die Funktionen analysiert wurden.
    ;;------------------------------------------------
    (when (eq special-form-name 'FLET)
      (mapc #'bind-local-fun local-fun-names local-fun-list))

    ;; Den Rumpf von LABELS/FLET uebersetzen
    ;; -------------------------------------
    (multiple-value-bind (decl forms)
        (p1-get-decl/forms body)

      ;; Syntaktische Analyse der Deklarationen
      ;;---------------------------------------
      (p1-check-declare (rest decl))

      ;; Bekanntgeben der Deklarationen
      ;;-------------------------------
      (p1-declare decl)

      (setq p1Body (p1-progn forms)))

    (make-instance 'labels-form :fun-list local-fun-list :body p1Body)))

;;------------------------------------------------------------------------------
;; if test then [else]
;;------------------------------------------------------------------------------
(defun p1-if (test_then_else)
  (let (test then else)
    (tagbody
       (setq test
             (if (atom test_then_else)
               (go no-match)
               (pop test_then_else)))
       (setq then
             (if (atom test_then_else)
               (go no-match)
               (pop test_then_else)))
       (setq else
             (if (p1-endp test_then_else)
               nil
               (pop test_then_else)))
       (when (null test_then_else) (go end))

     NO-MATCH
       (clicc-error NO_MATCH_SF "(TEST THEN &OPTIONAL ELSE)" "IF")
     END)

    (make-instance 'if-form
                   :pred (p1-form test)
                   :then (p1-form then)
                   :else (p1-form else))))

;;------------------------------------------------------------------------------
;; block name {form}*
;;------------------------------------------------------------------------------
(defun p1-block (name_forms)

  (when (atom name_forms) (clicc-error NO_MATCH_SF "(NAME . BODY)" "BLOCK"))
  
  (let ((name  (first name_forms))
        (forms (rest  name_forms)))
    
    (when (not (symbolp name)) (clicc-error NO_LEGAL_BLOCKNAME name))

    (let ((*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*))
          (cont (make-instance 'cont :read 0)))
      
      ;; Block wird durch Let/cc repraesentiert
      ;;---------------------------------------
      (bind-block name cont)
      (let ((body (p1-progn forms)))
        (if (> (?read cont) 0)
            (make-instance 'let/cc-form :cont cont :body body)
            body)))))

;;------------------------------------------------------------------------------
;; return-from name [result]
;;------------------------------------------------------------------------------
(defun p1-return-from (name_result)
  (let (name result)
    (tagbody
       (setq name
             (if (atom name_result)
                 (go no-match)
                 (pop name_result)))
       (setq result
             (if (p1-endp name_result)
                 nil
                 (pop name_result)))
       (when (null name_result) (go end))
     NO-MATCH
       (clicc-error NO_MATCH_SF "(BLOCK-NAME &OPTIONAL VALUE)" "RETURN-FROM")
     END)

    (when (not (symbolp name)) (clicc-error NO_LEGAL_BLOCKNAME name))
    (let ((cont (get-block-bind name)))
      (incf (?read cont))

      ;; Return-From wird durch Aufruf einer Continuation repraesentiert
      ;;----------------------------------------------------------------
      (make-instance 'app
                     :form cont
                     :arg-list (list (p1-form result))))))

;;------------------------------------------------------------------------------
(defmacro tagp (tag/statement)
  `(or (symbolp ,tag/statement) (integerp ,tag/statement)))

;;------------------------------------------------------------------------------
;; tagbody { tag || statement }*
;;------------------------------------------------------------------------------
(defun p1-tagbody (tag/statement-list)
  (let ((*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*))
        (tagbody-form (make-instance 'tagbody-form)))
    
    ;; Saemtliche Marken des TAGBODY-Konstruktes binden.
    ;;--------------------------------------------------
    (let ((tag-list ()))                ; Liste der gelesenenen Tags
      (dolist (tag/statement tag/statement-list)
        (when (tagp tag/statement)

          ;; Ueberpruefen, ob die Sprungmarke mehrfach definiert wird
          ;;---------------------------------------------------------
          (when (member tag/statement tag-list)
            (clicc-error "Tag ~S used more than once in TAGBODY"
                         tag/statement))

          ;; Marke merken zwecks Pruefung auf Doppeldeklaration
          ;;---------------------------------------------------
          (push tag/statement tag-list)

          (bind-tag tag/statement
                    (make-instance 'tagged-form :tagbody tagbody-form)))))


    ;; Zusammenfassen von durch Tags getrennten Forms zu Tagged-Forms
    ;; und Bearbeiten der Forms in einer Umgebung, in der die Zuordnung
    ;; von GO-Forms zu Tags erfolgen kann.
    ;;---------------------------------------------------------------
    (let (tagged-form
          first-form
          (forms (empty-queue))
          (tagged-form-list (empty-queue)))

      (dolist (tag/statement tag/statement-list)
        (cond
          ((tagp tag/statement)
           (cond
             ((null tagged-form)
              (setq first-form (p1-progn (queue2list forms))))
             (t (setf (?form tagged-form) (p1-progn (queue2list forms)))
                (add-q tagged-form tagged-form-list)))
           (setq tagged-form (get-tag-bind tag/statement))
           (setq forms (empty-queue)))
        
          (t (add-q tag/statement forms))))

      ;; Resultat von Tagbody ist nil, ist nur notwendig, wenn am Ende kein
      ;; GO steht
      ;;-----------------------------
      (unless (let ((last-form (last-q forms)))
                (and (consp last-form) (eq 'L:GO (car last-form))))
        (add-q nil forms))
      
      (cond
        ((null tagged-form)

         ;; keine Tags, dann progn generieren
         ;;----------------------------------
         (p1-progn (queue2list forms)))
           
        (t (setf (?form tagged-form) (p1-progn (queue2list forms)))
           (add-q tagged-form tagged-form-list)
           (setf (?first-form tagbody-form) first-form)
           (setf (?tagged-form-list tagbody-form)
                 (queue2list tagged-form-list))
           tagbody-form)))))

;;------------------------------------------------------------------------------
;; go tag
;;------------------------------------------------------------------------------
(defun p1-go (tag_rest)
  (when (or (atom tag_rest) (rest tag_rest))
    (clicc-error NO_MATCH_SF "(TAG)" "GO"))
  
  (let ((tag (first tag_rest)))
    (when (not (tagp tag)) (clicc-error "~S is no valid tag"))

    (get-tag-bind tag)))

;;------------------------------------------------------------------------------
;; multiple-value-call function {form}*
;;------------------------------------------------------------------------------
(defun p1-multiple-value-call (fun_forms)
  (when (atom fun_forms)
    (clicc-error NO_MATCH_SF "(FUNCTION &REST FORMS)" "MULTIPLE-VALUE-CALL"))

  (let ((fun (car fun_forms))
        (forms (cdr fun_forms)))
    (cond
      ((null forms) (p1-form `(L::FUNCALL ,fun)))
    
      ((atom forms)
       (clicc-error NO_MATCH_SF "(FUNCTION &REST FORMS)" "MULTIPLE-VALUE-CALL"))
    
      ((and (= (length forms) 1)
            (consp fun) (eq 'L::FUNCTION (car fun))
            (consp (cdr fun))
            (consp (cadr fun))
            (eq 'L::LAMBDA (caadr fun)))
       (p1-form `((MV-LAMBDA . ,(cdadr fun)) ,(first forms))))
    
      (t (labels ((gen-nested-mv-lambdas (forms rest-vars)
                    (if (p1-endp forms)
                        `(L::APPLY
                          ,fun
                          ,(if (= (length rest-vars) 1)
                               (first rest-vars)
                               `(L::append ,@rest-vars)))
                        (let ((rest-var (gensym)))
                          `((MV-LAMBDA (L::&REST ,rest-var)
                             ,(gen-nested-mv-lambdas
                               (cdr forms)
                               (append rest-vars (list rest-var))))
                            ,(first forms))))))
           (p1-form (gen-nested-mv-lambdas forms ())))))))

;;------------------------------------------------------------------------------
;; multiple-value-prog1 form {form}*
;;------------------------------------------------------------------------------
(defun p1-multiple-value-prog1 (form1_body)
  (when (atom form1_body)
    (clicc-error NO_MATCH_SF
                 "(FIRST-FORM &REST OTHER-FORMS)" "MULTIPLE-VALUE-PROG1"))
  
  (let ((form1 (first form1_body))
        (body (rest  form1_body))
        (rest-var (gensym)))
    (if (null body)
        (p1-form form1)
        (p1-form
         `((MV-LAMBDA (L::&rest ,rest-var)
            ,@body (L::APPLY #'L::VALUES ,rest-var))
           ,form1)))))

;;------------------------------------------------------------------------------
;; catch tag {form}*
;; 
;; wird durch eine Systemfunktion realisiert, welche als Argument
;; das Tag und den Rumpf in Form einer Closure erhaelt.
;;------------------------------------------------------------------------------
(defun p1-catch (tag_forms)
  (when (atom tag_forms)
    (clicc-error NO_MATCH_SF "(TAG &REST FORMS)" "CATCH"))
  
  (p1-form `(rt::CATCH ,(first tag_forms)
             #'(L::LAMBDA () ,@(rest  tag_forms)))))

;;------------------------------------------------------------------------------
;; unwind-protect protected-form {cleanup-form}*
;; 
;; Wird durch eine Systemfunktion realisiert, welche als
;; Argumente die Protected Form und die Clean-up Formen als Closure erhaelt.
;;------------------------------------------------------------------------------
(defun p1-unwind-protect (protected-form_cleanup-forms)
  (when (atom protected-form_cleanup-forms)
    (clicc-error NO_MATCH_SF
                 "(PROTECTED-FORM &REST CLEANUP-FORMS)" "UNWIND-PROTECT"))
  
  (p1-form `(rt::UNWIND-PROTECT
             #'(L::LAMBDA () ,(first protected-form_cleanup-forms))
             #'(L::LAMBDA () ,@(rest  protected-form_cleanup-forms)))))


;;------------------------------------------------------------------------------
;; throw tag result
;;
;; Wird durch den Aufruf der Systemfunktion 'rt::throw' realisiert, die
;; mittels mv-lambda aufgerufen wird.
;;------------------------------------------------------------------------------
(defun p1-throw (tag_result)
  (let (tag result)
    (tagbody
       (setq tag
             (if (atom tag_result)
               (go no-match)
               (pop tag_result)))
       (setq result
             (if (atom tag_result)
               (go no-match)
               (pop tag_result)))
       (when (null tag_result) (go end))
     NO-MATCH
       (clicc-error NO_MATCH_SF "(TAG RESULT)" "THROW")
     END)

    (let ((tag-var (gensym))
          (rest-var (gensym)))
      (p1-form `(L::LET ((,tag-var ,tag))
                 ((MV-LAMBDA (&rest ,rest-var)
                   (L::APPLY #'rt::THROW ,tag-var ,rest-var))
                  ,result))))))

;;------------------------------------------------------------------------------
;; compiler-let ({ var || (var value) }*) {form}* 
;;------------------------------------------------------------------------------
(defun p1-compiler-let (vars_forms)
  (declare (ignore vars_forms))
  (clicc-error NOT_IMPLEMENTED 'COMPILER-LET)
  nil)

;;------------------------------------------------------------------------------
;; progv symbols values {form}*
;;------------------------------------------------------------------------------
(defun p1-progv (symbols_values_forms)
  (let (symbols values forms)
    (tagbody
       (setq symbols
             (if (atom symbols_values_forms)
               (go no-match)
               (pop symbols_values_forms)))
       (setq values
             (if (atom symbols_values_forms)
               (go no-match)
               (pop symbols_values_forms)))
       (setq forms
             (if (not (listp symbols_values_forms))
               (go no-match)                      
               symbols_values_forms))
       (go end)
     NO-MATCH
       (clicc-error NO_MATCH_SF "(SYMBOLS VALS &REST FORMS)" "PROGV")
     END)

    (p1-form `(rt::PROGV ,symbols ,values #'(L::LAMBDA () ,@forms)))))

;;------------------------------------------------------------------------------
(defun parse-the (value-type_form)
  (let (value-type form)
    (tagbody
       (setq value-type
             (if (atom value-type_form)
               (go no-match)
               (pop value-type_form)))
       (setq form
             (if (atom value-type_form)
               (go no-match)
               (pop value-type_form)))
       (when (null value-type_form) (go end))
     NO-MATCH
       (clicc-error NO_MATCH_SF "(VALUE-TYPE FORM)" "THE")
     END)
    (values value-type form))) 

;;------------------------------------------------------------------------------
;; the value-type form
;;------------------------------------------------------------------------------
(defun p1-the (value-type_form)
  (multiple-value-bind (value-type form)
      (parse-the value-type_form)
    (p1-form `(L::LET ((result ,form))
               (L::IF
                (L::TYPEP result (L::QUOTE ,value-type))
                result
                (rt::the-type-error result (L::QUOTE ,value-type))))))) 

;;------------------------------------------------------------------------------
;; eval-when ({situation}*) {form}* 
;;------------------------------------------------------------------------------
(defun p1-eval-when (situations_forms)
  (declare (ignore situations_forms))
  (clicc-error NOT_IMPLEMENTED "EVAL-WHEN")
  nil)

;;------------------------------------------------------------------------------
(provide "p1spform")
