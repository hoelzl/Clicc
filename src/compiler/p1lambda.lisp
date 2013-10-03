;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Lambda-Expressions
;;;
;;; $Revision: 1.33 $
;;; $Log: p1lambda.lisp,v $
;;; Revision 1.33  1994/02/01  11:28:32  hk
;;; In p1-named-lambda wird *current-fun* an die gerade generierte fun
;;; gebunden.
;;;
;;; Revision 1.32  1993/10/07  08:04:37  ft
;;; ll-par-spec hinzugefuegt und Fehler darin behoben.
;;;
;;; Revision 1.31  1993/08/19  16:47:38  hk
;;; Verwendung von save-lex-var-name.
;;;
;;; Revision 1.30  1993/07/14  15:00:31  hk
;;; parse-lambda-list in p1-lambda-list integriert. Dadurch wird
;;; verhindert, dass in Instanzen vom Typ params zunaechst Symbole
;;; statt Instanzen vom Typ var eingetragen werden.
;;;
;;; Revision 1.29  1993/07/02  11:30:17  ft
;;; Der Name des impliziten Blocks um den Rumpf einer Funktion
;;; muss p1-named-lambda jetzt als Parameter uebergeben werden.
;;;
;;; Revision 1.28  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.27  1993/02/16  16:56:22  hk
;;; Revision Keyword eingefuegt, Symbole des zu uebersetzenden Programms
;;; durch clicc-lisp:: gekennzeichnet.
;;;
;;; Revision 1.26  1993/01/29  06:58:40  ft
;;; consp statt listp zur Pruefung auf erweiterte Funktionsnamen benutzt.
;;;
;;; Revision 1.25  1993/01/22  15:39:46  ft
;;; Fehler in p1-named-lambda behoben.
;;;
;;; Revision 1.24  1993/01/22  15:00:14  ft
;;; Aenderungen fuer die Verarbeitung von erweiterten Funktionsnamen.
;;;
;;; Revision 1.23  1993/01/19  11:12:48  hk
;;; In parse-lambda-list Fehler behoben.
;;;
;;; Revision 1.22  1993/01/18  10:31:41  hk
;;; Fehler in parse-lambda-list korrigiert (null -> empty-queue-p),
;;; Aufrufe von p1-lambda-list-error in p1-bind-param in clicc-error umbenannt.
;;;
;;; Revision 1.21  1993/01/08  16:18:23  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.20  1993/01/08  15:50:16  hk
;;; clicc-error -> clcerror
;;;
;;; Revision 1.19  1992/12/01  11:48:50  hk
;;; Bindungsfehler von dem Argument von mv-lambda behoben.
;;;
;;; Revision 1.18  1992/11/26  12:22:29  hk
;;; ?write von dynamischen Variablen wird erhoeht, wenn diese beschrieben oder
;;; gebunden werden, damit bestimmt werden kann, ob spaeter ein illegales
;;; defconstant erfolgt.
;;;
;;; Revision 1.17  1992/11/02  10:35:57  hk
;;; optional-init-default gestrichen, durch nil ersetzt.
;;;
;;; Revision 1.16  1992/10/13  16:21:51  hk
;;; Kommentar von parse-named-lambda korrigiert.
;;;
;;; Revision 1.15  1992/09/25  07:45:29  kl
;;; Schreibfehler korrigiert.
;;;
;;; Revision 1.14  1992/09/09  12:59:51  kl
;;; In p1-bind-param ein push durch pushnew ersetzt.
;;;
;;; Revision 1.13  1992/08/15  13:29:00  kl
;;; parse-named-lambda vereinfacht.
;;;
;;; Revision 1.12  1992/08/06  17:23:42  kl
;;; Fehler behoben: In p1-bind-param Vorkommen von param durch symbol ersetzt.
;;;
;;; Revision 1.11  1992/08/06  15:55:35  kl
;;; Fehler in p1-bind-param behoben. Zwei Zeilen geloescht.
;;;
;;; Revision 1.10  1992/08/06  13:09:58  hk
;;; Schreibfehler, Code umgestellt.
;;;
;;; Revision 1.9  1992/08/06  11:58:41  hk
;;; Aux-variablen muessen durch let* ausgedrueckt werden und nicht durch let.
;;;
;;; Revision 1.8  1992/08/06  11:39:42  hk
;;; Aux-Variablen duerfen nicht in den impliziten Block von Funktionen
;;; verschoben werden.
;;;
;;; Revision 1.7  1992/07/30  10:30:22  hk
;;; .
;;;
;;; Revision 1.6  1992/07/29  17:19:41  hk
;;; Aufgeraeumt.
;;;
;;; Revision 1.5  1992/07/22  17:40:23  hk
;;; Zugriffsfkt. auf global-env geaendert.
;;;
;;; Revision 1.4  1992/06/05  13:06:09  hk
;;; Rest Param. in parse-lambda-list werden zunaechst in einer queue gehalten.
;;;
;;; Revision 1.3  1992/06/04  12:56:04  hk
;;; Opt- Rest- und Key-Variablen werden auf nil gesetzt, falls nicht angegeben.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Ein Wert, der mittels read nicht generiert werden kann.
;;------------------------------------------------------------------------------
(defvar *NOT-READABLE* (gensym))

(defvar *LAMBDA-LIST*  ())              ; Aktuelle Lambda-Liste fuer Fehlermeld.

;;------------------------------------------------------------------------------
;; Syntax eines LAMBDA-Ausdruckes:
;;
;; (LAMBDA ( { var }*
;;           [ &optional { var || (var [initform [svar]]) }* ]
;;           [ &rest var ]
;;           [ &key { var || ( { var || (keyword var) } [initform [svar]]) }* ]
;;           [ &aux { var || (var [initform]) }* ])
;;    [[ { declaration }* || doc-string ]]
;;    { form }*)
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Prueft ob das Argument ein Lambda-Listen-Keyword ist.
;;------------------------------------------------------------------------------
(defun lambda-list-keyword-p (param)
  (and (symbolp param) (eql #\& (char (symbol-name param) 0))))

;;------------------------------------------------------------------------------
;; Ueberprueft einen Lambda-Ausdruck auf syntaktische Korrektheit
;; bzgl. der Komponenten:   name lambda-list body
;; Resultate : (MV) <name> <lambda-list_body>
;;------------------------------------------------------------------------------
(defun parse-named-lambda (caller name_lambda-expr)
  (when (atom name_lambda-expr)
    (clicc-error NO_MATCH_SF "(NAME &REST LAMBDA-LIST_BODY)" caller))

  (let ((name        (first name_lambda-expr))
        (lambda-expr (rest  name_lambda-expr)))

    (unless (or (symbolp name)
                (and (consp name) (eql (car name) 'clicc-lisp::SETF)))
      (clicc-error 
       "The name ~S in ~S must be a symbol or a list whose car is setf." 
       name caller))

    (values name lambda-expr)))

;;------------------------------------------------------------------------------
(defun parse-lambda-body (lambda-list_body)
  (when (atom lambda-list_body)
    (clicc-error
     "The argument list of a LAMBDA-EXPRESSION does not match ~
              the lambda list ((&REST ARGLIST) &BODY BODY)"))
  (values (first lambda-list_body) (rest lambda-list_body)))

;;------------------------------------------------------------------------------
;; Ueberprueft einen Lambda-Ausdruck auf syntaktische Korrektheit
;; bzgl. der Komponenten:
;; lambda-list [[ { declaration }* || doc-string ]] { form }*
;; Resultate   : (MV) <lambda-list> <doc-string> <declarations> <forms>
;;------------------------------------------------------------------------------
(defun parse-lambda-expr (lambda-list_body)
  (let (lambda-list body doc-string decl forms)
    (setq lambda-list
          (if (atom lambda-list_body)
            (clicc-error
             "The argument list of a LAMBDA-EXPRESSION does not match ~
              the lambda list ((&REST ARGLIST) &BODY BODY)")
            (pop lambda-list_body)))
    (setq body lambda-list_body)

    (multiple-value-setq (doc-string decl forms)
      (p1-get-doc-string/decl/forms body))
    (values lambda-list doc-string decl forms)))

;;------------------------------------------------------------------------------
;; Ueberprueft eine Lambda-Liste auf syntaktische Korrektheit.
;; Die Parameter werden in eine Struktur vom Typ 'params' einsortiert,
;; die Parameter werden gebunden
;; Resultate : (MV) - Lambda-Liste in der abstrakten Syntax
;;                  - Liste von Aux-Var. + Initforms in konkreter Syntax
;;------------------------------------------------------------------------------
(defun p1-lambda-list (lambda-list special-vars)
  (let ((*LAMBDA-LIST* lambda-list)
        (state   0)                     ; Zustand fuer simple, &opt, ...
        (simple            (empty-queue))
        (optional          (empty-queue))
        (rest              (empty-queue))
        (key               (empty-queue))
        (allow-other-keys nil)
        (aux-vars          (empty-queue))
        (all-params (empty-queue)))     ; Liste aller Parameter der Lambda-Liste

    (labels
        ((next-lambda-list-keyword ()
           (case state
             (1 (when (empty-queue-p optional)
                  (clicc-warning "No optionals specified after &OPTIONAL")))
             (2 (when (empty-queue-p rest)
                  (p1-lambda-list-error "No variable follows &REST keyword."))
                (when (> (length (queue2list rest)) 1)
                  (p1-lambda-list-error "Second variable seen after &REST.")))
             (3 (when (empty-queue-p key)
                  (clicc-warning "No keys specified after &KEY")))
             (5 (when (empty-queue-p aux-vars)
                  (clicc-warning "No aux-parameter specified after &AUX")))
             (t nil)))

         (bind-param (symbol)
           (let ((var (p1-bind-param symbol special-vars)))
             (add-q var all-params)
             var))

         ;; var || (var [initform [svar]])
         ;;-------------------------------
         (parse-optional-param (param-declaration)
           (let ((param-decl param-declaration)
                 var
                 (initform nil)
                 (svar nil))
             (cond
               ((atom param-decl)
                (setq var param-decl)
                (setq initform (p1-form nil)))
               (t
                (setq var (pop param-decl))
                (setq initform (p1-form (if (p1-endp param-decl)
                                            nil
                                            (pop param-decl))))
                (setq svar (if (p1-endp param-decl)
                               nil
                               (bind-param (pop param-decl))))
                (unless (null param-decl)
                  (p1-lambda-list-error IFD_LL_ELEM
                                        param-declaration
                                        "(VAR || (VAR [INITFORM [SVAR]]))"))))
               
             (setq var (bind-param var)) ; erst nach initform binden !

             (make-instance 'opt :var var :init initform :suppl svar)))
           
         ;; var || ( { var || (keyword var) } [initform [svar]])
         ;;-----------------------------------------------------
         (parse-key-param (param-declaration)
           (let ((param-decl param-declaration)
                 keyword var
                 initform
                 svar
                 key-part)
    
             (labels ((make-keyword (sym)
                        (intern (symbol-name sym) "KEYWORD")))
               (cond
                 ((atom param-decl)
                  (setq var param-decl)
                  (setq keyword (make-keyword var))
                  (setq initform (p1-form nil)))
                 (t
                  (tagbody
                     (setq key-part (pop param-decl))
                     (cond
                       ((atom key-part)
                        (setq var key-part)
                        (setq keyword (make-keyword var)))
                       (t (setq keyword (pop key-part))
                          (setq var
                                (if (atom key-part)
                                    (go NO-MATCH)
                                    (pop key-part)))
                          (unless (null key-part) (go NO-MATCH))))
                     (setq initform (p1-form (if (p1-endp param-decl)
                                                 nil
                                                 (pop param-decl))))
                     (setq svar (if (p1-endp param-decl)
                                    nil
                                    (bind-param (pop param-decl))))
                     (unless param-decl (go OK))
           
                   NO-MATCH
           
                     (p1-lambda-list-error
                      IFD_LL_ELEM
                      param-declaration
                      "(VAR || ( { VAR || (KEYWORD VAR) } [INITFORM [SVAR]]))")
                       
                   OK)))

               (setq var (bind-param var)) ; erst nach initform binden !
               (setq keyword (p1-make-symbol keyword))

               (make-instance 'key
                              :var var
                              :init initform
                              :suppl svar
                              :sym keyword))))

    
         ;; var || (var [initform])
         ;; Resultat : Eine Liste der Form (var initform)
         ;;----------------------------------------------
         (parse-aux-param (param-declaration)
           (let ((param-decl param-declaration)
                 var initform)
             (cond
               ((atom param-decl)
                (setq var param-decl)
                (setq initform nil))
               (t (setq var (pop param-decl))
                  (setq initform
                        (if (p1-endp param-decl)
                            nil
                            (pop param-decl)))
                  (unless (null param-decl)
                    (p1-lambda-list-error IFD_LL_ELEM
                                          param-declaration
                                          "(VAR || (VAR [INITFORM]))"))))
             (list var initform))))

      (do (next-param)
          ((p1-endp lambda-list))

        (setq next-param (pop lambda-list))
        (case next-param
          (&OPTIONAL
           (when (> state 0)
             (p1-lambda-list-error
              "&OPTIONAL seen after optionals already processed."))
           (setq state 1))

          (&REST
           (next-lambda-list-keyword)
           (when (> state 1)
             (p1-lambda-list-error
              "&REST seen after rest argument already processed."))
           (setq state 2))

          (&KEY
           (next-lambda-list-keyword)
           (when (> state 2)
             (p1-lambda-list-error
              "&KEY seen after key arguments already processed."))
           (setq state 3))

          (&ALLOW-OTHER-KEYS
           (next-lambda-list-keyword)
           (when (/= state 3)
             (p1-lambda-list-error
              "&ALLOW-OTHER-KEYS has to immediately follow key arguments."))
           (setq state 4)
           (setq allow-other-keys t))

          (&AUX
           (next-lambda-list-keyword)
           (when (> state 4)
             (p1-lambda-list-error
              "&AUX seen after aux arguments already processed."))
           (setq state 5))

          (otherwise
           (case state
             (0                         ; Einfache Parameter
              (add-q (bind-param next-param) simple))
             (1                         ; &OPTIONAL
              (add-q (parse-optional-param next-param) optional))
             (2                         ; &REST
              (add-q (bind-param next-param) rest))
             (3                         ; &KEY
              (add-q (parse-key-param next-param) key))
             (4
              (p1-lambda-list-error
               "Parameter ~s not allowed after &ALLOW-OTHER-KEYS." next-param))
             (5                         ; &AUX
              (add-q (parse-aux-param next-param) aux-vars))))))

      (next-lambda-list-keyword)
      (let ((params (make-instance 'params
                                   :var-list (queue2list simple)
                                   :opt-list (queue2list optional)
                                   :rest (if (empty-queue-p rest)
                                             nil
                                             (first (queue2list rest)))
                                   :key-list (queue2list key)
                                   :allow-other-keys allow-other-keys)))

        ;; Liste aller durch den LAMBDA-Ausdruck gebundenen Variablen eintragen
        ;;---------------------------------------------------------------------
        (setf (?all-vars params) (queue2list all-params))
       
        (values params (queue2list aux-vars))))))

;;------------------------------------------------------------------------------
;; NAMED-LAMBDA name lambda-list {declaration || doc-string}* {form}*
;;------------------------------------------------------------------------------
(defun p1-named-lambda (fun name function-block-name lambda-list_body)
  (let ((*FUN-NAME* name)
        (*CURRENT-FUN* fun)
        (*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*)))

    (multiple-value-bind (params aux-list decl body)
        (p1-unnamed-lambda lambda-list_body)

      (setq body `(clicc-lisp::BLOCK ,function-block-name ,@body))
      (setq body `(clicc-lisp::LET* ,aux-list ,decl ,body))

      (setf (?params fun) params)
      (setf (?par-spec fun) (calc-par-spec params))
      (setf (?body fun) (p1-form body))))
  fun)

;;------------------------------------------------------------------------------
;; Hilfsfunktion fuer p1-named-lambda und p1-mv-lambda
;;------------------------------------------------------------------------------
(defun p1-unnamed-lambda (lambda-list_body)
  (multiple-value-bind (lambda-list doc-string decl body)
      (parse-lambda-expr lambda-list_body)
    (declare (ignore doc-string))

    ;; Syntaktische Analyse der Deklarationen
    ;;---------------------------------------
    (p1-check-declare (rest decl))
    
    ;; Bekanntgeben der Deklarationen VOR dem Analysieren der Lambda-Liste,
    ;; wenn die Bindungsbereiche von Deklarationen nach [Ste84] zugrunde
    ;; gelegt werden.
    ;;---------------
    (p1-declare decl)

    ;; Analysieren der Lambda-Liste unter Beachtung der SPECIAL-Deklarationen.
    ;;------------------------------------------------------------------------
    (multiple-value-bind (params aux-list)
        (p1-lambda-list lambda-list
                        (p1-get-special-declared-vars (rest decl)))

      ;; Bekanntgeben der Deklarationen NACH dem Analysieren der Lambda-Liste,
      ;; wenn die Bindungsbereiche von Deklarationen nach [Ste90] zugrunde
      ;; gelegt werden.
      ;;---------------
      ;;   (p1-declare decl)

      (values params aux-list decl body))))

;;------------------------------------------------------------------------------
;; MV-LAMBDA, neu eingefuegtes Konstrukt zur Darstellung von multiple-value-...
;; Syntax: ((MV-LAMBDA <lambda-list> <body>) arg)
;;------------------------------------------------------------------------------
(defun p1-mv-lambda (lambda-list_body args)
  (cond
    ((= (length args) 1)
     (let ((p1-arg (p1-form (first args)))
           (*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*))) 
       (multiple-value-bind (params aux-list decl body)
           (p1-unnamed-lambda lambda-list_body)
         (setq body `(clicc-lisp::LET* ,aux-list ,decl ,@body))
         (make-instance 'mv-lambda
                        :params params
                        :body (p1-form body)
                        :arg p1-arg))))

    (t (clc-error "internal, MV-LAMBDA needs exactly 1 argument")
       empty-list)))

;;------------------------------------------------------------------------------
;; Lambda-Applikationen werden in Let-Ausdruecke umgewandelt. Falls die Lambda-
;; Liste nicht nur required Parameter enthaelt, wird der Rumpf des Let-Ausdrucks
;; zu einem Aufruf einer unbenannten Funktion, die als Parameter die uebrigen
;; Parameter enthaelt.
;; 
;; ((lambda (<req> <rest/opt/key/aux>) <decl> <body>) <req-args> <rest-args>)
;; -->
;; (let (<req+args>) <decl>
;;    (funcall (function (lambda (<rest/opt/key/aux>) <decl> <body>))
;;       <rest-args>))
;;
;; ((lambda (<req>) <decl> <body>) <req-args>)
;; -->
;; (let (<req+args>) <decl> <body>)
;;------------------------------------------------------------------------------
(defun p1-lambda-app (lambda-list_body args)
  (multiple-value-bind (lambda-list doc-string decl body)
      (parse-lambda-expr lambda-list_body)
    (declare (ignore doc-string))
    
    (let ((req-params-args (empty-queue))
          param)
      (loop
       (when (p1-endp lambda-list)
         (return))
       (setq param (car lambda-list))
       (when (lambda-list-keyword-p param)
         (return))
       (add-q (list param
                    (cond
                      ((atom args)
                       (clc-error "Too few arguments in lambda application")
                       empty-list)
                      (t (pop args))))
              req-params-args)
       (pop lambda-list))

      (p1-form
       `(clicc-lisp::LET ,(queue2list req-params-args)
         ,decl
         ,(cond
            (lambda-list
             `(clicc-lisp::FUNCALL
               #'(clicc-lisp::LAMBDA ,lambda-list ,decl ,@body) ,@args))
            (t (when args
                 (clc-error "Too many arguments in lambda application"))
               `(clicc-lisp::PROGN ,@body))))))))

;;------------------------------------------------------------------------------
;; Berechnen der Parameter-Spezifikation
;;------------------------------------------------------------------------------
(defun calc-par-spec (params)
  (let ((nsimple (length (?var-list params))))
    (if (or (?opt-list params)
            (?rest     params)
            (?key-list params))
        (1- (- nsimple))
        nsimple)))

;;------------------------------------------------------------------------------
;; ll-par-spec: Ermittle den par-spec einer lambda-list
;;------------------------------------------------------------------------------
(defun ll-par-spec (lambda-list)
  (let ((numoreq 0))
    (dolist (param lambda-list)
      (if (lambda-list-keyword-p param)
          (if (eq param 'L::&AUX)
              (return-from ll-par-spec numoreq)
              (return-from ll-par-spec (1- (- numoreq))))
          (incf numoreq)))
    numoreq))

;;------------------------------------------------------------------------------
;; Bindet eine lexikalische Variable
;; Resultat: Darstellung der Variablen in der abstrakten Syntax
;;------------------------------------------------------------------------------
(defun p1-bind-lexical-var (symbol)
  (let ((var (make-instance 'local-static :symbol symbol)))
    (bind-var symbol var)
    var))

;;------------------------------------------------------------------------------
;; Bindet eine dynamische Variable
;; Resultat: Darstellung der Variablen in der abstrakten Syntax
;;------------------------------------------------------------------------------
(defun p1-bind-special-var (symbol)
  (let* ((var (get-global-dynamic symbol)))
    (bind-var symbol var)
    (incf (?write var))
    var))

;;------------------------------------------------------------------------------
(defun p1-bind-param (symbol special-vars)
           
  ;; Statisch semantische Analyse des Parameters
  ;;--------------------------------------------
  (cond
    ((not (symbolp symbol)) (clicc-error NO_VAR symbol))
    ((keywordp symbol) (clicc-error IS_KEYWORD symbol))
    ((p1-defconstant-p symbol) (clicc-error DECLARED_CONSTANT symbol))
    
    ;; Wurde das Symbol mit PROCLAIM oder mit
    ;; DECLARE als SPECIAL deklariert ?
    ;;---------------------------------
    ((or (p1-proclaimed-special-p symbol)
         (member symbol special-vars))
                     
     ;; Namen der dynamisch gebundenen Variablen merken,
     ;; um bei globalen Konstanten
     ;; ueberpruefen zu koennen, ob eine lokale Bindung
     ;; gleichen Namens bereits uebersetzt wurde.
     ;;------------------------------------------
     (p1-bind-special-var symbol))

    ;; Sonst wird 'symbol' lexikalisch gebunden.
    ;; Der Name der lexikalisch gebundenen Variablen wird
    ;; abgespeichert, um ueberpruefen zu koennen, ob
    ;; - bei globalen Konstanten eine lokale Bindung und
    ;; - von global deklarierten SPECIAL-Variablen eine
    ;; lexikalische Bindung gleichen Namens existiert.
    ;;------------------------------------------------
    (T (save-lex-var-name symbol)
       (p1-bind-lexical-var symbol))))

;;------------------------------------------------------------------------------
;; Ausgaberoutine fuer Fehler in Lambda-Listen
;;------------------------------------------------------------------------------
(defun p1-lambda-list-error (error-string &rest args)
  (apply #'clicc-error
         (concatenate 'string
                      "~S is an ill-formed lambda-list ---~%           "
                      error-string)
         *LAMBDA-LIST*
         args))

;;------------------------------------------------------------------------------
(provide "p1lambda")
