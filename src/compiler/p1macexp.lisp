;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Makro Expansions Funktionen fuer COMMON-LISP Makros
;;;
;;; $Revision: 1.14 $
;;; $Log: p1macexp.lisp,v $
;;; Revision 1.14  1994/02/16  10:56:54  hk
;;; COND wird geschickter expandiert, so daß aus die abschließende T
;;; Klausel nicht in ein (if T ..) expandiert.
;;;
;;; Revision 1.13  1994/02/09  14:54:31  hk
;;; Lisp Symbole der Quellsprache mit L: versehen.
;;;
;;; Revision 1.12  1993/12/14  12:32:28  hk
;;; 'setf --> 'L::setf
;;;
;;; Revision 1.11  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.10  1993/04/14  12:36:12  hk
;;; runtime:: -> RT::.
;;;
;;; Revision 1.9  1993/03/25  15:34:13  hk
;;; p1-assert eingefuegt.
;;;
;;; Revision 1.8  1993/02/16  16:54:58  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.7  1993/01/08  15:49:28  hk
;;; clicc-error -> clcerror
;;;
;;; Revision 1.6  1992/12/07  13:50:41  hk
;;; p1-dolist: THE und VALUES gestrichen.
;;;
;;; Revision 1.5  1992/12/03  16:33:58  hk
;;; ecase und etypecase neu definiert.
;;;
;;; Revision 1.4  1992/08/05  13:20:04  hk
;;; Syntaktische Aenderungen.
;;;
;;; Revision 1.3  1992/07/10  09:50:59  hk
;;; In WITH-OPEN-STREAM zunaechst CHECK-TYPE auskommentiert.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Fehlermeldungen bei der Makroexpansion
;;------------------------------------------------------------------------------
(defconstant NO_LEGAL_LIST "~S is not a legal list for a ~A form")
(defconstant NOT_A_LIST "~S should be a list.")
(defconstant TOPLEVEL_FORM "~s may only be used as toplevel-form")

;;------------------------------------------------------------------------------
(defun p1-gensym (var)
  (declare (ignore var))
  (gensym))

;;------------------------------------------------------------------------------
;; AND {form}*
;;------------------------------------------------------------------------------
(defun p1-and (forms)
  (typecase forms
    (null 'L:T)                          ; (AND) = T
    (cons
     (if (rest forms)
         `(L:IF ,(first forms) (L:AND ,@(rest forms)))
         (first forms)))                ; (AND object) = object
    (t (clc-error NO_LEGAL_LIST forms "AND")
       'L:T)))

;;------------------------------------------------------------------------------
;; OR {form}*
;;------------------------------------------------------------------------------
(defun p1-or (forms)
  (typecase forms
    (null NIL)                          ; (OR) = NIL
    (cons
     (let ((form1 (first forms))
           (more-forms (rest  forms)))
       (cond
         ((null more-forms)             ; (OR <form>) = <form>
          form1)
         ((atom form1)                  ; (OR <atom> &rest)
          `(L:IF ,form1
            ,form1
            (L:OR ,@more-forms)))
         (t (let ((newsym (gensym)))
              `(L:LET ( (,newsym ,form1))
                (L:IF ,newsym
                      ,newsym
                      (L:OR ,@more-forms))))))))
    (t (clc-error NO_LEGAL_LIST forms "OR")
       nil)))

;;------------------------------------------------------------------------------
;; PSETQ {var form}*
;;------------------------------------------------------------------------------
(defun p1-psetq (var_form-list)
  (if (null var_form-list)
      NIL                               ; (PSETQ) = NIL
      (labels ((p1-psetq-var (var_form-list)
                 (cond
                   ((atom var_form-list)
                    (clc-error NOT_A_LIST var_form-list)
                    nil)
                   (t (let ((var (first var_form-list)))
                        (cond
                          ((not (symbolp var))
                           (clicc-error NOT_A_SYMBOL
                                        "(FIRST VAR_FORM-LIST)" var)
                           nil)
                          (t `(L:SETQ
                               ,var
                               ,@(p1-psetq-form (rest var_form-list)))))))))
               (p1-psetq-form (form-list)
                 (cond
                   ((null form-list) nil) ; (PSETQ A) = (SETQ A)
                   ((atom form-list)
                    (clc-error NO_LEGAL_LIST var_form-list "PSETQ")
                    nil)
                   ((rest form-list)
                    (list `(L:PROG1 ,(first form-list)
                            (L:PSETQ ,@(rest form-list)))))
                   (t (list (first form-list))))))
      
        ;; Body of Labels
        ;;---------------
        `(PROGN ,(p1-psetq-var var_form-list) NIL))))

;;------------------------------------------------------------------------------
;; PROG1 first {form}* 
;;------------------------------------------------------------------------------
(defun p1-prog1 (first_forms)
  (cond
    ((atom first_forms)
     (clc-error ILLEGAL_CALL "PROG1" "(FIRST &REST FORMS)")
     nil)
    (t (let ((newsym (gensym)))
         `(L:LET ( (,newsym ,(first first_forms)))
           ,@(rest first_forms)
           ,newsym)))))

;;------------------------------------------------------------------------------
;; PROG2 first second {form}*
;;------------------------------------------------------------------------------
(defun p1-prog2 (first_second_forms)
  (cond
    ((atom first_second_forms)
     (clc-error ILLEGAL_CALL "PROG2" "(FIRST SECOND &REST FORMS)")
     nil)
    (t `(L:PROGN ,(first first_second_forms)
         (L:PROG1 ,@(rest first_second_forms))))))

;;------------------------------------------------------------------------------
;; WHEN test {form}*
;;------------------------------------------------------------------------------
(defun p1-when (test_forms)
  (when (atom test_forms)
    (clicc-error ILLEGAL_CALL "WHEN" "(PRED &BODY FORMS)"))

  `(L:IF ,(first test_forms)
    (L:PROGN ,@(rest test_forms))))

;;------------------------------------------------------------------------------
;; UNLESS test {form}*
;;------------------------------------------------------------------------------
(defun p1-unless (test_forms)
  (when (atom test_forms)
    (clicc-error ILLEGAL_CALL "UNLESS" "(PRED &BODY FORMS)"))

  `(L:IF (L:NOT ,(first test_forms))
    (L:PROGN ,@(rest test_forms))
    NIL))

;;------------------------------------------------------------------------------
;; COND {(test {form}*)}*
;;------------------------------------------------------------------------------
(defun p1-cond (clauses)
  (cond
    ((null clauses) NIL)                ; (COND) = NIL
    ((atom clauses)
     (clicc-error NO_LEGAL_LIST clauses "COND"))
    (t (let ((first-clause (first clauses))
             (rest-clauses (rest  clauses))
             pred-first-clause
             forms-first-clause)
         (when (atom first-clause)
           (clicc-error NO_LEGAL_LIST first-clause "COND"))
         (setq pred-first-clause  (first first-clause)
               forms-first-clause (rest  first-clause))
         (cond
           ((eq 'L:T pred-first-clause)
            (if forms-first-clause
                `(L:PROGN ,@forms-first-clause)
                'L:T))            
           (forms-first-clause
            `(L:IF ,pred-first-clause
              (L:PROGN ,@forms-first-clause)
              (L:COND ,@rest-clauses)))
           (t `(L:OR ,pred-first-clause
                (L:COND ,@rest-clauses))))))))

;;------------------------------------------------------------------------------
;; CASE keyform {( {({key}*) || key} {form}*) }*
;;------------------------------------------------------------------------------
(defun p1-case (keyform_clauses)
  (when (atom keyform_clauses)
    (clicc-error ILLEGAL_CALL keyform_clauses
                 "(KEYVAL &REST KEY-CONSEQUENT-PAIRS)"))
  (let ((keyform (first keyform_clauses))
        (clauses (rest  keyform_clauses))
        p1Keyform
        (p1Clauses ())
        p1Case)
    (setq p1Keyform
          (if (atom keyform) keyform (gensym)))
    (do (first-clause
         keylist
         test)
        
        ((null clauses)                 ; Alle Klauseln bearbeitet
         (setq p1Case (cons 'L:COND (reverse p1Clauses))))
      
      (setq first-clause
            (if (atom clauses)
                (clicc-error NO_LEGAL_CLAUSE clauses "CASE")
                (pop clauses)))
      (setq keylist
            (if (atom first-clause)
                (clicc-error NO_LEGAL_CLAUSE clauses "CASE")
                (first first-clause)))
      (setq test
            (cond
              ((or (eq keylist 'L:T) (eq keylist 'L:OTHERWISE))
               'L:T)
              (t (when (and (atom keylist) (not (null keylist)))
                   (setq keylist (list keylist)))
                 (do ((test-list ()))
                     ((p1-endp keylist) ; Keylist bearbeitet
                      (cons 'L:OR (reverse test-list)))
                   (push `(L:EQL ,p1Keyform (L:QUOTE ,(pop keylist)))
                         test-list)))))
      (push `(,test (L:PROGN ,@(rest first-clause)))
            p1Clauses ))

    (if (atom keyform)
        p1Case
        `(L:LET ( (,p1Keyform ,keyform))
          ,p1Case))))

;;------------------------------------------------------------------------------
;; ECASE keyform {( {({key}*) || key} {form}*) }*
;;------------------------------------------------------------------------------
(defun p1-ecase (keyform_clauses)
  (when (atom keyform_clauses)
    (clicc-error ILLEGAL_CALL keyform_clauses
                 "(KEYVAL &REST KEY-CONSEQUENT-PAIRS)"))
  (let ((keyform (first keyform_clauses))
        (clauses (rest  keyform_clauses))
         p1Keyform
         (p1Clauses (empty-queue))
         p1Case)
    (setq p1Keyform
          (if (atom keyform) keyform (gensym)))
    (do (first-clause
         keylist
         test)
        
        ((null clauses)                 ; Alle Klauseln bearbeitet
         (add-q `(L:T (L:ERROR
                       "ecase: the value ~a is not a legal value" ,p1Keyform))
                p1Clauses)
         (setq p1Case (cons 'L:COND (queue2list p1Clauses))))
      
      (setq first-clause
            (if (atom clauses)
              (clicc-error NO_LEGAL_CLAUSE clauses "ECASE")
              (pop clauses)))
      (setq keylist
            (if (atom first-clause)
              (clicc-error NO_LEGAL_CLAUSE clauses "ECASE")
              (first first-clause)))
      (setq test
            (cond
              ((or (eq keylist 'L:T) (eq keylist 'L:OTHERWISE))
               (clicc-error NO_LEGAL_CLAUSE first-clause "ECASE"))
              (t (when (and (atom keylist) (not (null keylist)))
                   (setq keylist (list keylist)))
                 (do ((test-list ()))
                     ((p1-endp keylist) ; Keylist bearbeitet
                      (cons 'L:OR (reverse test-list)))
                   (push `(L:EQL ,p1Keyform (L:QUOTE ,(pop keylist)))
                         test-list)))))
      (add-q (cons test (rest first-clause)) p1Clauses))

    (if (atom keyform)
      p1Case
      `(L:LET ( (,p1Keyform ,keyform))
        ,p1Case))))

;;------------------------------------------------------------------------------
;; TYPECASE keyform {(type {form}*)}*
;;------------------------------------------------------------------------------
(defun p1-typecase (keyform_clauses)
  (when (atom keyform_clauses)
    (clicc-error ILLEGAL_CALL
                 keyform_clauses "(KEYFORM &REST KEY-CONSEQUENT-PAIRS)"))
  (let ((keyform (first keyform_clauses))
        (clauses (rest  keyform_clauses))
        p1Keyform
        (p1Clauses ())
        p1Typecase)
    (setq p1Keyform
          (if (atom keyform) keyform (gensym)))
    (do (first-clause type)
        ((null clauses)                 ; Alle Klauseln bearbeitet
         (setq p1Typecase (cons 'L:COND (reverse p1Clauses))))
      (setq first-clause
            (if (atom clauses)
                (clicc-error NO_LEGAL_CLAUSE clauses "TYPECASE")
                (pop clauses)))
      (setq type
            (if (atom first-clause)
                (clicc-error NO_LEGAL_CLAUSE clauses "TYPECASE")
                (first first-clause)))
      (push (list (if (or (eq type 'L:T) (eq type 'L:OTHERWISE))
                      'L:T
                      `(L:TYPEP ,p1Keyform (L:QUOTE ,type)))
                  `(L:PROGN ,@(rest first-clause)))
            p1Clauses))

    (if (atom keyform)
        p1Typecase
        `(L:LET ((,p1Keyform ,keyform))
          ,p1Typecase))))

;;------------------------------------------------------------------------------
;; ETYPECASE keyform {(type {form}*)}*
;;------------------------------------------------------------------------------
(defun p1-etypecase (keyform_clauses)
  (when (atom keyform_clauses)
    (clicc-error ILLEGAL_CALL
                 keyform_clauses "(KEYFORM &REST KEY-CONSEQUENT-PAIRS)"))
  (let ((keyform (first keyform_clauses))
        (clauses (rest  keyform_clauses))
        p1Keyform
        (p1Clauses (empty-queue))
        p1Typecase)
    (setq p1Keyform (if (atom keyform) keyform (gensym)))

    (do (first-clause type)
        ((null clauses)                 ; Alle Klauseln bearbeitet
         (add-q `(L:T (error "etypecase: the value ~a is not a legal value"
                       ,p1Keyform))
                p1Clauses)
         (setq p1Typecase (cons 'L:COND (queue2list p1Clauses))))
      (setq first-clause
            (if (atom clauses)
                (clicc-error NO_LEGAL_CLAUSE clauses "ETYPECASE")
                (pop clauses)))
      (setq type
            (if (atom first-clause)
                (clicc-error NO_LEGAL_CLAUSE clauses "ETYPECASE")
                (first first-clause)))
      (when (or (eq type 'L:T) (eq type 'L:OTHERWISE))
        (clicc-error NO_LEGAL_CLAUSE first-clause "ETYPECASE"))
      (add-q (cons `(L:TYPEP ,p1Keyform (L:QUOTE ,type))
                   (rest first-clause))
             p1Clauses))

    (if (atom keyform)
        p1Typecase
        `(L:LET ((,p1Keyform ,keyform))
          ,p1Typecase))))

;;------------------------------------------------------------------------------
;; RETURN [result]
;;------------------------------------------------------------------------------
(defun p1-return (result_rest)
  (let ((result (if (atom result_rest) NIL (pop result_rest))))
    (when result_rest
      (clicc-error ILLEGAL_CALL "RETURN" "(&OPTIONAL (RESULT NIL))"))

    `(L:RETURN-FROM NIL ,result)))

;;------------------------------------------------------------------------------
;; LOOP {form}*
;;------------------------------------------------------------------------------
(defun p1-loop (forms)
  (let ((newsym (gensym)))
    `(L:BLOCK NIL
      (L:TAGBODY
         ,newsym                        ; Sprungmarke fuer den Beginn
                                        ; des Rumpfes
         (L:PROGN ,@forms)              ; Falls in Forms atomare Ausdruecke
                                        ; vorkommen,
                                        ; wird eine Warnung ausgegeben
         (L:GO ,newsym)))))

;;------------------------------------------------------------------------------
(defun p1-do/do* (macro varlist_endclause_body)
  (let (varlist
        endclause
        decl
        body
        endtest
        endresult
        new-varlist
        (newsym (gensym))
        step-list)
    (tagbody
       (setq varlist
             (if (atom varlist_endclause_body)
                 (go no-match)
                 (pop varlist_endclause_body)))
       (setq endclause
             (if (atom varlist_endclause_body)
                 (go no-match)
                 (pop varlist_endclause_body)))
       (setq endtest
             (if (atom endclause)
                 (go no-match)
                 (pop endclause)))
       (setq endresult endclause)
       (setq body varlist_endclause_body)
       (go end)
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    macro
                    "((&REST VARLIST) (ENDTEST &REST ENDRESULT) &BODY BODY)")
     END)

    ;; Bearbeiten der <varlist>
    ;; (var init) wird in <new-varlist> eingetragen
    ;; var step   wird in <step-list>   eingetragen
    ;;---------------------------------------------
    (do (index-var-spec
         var   
         init
         (ivs-list varlist (cdr ivs-list))) ; ivs = index-variable specifier

        ((p1-endp ivs-list))

      (setq index-var-spec (first ivs-list))
      (when (atom index-var-spec)       ; <index-var-spec> = VAR
        (setq index-var-spec (list index-var-spec)))

      ;; <index-var-spec> = (VAR [INIT [STEP]])
      ;;---------------------------------------
      (setq var (pop index-var-spec))
      (setq init
            (if (atom index-var-spec)
                NIL
                (pop index-var-spec)))
      (push (list var init) new-varlist)
      (when (not (atom index-var-spec))
        (push var                  step-list)
        (push (pop index-var-spec) step-list))
      (when (not (null index-var-spec))
        (clicc-error ILL_FORMED 'index-variable-specifier (first ivs-list)
                     "(VAR [INIT [STEP]])")))
    (multiple-value-setq (decl body)
      (p1-get-decl/forms body))

    ;; >> Makroexpansion
    ;;    --------------
    `(,(if (eq macro 'L:DO) 'L:PROG 'L:PROG*)
      ,(reverse new-varlist)
      ,decl
      ,newsym
      (L:IF ,endtest
            (L:RETURN (L:PROGN ,@endresult)))
      ,@body
      ,@(if step-list
            (list (cons (if (eq macro 'L:DO) 'L:PSETQ 'L:SETQ)
                        (reverse step-list))))
      (L:GO ,newsym))))

;;------------------------------------------------------------------------------
;; DO ({(var [init [step]]) || var}*)
;;    (end-test {result}*)
;;    {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-do (varlist_endclause_body)
  (p1-do/do* 'L:DO varlist_endclause_body))

;;------------------------------------------------------------------------------
;; DO* ({(var [init [step]]) || var}*)
;;     (end-test {result}*)
;;     {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-do* (varlist_endclause_body)
  (p1-do/do* 'L:DO* varlist_endclause_body))

;;------------------------------------------------------------------------------
;; DOLIST (var listform [resultform]) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-dolist (var-list-result_body)
  (let (var-list-result
        var
        listform
        resultform
        decl
        forms
        newsym)
    (tagbody
       (setq var-list-result
             (if (atom var-list-result_body)
                 (go no-match)
                 (car var-list-result_body)))
       (setq var
             (if (atom var-list-result)
                 (go no-match)
                 (pop var-list-result)))
       (setq listform
             (if (atom var-list-result)
                 (go no-match)
                 (pop var-list-result)))
       (setq resultform
             (if (atom var-list-result)
                 NIL
                 (pop var-list-result)))
       (when (null var-list-result) (go end))
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "DOLIST"
                    "((VAR LISTFORM &OPTIONAL (RESULTFORM NIL)) &BODY BODY)")
     END)

    (multiple-value-setq (decl forms)
      (p1-get-decl/forms (rest var-list-result_body)))
    (setq newsym (gensym))
    `(L:DO (,var
            (,newsym ,listform (L:CDR ,newsym)))
      ((L:ATOM ,newsym) (L:SETQ ,var NIL) ,resultform)
      ,decl
      (L:SETQ ,var (L:CAR ,newsym))
      ,@forms)))

;;------------------------------------------------------------------------------
;; DOTIMES (var countform [resultform]) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-dotimes (var-count-result_body)
  (let (var-count-result
        var
        countform
        resultform
        newsym)
    (tagbody
       (setq var-count-result
             (if (atom var-count-result_body)
                 (go no-match)
                 (car var-count-result_body)))
       (setq var
             (if (atom var-count-result)
                 (go no-match)
                 (pop var-count-result)))
       (setq countform
             (if (atom var-count-result)
                 (go no-match)
                 (pop var-count-result)))
       (setq resultform
             (if (atom var-count-result)
                 NIL
                 (pop var-count-result)))
       (when (null var-count-result) (go end))
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "DOTIMES"
                    "((VAR COUNTFORM &OPTIONAL (RESULTFORM NIL)) &BODY BODY)")
     END)

    (setq newsym (gensym))
    `(L:DO ((,newsym ,countform)
            (,var 0 (1+ ,var)))
      ((>= ,var ,newsym) ,resultform)
      (L:DECLARE (L:FIXNUM ,var ,newsym))
      ,@(rest var-count-result_body))))

;;------------------------------------------------------------------------------
(defun p1-prog/prog* (macro bindings_body)
  (when (atom bindings_body)
    (clicc-error ILLEGAL_CALL macro "(BINDINGS &BODY BODY)"))
  (let ((bindings (first bindings_body))
        (body     (rest  bindings_body))
        decl
        forms)
    (multiple-value-setq (decl forms) (p1-get-decl/forms body))

    `(L:BLOCK NIL
      (,(if (eq macro 'L:PROG) 'L:LET 'L:LET*) ,bindings
       ,decl
       (L:TAGBODY ,@forms)))))

;;------------------------------------------------------------------------------
;; PROG ({var || (var [init])}) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------

(defun p1-prog (bindings_body)
  (p1-prog/prog* 'L:PROG bindings_body))

;;------------------------------------------------------------------------------
;; PROG* ({var || (var [init])}*) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-prog* (bindings_body)
  (p1-prog/prog* 'L:PROG* bindings_body))

;;------------------------------------------------------------------------------
;; MULTIPLE-VALUE-LIST form
;;------------------------------------------------------------------------------
(defun p1-multiple-value-list (form_rest)
  (when (or (atom form_rest)
            (rest form_rest))
    (clicc-error ILLEGAL_CALL "MULTIPLE-VALUE-LIST" "(FORM)"))
  `(L:MULTIPLE-VALUE-CALL #'L:LIST ,(first form_rest)))

;;------------------------------------------------------------------------------
;; MULTIPLE-VALUE-BIND ({var}*) values-form {declaration}* {form}* 
;;------------------------------------------------------------------------------
(defun p1-multiple-value-bind (names-form_body)
  (let (names form body newsym)
    (tagbody
       (setq names
             (if (atom names-form_body)
               (go no-match)
               (pop names-form_body)))
       (setq form
             (if (atom names-form_body)
               (go no-match)
               (pop names-form_body)))
       (setq body names-form_body)
       (go end)
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "MULTIPLE-VALUE-BIND"
                    "(NAMES FORM &BODY BODY)")
     END)
    (setq newsym (gensym))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:MULTIPLE-VALUE-CALL
      #'(L:LAMBDA (L:&OPTIONAL ,@names L:&REST ,newsym)
          (L:DECLARE (L:IGNORE ,newsym))
          ,@body)
      ,form)))

;;------------------------------------------------------------------------------
;; MULTIPLE-VALUE-SETQ variables form
;;------------------------------------------------------------------------------
(defun p1-multiple-value-setq (variables_form)
  (let (variables form bindings setq-list)
    (tagbody
       (setq variables
             (if (atom variables_form)
               (go no-match)
               (pop variables_form)))
       (setq form
             (if (atom variables_form)
               (go no-match)
               (pop variables_form)))
       (when (null variables_form) (go end))
     NO-MATCH
       (clicc-error ILLEGAL_CALL "MULTIPLE-VALUE-SETQ" "(VARS FORMS)")
     END)

    (setq bindings
          (mapcar #'p1-gensym variables))
    (setq setq-list
          (mapcar #'(lambda (variable newsym)
                      `(L:SETQ ,variable ,newsym))
                  variables
                  bindings))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:MULTIPLE-VALUE-BIND ,bindings ,form
      ,@setq-list
      ,(first bindings))))

;;------------------------------------------------------------------------------
;; LOCALLY {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-locally (decls-forms)
  `((L:LAMBDA () ,@decls-forms)))

;;------------------------------------------------------------------------------
;; REMF place indicator
;;------------------------------------------------------------------------------
(defun p1-remf (place_indicator_rest)
  (let (place indicator newsym found)
    (tagbody
       (setq place
             (if (atom place_indicator_rest)
                 (go no-match)
                 (pop place_indicator_rest)))
       (setq indicator
             (if (atom place_indicator_rest)
                 (go no-match)
                 (pop place_indicator_rest)))
       (when (null place_indicator_rest) (go end))
     NO-MATCH
       (clicc-error ILLEGAL_CALL "REMF" "(PLACE INDICATOR)")
     END)
    (multiple-value-bind (vars vals store-vars store-form access-form)
        (p1-get-setf-method place)
      (setq newsym (gensym))
      (setq found  (gensym))

      ;; >> Makroexpansion
      ;;    --------------
      `(L:LET* (,@(mapcar #'list vars vals) (,newsym ,indicator))
        (L:MULTIPLE-VALUE-BIND (,(first store-vars) ,found)
            (RT::REMF-INTERNAL ,access-form ,newsym)
          (L:IF ,found
                (L:PROGN ,store-form L:T)
                NIL))))))

;;------------------------------------------------------------------------------
;; DO-SYMBOLS (var [package [result]]) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-do-symbols (var_pkg_res-body)
  (let (var_pkg_res
        var   package   result
        code-body   decl   forms)
    (tagbody
       (when (atom var_pkg_res-body) (go no-match))
       (setq var_pkg_res (first var_pkg_res-body)
             code-body   (rest  var_pkg_res-body))
       (setq var
             (if (atom var_pkg_res)
                 (go no-match)
                 (pop var_pkg_res)))
       (setq package
             (if (atom var_pkg_res)
                 'L:*PACKAGE*
                 (pop var_pkg_res)))
       (setq result
             (if (atom var_pkg_res)
                 NIL
                 (pop var_pkg_res)))
       (when (null var_pkg_res) (go end))

     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "DO-SYMBOLS"
                    "((VAR &OPTIONAL (PACKAGE *PACKAGE*) RESULT-FORM) ~
                           &BODY CODE-BODY)")
     END)
    (multiple-value-setq (decl forms) (p1-get-decl/forms code-body))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:BLOCK NIL
      (RT::DO-SYMBOLS-ITERATOR
          #'(L:LAMBDA (,var) ,decl (L:TAGBODY ,@forms))
        ,package
        )
      (L:LET ((,var NIL)) ,decl ,result))))

;;------------------------------------------------------------------------------
;; DO-EXTERNAL-SYMBOLS (var [package [result]])
;; {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-do-external-symbols (var_pkg_res-body)
  (let (var_pkg_res
        var
        package
        result
        code-body
        decl
        forms)
    (tagbody
       (when (atom var_pkg_res-body) (go no-match))
       (setq var_pkg_res (first var_pkg_res-body)
             code-body   (rest  var_pkg_res-body))
       (setq var
             (if (atom var_pkg_res)
               (go no-match)
               (pop var_pkg_res)))
       (setq package
             (if (atom var_pkg_res)
               'L:*PACKAGE*
               (pop var_pkg_res)))
       (setq result
             (if (atom var_pkg_res)
               NIL
               (pop var_pkg_res)))
       (when (null var_pkg_res) (go end))

     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "DO-EXTERNAL-SYMBOLS"
                    "((VAR &OPTIONAL (PACKAGE *PACKAGE*) RESULT-FORM) ~
                           &BODY CODE-BODY)")
     END)
    (multiple-value-setq (decl forms) (p1-get-decl/forms code-body))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:BLOCK NIL
      (RT::DO-EXTERNAL-SYMBOLS-ITERATOR
          #'(L:LAMBDA (,var) ,decl (L:TAGBODY ,@forms))
        ,package)
      (L:LET ((,var NIL)) ,decl ,result))))

;;------------------------------------------------------------------------------
;; DO-ALL-SYMBOLS (var [result-form]) {declaration}* {tag || statement}*
;;------------------------------------------------------------------------------
(defun p1-do-all-symbols (var_res-body)
  (let (var_res
        var
        result-form
        code-body
        decl
        forms)
    (tagbody
       (when (atom var_res-body) (go no-match))
       (setq var_res   (first var_res-body)
             code-body (rest  var_res-body))
       (setq var
             (if (atom var_res)
               (go no-match)
               (pop var_res)))
       (setq result-form
             (if (atom var_res)
               NIL
               (pop var_res)))
       (when (null var_res) (go end))
     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "DO-ALL-SYMBOLS"
                    "((VAR &OPTIONAL RESULT-FORM) &BODY CODE-BODY)")
     END)
    (multiple-value-setq (decl forms) (p1-get-decl/forms code-body))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:BLOCK NIL
      (RT::DO-ALL-SYMBOLS-ITERATOR
          #'(L:LAMBDA (,var) ,decl (L:TAGBODY ,@forms)))
      (L:LET ((,var NIL)) ,decl ,result-form))))

;;------------------------------------------------------------------------------
;; WITH-OPEN-STREAM (var stream) {declaration}* {form}* 
;;------------------------------------------------------------------------------
(defun p1-with-open-stream (var_stream-body)
  (let (var_stream
        var
        stream
        body
        decl
        forms
        newsym)
    (tagbody
       (when (atom var_stream-body) (go no-match))
       (setq var_stream (first var_stream-body)
             body (rest  var_stream-body))
       (setq var
             (if (atom var_stream)
                 (go no-match)
                 (pop var_stream)))
       (setq stream
             (if (atom var_stream)
                 (go no-match)
                 (pop var_stream)))
       (when (null var_stream) (go end))

     NO-MATCH
       (clicc-error ILLEGAL_CALL
                    "WITH-OPEN-STREAM" "((VAR STREAM) &BODY BODY)")
     END)
    (multiple-value-setq (decl forms) (p1-get-decl/forms body))
    (setq newsym (gensym))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:LET ((,var    ,stream)
             (,newsym L:T))
      ,decl
      (L:UNWIND-PROTECT
           (L:MULTIPLE-VALUE-PROG1
               (L:PROGN ,@forms)
             (L:SETQ ,newsym NIL))
        (L:CLOSE ,var :ABORT ,newsym)))))

;;------------------------------------------------------------------------------
;; WITH-INPUT-FROM-STRING (var string {keyword value}*) {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-with-input-from-string (var_string_keys-body)
  (let (args
        var
        string
        index
        start
        end
        decl
        forms
        expanded-body
        (secret (gensym)))
    (when (atom var_string_keys-body)
      (clicc-error ILLEGAL_CALL
                   "WITH-INPUT-FROM-STRING"
                   "((STREAMVAR STRING &KEY INDEX START END) &BODY BODY)"))

    (setq args (first var_string_keys-body))
    (when (atom args)
      (clicc-error ILLEGAL_CALL
                   "WITH-INPUT-FROM-STRING"
                   "(STREAMVAR STRING &KEY INDEX START END)"))
    (setq var (pop args))
    (when (atom args)
      (clicc-error ILLEGAL_CALL
                   "WITH-INPUT-FROM-STRING"
                   "(STREAMVAR STRING &KEY INDEX START END)"))
    (setq string (pop args))
    (when (oddp (length args))
      (clicc-error "Unpaired item in keyword portion of an argument list"))
    (setq index (getf args :index secret))
    (setq start (getf args :start))
    (setq end   (getf args :end))
    (multiple-value-setq (decl forms)
      (p1-get-decl/forms (rest var_string_keys-body)))
    (setq expanded-body
          (if (not (eq index secret))
              `((L::MULTIPLE-VALUE-PROG1
                    (L::PROGN ,@forms)
                  (L::SETF ,index (L::FILE-POSITION ,var))))
              forms))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:WITH-OPEN-STREAM (,var (L:MAKE-STRING-INPUT-STREAM ,string ,start ,end))
      ,decl
      ,@expanded-body)))

;;------------------------------------------------------------------------------
;; WITH-OUTPUT-TO-STRING (var [string]) {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-with-output-to-string (var_string-body)
  (let (var_string
        var
        string
        (string-p nil)
        body)
    (tagbody
       (when (atom var_string-body) (go no-match))
       (setq var_string (first var_string-body)
             body       (rest  var_string-body))
       (setq var
             (if (atom var_string)
               (go no-match)
               (pop var_string)))
       (setq string
             (if (atom var_string)
               NIL
               (progn
                 (setq string-p t)
                 (pop var_string))))
       (when (null var_string) (go end))

     NO-MATCH
       (clicc-error ILLEGAL_CALL "WITH-OUTPUT-TO-STRING"
                    "((STREAMVAR &OPTIONAL (STRING NIL)) &BODY BODY)")
     END)

    ;; >> Makroexpansion
    ;;    --------------
    `(WITH-OPEN-STREAM
      (,var (MAKE-STRING-OUTPUT-STREAM ,@(if string-p (list string) nil)))
      ,@body
      ,@(if string-p nil `((GET-OUTPUT-STREAM-STRING ,var))))))

;;------------------------------------------------------------------------------
;; WITH-OPEN-FILE (stream filename {options}*) {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-with-open-file (stream_fname_options-body)
  (let (stream_fname_options
        stream
        filename
        options
        body
        decl
        forms
        newsym)
    (tagbody
       (when (atom stream_fname_options-body) (go no-match))
       (setq stream_fname_options (first stream_fname_options-body)
             body                 (rest  stream_fname_options-body))
       (setq stream
             (if (atom stream_fname_options)
                 (go no-match)
                 (pop stream_fname_options)))
       (setq filename
             (if (atom stream_fname_options)
                 (go no-match)
                 (pop stream_fname_options)))
       (setq options stream_fname_options)
       (go end)

     NO-MATCH
       (clicc-error ILLEGAL_CALL "WITH-OPEN-FILE"
                    "((STREAM FILENAME &REST OPEN-OPTIONS) &BODY BODY)")
     END)
    (multiple-value-setq (decl forms) (p1-get-decl/forms body))
    (setq newsym (gensym))

    ;; >> Makroexpansion
    ;;    --------------
    `(L:LET ((,stream (L:OPEN ,filename ,@options))
             (,newsym L:T))
      ,decl
      (L:UNWIND-PROTECT
           (L:MULTIPLE-VALUE-PROG1 (L:PROGN ,@forms) (L:SETQ ,newsym NIL))
        (L:WHEN (L:STREAMP ,stream)
          (L:CLOSE ,stream :ABORT ,newsym))))))

;;------------------------------------------------------------------------------
;; ASSERT test-form [({place}*) [string {arg}*]]
;;------------------------------------------------------------------------------
(defun p1-assert (test_places_string_args)
  (let (test places string args)
    (tagbody
       (when (atom test_places_string_args) (go no-match))
       (setq test (pop test_places_string_args))
       (when (null test_places_string_args) (go END))
       (when (atom test_places_string_args) (go no-match))
       (setq places (pop test_places_string_args))
       (when (null test_places_string_args) (go END))
       (when (atom test_places_string_args) (go no-match))
       (setq string (pop test_places_string_args))
       (when (null test_places_string_args) (go END))
       (when (atom test_places_string_args) (go no-match))
       (setq args test_places_string_args)
       (go END)

     NO-MATCH
       (clicc-error ILLEGAL_CALL "ASSERT"
                    "(test-form &optional places string arguments)")

     END)

    `(L:UNLESS ,test
      ,(if string
           `(L:ERROR ,string ,@args)
           `(L:ERROR "The assertion ~s failed." ',test)))))

;;------------------------------------------------------------------------------
(provide "p1macexp")
