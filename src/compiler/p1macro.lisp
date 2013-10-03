;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Der Makroexpansinsmechanismus
;;;
;;; $Revision: 1.32 $
;;; $Log: p1macro.lisp,v $
;;; Revision 1.32  1994/06/07  15:35:23  hk
;;; In p1-expand-user-macro: case -> ecase
;;;
;;; Revision 1.31  1994/06/03  14:10:27  hk
;;; Bessere Fehlermeldung, falls Makro-Expansion scheitert
;;;
;;; Revision 1.30  1994/03/03  13:49:48  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.29  1993/11/30  08:15:49  hk
;;; Fehlermeldung für undefinierte Funktionen bei Makroexpansion
;;; verschönert
;;;
;;; Revision 1.28  1993/07/02  11:31:59  ft
;;; Anpassung an die geaenderte Definition von p1-named-lambda.
;;;
;;; Revision 1.27  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.26  1993/05/10  11:56:44  hk
;;; Fehlerhaftes when durch if ersetzt.
;;;
;;; Revision 1.25  1993/04/15  09:26:33  hk
;;; init-macro-error-funs benutzt in-compile-time-env.
;;;
;;; Revision 1.24  1993/04/07  11:42:07  hk
;;; Fehlermeldung in p1-expand-user-macro neu formatiert.
;;;
;;; Revision 1.23  1993/02/16  16:54:41  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.22  1993/01/20  10:02:52  ft
;;; In init-macro-error-funs defined-fun durch global-fun ersetzt.
;;;
;;; Revision 1.21  1993/01/19  11:12:04  hk
;;; In p1-gen-macro-function werden keine &aux Variablen sondern let*
;;; generiert.
;;;
;;; Revision 1.20  1993/01/12  15:37:12  ft
;;; Aufruf von redef-op-error in p1-defmacro korrigiert.
;;;
;;; Revision 1.19  1993/01/08  16:23:36  hk
;;; Fehler behoben.
;;;
;;; Revision 1.18  1993/01/08  15:48:45  hk
;;; clicc-error -> clcerror, redef-op-error benutzt.
;;;
;;; Revision 1.17  1993/01/06  13:17:17  hk
;;; init-macro-error-funs von p0init nach hier kopiert.
;;;
;;; Revision 1.16  1993/01/05  17:01:31  hk
;;; In p1-macrolet wurde in-compile-time-env eingefuegt.
;;;
;;; Revision 1.15  1992/11/25  16:05:03  ft
;;; Aenderungen in der Behandlung des Expansionsabbruchs.
;;;
;;; Revision 1.14  1992/11/20  13:51:11  ft
;;; p1-expand-user-macro an die Aenderungen in zw-apply angepasst.
;;;
;;; Revision 1.13  1992/11/19  12:18:27  ft
;;; Entfernen der Fehlerbehandlungsfunktionen (dm-...).
;;;
;;; Revision 1.12  1992/10/08  16:55:44  hk
;;; Prueft auf Mehrfachdefinition in macrolet.
;;;
;;; Revision 1.11  1992/09/04  16:33:20  kl
;;; Supplied-Parameter liefern jetzt einheitlich nil oder T.
;;;
;;; Revision 1.10  1992/08/07  11:30:12  hk
;;; Neu: p1-macrolet.
;;;
;;; Revision 1.9  1992/07/29  16:10:45  hk
;;; Ueberarbeitet.
;;;
;;; Revision 1.8  1992/07/23  10:03:54  hk
;;; :SYSTEM-MACRO --> :SYS-MACRO
;;;
;;; Revision 1.7  1992/07/23  08:55:27  hk
;;; Schreibfehler.
;;;
;;; Revision 1.6  1992/07/23  08:37:06  hk
;;; set-global-operator-def --> set-macro.
;;;
;;; Revision 1.5  1992/07/23  08:34:50  hk
;;; Ignore Decl. eingefuegt.
;;;
;;; Revision 1.4  1992/07/22  19:12:03  hk
;;; Nicht benutze Var. gestrichen.
;;;
;;; Revision 1.3  1992/07/22  17:22:35  hk
;;; Aenderungen an der Uebersetzungszeitumgebung.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "CLICC")
(require "p1macexp")                    ; Makro Expansions Funktionen
(require "p1setf")                      ; Modul zum Expandieren von SETF-Formen

;;------------------------------------------------------------------------------
;; Expandiert einen LISP Ausdruck, falls es sich um einen Makroaufruf handelt.
;; Resultate: 1. der LISP Ausdruck ist ein Makroaufruf:
;;               (MV) der expandierte Ausdruck und T
;;            2. sonst: (MV) der urspruengliche Ausdruck und NIL
;;------------------------------------------------------------------------------
(defun p1-macroexpand (form)
  (let ((*CURRENT-FORM* form))          ; Aktuelle Form fuer Fehlermeldungen

    (multiple-value-bind (expanded-form expanded-p)
        (p1-macroexpand-1 form)
      (if expanded-p
        (values (p1-macroexpand expanded-form) T)
        (values form NIL)))))

;;------------------------------------------------------------------------------
(defmacro p1-expand-system-macro (macro-bind call)
  `(FUNCALL ,macro-bind (REST ,call)))

;;------------------------------------------------------------------------------
;; Expandiert einen LISP Ausdruck einmal, falls es sich um einen
;; Makroaufruf handelt.
;; Resultate: 1. Es handelt sich um einen Makroaufruf:
;;               (MV) der expandierte LISP Ausdruck und T
;;            2. Sonst: (MV) der unveraenderte LISP Ausdruck und NIL
;;------------------------------------------------------------------------------
(defun p1-macroexpand-1 (form)
  (if (or (atom form)
          (not (symbolp (first form))))
    (values form nil)
    (let ((operator-def (get-operator-def (first form))))
      (case (car operator-def)
        ((:USER-MACRO :LOCAL-MACRO)
         (values (p1-expand-user-macro (cdr operator-def) form) T))
        (:SYS-MACRO
         (values (p1-expand-system-macro (cdr operator-def) form) T))
        (otherwise
         (values form nil))))))

;;------------------------------------------------------------------------------
;; Expandiert den Aufruf eines benutzerdefinierten Makros
;; Resultat: der expandierte LISP Ausdruck
;;------------------------------------------------------------------------------
(defun p1-expand-user-macro (macro-fun form)
  (multiple-value-bind (evaluated-form evaluated-p)
      (catch 'cant-eval
        (values
         (zw-apply macro-fun (list form) (zw-empty-env))
         T))
    (cond
      (evaluated-p evaluated-form)
      (t
       (clc-error "It was impossible to expand ~S, because ~a."
                  *CURRENT-FORM*
                  (typecase evaluated-form
                    (sym
                     (format nil
                             "reference to global variable ~s is not allowed"
                             (?symbol evaluated-form)))
                    (defined-named-const
                     (format
                      nil
                      (ecase (?value evaluated-form)
                        (:forward "function ~s is not defined")
                        (:unknown
                         "value of constant ~s is not known at compile time"))
                      (?symbol evaluated-form)))
                    (setq-form
                     (format
                      nil
                      "assignment to global variable ~s is not allowed"
                      (?symbol (?sym (?var (?location evaluated-form))))))
                    (t (format nil "~%~s~%can not be evaluated at compile-time"
                               evaluated-form))))
       nil))))
    
;;------------------------------------------------------------------------------
;; DEFMACRO name lambda-list { declaration || doc-string }* { form }*
;;------------------------------------------------------------------------------
(defun p1-defmacro (name_lambda-list_body)
  (multiple-value-bind (name lambda-list_body)
      (parse-named-lambda 'DEFMACRO name_lambda-list_body)

    (clicc-message "Analyse DEFMACRO     ~A" name)

    ;; Ueberpruefen, ob eine Doppeldefinition vorliegt.
    ;;------------------------------------------------
    (let ((operator-key (car (get-operator-def name))))
      (case operator-key
        ((nil))
        (:FORWARD (clicc-error "Defining macro ~A after first use" name))
        (t (redef-op-error operator-key name))))

    (multiple-value-bind (lambda-list body)
        (parse-lambda-body lambda-list_body)
      (set-macro name (p1-gen-macro-fun name lambda-list body)))))

;;------------------------------------------------------------------------------
;; MACROLET ( { (name lambda-list {declaration || doc-string}* {form}*) }* )
;;        {declaration}* {form}*
;;------------------------------------------------------------------------------
(defun p1-macrolet (macros_body)
 (when (atom macros_body)
    (clicc-error NO_MATCH_SF "(MACROS &REST BODY)" "MACROLET"))
 (let ((macros (first macros_body))
        (body  (rest  macros_body))
        (local-fun-names (empty-queue))
        (local-fun-list (empty-queue))

        ;; MACROLET fuehrt neue, lokale Bindungen ein. Deshalb muss hier eine
        ;; neue, lokale Umgebung angelegt werden.
        ;;---------------------------------------
        (*LOCAL-ENVIRONMENT* (copy-env *LOCAL-ENVIRONMENT*)))

    ;; ZWS-Strukturen fuer die lokalen Expansions-Funktionen erzeugen
    ;;---------------------------------------------------------------
    (dolist (name_lambda-expr macros)
      (multiple-value-bind (name lambda-list_body)
          (parse-named-lambda 'LOCAL-MACRO name_lambda-expr)
        (declare (ignore lambda-list_body))
        (unless (symbolp name)
          (clicc-error "The name ~S of a local macro must be a symbol." name))
        (add-q name local-fun-names)
        (add-q (make-instance 'local-fun :symbol name) local-fun-list)))
    (setq local-fun-names (queue2list local-fun-names))
    (setq local-fun-list (queue2list local-fun-list))

    ;; Pruefen auf Mehrfachdefinition
    ;;-------------------------------
    (let ((dupl (duplicates-in-list local-fun-names)))
      (when dupl
        (clicc-warning "multiple macros with same name defined in macrolet: ~s"
                       (if (> (length dupl) 1) dupl (first dupl)))))

    ;; Die lokalen Makros in Expansions-Funktionen in
    ;; Zwischensprachdarstellung uebersetzen.
    ;;----------------------------------------------
    (mapc #'(lambda (fun name_lambda-expr)
              (let ((name (first name_lambda-expr))
                    (lambda-expr (rest  name_lambda-expr)))
                (multiple-value-bind (lambda-list body)
                    (parse-lambda-body lambda-expr)
                  (in-compile-time-env
                   (p1-named-lambda fun
                                    name
                                    (if (consp name)
                                        (second name) name)
                                    (p1-gen-macro-function lambda-list
                                                           body))))))

          local-fun-list 
          macros)

    ;; Die lokalen Makrodefinitionen bekanntgeben, NACHDEM sie analysiert
    ;; wurden.
    ;;-------------------------------------------------------------------
    (mapc #'bind-local-macro local-fun-names local-fun-list)

    (multiple-value-bind (decl forms)
        (p1-get-decl/forms body)
      (p1-check-declare (rest decl))
      (p1-declare decl)
      (p1-progn forms))))

;;------------------------------------------------------------------------------
(defun p1-gen-macro-fun (name lambda-list body)
  (in-compile-time-env
   (p1-named-lambda (make-instance 'global-fun
                                   :symbol name)
                    name
                    (if (consp name) (second name) name)
                    (p1-gen-macro-function lambda-list body))))

;;------------------------------------------------------------------------------
;; gueltige lambda-listen fuer DEFMACRO haben die Gestalt:
;;
;;	( [ &whole sym ]
;;	  [ &environment sym ]
;;	  { v }*
;;	  [ &optional { sym || ( v [ init [ v ] ]) }* ]
;;	  {  [ { &rest || &body } v ]
;;	     [ &key { sym || ( { sym || ( key v) } [ init [ v ]]) }*
;;		    [ &allow-other-keys ]]
;;	     [ &aux { sym || ( v [ init ]) }* ]
;;	  ||  . sym }
;;	)
;;
;; wobei v eine Abkuerzung fuer { defmacro-lambda-list || sym } ist.
;; Ein Symbol kann als DEFMACRO lambda-liste angegeben werden;
;; in diesem Fall ist
;; (DEFMACRO <name> <symbol> ...) aequivalent zu
;; (DEFMACRO <name> (&REST <symbol>) ...).
;; Defmacro-lambda-list ist definiert als:
;;
;;	( { v }*
;;	  [ &optional { sym || ( v [ init [ v ] ]) }* ]
;;	  {  [ { &rest || &body } v ]
;;	     [ &key { sym || ( { sym || ( key v) } [ init [ v ]]) }*
;;		    [ &allow-other-keys ]]
;;	     [ &aux { sym || ( v [ init ]) }* ]
;;	  ||  . sym }
;;	)


(defconstant MACRO_BAD_KEY "Defmacro-lambda-list contains illegal use of ~s.")

;;------------------------------------------------------------------------------
;; Generiert aus einer Makro-Definition die entsprechende Expansionsfunktion
;;------------------------------------------------------------------------------
(defun p1-gen-macro-function (lambda-list body)
  (let ((var-init-queue (empty-queue))
        (key-check nil)
        (arg-check nil)
        whole                           ; Whole Variable
        env                             ; Environment Variable
        (envp nil))                     ; &environment angegben ?

    ;; Sicherstellen, dass eine Liste vorliegt
    ;;----------------------------------------
    (cond ((listp lambda-list))
          ((symbolp lambda-list) (setq lambda-list (list '&rest lambda-list)))
          (t (clicc-error
              "The defmacro-lambda-list ~s is not a symbol or a list."
              lambda-list)))
  
    ;; &whole und &environment gesondert behandeln
    ;;---------------------------------------------
    (cond ((and (listp lambda-list) (eq (car lambda-list) '&whole))
           (setq whole (cadr lambda-list))
           (setq lambda-list (cddr lambda-list)))
          (t (setq whole (gensym))))
    (cond ((and (listp lambda-list) (eq (car lambda-list) '&environment))
           (setq env (cadr lambda-list))
           (setq lambda-list (cddr lambda-list))
           (setq envp t))
          (t (setq env (gensym))))

    (labels ((p1-macro-lambda-list (lambda-list whole top)
               (do ((optionalp nil)
                    (restp nil)
                    (keyp nil)
                    (allow-other-keys-p nil)
                    (auxp nil)
                    (rest nil)
                    (allow-other-keys nil)
                    (keys nil)
                    (no-check nil)
                    (n (if top 1 0)))   ; Position des gerade bearbeiteten Par.
      
                   ((not (consp lambda-list))
                    (when lambda-list
                      (when restp (clicc-error MACRO_BAD_KEY '&rest))
                      (add-q (list lambda-list (nthcdr-code n whole))
                             var-init-queue)
                      (setq no-check t))
                    (when (and rest (not allow-other-keys))
                      (push (cons rest keys) key-check))
                    (unless no-check (push (cons whole n) arg-check)))

                 (let ((v (car lambda-list)))
                   (cond
                     ((eq v '&optional)
                      (when optionalp (clicc-error MACRO_BAD_KEY '&optional))
                      (setq optionalp t)
                      (pop lambda-list))
        
                     ((or (eq v '&rest) (eq v '&body))
                      (when restp (clicc-error MACRO_BAD_KEY v))
                      (p1-macro-param (cadr lambda-list) (nthcdr-code n whole))
                      (setq restp t optionalp t no-check t)
                      (setq lambda-list (cddr lambda-list)))
        
                     ((eq v '&key)
                      (when keyp (clicc-error MACRO_BAD_KEY '&key))
                      (setq rest (gensym))
                      (add-q (list rest (nthcdr-code n whole)) var-init-queue)
                      (setq keyp t restp t optionalp t no-check t)
                      (pop lambda-list))
        
                     ((eq v '&allow-other-keys)
                      (when (or (not keyp) allow-other-keys-p)
                        (clicc-error MACRO_BAD_KEY '&allow-other-keys))
                      (setq allow-other-keys-p t)
                      (setq allow-other-keys t)
                      (pop lambda-list))
        
                     ((eq v '&aux)
                      (when auxp (clicc-error MACRO_BAD_KEY '&aux))
                      (setq auxp t
                            allow-other-keys-p t
                            keyp t
                            restp t
                            optionalp t)
                      (pop lambda-list))
        
                     (auxp
                      (let (x (init nil))
                        (cond ((symbolp v) (setq x v))
                              (t (setq x (car v))
                                 (unless (endp (cdr v)) (setq init (cadr v)))))
                        (p1-macro-param x init))
                      (pop lambda-list))
        
                     (keyp
                      (let ((temp (gensym)) x k (init nil) (sv nil))
                        (cond ((symbolp v)
                               (setq x v k (intern (string v) "KEYWORD")))
                              (t (if (symbolp (car v))
                                     (setq x (car v)
                                           k (intern (string (car v))
                                                     "KEYWORD"))
                                     (setq x (cadar v) k (caar v)))
                                 (unless (endp (cdr v))
                                   (setq init (cadr v))
                                   (unless (endp (cddr v))
                                     (setq sv (caddr v))))))
                        (p1-macro-param temp `(getf ,rest ,k 'failed))
                        (p1-macro-param x `(if (eq ,temp 'failed) ,init ,temp))
                        (when sv (p1-macro-param sv `(not (eq ,temp 'failed))))
                        (push k keys))
                      (pop lambda-list))
        
                     (optionalp
                      (let (x (init nil) (sv nil))
                        (cond ((symbolp v) (setq x v))
                              (t (setq x (car v))
                                 (unless (endp (cdr v))
                                   (setq init (cadr v))
                                   (unless (endp (cddr v))
                                     (setq sv (caddr v))))))
                        (p1-macro-param x
                                        `(if ,(nthcdr-code n whole)
                                          ,(nth-code n whole)
                                          ,init))
                        (when sv 
                          (p1-macro-param sv `(not (null 
                                                    ,(nthcdr-code n whole))))))
                      (incf n)
                      (pop lambda-list))
        
                     (t (p1-macro-param v `(if ,(nthcdr-code n whole)
                                            ,(nth-code n whole)
                                            (dm-too-few-arguments)))
                        (incf n)
                        (pop lambda-list))))))
             

             ;;----------------------------------------------------------------
             ;; Bearbeitet einen Makro-parameter, der ein Symbol oder eine
             ;; dotted Liste sein kann.
             ;;----------------------------------------------------------------
             (p1-macro-param (v init)
               (if (symbolp v)
                   (add-q (if init (list v init) v) var-init-queue)
                   (let ((temp (gensym)))
                     (add-q (if init (list temp init) temp) var-init-queue)
                     (p1-macro-lambda-list v temp nil))))
             
             ;;----------------------------------------------------------------
             ;; generiert Code aequivalent zu (nth n v)
             ;;----------------------------------------------------------------
             (nth-code (n v)
               (if (> n 3)
                   `(nth ,n ,v)
                   (case n
                     (0 `(car ,v))
                     (1 `(cadr ,v))
                     (2 `(caddr ,v))
                     (3 `(cadddr ,v)))))
             


             ;;----------------------------------------------------------------
             ;; generiert Code aequivalent zu (nthcdr n v)
             ;;----------------------------------------------------------------
             (nthcdr-code (n v)
               (if (> n 4)
                   `(nthcdr ,n ,v)
                   (case n
                     (0 v)
                     (1 `(cdr ,v))
                     (2 `(cddr ,v))
                     (3 `(cdddr ,v))
                     (4 `(cddddr ,v)))))
             
             ;;----------------------------------------------------------------
             ;; Resultate: doc decls body
             ;;----------------------------------------------------------------
             (find-doc (body ignore-doc)
               (if (endp body)
                   (values nil nil nil)
                   (let ((d (p1-macroexpand (car body))))
                     (cond ((stringp d)
                            (if (or (endp (cdr body)) ignore-doc)
                                (values nil nil (cons d (cdr body)))
                                (multiple-value-bind (doc decls b)
                                    (find-doc (cdr body) t)
                                  (declare (ignore doc))
                                  (values d decls b))))
                           ((and (consp d) (eq (car d) 'declare))
                            (multiple-value-bind (doc decls b)
                                (find-doc (cdr body) ignore-doc)
                              (values doc (cons d decls) b)))
                           (t (values nil nil (cons d (cdr body)))))))))
             
      (p1-macro-lambda-list lambda-list whole t)
  
      (multiple-value-bind (doc decls body) (find-doc body nil)
        (declare (ignore doc))

        ;; key-check enthaelt Tupel, bestehend aus &Rest-Variable und erlaubten
        ;; Keywords
        ;;---------
        (dolist (kc key-check)
          (push `(unless (getf ,(car kc) :allow-other-keys)
                  (do ((lambda-list ,(car kc) (cddr lambda-list)))
                      ((endp lambda-list))
                    (unless (member (car lambda-list) ',(cdr kc))
                      (dm-key-not-allowed (car lambda-list)))))
                body))

        ;; arg-check enthaelt Tupel, bestehend aus &Whole-Variable und Anzahl
        ;; der zulaessigen Argumente.
        ;;---------------------------
        (dolist (ac arg-check)
          (push `(unless (endp ,(nthcdr-code (cdr ac) (car ac)))
                  (dm-too-many-arguments))
                body))

        (unless (empty-queue-p var-init-queue)
          (setq body `((let* ,(queue2list var-init-queue) ,@body))))

        (unless envp (push `(declare (ignore ,env)) body))
    
        `((,whole ,env) ,@(append decls body))))))


;;------------------------------------------------------------------------------
;; Definition von Fehlerbehandlungsfunktionen fuer die Makroexpansion
;;------------------------------------------------------------------------------
(defun init-macro-error-funs ()
  (in-compile-time-env
   (set-sys-fun 
    'dm-too-few-arguments
    (p1-named-lambda 
     (make-instance 'global-fun
                    :symbol 'dm-too-few-arguments) 
     'dm-too-few-arguments
     'dm-too-few-arguments
     '(() (error "Too few arguments are supplied to defmacro-lambda-list."))))
  
   (set-sys-fun 
    'dm-too-many-arguments
    (p1-named-lambda
     (make-instance 'global-fun
                    :symbol 'dm-too-many-arguments)
     'dm-too-many-arguments
     'dm-too-many-arguments
     '(() (error "Too many arguments are supplied to defmacro-lambda-list."))))
  
   (set-sys-fun 
    'dm-key-not-allowed
    (p1-named-lambda 
     (make-instance 'global-fun
                    :symbol 'dm-key-not-allowed)
     'dm-key-not-allowed
     'dm-key-not-allowed
     '((key) (error "The key ~s is not allowed." key))))))

;;------------------------------------------------------------------------------
(provide "p1macro")

