;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Type Specifiers
;;;
;;; $Revision: 1.30 $
;;; $Log: p1type.lisp,v $
;;; Revision 1.30  1993/12/14  13:01:29  sma
;;; Vorhandensein des neuen Typtests rt::plain-vector-p ausgenutzt für
;;; Test auf (simple-array * (*)).
;;;
;;; Revision 1.29  1993/12/09  14:22:17  sma
;;; simple-vectorp und simple-stringp in simple-vector-p (dito string)
;;; korrigiert, desweiteren wird jetzt auf simple-vector-p auch bei
;;; (simple-array T (*)) statt (simple-array * (*)) optimiert (dito
;;; string). Außerdem einige Leerzeilen am Ende gelöscht.
;;;
;;; Revision 1.28  1993/11/09  11:52:05  hk
;;; Fehler in p1-type-expand behoben: (eq 1 ...) --> (eql 1 ...)
;;;
;;; Revision 1.27  1993/09/07  09:50:04  ft
;;; p1-def-built-in hat einen neuen Key-Parameter fuer die Angabe eines
;;; Werts fuer die Annotation order erhalten.
;;;
;;; Revision 1.26  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.25  1993/05/06  16:43:44  hk
;;; kleiner Fehler
;;;
;;; Revision 1.24  1993/05/06  16:39:46  hk
;;; Message fuer DEF-BUILT-IN.
;;;
;;; Revision 1.23  1993/04/21  08:58:59  ft
;;; Anpassung an die geaenderten Parameter von p1-def-built-in.
;;;
;;; Revision 1.22  1993/04/20  14:33:54  ft
;;; Erweiterung um Funktionen fuer die neue Top-Level-Form 'def-built-in'.
;;;
;;; Revision 1.21  1993/04/14  16:53:49  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.20  1993/04/13  11:49:38  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.19  1993/04/13  11:45:42  hk
;;; Vorkommen von check-integer durch seinen Rumpf ersetzt.
;;;
;;; Revision 1.18  1993/04/08  10:51:25  hk
;;; L:: vor find-class vergessen.
;;;
;;; Revision 1.17  1993/04/06  15:02:51  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.16  1993/04/06  14:34:50  hk
;;; :STRUCT Type wurde nicht benutzt, also gestrichen, :class Type expandiert
;;; in (typep-class x (find-class 'c)), Symbole mit L:: gekennzeichnet,
;;; Optimierung fuer array-internal, integer-internal schon in pass1.
;;;
;;; Revision 1.15  1993/04/06  14:00:57  ft
;;; p1-type-expand erzeugt jetzt wieder reine Quellsprach-Ausdruecke.
;;;
;;; Revision 1.14  1993/04/03  10:04:45  hk
;;; p1-typep als Quelltext transformierendes compiler-macro implementiert,
;;; fuer Klassen wird faelschlich schon Zwischensprache verwendet.
;;;
;;; Revision 1.13  1993/03/23  07:38:01  ft
;;; p1-typep liefert jetzt zwei Werte (siehe p1-call).
;;;
;;; Revision 1.12  1993/03/12  09:53:28  ft
;;; Benutzung von class-defs statt rt::sym2class.
;;;
;;; Revision 1.11  1993/02/16  16:33:28  hk
;;; ...-internal Typen intern im clicc-lisp Package, Revision Keyword eingefuegt.
;;;
;;; Revision 1.10  1992/12/10  06:31:23  ft
;;; Auswertung der Expansion von Klassentypen verzoegert.
;;;
;;; Revision 1.9  1992/11/25  17:49:15  hk
;;; T-INTERNAL -> (p1-form 'T), ..
;;;
;;; Revision 1.8  1992/11/17  12:14:55  ft
;;; Fehler in der Verarbeitung von Klassen durch p1-typep behoben.
;;;
;;; Revision 1.7  1992/11/11  12:55:45  ft
;;; Umgang mit Klassen korrigiert.
;;;
;;; Revision 1.6  1992/10/15  09:46:53  ft
;;; Anpassung an die Aenderung in global-env:types.
;;;
;;; Revision 1.5  1992/10/12  12:02:50  ft
;;; Fehler im case von p1-type-expand behoben.
;;;
;;; Revision 1.4  1992/10/07  13:51:28  ft
;;; Anpassung an die Aenderungen im Slot types des global-env.
;;;
;;; Revision 1.3  1992/08/05  11:59:02  hk
;;; Syntaktische Aenderungen.
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
;; DEFTYPE name lambda-list { declaration || doc-string }* { form }*  [Macro]
;;------------------------------------------------------------------------------
(defun p1-deftype (name_lambda-list_body)
  (when (atom name_lambda-list_body)
    (clicc-error ILLEGAL_CALL "DEFTYPE" "(NAME (&REST ARGLIST) &BODY BODY)"))
  (let ((name             (first name_lambda-list_body))
        (lambda-list_body (rest  name_lambda-list_body))
        type-def)
    (unless (symbolp name) (clicc-error NO_NAME name "DEFTYPE"))
    (clicc-message "Analyse DEFTYPE      ~A" name)
    (setq type-def (get-type-def name))
    (ecase (car type-def)
      (:TYPE
       (clc-error "Type Specifier ~A declared twice" name))
      (:CLASS
       (clc-error "Type Specifier ~A already declared as a class" name))
      ((nil)

       ;; Bekanntgeben der Makrodefinition
       ;; --------------------------------
       (multiple-value-bind (lambda-list body) 
           (parse-type-expander lambda-list_body)
         (set-type-def name (p1-gen-macro-fun name lambda-list body)))))))

;;------------------------------------------------------------------------------
;; DEF-BUILT-IN name
;;       :TYPE-EXPANDER ( lambda-list { declaration || doc-string }* { form }* )
;;       :SUPERCLASSES ( { built-in-class-name }* )
;;------------------------------------------------------------------------------
(defun p1-def-built-in (args)
  (when (atom args)
    (clicc-error
     ILLEGAL_CALL "DEF-BUILT-IN"
     "NAME :TYPE-EXPANDER TYPE-EXPANDER :SUPERCLASSES SUPERCLASSES"))
  (apply #'do-def-built-in args))

(defun do-def-built-in (name &key type-expander superclasses order)
  (let ((built-in-def (get-built-in name)))
    (clicc-message "Analyse DEF-BUILT-IN ~A" name)
    (unless (symbolp name) (clicc-error NO_NAME name "DEF-BUILT-IN"))
    (ecase (car built-in-def)
      (:BUILT-IN
       (clicc-error "built-in class ~A declared twice" name))
      (:CLASS
       (clicc-error "Type Specifier ~A already declared as a class" name))
      (:TYPE
       (clicc-error "Type Specifier ~A already declared as a type" name))
      (:struct
       (clicc-error "Type Specifier ~A already declared as a structure" name))
      ((nil)
       (multiple-value-bind (lambda-list body) 
           (parse-type-expander type-expander)
         (setf type-expander (p1-gen-macro-fun name lambda-list body))
         (setf superclasses
          (mapcar #'(lambda (superclass-name)
                      (let ((superclass-def (get-class-entry superclass-name))
                            superclass)
                        (ecase (car superclass-def)
                          ((:TYPE :STRUCT)
                           (clicc-error 
                            "~S is not a legal superclass" 
                            superclass-name))
                          (:BUILT-IN
                           (setf superclass 
                             (cdr superclass-def)))
                          (:CLASS
                           (clicc-error
                            "There is already a user-defined class named ~A"
                            superclass-name))
                          ((nil)
                           (clicc-error
                            "The superclass ~A is not defined" 
                            superclass-name)))
                        superclass))
                  superclasses))

         ;; Bekanntgeben der Makrodefinition
         ;; --------------------------------
         (set-built-in name (make-instance 'built-in-class-def
                                           :symbol name
                                           :type-expander type-expander
                                           :super-list superclasses
                                           :slot-descr-list '()
                                           :order order)))))))

;;------------------------------------------------------------------------------
;; Parser fuer die Parameter eines DEFTYPE bzw. den expander eines DEF-BUILT-IN
;;------------------------------------------------------------------------------
(defun parse-type-expander (lambda-list_body)
  (when (atom lambda-list_body) (clicc-error ILLEGAL_L_EXPR))
  (let ((lambda-list (first lambda-list_body))
        (body (rest  lambda-list_body)))
    
    ;; Replace undefaultized optional parameter X by (X '*).
    ;;------------------------------------------------------
    (do ((l lambda-list (cdr l))
         (m nil (cons (car l) m)))
        ((null l))
      (when (member (car l) lambda-list-keywords)
        (unless (eq (car l) '&optional) (return nil))
        (setq m (cons '&optional m))
        (setq l (cdr l))
        (do ()
            ((or (null l) (member (car l) lambda-list-keywords)))
          (if (symbolp (car l))
              (setq m (cons (list (car l) ''*) m))
              (setq m (cons (car l) m)))
          (setq l (cdr l)))
        (setq lambda-list (nreconc m l))
        (return nil)))
    (values lambda-list body)))
    
;;------------------------------------------------------------------------------
;; Expandiert einen spezifizierten Typ in die kanonische Darstellung
;;------------------------------------------------------------------------------
(defun p1-type-expand (type &aux atomic-type)
  (loop
    (setq atomic-type nil)
    (when (atom type)
      (setq type (list type))
      (setq atomic-type t))
    (let* ((type-cons (get-type-def (first type)))
           (type-key  (first type-cons))
           (type-expander (rest type-cons)))
      (ecase type-key
        (:TYPE (setq type (p1-expand-user-macro type-expander type)))
        (:BUILT-IN (setq type 
                         (p1-expand-user-macro (?type-expander type-expander) 
                                               type)))
        (:CLASS (unless (eql 1 (length type))
                  (clc-error "illegal type specifier ~a" type))
                (return `(L::CLASS-INTERNAL ,(first type))))
        ((nil) (return (if atomic-type (first type) type)))))))

;;------------------------------------------------------------------------------
;; Prueft, ob ein Ausdruck ein Aufruf ist
;;------------------------------------------------------------------------------
(defun callp (form)
  (and (consp form) (not (eq (first form) 'QUOTE))))

;;------------------------------------------------------------------------------
(defun quote-form-p (form)
  (and (consp form) (eq (first form) 'QUOTE)))

;;------------------------------------------------------------------------------
;; TYPEP object type
;;
;; Die Expansionsfunktion fuer das Compiler-Macro zu typep.
;; Falls der Type-Specifier als Konstante angegeben ist, wird expandiert,
;; sonst wird keine Expansion vorgenommen.
;;
;; Z.B. (typep x 'keyword)    --> (keywordp x)
;; (typep x '(and t1 t2))     --> (and (typep x 't1) (typep x 't2))
;; (typep x '(or  t1 t2))     --> (or  (typep x 't1) (typep x 't2))
;; (typep x '(not t))         --> (not (typep x 't))
;; (typep x '(member f1 f2))  --> (or (eql x 'f1) (eql x 'f2))
;; (typep x '(satisfies fun)) --> (fun x)
;;------------------------------------------------------------------------------
(defun  p1-typep (form)
  (match-args
   form TYPEP (object type)
   (when (not (quote-form-p type))
     (return-from p1-typep form))
   (when (callp object)
     (let ((newsym (gensym)))
       (return-from p1-typep `(LET ((,newsym ,object)) (TYPEP ,newsym ,type)))))
   (let ((expanded-type (p1-type-expand (parse-quote (rest type)))))
     (cond
       ((eq expanded-type 'L::NIL) 'L::NIL)   ; Type NIL: immer FALSE
       ((eq expanded-type 'L::T)   'L::T)     ; Type T  : immer TRUE
       ((atom expanded-type)
        (clc-error "Illegal type specifier ~A" expanded-type)
        'L::NIL)
       (t (let ((type-specifier (first expanded-type))
                (args           (rest  expanded-type)))
            (case type-specifier
              (L::MEMBER
               `(L::OR ,@(mapcar #'(lambda (object-of-set)
                                  `(L::EQL ,object (L::QUOTE ,object-of-set)))
                       args)))
              (L::EQL
               (unless (eql 1 (length args))
                 (clc-error "Illegal type specifier ~A" expanded-type))
               `(L::EQL ,object (L::QUOTE ,(first args))))
              (L::NOT
               (unless (eql 1 (length args))
                 (clc-error "Illegal type specifier ~A" expanded-type))
               `(L::NOT (L::TYPEP ,object (L::QUOTE ,(first args)))))
              (L::AND
               `(L::AND ,@(mapcar 
                        #'(lambda (specified-type)
                            `(L::TYPEP ,object (L::QUOTE ,specified-type)))
                        args)))
              (L::OR
               `(L::OR ,@(mapcar 
                       #'(lambda (specified-type)
                           `(L::TYPEP ,object (L::QUOTE ,specified-type)))
                       args)))
              
              (L::SATISFIES
               (unless (eql 1 (length args))
                 (clc-error "Illegal type specifier ~A" expanded-type))
               `(,(first args) ,object))
              
              (L::ARRAY-INTERNAL
               (let ((type (first  args))
                     (dims (second args)))
                 (cond
                   ((and (listp dims) (eql 1 (length dims)))
                    (cond
                      ((or (eq type 'L::character)
                           (eq type 'L::standard-char))
                       (if (eq 'L::* (first dims))
                           `(L::stringp ,object)
                           `(and (L::stringp ,object)
                             (L::eql (L::length ,object)
                              ,(first dims)))))
                      ((eq type 'L::*)
                       (if (eq 'L::* (first dims))
                           `(L::vectorp ,object)
                           `(L::and (L::vectorp ,object)
                             (L::eql (L::length ,object) ,(first dims)))))
                      (t `(RT::CHECK-ARRAY
                           ,object (L::QUOTE ,type) (L::QUOTE ,dims)))))
                   ((and (eq type 'L::*) (eq dims 'L::*))
                    `(L::arrayp ,object))
                   (t `(RT::CHECK-ARRAY
                        ,object (L::QUOTE ,type) (L::QUOTE ,dims))))))

              (L::SIMPLE-ARRAY-INTERNAL
               (let ((type (first  args))
                     (dims (second args)))
                 (cond
                   ((and (listp dims) (eql 1 (length dims)))
                    (cond
                      ((or (eq type 'L::character)
                           (eq type 'L::standard-char))
                       (if (eq 'L::* (first dims))
                           `(L::simple-string-p ,object)
                           `(L::and (L::simple-string-p ,object)
                             (L::eql (L::length ,object) ,(first dims)))))
                      ((eq type 'L::T)
                       (if (eq 'L::* (first dims))
                           `(L::simple-vector-p ,object)
                           `(L::and (L::simple-vector-p ,object)
                             (L::eql (L::length ,object) ,(first dims)))))
                      ((eq type 'L::*)
                       (if (eq 'L::* (first dims))
                           `(RT::plain-vector-p ,object)
                           `(L::and (RT::plain-vector-p ,object)
                             (L::eql (L::length ,object) ,(first dims)))))
                      (t
                       `(RT::check-simple-array
                         ,object (L::QUOTE ,type) (L::QUOTE ,dims)))))
                   ((and (eq type 'L::*) (eq dims 'L::*))
                    `(rt::simple-array-p ,object))
                   (t `(RT::check-simple-array
                        ,object (L::QUOTE ,type) (L::QUOTE ,dims))))))
              
              (L::INTEGER-INTERNAL
               (let ((low  (first  args))
                     (high (second args)))
                 (cond
                   ((and (integerp low) (integerp high))
                    `(L::AND (L::INTEGERP ,object) (L::<= ,low ,object ,high)))
                   ((integerp low) `(RT::FIXNUM-LOW-P ,object ,low))
                   ((integerp high) `(RT::FIXNUM-LOW-P ,object ,high))
                   (T `(L::INTEGERP ,object)))))
              
              (L::CLASS-INTERNAL
               `(RT::TYPEP-CLASS ,object (L::find-class ',(first  args))))
              (t (clc-error "Illegal type specifier ~S" expanded-type)
                 'L::NIL))))))))

;;------------------------------------------------------------------------------
(provide "p1type")
