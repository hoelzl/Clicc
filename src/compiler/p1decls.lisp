;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : 9. Declarations
;;;
;;; $Revision: 1.17 $
;;; $Log: p1decls.lisp,v $
;;; Revision 1.17  1994/02/21  10:02:19  ft
;;; Proklamation von Klassen als INSTANCEABLE, SUBCLASSABLE und
;;; SPECIALIZABLE ermöglicht.
;;;
;;; Revision 1.16  1994/02/08  13:28:35  hk
;;; name2fun nach p1env
;;;
;;; Revision 1.15  1994/02/08  13:16:40  sma
;;; Annotation my-last-arg-may-be-rest-var bei funs, die angibt, daß das
;;; letzte Argument der hiermit annotierten Funktion auch ein rest-listen
;;; Parameter sein kann. Die Annotation enthält in diesem Falle den Namen
;;; (als Keyword) ihrer Funktion.
;;;
;;; Revision 1.14  1994/02/02  09:22:05  hk
;;; Bearbeitung von CLiCC-spezifischen Deklarationen eingefügt, mit denen
;;; angegeben werden kann, wie Funktionen des Laufzeitsystems optimiert
;;; werden sollen.
;;;
;;; Revision 1.13  1993/08/19  16:49:22  hk
;;; Verwendung von lex-var-name-p.
;;;
;;; Revision 1.12  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.11  1993/05/14  09:30:08  hk
;;; clicc-lisp:: -> L::, in Format-Strings: ~A -> ~S.
;;;
;;; Revision 1.10  1993/05/06  14:44:53  hk
;;; p1-proclaim korrigiert.
;;;
;;; Revision 1.9  1993/05/03  14:49:26  hk
;;; DECLAIM gemaess CLTL2 implementiert, PROCLAIM darauf zurueckgefuehrt.
;;;
;;; Revision 1.8  1993/04/03  09:38:32  hk
;;; Inline Deklarationen werden vollstaendig ignoriert.
;;;
;;; Revision 1.7  1993/02/16  17:02:03  hk
;;; Revision Keyword eingefuegt, Symbole des zu uebersetzenden Programms
;;; durch clicc-lisp:: gekennzeichnet.
;;;
;;; Revision 1.6  1993/01/29  15:50:39  hk
;;; STRING-CHAR entfernt aus p1-check-if-valid-declaration.
;;;
;;; Revision 1.5  1993/01/08  15:50:44  hk
;;; clicc-error -> clcerror
;;;
;;; Revision 1.4  1992/07/23  10:01:30  hk
;;; :USER-FUNCTION --> :USER-FUN, etc. .
;;;
;;; Revision 1.3  1992/07/22  17:34:15  hk
;;; Zugriffsfkt. auf (global)-env geaendert.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(defconstant SYM_DECL "In declaration ~S ~S must be a symbol")

;;------------------------------------------------------------------------------
;; Ueberprueft eine Deklaration auf syntaktische Korrektheit bzgl. einer Liste
;; von Deklarations-Spezifikationen.
;; declaration ::= ( { decl-specifier . rest }* )
;;------------------------------------------------------------------------------
(defun p1-check-declare (decl-specs)
  (do (decl-spec)
      ((p1-endp decl-specs))
    (setq decl-spec (pop decl-specs))
    (when (atom decl-spec)
      (clicc-error "~S is no declaration specifier" decl-spec))))

;;------------------------------------------------------------------------------
(defun p1-proclaim-special (symbols)
  (dolist (symbol symbols)
    (cond
      ((not (symbolp symbol))
       (clc-error SYM_DECL `(PROCLAIM (QUOTE (SPECIAL ,@symbols))) symbol))

      ;; Von einer als global SPECIAL deklarierten Variablen duerfen keine
      ;; lokalen lexikalischen Bindungen gleichen Namens vorher existieren.
      ;;-------------------------------------------------------------------
      ((lex-var-name-p symbol)
       (clc-error
        "There exists a lexical binding of SPECIAL proclaimed variable ~S"
        symbol))
    
      ;; Wird ein Symbol als SPECIAL proklamiert, so muss das Symbol in die
      ;; Symboltabelle eingetragen werden.
      ;;----------------------------------
      (t (p1-make-symbol symbol)
         (push symbol (?special-decls *GLOBAL-ENVIRONMENT*))))))

;;------------------------------------------------------------------------------
(defun p1-proclaimed-special-p (symbol)
  (member symbol (?special-decls *GLOBAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
(defun p1-declare-special (symbols)
  (dolist (symbol symbols)
    (if (not (symbolp symbol))
        (clc-error SYM_DECL `(DECLARE (SPECIAL ,@symbols)) symbol)
        (p1-bind-special-var symbol))))

;;------------------------------------------------------------------------------
(defun p1-proclaim-ignore (variables)
  (dolist (variable variables)
    (if (not (symbolp variable))
        (clc-error SYM_DECL `(PROCLAIM (QUOTE (IGNORE ,@variables))) variable)
        (push variable (?ignore-decls *GLOBAL-ENVIRONMENT*)))))

;;------------------------------------------------------------------------------
(defun p1-proclaim-export-goal (symbols goal)
  (dolist (symbol symbols)
    (let ((class
           (cdr (or (get-class-entry symbol) (bind-forward-class symbol)))))
      (cond
        ((built-in-class-def-p class)
         (clc-error "it is illegal to export the built-in-class ~A." symbol))
        ((imported-class-p class)
         (clc-error "it is illegal to export the imported-class ~A." symbol))
         ((defined-class-p class)
          (unless (member goal (?export-goals class))
            (case goal
              (:instanceable
               (push :full-instanceable (?export-goals class)))
              (:subclassable
               (push :full-subclassable (?export-goals class)))
              (:specializable
               (push :full-specializable (?export-goals class))))))))))

;;------------------------------------------------------------------------------
(defun p1-declare-ignore (variables)
  (dolist (variable variables)
    (if (not (symbolp variable))
        (clc-error SYM_DECL `(DECLARE (IGNORE ,@variables)) variable)
        (push variable (?ignore-decls *LOCAL-ENVIRONMENT*)))))

;;------------------------------------------------------------------------------
(defun p1-declared-ignore-p (variable)
  (or (member variable (?ignore-decls *LOCAL-ENVIRONMENT*))
      (member variable (?ignore-decls *GLOBAL-ENVIRONMENT*))))

;;------------------------------------------------------------------------------
(defun p1-check-if-valid-declaration (decl-specifier)
   (when (not (member decl-specifier

   '(L::INLINE L::NOTINLINE
     L::TYPE L::FTYPE L::OPTIMIZE
     L::DECLARATION L::ARRAY L::ATOM
     L::BIGNUM L::BIT L::BIT-VECTOR
     L::CHARACTER L::COMMON L::COMPILED-FUNCTION
     L::COMPLEX L::CONS L::DOUBLE-FLOAT
     L::FIXNUM L::FLOAT L::FUNCTION
     L::HASH-TABLE L::INTEGER L::KEYWORD
     L::LIST L::LONG-FLOAT L::NIL L::NULL
     L::NUMBER L::PACKAGE L::PATHNAME
     L::RANDOM-STATE L::RATIO L::RATIONAL
     L::READTABLE L::SEQUENCE L::SHORT-FLOAT
     L::SIGNED-BYTE L::SIMPLE-ARRAY
     L::SIMPLE-BIT-VECTOR L::SIMPLE-STRING
     L::SIMPLE-VECTOR L::SINGLE-FLOAT
     L::STANDARD-CHAR L::STREAM L::STRING
     L::SYMBOL L::T L::UNSIGNED-BYTE
     L::VECTOR)))

      (clicc-warning "The declaration specifier ~S is unknown."
                     decl-specifier)))

;;------------------------------------------------------------------------------
;; declare {decl-spec}*
;;------------------------------------------------------------------------------
(defun p1-declare (declaration)
  (dolist (decl-spec (rest declaration))
    (let ((decl-value (rest decl-spec)))
      (case (first decl-spec)
        (L::SPECIAL
         (p1-declare-special decl-value))
        (L::IGNORE
         (p1-declare-ignore decl-value))
        (:simp-when-n-args
         (setf (?simp-when-n-args *current-fun*)
               (list (first decl-value) (name2fun (second decl-value)))))
        (:simp-when-no-result
         (setf (?simp-when-no-result *current-fun*)
               (name2fun (first decl-value))))
        (:simp-when-arg-n=cons
         (setf (?simp-when-arg-n=cons *current-fun*)
               (list (first decl-value) (name2fun (second decl-value)))))
        (:simp-when-some-arg-not-cons/pathn/string/bitv
         (setf (?simp-when-some-arg-not-cons/pathn/string/bitv *current-fun*)
               (name2fun (first decl-value))))
        (:simp-when-some-arg-not-num/char
         (setf (?simp-when-some-arg-not-num/char *current-fun*)
               (name2fun (first decl-value))))
        (:simp-test-fun-when-not-testnot
         (setf (?simp-test-fun-when-not-testnot *current-fun*)
               (list (pop decl-value)
                     (pop decl-value)
                     (p1-make-symbol (pop decl-value))
                     (name2fun (pop decl-value))
                     (p1-make-symbol (pop decl-value)))))
        (:simp-when-only-test=value
         (setf (?simp-when-only-test=value *current-fun*)
               (list (pop decl-value)
                     (p1-make-symbol (pop decl-value))
                     (name2fun (pop decl-value))
                     (name2fun (pop decl-value)))))
        (:my-last-arg-may-be-rest-var
         (setf (?my-last-arg-may-be-rest-var *current-fun*) (first decl-value)))
        (otherwise
         (p1-check-if-valid-declaration (first decl-spec)))))))

;;------------------------------------------------------------------------------
;; PROCLAIM decl-spec
;; Toplevel Form
;;------------------------------------------------------------------------------
(defun p1-proclaim (decl-spec_rest)
  (unless (= (length decl-spec_rest) 1)
    (clicc-error NARGS_NC "PROCLAIM" (length decl-spec_rest)))

  ;; EVALUIEREN des Argumentes von PROCLAIM zur Uebersetzungszeit, dann weiter
  ;; mit DECLAIM
  ;;----------------------------------------------------------------------
  (p1-top-level-form `(L::DECLAIM ,(p1-eval (first decl-spec_rest)))))

;;------------------------------------------------------------------------------
;; DECLAIM {decl-spec}*
;; Toplevel Form
;;------------------------------------------------------------------------------
(defun p1-declaim (declarations)
  (dolist (decl-spec declarations)
    (case (first decl-spec)
      (L::SPECIAL
       (p1-proclaim-special (rest decl-spec)))
      (L::IGNORE
       (p1-proclaim-ignore (rest decl-spec)))
      ((:INSTANCEABLE :SUBCLASSABLE :SPECIALIZABLE)
       (p1-proclaim-export-goal (rest decl-spec) (first decl-spec)))
      (otherwise
       (p1-check-if-valid-declaration (first decl-spec))))))

;;------------------------------------------------------------------------------
;; Aufsplitten eines Rumpfes in die Komponenten
;; Dokumentationsstring, Deklaration und Rumpf
;; Resultat: (MV) doc-string (DECLARE {decl-spec}*) {form}*
;;------------------------------------------------------------------------------
(defun p1-get-doc-string/decl/forms (body)
  (let ((doc-string "")
        (doc-string-p nil)              ; supplied-p fuer doc-string
        (decl-list ())
        (forms body)
        first-form)
    (loop
      (cond
        ((p1-endp forms) (return))
        ((stringp (setq first-form (first forms)))
         (cond
           ;; Doc-String bereits gelesen ?
           ;;-----------------------------
           (doc-string-p (return))

           ;; Folgt <declaration> oder <form> ?
           ;;----------------------------------
           ((rest forms)
            (setq doc-string first-form)
            (setq doc-string-p t))
           
           ;; Sonst ist String die einzige <form> des Rumpfes
           ;;------------------------------------------------
           (t (return))))
        
        ;; Eine Deklaration muss nach [CL2] direkt erkannt werden.
        ;; Darf also nicht mehr aus einer Makroexpansion entstehen.
        ;;---------------------------------------------------------
        (t
         ;; Ist erste Form ein Atom oder beginnt nicht mit DECLARE ?
         ;;---------------------------------------------------------
         (if (or (atom first-form)
                 (not (eq (first first-form) 'L::DECLARE)))
           (return)
           
           ;; DECLARE-Form vorhanden, aufsammeln in 'decl-list'
           ;;--------------------------------------------------
           (setq decl-list
                 (append (rest first-form) decl-list)))))
      (pop forms))

    ;; Reihenfolge der Deklarationen beachten !
    ;;-----------------------------------------
    (values doc-string (cons 'L::DECLARE (reverse decl-list)) forms)))

;;------------------------------------------------------------------------------
;; Aufsplitten eines Rumpfes in die Komponenten Deklaration und Rumpf
;; Resultat: (MV) (DECLARE {decl-spec}*) {form}*
;;------------------------------------------------------------------------------
(defun p1-get-decl/forms (body)
  (let ((decl-list ())
        first-form)
    (loop
      (when (p1-endp body)
        (return))
      
      ;; Ein Makroaufruf darf in eine Deklaration expandieren
      ;; und muss als solche dann erkannt werden.
      ;;-----------------------------------------
      (setq first-form (first body))
      (when (or (atom first-form)
                (not (eq (first first-form) 'L::DECLARE)))
        (return))                      ; Erste Form des Rumpfes gefunden
      (setq decl-list                   ; DECLARE-Form vorhanden
            (append (rest first-form) decl-list))
      (pop body))
    (values (cons 'L::DECLARE (reverse decl-list)) body)))

;;------------------------------------------------------------------------------
;; Resultat: Liste der als SPECIAL deklarierten Variablen
;;------------------------------------------------------------------------------
(defun p1-get-special-declared-vars (decl-specs)
  (cond
    ((null decl-specs) ())
    ((eq (caar decl-specs) 'L::SPECIAL)
     (append (cdar decl-specs)
             (p1-get-special-declared-vars (rest decl-specs))))
    (t (p1-get-special-declared-vars (rest decl-specs)))))

;;------------------------------------------------------------------------------
(provide "p1decls")
