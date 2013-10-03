;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Environments
;;;
;;; $Revision: 1.55 $
;;; $Log: p1env.lisp,v $
;;; Revision 1.55  1994/06/10  20:40:14  hk
;;; name2fun: liefert nil, wenn Funktion nicht gefunden.
;;;
;;; Revision 1.54  1994/05/26  10:10:06  hk
;;; dynamische Variablen in Ausdr"ucken, die zur Compile-Zeit evaluiert
;;; werden, werden nicht in global-env eingetragen, da dies f"ur Symbole
;;; auch nicht geschieht. Vorher konnte ein sym f"alschlich in den
;;; Zwischencode gelangen.
;;;
;;; Revision 1.53  1994/04/18  15:40:39  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - UNEXPANDED-FOREIGN-FUN als moeglicher Operator ins Globale
;;;   Environment eingetragen.
;;;
;;; Revision 1.52  1994/03/12  18:34:02  jh
;;; Macro get-global-setf-fun eingefuegt.
;;;
;;; Revision 1.51  1994/02/08  13:28:14  hk
;;; Neue Funktion name2fun: Das Argument ist ein 'extended function
;;; designator' also eine Symbol oder eine Liste der Gestalt (setf
;;; <symbol>).
;;;
;;; Revision 1.50  1993/12/07  15:04:12  ft
;;; find im Rumpf von get-symbol-bind durch dolist ersetzt.
;;;
;;; Revision 1.49  1993/11/03  11:42:39  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.48  1993/09/17  13:52:36  ft
;;; Explizite Angabe des Tests bei allen Aufrufen von assoc die nur eq
;;; benoetigen.
;;;
;;; Revision 1.47  1993/08/19  16:47:06  hk
;;; Neue Macros save-lex-var-name und lex-var-name-p.
;;;
;;; Revision 1.46  1993/06/17  20:45:16  hk
;;; Slot call-in-funs: Typ von () nach list.
;;;
;;; Revision 1.45  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.44  1993/05/31  17:04:56  pm
;;; Global-Environment um slot fuer call-in-Funktionen erweitert
;;;
;;; Revision 1.43  1993/05/10  07:03:29  hk
;;; Klammerfehler in op-as-fun-error behoben, durch (clicc clicc) gefunden.
;;;
;;; Revision 1.42  1993/04/20  14:30:00  ft
;;; Neue Verwaltung des Types-Slot des Global-Environment.
;;;
;;; Revision 1.41  1993/04/14  10:35:46  hk
;;; Neues Macro set-imported-setf-fun.
;;;
;;; Revision 1.40  1993/04/07  10:16:29  hk
;;; Neues Macro set-imported-fun (= set-sys-fun)
;;;
;;; Revision 1.39  1993/04/06  14:52:04  ft
;;; Kommentar fuer die Darstellung von Typ-Eintraegen hinzugefuegt.
;;;
;;; Revision 1.38  1993/04/03  09:41:41  hk
;;; Kommentar in global-env zu :COMPILER-MACRO.
;;;
;;; Revision 1.37  1993/04/03  09:40:09  hk
;;; inline-decls in local und global env gestrichen,
;;; get-global-fun an :COMPILER-MACRO angepasst.
;;;
;;; Revision 1.36  1993/04/01  11:01:26  hk
;;; :COMPILER-MACRO in redef-op-error beruecksichtigt.
;;;
;;; Revision 1.35  1993/03/30  12:01:22  hk
;;; Neues Macro set-compiler-macro.
;;;
;;; Revision 1.34  1993/03/26  14:59:34  hk
;;; Neue Funktion op-as-fun-error, redef-op-error umgeschrieben.
;;;
;;; Revision 1.33  1993/02/23  08:40:44  ft
;;; set-built-in-class-entry korrigiert.
;;;
;;; Revision 1.32  1993/02/16  17:00:50  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.31  1993/02/03  13:33:38  hk
;;; In get-var-bind ~a durch ~s ersetzt.
;;;
;;; Revision 1.30  1993/02/01  11:32:10  pm
;;; Neue Environment-Slots und Zugriffsfunktionen fuer die Verarbeitung
;;; von Foreign Types.
;;;
;;; Revision 1.29  1993/01/22  14:57:45  ft
;;; Neue Environment-Slots und Zugriffsfunktionen fuer die Verarbeitung
;;; von erweiterten Funktionsnamen.
;;;
;;; Revision 1.28  1993/01/08  15:33:58  hk
;;; Strings in redef-op-error ueberarbeitet.
;;;
;;; Revision 1.27  1992/12/10  14:28:05  ft
;;; Writer-Anwendung durch entspr. SETF-Methoden-Aufruf ersetzt.
;;;
;;; Revision 1.26  1992/12/03  17:07:39  hk
;;; Neu: set-top-level.
;;;
;;; Revision 1.25  1992/11/26  14:32:41  ft
;;; Log Message: siehe Revision 1.24.
;;;
;;; Revision 1.24  1992/11/26  13:16:36  ft
;;; get-type-def und get-class-entry korrigiert.
;;;
;;; Revision 1.23  1992/11/25  16:20:18  ft
;;; Aenderung in der Repraesentation von Typen im Global-Environment.
;;;
;;; Revision 1.22  1992/11/03  16:10:55  pm
;;; kleine Schoenheitkorrekturen
;;;
;;; Revision 1.21  1992/11/02  14:39:12  pm
;;; set-unexpanded-foreign-fun
;;;
;;; Revision 1.20  1992/10/20  09:21:50  ft
;;; copy-(global-)env vereinfacht.
;;;
;;; Revision 1.19  1992/10/19  13:10:05  ft
;;; Klammerungsfehler in redef-op-error beseitigt.
;;;
;;; Revision 1.18  1992/10/19  12:45:20  ft
;;; Zugriffsfunktionen auf global-env:?funs erweitert.
;;; Funktion redef-op-error eingefuehrt.
;;;
;;; Revision 1.17  1992/10/16  13:25:56  pm
;;; Kommentar in global-env eingefuegt
;;;
;;; Revision 1.16  1992/10/15  08:23:31  ft
;;; Fehler in set-global-class-entry beseitigt.
;;;
;;; Revision 1.15  1992/10/14  12:11:11  ft
;;; Aenderung der Unterbringung von Klassen im types-Slots von global-env.
;;;
;;; Revision 1.14  1992/10/09  10:49:59  ft
;;; saved-sym-consts von let in defvar geaendert.
;;;
;;; Revision 1.13  1992/10/08  08:50:03  ft
;;; Aenderungen am structures Slot rueckgaengig gemacht.
;;;
;;; Revision 1.12  1992/10/07  10:38:58  ft
;;; Typen, Slots und Strukturen im Slot types des global-env zusammengefasst.
;;;
;;; Revision 1.11  1992/10/02  14:19:19  hk
;;; *warn-unbound-vars* und special-symbol-name-p in get-var-bind.
;;;
;;; Revision 1.10  1992/09/22  10:30:00  uho
;;; Layout fuer save-sym-status, reset-sym-status geandert,
;;; damit sie durch ETAGS erkannt werden.
;;;
;;; Revision 1.9  1992/09/15  13:39:08  hk
;;; Kommentare ergaenzt, Slots in global-env umsortiert.
;;;
;;; Revision 1.8  1992/08/07  10:00:40  hk
;;; Neue Funktion bind-local-macro.
;;;
;;; Revision 1.7  1992/07/27  16:34:54  hk
;;; defstructs durch defclass1 ersetzt, Umbenennungen, copy-global-env neu.
;;;
;;; Revision 1.6  1992/07/09  15:51:29  hk
;;; Zunaechst vor jeder Uebersetzung *init-system* auf t.
;;;
;;; Revision 1.5  1992/07/07  11:16:49  hk
;;; Globale Variable *symbol-list* gestrichen, da nicht benutzt.
;;;
;;; Revision 1.4  1992/07/07  10:56:39  hk
;;; Schreibfehler.
;;;
;;; Revision 1.3  1992/07/07  09:43:42  hk
;;; global-env-symbols enthaelt keine A-Liste mehr, sondern nur noch Objekte
;;; vom Typ sym. Die Zuordnung von Symbol zu Sym erfolgt durch ?symbol in sym.
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
;; Die globale Umgebung:
;;------------------------------------------------------------------------------

(defvar *GLOBAL-ENVIRONMENT*)
(defvar *DEFAULT-GLOBAL-ENVIRONMENT*)

;;------------------------------------------------------------------------------
;; Dynamische Variable. Gibt an, wenn ein Lisp-Ausdruck nur deshalb in die
;; Zwischensprache transformiert wird, um spaeter interpretiert zu werden.
;; Wenn sie den Wert T hat, aendert sich das Verhalten von p1-make-new-symbol.
;;------------------------------------------------------------------------------
(defvar *COMPILER-EVAL* nil)

;;------------------------------------------------------------------------------
(defclass1 class-entry ()               ;Klassen-Eintrag im types-Slot
                                        ;der Klasse global-env
  class-def                             ;Instanz von class-def
  (forward :type bool)                  ;T=forwarded, NIL=defined
  (finalized :type bool)                ;T=finalized, NIL=not finalized
  (reader :type list)                   ;A-Liste: Slot-Name . Reader-Funs
  (writer :type list))                  ;A-Liste: Slot-Name . Writer-Funs

;;------------------------------------------------------------------------------
(defclass1 global-env ()
  
  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: Cons-Paar X
  ;; (:SPECIAL-FORM . Transformationsfunktion in die Zwischensprache)
  ;; :TOP-LEVEL noch nicht implementiert
  ;; (:SYS-MACRO . Transformationsfunktion in die Quellsprache)
  ;; (:USER-MACRO . defined-fun) Die Funktion wird zur Uebersetzungszeit
  ;;                             interpretiert und transformiert einen
  ;;                             Quellsprachenausdruck in die Quellsprache
  ;; (:IMPORTED-FUN . imported-fun)
  ;; (:GLOBAL-FUN . global-fun)
  ;; (:FORWARD . defined-named-const)
  ;; (:GENERIC-FUN . generic-fun)
  ;; (:FOREIGN-FUN . foreign-fun)
  ;; (:UNEXPANDED-FOREIGN-FUN . foreign-fun)
  ;; oder
  ;; (:COMPILER-MACRO . (expander . X))
  ;;-----------------------------
  (operators     :initform () :type list)

  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: dynamic
  ;; Alle definierenden und angewandten Vorkommen von dynamischen Variablen
  ;; mit gleichem Namen werden durch genau ein Zwischensprachkonstrukt
  ;; repraesentiert.
  ;;----------------
  (dyn-vars      :initform () :type list)

  ;; Liste mit Elementen vom typ sym.
  ;; Die Komponente symbol von sym dient als Schluessel
  ;;---------------------------------------------------
  (syms          :initform () :type list)

  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: Cons-Paar
  ;; (:TYPE              . type-expander)
  ;; (:CLASS             . Instanz von class-entry)
  ;;                       An dieser Stelle kann keine Instanz von defined-class
  ;;                       benutzt werden, da zusaetzlich Marker sowie die Namen
  ;;                       der Zugriffsfunktionen hier untergebracht sind.
  ;; (:BUILT-IN          . Instanz von built-in-class-def)
  ;; (:STRUCT            . Instanz von structure-class-def)
  ;;-----------------------------------
  (types         :initform () :type list)

  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: Name des foreign-type
  ;;----------
  (fftypes :initform () :type list)

  ;; A-Liste
  ;; Schlussel: Symbol
  ;; Wert: call-in-fun
  ;;------------------
  (call-in-funs :initform () :type list)

  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: ???
  ;;----------
  (structures    :initform () :type list)

  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: Cons-Paar
  ;; (:COMPLEX-SETF-METHOD .  Transformationsfunktion in die Quellsprache)
  ;; (:SIMPLE-SETF-METHOD . defined-fun)
  ;;                             Die Funktion wird zur Uebersetzungszeit
  ;;                             interpretiert und transformiert einen
  ;;                             Quellsprachenausdruck in die Quellsprache
  ;;----------------------------------------------------------------------
  (setf-methods  :initform () :type list)
  
  ;; A-Liste
  ;; Schluessel: Symbol
  ;; Wert: Cons-Paar
  ;; (:FORWARD . defined-named-const)
  ;; (:IMPORTED-FUN . imported-fun)
  ;; (:GLOBAL-FUN . global-fun)
  ;; (:GENERIC-FUN . generic-fun)
  ;;----------------------------------------------------------------------
  (setf-functions :initform () :type list)

  ;; Liste der Toplevel-Forms des Moduls in Zwischensprachdarstellung
  ;;-----------------------------------------------------------------
  (toplevel-forms :initform () :type list)

  (special-decls :initform () :type list)

  (ignore-decls :initform () :type list)

  ;; Kontextbedingung: Wenn eine globale SPECIAL-Deklaration existiert,
  ;; duerfen keine lokalen Bindungen gleichen Namens vorher existieren.
  ;; In 'lex-vars' werden daher die Namen saemtlicher lexikalischer Variablen
  ;; abgespeichert.
  ;;---------------
  (lex-vars :initform () :type list))

;;------------------------------------------------------------------------------
(defun copy-global-env (old)
  (make-instance 'global-env
    :operators (?operators old)
    :syms (?syms old)
    :special-decls (?special-decls old)
    :types (?types old)
    :fftypes (?fftypes old)
    :structures (?structures old)
    :setf-methods (?setf-methods old)
    :setf-functions (?setf-functions old)
    :toplevel-forms (?toplevel-forms old)
    :ignore-decls (?ignore-decls old)
    :lex-vars (?lex-vars old)
    :dyn-vars (?dyn-vars old)))

;;------------------------------------------------------------------------------
(defvar saved-sym-consts)
  
(defun save-sym-status ()
  (setq saved-sym-consts
        (mapcar #'?constant-value (?syms *GLOBAL-ENVIRONMENT*))))

(defun reset-sym-status ()
  (mapc #'(lambda (sym val)
            (setf (?used sym) nil)
            (setf (?constant-value sym) val))
        (?syms *GLOBAL-ENVIRONMENT*)
        saved-sym-consts))

;;------------------------------------------------------------------------------
(defmacro in-compile-time-env (&body forms)
  `(let ((*compiler-eval* t))
    ,@forms))  
  
;;------------------------------------------------------------------------------
;; Operators: Special Forms, globale Makros, globale Funktionen
;;------------------------------------------------------------------------------
(defmacro set-global-op (name type op)
  `(push (cons ,name (cons ,type ,op)) (?operators *GLOBAL-ENVIRONMENT*)))

(defmacro set-special-form (name expander)
  `(set-global-op ,name :SPECIAL-FORM ,expander))

(defmacro set-top-level (name expander)
  `(set-global-op ,name :TOP-LEVEL ,expander))

(defmacro set-sys-macro (name expander)
  `(set-global-op ,name :SYS-MACRO ,expander))

(defmacro set-macro (name expander)
  `(set-global-op ,name :USER-MACRO ,expander))

(defmacro set-sys-fun (name fun)
  `(set-global-op ,name :IMPORTED-FUN ,fun))

(defmacro set-imported-fun (name fun)
  `(set-global-op ,name :IMPORTED-FUN ,fun))

(defmacro set-global-fun (name fun)
  `(set-global-op ,name :GLOBAL-FUN ,fun))

(defmacro set-forward-fun (name fun)
  `(set-global-op ,name :FORWARD ,fun))

(defmacro set-generic-fun (name fun)
  `(set-global-op ,name :GENERIC-FUN ,fun))

(defmacro set-foreign-fun (name fun)
  `(set-global-op ,name :FOREIGN-FUN ,fun))

(defmacro set-unexpanded-foreign-fun (name fun)
  `(set-global-op ,name :UNEXPANDED-FOREIGN-FUN ,fun))

(defmacro set-compiler-macro (name fun)
  `(let ((entry (assoc ,name (?operators *GLOBAL-ENVIRONMENT*))))
    (if entry
        
        ;; Bestehenden Eintrag (name . (:typ . op)) zu
        ;; (name . (:COMPILER-MACRO . (op' . (:typ . op)))) erweitern.
        ;;------------------------------------------------------------
        (setf (cdr entry) (cons :COMPILER-MACRO (cons ,fun (cdr entry))))

        ;; Neuen Eintrag (name . (:COMPILER-MACRO . (op' . nil)))
        ;;-------------------------------------------------------
        (set-global-op ,name :COMPILER-MACRO (list ,fun)))))

(defmacro get-global-op (name)
  `(cdr (assoc ,name (?operators *GLOBAL-ENVIRONMENT*) :test #'eq)))

(defmacro get-global-fun (name)
  `(let ((op (get-global-op ,name)))
    (cdr (if (eq :COMPILER-MACRO (car op)) (cddr op)
             op))))

(defun redef-op-error (operator-key name)
  (clicc-error "It is illegal to redefine the ~a ~S"
               (ecase operator-key
                 (:SPECIAL-FORM "special form")
                 ((:SYS-MACRO :USER-MACRO) "macro")
                 ((:GLOBAL-FUN :LOCAL-FUN :IMPORTED-FUN :COMPILER-MACRO)
                  "function")
                 (:GENERIC-FUN "generic function")
                 (:FOREIGN-FUN "foreign function"))
               name))

(defun op-as-fun-error (operator-key name)
  (clicc-error "It is illegal to use the ~a ~s as a functional object"
               (ecase operator-key
                 (:SPECIAL-FORM "special form")
                 ((:SYS-MACRO :USER-MACRO :LOCAL-MACRO) "macro")
                 (:FOREIGN-FUN "foreign function"))
               name))

;;------------------------------------------------------------------------------
;; SETF-Functions
;;------------------------------------------------------------------------------
(defmacro set-global-setf-fun-def (name type function)
  `(push (cons (second ,name) (cons ,type ,function))
         (?setf-functions *GLOBAL-ENVIRONMENT*)))

(defmacro set-forward-setf-fun (name const)
  `(set-global-setf-fun-def ,name :FORWARD ,const))

(defmacro set-imported-setf-fun (name fun)
  `(set-global-setf-fun-def ,name :IMPORTED-FUN ,fun))

(defmacro get-global-setf-fun-def (name)
  `(cdr (assoc (second ,name) (?setf-functions *GLOBAL-ENVIRONMENT*)
         :test #'eq)))

(defmacro get-global-setf-fun (name)
  `(cdr (get-global-setf-fun-def ,name)))

;;------------------------------------------------------------------------------
;; Names of previous lexical variables
;;------------------------------------------------------------------------------
(defmacro save-lex-var-name (symbol)
  `(let ((symbol ,symbol))
    (when (symbol-package symbol)
      (pushnew symbol (?lex-vars *GLOBAL-ENVIRONMENT*)))))

(defmacro lex-var-name-p (symbol)
  `(member ,symbol (?lex-vars *GLOBAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
;; Global Symbols
;;------------------------------------------------------------------------------
(defmacro bind-symbol (sym)
  `(push ,sym (?syms *GLOBAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
(defmacro get-symbol-bind (symbol)
  `(dolist (sym (?syms *GLOBAL-ENVIRONMENT*))
     (when (eq ,symbol (?symbol sym)) (return sym))))

;;------------------------------------------------------------------------------
;; Dynamic Variables
;;------------------------------------------------------------------------------
(defun find-global-dynamic (name)
  (assoc name (?dyn-vars *GLOBAL-ENVIRONMENT*) :test #'eq))

(defun get-global-dynamic (name)
  (let ((bind (find-global-dynamic name)))
    (if bind
        (cdr bind)
        (let ((var (make-instance 'dynamic
                                  :sym (p1-make-symbol name))))
          (unless *compiler-eval*
            (push (cons name var)
                  (?dyn-vars *GLOBAL-ENVIRONMENT*)))
          var))))
                
;;------------------------------------------------------------------------------
;; Type Specifiers
;;------------------------------------------------------------------------------
(defmacro set-type-def (type type-expander)
  `(push (cons ,type (cons :TYPE ,type-expander))
    (?types *GLOBAL-ENVIRONMENT*)))

(defmacro get-type-def (type)
  `(cdr (assoc ,type (?types *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Classes
;;------------------------------------------------------------------------------
(defmacro set-global-class-entry (name class forward finalized reader writer)
  `(push (cons ,name (cons :CLASS (make-instance 'class-entry 
                                                  :class-def ,class
                                                  :forward ,forward
                                                  :finalized ,finalized
                                                  :reader ,reader
                                                  :writer ,writer)))
    (?types *GLOBAL-ENVIRONMENT*)))

(defmacro set-class-entry (name class reader writer)
  `(set-global-class-entry ,name ,class nil t ,reader ,writer))

(defmacro set-forward-class-entry (name class)
  `(set-global-class-entry ,name ,class t nil nil nil))

(defmacro set-unfinalized-class-entry (name class reader writer)
  `(set-global-class-entry ,name ,class nil nil ,reader ,writer))

(defun get-class-entry (name)
  (cdr (assoc name (?types *GLOBAL-ENVIRONMENT*) :test #'eq)))

(defmacro set-built-in (name class)
  `(push (cons ,name (cons :BUILT-IN ,class)) (?types *GLOBAL-ENVIRONMENT*)))

(defmacro get-built-in (name)
  `(cdr (assoc ,name (?types *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Foreign Types
;;------------------------------------------------------------------------------
(defmacro set-fftype (name fftype)
  `(push (cons ,name ,fftype) (?fftypes *GLOBAL-ENVIRONMENT*)))

(defmacro get-fftype (name)
  `(cdr (assoc ,name (?fftypes *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defmacro set-call-in-fun (name fun)
  `(push (cons ,name ,fun) (?call-in-funs *GLOBAL-ENVIRONMENT*)))

(defmacro get-call-in-fun (name)
  `(cdr (assoc ,name (?call-in-funs *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Generalized Variables
;;------------------------------------------------------------------------------
(defmacro set-setf-method-def (name function setf-method-type)
  `(push (cons ,name (cons ,setf-method-type ,function))
    (?setf-methods *GLOBAL-ENVIRONMENT*)))

(defmacro get-setf-method-def (name)
  `(cdr (assoc ,name (?setf-methods *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Structures
;;------------------------------------------------------------------------------
(defmacro set-struct-def (name struct)
  `(push (cons ,name ,struct) (?structures *GLOBAL-ENVIRONMENT*)))
 
(defmacro get-struct-def (name)
  `(cdr (assoc ,name (?structures *GLOBAL-ENVIRONMENT*) :test #'eq)))

;;------------------------------------------------------------------------------
;; Toplevel Forms
;;------------------------------------------------------------------------------
(defmacro save-toplevel-form (form)
  `(push ,form (?toplevel-forms *GLOBAL-ENVIRONMENT*)))

(defmacro get-toplevel-form-list ()
  `(reverse (?toplevel-forms *GLOBAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
;; Die lexikalische (oder lokale) Umgebung:
;;------------------------------------------------------------------------------
(defclass1 env ()
  (funs         :initform () :type list)
  (setf-funs    :initform () :type list)
  (vars         :initform () :type list)
  (blocks       :initform () :type list)
  (tags         :initform () :type list)
  (ignore-decls :initform () :type list))

;;------------------------------------------------------------------------------
(defun copy-env (old)
  (make-instance 'env
    :funs (?funs old)
    :setf-funs (?setf-funs old)
    :vars (?vars old)
    :blocks (?blocks old)
    :tags (?tags old)
    :ignore-decls (?ignore-decls old)))

(defvar *LOCAL-ENVIRONMENT*)

;;------------------------------------------------------------------------------
(defun get-operator-def (name)
  (let ((operator-def (cdr (assoc name (?funs *LOCAL-ENVIRONMENT*) :test #'eq))))
    (if operator-def
      operator-def
      (get-global-op name))))

(defun get-setf-fun-def (name)
  (let ((setf-fun-def (cdr (assoc (second name) 
                                  (?setf-funs *LOCAL-ENVIRONMENT*)
                                  :test #'eq))))
    (if setf-fun-def
        setf-fun-def
      (get-global-setf-fun-def name))))

;;------------------------------------------------------------------------------
;; Find the function which is denoted by a extended function designator
;;------------------------------------------------------------------------------
(defun name2fun (name)
  (let* ((operator-def (if (consp name)
                           (get-setf-fun-def name)
                           (get-operator-def name))))

    (when (eq :COMPILER-MACRO (car operator-def))
      (setq operator-def (cddr operator-def)))
    (case (car operator-def)
      ((:IMPORTED-FUN :GLOBAL-FUN :LOCAL-FUN) (cdr operator-def))
      (t nil))))

;;------------------------------------------------------------------------------
;; Funktionsbindungen fuer LABELS/FLET
;;------------------------------------------------------------------------------
(defun bind-local-fun (name fun)
  (if (consp name)
      (push (cons name (cons :LOCAL-SETF-FUN fun)) 
            (?setf-funs *LOCAL-ENVIRONMENT*))
    (push (cons name (cons :LOCAL-FUN fun)) (?funs *LOCAL-ENVIRONMENT*))))

;;------------------------------------------------------------------------------
;; Funktionsbindungen fuer MACROLET
;;------------------------------------------------------------------------------
(defun bind-local-macro (name fun)
  (push (cons name (cons :LOCAL-MACRO fun)) (?funs *LOCAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
;; Variablen-Bindungen
;;------------------------------------------------------------------------------
(defmacro bind-var (name var)
  `(push (cons ,name ,var) (?vars *LOCAL-ENVIRONMENT*)))

;;------------------------------------------------------------------------------
(defun get-var-bind (name)
  (labels ((special-symbol-name-p (s)
             (let ((name (symbol-name s)))
               (and (eql (char name 0) #\*)
                    (eql (char name (1- (length name))) #\*)))))
    (let ((var-bind (assoc name (?vars *LOCAL-ENVIRONMENT*) :test #'eq)))
      (cond
        (var-bind (cdr var-bind))
        (t (unless (p1-proclaimed-special-p name)
             (when (or *warn-unbound-vars* (not (special-symbol-name-p name)))
               (clicc-warning "Assuming ~20s is global variable." name)))

           (get-global-dynamic name))))))

;;------------------------------------------------------------------------------
;; BLOCK-Bindungen fuer BLOCK/RETURN-FROM
;;------------------------------------------------------------------------------
(defmacro bind-block (name cont)
  `(push (cons ,name ,cont) (?blocks *LOCAL-ENVIRONMENT*)))

(defun get-block-bind (name)
  (let ((block-bind (assoc name (?blocks *LOCAL-ENVIRONMENT*) :test #'eq)))
    (if block-bind
      (cdr block-bind)
      (clicc-error "No BLOCK named ~S" name))))

;;------------------------------------------------------------------------------
;; TAG-Bindungen fuer TAGBODY/GO
;;------------------------------------------------------------------------------
(defmacro bind-tag (tag tagged-form)
  `(push (cons ,tag ,tagged-form) (?tags *LOCAL-ENVIRONMENT*)))

(defun get-tag-bind (tag)
  (let ((tag-bind (assoc tag (?tags *LOCAL-ENVIRONMENT*) :test #'eql)))
    (if tag-bind
      (cdr tag-bind)
      (clicc-error "There is no TAG for (go ~S)" tag))))

;;------------------------------------------------------------------------------
(provide "p1env")
