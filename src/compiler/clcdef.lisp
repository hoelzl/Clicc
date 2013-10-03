;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Definitionen fuer Strukturen, Makros, Konstanten, Variablen  
;;;
;;; $Revision: 1.48 $
;;; $Log: clcdef.lisp,v $
;;; Revision 1.48  1994/06/07  16:09:16  hk
;;; Erneut: Für CMU17 müssen bei make-instance Symbole statt Klassen
;;; verwendet werden.
;;;
;;; Revision 1.47  1994/06/03  14:18:32  hk
;;; Versionsnummer auf "0.6.4" erh"oht
;;;
;;; Revision 1.46  1994/05/05  15:17:03  hk
;;; Versionsnummer auf 0.6.3 erh"oht
;;;
;;; Revision 1.45  1994/05/05  15:16:18  hk
;;; Raw-Slots deaktiviert, damit sich CLiCC wieder selbst "ubersetzten
;;; kann.
;;;
;;; Revision 1.44  1994/02/02  09:12:27  hk
;;; defzws geändert. Nun können Slots mit der Option :raw {t | nil}
;;; versehen werden. Raw Slots belegen nur dann Speicherplatz, wenn ihnen
;;; ein Wert ungleich der initform zugewiesen wird. Als initform dürfen
;;; nur Symbole und Zahlen angegeben werden. Der Defaultwert der initform
;;; ist nil (nicht unbound). Instanzen mit Raw Slots haben einen
;;; zusätzlichen Slot, der in einer A-Liste die Werte der Raw Slots
;;; enthält. Man spart also nur dann Speicherplatz, wenn mindestens 2 Raw
;;; Slots vorhanden sind.
;;; (defvar *CURRENT-FORM*) eingefügt.
;;;
;;; Revision 1.43  1993/12/22  09:22:50  hk
;;; Für CMU17 müssen bei make-instance Symbole statt Klassen verwendet
;;; werden.
;;;
;;; Revision 1.42  1993/12/06  16:44:52  hk
;;; Definition und Initialisierung einiger globaler Variablen nach hier
;;; verlagert. Englische Kommentare für Variablen, die der Benutzer
;;; verändern darf.
;;;
;;; Revision 1.41  1993/11/05  14:20:50  hk
;;; require an den Dateianfang verschoben, um Probleme mit akcl zu vermeiden.
;;;
;;; Revision 1.40  1993/10/25  11:35:20  hk
;;; (defconstant *CLICC-Version* "0.6.2")
;;;
;;; Revision 1.39  1993/09/09  06:14:47  ft
;;; *METHOD-CACHE-LIMIT* ist ab jetzt eine Variable.
;;;
;;; Revision 1.38  1993/08/20  11:47:59  ft
;;; Neue Programmkonstante *METHOD-CACHE-LIMIT* eingefuehrt, die angibt
;;; wieviele effektive Methoden eine generische Funktion besitzen darf,
;;; ohne dass ein Methoden-Cache angelegt wird.
;;;
;;; Revision 1.37  1993/07/23  08:26:29  hk
;;; (defconstant *CLICC-Version* 0.6.1)
;;;
;;; Revision 1.36  1993/07/11  13:34:17  kl
;;; Neue Variable *ITERATIONS* eingefuehrt.
;;;
;;; Revision 1.35  1993/06/19  21:13:21  hk
;;; *clicc-version* auf 0.6
;;;
;;; Revision 1.34  1993/06/17  10:01:58  hk
;;; Die Make Funktion, die von defzws generiert wird, ruft nun make-instance
;;; mit einer Klasse als erstem Argument auf.
;;;
;;; Revision 1.33  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.32  1993/06/04  13:12:40  hk
;;; Neue Variable *C-max-line-count*.
;;;
;;; Revision 1.31  1993/05/19  15:10:55  hk
;;; *zw-sym-fun-hash-table* :
;;; make-hash-table erst in init-zw-sym-fun-hash-table.
;;;
;;; Revision 1.30  1993/05/19  10:24:12  hk
;;; *CLICC-INPUT* gestrichen.
;;;
;;; Revision 1.29  1993/05/19  08:52:24  hk
;;; (defvar *ffi-package*)
;;;
;;; Revision 1.28  1993/04/22  12:45:04  hk
;;; Unbenutzte Variablen Definitionen gestrichen.
;;;
;;; Revision 1.27  1993/04/18  16:57:42  atr
;;; Seiteneffektanalyse angeschaltet.
;;;
;;; Revision 1.26  1993/04/16  08:09:39  hk
;;; Neue Variablen *LISP-MODULE*, *INLINE-MODULE*.
;;;
;;; Revision 1.25  1993/04/02  11:53:31  uho
;;; *MODULE-COMPILER* eingefuehrt um den Modul-Compiler aktivieren
;;; zu koennen.
;;;
;;; Revision 1.24  1993/03/18  15:47:15  hk
;;; *NO-SIDE-EFFECT-ANALYSIS* mit T initialisiert.
;;;
;;; Revision 1.23  1993/03/18  15:19:46  ft
;;; Tippfehler.
;;;
;;; Revision 1.22  1993/03/18  15:18:23  ft
;;; Neuen Schalter *NO-SIDE-EFFECT-ANALYSIS* eingebaut.
;;;
;;; Revision 1.21  1993/03/18  14:48:52  uho
;;; Globale Variable *SYNTAX-EXPORT* eingefuehrt, die die Liste
;;; der Konstrukte enthaelt, die syntaktische exportiert werden.
;;; In defzws werden Accessor-Funktionen als inline deklariert.
;;; In defzws werden Konstruktorfunktionen fuer die jeweilige Klasse in der Form
;;; make-<name> angelegt.
;;;
;;; Revision 1.20  1993/02/16  15:48:04  hk
;;; Revision Keyword eingefuegt, *clicc-package* entfernt.
;;;
;;; Revision 1.19  1993/01/21  09:43:50  uho
;;; *FLAT-IFS* eingefuehrt, Option zum Generieren flacher IFs.
;;;
;;; Revision 1.18  1993/01/20  15:17:49  kl
;;; Ladereihenfolge umgestellt. clcmisc wird jetzt schon hier geladen.
;;;
;;; Revision 1.17  1993/01/14  12:48:10  hk
;;; *user-package* und *runtime-package* neu.
;;;
;;; Revision 1.16  1993/01/02  12:57:06  kl
;;; *NO-CODEGEN* zum Abschalten der Codegenerierung eingebaut.
;;; Deklaration der Variablen *SPLIT* aus clcload hierhin verlegt.
;;;
;;; Revision 1.15  1992/11/20  13:52:52  ft
;;; Variable *zw-sym-fun-hash-table* eingefuehrt.
;;;
;;; Revision 1.14  1992/10/15  11:47:06  uho
;;; *COMPILE-MODULE* eingefuegt, um neues Modulsystem zu implementieren.
;;;
;;; Revision 1.13  1992/10/07  17:37:22  hk
;;; Neue Variable *OPTIMIZE*.
;;;
;;; Revision 1.12  1992/10/05  16:18:22  uho
;;; Kommentar zu *MEMSIZES* korrigiert.
;;;
;;; Revision 1.11  1992/10/05  11:11:41  uho
;;; Initialisierung von *MEMSIZES* entfernt.
;;;
;;; Revision 1.10  1992/10/05  10:48:26  uho
;;; *MEMSIZES* eingefuegt, um Speicherbreichsverwaltung parametrisierbar
;;; zu machen.
;;;
;;; Revision 1.9  1992/09/29  20:38:41  hk
;;; (defvar *TAB*) entfernt, nicht mehr benutzt.
;;;
;;; Revision 1.8  1992/09/25  16:44:16  kl
;;; Umstellung auf die neuen Dateien zsdef.lisp und zsops.lisp.
;;;
;;; Revision 1.7  1992/08/14  08:45:16  kl
;;; Konstante *CLICC-Version* eingefuehrt.
;;;
;;; Revision 1.6  1992/08/05  09:43:02  hk
;;; *SYS-FUN-DEF* definiert.
;;;
;;; Revision 1.5  1992/07/29  14:09:07  hk
;;; Neu Variablen *keyword-package* und *lisp-package*.
;;;
;;; Revision 1.4  1992/07/22  15:41:04  hk
;;; Macro defclass1, um Structuren wie mit defzws definieren zu koennen.
;;;
;;; Revision 1.3  1992/07/08  15:36:03  hk
;;; Neue Variable *sym-adr*, gestrichen: *c-name-prefix*.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;; Changes  :
;;; 03.02.92 : Neue Komponente 'constants' in der Struktur fun.
;;; 04.02.92 : Neue Struktur STRUCTURED-CONST.
;;; 10.02.92 : Neue Komponente 'module' in der struktur SYM
;;; 12.02.92 : Neue Komponenten 'ref-fix-array' und 'ref-float-array'
;;;            in der Struktur STRUCTURED-CONST, damit waehrend der
;;;            Codegenerierung die entsprechenden Deklarationen erzeugt werden
;;;            koennen.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Global constants
;;------------------------------------------------------------------------------
(defconstant *CLICC-Version* "0.6.4")   ; version number of the compiler

;;------------------------------------------------------------------------------
;; Global variables. The default values may be adjusted by the user.
;;------------------------------------------------------------------------------
(defparameter *ANSI-C* T)               ; generate ANSI C code
(defparameter *warn-unbound-vars* nil)  ; give warning if variable with name
                                        ; *...* is not bound and has no special
                                        ; declaration
(defparameter *SPLIT-FILES* NIL)        ; a separate file for each global
                                        ; function
(defparameter *C-max-line-count* 50000) ; split file into file1, file2, ..
                                        ; if more lines than indicated
(defparameter *FLAT-IFS* nil)           ; Should if forms be expressed by
                                        ; goto statements or by if statements
(defparameter *ONLY-PASS1* NIL)         ; Do only macroexpansion and
                                        ; transformation into the intermediate
                                        ; language. This may be used to check
                                        ; the restrictions od CL_0.
(defparameter *NO-CODEGEN* NIL)         ; No code generation
(defparameter *NO-SIDE-EFFECT-ANALYSIS* NIL) ; No side effect analysis
(defparameter *OPTIMIZE*   T)           ; Do optimizations
(defparameter *ITERATIONS* 1)           ; Number of analyzation / optimization
                                        ; cycles
(defparameter *METHOD-CACHE-LIMIT* 4)   ; use a method cache if a generic
                                        ; function has more methods than this
                                        ; value
(DEFVAR *MEMSIZES*    
  '((2000 . 5000) (#x10000 . #x60000)  (#x600 . #x6000) (#x200 . #x2000))) 
;;          stack           form-heap           fx-heap          fl-heap
                                        ; a list of pairs (min . max)
                                        ; the sizes of the different memory
                                        ; areas: lisp-stack, form-heap,
                                        ; fixnum/character-heap and float-heap
(defvar *SHOW-VERSION* NIL)             ; show version and copyright message
(defvar *CLICC-PRINT* NIL)              ; show messages

;;------------------------------------------------------------------------------
;; global variables
;;------------------------------------------------------------------------------
(defvar *FILENAME*)                     ; Name der Datei, die uebersetzt
                                        ; werden soll
(defvar *EXTENSION*)                    ; Extension der Hauptdatei, falls
                                        ; angegeben, "" sonst.
(defvar *OUT-FILENAME*)                 ; Der Name (ohne Extension) der Datei,
                                        ; in der der Code generiert wird.
(defvar *MODULE-COMPILER*)              ; Soll ein Modul uebersetzt werden?
                                        ; T: Modul wird uebersetzt und
                                        ;    Headerfiles geschrieben
                                        ; NIL: Komplettkompilation
(defvar *LISP-MODULE*)                  ; Das Lisp Modul soll uebersetzt werden
(defvar *INLINE-MODULE*)                ; Code fuer Lisp Funktionen generieren
                                        ; ueber deren Code der Compiler
                                        ; spezielles Wissen hat.
(defvar *module*)                       ; das gerade bearbeitete Modul
(defvar *NERRORS*)                      ; Zaehler fuer Fehler
(defvar *NWARNINGS*)                    ; Zaehler fuer Warnungen
(defvar *keyword-package*)
(defvar *lisp-package*)
(defvar *user-package*)
(defvar *runtime-package*)
(defvar *ffi-package*)
(defvar *zw-sym-fun-hash-table*)        ; Fuer unsere eigenes SYMBOL-FUNCTION
                                        ; im compiletime interpreter.
(defvar *CLICC-REQUIRED*)               ; Liste der Modulnamen fuer REQUIRE
(defvar *SYNTAX-EXPORT*)                ; Liste, in die fuer den Syntaxteil
                                        ; exportierten Konstrukte aufgesammelt
                                        ; werden.
                                        ; - Macros
                                        ; - Makrofunktionen
                                        ; - DEFSETFs
                                        ; - DEFTYPEs
                                        ; - specials
(defvar *CLICC-PATH-STRING*)            ; Ein String, der einen Pfadnamen
                                        ; enthaelt; die Wurzel des CLICC-
                                        ; Dateibaumes.

;;------------------------------------------------------------------------------
;; special declarations
;;------------------------------------------------------------------------------
(defvar *FUN-NAME*)                     ; Name der momentan uebersetzten
                                        ; Funktion bzw. "toplevel form"
(defvar *CURRENT-FORM*)                 ; Aktuelle Form fuer Fehlermeldungen
(defvar *CURRENT-FUN*)                  ; Momentan bearbeitete Funktion 
(defvar *SDF* NIL)                      ; Handelt es sich um eine vom System
                                        ; generierte Fkt. ?
(defvar *CLICC-FILENAME*)               ; Name der momentan uebersetzten Datei

;;------------------------------------------------------------------------------
;; Typ Bool
;;------------------------------------------------------------------------------
(deftype bool ()
  '(or null (member t)))

;;------------------------------------------------------------------------------
;; Hilfsfunktionen fuer defzws
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Argument: Property-Liste bestehend aus Keyword- / Ausdruck-Paaren
;;           k1 e1 k2 e2 ... kn en
;; Resultat: Values
;;           1. Property-Liste von Keyword- / Variablen-Paaren
;;              k1 v1 k2 v2 ... kn vn
;;           2. A-Liste von Variablen- / Ausdruck-Listen
;;              (v1 e1) (v2 e2) ... (vn en)
;;------------------------------------------------------------------------------
(defun args2kv/ve-list (args)
  (let ((kv (empty-queue))
        (ve (empty-queue)))
    (loop
     (when (endp args) (return))
     (let ((v (gensym)))
       (add-q (pop args) kv) (add-q v kv)
       (add-q (list v (pop args)) ve)))
    (values (queue2list kv) (queue2list ve))))

;;------------------------------------------------------------------------------
;; Argumente: 1. Property-Liste von Keyword- / Variablen-Paaren
;;               k1 v1 k2 v2 ... kn vn
;;            2. A-Liste von Keyword- / Wert-Paaren
;;               (l1 . i1) (l2 . i2) ... (lm . im)
;; Resultate: Values
;;            1. A-Liste von Keyword- / Variablen- / Wert-Listen
;;               der Keywords aus dem 1. Argument, die in dem 2. Argument
;;               vorhanden sind
;;            2. Teilliste des 1. Arguments, mit den Keywords,
;;               die nicht in dem 2. Argument gefunden wurden.
;;------------------------------------------------------------------------------
(defun split-raw-add-init (kv-list raw-ki-list)
  (let ((raw-kvi (empty-queue))
        (not-raw-kv (empty-queue)))
    (loop
     (when (endp kv-list) (return))
     (let* ((k (pop kv-list))
            (v (pop kv-list))
            (ki (assoc k raw-ki-list)))
       (if ki
           (add-q (list k v (cdr ki)) raw-kvi)
           (progn (add-q k not-raw-kv) (add-q v not-raw-kv)))))
    (values (queue2list raw-kvi) (queue2list not-raw-kv))))

;;------------------------------------------------------------------------------
;; Generiert einen Aufruf der Gestalt
;; (make-instance '<class> :raw `((:k <value> ...)) :k <value> ...)
;;------------------------------------------------------------------------------
(defun gen-raw-constructor-call (class args raw-ki-list)
  `(make-instance #-CMU17 (find-class ',class) #+CMU17 ',class ,@args)
#|
  (let ((raw-v (gensym)))
    (multiple-value-bind (kv ve)
        (args2kv/ve-list args)
      (when raw-ki-list (push (cons raw-v ()) ve))
      (multiple-value-bind (raw-kvi not-raw-kv)
          (split-raw-add-init kv raw-ki-list)
        (when raw-ki-list (push raw-v not-raw-kv) (push :raw not-raw-kv))
        `(let (,@ve)
          ,@(mapcar #'(lambda (kvi)
                        `(unless (eql ,@(cdr kvi))
                          (push (cons ,(first kvi) ,(second kvi)) ,raw-v)))
                    raw-kvi)
          (make-instance #-CMU17 (find-class ',class) #+CMU17 ',class
           ,@not-raw-kv)))))
|#)

;;------------------------------------------------------------------------------
;; defzws stuetzt sich nicht auf defstruct sondern auf defclass, weil
;; - in defclass fuer Slots eine Typspezifikation angegeben werden kann,
;;   ohne dass eine Initform angegeben werden muss,
;; - die Accessor-Funktionen generisch sind, und somit fuer verschiedene
;;   Klassen gleiche Namen haben koennen.
;;------------------------------------------------------------------------------
(defmacro defzws (name supers &rest slots)
  (let ((accessors (empty-queue))
        (real-slots (empty-queue))
        (raw-key-init-pairs (empty-queue))
        (raw-slot-accessors (empty-queue))
        raw-key-init-name)
    (dolist (slot-desc slots)
      (when (atom slot-desc) (setq slot-desc (list slot-desc)))
      (let* ((sym (car slot-desc))
             (acc-sym (intern-prefixed "?" sym))
             (initarg (intern (symbol-name sym) "KEYWORD")))
        (add-q acc-sym accessors)
        (cond
#|          ((getf (cdr slot-desc) :raw nil)
           (let ((init (getf (cdr slot-desc) :initform nil)))
             (unless (or (null init) (eq t init) (keywordp init) (numberp init)
                         (and (consp init) (eq 'quote (car init))))
               (internal-error 'defzws
                               "~a is not allowed as an initial value" init))
             (add-q (cons initarg init) raw-key-init-pairs)
             (add-q `(defmethod ,acc-sym ((i ,name))
                      (let ((a (assoc ,initarg (?raw i))))
                        (if a
                            (cdr a)
                            ,init)))
                    raw-slot-accessors)
             (add-q `(defmethod (setf ,acc-sym) (v (i ,name))
                      (let ((a (assoc ,initarg (?raw i))))
                        (if a
                            (setf (cdr a) v)
                            (unless (eql v ,init)
                              (push (cons ,initarg v) (?raw i))))
                        v))
                    raw-slot-accessors)))
|#          (T
           (remf (cdr slot-desc) :raw)
           (add-q (list* (car slot-desc)
                         :accessor acc-sym
                         :initarg initarg
                         (cdr slot-desc))
                  real-slots)))))

    (setq raw-key-init-pairs (queue2list raw-key-init-pairs))
#|
    (dolist (class supers)
      (let ((raw-key-init-name (intern-postfixed class "-RAW-KEY-INIT")))
        (when (boundp raw-key-init-name)
          (setq raw-key-init-pairs
                (append (symbol-value raw-key-init-name) raw-key-init-pairs)))))
|#

    (when raw-key-init-pairs
      (add-q `(raw :accessor ?raw :initarg :raw :initform nil) real-slots))
    
    `(progn
      (defclass ,name ,supers (,@(queue2list real-slots)))
      ,(when raw-key-init-pairs
              (setq raw-key-init-name (intern-postfixed name "-RAW-KEY-INIT"))
              `(defconstant ,raw-key-init-name
                ',raw-key-init-pairs))
      (proclaim '(inline ,@(queue2list accessors)))
      ,@(queue2list raw-slot-accessors)
      (defmacro ,(intern-prefixed "MAKE-" name) (&rest options)
        (gen-raw-constructor-call ',name options ,raw-key-init-name))
      (defun ,(intern-postfixed name "-P") (x)
        (typep x ',name)))))

;;------------------------------------------------------------------------------
(defmacro defclass1 (&rest args) `(defzws ,@args))

;;------------------------------------------------------------------------------
(provide "clcdef")
