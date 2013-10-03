;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Initialisierung des Compilers,
;;;            Initialisierung des Uebersetzungsumgebung.
;;;
;;; $Revision: 1.68 $
;;; $Log: p0init.lisp,v $
;;; Revision 1.68  1994/05/19  08:00:54  pm
;;; <file>-ffi.h wird nun nur noch bei Bedarf angelegt.
;;; Alle diese Relikte duerfen geloescht werden
;;;
;;; Revision 1.67  1994/04/22  14:13:31  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Erzeugte Include-Datei heisst jetzt <datei-name>-ffi.h
;;;
;;; Revision 1.66  1994/04/18  12:12:39  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Initialisierung des FFI
;;;
;;; Revision 1.65  1994/02/02  12:36:39  hk
;;; aref wird nicht mehr als compiler Macro definiert, das verursachte
;;; Probleme, wenn aref in Macroexpansionsfunktionen vorkam.
;;;
;;; Revision 1.64  1993/12/22  09:22:29  hk
;;; Für CMU17 müssen bei make-instance Symbole statt Klassen verwendet
;;; werden.
;;;
;;; Revision 1.63  1993/12/16  16:37:17  pm
;;; Kein Initialisieren des FFI mehr notwendig.
;;;
;;; Revision 1.62  1993/12/08  12:09:50  pm
;;; Meine *interface-file-queue* wird jetzt mit einer leeren Queue
;;; initialisiert.
;;;
;;; Revision 1.61  1993/12/06  16:31:32  hk
;;; Definition der Variablen *special-sys-funs* gestrichen, da unbenutzt.
;;; Angwandte Vorkommen von *init-system* gestrichen, da z.Z. wirkungslos.
;;; p0-init aufgeräumt.
;;;
;;; Revision 1.60  1993/11/03  11:43:01  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.59  1993/10/28  08:40:04  pm
;;; Mit den exports aus clcload abgeglichen.
;;;
;;; Revision 1.58  1993/09/28  14:49:55  pm
;;; Aenderungen fuer die C-Pointer eingebaut.
;;;
;;; Revision 1.57  1993/09/20  14:32:10  jh
;;; weight-mark-special-funs und weight-set-special-funs eingebaut.
;;;
;;; Revision 1.56  1993/08/25  14:50:46  jh
;;; p2 in opti umbenannt.
;;;
;;; Revision 1.55  1993/08/19  10:36:11  hk
;;; Auch Funktionen mit Namen (setf xxx) können special-sys-fun sein.
;;;
;;; Revision 1.54  1993/07/26  16:40:42  pm
;;; initialisierung hinzugefügt
;;;
;;; Revision 1.53  1993/07/07  15:31:47  uho
;;; Initialisierung von *C-NAME-PREFIX* aus der Codegenerierung nach
;;; p0-init verlegt.
;;;
;;; Revision 1.52  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.51  1993/05/31  17:04:17  pm
;;; Schreibfehler beseitigt
;;;
;;; Revision 1.50  1993/05/23  17:52:07  pm
;;; p0-init-foreign-types um die Eintraege fuer die
;;; primitiven Typen ergaenzt
;;;
;;; Revision 1.49  1993/05/21  13:57:32  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.48  1993/05/19  08:52:34  hk
;;; (setq *ffi-package* (find-package FFI))
;;;
;;; Revision 1.47  1993/05/06  14:51:15  uho
;;; Ausgabe des Zeilenumbruchs nach Syntax-Import entfernt.
;;;
;;; Revision 1.46  1993/05/06  08:33:55  pm
;;; (use-package FFI) rausgeworfen
;;;
;;; Revision 1.45  1993/05/03  14:44:29  hk
;;; Neues Macro def-global-op, das die Funktionen p0-init-special-form,
;;; p0-init-sys-macro und p0-init-compiler-macro definiert.
;;; p0-init-sys-fun gestrichen.
;;;
;;; Revision 1.44  1993/05/03  12:09:46  pm
;;; initialisieren der foreign-types
;;;
;;; Revision 1.43  1993/04/22  12:19:54  hk
;;; *init-system* ist vorlaeufig immer T. Einlesen von lisp.def und
;;; lisp.syntax, ausser wenn *lisp-module* = T.
;;; p0-init-sys-funs und read-sys-header-files gestrichen.
;;;
;;; Revision 1.42  1993/04/20  14:27:23  ft
;;; Die Definition der Typ-Klassen erfolgt jetzt in lisp.lisp.
;;;
;;; Revision 1.41  1993/04/19  12:22:14  kl
;;; Initialisierung der Systemfunktionen erweitert.
;;;
;;; Revision 1.40  1993/04/08  13:31:04  ft
;;; Definition der restl. Klassen von vordef. Typen.
;;;
;;; Revision 1.39  1993/04/08  07:50:45  hk
;;; Compiler Macros fuer aref und set-aref bekanntgegeben.
;;;
;;; Revision 1.38  1993/04/03  10:06:59  hk
;;; p1-typep und p1-make-instance als Compiler-Macro bekanntgegeben.
;;;
;;; Revision 1.37  1993/03/30  12:29:08  ft
;;; find-class als special-form deklariert.
;;;
;;; Revision 1.36  1993/03/25  15:40:03  hk
;;; find-class auf 'special-sys-fun, 'special-sys-fun-with-mv und
;;; 'imported-fun angewendet um Kompatibilitaet mit cl0 zu erreichen.
;;;
;;; Revision 1.35  1993/03/25  15:35:11  hk
;;; Macro ASSERT wird nun unterstuetzt.
;;;
;;; Revision 1.34  1993/03/10  10:06:36  ft
;;; Meldungen in p0-init von format auf clicc-message umgestellt.
;;;
;;; Revision 1.33  1993/02/23  08:39:39  ft
;;; Initialisierung der built-in-classes korrigiert.
;;;
;;; Revision 1.32  1993/02/17  14:38:47  ft
;;; Weitere built-in-classes hinzugefuegt.
;;;
;;; Revision 1.31  1993/02/17  11:15:13  kl
;;; Intern-prefixed verwendet.
;;;
;;; Revision 1.30  1993/02/16  17:10:21  hk
;;; Revision Keyword eingefuegt, Symbole des zu uebersetzenden Programms
;;; durch clicc-lisp:: gekennzeichnet, sys-fun.def wird in das clicc-lisp
;;; Package gelesen, init-macro-error-funs wird erst nach init-pass1 ausgefuehrt
;;;
;;; Revision 1.29  1993/01/14  14:31:59  hk
;;; p0-init-sys-fun ruft neues cg-set-C-name
;;;
;;; Revision 1.28  1993/01/14  12:48:31  hk
;;; *user-package* und *runtime-package* neu.
;;;
;;; Revision 1.27  1993/01/07  10:00:20  hk
;;; Fehler mit special-sys-fun behoben.
;;;
;;; Revision 1.26  1993/01/06  13:20:53  hk
;;; Funktionen nach p1macro und p1eval ausgelagert.
;;;
;;; Revision 1.25  1993/01/06  13:03:40  hk
;;; Funktionen {p1,p2,p3,cg}-special-funs vereinheitlicht.
;;;
;;; Revision 1.24  1993/01/06  11:17:41  ft
;;; Erweiterung um logische Operationen auf Zahlen.
;;;
;;; Revision 1.23  1992/12/22  16:46:42  hk
;;; gensym in init-zw-sym-fun-hash-table eingetragen.
;;;
;;; Revision 1.22  1992/12/16  10:21:12  ft
;;; define-modify-macro und define-setf-method in p0-init-toplevel-forms
;;; auskommentiert.
;;;
;;; Revision 1.21  1992/12/08  09:52:32  hk
;;; p0-init-toplevel-forms definiert, aber noch nicht benutzt,
;;; p0-init-sys-macros aufgeraeumt.
;;;
;;; Revision 1.20  1992/11/26  16:50:13  hk
;;; Neu: cg-%vector-length.
;;;
;;; Revision 1.19  1992/11/26  15:10:40  hk
;;; %cons -> cons, :super-list von T -> ().
;;;
;;; Revision 1.18  1992/11/26  14:34:25  ft
;;; Erweiterung von init-zw-sym-fun-hash-table.
;;;
;;; Revision 1.17  1992/11/25  17:51:27  hk
;;; Inline Compilation von %car, %cdr, %rplaca, %rplacd, %cons und
;;; einige Umbenennungen: check-integer-low -> fixnum-low-p ...
;;;
;;; Revision 1.16  1992/11/25  16:09:54  ft
;;; Erweiterung um init-build-in-classes.
;;;
;;; Revision 1.15  1992/11/23  14:33:50  ft
;;; Erweiterung der u.g. Hash-Table.
;;;
;;; Revision 1.14  1992/11/20  13:45:33  ft
;;; Initialisierungsfkt. fuer zw-symbol-fun's Hash-Table erstellt.
;;;
;;; Revision 1.13  1992/11/19  13:22:10  ft
;;; Eigene Funktion fuer Makro-Fehlerbeh.fkt.
;;;
;;; Revision 1.12  1992/11/19  12:11:18  ft
;;; Erweiterung von p0-init-macros um die Definition von
;;; Fehlerbehandlungsfunktionen fuer die Makroexpansion.
;;;
;;; Revision 1.11  1992/09/29  21:01:44  hk
;;; Kommentiert, (setq *TAB* 0) entfernt.
;;;
;;; Revision 1.10  1992/09/24  09:02:37  hk
;;; *sym-adr* mit 1 initialisiert, da NIL gesondert behandelt wird.
;;;
;;; Revision 1.9  1992/08/10  16:50:33  hk
;;; Funktion values wird dargestellt durch special-sys-fun-with-mv.
;;;
;;; Revision 1.8  1992/08/05  09:43:37  hk
;;; *SYS-FUN-DEF* angewendet.
;;;
;;; Revision 1.7  1992/07/29  14:10:32  hk
;;; Neue Variablen *keyword-package* und *lisp-package*.
;;;
;;; Revision 1.6  1992/07/28  12:28:51  hk
;;; Aufruf von cg-init in p0-init, simple-array-p --> rt::simple-array-p.
;;;
;;; Revision 1.5  1992/07/27  16:31:13  hk
;;; Umbenennungen, Vereinfachungen, p0-init neu.
;;;
;;; Revision 1.4  1992/07/08  14:52:20  hk
;;; p0-init-global-vars initialisiert, *c-name-prefix* hier gestrichen.
;;;
;;; Revision 1.3  1992/07/06  10:07:33  hk
;;; Importierte Fkt. aus Runtime-Package bekommen Defaultwert in Slot adr.
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
(defun p0-init ()

  (setq *LOCAL-ENVIRONMENT* (make-instance 'env))
  (setq *keyword-package* (find-package "KEYWORD"))
  (setq *lisp-package* (find-package "LISP"))
  (setq *user-package* (find-package "USER"))
  (setq *runtime-package* (find-package "RUNTIME"))
  (setq *ffi-package* (find-package "FFI"))
  (setq *interface-file-queue* (empty-queue))
  (setq *ffi-signatures* '())
  (setq *c-name-prefix* 0)

  (init-pass1)
  (setq *GLOBAL-ENVIRONMENT* (make-instance 'global-env))

  (clicc-message "Initialize Special Forms")
  (p0-init-special-form)

  (clicc-message "Initialize System Macros")
  (p0-init-sys-macro)
  (p0-init-defsetf)

  (clicc-message "Initialize System Functions")
  (opti-mark-special-funs)
  (p3-mark-special-funs)
  (sc-mark-special-funs)
  (ti-mark-special-funs)
  (weight-mark-special-funs)
  (cg-mark-special-funs)
  (p0-mark-special-sys-fun-with-mv)
  (import-read *SYS-DEF*)
  (unless *lisp-module*
    (clicc-message "Reading lisp.def")
    (import-read *LISP-DEF*))
  (opti-set-special-funs)
  (p3-set-special-funs)
  (sc-set-special-funs)
  (ti-set-special-funs)
  (weight-set-special-funs)
  (cg-set-special-funs)

  (p0-init-bq-funs)
  (p0-init-fftypes)

  (clicc-message "Initialize Evaluator")
  (init-zw-sym-fun-hash-table)

  (init-macro-error-funs)
  (p0-init-compiler-macro)
  (init-built-in-class-t)
     
  (unless *lisp-module*
    (clicc-message "Reading lisp.syntax")
    (syntax-import *LISP-DEF*)))

;;------------------------------------------------------------------------------
;; Initialisieren der SETF-Methoden
;;------------------------------------------------------------------------------
(defun p0-init-defsetf ()
  (set-setf-method-def 'L::apply
                       #'define-setf-method-apply :COMPLEX-SETF-METHOD)
  (set-setf-method-def 'L::getf
                       #'define-setf-method-getf :COMPLEX-SETF-METHOD))

;;------------------------------------------------------------------------------
;; Initialisieren der System-Funktionen in der Compiler Umgebung
;;------------------------------------------------------------------------------
(defmacro p0-special-funs ((slot prefix) &rest names)
  (labels ((prefix-name (name) 
             (intern (concatenate 'string prefix "-" (string name))))
           (prefix-set-name (name) 
             (intern
              (concatenate 'string prefix "-SET-" (string (second name))))))
    `(progn
      
      (defun ,(prefix-name "MARK-SPECIAL-FUNS") ()
        (dolist (name ',names)
          (when (consp name) (setq name (second name))) ; (setf name)
          (setf (get name 'zws-type)
                #-CMU17 (find-class 'special-sys-fun)
                #+CMU17 'special-sys-fun)))

      (defun ,(prefix-name "SET-SPECIAL-FUNS") ()
        ,@(mapcar #'(lambda (name)
                      (if (consp name)
                          `(let ((fun (cdr (get-global-setf-fun-def ',name))))
                            (when fun
                              (setf (,slot fun)
                                    #',(prefix-set-name name))))
                          `(let ((fun (get-global-fun ',name)))
                            (when fun
                              (setf (,slot fun)
                                    #',(prefix-name name))))))
                  names)))))

(defun p0-mark-special-sys-fun-with-mv ()
  (setf (get 'values 'zws-type)
        #-CMU17 (find-class 'special-sys-fun-with-mv)
        #+CMU17 'special-sys-fun-with-mv))

;;------------------------------------------------------------------------------
;; Ermittelt die Symbole der von Back-Quote benutzten Funktionen
;;------------------------------------------------------------------------------
(defun p0-init-bq-funs ()
  (let ((bq-append (first (read-from-string "`(,@x ,@y)")))
        (bq-cons   (first (read-from-string "`(1 ,@x)")))
        (bq-list   (first (read-from-string "`(,x)")))
        (bq-list*  (first (read-from-string "`(1 2 ,@x)"))))

    (unless (eq bq-append 'L::append)
      (set-sys-fun bq-append (get-global-fun 'L::append)))
    (unless (eq bq-cons 'L::cons)
      (set-sys-fun bq-cons   (get-global-fun 'L::cons)))
    (unless (eq bq-list 'L::list)
      (set-sys-fun bq-list   (get-global-fun 'L::list)))
    (unless (eq bq-list* 'L::list*)
      (set-sys-fun bq-list*  (get-global-fun 'L::list*)))))

;;------------------------------------------------------------------------------
;; Anlegen der Klassen fuer vordefinierte Typen in der Globalen Umgebung
;; (vgl. [SteeleII] S 783)
;;------------------------------------------------------------------------------
(defun init-built-in-class-t ()
  (set-class-entry 'L::T
                   (make-instance 'class-def
                                  :super-list '()
                                  :class-precedence-list '(L::T)
                                  :slot-descr-list '()
                                  :symbol 'L::T)
                   NIL NIL)) 

;;------------------------------------------------------------------------------
;; Vordefinieren einiger Typen.
;;------------------------------------------------------------------------------
(defun p0-init-fftypes ())
  
;;------------------------------------------------------------------------------
;; Macro zum Initialisieren der globalen Umgebung
;;------------------------------------------------------------------------------
(defmacro def-global-op (op &rest names)
  (let ((set-function (intern-prefixed "SET-" op)))
    `(defun ,(intern-prefixed "P0-INIT-" op) ()
      ,@(mapcar
         #'(lambda (name)
             `(,set-function ',name #',(intern-prefixed "P1-" name)))
         names))))
    
;;------------------------------------------------------------------------------
;; Initialisieren der Special Forms in der Compiler Umgebung
;;------------------------------------------------------------------------------
(def-global-op special-form
  L::BLOCK
  L::CATCH
  L::COMPILER-LET
  L::DECLARE
  L::EVAL-WHEN
  L::FIND-CLASS
  L::FLET
  L::FUNCTION
  L::GO
  L::IF
  L::LABELS
  L::LET
  L::LET*
  L::MACROLET
  L::MULTIPLE-VALUE-CALL
  L::MULTIPLE-VALUE-PROG1
  L::PROGN
  L::PROGV
  L::QUOTE
  L::RETURN-FROM
  L::SETQ
  L::TAGBODY
  L::THE
  L::THROW
  L::UNWIND-PROTECT)

;;------------------------------------------------------------------------------
;; Bekanntgeben der Toplevel-Forms
;;------------------------------------------------------------------------------
(def-global-op top-level
  L::defclass
  L::declaim
  L::defconstant
  L::defgeneric
  ;; L::define-modify-macro
  ;; L::define-setf-method
  L::defmacro
  L::defmethod
  L::defparameter
  L::defsetf
  L::defstruct
  L::deftype
  L::defun
  L::defvar
  L::in-package
  L::use-package
  L::provide
  L::require
  L::load
  ;; L::load-foreign
  L::export
  L::proclaim)

;;------------------------------------------------------------------------------
;; Initialisieren der System Makros in der Compiler Umgebung
;;------------------------------------------------------------------------------
(def-global-op sys-macro
  L::AND
  L::ASSERT
  L::CASE
  ;;  CCASE                  Nicht implementiert
  ;;  CHECK-TYPE             Nicht implementiert
  L::COND
  ;;  CTYPECASE              Nicht implementiert
  L::DECF
  L::DO
  L::DO*
  L::DO-ALL-SYMBOLS
  L::DO-EXTERNAL-SYMBOLS
  L::DO-SYMBOLS
  L::DOLIST
  L::DOTIMES
  L::ECASE
  L::ETYPECASE
  L::INCF
  L::LOCALLY
  L::LOOP
  L::MULTIPLE-VALUE-BIND
  L::MULTIPLE-VALUE-LIST
  L::MULTIPLE-VALUE-SETQ
  L::OR
  L::POP
  L::PROG
  L::PROG*
  L::PROG1
  L::PROG2
  L::PSETF
  L::PSETQ
  L::PUSH
  L::PUSHNEW
  L::REMF
  L::RETURN
  L::ROTATEF
  L::SETF
  L::SHIFTF
  ;;  STEP                   Nicht implementiert
  ;;  TIME                   Nicht implementiert
  ;;  TRACE                  Nicht implementiert
  L::TYPECASE
  L::UNLESS
  ;;  UNTRACE                Nicht implementiert
  L::WHEN
  L::WITH-INPUT-FROM-STRING
  L::WITH-OPEN-FILE
  L::WITH-OPEN-STREAM
  L::WITH-OUTPUT-TO-STRING)

;;------------------------------------------------------------------------------
;; System Compiler Macros bekanntgeben
;;------------------------------------------------------------------------------
(def-global-op compiler-macro
  L::typep
  L::make-instance)

;;------------------------------------------------------------------------------
(provide "p0init")
