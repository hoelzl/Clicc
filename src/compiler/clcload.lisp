;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Startmodul zum Laden des Compilers in ein Common-LISP System
;;;
;;; $Revision: 1.95 $
;;; $Log: clcload.lisp,v $
;;; Revision 1.95  1994/05/22  15:05:45  sma
;;; Neuer Key "obrep" mit dem die Datenrepräsenation eingestellt werden
;;; kann.
;;;
;;; Revision 1.94  1994/05/19  07:57:22  pm
;;; Fehler behoben
;;;
;;; Revision 1.93  1994/04/22  14:09:37  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Ueberfluessiges exportiertes Symbol entfernt
;;;
;;; Revision 1.92  1994/04/18  12:05:29  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Liste der exportierten Symbole ergaenzt
;;;
;;; Revision 1.91  1994/02/02  09:36:39  hk
;;; printzs wird nicht geladen, da es sich (noch) nicht mit raw Slots
;;; verträgt.
;;;
;;; Revision 1.90  1994/01/05  13:23:58  hk
;;; Vorkommen von *init-system* entfernt.
;;;
;;; Revision 1.89  1993/12/22  11:10:37  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.88  1993/12/22  10:56:06  hk
;;; Kein (require "printzs") bei CMU17, da bug in
;;; pcl:class-direct-superclasses
;;;
;;; Revision 1.87  1993/12/16  15:21:53  hk
;;; Rücksetzen des gensym-counter von clcload nach clcmain, damit es auch
;;; bei dem standalone CLiCC wirkt und man möglichst gleiche Symbole
;;; erhält.
;;;
;;; Revision 1.86  1993/12/15  15:38:00  hk
;;; Variablen *SPLIT-FILES* und *ONLY-PASS1* werden von ask-runtime
;;; gebunden, so da"s keine Auswirkungen auf sp"atere Aufrufe von clicc
;;; erfolgen. Neue Funktion check-split pr"uft, ob Doppeldefinitionen
;;; durch *SPLIT-FILES* auftreten k"onnen.
;;;
;;; Revision 1.85  1993/12/15  12:55:06  hk
;;; Änderungen von pm in r1.83 rückgängig gemacht, werden in der nächsten
;;; Version wieder hinzugefügt.
;;;
;;; Revision 1.84  1993/12/14  14:05:21  hk
;;; setf und file-position werden nicht mehr von lisp nach clicc-lisp
;;; importiert
;;;
;;; Revision 1.83  1993/12/14  13:57:19  pm
;;; exports der ffi-symbole aus dem rt-package gestrichen.
;;;
;;; Revision 1.82  1993/12/06  16:37:21  hk
;;; Initialisierung der Variablen *C-max-line-count* und *SPLIT-FILES*
;;; nach clcdef. Änderung der Paramterliste der Funktion clicc, so daß die
;;; Parameter an globale Variablen zugewiesen werden und nicht mehr
;;; gebunden werden. Dadurch wird das manuelle Aufrufen einzelner
;;; Compilerphasen erleichtert. Die Werte gewisser Optionen werden dadurch
;;; beim nächsten Aufruf als Defaultwert genommen.
;;;
;;; Revision 1.81  1993/11/29  10:03:32  uho
;;; Initialisierung der Pfadnamen wird nun in der Funktion
;;; 'setup-clcload-pathes' vorgenommen, diese auf top-level aufgerufen.
;;;
;;; Revision 1.80  1993/11/09  12:42:10  hk
;;; Es wird nun (gensym 0) und (setq *gensym-counter* 1) ausgeführt.
;;;
;;; Revision 1.79  1993/11/08  11:26:47  hk
;;; Fehlendes (require "config") eingefügt.
;;;
;;; Revision 1.78  1993/11/08  11:16:05  hk
;;; Initialisierung von *CLICC-PATH-STRING* gestrichen, geschieht in
;;; config.lisp. use-list des Packages CLICC wird nur noch anhand der
;;; Features PCL und CLOS bestimmt.
;;;
;;; Revision 1.77  1993/11/03  11:45:06  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.76  1993/10/26  11:33:02  pm
;;; Ableich der FFI-Exports mit denen aus clicc.lisp
;;;
;;; Revision 1.75  1993/09/28  14:49:38  pm
;;; Aenderungen fuer die C-Pointer eingebaut.
;;;
;;; Revision 1.74  1993/09/20  12:40:52  pm
;;; Einmal mal was getan, und schon ein Fehler.
;;;
;;; Revision 1.73  1993/09/19  15:10:28  pm
;;; kleine Aenderungen
;;;
;;; Revision 1.72  1993/09/09  10:05:27  uho
;;; Ein paar 'pathname-name' und 'pathname-type' Aufrufe eingestreut um
;;; mit den Filenamen unter DOS besser zurecht zu kommen.
;;;
;;; Revision 1.71  1993/08/31  09:30:57  uho
;;; Aenderungen fuer die 22Aug93 Version von CLISP eingebaut.
;;;
;;; Revision 1.70  1993/08/24  11:20:08  pm
;;; Erweiterungen um C-Pointer
;;;
;;; Revision 1.69  1993/07/28  13:24:53  uho
;;; Keine .lisp-Extension mehr bei (require clisp-compat)
;;;
;;; Revision 1.68  1993/07/28  09:11:18  uho
;;; Das CLICC-Package 'used' das LISP-Package nun fuer alle Versionen
;;; explizit.
;;;
;;; Revision 1.67  1993/07/27  19:34:24  wg
;;; Bugfix: defpackage's in make-package geaendert. Laeuft jetzt unter
;;; CMU-CL.
;;;
;;; Revision 1.66  1993/07/27  08:21:11  hk
;;; (setq *ti-verbosity* 3) entfernt, da es in tidef gesetzt wird.
;;;
;;; Revision 1.65  1993/07/26  18:46:37  wg
;;; Definition von require fuer CMU-CL V.16f geaendert.
;;;
;;; Revision 1.64  1993/07/22  09:05:02  pm
;;; Weitere Typen fuer das FFI eingefügt
;;;
;;; Revision 1.63  1993/06/22  12:49:28  uho
;;; Absolute Output-Pfade fuer do-inline-module und do-lisp-module
;;;
;;; Revision 1.62  1993/06/19  21:46:50  hk
;;; Split-files wird beim Aufruf (clicc ...) nicht mehr durch eine globale
;;; Variable, sondern nur noch durch Keyword Parameter gesteuert. Dadurch
;;; wird verhindert, dass der Aufruf von (ask-runtime) das Verhalten eines
;;; spaeteren aufrufs von (clicc ..) beeinflusst.
;;;
;;; Revision 1.61  1993/06/19  20:39:42  hk
;;; Typoptimierungen im Lisp Modul wieder angeschaltet.
;;; Loop in ask-runtime wieder eingebaut, sie wird jedoch nach A)ll
;;; automatisch verlassen.
;;;
;;; Revision 1.60  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.59  1993/06/16  12:54:59  hk
;;; call-next-method defmethod defgeneric defclass nicht mehr aus LISP
;;; importiert.
;;;
;;; Revision 1.58  1993/06/15  13:55:17  hk
;;; bq-read wird nicht nur von allegro required, keine Loop mehr in ask-runtime
;;;
;;; Revision 1.57  1993/06/08  13:40:02  jh
;;; do-to fuer das lisp-Modul ausgeschaltet.
;;;
;;; Revision 1.56  1993/06/08  10:57:30  kl
;;; Bindungen von *ti-level* und *ti-verbosity* entfernt.
;;;
;;; Revision 1.55  1993/06/04  14:03:48  hk
;;; Einige Imports ins Lisp Package gestrichen, *MEMSIZES* nach clcdef,
;;; *C-max-line-count* auf 50000, *split* durch *SPLIT-FILES* ersetzt,
;;; neues Keyword :max-lines, Funktionen has-prefix und strip-extension
;;; gestrichen.
;;;
;;; Revision 1.54  1993/06/03  14:44:54  jh
;;; (gensym 0) durch (setq *gensym-counter* 0) ersetzt.
;;;
;;; Revision 1.53  1993/06/03  14:15:14  jh
;;; (gensym 0) in clicc eingebaut, damit die erzeugten Symbole moeglichst
;;; kleine Zahlen angehaengt bekommen.
;;;
;;; Revision 1.52  1993/05/31  17:01:48  pm
;;; Laufzeit-Funktionen im FFI-Package exportiert
;;;
;;; Revision 1.51  1993/05/23  17:48:39  pm
;;; Defpackage FFI um die Eintraege fuer
;;; die primitiven Typen und die in Lisp geschriebenen Laufzeit-
;;; Funktionen erweitert
;;;
;;; Revision 1.50  1993/05/21  13:56:46  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.49  1993/05/19  10:24:32  hk
;;; *CLICC-INPUT* gestrichen.
;;;
;;; Revision 1.48  1993/05/19  10:23:36  uho
;;; Anpassung fuer CLISP eingefuegt.
;;;
;;; Revision 1.47  1993/05/14  10:24:56  hk
;;; Weniger Symbole von Lisp nach CLICC-LISP importiert.
;;;
;;; Revision 1.46  1993/05/12  13:23:07  hk
;;; Fuer Allegro: (setq *bq-level* 0)
;;;
;;; Revision 1.45  1993/05/11  11:10:41  hk
;;; Inlining fuer Lisp Und Inline Modul angeschaltet, (setq *ti-verbosity* 3).
;;;
;;; Revision 1.44  1993/05/10  15:57:35  hk
;;; lisp-module -> do-lisp-module, inline-module -> do-inline-module.
;;;
;;; Revision 1.43  1993/05/10  12:23:05  jh
;;; Inlining fuer lisp- und inline-module ausgeschaltet.
;;;
;;; Revision 1.42  1993/05/09  14:17:40  kl
;;; Typinferenz fuer das Lisp-Modul eingeschaltet.
;;;
;;; Revision 1.41  1993/05/08  18:49:37  hk
;;; Typinferenz fuer Lisp-modul wg. Fehler abgeschaltet.
;;;
;;; Revision 1.40  1993/05/06  14:23:24  pm
;;; (defpackage FFI) aufgerauemt
;;; ti-level auf 3 gesetzt
;;;
;;; Revision 1.39  1993/05/05  10:06:11  hk
;;; :split aktiviert, clicc-runtime gestrichen.
;;;
;;; Revision 1.38  1993/05/03  15:25:36  hk
;;; Typinferenz angeschaltet.
;;;
;;; Revision 1.37  1993/05/03  12:08:02  pm
;;; defmethod fuer das FFI eingebaut
;;;
;;; Revision 1.36  1993/04/22  12:44:27  hk
;;; Keywords :header-files :comile-module :include beim Aufruf von
;;; clicc gestrichen.
;;;
;;; Revision 1.35  1993/04/22  12:32:47  hk
;;; Package Runtime in RT umbenannt, hat Nickname RUNTIME.
;;; Alle Common Lisp Symbole von lisp nach clicc-lisp importiert und dort
;;; exportiert. Neue Keywords :lisp-module und :inline-module beim
;;; Aufruf von clicc. Ask-Runtime ruft nur noch lisp-module oder
;;; inline-module auf.
;;;
;;; Revision 1.34  1993/04/08  11:23:10  uho
;;; CLISP spezifischen Code eingefuegt.
;;;
;;; Revision 1.33  1993/04/06  10:21:56  hk
;;; (rename-package LISP CLICC-LISP '(L))
;;;
;;; Revision 1.32  1993/04/06  10:18:24  hk
;;; (rename-package LISP CLICC-LISP L)
;;;
;;; Revision 1.31  1993/04/06  09:58:40  hk
;;; Package CLICC-LISP hat nun den Nickname L (also L:MAPCAR ..).
;;;
;;; Revision 1.30  1993/04/02  11:58:58  uho
;;; Neues Keyword bwim Aufruf von clicc: :module-compiler, um den Modul-
;;; compiler zu aktivieren (Default: Komplettcompilation)
;;;
;;; Revision 1.29  1993/03/22  11:07:27  hk
;;; Rename-Package Bug von allegro-v4.1 umgangen.
;;;
;;; Revision 1.28  1993/03/19  09:48:21  hk
;;; *SHOW-VERSION* in ask-runtime auf nil.
;;;
;;; Revision 1.27  1993/03/05  16:33:46  kl
;;; Compilerschalter fuer den Intensitaetsgrad der Typinferenz eingebaut.
;;;
;;; Revision 1.26  1993/02/25  12:41:03  jh
;;; Print
;;;
;;; Revision 1.25  1993/02/18  15:06:57  kl
;;; Liste der waehrend der Makroexpansion benutzten Funktionen erweitert.
;;;
;;; Revision 1.24  1993/02/17  08:25:14  kl
;;; Anpassungen fuer Lucid und CMU vorgenommen.
;;;
;;; Revision 1.23  1993/02/16  17:32:00  hk
;;; Neue Packages clicc-lisp, clicc-user etc, Revision Keyword, ...
;;;
;;; Revision 1.22  1993/01/21  13:49:58  uho
;;; Neue clicc-Option :flat-ifs  zum optinalen Generieren falcher ifs
;;;
;;; Revision 1.21  1993/01/11  16:50:26  hk
;;; getenv -> c-getenv
;;;
;;; Revision 1.20  1993/01/11  16:47:37  hk
;;; getenv -> c-getenv
;;;
;;; Revision 1.19  1993/01/02  12:58:46  kl
;;; *NO-CODEGEN* zum Abschalten der Codegenerierung eingebaut.
;;; Deklaration von *SPLIT* nach clcdef verlegt.
;;;
;;; Revision 1.18  1992/12/21  12:32:26  hk
;;; In ask-runtime wird das Verzeichnis bei Anzeige erneut gelesen.
;;;
;;; Revision 1.17  1992/11/23  11:38:48  hk
;;; In clicc-runtime wird *CLICC-PRINT* an nil gebunden.
;;;
;;; Revision 1.16  1992/11/05  09:06:40  kl
;;; (require printzs) nicht nur fuer allegro. compile-clicc erweitert.
;;;
;;; Revision 1.15  1992/10/13  16:16:24  uho
;;; *COMPILE-MODULE* fuer neues Modulsystem eingefuegt.
;;;
;;; Revision 1.14  1992/10/09  15:03:03  hk
;;; Neue Optionen fuer ask-runtime.
;;;
;;; Revision 1.13  1992/10/05  16:15:52  uho
;;; *MEMSIZES* Defaultwerte auf Anzahlen statt kB umgestellt.
;;;
;;; Revision 1.12  1992/10/05  11:20:47  uho
;;; *MEMSIZES* zum Parametrisieren der Speicheranforderung eingefuegt.
;;;
;;; Revision 1.11  1992/09/21  14:01:23  hk
;;; excl:*enable-package-locked-errors* nur fuer allegro-4.1
;;;
;;; Revision 1.10  1992/08/27  14:15:10  kl
;;; ask-runtime verbessert.
;;;
;;; Revision 1.9  1992/08/12  08:42:22  hk
;;; *CLICC-VERSION* gestrichen, wurde nicht benutzt.
;;;
;;; Revision 1.8  1992/08/12  08:39:25  hk
;;; Deklarationen fuer globale Variablen eingefuegt.
;;;
;;; Revision 1.7  1992/08/11  16:20:36  hk
;;; Angepasst an CMU-Lisp.
;;;
;;; Revision 1.6  1992/08/10  11:12:53  hk
;;; Angepasst an CMU-Lisp.
;;;
;;; Revision 1.5  1992/08/06  12:01:21  hk
;;; printzs wird fuer Allegro automatisch geladen.
;;;
;;; Revision 1.4  1992/07/06  15:43:19  hk
;;; In Allegro4.1 in das LISP-Package string-char importiert + exportiert.
;;;
;;; Revision 1.3  1992/06/04  09:45:11  hk
;;; *clicc-print* auf t gesetzt (zum debuggen besser geeignet).
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; System-spezifische Dinge
;;------------------------------------------------------------------------------
#+(or allegro-v4.0 allegro-v4.1)
(progn (setq excl:*cltl1-in-package-compatibility-p* t)
       (setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)
       #+allegro-v4.1 (setq excl:*enable-package-locked-errors* nil)
       (import '(cltl1:string-char
                 cltl1:string-char-p)
               "LISP")
       (export '(cltl1:string-char
                 cltl1:string-char-p)
               "LISP")
       #+allegro-v4.1 (setq excl:*enable-package-locked-errors* t))

#+allegro-v4.0
(progn (import '(cltl1:require
                 cltl1:provide
                 cltl1:*modules*)
               "LISP")
       (export '(cltl1:require
                 cltl1:provide
                 cltl1:*modules*)
               "LISP"))

#+AKCL
(multiple-value-bind (sym status)
    (find-symbol "TAG" "LISP")
  (when (eq status :EXTERNAL)
    (unexport sym "LISP")))

#+CLISP
(require "clisp-compat")


#+CMU
(progn
  (defvar *modules* ())
  (defun require (&rest x) 
    (unless (member (car x) *modules* :test #'equal) (apply #'load x)))
  (defun provide (x)  (pushnew x *modules*)))

;;------------------------------------------------------------------------------
;; Package RUNTIME erzeugen
;;------------------------------------------------------------------------------
(unless (find-package "RT")
        (make-package "RT" :use () :nicknames '("RUNTIME")))

;;------------------------------------------------------------------------------
;; Package FFI erzeugen
;;------------------------------------------------------------------------------
(unless (find-package "FFI")
        (make-package "FFI" :use ()))

(in-package "FFI" :use ())

(lisp:export 
 '(c-array c-char c-char-p c-double c-double-p c-enum c-float c-float-p c-fun
   c-handle c-int c-int-p c-long c-long-double c-long-double-p c-long-p c-ptr
   c-short c-short-p c-string c-string-p c-struct c-union c-unsigned-char
   c-unsigned-char-p c-unsigned-int c-unsigned-int-p c-unsigned-long
   c-unsigned-long-p c-unsigned-short c-unsigned-short-p c-vararg c-void
   copy-c-string def-c-type def-call-in def-call-out foreign-package-name
   free lisp-character lisp-float lisp-integer load-foreign make-c-string
   make-lisp-string)
 "FFI")

(lisp:in-package "USER")

;;------------------------------------------------------------------------------
;; Definition der Funktion rt::c-getenv
;;------------------------------------------------------------------------------
#+(or EXCL KCL)
(defun rt::c-getenv (name)
  (system:getenv name))
#+LUCID
(defun rt::c-getenv (name)
  (lcl:environment-variable name))
#+CMU
(defun rt::c-getenv (name)
  (setq name (intern name "KEYWORD"))
  (cdr (assoc name extensions:*ENVIRONMENT-LIST*)))
#+CLISP
(defun rt::c-getenv (name)
  (system::getenv name))

;;------------------------------------------------------------------------------
;; Eigene Lisp- und User-Packages generieren
;;------------------------------------------------------------------------------
(unless (find-package "CLICC-LISP")
        (make-package "CLICC-LISP" :use () :nicknames '("L")))

(import '(
   
   ;; Lambda-List-Keywords
   ;;---------------------
   &allow-other-keys &aux &body &environment &key &optional &rest &whole
   
   ;; Miscellaneous
   ;;--------------
   lambda declare otherwise *
   
   ;; Special Operators
   ;;------------------
   block catch eval-when flet function go if labels let let* 
   locally macrolet multiple-value-call multiple-value-prog1 progn progv quote
   return-from setq tagbody the throw unwind-protect
     
   ;; Atomic Type Specifiers
   ;;-----------------------
   array atom bignum bit bit-vector
   character compiled-function complex cons double-float
   fixnum float function hash-table
   integer keyword list long-float nil null number package
   pathname random-state ratio rational readtable sequence
   short-float signed-byte simple-array simple-bit-vector
   simple-string simple-vector single-float standard-char stream string t
   unsigned-byte vector
   
   ;; Compound-Only Type Specifier Names
   ;;-----------------------------------
   and eql member mod not or satisfies values

   ;; Declaration Identifiers
   ;;------------------------
   ignore
   
   ;; Macros, Data and Control Flow
   ;;------------------------------
   psetq return
   and cond or when unless case ccase ecase typecase ctypecase etypecase
   multiple-value-bind multiple-value-list multiple-value-setq 
   prog prog* prog1 prog2 psetf shiftf rotatef

   ;; Other Macros
   ;;-------------
   assert decf do do* do-all-symbols
   do-external-symbols do-symbols dolist dotimes loop remf with-open-stream
   with-input-from-string with-output-to-string with-open-file

   ;; Functions wich are used during macro expansion
   ;;-----------------------------------------------
   >= car cdr close streamp open get-output-stream-string
   make-string-output-stream make-string-input-stream list 
   list* 1+ 1- ash logxor logior logand logandc2
   typep eql error values not

   ;; Functions wich are used during setf expansion
   ;;----------------------------------------------
   cons append list apply aref funcall + - adjoin first rest

   ;; Functions which are used for macro expander function generation
   ;;----------------------------------------------------------------
   getf car cadr caddr cadddr cdr cddr cdddr cddddr endp error nth nthcdr
   null not eq endp member)

   "CLICC-LISP")

  ;;---------------------------------------------------------------------------
  
(in-package "CLICC-LISP" :use ())

(lisp:export '(
          &allow-other-keys &aux &body &environment &key &optional
          &rest &whole * ** *** *break-on-signals* *compile-file-pathname*
          *compile-file-truename* *compile-print* *compile-verbose* *debug-io*
          *debugger-hook* *default-pathname-defaults* *error-output* *features*
          *gensym-counter* *load-pathname* *load-print* *load-truename*
          *load-verbose* *macroexpand-hook* *modules* *package* *print-array*
          *print-base* *print-case* *print-circle* *print-escape* *print-gensym*
          *print-length* *print-level* *print-lines* *print-miser-width*
          *print-pprint-dispatch* *print-pretty* *print-radix* *print-readably*
          *print-right-margin* *query-io* *random-state* *read-base*
          *read-default-float-format* *read-eval* *read-suppress* *readtable*
          *standard-input* *standard-output* *terminal-io* *trace-output* + ++
          +++ - / // /// /= 1+ 1- < <= = > >= abort abs acons acos acosh
          add-method adjoin adjust-array adjustable-array-p allocate-instance
          alpha-char-p alphanumericp and append apply apropos apropos-list aref
          arithmetic-error arithmetic-error-operands arithmetic-error-operation
          array array-dimension array-dimension-limit array-dimensions
          array-displacement array-element-type array-has-fill-pointer-p
          array-in-bounds-p array-rank array-rank-limit array-row-major-index
          array-total-size array-total-size-limit arrayp ash asin asinh assert
          assoc assoc-if assoc-if-not atan atanh atom base-char base-string
          bignum bit bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand
          bit-nor bit-not bit-orc1 bit-orc2 bit-vector bit-vector-p bit-xor
          block boole boole-1 boole-2 boole-and boole-andc1 boole-andc2 boole-c1
          boole-c2 boole-clr boole-eqv boole-ior boole-nand boole-nor boole-orc1
          boole-orc2 boole-set boole-xor both-case-p boundp break
          broadcast-stream broadcast-stream-streams built-in-class butlast byte
          byte-position byte-size caaaar caaadr caaar caadar caaddr caadr caar
          cadaar cadadr cadar caddar cadddr caddr cadr call-arguments-limit
          call-method call-next-method car case catch ccase cdaaar cdaadr cdaar
          cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
          cdr ceiling cell-error cell-error-name cerror change-class char
          char-code char-code-limit char-downcase char-equal char-greaterp
          char-int char-lessp char-name char-not-equal char-not-greaterp
          char-not-lessp char-upcase char/= char< char<= char= char> char>=
          character characterp check-type cis class class-name class-of
          clear-input clear-output close clrhash code-char coerce
          compilation-speed compile compile-file compile-file-pathname
          compiled-function compiled-function-p compiler-macro
          compiler-macro-function complement complex complexp
          compute-applicable-methods compute-restarts concatenate
          concatenated-stream concatenated-stream-streams cond condition
          conjugate cons consp constantly constantp continue control-error
          copy-alist copy-list copy-pprint-dispatch copy-readtable copy-seq
          copy-structure copy-symbol copy-tree cos cosh count count-if
          count-if-not ctypecase debug decf declaim declaration declare
          decode-float decode-universal-time defclass defconstant defgeneric
          define-compiler-macro define-condition define-method-combination
          define-modify-macro define-setf-expander defmacro defmethod defpackage
          defparameter defsetf defstruct deftype defun defvar delete
          delete-duplicates delete-file delete-if delete-if-not delete-package
          denominator deposit-field describe describe-object destructuring-bind
          digit-char digit-char-p directory directory-namestring disassemble
          division-by-zero do do* do-all-symbols do-external-symbols do-symbols
          documentation dolist dotimes double-float double-float-epsilon
          double-float-negative-epsilon dpb dribble dynamic-extent ecase
          echo-stream echo-stream-input-stream echo-stream-output-stream ed
          eighth elt encode-universal-time end-of-file endp enough-namestring
          ensure-generic-function eq eql equal equalp error etypecase eval
          eval-when evenp every exp export expt extended-char fboundp fceiling
          fdefinition ffloor fifth file-author file-error file-error-pathname
          file-length file-namestring file-position file-stream
          file-string-length file-write-date fill fill-pointer find
          find-all-symbols find-class find-if find-if-not find-method
          find-package find-restart find-symbol finish-output first fixnum flet
          float float-digits float-precision float-radix float-sign
          floating-point-inexact floating-point-invalid-operation
          floating-point-overflow floating-point-underflow floatp floor
          fmakunbound force-output format formatter fourth fresh-line fround
          ftruncate ftype funcall function function-keywords
          function-lambda-expression functionp gcd generic-function gensym
          gentemp get get-decoded-time get-dispatch-macro-character
          get-internal-real-time get-internal-run-time get-macro-character
          get-output-stream-string get-properties get-setf-expansion
          get-universal-time getf gethash go graphic-char-p handler-bind
          handler-case hash-table hash-table-count hash-table-p
          hash-table-rehash-size hash-table-rehash-threshold hash-table-size
          hash-table-test host-namestring identity if ignorable ignore
          ignore-errors imagpart import in-package incf initialize-instance
          inline input-stream-p inspect integer integer-decode-float
          integer-length integerp interactive-stream-p intern
          internal-time-units-per-second intersection invalid-method-error
          invoke-debugger invoke-restart invoke-restart-interactively isqrt
          keyword keywordp labels lambda lambda-list-keywords
          lambda-parameters-limit last lcm ldb ldb-test ldiff
          least-negative-double-float least-negative-long-float
          least-negative-normalized-double-float
          least-negative-normalized-long-float
          least-negative-normalized-short-float
          least-negative-normalized-single-float least-negative-short-float
          least-negative-single-float least-positive-double-float
          least-positive-long-float least-positive-normalized-double-float
          least-positive-normalized-long-float
          least-positive-normalized-short-float
          least-positive-normalized-single-float least-positive-short-float
          least-positive-single-float length let let* lisp-implementation-type
          lisp-implementation-version list list* list-all-packages list-length
          listen listp load load-logical-pathname-translations load-time-value
          locally log logand logandc1 logandc2 logbitp logcount logeqv
          logical-pathname logical-pathname-translations logior lognand lognor
          lognot logorc1 logorc2 logtest logxor long-float long-float-epsilon
          long-float-negative-epsilon long-site-name loop loop-finish
          lower-case-p machine-instance machine-type machine-version
          macro-function macroexpand macroexpand-1 macrolet make-array
          make-broadcast-stream make-concatenated-stream make-condition
          make-dispatch-macro-character make-echo-stream make-hash-table
          make-instance make-instances-obsolete make-list make-load-form
          make-load-form-saving-slots make-method make-package make-pathname
          make-random-state make-sequence make-string make-string-input-stream
          make-string-output-stream make-symbol make-synonym-stream
          make-two-way-stream makunbound map map-into mapc mapcan mapcar mapcon
          maphash mapl maplist mask-field max member member-if member-if-not
          merge merge-pathnames method method-combination
          method-combination-error method-qualifiers min minusp mismatch mod
          most-negative-double-float most-negative-fixnum
          most-negative-long-float most-negative-short-float
          most-negative-single-float most-positive-double-float
          most-positive-fixnum most-positive-long-float
          most-positive-short-float most-positive-single-float muffle-warning
          multiple-value-bind multiple-value-call multiple-value-list
          multiple-value-prog1 multiple-value-setq multiple-values-limit
          name-char namestring nbutlast nconc next-method-p nil nintersection
          ninth no-applicable-method no-next-method not notany notevery
          notinline nreconc nreverse nset-difference nset-exclusive-or
          nstring-capitalize nstring-downcase nstring-upcase nsublis nsubst
          nsubst-if nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not
          nth nth-value nthcdr null number numberp numerator nunion oddp open
          open-stream-p optimize or otherwise output-stream-p package
          package-error package-error-package package-name package-nicknames
          package-shadowing-symbols package-use-list package-used-by-list
          packagep pairlis parse-error parse-integer parse-namestring pathname
          pathname-device pathname-directory pathname-host pathname-match-p
          pathname-name pathname-type pathname-version pathnamep peek-char phase
          pi plusp pop position position-if position-if-not pprint
          pprint-dispatch pprint-exit-if-list-exhausted pprint-fill
          pprint-indent pprint-linear pprint-logical-block pprint-newline
          pprint-pop pprint-tab pprint-tabular prin1 prin1-to-string princ
          princ-to-string print print-not-readable print-not-readable-object
          print-object print-unreadable-object probe-file proclaim prog prog*
          prog1 prog2 progn program-error progv provide psetf psetq push pushnew
          quote random random-state random-state-p rassoc rassoc-if
          rassoc-if-not ratio rational rationalize rationalp read read-byte
          read-char read-char-no-hang read-delimited-list read-from-string
          read-line read-preserving-whitespace reader-error readtable
          readtable-case readtablep real realp realpart reduce
          reinitialize-instance rem remf remhash remove remove-duplicates
          remove-if remove-if-not remove-method remprop rename-file
          rename-package replace require rest restart restart-bind restart-case
          restart-name return return-from revappend reverse room rotatef round
          row-major-aref rplaca rplacd safety satisfies sbit scale-float schar
          search second sequence serious-condition set set-difference
          set-dispatch-macro-character set-exclusive-or set-macro-character
          set-pprint-dispatch set-syntax-from-char setf setq seventh shadow
          shadowing-import shared-initialize shiftf short-float
          short-float-epsilon short-float-negative-epsilon short-site-name
          signal signed-byte signum simple-array simple-base-string
          simple-bit-vector simple-bit-vector-p simple-condition
          simple-condition-format-arguments simple-condition-format-control
          simple-error simple-string simple-string-p simple-type-error
          simple-vector simple-vector-p simple-warning sin single-float
          single-float-epsilon single-float-negative-epsilon sinh sixth sleep
          slot-boundp slot-exists-p slot-makunbound slot-missing slot-unbound
          slot-value software-type software-version some sort space special
          special-operator-p speed sqrt stable-sort standard standard-char
          standard-char-p standard-class standard-generic-function
          standard-method standard-object step storage-condition store-value
          stream stream-element-type stream-error stream-error-stream
          stream-external-format streamp string string-capitalize
          string-downcase string-equal string-greaterp string-left-trim
          string-lessp string-not-equal string-not-greaterp string-not-lessp
          string-right-trim string-stream string-trim string-upcase string/=
          string< string<= string= string> string>= stringp structure
          structure-class structure-object style-warning sublis subseq subsetp
          subst subst-if subst-if-not substitute substitute-if substitute-if-not
          subtypep svref sxhash symbol symbol-function symbol-macrolet
          symbol-name symbol-package symbol-plist symbol-value symbolp
          synonym-stream synonym-stream-symbol t tagbody tailp tan tanh tenth
          terpri the third throw time trace translate-logical-pathname
          translate-pathname tree-equal truename truncate two-way-stream
          two-way-stream-input-stream two-way-stream-output-stream type
          type-error type-error-datum type-error-expected-type type-of typecase
          typep unbound-slot unbound-slot-instance unbound-variable
          undefined-function unexport unintern union unless unread-char
          unsigned-byte untrace unuse-package unwind-protect
          update-instance-for-different-class
          update-instance-for-redefined-class upgraded-array-element-type
          upgraded-complex-part-type upper-case-p use-package use-value
          user-homedir-pathname values values-list variable vector vector-pop
          vector-push vector-push-extend vectorp warn warning when
          wild-pathname-p with-accessors with-compilation-unit
          with-condition-restarts with-hash-table-iterator
          with-input-from-string with-open-file with-open-stream
          with-output-to-string with-package-iterator with-simple-restart
          with-slots with-standard-io-syntax write write-byte write-char
          write-line write-string write-to-string y-or-n-p yes-or-no-p zerop)

        "CLICC-LISP"
        )

(lisp:in-package "USER")

;;------------------------------------------------------------------------------
(unless (find-package "CLICC-USER")
        (make-package "CLICC-USER" :use '("CLICC-LISP")))

;;------------------------------------------------------------------------------
;; Den Rest in das Package CLICC
;;------------------------------------------------------------------------------
(unless (find-package "CLICC")
  #+CLOS          (make-package "CLICC" :use '("LISP" "CLOS"))
  #+PCL           (make-package "CLICC" :use '("LISP" "PCL"))
  #-(or PCL CLOS) (make-package "CLICC" :use '("LISP"))
)

(in-package "CLICC")

(import
 '(do-lisp-module do-inline-module ask-runtime compile-clicc clicc do-clicc)
 "USER")

;;------------------------------------------------------------------------------
;; Eigenen Backquote Reader installieren, um eine normierte Darstellung
;; fuer eingelesene Backquote Ausdruecke zu erhalten.
;;------------------------------------------------------------------------------
(require "bq-read")

;;------------------------------------------------------------------------------
(require "clcdef")

;;------------------------------------------------------------------------------
;; CLICC soll gespraechig sein
;;------------------------------------------------------------------------------
(setq *SHOW-VERSION* t)
(setq *CLICC-PRINT* t)

;;------------------------------------------------------------------------------
(require "config")

;;------------------------------------------------------------------------------
;; DIE Funktion zum Aufruf von CLICC
;; Resultat zeigt an, ob die Uebersetzung erfolgreich war
;;------------------------------------------------------------------------------
(defun clicc (filename &KEY
                       (out              nil)
                         
                       (module-compiler  nil)
                       (lisp-module      nil)
                       (inline-module    nil)
                         
                       (verbose          *SHOW-VERSION*)
                       (print            *CLICC-PRINT*)
                       (memsizes         *MEMSIZES*)
                       (no-codegen       *NO-CODEGEN*)
                       (ti-level         *TI-Level*)
                       (obrep            *OBREP*)
                       (max-lines        *C-max-line-count*)
                       (split            *SPLIT-FILES*)
                       (flat-ifs         *FLAT-IFS*))

  (setq *FILENAME*         filename)
  (setq *OUT-FILENAME*     out)
  
  (setq *MODULE-COMPILER*  module-compiler)
  (setq *LISP-MODULE*      lisp-module)
  (setq *INLINE-MODULE*    inline-module)

  ;; Durch die Zuweisung werden die aktuellen Einstellungen zum Defaultwert
  ;; für den nächsten Aufruf
  ;;------------------------
  (setq *SHOW-VERSION*     verbose)
  (setq *CLICC-PRINT*      print)
  (setq *MEMSIZES*         memsizes)
  (setq *NO-CODEGEN*       no-codegen)
  (setq *TI-Level*         ti-level)
  (setq *OBREP*            obrep)
  (setq *C-max-line-count* max-lines)
  (setq *SPLIT-FILES*      split)
  (setq *FLAT-IFS*         flat-ifs)
  
  ;; Package automatisch auf CLICC setzen, damit Debugging erleichtert wird
  ;;-----------------------------------------------------------------------
  (in-package "CLICC")
  
  (let* ((lisp-package (find-package "LISP"))
         (lisp-name (package-name lisp-package))
         (lisp-nick (package-nicknames lisp-package))
         (user-package (find-package "USER"))
         (user-name (package-name user-package))
         (user-nick (package-nicknames user-package))
         (*package* *package*))
    (unwind-protect
         (progn
           #+(or allegro-v4.0 allegro-v4.1)(setq *bq-level* 0)
           #+allegro-v4.1 (setq excl:*enable-package-locked-errors* nil)
           (rename-package lisp-package "original-lisp")
           #+allegro-v4.1 (setq excl:*enable-package-locked-errors* t)
           (rename-package user-package "original-user")
           (rename-package "CLICC" "original-clicc")
           (when (find-package "CLICC-CLICC")
             (rename-package "CLICC-CLICC" "CLICC"))
           (rename-package "CLICC-LISP" "LISP")
           (rename-package "CLICC-USER" "USER")
           (do-clicc))
      (rename-package "LISP" "CLICC-LISP" '("L"))
      (rename-package "USER" "CLICC-USER")
      (when (find-package "CLICC")
        #+allegro-v4.1 (rename-package "CLICC" "CLICC")
        (rename-package "CLICC" "CLICC-CLICC"))
      (rename-package "original-clicc" "CLICC")
      #+allegro-v4.1 (setq excl:*enable-package-locked-errors* nil)
      (rename-package lisp-package lisp-name lisp-nick)
      #+allegro-v4.1 (setq excl:*enable-package-locked-errors* t)
      (rename-package user-package user-name user-nick))))

;;------------------------------------------------------------------------------
;; Die restlichen Definitionen dienen zur leichteren Handhabung der Uebersetzung
;; von clicc bzw. des Clicc-Laufzeitsystems
;;------------------------------------------------------------------------------
(defvar *CLICC-LISP-PATH*)
(defvar *CLICC-LISP2C-STRING*)
(defvar *CLICC-LISP2C-PATH*)
(defvar *CLICC-COMPILER-PATH*)
(defvar *lisp-extension*)
(defvar *compiled-extension*)

;;------------------------------------------------------------------------------
;; Pfadnamen fuer CLiCC Directories werden gesetzt.
;; Dies sind die Pfadnamen fuer die Verzeichnisse des Laufzeitsystems und den
;; Quellcode des Compilers.
;; Die Wurzel des CLiCC-Filebaums und die Stellen, wo lisp.def/syntax und
;; sys.def/syntax zu finden sind, werden in config.lisp in der Funktion
;; setup-clicc-pathenames gesetzt.
;;------------------------------------------------------------------------------
(defun setup-clcload-pathnames ()

  (setq *CLICC-LISP-PATH*
        (pathname-directory (concatenate 'string
                                         *CLICC-PATH-STRING*
                                         "src/runtime/lisp/")))
  (setq *CLICC-LISP2C-STRING*
        (concatenate 'string
                     *CLICC-PATH-STRING*
                     "src/runtime/lisp2c/"))
  (setq *CLICC-LISP2C-PATH* (pathname-directory *CLICC-LISP2C-STRING*))

  (setq *CLICC-COMPILER-PATH*
        (pathname-directory (concatenate 'string
                                         *CLICC-PATH-STRING*
                                         "src/compiler/"))))

(setup-clcload-pathnames)

  
(setq *lisp-extension* "lisp")

#+LUCID
(progn
  (import '(lcl:quit lcl:cd lcl:pwd))
  #-APOLLO
  (setq *compiled-extension* "sbin")
  #+APOLLO
  (setq *compiled-extension* "lbin"))
#+EXCL
(progn
  (import '(excl:exit excl:chdir excl:current-directory))
  (setq *compiled-extension* "fasl"))
#+KCL
(progn 
  (setq *lisp-extension* "lsp")
  (setq *compiled-extension* "o"))
#+CMU
(progn
  (import '(extensions:quit))
  (setq *compiled-extension* "sparcf"))
#+CLISP
(progn
  (setq *lisp-extension* "lsp")
  (setq *compiled-extension* "fas"))

;;------------------------------------------------------------------------------
;; Fragt, welche in LISP geschriebenen CLICC-Laufzeitsystem-Funktionen mittels
;; clicc nach C uebersetzt werden sollen.
;;------------------------------------------------------------------------------
(defun ask-runtime ()
  (let ((*SHOW-VERSION* nil)
        (*CLICC-PRINT* t)
        (*SPLIT-FILES* *SPLIT-FILES*)
        (*ONLY-PASS1* *ONLY-PASS1*))

    (loop
     (format
      t
      "~&A)ll L)isp-module I)nline-module Q)uit~%~
       P)rint messages is ~a  S)plit is ~a  O)nly Pass1 is ~a  ~
       K)ill C-Files ~%~
       choose: "
      (if *CLICC-PRINT* "on" "off")
      (if *SPLIT-FILES* "on" "off")
      (if *ONLY-PASS1* "on" "off")) 

     (case (let ((*package* (find-package "CLICC"))) (read))
           (Q (return))    
           (A (do-inline-module) (do-lisp-module) (return))
           (L (do-lisp-module))
           (I (do-inline-module))
           (O (setq *ONLY-PASS1* (not *ONLY-PASS1*)))
           (P (setq *CLICC-PRINT* (not *CLICC-PRINT*)))
           (S (setq *SPLIT-FILES* (not *SPLIT-FILES*)))
           (K (mapc #'delete-file
                    (directory (make-pathname
                                :directory *CLICC-LISP2C-PATH*
                                :name :wild
                                :type (pathname-type "any.c")))))))))

;;------------------------------------------------------------------------------
;; Pr"uft, ob Probleme durch Splitting auftreten werden
;;------------------------------------------------------------------------------
(defun check-split (unsplitted splitted)
  (not
   (cond
     (*SPLIT-FILES*
      (when (probe-file (concatenate 'string
                                     *CLICC-LISP2C-STRING* unsplitted ".c"))
        (format t "There is an unsplitted version, use K)ill first~%")
        t))
     (t (when (probe-file (concatenate 'string
                                       *CLICC-LISP2C-STRING* splitted ".c"))
          (format t "There is an splitted version, use K)ill first~%")
          t)))))

;;------------------------------------------------------------------------------
;; Compiliert das Lisp Modul des Laufzeitsystems
;;------------------------------------------------------------------------------
(defun do-lisp-module ()
  (when (check-split "lisp" "Fread")
    (let ((*C-max-line-count* nil)
          (*delete-verbosity* 2)
          (*default-pathname-defaults*
           (make-pathname :directory *CLICC-LISP-PATH*)))
      (clicc "lisp" 
             :out (concatenate 'string *CLICC-LISP2C-STRING* "lisp")
             :module-compiler t
             :lisp-module t))))

;;------------------------------------------------------------------------------
;; Compiliert die Inline Funktionen, ueber die der Compiler spezielles Wissen
;; hat
;;------------------------------------------------------------------------------
(defun do-inline-module ()
  (when (check-split "inline" "Fconsp")
    (let ((*delete-verbosity* 2)
          (*default-pathname-defaults*
           (make-pathname :directory *CLICC-LISP-PATH*)))
      (clicc "inline" 
             :out (concatenate 'string *CLICC-LISP2C-STRING* "inline")
             :module-compiler t
             :inline-module t
             :lisp-module t))))

;;------------------------------------------------------------------------------
;; Kompiliert alle in *MODULES* angegebenen Dateien neu, falls noetig.
;;------------------------------------------------------------------------------
(defun compile-clicc (&optional (module-list (reverse *MODULES*)))
  (dolist (module module-list)
    (let* ((lisp-filename 
            (make-pathname
             :directory *CLICC-COMPILER-PATH*
             :name (pathname-name module)
             :type (pathname-type
                    (concatenate 'string "any." *lisp-extension*))))
           (sbin-filename
            (make-pathname
             :type (pathname-type
                    (concatenate 'string "any." *compiled-extension*))
             :defaults lisp-filename)))
      (when (probe-file lisp-filename)
        (when (os-file-newer-as lisp-filename sbin-filename)
          #+CMU
          (compile-file lisp-filename :load t)
          #-CMU
          (progn
            (compile-file lisp-filename)
            (load         sbin-filename)))))))

;;------------------------------------------------------------------------------
(defun os-file-newer-as (file1 file2)
  (let ((file2-write-date (and (probe-file file2) (file-write-date file2))))
    (or (null file2-write-date)
        (> (file-write-date file1) file2-write-date))))

;;------------------------------------------------------------------------------
(require "clcmain")
;;;#-CMU17(require "printzs")              ;bug in pcl:class-direct-superclasses
