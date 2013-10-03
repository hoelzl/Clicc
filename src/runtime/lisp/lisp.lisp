;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Das Lisp Modul mit Loads, Definitionen und Toplevel Forms,
;;;            die der Initialisierung dienen.
;;;
;;; $Revision: 1.20 $
;;; $Log: lisp.lisp,v $
;;; Revision 1.20  1994/05/24  14:08:34  sma
;;; most-positive-fixnum, most-negaitiv-fixnum werden jetzt in fspec.c je
;;; nach Obrep zur Laufzeit berechnet.
;;;
;;; Revision 1.19  1994/01/27  16:22:07  kl
;;; rt::fixnump eingeführt.
;;;
;;; Revision 1.18  1994/01/24  16:22:10  sma
;;; Symbol T wird jetzt hier definiert.
;;;
;;; Revision 1.17  1993/12/09  17:35:28  sma
;;; Typ-Definitionen von simple-array, simple-vector, simple-string
;;; korrigiert. Ladereihenfolge der Lisp-Dateien des Laufzeitsystems
;;; geändert.
;;;
;;; Revision 1.16  1993/11/25  17:19:59  hk
;;; error.lisp wird nach print.lisp geladen, damit die special
;;; Deklarationen für *print-level* etc. bekannt sind.
;;;
;;; Revision 1.15  1993/08/20  08:47:36  hk
;;; list wird nach seq geladen, da check-seq-test nun ein Macro ist
;;;
;;; Revision 1.14  1993/07/26  14:47:00  hk
;;; import-nil-and-t erst nach Initialierung von *package* ausfuehren.
;;;
;;; Revision 1.13  1993/07/26  13:55:56  hk
;;; export-nil-and-t gestrichen.
;;; import-nil-and-t wird frueh in der Initialisierung des Lisp Moduls
;;; ausgefuehrt, damit keine Probleme bei einem spaeteren export
;;; auftreten.
;;;
;;; Revision 1.12  1993/07/26  12:32:08  hk
;;; *package* sofort nach dem Laden von packg.lisp mit dem Lisp Package
;;; initialisieren, damit es fuer folgende Package Operationen,
;;; insbesondere export, einen vernuenftigen Wert hat.
;;;
;;; Revision 1.11  1993/06/30  16:19:49  hk
;;; hash nach struct laden
;;;
;;; Revision 1.10  1993/06/17  17:33:47  hk
;;; Reihenfolge der LOAD Befehle umgestellt.
;;;
;;; Revision 1.9  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.8  1993/06/16  14:58:48  hk
;;; allsyms wird geladen.
;;;
;;; Revision 1.7  1993/06/05  23:05:47  hk
;;; Symbol '* exportiert.
;;;
;;; Revision 1.6  1993/05/13  14:02:30  hk
;;; Expander vom Typ vector korrigiert.
;;;
;;; Revision 1.5  1993/05/11  10:56:39  hk
;;; DEFSETF fuer first, .., fourth, rest gestrichen.
;;;
;;; Revision 1.4  1993/05/08  18:22:10  hk
;;; def-built-in eingefuegt,
;;; (defsetf SYMBOL-PLIST rt::SET-SYMBOL-PLIST) gestrichen.
;;;
;;; Revision 1.3  1993/05/04  13:14:41  pm
;;; (load foreign.lisp) eingefuegt
;;;
;;; Revision 1.2  1993/04/22  10:34:50  hk
;;; In export-nil-and-t *package* an Lisp Package gebunden.
;;;
;;; Revision 1.1  1993/04/22  08:57:32  hk
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "LISP" :use ())
(export
 '(MOST-POSITIVE-FIXNUM MOST-NEGATIVE-FIXNUM SHORT-FLOAT-EPSILON
   SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON PI
   ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT ARRAY-TOTAL-SIZE-LIMIT
   CALL-ARGUMENTS-LIMIT LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT
   MULTIPLE-VALUES-LIMIT CHAR-CODE-LIMIT
   ARRAY ATOM BIGNUM BIT BIT BIT-VECTOR CHARACTER COMPILED-FUNCTION COMPLEX
   CONS DOUBLE-FLOAT FIXNUM FLOAT FUNCTION INTEGER KEYWORD LIST LONG-FLOAT MOD
   NULL NUMBER RANDOM-STATE RATIO RATIONAL SEQUENCE SHORT-FLOAT SIGNED-BYTE
   SIMPLE-BIT-VECTOR SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR
   STRING SYMBOL UNSIGNED-BYTE VECTOR T))

;;------------------------------------------------------------------------------
;; Das Symbol '* wird vom Laufzeitsystem benutzt, um unspezifizierte Teile von
;; Typen auszudruecken.
;;------------------------------------------------------------------------------
(export '*)

;;------------------------------------------------------------------------------
;; Konstanten
;;------------------------------------------------------------------------------
(defconstant T 'T)

(defconstant MOST-POSITIVE-FIXNUM (rt::most-positive-fixnum))
(defconstant MOST-NEGATIVE-FIXNUM (rt::most-negative-fixnum))
(defconstant SHORT-FLOAT-EPSILON     1.1920929e-7) ; from ALLEGRO
(defconstant SHORT-FLOAT-NEGATIVE-EPSILON 1.1920929e-7) ; from ALLEGRO
(defconstant PI                      3.141592653589793)
(defconstant ARRAY-DIMENSION-LIMIT   MOST-POSITIVE-FIXNUM)
(defconstant ARRAY-RANK-LIMIT        MOST-POSITIVE-FIXNUM)
(defconstant ARRAY-TOTAL-SIZE-LIMIT  MOST-POSITIVE-FIXNUM)
(defconstant CALL-ARGUMENTS-LIMIT    MOST-POSITIVE-FIXNUM)
(defconstant LAMBDA-LIST-KEYWORDS
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment))
(defconstant LAMBDA-PARAMETERS-LIMIT  MOST-POSITIVE-FIXNUM)
(defconstant MULTIPLE-VALUES-LIMIT   20) ; (1+ C:MV_BUFSIZE)
(defconstant CHAR-CODE-LIMIT         256)
(defconstant WRONG_TYPE "~S should be of type ~S")

;;------------------------------------------------------------------------------
;; Type Definitionen
;;------------------------------------------------------------------------------
(def-built-in ARRAY 
    :type-expander ((&optional type size)
                    (cond
                      ((eq '* type) nil)
                      ((symbolp type) nil)
                      (t (error "array type: illegal type ~a" type)))
                    (cond
                      ((eq '* size) nil)
                      ((and (integerp size) (plusp size))
                       (setq size (list size)))
                      ((and (listp size)
                            (every #'(lambda (x)
                                       (or (eq '* x)
                                           (and (integerp x) (plusp x))))
                                   size))
                       nil)
                      (t (error "array type: illegal size ~a" size)))
                    `(lisp::array-internal ,type ,size))
    :superclasses ())
(def-built-in VECTOR
  :type-expander
  ((&optional element-type size) `(array ,element-type (,size)))
  :superclasses (array))
(def-built-in BIT-VECTOR
    :type-expander ((&optional size) `(array bit (,size)))
    :superclasses (vector))
(def-built-in STRING
    :type-expander ((&optional size) `(array standard-char (,size)))
    :superclasses (vector))

(def-built-in CHARACTER
    :type-expander (() '(satisfies characterp))
    :superclasses ())
(def-built-in FUNCTION
    :type-expander (() '(satisfies functionp))
    :superclasses ())

(def-built-in NUMBER
    :type-expander (() '(satisfies numberp))
    :superclasses ())
(def-built-in COMPLEX
    :type-expander (() '(satisfies complexp))
    :superclasses (number))
(def-built-in FLOAT
    :type-expander (() '(satisfies floatp))
    :superclasses (number))
(def-built-in RATIONAL
    :type-expander (() '(satisfies rationalp))
    :superclasses (number))
(def-built-in RATIO
    :type-expander (() '(satisfies rt::ratiop))
    :superclasses (rational))
(def-built-in INTEGER
    :type-expander ((&optional low high)
                    (cond
                      ((eq '* low) nil)
                      ((integerp low) nil)
                      ((and (consp low) (integerp (car low)) (null (cdr low)))
                       (setq low (1+ low)))
                      (t (error "integer type: illegal limit ~a" low)))
                    (cond
                      ((eq '* high) nil)
                      ((integerp high) high)
                      ((and (consp high) (integerp (car high))
                            (null (cdr high)))
                       (setq high (1- high)))
                      (t (error "integer type: illegal limit ~a" high)))
                    `(lisp::integer-internal ,low ,high))
    :superclasses (rational))

(def-built-in SEQUENCE
    :type-expander (() '(or list vector))
    :superclasses ())
(def-built-in LIST
    :type-expander (() '(satisfies listp))
    :superclasses (sequence))
(def-built-in CONS
    :type-expander (() '(satisfies consp))
    :superclasses (list))
(def-built-in SYMBOL
    :type-expander (() '(satisfies symbolp))
    :superclasses ())
(def-built-in NULL
    :type-expander (() '(satisfies null))
    :superclasses (symbol list))

(deftype ATOM () '(satisfies atom))
;;(deftype BIGNUM () '(satisfies rt::bignump))
(deftype BIT () '(integer 0 1))
(deftype COMPILED-FUNCTION () '(satisfies compiled-function-p))
(deftype DOUBLE-FLOAT      () '(satisfies floatp))
;(deftype FIXNUM () `(integer most-negative-fixnum most-positive-fixnum))
(deftype FIXNUM () `(satisfies rt::fixnump)) 
(deftype KEYWORD           () '(satisfies keywordp))
(deftype LONG-FLOAT        () '(satisfies floatp))
(deftype MOD              (n) `(integer 0 (,n)))
;;       NIL                  --> wird gesondert behandelt
(deftype SHORT-FLOAT       () '(satisfies floatp))

(deftype SIGNED-BYTE (&optional s)
 (cond
    ((eq s '*) 'integer)
    ((and (integerp s) (plusp s))
     `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s)))))
    (t (error "signed-byte type: illegal bit count ~s" s))))

(deftype SIMPLE-ARRAY (&optional type size)
  (cond
    ((eq '* type) nil)
    ((symbolp type) nil)
    (t (error "array type: illegal type ~a" type)))
  (cond
    ((eq '* size) nil)
    ((and (integerp size) (plusp size)) (setq size (list size)))
    ((and (listp size)
          (every #'(lambda (x)
                     (or (eq '* x) (and (integerp x) (plusp x)))) size))
     nil)
    (t (error "array type: illegal size ~a" size)))
   `(lisp::simple-array-internal ,type ,size))

(deftype SIMPLE-BIT-VECTOR (&optional size) `(simple-array bit (,size)))
(deftype SIMPLE-STRING (&optional size) `(simple-array standard-char (,size)))
(deftype SIMPLE-VECTOR (&optional size) `(simple-array t (,size)))
(deftype SINGLE-FLOAT      () '(satisfies floatp))
(deftype STANDARD-CHAR     () '(satisfies standard-char-p))
;;       T                    --> wird gesondert behandelt
(deftype UNSIGNED-BYTE (&optional s)
  (cond
    ((eq s '*) '(integer 0))
    ((and (integerp s) (plusp s)) `(integer 0 ,(1- (expt 2 s))))
    (t (error "unsigned-byte type: illegal bit count ~s" s))))

;;------------------------------------------------------------------------------
;; Setf Definitionen
;;------------------------------------------------------------------------------
(defsetf SYMBOL-VALUE SET)

(load "stream")                         ; vor allen anderen fuer IO
(load "array")
(load "char")
(load "clos")
(load "coerce")
(load "file")
(load "pathname")
(load "filesys")
(load "map")
(load "misc")
(load "num")
(load "struct")                         ; vor read fuer struct-reader
                                        ; und vor packg
(load "packg")                          ; vor read + sym

(setq *package* (rt:ensure-package "LISP")) ; vor allsyms: (export t)
(import '(nil))

(load "pred")
(load "hash")                           ; nach struct
(load "read")
(load "print")
(load "format")                         ; nach print
(load "error")                          ; nach print
(load "seq")
(load "list")                           ; nach seq
(load "string")
(load "equal")
(load "sym")
(load "typspc")
(load "yesno")
(load "foreign")
(load "environ")

(load "startup")

;; Die folgende Datei braucht nur geladen zu werden, wenn das Laufzeitsystem
;; sicherstellen muss, dass alle 973 externen Symbole im Lisp Package
;; vorhanden sind.
;;----------------
(load "allsyms")
