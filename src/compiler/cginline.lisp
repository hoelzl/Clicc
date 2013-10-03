;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung
;;;            Erzeugen von Inline-Code fuer ausgewaehlte Systempraedikate
;;;
;;; $Revision: 1.40 $
;;; $Log: cginline.lisp,v $
;;; Revision 1.40  1994/05/20  08:41:15  hk
;;; Noch ein Fehler in  get-arg-loc behoben.
;;;
;;; Revision 1.39  1994/05/18  15:08:51  sma
;;; EQL-Makro für eql-Funktion eingeführt.
;;;
;;; Revision 1.38  1994/05/18  08:52:56  hk
;;; Fehler in get-arg-loc behoben.
;;;
;;; Revision 1.37  1994/05/06  12:40:39  sma
;;; Fehler in cg-struct-size korrigiert.
;;;
;;; Revision 1.36  1994/04/28  09:59:02  sma
;;; Auskommentiere Funktion entfernt; C-MacroCall "LOAD_FIXNUM" -->
;;; C-integer; cg-cons erzeugt Makroaufruf von ALLOC_CONS statt direkten
;;; Code zur Erzeugung einer CONS-Zelle.
;;;
;;; Revision 1.35  1994/01/27  16:21:17  kl
;;; rt::fixnump eingetragen.
;;;
;;; Revision 1.34  1994/01/27  14:47:51  sma
;;; 1-fix und <fix werden jetzt inline-compiliert.
;;;
;;; Revision 1.33  1994/01/24  16:32:18  sma
;;; rt::struct-type entfernt (jetzt in LISP in struct.lisp implementiert).
;;; Optimierungen für not auskommentiert. Genau Erklärung siehe pred.lisp.
;;;
;;; Revision 1.32  1994/01/22  17:51:22  sma
;;; Fehlende Klammer ergänzt.
;;;
;;; Revision 1.31  1994/01/21  16:50:08  sma
;;; Optimierung! CC-get-int eingeführt. Insbesondere bei instance-ref,
;;; structure-ref und die logischen funktionen bekommen jetzt eine
;;; integer-konstante direkt uebergeben statt sie auf den stack zu
;;; schieben und sofort wieder davon auszulesen.
;;;
;;; Revision 1.30  1994/01/21  13:20:42  sma
;;; Alle für Symbole notwendigen Funktionen des rt::-Packages werden jetzt
;;; inline compiliert. LOAD_BOOL in pred-result eingeführt. Fehler in
;;; instance-set und set-svref-internal korriert. slot-set-unbound
;;; gelöscht.
;;;
;;; Revision 1.29  1993/12/16  16:30:50  pm
;;; FFI-Funktionen vom rt:: Package ins FFI: Package geschoben.
;;;
;;; Revision 1.28  1993/12/14  12:50:25  sma
;;; Namensänderungen durch Einführung von plain-vector-Typ.
;;; rt::%vector-length heißt jetzt rt::plain-vector-length, neue Funktion
;;; rt::plain-vector-p, rt::svref-internal und rt::set-svref-internal.
;;;
;;; Revision 1.27  1993/12/09  14:13:16  sma
;;; Änderungen für neue array-Repräsentation. stringp, vectorp, arrayp
;;; sind in direkt Lisp kodiert, neu/verändert sind simple-bit-vector-p
;;; und simple-vector-p.
;;;
;;; Revision 1.26  1993/11/01  15:25:46  hk
;;; Fehler in get-arg-loc (var-ref) behoben. Im Zusammenspiel mit opt-args
;;; wurden in gewissen Fällen lokale Variablen überschrieben, obwohl sie
;;; später noch benutzt wurden.
;;;
;;; Revision 1.25  1993/10/26  14:58:14  sma
;;; EQ Makro wird benutzt; CAR,CDR Makros eingeführt; mehr Typtest-Makros.
;;;
;;; Revision 1.24  1993/09/28  14:45:55  pm
;;; Die C-Konvertierungsfunktionen werden jetzt inline compileiert
;;;
;;; Revision 1.23  1993/09/10  15:08:28  hk
;;; get-arg-loc spezialisiert über form statt t.
;;;
;;; Revision 1.22  1993/09/10  11:49:54  hk
;;; Fehler in cg-eq behoben, wenn beide Argumente einfache Konstanten
;;; waren.
;;;
;;; Revision 1.21  1993/08/18  15:25:32  ft
;;; Die Funktionen instancep, instance-set, instance-ref und
;;; set-slot-unbound werden jetzt inline compiliert.
;;;
;;; Revision 1.20  1993/07/22  13:00:54  hk
;;; In cg-%car und cg-%cdr GET_FORM durch GET_CAR ersetzt.
;;;
;;; Revision 1.19  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.18  1993/04/06  17:28:30  hk
;;; ?codegen -> ?c-inline.
;;;
;;; Revision 1.17  1993/04/06  17:09:29  hk
;;; shift-right, shift-left -> %shift-right, %shift-left.
;;;
;;; Revision 1.16  1993/02/16  15:50:26  hk
;;; Revision Keyword eingefuegt, Symbole im zu uebersetzenden Programm
;;; werden im clicc-lisp Package angesprochen.
;;;
;;; Revision 1.15  1993/01/13  15:06:31  ft
;;; Erweiterung um Funktionen fuer ash.
;;;
;;; Revision 1.14  1993/01/07  10:00:20  hk
;;; Fehler mit special-sys-fun behoben.
;;;
;;; Revision 1.13  1993/01/07  08:31:04  hk
;;; Fehler in macrolet von cg-special-funs behoben.
;;;
;;; Revision 1.12  1993/01/06  13:03:40  hk
;;; Funktionen {p1,p2,p3,cg}-special-funs vereinheitlicht.
;;;
;;; Revision 1.11  1993/01/06  11:18:53  ft
;;; Erweiterung um logische Operationen auf Zahlen.
;;;
;;; Revision 1.10  1992/12/03  14:53:26  hk
;;; typecase -> etypecase
;;;
;;; Revision 1.9  1992/11/26  16:46:17  hk
;;; Neu cg-%vector-length.
;;;
;;; Revision 1.8  1992/11/26  16:00:13  hk
;;; cg-init von cgmain.lisp -> hier, get-arg geaendert, etc.
;;;
;;; Revision 1.7  1992/11/25  17:51:27  hk
;;; Inline Compilation von %car, %cdr, %rplaca, %rplacd, %cons und
;;; einige Umbenennungen: check-integer-low -> fixnum-low-p ...
;;;
;;; Revision 1.6  1992/10/08  14:04:23  hk
;;; cg-eq: Optimierung nach p2, null-form beachtet, cg-get-arg korrigiert.
;;;
;;; Revision 1.5  1992/09/25  17:24:44  kl
;;; C-eq und C-eql auf die neue Repraesentation der einfachen Literale
;;; umgestellt.
;;;
;;; Revision 1.4  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.3  1992/07/30  10:27:07  hk
;;; .
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
;; Bekanntgeben der Funktionen, die inline kompiliert werden.
;;------------------------------------------------------------------------------
(p0-special-funs
 (?c-inline "CG")
 clicc-lisp::eq
 clicc-lisp::eql
 clicc-lisp::integerp
 rt::fixnump
 rt::fixnum-low-p
 rt::fixnum-high-p
 clicc-lisp::consp
 clicc-lisp::characterp
 rt::plain-vector-p
 clicc-lisp::simple-vector-p
 clicc-lisp::simple-string-p
 clicc-lisp::simple-bit-vector-p
 clicc-lisp::floatp
 clicc-lisp::atom
 clicc-lisp::symbolp
 rt::symp
 clicc-lisp::listp
 clicc-lisp::numberp
 clicc-lisp::functionp
 clicc-lisp::values
 clicc-lisp::cons
 rt::%car
 rt::%cdr
 rt::%rplaca
 rt::%rplacd
 rt::%logior
 rt::%logxor
 rt::%logand
 rt::%lognot
 rt::%shift-right
 rt::%shift-left
 rt::instancep
 rt::instance-ref
 rt::instance-set
 rt::plain-vector-length
 rt::svref-internal
 rt::set-svref-internal
 rt::symbol-value
 rt::symbol-plist
 rt::symbol-package
 (L::setf rt::symbol-value)
 (L::setf rt::symbol-plist)
 (L::setf rt::symbol-package)
 rt::symbol-name
 rt::constant-flag-p
 rt::set-constant-flag
 rt::unbound-value
 rt::unbound-value-p
 rt::structp
 rt::struct-size
 rt::structure-ref
 (L::setf rt::structure-ref)
 rt::<fix
 rt::1-fix

 ffi::c-struct-p
 ffi::c-char-p
 ffi::c-short-p
 ffi::c-int-p
 ffi::c-long-p
 ffi::c-unsigned-char-p
 ffi::c-unsigned-short-p
 ffi::c-unsigned-int-p
 ffi::c-unsigned-long-p
)

;;------------------------------------------------------------------------------
;; Hilfsfunktion, die Code f"ur den Zugriff auf das Resultat eines Ausdrucks
;; generiert. Bei Variablenzugriffen wird ein Kopieren des Werts vermieden.
;;------------------------------------------------------------------------------
(defun CC-get-arg (form)
  (CC-dest (get-arg-loc form)))

;;------------------------------------------------------------------------------
(defmethod get-arg-loc ((form var-ref))
  (etypecase (?var form)
    (dynamic (?var form))
    (static
     (let ((var (?var form)))

       ;; Der Stackframe des Aufrufers wird evtl. gerade wiederverwendet. Um
       ;; sicherzustellen, daß bei nachfolgenden Aufrufen von get-arg-loc
       ;; diese Variable nicht überschrieben wird, mu"s sie ggf. nach unten
       ;; kopiert werden.  Wenn sie bereits an der richtigen Position liegt,
       ;; dann mu"s *stack-top* erh"oht werden, damit sie nicht von
       ;; nachfolgenden Argumenten "uberschrieben wird.
       ;;-----------------------------------------------------
       (cond
        ((eql *level* (?level var))
         (if (> (?offset var) *stack-top*)
             (call-next-method)
             (progn (incf *stack-top*) var)))

        ;; In diesem Fall wird *stack-top* nicht erh"oht, damit keine L"ucken
        ;; im Stack entstehen. Ist das ein Problem f"ur opt-args ???
        ;;----------------------------------------------------------
        (T var))))))
                 
(defmethod get-arg-loc ((form form))
  (let ((*result-spec* (stacktop-result-location)))
    (cg-form form)
    (incf *stack-top*)
    *result-spec*))

;;------------------------------------------------------------------------------
;; Hilfsfunktion, die bei Konstanten vom Typ INT sofort die Konstante
;; zurückliefert und andernfalls einen GET_FIXNUM-Aufruf und den 'üblichen'
;; Code generiert.
;;------------------------------------------------------------------------------
(defun CC-get-int (form)
  (if (int-p form)
      (?value form)
      (CC-MacroCall "GET_FIXNUM" (CC-get-arg form))))

;;------------------------------------------------------------------------------
;; Hilfsfunktion, die bei Bedarf Boolesche C-Werte in die LISP-Werte
;; true / false umwandelt.
;;------------------------------------------------------------------------------
(defun pred-result (pred)
  (case *result-spec*
    ((NIL))
    (C-bool (setq *C-bool* pred))
    (T (C-MacroCall "LOAD_BOOL" pred (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
;; Hilfsfunktion, die das Resultat einer inline kompilierten Funktion an die
;; in *result-spec* angegebene Position kopiert.
;;------------------------------------------------------------------------------
(defun C-result (source)
  (case *result-spec*
    
    ;; Resultat wird nicht benoetigt
    ;;------------------------------
    ((NIL))
    
    ;; Boolesches Resultat gewuenscht
    ;;-------------------------------
    (C-bool (setq *C-bool* (CC-make-bool source)))

    ;; Normales Resultat erzeugen
    ;;---------------------------
    (T (C-copy source (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
;; EQ, inline
;; ACHTUNG: es wird davon ausgegangen, dass Zeiger die groesste Komponente
;; in CL_FORM sind.
;; Optimiert, wenn einer der Parameter konstant ist.
;;------------------------------------------------------------------------------
(defun cg-eq (form1 form2)
  (pred-result
   (let ((*stack-top* *stack-top*)
         (const-arg 0))

     ;; pruefen, welche der Argumente konstant sind
     ;;--------------------------------------------
     (if (or (simple-literal-p form1) (sym-p form1))
         (incf const-arg 1)
         (setq form1 (CC-get-arg form1)))
     (if (or (simple-literal-p form2) (sym-p form2))
         (incf const-arg 2)
         (setq form2 (CC-get-arg form2)))

     (case const-arg

       ;; eines der Argumente ist konstant
       ;;---------------------------------
       ((1 2) (when (= const-arg 2)
                (rotatef form1 form2))
        (typecase form1
          (null-form (getCode "CL_NILP(~A)" form2))
          (int
           (getCode "CL_FIXNUMP(~A) && GET_FIXNUM(~A) == ~A"
                    form2 form2 (?value form1)))
          (character-form
           (getCode "CL_CHARP(~A) && GET_CHAR(~A) == ~A"
                    form2 form2 (CC-character (?value form1))))
          (sym (getCode "CL_SYMBOLP(~A) && GET_SYMBOL(~A) == ~A"
                        form2 form2 (CC-symbol form1)))
          (T "FALSE")))
	      
       ;; beide Argumente sind konstant
       ;;------------------------------
       (3 (CC-bool (cond
                     ((and (simple-literal-p form1) (simple-literal-p form2))
                      (eq (?value form1) (?value form2)))
                     ((and (sym-p form1) (sym-p form2))
                      (eq form1 form2))
                     (T nil))))

       (t (getCode "EQ(~A, ~A)" form1 form2))))))

;;------------------------------------------------------------------------------
;; EQL, inline
;; Optimierung:
;; Wenn eines der Argument vom Typ FLOAT ist, dann direkt vergleichen.
;;------------------------------------------------------------------------------
(defun cg-eql (form1 form2)
  (pred-result
   (let ((*stack-top* *stack-top*) 
         (floatconst 0))
     
     ;; Pruefen, ob FLOAT Konstanten vorliegen
     ;;---------------------------------------
     (if (float-form-p form1)
         (incf floatconst 1)
         (setq form1 (CC-get-arg form1)))
     (if (float-form-p form2)
         (incf floatconst 2)
         (setq form2 (CC-get-arg form2)))

     (case floatconst

       ;; 'Normales' EQL
       ;;---------------
       (0 (getCode "EQL(~A, ~A)" form1 form2))
     
       ;; beide Argumente sind vom Typ FLOAT
       ;;-----------------------------------
       (3 (CC-bool (= (?value form1) (?value form2))))
     
       ;; eines der Argumente ist vom Typ FLOAT
       ;;--------------------------------------
       (T (when (= floatconst 2)
            (rotatef form1 form2))
          (getCode "CL_FLOATP(~A) && GET_FLOAT(~A) == ~A"
                   form2 form2 (?value form1)))))))

;;------------------------------------------------------------------------------
;; Typtests...
;;------------------------------------------------------------------------------
(defun cg-integerp (x-loc)
  (pred-result (getCode "CL_FIXNUMP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-fixnump (x-loc)
  (cg-integerp x-loc))

;;------------------------------------------------------------------------------
(defun cg-fixnum-low-p (val low)
  (setq val (CC-get-arg val))
  (setq low (CC-get-arg low))
  (pred-result
   (getCode "CL_FIXNUMP(~A) && GET_FIXNUM(~A) >= GET_FIXNUM(~A)" val val low)))

;;------------------------------------------------------------------------------
(defun cg-fixnum-high-p (val high)
  (setq val (CC-get-arg val))
  (setq high(CC-get-arg high))
  (pred-result
   (getCode "CL_FIXNUMP(~A) && GET_FIXNUM(~A) <= GET_FIXNUM(~A)" val val high)))


;;------------------------------------------------------------------------------
(defun cg-consp (x-loc)
  (pred-result
   (getCode "CL_CONSP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-characterp (x-loc)
  (pred-result
   (getCode "CL_CHARP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-plain-vector-p (x-loc)
  (pred-result
   (getCode "CL_SMVECP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-simple-vector-p (x-loc)
  (pred-result
   (getCode "CL_SMVEC_T_P(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-simple-string-p (x-loc) 
  (pred-result
   (getCode "CL_SMSTRP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-simple-bit-vector-p (x-loc) 
  (pred-result
   (getCode "CL_SMVEC_BIT_P(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-floatp (x-loc)
  (pred-result
   (getCode "CL_FLOATP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-atom (x-loc)
  (pred-result
   (getCode "CL_ATOMP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-symbolp (x-loc)
  (setq x-loc (CC-get-arg x-loc))
  (pred-result
   (getCode "CL_SYMBOLP(~A) || CL_NILP(~A)" x-loc x-loc)))

;;------------------------------------------------------------------------------
(defun cg-symp (x-loc)
  (pred-result
   (getCode "CL_SYMBOLP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-listp (x-loc)
  (pred-result
   (getCode "CL_LISTP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-numberp (x-loc)
  (pred-result
   (getCode "CL_NUMBERP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-functionp (x-loc)
  (pred-result
   (getCode "CL_FUNCTIONP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
(defun cg-instancep (x-loc)
  (pred-result
   (getCode "CL_INSTANCEP(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
;; FFI
;;------------------------------------------------------------------------------
(defun cg-c-struct-p (x-loc)
  (pred-result
   (getCode "CL_C_STRUCT_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-char-p (x-loc)
  (pred-result
   (getCode "CL_C_CHAR_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-short-p (x-loc)
  (pred-result
   (getCode "CL_C_SHORT_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-int-p (x-loc)
  (pred-result
   (getCode "CL_C_INT_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-long-p (x-loc)
  (pred-result
   (getCode "CL_C_LONG_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-unsigned-char-p (x-loc)
  (pred-result
   (getCode "CL_C_UNSIGNED_CHAR_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-unsigned-short-p (x-loc)
  (pred-result
   (getCode "CL_C_UNSIGNED_SHORT_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-unsigned-int-p (x-loc)
  (pred-result
   (getCode "CL_C_UNSIGNED_INT_P(~A)" (CC-get-arg x-loc))))

(defun cg-c-unsigned-long-p (x-loc)
  (pred-result
   (getCode "CL_C_UNSIGNED_LONG_P(~A)" (CC-get-arg x-loc))))

;;------------------------------------------------------------------------------
;; CAR, CDR, etc.
;;------------------------------------------------------------------------------
(defun cg-%car (cell)
  (C-result (CC-MacroCall "GET_CAR" (CC-get-arg cell))))

;;------------------------------------------------------------------------------
(defun cg-%cdr (cell)
  (C-result (CC-MacroCall "GET_CDR" (CC-get-arg cell))))

;;------------------------------------------------------------------------------
(defun cg-cons (x y)
  (setq x (CC-get-arg x))
  (setq y (CC-get-arg y))
  (case *result-spec*
    ((NIL))
    (C-BOOL (setq *C-bool* C-TRUE))
    (T (C-MacroCall "ALLOC_CONS" (CC-stack *stack-top*) x y
                    (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
(defun cg-%rplaca (x y)
  (setq x (get-arg-loc x))
  (setq y (get-arg-loc y))
  (C-copy (CC-dest y) (CC-MacroCall "GET_CAR" (CC-dest x)))
  (to-result-loc x))

;;------------------------------------------------------------------------------
(defun cg-%rplacd (x y)
  (setq x (get-arg-loc x))
  (setq y (get-arg-loc y))
  (C-copy (CC-dest y) (CC-MacroCall "GET_CDR" (CC-dest x)))
  (to-result-loc x))

;;------------------------------------------------------------------------------
;; Operationen auf FIXNUMs
;;------------------------------------------------------------------------------
(defun cg-%logior (x y)
  (setq x (CC-get-int x))
  (setq y (CC-get-int y))
  (C-integer (CC-opIor x y) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-%logxor (x y)
  (setq x (CC-get-int x))
  (setq y (CC-get-int y))
  (C-integer (CC-op^ x y) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-%logand (x y)
  (setq x (CC-get-int x))
  (setq y (CC-get-int y))
  (C-integer (CC-op& x y) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-%lognot (x)
  (setq x (CC-get-int x))
  (C-integer (CC-op~ x) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-%shift-right (i c)
  (setq i (CC-get-int i))
  (setq c (CC-get-int c))
  (C-integer (CC-op>> i c) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-%shift-left (i c)
  (setq i (CC-get-int i))
  (setq c (CC-get-int c))
  (C-integer (CC-op<< i c) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
;; Instanzen...
;;------------------------------------------------------------------------------
(defun cg-instance-ref (instance offset)
  (setq instance (CC-get-arg instance))
  (setq offset (CC-get-int offset))
  (C-result (CC-MacroCall "OFFSET"
                          (getCode "AR_BASE(GET_FORM(~A))" instance)
                          (CC-op+ offset 1))))

;;------------------------------------------------------------------------------
(defun cg-instance-set (newvalue instance offset)
  (setq newvalue (get-arg-loc newvalue))
  (setq instance (CC-get-arg instance))
  (setq offset (CC-get-int offset))
  (C-copy (CC-dest newvalue)
          (CC-MacroCall "OFFSET"
                        (getCode "AR_BASE(GET_FORM(~A))" instance)
                        (CC-op+ offset 1)))
  (to-result-loc newvalue))

;;------------------------------------------------------------------------------
;; Vektoren...
;;------------------------------------------------------------------------------
(defun cg-plain-vector-length (x)
  (setq x (CC-get-arg x))
  (C-integer (CC-MacroCall "AR_SIZE" (CC-MacroCall "GET_FORM" x))
             (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-svref-internal (vector index)
  (setq vector (CC-get-arg vector))
  (setq index (CC-get-int index))
  (C-result (CC-MacroCall "OFFSET"
                          (getCode "AR_BASE(GET_FORM(~A))" vector) index)))

;;------------------------------------------------------------------------------
(defun cg-set-svref-internal (newvalue vector index)
  (setq newvalue (get-arg-loc newvalue))
  (setq vector (CC-get-arg vector))
  (setq index (CC-get-int index))
  (C-Copy (CC-dest newvalue)
          (CC-MacroCall "OFFSET"
                        (getCode "AR_BASE(GET_FORM(~A))" vector) index))
  (to-result-loc newvalue))

;;------------------------------------------------------------------------------
;; Symbole
;;------------------------------------------------------------------------------
(defun cg-symbol-value (sym)
  (C-result (CC-MacroCall "SYM_VALUE" (CC-get-arg sym))))

(defun cg-set-symbol-value (value sym)
  (setq value (get-arg-loc value))
  (C-Copy (CC-dest value) (CC-MacroCall "SYM_VALUE" (CC-get-arg sym)))
  (to-result-loc value))

(defun cg-symbol-plist (sym)
  (C-result (CC-MacroCall "SYM_PLIST" (CC-get-arg sym))))

(defun cg-set-symbol-plist (value sym)
  (setq value (get-arg-loc value))
  (C-Copy (CC-dest value) (CC-MacroCall "SYM_PLIST" (CC-get-arg sym)))
  (to-result-loc value))

(defun cg-symbol-package (sym)
  (C-result (CC-MacroCall "SYM_PACKAGE" (CC-get-arg sym))))

(defun cg-set-symbol-package (value sym)
  (setq value (get-arg-loc value))
  (C-Copy (CC-dest value) (CC-MacroCall "SYM_PACKAGE" (CC-get-arg sym)))
  (to-result-loc value))

(defun cg-symbol-name (sym)
  (C-MacroCall "LOAD_SMSTR" (CC-MacroCall "SYM_NAME" (CC-get-arg sym))
               (CC-dest *result-spec*)))

(defun cg-constant-flag-p (sym)
  (pred-result
   (getCode "SYM_IS_CONST(~A)" (CC-get-arg sym))))

(defun cg-set-constant-flag (sym)
  (C-MacroCall "SYM_SET_CONST" (CC-get-arg sym)))

(defun cg-unbound-value ()
  (C-MacroCall "LOAD_UNBOUND" (CC-dest *result-spec*)))

(defun cg-unbound-value-p (object)
  (pred-result
   (getCode "CL_UNBOUNDP(~A)" (CC-get-arg object))))

;;------------------------------------------------------------------------------
;; Strukturen
;;------------------------------------------------------------------------------
(defun cg-structp (object)
  (pred-result
   (getCode "CL_STRUCTP(~A)" (CC-get-arg object))))

;;------------------------------------------------------------------------------
(defun cg-struct-size (struct)
  (C-integer (getCode "AR_SIZE(GET_FORM(~A))" (CC-get-arg struct))
             (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------
(defun cg-structure-ref (struct index)
  (C-result (CC-MacroCall "OFFSET" 
                          (getCode "AR_BASE(GET_FORM(~A))" (CC-get-arg struct))
                          (CC-op+ (CC-get-int index) 1))))

;;------------------------------------------------------------------------------
(defun cg-set-structure-ref (newvalue struct index)
  (setq newvalue (get-arg-loc newvalue))
  (C-Copy (CC-dest newvalue)
          (CC-MacroCall "OFFSET"
                        (getCode "AR_BASE(GET_FORM(~A))" (CC-get-arg struct))
                        (CC-op+ (CC-get-int index) 1)))
  (to-result-loc newvalue))

;;------------------------------------------------------------------------------
;; Arithmetische Funktionen
;;------------------------------------------------------------------------------
(defun cg-<fix (a b)
  (setq a (CC-get-int a))
  (setq b (CC-get-int b))
  (pred-result
   (CC-op< a b)))

;;------------------------------------------------------------------------------

(defun cg-1-fix (a)
  (C-integer (CC-op- (CC-get-int a) 1) (CC-dest *result-spec*)))

;;------------------------------------------------------------------------------


(provide "cginline")
