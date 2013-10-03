;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung
;;;            Generierung von konstanten Daten
;;;            - Arrays
;;;            - Strukturen
;;;            - Listen
;;;            - Characters
;;;            - Floats
;;;            - Fixnums
;;;            - Symbole
;;;            - Global Closures
;;;            - Klassen
;;;
;;; $Revision: 1.59 $
;;; $Log: cgconst.lisp,v $
;;; Revision 1.59  1994/05/22  15:03:31  sma
;;; Die neue globale Variable *OBREP* regelt die Art der
;;; Datenrepräsentation. Sie ist auf 1 für die bisherige Datenrepräsenatation
;;; gesetzt. Die Anpassungen beziehen sich auf die Größe von CONS-Zellen
;;; und die Codeerzeugung für FIXNUMs und FLOATs.
;;;
;;; Revision 1.58  1994/04/29  10:59:32  hk
;;; Der exported Slot eines Symbols kann auch den Wert 'inline haben. Nur
;;; aus dem Package exportieren, wenn der Wert T vorliegt.
;;;
;;; Revision 1.57  1994/04/28  10:12:56  sma
;;; Und noch ein Fehler in C-string beseitigt (es fehlte das 3. Argument).
;;; Sorry.
;;;
;;; Revision 1.56  1994/04/28  10:07:16  sma
;;; Auskommentiere Programmteile gelöscht; falsche Kommentar-Bilder
;;; gelöscht; C-const-t und CC-const-t gelöscht; C-integer, C-float und
;;; C-character geändert, das 3. Argument stacktop generiert wird; statt
;;; CL_INIT2(x) wird wieder `CL_INIT x =' erzeugt; Erzeugung von
;;; statischen konstanten String und Floats abstrahiert.
;;;
;;; Revision 1.55  1994/01/26  13:35:28  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.54  1994/01/21  16:49:38  ft
;;; Behelfskorrektur an *SECRET-UNBOUND-SLOT-VALUE*.
;;;
;;; Revision 1.53  1994/01/21  08:20:06  ft
;;; Änderung der Zwischensprachrepr. des Werts unbound für Slots.
;;;
;;; Revision 1.52  1994/01/21  08:13:30  sma
;;; Erneute Änderung der Symbolrepräsentation (letzte Änderung war keine
;;; so gute Idee)
;;;
;;; Revision 1.51  1994/01/13  16:34:56  sma
;;; Änderung der Symbol-Repräsentation.
;;;
;;; Revision 1.50  1993/12/09  14:05:35  sma
;;; Verwendet jetzt MAKE_CONSREF statt MAKE_LIST.
;;;
;;; Revision 1.49  1993/11/30  08:39:00  ft
;;; Verarbeitung von Slotbeschreibungen korrigiert.
;;;
;;; Revision 1.48  1993/10/29  15:01:04  sma
;;; (C-CL_INIT-Decl ...) statt (C-ArrayInitDecl "CL_INIT" ...)
;;;
;;; Revision 1.47  1993/09/06  16:51:49  sma
;;; Statt direkt CL-INIT-Strukturen zu erzeugen werden MAKE_* Makros
;;; generiert. Siehe für deren Implementierung obrep?.h.
;;;
;;; Revision 1.46  1993/08/26  09:32:42  hk
;;; C-const-string verändert, daß keine Problem mehr mit
;;; CALL-ARGUMENTS-LIMIT auftreten. CCC-character kann jetzt auch \ooo
;;; Zeichen und Umlaute ausgeben.
;;;
;;; Revision 1.45  1993/08/20  10:41:44  hk
;;; :element-type eingefuegt bei make-array mit :displaced-to.
;;;
;;; Revision 1.44  1993/07/20  18:20:19  pm
;;; ?named-constant-base in named-const-base umbenannt
;;;
;;; Revision 1.43  1993/07/20  15:44:33  hk
;;; cg-named-constants und calc-named-const-base definiert, (cg-form
;;; float-form) traegt nur die Adresse ein, wenn diese bei der Bearbeitung
;;; einer named-const schon gesetzt wurde.
;;;
;;; Revision 1.42  1993/07/19  15:55:17  hk
;;; big-float nicht mehr notwendig, da float-form nun einen Slot adr hat
;;;
;;; Revision 1.41  1993/07/19  11:25:31  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.40  1993/07/19  11:24:24  hk
;;; Initialisierung von fixnum-array-name und float-array-name verschoben.
;;;
;;; Revision 1.39  1993/07/19  11:20:14  hk
;;; Fehler in cg-floats und cg-fixnums behoben: or -> and
;;;
;;; Revision 1.38  1993/07/13  11:08:16  uho
;;; Bei Instruktionen, die Simple-Strings ansprechen, wird der String in
;;; einem Kommentar zur Instruktion genannt ('cg-form (simple-literal)').
;;; In 'calc-symbol-base' wird nun die Hilfsfunktion 'calc-C-name'
;;; aufgerufen, die einen gueltigen C-Identifkator generiert.
;;;
;;; Revision 1.37  1993/07/09  15:29:19  hk
;;; Keine Unterdrueckung von Warnings ueber uninterned Symbols
;;; im Lisp Modul.
;;;
;;; Revision 1.36  1993/07/07  15:49:56  uho
;;; CALC-SYMBOL-BASE wandelt den Modulnamen jetzt in einen gueltigen
;;; C-Namen um.
;;;
;;; Revision 1.35  1993/06/19  22:17:47  hk
;;; Keine Warnings ueber uninterned Symbols im Lisp Modul.
;;;
;;; Revision 1.34  1993/06/17  12:47:44  ft
;;; Codeerzeugung für Klassen ohne Slots korrigiert.
;;;
;;; Revision 1.33  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.32  1993/05/22  10:35:20  ft
;;; Erweiterung um Codeerzeugung für globale Funktionen in konstanten Daten.
;;;
;;; Revision 1.31  1993/05/08  18:46:52  hk
;;; (cg-form structured-literal) korrigiert, Methoden fuer sym, class-def,
;;; simple-literal nach hier.
;;;
;;; Revision 1.30  1993/04/22  11:13:20  hk
;;; Anpassung an die neue Definition von SYMVAL und SYMBOL.
;;; Fehler behoben: uninterned Syms haben im Package Slot :uninterned,
;;; nicht nil. Symbol-Array bekommt den Namen S<module name>. Das Symbol NIL
;;; ist schon in Ssys definiert.
;;;
;;; Revision 1.29  1993/03/25  10:11:43  ft
;;; Codeerzeugung fuer Klassen korrigiert.
;;;
;;; Revision 1.28  1993/03/22  17:31:49  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.27  1993/03/18  14:53:00  ft
;;; gen-literals erzeugt jetzt code fuer ungebundene Slots.
;;;
;;; Revision 1.26  1993/03/17  15:54:15  hk
;;; Aufruf von add-comment in CC-special eingefuegt. Aufrufe von
;;; add-comment an die Aenderung der Definition (auto Space) angepasst.
;;;
;;; Revision 1.25  1993/03/12  09:39:56  ft
;;; Codeerzeugung fuer die Klassen.
;;;
;;; Revision 1.24  1993/02/16  16:08:02  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.23  1993/01/28  15:25:26  uho
;;; Generierung von Kommentaren fuer die Tabelle der Symbole
;;;
;;; Revision 1.22  1993/01/26  13:52:38  uho
;;; Fuer LOAD_SYMBOL erscheint nun der Namen des Symbols als Kommentar
;;;
;;; Revision 1.21  1993/01/26  08:45:24  sma
;;; integer overflow extra behandelt
;;;
;;; Revision 1.20  1992/10/02  14:07:22  hk
;;; Fehlerbehandlung jetzt lokal
;;;
;;; Revision 1.19  1992/09/29  21:12:37  hk
;;; Message 'generating Symbols' entfernt.
;;;
;;; Revision 1.18  1992/09/25  17:27:25  kl
;;; Umstellung auf die neue Repraesentation der einfachen Literale.
;;; Zusaetzlich einige Fehlermeldungen eingefuegt bzw. verbessert.
;;;
;;; Revision 1.17  1992/09/24  08:49:24  hk
;;; Schreibfehler.
;;;
;;; Revision 1.16  1992/09/24  08:27:57  hk
;;; Das Symbol Nil wird extra generiert, da es nicht mehr in ?sym-list von
;;; *module* enthalten ist.
;;;
;;; Revision 1.15  1992/09/23  08:29:05  hk
;;; Schreibfehler
;;;
;;; Revision 1.14  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.13  1992/08/11  12:37:38  hk
;;; C-Ln --> C-Decl, falls Variablen deklariert werden.
;;;
;;; Revision 1.12  1992/07/28  10:43:51  hk
;;; Paramterliste von cg-gen-symbols ist nun (), Schreibfehler beseitigt.
;;;
;;; Revision 1.11  1992/07/23  12:43:19  hk
;;; Schreibfehler.
;;;
;;; Revision 1.10  1992/07/22  12:58:52  hk
;;; Fehlermeldung von cg-package-cell geaendert.
;;;
;;; Revision 1.9  1992/07/07  15:33:15  hk
;;; Forwaertsdeklaration auf static Variablen mittels "extern".
;;;
;;; Revision 1.8  1992/07/02  15:23:12  hk
;;; Inhalt von class in literal-instance ist zur Zeit sym und nicht symbol.
;;;
;;; Revision 1.7  1992/06/11  11:17:02  hk
;;; cg-error -> error.
;;;
;;; Revision 1.6  1992/06/10  16:43:32  hk
;;; In Fehlermeldung von cg-package-cell Symbol angeben, nicht sym.
;;;
;;; Revision 1.5  1992/06/05  11:17:38  hk
;;; Argument von CC-special ist vom Typ dynamic und nicht sym.
;;;
;;; Revision 1.4  1992/06/05  11:15:42  hk
;;; *** empty log message ***
;;;
;;; Revision 1.3  1992/06/04  12:39:10  hk
;;; ?const -> ?constant-value
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;
;;; 05.02.91 hk
;;; Konstanten werden nicht mehr global in 'const_forms', sondern
;;; direkt vor der jeweiligen globalen Funktion angelegt.
;;;
;;; 05.08.91 hk
;;; Konstante Strings und Floats werden direkt im Code und
;;; nicht mehr dem Array 'const_forms' abgelegt.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")     

;;------------------------------------------------------------------------------
;; Datenrepräsentationsverfahren (Objekt-Repräsentation)
;;------------------------------------------------------------------------------
(defvar *OBREP* 1)                      ; 1, 2 oder 3 möglich
(defconstant min-smallfixnum -1000)     ; Bereich der vordefinierten 
(defconstant max-smallfixnum  1000)     ; Konstanten bei Obrep = 2

;;------------------------------------------------------------------------------
;; Code für den Zugriff auf `Literale' generieren
;;------------------------------------------------------------------------------
(defmethod cg-form ((form sym))
  (case *result-spec*
    ((nil))
    (C-BOOL (setq *C-bool* C-TRUE))
    (t (C-symbol form (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((form class-def))
  (case *result-spec*
    ((nil))
    (C-BOOL (setq *C-bool* C-TRUE))
    (t (C-class form (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((form simple-literal))  
  (case *result-spec*
    ((nil))
    (C-BOOL (setq *C-bool* (CC-bool (not (null-form-p form)))))
    (t (let ((dest (CC-dest *result-spec*)))
         (etypecase form
           (null-form      (C-nil dest))
           (int            (C-integer   (?value form) dest))
           (character-form (C-character (?value form) dest))
           (float-form     (C-float form dest)))))))

;;------------------------------------------------------------------------------
(defmethod cg-form ((obj structured-literal))
  (case *result-spec*
    ((nil))
    (C-BOOL (setq *C-bool* C-TRUE))
    (t (C-MacroCall (CC-NameConc "LOAD_" 
                                 (typecase (?value obj)
                                   (cons "CONS")
                                   (string (add-comment (?value obj)) "SMSTR")
                                   (vector "SMVEC_T")
                                   (array "SMAR_T")
                                   (literal-instance "STRUCT")
                                   (T (error "internal (cg-form), ~S~
                                         has unknown type" (?value obj)))))
                    (CC-cast "CL_FORM *" (?adr obj))
                    (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
;; Erzeugt CL_INIT-Array mit konstanten Daten
;;------------------------------------------------------------------------------
(defun gen-literals (literals name-postfix)
  (let (array-name
        float-array-name
        fixnum-array-name
        float-table
        fixnum-table)
    
    (labels
        ((gen-const-vector (elt adr)
           (let ((new-adr adr))
             (dotimes (i (length elt))
               (setq new-adr (gen-immed (aref elt i) new-adr))))
           (let ((new-adr adr))
             (dotimes (i (length elt))
               (setq new-adr (gen-const (aref elt i) new-adr)))
             new-adr))
         
         (gen-const (elt adr)
           (typecase elt
             
             (cons
              (add-comment (format nil "CONS(~A)" adr))
              (case *OBREP*
                (1 (incf adr 2))
                ((2 3) (C-init (CC-MacroCall "MAKE_CONS"))
                 (incf adr 3)))
              (let ((new-adr (gen-immed (car elt) adr)))
                (gen-immed (cdr elt) new-adr))
              (let ((new-adr (gen-const (car elt) adr)))
                (gen-const (cdr elt) new-adr)))
             
             (string
              (C-const-string elt)
              (+ 2 adr))
             
             (vector
              (add-comment (format nil "VECTOR(~A)" adr))
              (incf adr (1+ (length elt)))
              (C-init (CC-MacroCall "MAKE_VECTOR" (length elt)))
              (gen-const-vector elt adr))
             
             (array
              (let ((vector (make-array (array-total-size elt)
                                        :displaced-to elt
                                        :element-type
                                        (array-element-type elt))))
                ;; Array-Header
                ;;-------------
                (C-init (CC-MacroCall "MAKE_ARRAY" 
                                      (array-rank elt)
                                      (CC-arrayptr fixnum-array-name
                                                   (length fixnum-table))
                                      (length vector)))
                (dolist (dim (array-dimensions elt))
                  (vector-push-extend dim fixnum-table))
                (gen-const-vector vector (1+ adr))))
             
             (literal-instance
              (let* ((list (?value-list elt))
                     (len (length list)))
                (incf adr (+ 2 len))
                (C-init (CC-MacroCall "MAKE_STRUCT" (1+ len)
                                      (CC-const-symbol (?class elt))))
                (gen-const-vector list adr)))
             
             (t adr)))
         
         (gen-immed (elt adr)
           (typecase elt
             
             ;; Die einfachen Literale:
             ;;------------------------
             (null-form      (C-const-nil) adr)
             (character-form (C-const-character (?value elt)) adr)
             (int            (C-const-fixnum (?value elt)) adr)
             (float-form     (C-const-float (float-address (?value elt))) adr)
             
             ;; Die Typen, die innerhalb strukturierter Literale auftreten:
             ;;------------------------------------------------------------
             (null           (C-const-nil) adr)
             (character      (C-const-character elt) adr)
             (sym            (C-const-symbol    elt) adr)
             (integer        (C-const-fixnum    elt) adr)
             (float          (C-const-float (float-address elt)) adr)
             (class-def      (C-const-class elt) adr)
             (global-fun     (C-const-fun elt) adr)
             (T (cond
                  ((eq elt :unbound) (C-const-unbound) adr)
                  (T (C-init (CC-MacroCall 
                              (cg-type-spec elt)
                              (CC-arrayptr array-name
                                           (if (is-array elt) (1+ adr) adr))))
                     (+ adr (sizeof elt)))))))
         
         (sizeof (elt)
           (typecase elt
             (cons (+ (sizeof (car elt)) (sizeof (cdr elt))
                      (case *OBREP* (1 2) ((2 3) 3))))
             (string 2)
             (vector
              (let ((size (1+ (length elt))))
                (dotimes (i (length elt))
                  (incf size (sizeof (aref elt i))))
                size))
             (literal-instance
              (1+ (length (?value-list elt))))
             (array
              (let ((vector (make-array (array-total-size elt)
                                        :displaced-to elt
                                        :element-type
                                        (array-element-type elt))))
                (1+ (sizeof vector))))
             (T 0)))
         
         (float-address (float)
           (let ((pos (position float float-table)))
             (CC-arrayptr float-array-name
                          (if pos pos (vector-push-extend float float-table)))))
         
         (cg-floats ()
           (when (and float-table (> (length float-table) 0))
             (C-StaticArrayInitDecl "double" float-array-name)
             (C-initstart)
             (map nil 
                  #'(lambda (float) 
                      (C-init float))
                  float-table)
             (C-initend)))
         
         (cg-fixnums ()
           (when (and fixnum-table (> (length fixnum-table) 0))
             (C-StaticArrayInitDecl "long" fixnum-array-name)
             (C-initstart)
             (map nil 
                  #'(lambda (fixnum) 
                      (C-init (CC-fixnum fixnum)))
                  fixnum-table)
             (C-initend))))
    
      ;; Start der Funktion...
      ;;----------------------
      (when literals
        
        (setq array-name (CC-NameConc "K" name-postfix))

        (C-empty-Ln)
        (when (dolist (const literals nil)
                (when (and (structured-literal-p const)
                           (?needs-fixnum-array const))
                  (return t)))
          (setq fixnum-table (make-array 1 :fill-pointer 0 :adjustable t))
          (setq fixnum-array-name (CC-NameConc "X" name-postfix))
          (C-ExternArrayDecl "long" fixnum-array-name))
        
        (when (dolist (const literals nil)
                (when (or (float-form-p const)
                          (?needs-float-array const))
                  (return t)))
          (setq float-table (make-array 1 :fill-pointer 0 :adjustable t))
          (setq float-array-name (CC-NameConc "O" name-postfix))
          (C-ExternArrayDecl "double" float-array-name))

        (when (= *OBREP* 2) (init-extra-tables name-postfix))
        
        (C-CL_INIT-Decl array-name)
        (C-initstart)
        (let ((adr 0) (i 0))
          (dolist (struct literals)
            (add-comment (format nil "~a" adr))
            (incf i 2)
            (etypecase struct
              (float-form (setf (?adr struct) (float-address (?value struct))))
              (structured-literal
               (let ((elt (?value struct)))
                 (setf (?adr struct)
                       (CC-arrayptr array-name
                                    (if (is-array elt) (1+ adr) adr)))
                 (setq adr (gen-const elt adr)))))))
        (C-initend)
      
        (cg-floats)
        (cg-fixnums)
        (when (= *OBREP* 2) (cg-extra-tables))))))

;;------------------------------------------------------------------------------
(defun C-CL_INIT-Decl (name)
  (C-ArrayInitDecl "CL_INIT" name))

(defun C-Static-CL_INIT-Decl (name)
  (C-StaticArrayInitDecl "CL_INIT" name))

;;------------------------------------------------------------------------------
(defun is-array (elt) (and (arrayp elt) (not (vectorp elt))))

;;------------------------------------------------------------------------------
(defun cg-type-spec (elt)
  (typecase elt
    (cons "MAKE_CONSREF")
    (string "MAKE_STRREF")
    (vector "MAKE_VECREF")
    (array "MAKE_ARREF")
    (literal-instance "MAKE_STRUCTREF")
    (class-def "MAKE_CLASSREF")
    (T (error "internal (cg-type-spec), ~S has unknown type" elt))))
  
;;------------------------------------------------------------------------------
;; initialisiert den naechsten Eintrag in 'const_forms' mit NIL, ...
;;------------------------------------------------------------------------------
(defun C-const-nil ()                  
  (C-init (CC-const-nil)))

;;------------------------------------------------------------------------------
(defun CC-const-nil ()
  "MAKE_NIL")

;;------------------------------------------------------------------------------
(defun C-const-unbound ()
  (C-init (CC-const-unbound)))

;;------------------------------------------------------------------------------
(defun CC-const-unbound ()
  "MAKE_UNBOUND")

;;------------------------------------------------------------------------------
(defun C-const-fixnum (i)
  (C-init (CC-const-fixnum i)))

;;------------------------------------------------------------------------------
(defun CC-const-fixnum (i)
  (case *OBREP*
    ((1 3) (CC-MacroCall "MAKE_FIXNUM" (CC-fixnum i)))
    (2 (CC-MacroCall "MAKE_FIXNUMREF" (extrafixnum i)))))

;;------------------------------------------------------------------------------
;; Umgeht die Warnung "integer-overflow" des acc bei -2^31
;;------------------------------------------------------------------------------
(defun CC-fixnum (i)
  (if (eql i C-most-negative-fixnum)
      (format nil "(~A) - 1" (incf i))
      (format nil "~A" i)))

;;------------------------------------------------------------------------------
(defun C-const-float (f)
  (C-init (CC-const-float f)))

(defun CC-const-float (f)
  (case *OBREP*
    ((1 3) (CC-MacroCall "MAKE_FLOAT" f))
    (2 (CC-MacroCall "MAKE_FLOATREF" (extrafloat f)))))

;;------------------------------------------------------------------------------
(defun C-const-character (c)
  (C-init (CC-const-character c)))

(defun CC-const-character (c)
  (CC-MacroCall "MAKE_CHAR" (CC-character c)))

;;------------------------------------------------------------------------------
(defun C-const-symbol (sym)
  (C-init (CC-const-symbol sym)))

(defun CC-const-symbol (sym)
  (CC-MacroCall "MAKE_SYMREF" (CC-symbol sym)))

;;------------------------------------------------------------------------------
(defun C-const-string (string)
  (C-init 
   (CC-MacroCall "MAKE_STRING"
                 (length string)
                 (CC-const-string string))))

;;------------------------------------------------------------------------------
(defun CC-const-string (string)
  (labels
      ((char-ok-for-C (c)
         (and (graphic-char-p c)
              (not (find c "'\"\\")))))
    
    (let ((new-string "\""))
      (loop
       (when (= 0 (length string))
         (return (concatenate 'string new-string "\"")))
       (let ((p (position-if-not #'char-ok-for-C string)))
         (cond
           (p
            (setq new-string
                  (concatenate 'string
                               new-string
                               (subseq string 0 p)
                               (CCC-character (char string p))))
            (setq string (subseq string (1+ p))))
           (t
            (setq new-string (concatenate 'string
                                          new-string string))
            (setq string ""))))))))

;;------------------------------------------------------------------------------
(defun C-float (float dest)
  (if (slot-boundp float 'adr)
      (C-MacroCall "LOAD_FLOAT" (CC-stack *stack-top*) (?adr float)   dest)
      (C-MacroCall "GEN_FLOAT"  (CC-stack *stack-top*) (?value float) dest)))

;;------------------------------------------------------------------------------
(defun C-string (string dest)
  (C-MacroCall "GEN_SMSTR" 
               (length string) 
               (CC-const-string string)
               dest))

;;------------------------------------------------------------------------------
(defun CC-special (dynamic)
  (let ((sym (?sym dynamic)))
    (add-comment (string (?symbol sym)))
    (CC-MacroCall "SYMVAL" (?base sym) (?adr sym))))

;;------------------------------------------------------------------------------
(defun CC-symbol (sym)
  (add-comment (string (?symbol sym)))
  (CC-MacroCall "SYMBOL" (?base sym) (?adr sym)))

;;------------------------------------------------------------------------------
(defun C-symbol (sym dest)
  (C-MacroCall "LOAD_SYMBOL" (CC-symbol sym) dest))

;;------------------------------------------------------------------------------
(defun C-nil (dest)
  (C-MacroCall "LOAD_NIL" dest))

;;------------------------------------------------------------------------------
(defun C-t (dest)
  (C-MacroCall "LOAD_T" dest))

;;------------------------------------------------------------------------------
(defun C-integer (i dest)
  (if (and (= *OBREP* 2) (integerp i) (< min-smallfixnum i max-smallfixnum))
      (C-MacroCall "LOAD_SMALLFIXNUM" i dest)
      (C-MacroCall "LOAD_FIXNUM" (CC-stack *stack-top*) (CC-fixnum i) dest)))

;;------------------------------------------------------------------------------
(defun CCC-character (c)
  (case c
    (#\newline "\\n")
    (#\backspace "\\b")
    (#\tab "\\t")
    (#\page "\\f")
    (#\return "\\r")
    (#\rubout "\\177")
    (#\\ "\\\\")
    (#\" "\\\"")
    (#\' "\\'")
    (T (cond ((graphic-char-p c) (string c))
             (T (let ((code (char-code c))
                      (*print-base* 8))
                  ;; Ausgabe in oktal, 3 Zeichen breit, mit führenden Nullen
                  ;;--------------------------------------------------------
                  (concatenate 'string "\\" (format nil "~3,'0o" code))))))))

;;------------------------------------------------------------------------------
(defun CC-character (c)
  (concatenate 'string "'" (CCC-character c) "'"))

;;------------------------------------------------------------------------------
(defun C-character (c dest)
  (C-MacroCall "LOAD_CHAR" (CC-stack *stack-top*) (CC-character c) dest))

;;------------------------------------------------------------------------------
(defun C-const-class (class)
  (C-init (CC-MacroCall "MAKE_CLASSREF" (CC-class class))))

;;------------------------------------------------------------------------------
(defun CC-class (class)
  (add-comment (string (?symbol (?symbol class))))
  (CC-MacroCall "CLASS" (?adr class)))

;;------------------------------------------------------------------------------
(defun C-class (class dest)
    (C-MacroCall "LOAD_CLASS" (CC-class class) dest))

;;------------------------------------------------------------------------------
(defun C-const-fun (fun)
  (C-init (CC-MacroCall "MAKE_GLOBFUN" 
                        (CC-Address (CC-NameConc "C" (?adr fun))))))

;;------------------------------------------------------------------------------
;; Berechnet den Namen des Arrays, in dem die Symbole eines Moduls alloziert
;; werden
;;------------------------------------------------------------------------------
(defun calc-symbol-base (module-name)
  (calc-C-name "S" module-name))

;;------------------------------------------------------------------------------
;; Berechnet den Namen des Arrays, in dem die named Constants eines Moduls
;; alloziert werden
;;------------------------------------------------------------------------------
(defun calc-named-const-base (module-name)
  (calc-C-name "C" module-name))

;;------------------------------------------------------------------------------
;; (?package-list *module*) enthaelt eine Liste aller Packages,
;; die das uebersetzte Programm benutzt.
;;------------------------------------------------------------------------------
;; vorlaeufige Package-Beschreibung erzeugen:
;; NIL: uninterned Symbol
;;  0 : Symbol in Keyword-Package
;;  n : internal Symbol
;; -n : external Symbol
;; (wobei n der Index des Packages ist in dem zur Laufzeit erzeugten
;;  Array *package-list*)
;;------------------------------------------------------------------------------
(defun cg-package-cell (sym)
  (let ((packg (?package sym))
        (exported (?exported sym)))
    (if (eq :uninterned packg)
        (CC-const-nil)
        (let ((i (position packg (?package-list *module*) :test #'string=)))
          (if i
              (CC-const-fixnum (if (eq exported T) (- i) i))
              (progn
                (clicc-warning
                 "~a will be uninterned, package ~a is unknown"
                 (?symbol sym) packg)
                (CC-const-nil)))))))
  
;;------------------------------------------------------------------------------
;; Ein Array mit allen Symbolen des Moduls angelegen und intialisieren.
;;------------------------------------------------------------------------------
(defun cg-gen-symbols ()
  (C-empty-Ln)
  
  ;; Fuer alle Symbole, die als Konstanten deklariert sind,
  ;; grosse konstante Werte anlegen.
  ;;--------------------------------------------------
  (gen-literals
   (mapcan #'(lambda (sym)
               (let ((value (?constant-value sym)))
                 (when (constant-value-p sym)
                   (typecase value
                     ((or float-form structured-literal) (list value))
                     (T nil)))))
           (?sym-list *module*))
   (?symbol-base *module*))
  
  (when (= *OBREP* 2) (init-extra-tables (?symbol-base *module*)))

  (C-empty-Ln)
  (C-CL_INIT-Decl (?symbol-base *module*))
  (C-initstart)

  (let ((i 0))
    (dolist (sym (?sym-list *module*))
      ;; Kommentar ist Index in das Array
      ;; --------------------------------
      (add-comment (format nil "SYMBOL(~A)" i))
      (C-Newline)
      
      ;; Symbol
      ;;-------
      (C-init (CC-MacroCall
               (if (eq (?constant-value sym) :no-const)
                   "MAKE_SYMBOL"
                   "MAKE_CONST_SYMBOL")
               ;; Name
               ;;-----
               (length (?name sym))
               (CC-const-string (?name sym))

               ;; Wert Zelle
               ;;-----------
               (let ((value (?constant-value sym)))
                 (if (constant-value-p sym)
                     (typecase value
                       (null-form
                        (CC-const-nil))
                       (int 
                        (CC-const-fixnum (?value value)))
                       (character-form 
                        (CC-const-character (?value value)))
                       (sym 
                        (CC-const-symbol value))
                       (float-form
                        (CC-const-float (?adr value)))
                       (structured-literal
                        (CC-MacroCall (cg-type-spec (?value value)) 
                                      (?adr value)))
                       (t (error "constant ~a not implemented" value)))
                     ;; else
                     ;;-----
                     (CC-const-unbound)))
               ;; Package
               ;;--------
               (cg-package-cell sym)))
      
      (incf i)))
  
  ;; Das letzte Symbol wird durch eine NULL-Form abgeschlossen. Diese
  ;; Eigenschaft wird vom Garbage-Collector und der Funktion
  ;; setup_symbols_iterator ausgenutzt.
  ;;-------------------------------------------------------------------
  (C-Ln "END_SYMDEF")
  (C-initend)
  (when (= *OBREP* 2) (cg-extra-tables)))

;;------------------------------------------------------------------------------
(defun cg-gen-classes()
  (C-empty-Ln)

  ;; Erzeuge Code fuer die Komponenten einer Klasse
  ;;-----------------------------------------------
  (gen-literals
   (mapcan #'(lambda (class)
               (if (null-form-p (?slot-descr-list class))
                   `(,(?class-precedence-list class))
                   `(,(?class-precedence-list class)
                     ,(?slot-descr-list class))))
           (?class-def-list *module*))
   "classes")

  (C-empty-Ln)
  (C-CL_INIT-Decl "classes")
  (C-initstart)
  (let ((i 0))
    (dolist (class (?class-def-list *module*))

      ;; Kommentar ist Index in das Array
      ;; --------------------------------
      (add-comment (format nil "CLASS(~A)" i))
      (incf i)
      (C-Newline)

      (C-init (CC-MacroCall
               "MAKE_CLASS"
               ;; Name
               ;;-----
               (CC-symbol (?symbol class))

               ;; Class Precedence List
               ;;----------------------
               (CC-MacroCall
                (cg-type-spec (?value (?class-precedence-list class))) 
                (?adr (?class-precedence-list class)))
               
               ;; Number of Slots
               ;;----------------
               (if (null-form-p (?slot-descr-list class))
                   0
                   (length (?value (?slot-descr-list class))))

               ;; Slot Info List
               ;;---------------
               (if (null-form-p (?slot-descr-list class))
                   (CC-const-nil)
                   (CC-MacroCall
                    (cg-type-spec (?value (?slot-descr-list class))) 
                    (?adr (?slot-descr-list class))))))))
  
  (C-initend))

;;------------------------------------------------------------------------------
;; Ein Array mit allen Symbolen des Moduls angelegen und intialisieren.
;;------------------------------------------------------------------------------
(defun cg-gen-named-constants ()
  
  ;; Fuer alle Symbole, die als Konstanten deklariert sind,
  ;; grosse konstante Werte anlegen.
  ;;--------------------------------------------------
  (gen-literals
   (mapcan #'(lambda (const)
               (let ((value (?value const)))
                 (unless (eq :unknown value)
                   (typecase value
                     ((or float-form structured-literal) (list value))
                     (T nil)))))
           (?named-const-list *module*))
   (?named-const-base *module*)))

;;------------------------------------------------------------------------------
;; Die folgenden Funktionen werden nur für OBREP = 2 benötigt und bauen
;; Tabellen mit den zusätzlichen Datenstrukturen für Fixnums und Floats auf
;;------------------------------------------------------------------------------
(defvar extrafixnum-table)
(defvar extrafixnum-name)
(defvar extrafloat-table)
(defvar extrafloat-name)

;;------------------------------------------------------------------------------

(defun init-extra-tables (name-postfix)
  (setq extrafixnum-table (make-array 1 :fill-pointer 0 :adjustable t))
  (setq extrafixnum-name (CC-NameConc "XI" name-postfix))
  (C-ExternArrayDecl "CL_INIT" extrafixnum-name)

  (setq extrafloat-table (make-array 1 :fill-pointer 0 :adjustable t))
  (setq extrafloat-name (CC-NameConc "XF" name-postfix))
  (C-ExternArrayDecl "CL_INIT" extrafloat-name))

(defun cg-extra-tables ()
  (when (and extrafixnum-table (> (length extrafixnum-table) 0))
    (C-StaticArrayInitDecl "CL_INIT" extrafixnum-name)
    (C-initstart)
    (map nil #'(lambda (fixnum) 
                 (C-init (CC-MacroCall "MAKE_FIXNUM" fixnum)))
         extrafixnum-table)
    (C-initend))

  (when (and extrafloat-table (> (length extrafloat-table) 0))
    (C-StaticArrayInitDecl "CL_INIT" extrafloat-name)
    (C-initstart)
    (map nil #'(lambda (float) 
                 (C-init (CC-MacroCall "MAKE_FLOAT" float)))
         extrafloat-table)
    (C-initend))

  (setq extrafloat-table nil extrafixnum-table nil))

(defun extrafixnum (i)
  (if (< min-smallfixnum i max-smallfixnum)
      (CC-arrayptr "fixnum_ob" (- i min-smallfixnum))
      (let ((pos (position i extrafixnum-table)))
        (CC-arrayptr extrafixnum-name
                     (if pos pos 
                         (vector-push-extend i extrafixnum-table))))))

(defun extrafloat (f)
  (let ((pos (position f extrafloat-table)))
    (CC-arrayptr extrafloat-name
                 (* (if pos pos 
                        (vector-push-extend f extrafloat-table)) 2))))

;;------------------------------------------------------------------------------
(provide "cgconst")
