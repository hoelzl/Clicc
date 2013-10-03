;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Repraesentation des Typverbandes, Implementation der 
;;;            Typverbandsoperatoren und Deklaration der Elemente
;;;            des Typverbandes
;;;
;;; $Revision: 1.51 $
;;; $Log: titypes.lisp,v $
;;; Revision 1.51  1994/01/27  19:22:52  kl
;;; Typverband erweitert.
;;;
;;; Revision 1.50  1994/01/26  19:15:07  kl
;;; Typbenennungen für non-null-symbol eingeführt.
;;;
;;; Revision 1.49  1993/12/18  06:16:43  hk
;;; In dem Makro declare-zs-type: defparameter statt defconstant, um
;;; Probleme mit make-load-form zu vermeiden
;;;
;;; Revision 1.48  1993/12/09  10:29:58  hk
;;; provide wieder an das Dateiende
;;;
;;; Revision 1.47  1993/12/03  12:46:23  jh
;;; Funktion types-are-disjoined eingefuegt.
;;;
;;; Revision 1.46  1993/11/12  14:24:44  kl
;;; Neue Typbenennungen für package-t, stream-t, u. a. eingeführt.
;;;
;;; Revision 1.45  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.44  1993/06/07  11:07:57  kl
;;; Ausgabe der Listentypen geringfuegig umgestellt.
;;;
;;; Revision 1.43  1993/05/23  15:58:01  kl
;;; Typverband verkleinert und nicht verwendete Typbenennungen entfernt.
;;;
;;; Revision 1.42  1993/05/18  16:14:02  kl
;;; Typen der Typinferenz werden jetzt durch Strukturen repraesentiert.
;;;
;;; Revision 1.41  1993/05/15  13:40:23  kl
;;; Unnoetige Funktionen entfernt und Ausgabe des Typs string-t geaendert.
;;;
;;; Revision 1.40  1993/05/14  15:43:01  kl
;;; Zwei weitere not-Typen eingefuehrt.
;;;
;;; Revision 1.39  1993/04/19  12:23:08  kl
;;; Ausgabe der Typen an den geaenderten Verband angepasst.
;;;
;;; Revision 1.38  1993/04/14  14:59:52  kl
;;; Weitere Umbennungen durchgefuehrt.
;;;
;;; Revision 1.37  1993/04/14  14:46:13  kl
;;; package-t entfernt und Typen umbenannt.
;;;
;;; Revision 1.36  1993/02/17  07:46:27  kl
;;; Inline-Deklaration fuer (das Makro) not-type entfernt.
;;;
;;; Revision 1.35  1993/02/16  16:09:22  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.34  1993/02/02  09:28:25  kl
;;; Makros zs-typecase und update-type-f hierhin verlegt.
;;;
;;; Revision 1.33  1993/01/24  16:38:45  kl
;;; Die Const-funs werden nicht mehr definiert.
;;;
;;; Revision 1.32  1993/01/21  12:05:29  kl
;;; Typen werden wieder durch Integers repraesentiert. Typverband aufgeraumt.
;;;
;;; Revision 1.31  1993/01/19  12:00:03  kl
;;; Typkonstruktor list-of eingefuehrt. Typen werden jetzt durch Instanzen
;;; der Klassen zs-type repraesentiert.
;;;
;;; Revision 1.30  1993/01/17  16:06:14  kl
;;; ratio-t und complex-t und die dazu gehoerenden Vereinigungstypen entfernt.
;;; Dafuer other-t eingefuehrt.
;;;
;;; Revision 1.29  1993/01/15  18:26:10  kl
;;; Reihenfolge der Definitionen und Deklarationen geaendert.
;;; Praedikat primitive-type-p eingefuehrt.
;;;
;;; Revision 1.28  1993/01/08  19:18:39  kl
;;; Typ cons in list-cons und non-list-cons aufgetrennt.
;;;
;;; Revision 1.27  1993/01/06  13:11:29  kl
;;; multiple-type-join entfernt und Kommentare angepasst.
;;;
;;; Revision 1.26  1993/01/02  17:06:21  kl
;;; Fehler behoben und truth-t in bool-t umbenannt.
;;;
;;; Revision 1.25  1992/12/31  12:36:47  kl
;;; Neuen Typen t-t und truth-t eingefuegt.
;;;
;;; Revision 1.24  1992/12/28  16:53:32  kl
;;; output-type verbessert.
;;;
;;; Revision 1.23  1992/12/21  09:01:57  kl
;;; Typverband verfeinert. Umstellung der Repraesentation auf Fixnums.
;;;
;;; Revision 1.22  1992/12/16  09:21:18  kl
;;; Zurueck zur Mengenrepraesentation der Typen. Type-eq verbessert.
;;;
;;; Revision 1.21  1992/12/10  10:13:41  kl
;;; Umstellung der Repraesentation des Typverbands auf simple-bit-vectors.
;;;
;;; Revision 1.20  1992/12/08  14:13:54  kl
;;; Typverband um die Typen byte-t und non-byte-fix-t erweitert.
;;;
;;; Revision 1.19  1992/12/02  13:26:32  kl
;;; Funktion output-type aus timain.lisp hierhin verlegt.
;;;
;;; Revision 1.18  1992/12/02  09:31:23  kl
;;; Typverband um den Typ non-nil-symbol-t erweitert.
;;;
;;; Revision 1.17  1992/12/01  10:15:38  kl
;;; Einige Makros in inline-deklarierte Funktionen ueberfuehrt.
;;;
;;; Revision 1.16  1992/12/01  10:09:39  kl
;;; type-meet eingefuegt und Kommentierung erweitert.
;;;
;;; Revision 1.15  1992/11/26  11:08:33  kl
;;; Neuen Typ atom-t eingefuegt.
;;;
;;; Revision 1.14  1992/11/26  07:13:12  kl
;;; zs-type-of voruebergehend nach tipass1.lisp verlegt. list-type verbessert.
;;;
;;; Revision 1.13  1992/11/24  15:26:54  kl
;;; list-type als Listentypkonstruktor implementiert.
;;;
;;; Revision 1.12  1992/11/20  09:07:59  kl
;;; Makros not-type und type-eq eingefuehrt.
;;;
;;; Revision 1.11  1992/11/17  13:56:31  kl
;;; Makros und Funktionen umbenannt und verbessert. Kommentare angepasst.
;;;
;;; Revision 1.10  1992/11/04  14:04:11  kl
;;; Kommentare verbessert.
;;;
;;; Revision 1.9  1992/11/04  13:22:30  kl
;;; Kommentare verbessert und erweitert. Internal-error eingebunden.
;;;
;;; Revision 1.8  1992/11/02  12:15:00  kl
;;; Schreibfehler korrigiert.
;;;
;;; Revision 1.7  1992/11/02  08:12:14  kl
;;; Dokumentation erweitert und zs-type-of verallgemeinert.
;;;
;;; Revision 1.6  1992/10/30  09:43:34  kl
;;; Typverband umgestellt. Datei an CMU- und Lucid-Lisp angepasst.
;;;
;;; Revision 1.5  1992/10/27  12:07:57  kl
;;; Typverband auf Vereinigungstypen umgestellt. Operationen beschleunigt.
;;;
;;; Revision 1.4  1992/10/15  19:16:42  kl
;;; Verbandsoperationen beschleunigt.
;;;
;;; Revision 1.3  1992/10/02  14:25:15  kl
;;; Funktionen types-are-... eingefuegt.
;;;
;;; Revision 1.2  1992/10/01  17:00:16  kl
;;; Typverbandsoperationen als generische Funktionen implementiert.
;;;
;;; Revision 1.1  1992/09/25  16:41:51  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Repraesentation des Typverbandes:
;;
;; Der in der APPLY-Typinferenz verwendete Typverband ist wie folgt aufgebaut:
;; Es gibt eine Menge P von primitiven Typen {p1, ..., pn}. Die Elemente der
;; Potenzmenge von P, das sind Mengen von primitiven Typen, heissen atomare
;; Typen.
;; Durch Anwendung von Typkonstruktoren auf die atomare Typen entsteht die
;; Menge Z der zusammengesetzten Typen. Die Menge der APPLY-Typen ist die 
;; Potenzmenge der Vereinigung der Mengen von primitiven und zusammengesetzten
;; Typen, also die Potenzmenge von (P U Z).
;; Die APPLY-Typen sind also Mengen von primitiven und zusammengesetzten Typen.
;; Eine solche Menge t1 = {p1, ..., pk, z1, ..., zl} hat als Bedeutung, dass 
;; ein Datum mit Typ t1 entweder Typ p1, ..., pk, z1, ..., oder Typ zl hat.
;;
;; Der einzige zur Zeit verwendete Typkonstruktor ist `list-cons-of'.
;;
;; Zur einfachen Handhabung sind alle primitiven, sowie einige atomare und
;; zusammengesetzte Typen benannt. Benannt sind in der Regel Typentsprechungen
;; zu Common Lisp-Typen.
;; Die in der APPLY-Typinferenz verwendeten Typen mit Typbenennung haben zur
;; Unterscheidung von Common Lisp-Typen alle einen Typnamen mit der Endung "-t".
;; So hat z. B. der alle Zahlen umfassende APPLY-Typ den Namen "number-t".
;;
;; Die APPLY-Typen sind Mengen von primitiven und zusammengesetzten Typen.
;; Diese Mengen werden durch kurze Bit-Vektoren (integers) repraesentiert.
;; 
;; Das Bottom-Element des Typverbandes ist als 0 repraesentiert. In der
;; Repraesentationen des Top-Elementes sind alle Bits gesetzt.
;;
;; Die benannten Typen werden beim Laden dieser Datei in der unten angegebenen
;; Art und Weise und Reihenfolge `deklariert'. Zu jedem deklarierten Typ gibt
;; es eine Konstante, die die Typrepraesentation des zugehoerigen Typs enthaelt.
;;
;; Beispiel:
;; Die Konstante mit Namen "null-t" repraesentiert den Typ `null' und
;; hat den Wert 1. Die Konstante mit Namen "t-t" repraesentiert den Typ zum 
;; Element `T' und hat den Wert 2. Die Konstante mit Namen "other-symbol-t" 
;; repraesentiert den Typ aller von `NIL' und `T' verschiedener Symbole und 
;; hat den Wert 4.
;; Der Vereinigungstyp (or null-t t-t other-symbol-t) ist mit "symbol-t"
;; benannt. Er ist die Typentsprechung zum Common Lisp-Typ `symbol'. 
;; Die Konstante mit Namen "symbol-t" hat den Wert 7.
;;------------------------------------------------------------------------------


(defstruct (ti-type (:print-function (lambda (type stream depth)
                                       (declare (ignore depth))
                                       (format stream "~S" 
                                               (output-type type)))))
  (bt 0 :type fixnum)
  (lt 0 :type fixnum))

;;------------------------------------------------------------------------------
;; Inline-Deklarationen zu den Typverbandsoperationen:
;;------------------------------------------------------------------------------
(proclaim
 '(inline 
   type-join multiple-type-join type-meet zs-subtypep 
   type-eq is-bottom-t types-are-conform
   list-cons-of list-of list-component
   simple-type-p))


;;------------------------------------------------------------------------------
;; Deklaration der Elemente des Typverbands:
;;
;; `Typdeklaration' des Typs t1 mit Namen n1 bedeutet hier, dass eine Konstante
;; mit Namen n1 definiert wird, deren Wert die Typrepraesentation von t1 ist.
;;
;; Zuerst werden der Bottom-Typ und die primitiven Typen deklariert. Dann 
;; werden einige atomare Typen benannt. Zuletzt wird zu einigen primitiven und 
;; atomaren Typen der entsprechende NOT-Typ deklariert.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Makro zum Deklarieren eines im Typverband der Typinferenz verwendeten Typs.
;; Hier könnte defconstant stehen, dann müßte man jedoch eine Methode für
;; make-load-form schreiben und das ist in vielen Lisps noch nicht (richtig)
;; definiert
;;------------------------------------------------------------------------------
(defmacro declare-zs-type (type-symbol type)
  `(defparameter ,type-symbol ,type))


;;------------------------------------------------------------------------------
;; Deklaration der Typen:
;;------------------------------------------------------------------------------
(declare-zs-type bottom-t              (make-ti-type :bt 0))

(declare-zs-type null-t                (make-ti-type :bt (ash 2 -1)))
(declare-zs-type t-symbol-t            (make-ti-type :bt (ash 2 0)))
(declare-zs-type other-symbol-t        (make-ti-type :bt (ash 2 1)))

(declare-zs-type list-cons-of-bottom-t (make-ti-type :bt (ash 2 2)))
(declare-zs-type non-list-cons-t       (make-ti-type :bt (ash 2 3)))

(declare-zs-type byte-t                (make-ti-type :bt (ash 2 4)))
(declare-zs-type non-byte-word-t       (make-ti-type :bt (ash 2 5))) 
(declare-zs-type non-word-fixnum-t     (make-ti-type :bt (ash 2 6))) 
(declare-zs-type bignum-t              (make-ti-type :bt (ash 2 7))) 
(declare-zs-type float-t               (make-ti-type :bt (ash 2 8)))

(declare-zs-type character-t           (make-ti-type :bt (ash 2 9)))

(declare-zs-type string-t              (make-ti-type :bt (ash 2 10)))
(declare-zs-type non-string-vector-t   (make-ti-type :bt (ash 2 11)))
(declare-zs-type non-vector-array-t    (make-ti-type :bt (ash 2 12)))

(declare-zs-type function-t            (make-ti-type :bt (ash 2 13)))
(declare-zs-type structure-t           (make-ti-type :bt (ash 2 14)))
(declare-zs-type class-t               (make-ti-type :bt (ash 2 15)))

(declare-zs-type package-t             (make-ti-type :bt (ash 2 16)))
(declare-zs-type stream-t              (make-ti-type :bt (ash 2 17)))
(declare-zs-type hash-table-t          (make-ti-type :bt (ash 2 18)))
(declare-zs-type readtable-t           (make-ti-type :bt (ash 2 19)))
(declare-zs-type pathname-t            (make-ti-type :bt (ash 2 20)))
(declare-zs-type random-state-t        (make-ti-type :bt (ash 2 21)))

;;------------------------------------------------------------------------------
(defconstant     bits-used             23)
(defconstant     top-number            (1- (ash 2 (1- bits-used))))

;;------------------------------------------------------------------------------
(declare-zs-type  list-component-top-t  (make-ti-type :lt top-number))
(declare-zs-type  list-cons-t           (make-ti-type 
                                         :bt (ti-type-bt list-cons-of-bottom-t)
                                         :lt top-number))



;;------------------------------------------------------------------------------
;; Implementation der Typverbandsoperationen:
;;
;; Die Verbandsoperationen Vereinigung, Schnitt, die Untertyprelation und die
;; Typgleichheit sind durch die entsprechenden Bit-Operation implementiert.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Liefert die kleinste obere Schranke der uebergebenen Typen.
;;------------------------------------------------------------------------------
(defun type-join (type1 type2)
  (make-ti-type :bt (logior (ti-type-bt type1)
                            (ti-type-bt type2))
                :lt (logior (ti-type-lt type1)
                            (ti-type-lt type2))))


(defun multiple-type-join (&rest types)
  (let ((result bottom-t))
    (dolist (type types result)
      (setf result (type-join result type)))))


;;------------------------------------------------------------------------------
;; Liefert die groesste untere Schranke der uebergebenen Typen.
;;------------------------------------------------------------------------------
(defun type-meet (type1 type2)
  (make-ti-type :bt (logand (ti-type-bt type1)
                            (ti-type-bt type2))
                :lt (logand (ti-type-lt type1)
                            (ti-type-lt type2))))

;;------------------------------------------------------------------------------
;; Untertyprelation im Typverband:
;;------------------------------------------------------------------------------
(defun zs-subtypep (type1 type2)
  (and (zerop (logandc2 (ti-type-bt type1)
                        (ti-type-bt type2)))
       (zerop (logandc2 (ti-type-lt type1)
                        (ti-type-lt type2)))))


;;------------------------------------------------------------------------------
;; Test auf Typgleichheit zweier Typen:
;;------------------------------------------------------------------------------
(defun type-eq (type1 type2)
  (and (eql (ti-type-bt type1)
            (ti-type-bt type2))
       (eql (ti-type-lt type1)
            (ti-type-lt type2))))


;;------------------------------------------------------------------------------
;; Testet, ob `type' das Bottom-Element des Typverbands ist.
;;------------------------------------------------------------------------------
(defun is-bottom-t (type)
  (and (zerop (ti-type-bt type))
       (zerop (ti-type-lt type))))


;;------------------------------------------------------------------------------
;; Prueft, ob zwei Typen disjunkt sind.
;;------------------------------------------------------------------------------
(defun types-are-disjoined (type1 type2)
  (is-bottom-t (type-meet type1 type2)))


;;------------------------------------------------------------------------------
;; Liefert, ob zwei Typen konform sind, d. h. ob der eine Typ Untertyp des 
;; anderen sein kann.
;;------------------------------------------------------------------------------
(defun types-are-conform (type1 type2)
  (not (types-are-disjoined type1 type2)))


;;------------------------------------------------------------------------------
;; Makros zur Deklaration einiger benannter Typen:
;;------------------------------------------------------------------------------
(defmacro declare-joined-type (join-symbol &rest types)
  `(declare-zs-type ,join-symbol (multiple-type-join ,@types)))


;;------------------------------------------------------------------------------
;; Makros zur Deklaration der NOT-Typen:
;;------------------------------------------------------------------------------
(defmacro declare-not-type (type)
  `(declare-zs-type ,(intern-prefixed "NOT-" type) (not-type ,type)))


(defmacro declare-not-types (types)
  `(progn . ,(mapcar #'(lambda (type) `(declare-not-type ,type)) types)))



;;------------------------------------------------------------------------------
;; Deklaration einiger benannter Vereinigungstypen:
;;------------------------------------------------------------------------------
(declare-joined-type word-t         byte-t       non-byte-word-t)
(declare-joined-type fixnum-t       word-t       non-word-fixnum-t)
(declare-joined-type integer-t      fixnum-t     bignum-t)
(declare-joined-type number-t       integer-t    float-t)

(declare-joined-type bool-t         null-t       t-symbol-t)
(declare-joined-type symbol-t       bool-t       other-symbol-t)
(declare-joined-type non-null-sym-t t-symbol-t   other-symbol-t)
(declare-joined-type list-t         null-t       list-cons-t)
(declare-joined-type cons-t         list-cons-t  non-list-cons-t)
(declare-joined-type all-list-t     list-t       cons-t)

(declare-joined-type vector-t       string-t     non-string-vector-t)
(declare-joined-type array-t        vector-t     non-vector-array-t)

(declare-joined-type sequence-t     all-list-t   array-t)


;;------------------------------------------------------------------------------
;; Das Top-Element des Typverbands ist die Typvereinigung aller Typen.
;;------------------------------------------------------------------------------
(declare-zs-type top-t (multiple-type-join symbol-t
                                           number-t
                                           sequence-t 
                                           character-t
                                           function-t
                                           structure-t 
                                           class-t
                                           package-t
                                           stream-t
                                           hash-table-t
                                           readtable-t
                                           pathname-t
                                           random-state-t))


;;------------------------------------------------------------------------------
;; Abbildung von Typen auf den zugeordneten NOT-Typ.
;;------------------------------------------------------------------------------
(defmacro not-type (type)
  `(make-ti-type :bt (logxor top-number (ti-type-bt ,type))
                 :lt (logxor top-number (ti-type-lt ,type))))


;;------------------------------------------------------------------------------
;; Deklaration der NOT-Typen:
;;------------------------------------------------------------------------------
(declare-not-types (null-t list-t
                    byte-t word-t fixnum-t bignum-t integer-t float-t number-t
                    symbol-t non-null-sym-t
                    character-t string-t vector-t array-t
                    function-t structure-t class-t
                    package-t stream-t hash-table-t readtable-t pathname-t
                    random-state-t))


;;------------------------------------------------------------------------------
;; Zusaetzliche Benennungen:
;;------------------------------------------------------------------------------
(declare-zs-type     atom-t               (not-type cons-t))
(declare-joined-type list-of-bottom-t     null-t list-cons-of-bottom-t)
(declare-joined-type or-symbol-string-t   symbol-t string-t)
(declare-joined-type or-sym-str-char-t    symbol-t string-t character-t)
(declare-joined-type or-null-character-t  null-t   character-t)
(declare-joined-type package-name-t       or-symbol-string-t)
(declare-joined-type package-or-name-t    package-t package-name-t)
(declare-joined-type my-stream-t          null-t symbol-t stream-t)
(declare-joined-type file-t               string-t pathname-t stream-t)



;;------------------------------------------------------------------------------
;; Weitere Funktion und Makros zum Typverband:
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Liefert den Listencons-Typ, der durch Anwendung des Typkonstruktors 
;; `list-cons-of' auf die uebergebenen Typen entsteht. 
;;------------------------------------------------------------------------------
(defun list-cons-of (&rest types)
  (if types
      (make-ti-type :bt (ti-type-bt list-cons-of-bottom-t)
                    :lt (ti-type-bt (apply #'multiple-type-join types)))
      null-t))
  

;;------------------------------------------------------------------------------
;; Liefert den Listentyp, der durch Anwendung des Typkonstruktors `list-of' auf
;; die uebergebenen Typen entsteht. 
;;------------------------------------------------------------------------------
(defun list-of (&rest types)
  (type-join null-t (apply #'list-cons-of types)))


;;------------------------------------------------------------------------------
;; Diese Abbildung liefert zu einem Typ list-of(A) den Typ A.
;;------------------------------------------------------------------------------
(defun list-component (type)
  (let ((result (make-ti-type :bt (ti-type-lt type))))
    (if (zs-subtypep list-cons-of-bottom-t result)
        (type-join result list-component-top-t)
        result)))


;;------------------------------------------------------------------------------
;; Menge der primitiven Typen.
;;------------------------------------------------------------------------------
(defconstant primitive-types (list bottom-t 
                                   byte-t non-byte-word-t non-word-fixnum-t
                                   fixnum-t bignum-t
                                   float-t
                                   null-t
                                   t-symbol-t
                                   other-symbol-t 
                                   list-cons-t 
                                   non-list-cons-t
                                   character-t
                                   string-t 
                                   non-string-vector-t 
                                   non-vector-array-t
                                   function-t
                                   structure-t
                                   class-t
                                   package-t
                                   stream-t
                                   hash-table-t
                                   readtable-t
                                   pathname-t
                                   random-state-t))


;;------------------------------------------------------------------------------
;; Einfache Typen sind die primitiven Typen und die Listentypen von prim. Typen.
;;------------------------------------------------------------------------------
(defconstant simple-types (append primitive-types 
                                 (mapcar #'list-cons-of primitive-types)))


;;------------------------------------------------------------------------------
;; Liefert, ob `type' ein einfacher Typ ist.
;;------------------------------------------------------------------------------
(defun simple-type-p (type)
  (member type simple-types :test #'type-eq))


;;------------------------------------------------------------------------------
;; Makrodefinition fuer ein typecase ueber die zs-Typen. Als Tag des
;; zs-typecase koennen <zs-Typ1>, <(conform <zs-Typ2>)>, <T> und <otherwise> 
;; verwendet werden, die erfuellt werden, wenn sas Typdatum Untertyp von 
;; <zs-Typ1> ist, zu <zs-Typ2> konform ist, bzw. immer erfuellt ist.
;;------------------------------------------------------------------------------
(defmacro zs-typecase (form &body body)
  `(let ((comp ,form))
    (cond ,@(mapcar #'(lambda (tag-forms-pair)
                        (let ((tag   (first tag-forms-pair))
                              (forms (rest  tag-forms-pair)))
                          (cond ((member tag '(T otherwise)) 
                                 `(T ,@forms))
                                ((and (consp tag) (eq 'conform (first tag)))
                                 `((types-are-conform comp ,(second tag))
                                   ,@forms))
                                (T
                                 `((zs-subtypep comp ,tag) ,@forms)))))
                    body))))


;;------------------------------------------------------------------------------
;; Aktualisiert den Typ des Feldes `field' durch Typvereinigung mit `type'.
;;------------------------------------------------------------------------------
(defmacro update-type-f (field type)
  `(setf ,field (type-join ,field ,type)))


;;------------------------------------------------------------------------------
;; Abbildung von Lisp-Typbezeichnern auf die in der Typinferenz verwendeten
;; Typen. Diese Funktion wird z. B. bei der Ermittlung des Resulttatstyps
;; der Funktion `concatenate' verwendet.
;;------------------------------------------------------------------------------
(defun get-intern-type (type-specifier)
  (cond ((sym-p type-specifier)
         (case (?symbol type-specifier)
           (string    string-t)
           (vector    vector-t)
           (array     array-t)
           (list      all-list-t)
           (sequence  sequence-t)
           (fixnum    fixnum-t)
           (integer   integer-t)
           (float     float-t)
           (number    number-t)
           (cons      cons-t)
           (symbol    symbol-t)
           (character character-t)
           (otherwise top-t)))
        (T top-t)))
  
                
;;------------------------------------------------------------------------------
;; Abbildung von Typrepraesentationen auf Typdarstellungen.
;; Fuer die in der Liste `pairs' angegebenen Typen findet diese Funktion eine
;; abkuerzende Schreibweise bei Typausgaben. Bei zusammengesetzten Typen wird
;; der Typkonstruktor explizit dargestellt.
;;------------------------------------------------------------------------------
(defun output-type (type &optional (with-list-decomposition T))
  (let ((pairs `((,top-t               top)
                 
                 ;; NOT-Typen:
                 (,not-null-t          not-null)
                 (,atom-t              atom-t)
                 (,not-list-t          not-list)
                 (,not-byte-t          not-byte)
                 (,not-word-t          not-word)
                 (,not-fixnum-t        not-fixnum)
                 (,not-bignum-t        not-bignum)
                 (,not-float-t         not-float)
                 (,not-integer-t       not-integer)
                 (,not-number-t        not-number)
                 (,not-symbol-t        not-symbol)
                 (,not-character-t     not-character)
                 (,not-array-t         not-array)
                 (,not-vector-t        not-vector)
                 (,not-function-t      not-function)
                 (,not-structure-t     not-structure)
                 (,not-string-t        not-string)
                 (,not-class-t         not-class)
                 (,not-package-t       not-package)
                 (,not-stream-t        not-stream)
                 
                 ;; Zusammengesetzte Typen:
                 (,sequence-t          sequence)
                 (,all-list-t          list)
                 (,list-of-bottom-t    list-of-bottom)
                 (,array-t             array)
                 (,vector-t            vector)
                 (,cons-t              cons)
                 (,symbol-t            symbol)
                 (,bool-t              bool)
                 (,number-t            number)
                 (,integer-t           integer)
                 (,fixnum-t            fixnum)
                 (,word-t              word)

                 ;; Primitive Typen:
                 (,byte-t              byte)
                 (,non-byte-word-t     non-byte-word)
                 (,bignum-t            bignum)
                 (,float-t             float)
                 (,null-t              null)
                 (,t-symbol-t          t-symbol)
                 (,other-symbol-t      other-symbol)
                 (,list-cons-of-bottom-t list-cons)
                 (,non-list-cons-t     non-list-cons)
                 (,string-t            string)
                 (,non-string-vector-t non-string-vector)
                 (,non-vector-array-t  non-vector-array)
                 (,character-t         character)
                 (,function-t          function)
                 (,structure-t         structure)
                 (,class-t             class)
                 (,package-t           package)
                 (,stream-t            stream)
                 (,hash-table-t        hash-table)
                 (,readtable-t         readtable)
                 (,pathname-t          pathname)
                 (,random-state-t      random-state)
                 )))

    (if (type-eq bottom-t type)
        'bottom
        (do (or-type-list
             (rest-pairs pairs (rest rest-pairs)))
            ((endp rest-pairs) (if (endp (rest or-type-list))
                                   (first or-type-list)
                                   (cons 'or or-type-list)))
          (let ((subtype        (first  (first rest-pairs)))
                (subtype-symbol (second (first rest-pairs))))
            
            (case subtype-symbol
              (list-cons 
               (setf subtype-symbol 
                     (if with-list-decomposition
                         `(list-cons-of ,(output-type (list-component type) 
                                                      nil))
                         'list-cons)))
              (list-of-bottom
               (setf subtype-symbol 
                     (if with-list-decomposition
                         `(list-of ,(output-type (list-component type) nil))
                         'list)))
              (otherwise nil))
          
            (when (zs-subtypep subtype type)
              (push subtype-symbol or-type-list)
              (setf type (type-meet type (not-type subtype)))))))))

;;------------------------------------------------------------------------------
(provide "titypes")


