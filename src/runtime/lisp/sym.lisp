;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem
;;;            - GET, SET-GET
;;;            - REMPROP
;;;            - GETF, SETF
;;;            - GET-PROPERTIES
;;;            - COPY-SYMBOL
;;;            - GENSYM, GENTEMP
;;;            - KEYWORDP
;;;
;;; $Revision: 1.8 $
;;; $Log: sym.lisp,v $
;;; Revision 1.8  1994/01/21  13:29:17  sma
;;; make-symbol aufgrund erneuter Änderung der Symbolrepäsentation
;;; verändert. Neuen Typ "rt::sym" eingeführt, der ein not-nil-symbol
;;; repräsentiert. Für diesen kann effizient in cginline ein Typtest
;;; generiert werden. Entsprechende Funktionen umgeschrieben.
;;;
;;; Revision 1.7  1994/01/13  16:46:55  sma
;;; Änderungen an Funktionen für symbols. Mehr Lisp, weniger C.
;;; rt::(set)-struct-ref-internal heißt jetzt rt::structure-ref. Die
;;; set-Variante wird jetzt mit setf definiert.
;;;
;;; Revision 1.6  1994/01/05  12:43:04  sma
;;; Namensänderung: rt::make-symbol-internal -> rt::make-symbol
;;;
;;; Revision 1.5  1993/12/09  17:18:59  sma
;;; rt::make-symbol-internal eingefügt, welches nur simple-strings
;;; akzeptiert. make-symbol ist jetzt eine Lisp-Funktion.
;;;
;;; Revision 1.4  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.3  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.2  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.8 $ eingefuegt
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(get remprop getf get-properties copy-symbol gensym *gensym-counter*
   gentemp keywordp
   symbol-value set symbol-plist symbol-name symbol-package boundp makunbound))

(export '(rt::set-prop rt::remf-internal) "RT")

;;--------------------------------------------------------------------------
; Verhalten, falls die Property-List-Funktionen auf Werte angewandt werden, 
; die keine Property-Listen sind: 
; - Liste mit gerader Anzahl von Elementen, INDICATOR nicht vorhanden,
;   (CDR(LAST pliste)) = Atom != NIL
;     - getf:     default
;     - set-getf: (indicator value ... ... . atom)
;     - remf-internal: (... ... . atom)
; - Liste mit ungerader Anzahl von Elementen, letztes Element = INDICATOR
;     - getf:     NIL
;     - set-getf: Error "RPLACA applied to empty List"
;     - remf-internal: letztes Element wird entfernt
; - Liste mit ungerader Anzahl von Elementen, (CDR (LAST pliste)) = ATOM
;   (CAR (LAST pliste)) = INDICATOR
;     - getf, set-getf, remf-internal: Error "CDR applied to non NIL atom"
;;--------------------------------------------------------------------------

;;--------------------------------------------------------------------------
(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

;;--------------------------------------------------------------------------
(defun (setf get) (value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator) value)
  value)

;;--------------------------------------------------------------------------
(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

;;--------------------------------------------------------------------------
;; Hilfsfunktionen fuer getf, set-getf
;;--------------------------------------------------------------------------
(defun get-prop (list indicator)
  (cond
    ((atom list) nil) 
    ((eq (car list) indicator) list)
    (t (get-prop (cddr list) indicator))))

;;--------------------------------------------------------------------------
;; Resultat: veraenderte / erweiterte Pliste
;;--------------------------------------------------------------------------
(defun rt:set-prop (plist indicator value)
  (let ((list (get-prop plist indicator)))
    (cond
      (list
       (rplaca (cdr list) value)
       plist)
      (t (cons indicator (cons value plist))))))

;;--------------------------------------------------------------------------
(defun getf (place indicator &optional default)
  (let ((list (get-prop place indicator)))
    (if list
      (cadr list)
      default)))

;;--------------------------------------------------------------------------
;; (remf place :test) -->
;; (multiple-value-bind (plist found)
;;     (remf-internal x :test)
;;   (if found
;;     (progn (setf place plist) t)
;;     nil))
;; Resultate
;; 1. verkleinerte Liste oder NIL
;; 2. Wert gibt an, ob Indicator gefunden wurde
;;--------------------------------------------------------------------------
(defun rt:remf-internal (list indicator)
  (cond
    ((atom list) (values nil nil))
    ((eq (car list) indicator) (values (cddr list) t))
    (t (do ((list1 list (cddr list1))
            (list2 (cddr list) (cddr list2)))
           ((atom list2) (values nil nil)) ;end test
         (when (eq (car list2) indicator)
           (rplacd (cdr list1) (cddr list2))
           (return-from rt:remf-internal (values list t)))))))
 
;;--------------------------------------------------------------------------
(defun get-properties (list indicator-list)
  (cond
    ((atom list) (values nil nil nil))
    ((member (car list) indicator-list) (values (car list) (cadr list) list))
    (t (get-properties (cddr list) indicator-list)))) 

;;------------------------------------------------------------------------------
;; make-symbol print-name
;;------------------------------------------------------------------------------
(defun make-symbol (print-name)
  (let ((sym (rt::make-symbol (string-to-simple-string print-name))))
    (setf (rt::symbol-plist sym) nil)
    (setf (rt::symbol-package sym) nil)
    (makunbound sym)))              ;liefert sym zurück

;;--------------------------------------------------------------------------
;; copy-symbol sym &optional copy-props
;;--------------------------------------------------------------------------
(defun copy-symbol (sym &optional copy-props)
  (let ((new-sym (make-symbol (symbol-name sym))))
    (when copy-props
      (when (boundp sym)
        (set new-sym (symbol-value sym)))
      (setf (symbol-plist new-sym) (copy-list (symbol-plist sym))))
    new-sym))

;;--------------------------------------------------------------------------
;; gensym &optional x
;;--------------------------------------------------------------------------
(defparameter *gensym-prefix* "G")
(defparameter *gensym-counter*  0)

(defun gensym (&optional x)
  (typecase x
    ((null))
    ((integer 0 *) (setq *gensym-counter* x))
    (string (setq *gensym-prefix* x))
    (otherwise (error "positive integer or string expected")))
  (prog1
      (make-symbol
       (concatenate 'string *gensym-prefix*
                    (prin1-to-string *gensym-counter*)))
    (incf *gensym-counter*)))

;;--------------------------------------------------------------------------
;; gentemp &optional (prefix "T") (package *package*)
;;--------------------------------------------------------------------------
(defparameter *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  (unless (stringp prefix)
    (error "string expected"))
  (loop
    (incf *gentemp-counter*)
    
    (let ((name (concatenate 'string
                             prefix
                             (prin1-to-string *gentemp-counter*))))
      (multiple-value-bind (sym exists)
          (intern name package) 
        (unless exists (return sym))))))
                           
;;--------------------------------------------------------------------------
;; keywordp object
;;--------------------------------------------------------------------------
(defun keywordp (object)
  (and (symbolp object) (eq (symbol-package object) *keyword-package*)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
(defconstant SYM_EXPECTED "~a is not a symbol")
(defconstant TRY_CHANGE_CONST "cannot change value of ~a, which is a constant")

(deftype rt::sym () '(satisfies rt::symp))
(defvar *nil-plist*)
(defvar *nil-package*)

;;------------------------------------------------------------------------------
;; SYMBOL-VALUE symbol
;;------------------------------------------------------------------------------
(defun symbol-value (sym)
  (typecase sym
    (rt::sym (rt::symbol-value sym))
    (null nil)
    (T (error SYM_EXPECTED sym))))

;;------------------------------------------------------------------------------
;; SET sym value
;;------------------------------------------------------------------------------
(defun set (sym value)
  (typecase sym
    (rt::sym (if (rt::constant-flag-p sym)
                 (error TRY_CHANGE_CONST sym)
                 (setf (rt::symbol-value sym) value)))
    (null (error TRY_CHANGE_CONST sym))
    (T (error SYM_EXPECTED sym))))

;;------------------------------------------------------------------------------
;; SYMBOL-PLIST symbol
;;------------------------------------------------------------------------------
(defun symbol-plist (sym)
  (typecase sym
    (rt::sym (rt::symbol-plist sym))
    (null *nil-plist*)
    (T (error SYM_EXPECTED sym))))

;;------------------------------------------------------------------------------
;; (SETF SYMBOL-PLIST) value sym
;;------------------------------------------------------------------------------
(defun (setf symbol-plist) (value sym)
  (typecase sym
    (rt::sym (setf (rt::symbol-plist sym) value))
    (null (setf *nil-plist* value))
    (T (error SYM_EXPECTED sym))))

;;------------------------------------------------------------------------------
;; SYMBOL-NAME symbol
;;------------------------------------------------------------------------------
(defun symbol-name (sym)
  (typecase sym
    (rt::sym (rt::symbol-name sym))
    (null "NIL")
    (T (error SYM_EXPECTED sym))))
  
;;------------------------------------------------------------------------------
;; SYMBOL-PACKAGE symbol
;;------------------------------------------------------------------------------
(defun symbol-package (sym)
  (typecase sym
    (rt::sym (rt::symbol-package sym))
    (null *nil-package*)
    (T (error SYM_EXPECTED sym))))

;;------------------------------------------------------------------------------
;; SYMBOL-PACKAGE-INDEX symbol
;;------------------------------------------------------------------------------
(defun symbol-package-index (sym)
  (typecase sym
    (rt::sym (rt::symbol-package sym))
    (null *nil-package*)
    (T nil)))

;;------------------------------------------------------------------------------
;; SET-SYMBOL-PACKAGE symbol package
;;------------------------------------------------------------------------------
(defun set-symbol-package (sym package)
  (typecase sym
    (rt::sym (setf (rt::symbol-package sym) package))
    (null (setf *nil-package* package))))

;;------------------------------------------------------------------------------
;; BOUNDP object
;;------------------------------------------------------------------------------
(defun boundp (object)
  (typecase object
    (rt::sym (not (rt::unbound-value-p (rt::symbol-value object))))
    (null T)
    (T (error SYM_EXPECTED object))))

;;------------------------------------------------------------------------------
;; MAKUNBOUND sym
;;------------------------------------------------------------------------------
(defun makunbound (sym)
  (typecase sym
    (rt::sym (if (rt::constant-flag-p sym)
                 (error TRY_CHANGE_CONST sym)
                 (setf (rt::symbol-value sym) (rt::unbound-value))))
    (null (error TRY_CHANGE_CONST sym))
    (T (error SYM_EXPECTED sym)))
  sym)
