;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (15. Characters)                             
;;;
;;; $Revision: 1.13 $
;;; $Log: char.lisp,v $
;;; Revision 1.13  1994/06/09  13:54:43  hk
;;; not null in alphanumericp gestrichen
;;;
;;; Revision 1.12  1994/05/04  13:22:45  sma
;;; Fehler in digit-char-p behoben.
;;;
;;; Revision 1.11  1994/04/06  09:25:16  hk
;;; Typtest auc character in rt::char= und rt::char/= eingef"ugt.
;;;
;;; Revision 1.10  1994/02/02  09:40:25  hk
;;; Deklaration :simp-when-n-args bei char-... Funktionen eingefügt.
;;;
;;; Revision 1.9  1994/01/14  09:19:32  sma
;;; Character-Funktionen neu geschrieben. Mehr Lisp, weniger C. Im C-Teil
;;; befinden sich nur noch die zeichensatzabhängigen Funktionen.
;;;
;;; Revision 1.8  1994/01/05  12:44:12  sma
;;; Namensänderung: rt::*-internal -> rt::*
;;; name-char ist jetzt Lisp-Funktion.
;;;
;;; Revision 1.7  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.6  1993/05/14  13:32:47  hk
;;; Funktion character definiert.
;;;
;;; Revision 1.5  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.13 $ eingefuegt
;;;
;;; Revision 1.3  1992/07/06  09:12:17  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/07/06  08:28:10  hk
;;; Neue Syntax fuer declaim fun-spec.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(standard-char-p graphic-char-p alpha-char-p upper-case-p lower-case-p
   both-case-p digit-char-p alphanumericp char= char/= char< char> char<=
   char>= char-equal char-not-equal char-lessp char-greaterp char-not-greaterp
   char-not-lessp char-code code-char character char-upcase char-downcase
   digit-char char-int char-name name-char))

(export 
 '(rt::char= rt::char/= rt::char< rt::char> rt::char<= rt::char>=
   rt::char-equal rt::char-not-equal rt::char-lessp rt::char-greaterp
   rt::char-not-greaterp rt::char-not-lessp) "RT")


;;------------------------------------------------------------------------------
;; Lokal, Typzusicherung
;;------------------------------------------------------------------------------
(defmacro must-be-character (char)
  `(if (characterp ,char)
    ,char
    (error WRONG_TYPE ,char 'character)))

;;------------------------------------------------------------------------------
;; STANDARD-CHAR-P char
;;------------------------------------------------------------------------------
(defun standard-char-p (char)
  (rt::standard-char-p (must-be-character char)))

;;------------------------------------------------------------------------------
;; GRAPHIC-CHAR-P char
;;------------------------------------------------------------------------------
(defun graphic-char-p (char)
  (rt::graphic-char-p (must-be-character char)))

;;------------------------------------------------------------------------------
;; ALPHA-CHAR-P char
;;------------------------------------------------------------------------------
(defun alpha-char-p (char)
  (rt::alpha-char-p (must-be-character char)))
  
;;------------------------------------------------------------------------------
;; UPPER-CASE-P char
;;------------------------------------------------------------------------------
(defun upper-case-p (char)
  (rt::upper-case-p (must-be-character char)))

;;------------------------------------------------------------------------------
;; LOWER-CASE-P char
;;------------------------------------------------------------------------------
(defun lower-case-p (char)
  (rt::lower-case-p (must-be-character char)))

;;------------------------------------------------------------------------------
;; BOTH-CASE-P char
;;------------------------------------------------------------------------------
(defun both-case-p (char)
  (rt::both-case-p (must-be-character char)))

;;------------------------------------------------------------------------------
;; DIGIT-CHAR-P char &optional (radix 10)
;;------------------------------------------------------------------------------
(defun digit-char-p (char &optional (radix 10))
  (unless (check-integer radix 2 36)
    (error "~A should be suitable radix" radix))
  (when (setq char (rt::digit-char (must-be-character char)))
    (if (< char radix)
        char
        nil)))

;;------------------------------------------------------------------------------
;; ALPHANUMERICP char
;;------------------------------------------------------------------------------
(defun alphanumericp (char)
  (or (alpha-char-p char) (digit-char-p char)))

;;------------------------------------------------------------------------------
;; Schnelle zweistellige Vergleichsfunktionen. Werden exportiert, damit der 
;; Optimierer die normalen Funktionen durch diese Varianten ersetzen kann.
;;------------------------------------------------------------------------------
(defun rt::char= (a b)
  (eql (must-be-character a) (must-be-character b)))
(defun rt::char/= (a b)
  (not (eql (must-be-character a) (must-be-character b))))
(defun rt::char< (a b)
  (< (char-code a) (char-code b)))
(defun rt::char> (a b)
  (> (char-code a) (char-code b)))
(defun rt::char<= (a b)
  (<= (char-code a) (char-code b)))
(defun rt::char>= (a b)
  (>= (char-code a) (char-code b)))

;;------------------------------------------------------------------------------
;; CHAR= char &rest more-chars
;;------------------------------------------------------------------------------
(defun char= (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char=))
  (dolist (c chars t)
    (when (rt::char/= c char)
      (return nil))))

;;------------------------------------------------------------------------------
;; CHAR/= char &rest more-chars
;;------------------------------------------------------------------------------
(defun char/= (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char/=))
  (do ((ch chars (cdr ch)))
      ((null ch) t)
    (when (dolist (c ch nil)
            (when (rt::char= char c)
              (return t)))
      (return nil))
    (setq char (car ch))))

;;------------------------------------------------------------------------------
;; CHAR< char &rest more-chars
;;------------------------------------------------------------------------------
(defun char< (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char<))
  (dolist (c chars t)
    (when (rt::char>= char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR> char &rest more-chars
;;------------------------------------------------------------------------------
(defun char> (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char>))
  (dolist (c chars t)
    (when (rt::char<= char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR<= char &rest more-chars
;;------------------------------------------------------------------------------
(defun char<= (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char<=))
  (dolist (c chars t)
    (when (rt::char> char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR>= char &rest more-chars
;;------------------------------------------------------------------------------
(defun char>= (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char>=))
  (dolist (c chars t)
    (when (rt::char< char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; dito für case-insensitive Vergleiche
;;------------------------------------------------------------------------------
(defun rt::char-equal (a b)
  (= (char-code (char-upcase a)) (char-code (char-upcase b))))
(defun rt::char-not-equal (a b)
  (/= (char-code (char-upcase a)) (char-code (char-upcase b))))
(defun rt::char-lessp (a b)
  (< (char-code (char-upcase a)) (char-code (char-upcase b))))
(defun rt::char-greaterp (a b)
  (> (char-code (char-upcase a)) (char-code (char-upcase b))))
(defun rt::char-not-greaterp (a b)
  (<= (char-code (char-upcase a)) (char-code (char-upcase b))))
(defun rt::char-not-lessp (a b)
  (>= (char-code (char-upcase a)) (char-code (char-upcase b))))

;;------------------------------------------------------------------------------
;; CHAR-EQUAL char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-equal (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-equal))
  (dolist (c chars t)
    (when (rt::char-equal c char)
      (return nil))))

;;------------------------------------------------------------------------------
;; CHAR-NOT-EQUAL char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-not-equal (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-not-equal))
  (do ((ch chars (cdr ch)))
      ((null ch) t)
    (when (dolist (c ch nil)
            (when (rt::char-not-equal char c)
              (return t)))
      (return nil))
    (setq char (car ch))))

;;------------------------------------------------------------------------------
;; CHAR-LESSP char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-lessp (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-lessp))
  (dolist (c chars t)
    (when (rt::char-not-lessp char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR-GREATERP char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-greaterp (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-greaterp))
  (dolist (c chars t)
    (when (rt::char-not-greaterp char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR-NOT-GREATERP char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-not-greaterp (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-not-greaterp))
  (dolist (c chars t)
    (when (rt::char-greaterp char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR-NOT-LESSP char &rest more-chars
;;------------------------------------------------------------------------------
(defun char-not-lessp (char &rest chars)
  (declare (:simp-when-n-args 2 rt::char-not-lessp))
  (dolist (c chars t)
    (when (rt::char-lessp char c)
      (return nil))
    (setq char c)))

;;------------------------------------------------------------------------------
;; CHAR-CODE char
;;------------------------------------------------------------------------------
(defun char-code (char)
  (unless (characterp char)
    (error WRONG_TYPE char 'character))
  (rt::char-code char))

;;------------------------------------------------------------------------------
;; CODE-CHAR code
;;------------------------------------------------------------------------------
(defun code-char (code)
  (if (check-integer code 0 char-code-limit)
      (rt::code-char code)
      nil))

;;------------------------------------------------------------------------------
;; CHARACTER object
;;------------------------------------------------------------------------------
(defun character (object)
  (cond
    ((characterp object) object)
    (T (when (symbolp object)
         (setq object (symbol-name object)))
       (if (and (stringp object) (= (length object) 1))
           (char object 0)
           (error "cannot coerce ~S to type character" object)))))

;;------------------------------------------------------------------------------
;; CHAR-UPCASE char
;;------------------------------------------------------------------------------
(defun char-upcase (char)
  (rt::char-upcase (must-be-character char)))

;;------------------------------------------------------------------------------
;; CHAR-DOWNCASE char
;;------------------------------------------------------------------------------
(defun char-downcase (char)
  (rt::char-downcase (must-be-character char)))

;;------------------------------------------------------------------------------
;; DIGIT-CHAR weight &optional (radix 10)
;;------------------------------------------------------------------------------
(defun digit-char (weight &optional (radix 10))
  (if (and (check-integer radix 2 36)
           (check-integer weight 0 (1- radix)))
      (elt "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight)
      nil))

;;------------------------------------------------------------------------------
;; CHAR-INT char
;;------------------------------------------------------------------------------
(defun char-int (char)
  (char-code char))

;;------------------------------------------------------------------------------
;; CHAR-NAME char
;;------------------------------------------------------------------------------
(defun char-name (char)
  (unless (characterp char)
    (error WRONG_TYPE char 'character))
  (cond
    ((char= char #\Space) "Space")
    ((char= char #\Newline) "Newline")
    ((char= char #\Backspace) "Backspace")
    ((char= char #\Tab) "Tab")
    ((char= char #\Linefeed) "Linefeed")
    ((char= char #\Page) "Page")
    ((char= char #\Return) "Return")
    ((char= char #\Rubout) "Rubout")
    (T nil)))

;;------------------------------------------------------------------------------
;; NAME-CHAR string
;;------------------------------------------------------------------------------
(defun name-char (string)
  (setq string (string string))
  (cond
    ((string-equal string "Space") #\Space) 
    ((string-equal string "Newline") #\Newline)
    ((string-equal string "Backspace") #\Backspace)
    ((string-equal string "Tab") #\Tab)
    ((string-equal string "Linefeed") #\Linefeed)
    ((string-equal string "Page") #\Page)
    ((string-equal string "Return") #\Return)
    ((string-equal string "Rubout") #\Rubout)
    (T nil)))
