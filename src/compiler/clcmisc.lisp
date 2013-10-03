;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Hilsfunktionen, Hilfsmakros, Fehlermeldungen, Par-spec
;;;
;;; $Revision: 1.35 $
;;; $Log: clcmisc.lisp,v $
;;; Revision 1.35  1994/06/10  23:31:11  hk
;;; Neue Funktion intern-postnum, die an ein Symbol eine Zahl anh"angt
;;;
;;; Revision 1.34  1994/06/03  15:25:41  uho
;;; Underscores werden in C Namen zu Us.
;;;
;;; Revision 1.33  1994/02/22  14:46:25  hk
;;; check-nparams gestrichen, Fehler in clc-check-nparams behoben.
;;;
;;; Revision 1.32  1994/02/21  10:26:14  kl
;;; clc-check-nparams erweitert und neue Unterfunktionen dazu geschrieben.
;;;
;;; Revision 1.31  1994/02/08  11:06:00  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.30  1993/11/17  23:00:31  hk
;;; In clc-probe-file wurde (open .. :probe) durch (probe-file ..)
;;; ersetzt, weil akcl sonst das limit für 'open files' überschritten
;;; hatte.
;;;
;;; Revision 1.29  1993/08/20  10:41:17  hk
;;; :element-type eingefuegt bei make-array mit :displaced-to.
;;;
;;; Revision 1.28  1993/07/28  11:14:51  hk
;;; merge-pathnames in clc-probe-file verwendet
;;;
;;; Revision 1.27  1993/07/26  13:45:24  hk
;;; Neue Funktion copy-array
;;;
;;; Revision 1.26  1993/07/19  11:37:09  hk
;;; Kommentar korrigiert
;;;
;;; Revision 1.25  1993/07/13  11:16:28  uho
;;; Funktionen 'C-ify' und 'unique-prefix' aus cgfuns hierher geschoben.
;;; Funktion 'calc-C-name' definiert, der aus einem String einen gueltigen
;;; C-Identifikator macht.
;;;
;;; Revision 1.24  1993/06/22  12:51:15  uho
;;; Extrabehandlung von *default-pathnames-defaults* fuer CLISP eingebaut
;;;
;;; Revision 1.23  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.22  1993/05/28  14:52:06  kl
;;; duplicates-in-list umgestellt.
;;;
;;; Revision 1.21  1993/05/06  15:13:21  hk
;;; break in clc-error.
;;;
;;; Revision 1.20  1993/04/07  16:07:07  hk
;;; Fehlermeldungen und Warnings nach *error-output*.
;;;
;;; Revision 1.19  1993/04/03  09:37:37  hk
;;; Neues Macro match-args, das die Argumente eines Aufrufs destrukturiert.
;;;
;;; Revision 1.18  1993/03/30  13:00:34  hk
;;; Neues Macro match zum destrukturieren von Listen in lokale Variablen.
;;;
;;; Revision 1.17  1993/02/17  11:15:59  kl
;;; Intern-prefixed und intern-postfixed akzeptieren statt Symbolen nun
;;; auch Strings.
;;;
;;; Revision 1.16  1993/02/16  15:27:21  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.15  1993/01/22  14:53:35  ft
;;; duplicates-in-list an erweiterte Funktionsnamen angepasst.
;;;
;;; Revision 1.14  1993/01/20  15:16:12  kl
;;; intern-prefixed und intern-postfixed eingefuehrt.
;;;
;;; Revision 1.13  1993/01/07  15:04:03  hk
;;; ~a durch ~s ersetzt.
;;;
;;; Revision 1.12  1992/12/02  10:58:51  hk
;;; Error und Warning Messages nicht bzw. als anderer Kommentar.
;;;
;;; Revision 1.11  1992/12/02  10:46:54  hk
;;; In clc-check-nparams wurde clicc-error durch clc-error ersetzt,
;;; damit clc-check-nparams auch in Pass3 benutzt werden kann.
;;;
;;; Revision 1.10  1992/11/05  09:16:35  kl
;;; subclasses und subclasses* nach printzs.lisp verlegt.
;;;
;;; Revision 1.9  1992/11/04  13:45:10  kl
;;; Funktion internal-error fuer CLICC-interne Fehler eingefuehrt.
;;;
;;; Revision 1.8  1992/11/03  12:36:49  ft
;;; Erweiterung um Hilfsfunktionen aus printzs.lisp.
;;;
;;; Revision 1.7  1992/10/08  16:51:33  hk
;;; Neue Funktion duplicates-in-list.
;;;
;;; Revision 1.6  1992/09/29  20:37:53  hk
;;; Kein (throw 'ABORT nil) mehr, clc-error neu, kein (tab) mehr.
;;;
;;; Revision 1.5  1992/08/07  11:47:36  hk
;;; Syntaktische Aenderungen.
;;;
;;; Revision 1.4  1992/07/09  16:34:16  hk
;;; Neue Funktionen map-array und update-array.
;;;
;;; Revision 1.3  1992/06/05  12:42:06  hk
;;; Aufruf von (break) in clicc-error.
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
;;                                ALLGEMEINE HILFSFUNKTIONEN
;;------------------------------------------------------------------------------
(defmacro 1++ (place)
  `(PROG1 ,place (INCF ,place)))

(defun collect-if (&rest args)
  (apply #'REMOVE-IF-NOT args))

(defun p1-endp (form)
  (typecase form
    (null t)
    (cons nil)
    (t (clicc-error NO_LIST_END form))))

;;------------------------------------------------------------------------------
;; Apply function f to each element of array a.
;; Return a as result.
;;------------------------------------------------------------------------------
(defun map-array (f a)
  (let* ((total-size (array-total-size a))
         (linear-array (make-array total-size
                                   :displaced-to a
                                   :element-type (array-element-type a))))
    (dotimes (i total-size)
      (funcall f (elt linear-array i)))
    a))

;;------------------------------------------------------------------------------
;; Apply function f to each element of array a and write the result of each
;; application back into the array.
;; Return a as result.
;;------------------------------------------------------------------------------
(defun update-array (f a)
  (let* ((total-size (array-total-size a))
         (linear-array (make-array total-size
                                   :displaced-to a
                                   :element-type (array-element-type a))))
    (dotimes (i total-size)
      (setf (elt linear-array i) (funcall f (elt linear-array i))))
    a))

;;------------------------------------------------------------------------------
;; Create a new array with the same dimensions as array a and apply the
;; function f to the elements of a to compute the values of the new array.
;; Returns the new array.
;;------------------------------------------------------------------------------
(defun copy-array (f a)
  (let* ((new (make-array (array-dimensions a)))
         (total-size (array-total-size a))
         (l (make-array total-size
                        :displaced-to a
                        :element-type (array-element-type a)))
         (l2 (make-array total-size
                         :displaced-to new
                         :element-type (array-element-type new))))
    
    (dotimes (i total-size)
      (setf (elt l2 i) (funcall f (elt l i))))
    new))

;;------------------------------------------------------------------------------
(defun intern-prefixed (prefix symbol-or-string)
  (intern (concatenate 'string prefix (string symbol-or-string))))

(defun intern-postfixed (symbol-or-string postfix)
  (intern (concatenate 'string (string symbol-or-string) postfix)))

(defun intern-postnum (symbol i)
  (intern (format nil "~A~A" symbol i)))

;;------------------------------------------------------------------------------
(defun mapappend (fun &rest args) 
  (if (some #'null args)
      '()
      (append (apply fun (mapcar #'first args))
              (apply #'mapappend fun (mapcar #'rest args)))))

;;------------------------------------------------------------------------------
(defun duplicates-in-list (l)
  (if (endp l)
       l
       (if (member (car l) (cdr l) :test #'equal)
           (cons (car l) (duplicates-in-list (cdr l)))
           (duplicates-in-list (cdr l)))))

;;------------------------------------------------------------------------------
;; Destrukturiert die Argumente eines Aufrufs der Form (name . args)
;;------------------------------------------------------------------------------
(defmacro match-args (form name p . body)
  `(match ,name (cdr ,form) ,p . ,body))

;;------------------------------------------------------------------------------
;; Fuehrt den Rumpf body in einer Umgebung aus, in der die Variablen der
;; strukturierten Lambda-Liste p an Werte aus dem strukturierten Datum in form
;; gebunden sind. name wird fuer Fehlermeldungen verwendet.
;; p := symbol | null | (p . p)
;;------------------------------------------------------------------------------
(defmacro match (name form p . body)
  (typecase p
    (null `(progn (unless (null ,form)
                    (clc-error "ignoring extra ~a in ~a" ,form ',name))
            . ,body))
    (symbol `(let ((,p ,form)) . ,body))
    (atom (error "illegal use of match macro"))
    (t `(if (atom ,form)
         (progn (clc-error "~a does not match ~a in ~a" ,form ',p ',name) nil)
         (match ,name (car ,form) ,(car p)
          (match ,name (cdr ,form) ,(cdr p) . ,body))))))

;;------------------------------------------------------------------------------
;; Abstrakter Datentyp Queue
;; Eine Cons-Zelle enthaelt im Car eine Liste und im Cdr den letzten Cons-Knoten
;; dieser Liste.
;; Eine leere Queue wird durch einen Cons-Knoten, der im Car und Cdr Nil
;; enthaelt, dargestellt.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
(defun empty-queue ()
  (cons nil nil))

(defun empty-queue-p (q)
  (null (car q)))

(defun list2queue (l)
  (cons l (last l)))                   ; (last ()) = ()

(defun queue2list (q)
  (car q))

(defun add-q (e q)
  (let ((new-cons (cons e nil)))
    (if (null (cdr q))
        (rplaca q new-cons)
        (rplacd (cdr q) new-cons))
    (rplacd q new-cons)))

(defun addnew-q (e q)
  (unless (member e (queue2list q))
    (add-q e q)))

;;------------------------------------------------------------------------------
;; Letztes Element der Queue
;;------------------------------------------------------------------------------
(defun last-q (q)
  (cadr q))

(defun set-last-q (e q)
  (rplaca (cdr q) e))


;;------------------------------------------------------------------------------
;; Stellt die Parameter-Spezifikation eine exakte Parameter-Anzahl dar ?
;;------------------------------------------------------------------------------
(defmacro exact-par-spec-p (par-spec)
  `(>= ,par-spec 0))

;;------------------------------------------------------------------------------
;; Dekodiert eine Parameter-Spezifikation, die eine Mindestanzahl bedeutet.
;;------------------------------------------------------------------------------
(defmacro at-least-params (par-spec)
  `(1- (ABS ,par-spec)))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun positive-par-spec (par-spec)
  (if (exact-par-spec-p par-spec)
      par-spec
      (at-least-params par-spec)))

;;------------------------------------------------------------------------------
;; Prüft, ob die Anzahl der aktuellen Parameter passend ist. Rückgabewerte sind:
;;  'not-enough-argsX, wenn zu wenig aktuelle Parameter vorliegen,
;;  'too-much-args, wenn zu viele Parameter vorliegen,
;;  nil sonst.
;;------------------------------------------------------------------------------
(defun number-of-args-is-not-ok (par-spec number-of-args)
  (if (exact-par-spec-p par-spec)
      (cond ((< number-of-args par-spec) 'not-enough-args)
            ((> number-of-args par-spec) 'too-much-args)
            (T nil))
      (cond ((< number-of-args (at-least-params par-spec)) 'not-enough-args)
            (T nil))))


;;------------------------------------------------------------------------------
;; Ueberprueft, ob die Anzahl der aktuellen Parameter korrekt ist
;;------------------------------------------------------------------------------
(defun clc-check-nparams (par-spec number-of-arguments symbol)
  (case (number-of-args-is-not-ok par-spec number-of-arguments)
    (not-enough-args 
     (clc-error NOT_ENOUGH_ARGS symbol (positive-par-spec par-spec)))
    (too-much-args
     (clc-error TOO_MUCH_ARGS symbol par-spec))
    (otherwise)))

;;------------------------------------------------------------------------------
;; Ausgabe von Meldungen, Error oder Warning
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
(defun say-where ()
  (when (and (not *CLICC-PRINT*)
             *FUN-NAME*)
    (when (not (symbolp *FUN-NAME*))
      (terpri *error-output*))
    (format *error-output* ";;; While analyzing ")
    (if (symbolp *FUN-NAME*)
      (format *error-output* "function ~s" *FUN-NAME*)
      (format *error-output* "~A" *FUN-NAME*))
    (terpri *error-output*)
    (setq *FUN-NAME* nil)))

;;------------------------------------------------------------------------------
;; Diese Funktion kann in allen Phasen des Compilers aufgerufen werden.
;; Sie gibt eine Fehlermeldung in einheitlicher Weise aus und erhoeht
;; *NERRORS*.
;;------------------------------------------------------------------------------
(defun clc-error (error-message &rest args-for-error-message)
  (incf *NERRORS*)
  (format *error-output* "Error: ")
  (apply #'format *error-output* error-message args-for-error-message)
  (terpri *error-output*)
  (terpri *error-output*)
  (break))

;;------------------------------------------------------------------------------
;; Diese Funktion darf nur in Pass1 aufgerufen werden.
;;------------------------------------------------------------------------------
(defun clicc-error (error-message &rest args-for-error-message)
  (incf *NERRORS*)
  (say-where)
  (if *CURRENT-FORM*
    (format *error-output* "Error in form ~s~% -> " *CURRENT-FORM*)
    (format *error-output* "Error: "))
  (apply #'format *error-output* error-message args-for-error-message)
  (terpri *error-output*)
  (terpri *error-output*)
  (break)
  (throw 'clicc-error nil))

;;------------------------------------------------------------------------------
(defun clicc-warning (warn-string &rest args-for-warn-string)
  (incf *NWARNINGS*)
  (say-where)
  (format *error-output* "; Warning: ")
  (apply #'format *error-output* warn-string args-for-warn-string)
  (terpri *error-output*))

;;------------------------------------------------------------------------------
(defun clicc-message (mess-string &rest args-for-mess-string)
  (when *CLICC-PRINT*
    (format t ";;; ")
    (apply #'format t mess-string args-for-mess-string)
    (terpri)))

;;------------------------------------------------------------------------------
(defun clicc-message-line (&optional (length 75))
  (clicc-message (make-string length :initial-element #\-)))

;;------------------------------------------------------------------------------
;; Funktion zur Behandlung interner Fehler:
;;------------------------------------------------------------------------------
(defun internal-error (function-name err-string &rest args-for-err-string)
  (error (concatenate 'string 
                      (format nil "internal (~S), " function-name)
                      (apply #'format nil err-string args-for-err-string))))


;;------------------------------------------------------------------------------
;; Umwandlung von Strings in gueltige C-Identifikatoren
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Generierung aus einem String einen neuen String, der als C-Identifikator
;; benutzt werden kann. Der String kann Zeichen enthalten, die in
;; C-Identifikatoren nicht verwendet werden duerfen.
;; 
;; Konvertierungsregeln:
;; Grossbuchstaben werden zu Kleinbuchstaben,
;; <,>,=,/,*,? werden zu L,G,E,N,X,Q
;; - wird zu _
;; alle weiteren Zeichen werden  zu _
;;
;; Diese Konvertierung ist nicht injektiv, daher wird dem generierten String
;; im allgemeinen ein eindeutiger Prefix vorangestellt.  
;; Als eindeutiger Prefix werden Strings der Form "Z<n>" verwendet, in denen
;; <n> einen natuerliche Zahl ist, die fuer jeden neuen Identifikator um 1
;; erhoeht wird.
;;
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Umwandlung eines Strings in einen C-Identifkator.  
;;
;; Wenn allow-illegal NIL ist, wird eine Fehlermeldung ausgegeben, falls der
;; String Zeichen enthaelt, die keine direkte Entsprechung in
;; C-Identifikatoren haben.  
;;
;; Ist allow-illegal T, so wird kein Fehler ausgeloest sondern der Verstoss im
;; zweiten Resultatswert gemeldet.
;;
;; lower-case-normal bestimmt, wie mit Buchstaben umgegangen werden soll.  Bei
;; lower-case-normal=T wird die Gross/Kleinschreibung beibehalten, bei
;; lower-case-normal T wird sie umgekehrt.
;;------------------------------------------------------------------------------
(defun C-ify (name &key (allow-illegal nil) (lower-case-normal nil))
  ;; lower-case-normal   T: a->a ; (A->A  illegal:=T)
  ;; lower-case-normal NIL: A->a ; (a->A  illegal:=T)
  (let* ((illegal nil)
         (upper-case-normal (not lower-case-normal))
         (result
          (map 'string
               #'(lambda (char)
                   (cond
                     ((and upper-case-normal (upper-case-p char))
                      (char-downcase char))
                     ((and lower-case-normal (lower-case-p char)) char)
                     ((digit-char-p char) char)
                     ((eql #\- char) #\_)
                     ((eql #\< char) #\L) ; less
                     ((eql #\> char) #\G) ; greater
                     ((eql #\= char) #\E) ; eql
                     ((eql #\/ char) #\N) ; not
                     ((eql #\* char) #\X)
                     ((eql #\% char) #\P)
                     ((eql #\? char) #\Q)
                     ((eql #\_ char) #\U) 
                     (t (setq illegal t)
                        (cond
                          ((and upper-case-normal (lower-case-p char))
                           (char-upcase char))
                          ((and lower-case-normal (upper-case-p char)) char)
                          (T #\_)))))
               name)))
    (when (or (zerop (length result))
              (digit-char-p (char result 0)))
      (setq illegal t))
    (when (and illegal (not allow-illegal))
      (clc-error "~s can't be transformed into C identifier" name))
    (values result illegal)))

;;------------------------------------------------------------------------------
;; Stelle einem String einen eindeutigen Prefix voran. Verwende die Variable
;; *C-NAME-PREFIX* zum Durchnumerieren der Prefixe.
;;------------------------------------------------------------------------------
(defun unique-prefix (string &key (prefix-letter "Z"))
  (incf *C-NAME-PREFIX*)
  (concatenate 'string
               prefix-letter
               (princ-to-string *C-NAME-PREFIX*) "_" string))

;;------------------------------------------------------------------------------
;; Bestimme einen eindeutigen gueltigen C-Identifikator mit angegebenem Prefix
;; zum uebergebenen Namen.
;;------------------------------------------------------------------------------
(defun calc-C-name (prefix name)
  (multiple-value-bind (c-name illegal)
      (C-ify name :allow-illegal T :lower-case-normal T)
    (if illegal
        (unique-prefix c-name :prefix-letter prefix)
        (concatenate 'string prefix c-name))))


;;------------------------------------------------------------------------------
;; ******** Betriebsystemabhaengig *********
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Testet, ob eine Datei mit Namen 'filename' geoeffnet werden kann.
;; Resultat: Dateiname, falls Datei existiert, NIL sonst.
;;------------------------------------------------------------------------------
(defun clc-probe-file (filename ext)
  (do ((extensions (if (and ext (string/= "" ext))
                     (list ext)
                     '(".lisp" ".lsp" ".cl" "")))
       filename-ext)
      ((null extensions) nil)
    (setq filename-ext
          (merge-pathnames (concatenate 'string filename (pop extensions))))
    (when (probe-file filename-ext)
      (return filename-ext))))

;;------------------------------------------------------------------------------
(defun strip-path (name)
  (let ((n (position #\/ name :from-end t)))
    (if n
      (subseq name (1+ n))
      name)))

;;------------------------------------------------------------------------------
(defun get-path (name)
  (let ((n (position #\/ name :from-end t)))
    (if n
      (subseq name 0 (1+ n))
      "")))

;;------------------------------------------------------------------------------
;; Teilt einen Pfadnamen-String auf in den Name und Extension
;;------------------------------------------------------------------------------
(defun split-name-ext (name)
  (let* ((slash-pos (position #\/ name :test #'char= :from-end t))
         (dot-pos   (position #\. name :test #'char= :start (if slash-pos 
                                                                (1+ slash-pos) 
                                                                0))))
    (if dot-pos
      (values (subseq name 0 dot-pos) (subseq name dot-pos))
      (values name ""))))

;;------------------------------------------------------------------------------
(provide "clcmisc")

