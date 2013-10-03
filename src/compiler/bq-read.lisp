;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem:  Backquote-Reader + Simplifier
;;;
;;; $Revision: 1.9 $
;;; $Log: bq-read.lisp,v $
;;; Revision 1.9  1994/01/11  16:12:37  hk
;;; Fehler in bq-attach-append bei `( ..... . const) behoben.
;;;
;;; Revision 1.8  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.7  1993/06/04  13:04:39  hk
;;; Schreibfehler.
;;;
;;; Revision 1.6  1993/06/04  12:44:21  hk
;;; vector-reader verbessert
;;;
;;; Revision 1.5  1993/05/12  13:02:24  hk
;;; VECTOR -> BQ-VECTOR.
;;;
;;; Revision 1.4  1993/02/16  16:08:30  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.3  1993/01/26  17:22:06  hk
;;; (in-package CLICC) statt (in-package RUNTIME)
;;;
;;; Revision 1.2  1992/12/22  16:38:48  hk
;;; cons-reader von allegro wird nun benutzt.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Backquote ...
;;------------------------------------------------------------------------------
;;; angelehnt an "Backquote, Appendix C in CLtL, 2nd Ed."
;;; erweitert um Backquote in Vektoren.
;;------------------------------------------------------------------------------

(defvar *comma*        (make-symbol "COMMA"))
(defvar *comma-atsign* (make-symbol "COMMA-ATSIGN"))
(defvar *comma-dot*    (make-symbol "COMMA-DOT"))
(defvar *bq-list*      (make-symbol "BQ-LIST"))
(defvar *bq-append*    (make-symbol "BQ-APPEND"))
(defvar *bq-list**     (make-symbol "BQ-LIST*"))
(defvar *bq-nconc*     (make-symbol "BQ-NCONC"))
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(defvar *bq-quote*     (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))
(defvar *bq-vector*    (make-symbol "BQ-VECTOR"))
(defvar *bq-level*     0)
(defvar *cons-reader*  (get-macro-character #\( ))
(defvar *vector-reader* (get-dispatch-macro-character #\# #\())

;;------------------------------------------------------------------------------
(defun backquote-reader (stream char)
  (declare (ignore char))
  (incf *bq-level*)
  (prog1
      (bq-completely-process (bq-read stream))
    (decf *bq-level*)))

;;------------------------------------------------------------------------------
(defun comma-reader (stream char)
  (declare (ignore char))
  (when (<= *bq-level* 0)
    (error "A comma appeared outside of a backquote"))
  (decf *bq-level*)
  (prog1
      (cons (case (peek-char nil stream t nil t)
              (#\@ (read-char stream t nil t) *comma-atsign*)
              (#\. (read-char stream t nil t) *comma-dot*)
              (otherwise *comma*))
            (bq-read stream))
    (incf *bq-level*)))

;;------------------------------------------------------------------------------
(defun bq-completely-process (x)
  (bq-remove-tokens (bq-simplify (bq-process x))))

;;------------------------------------------------------------------------------
(defun bq-process (x)
  (cond
    ;; `basic --> (QUOTE basic)
    ;;-------------------------
    ((atom x) (list *bq-quote* x))

    ;; `#(x1 x2 x3 ... xn) --> (apply #'vector `(x1 x2 x3 ... xn))
    ;;------------------------------------------------------------
    ((eq (car x) *bq-vector*)
     (let ((list (bq-completely-process (cdr x))))
       (if (eq 'QUOTE (car list))
         (list *bq-quote* (apply #'vector (cadr list)))
         (list 'APPLY `#'VECTOR list))))

    ;; `,form --> form
    ;;----------------
    ((eq (car x) *comma*) (cdr x))

    ;; `,@form --> ERROR
    ;;------------------
    ((eq (car x) *comma-atsign*) (error ",@~S after `" (cdr x)))

    ;; `,.form --> ERROR
    ;;------------------
    ((eq (car x) *comma-dot*) (error ",.~S after `" (cdr x)))

    ;; `(x1 x2 x3 ... xn . atom) -->
    ;;------------------------------
    (t (do ((p x (cdr p))
            (q '() (cons (bracket (car p)) q)))
           ((atom p)

            ;; --> (append [x1] [x2 [x3] ... [xn] (quote atom))
            ;;-------------------------------------------------
            (cons *bq-append*
                  (nreconc q (list (list *bq-quote* p)))))

         ;; `(x1 ... xn . ,form) --> (append [x1] ... [xn] form)
         ;;-----------------------------------------------------
         (when (eq (car p) *comma*)
           (return (cons *bq-append* (nreconc q (list (cdr p))))))

         ;; `(x1 ... xn . ,@form) --> ERROR
         ;;--------------------------------
         (when (eq (car p) *comma-atsign*) (error "Dotted ,@~s" (cdr p)))

         ;; `(x1 ... xn . ,.form) --> ERROR
         ;;--------------------------------
         (when (eq (car p) *comma-dot*) (error "Dotted ,@~s" (cdr p)))))))

;;------------------------------------------------------------------------------
(defun bracket (x)
  (cond
    ((atom x) (list *bq-list* (bq-process x)))
    ((eq (car x) *comma*) (list *bq-list* (cdr x)))
    ((eq (car x) *comma-atsign*) (cdr x))
    ((eq (car x) *comma-dot*) (list *bq-clobberable* (cdr x)))
    (t (list *bq-list* (bq-process x)))))

;;------------------------------------------------------------------------------
(defun maptree (fn x)
  (if (atom x)
    (funcall fn x)
    (let ((a (funcall fn (car x)))
          (d (maptree fn (cdr x))))
      (if (and (eql a (car x)) (eql d (cdr x)))
        x
        (cons a d)))))

;;------------------------------------------------------------------------------
(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))

;;------------------------------------------------------------------------------
(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) *comma*)
           (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))

;;------------------------------------------------------------------------------
(defun bq-simplify (x)
  (if (atom x)
    x
    (let ((x (if (eq (car x) *bq-quote*)
               x
               (maptree #'bq-simplify x))))
      (if (not (eq (car x) *bq-append*))
        x
        (bq-simplify-args x)))))

;;------------------------------------------------------------------------------
(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args) result))
              ((and (eq (caar args) *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args) result))
              ((and (eq (caar args) *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args) *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args) *bq-clobberable*)
               (bq-attach-append *bq-nconc* (cadar args) result))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args) result)))

;;------------------------------------------------------------------------------
(defun null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

;;------------------------------------------------------------------------------
(defun bq-attach-append (op item result)
  (cond ((or (null result) (equal result *bq-quote-nil*))
         (if (bq-splicing-frob item) (list op item) item))
        ((and (null-or-quoted item) (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

;;------------------------------------------------------------------------------
(defun bq-attach-conses (items result)
  (cond
    ((and (every #'null-or-quoted items)
          (null-or-quoted result))
     (list *bq-quote*
           (append (mapcar #'cadr items) (cadr result))))
    ((or (null result) (equal result *bq-quote-nil*))
     (cons *bq-list* items))
    ((and (consp result)
         (or (eq (car result) *bq-list*)
             (eq (car result) *bq-list**)))
     (cons (car result) (append items (cdr result))))
    (t (cons *bq-list** (append items (list result))))))

;;------------------------------------------------------------------------------
(defun bq-remove-tokens (x)
  (cond
    ((atom x) (cond
                ((eq x *bq-list*) 'list)
                ((eq x *bq-append*) 'append)
                ((eq x *bq-nconc*) 'nconc)
                ((eq x *bq-list**) 'list*)
                ((eq x *bq-quote*) 'quote)
                (T x)))
    
    ((eq (car x) *bq-clobberable*) (bq-remove-tokens (cadr x)))
    ((and (eq (car x) *bq-list**) (consp (cddr x)) (null (cdddr x)))
     (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
    (T (maptree #'bq-remove-tokens x))))

;;------------------------------------------------------------------------------
(defun vector-reader (stream char i)
  (if (> *bq-level* 0)
      (let ((list (funcall *cons-reader* stream char)))
        (when i
          (error "#~s( syntax is not allowed in backquoted expressions" i))
        (cons *bq-vector* list))
      (funcall *vector-reader* stream char i)))

;;------------------------------------------------------------------------------
(defun bq-read (stream)
  (read stream t nil t))


(set-dispatch-macro-character #\# #\( #'vector-reader)

(set-macro-character #\, #'comma-reader NIL)
(set-macro-character #\` #'backquote-reader NIL)

;;------------------------------------------------------------------------------
(provide "bq-read")
