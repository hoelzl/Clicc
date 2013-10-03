;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem
;;;            - WRITE
;;;            - PRIN1
;;;            - PRINT
;;;            - PPRINT (= PRINT)
;;;            - PRINC
;;;            - WRITE-TO-STRING
;;;            - PRIN1-TO-STRING
;;;            - PRINC-TO-STRING
;;;            (- WRITE-CHAR --> file-c)
;;;            - WRITE-STRING
;;;            - WRITE-LINE
;;;            - TERPRI
;;;            - FRESH-LINE
;;;
;;; $Revision: 1.26 $
;;; $Log: print.lisp,v $
;;; Revision 1.26  1994/06/07  09:30:50  hk
;;; In pprint einen Aufruf von terpri eingef"ugt.
;;;
;;; Revision 1.25  1994/06/02  14:14:16  hk
;;; Defaultwerte f"ur *print-length* und *print-level* abweichend vom
;;; Steele auf 50 gesetzt, da *print-circle* nicht beachtet wird.
;;; Beim Drucken von Strukturen wird :print-function beachtet.
;;; Print-Funktionen f"ur readtable, stream, package und pathname werden nun
;;; in den entsprechenden defstructs angegeben.
;;;
;;; Revision 1.24  1994/04/22  14:15:57  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Ausgabe verschoenert.
;;;
;;; Revision 1.23  1994/04/18  12:22:01  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Ausgaberoutine fuer Foreign-Types erweitert.
;;;
;;; Revision 1.22  1993/12/16  16:43:10  pm
;;; inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.21  1993/12/09  10:23:09  hk
;;; Fehler in write2 für Vectoren behoben.
;;;
;;; Revision 1.20  1993/12/06  16:13:06  hk
;;; Fehler in write2 behoben, Test auf structp nach pathnamep etc.
;;;
;;; Revision 1.19  1993/11/30  16:06:59  hk
;;; Fehler behoben.
;;;
;;; Revision 1.18  1993/11/30  15:48:04  hk
;;; Fehler behoben: *print-level* und *print-length* werden bei Strukturen
;;; beachtet.
;;;
;;; Revision 1.17  1993/08/17  15:27:56  hk
;;; Klammerfehler behoben.
;;;
;;; Revision 1.16  1993/08/17  14:07:05  hk
;;; Pathnames als #P, Fehler beim Drucken von Packages behoben.
;;;
;;; Revision 1.15  1993/07/22  08:30:49  pm
;;; Fehler bereinigt
;;;
;;; Revision 1.14  1993/07/21  13:34:40  pm
;;; Erweiterung um die Ausgabe fuer C-Strukturen
;;;
;;; Revision 1.13  1993/07/14  08:52:33  ft
;;; Anpassung an die geänderten Parameter von instance-ref/set.
;;;
;;; Revision 1.12  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.11  1993/05/23  17:56:48  pm
;;; write2 um die Ausgabe von C-Werten erweitert
;;;
;;; Revision 1.10  1993/05/21  13:58:50  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.9  1993/05/03  16:23:39  hk
;;; In write2: Drucken von foreign numbers komplett auskommentiert,
;;; da die Funktionen rt::foreign-number-p und make-integer nicht definiert sind.
;;;
;;; Revision 1.8  1993/05/03  15:30:14  hk
;;; (make-integer object) auskommentiert, da make-integer undefiniert ist.
;;;
;;; Revision 1.7  1993/05/03  12:43:08  pm
;;; Erweiterung um die Ausgabe fuer das FFI
;;;
;;; Revision 1.6  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.5  1993/02/25  15:21:15  ft
;;; Funktion write2 kann jetzt auch Instanzen drucken.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.26 $ eingefuegt
;;;
;;; Revision 1.3  1993/01/11  15:22:58  hk
;;; structure -> struct
;;;
;;; Revision 1.2  1992/09/30  17:24:52  hk
;;; #<Unbound> fuer unbound.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(*print-array* *print-base* *print-case* *print-circle* *print-escape*
   *print-gensym* *print-length* *print-level* *print-pretty* *print-radix*
   write prin1 print pprint princ write-to-string prin1-to-string
   princ-to-string write-string write-line terpri fresh-line))

;;------------------------------------------------------------------------------
;; Einschraenkungen:
;; - *print-radix* wird nicht beachtet
;; - *print-circle* wird nicht beachtet
;; - *read-default-float-format* wird nicht beachtet
;;------------------------------------------------------------------------------
(defparameter *print-array* t)
(defparameter *print-base* 10)
(defparameter *print-case* :upcase)
(defparameter *print-circle* nil)
(defparameter *print-escape* t)
(defparameter *print-gensym* t)
(defparameter *print-length* 50)
(defparameter *print-level* 50)
(defparameter *print-pretty* nil)
(defparameter *print-radix* nil)
 
;;------------------------------------------------------------------------------
(defun write (object &key
                     (stream *standard-output*)
                     ((:escape *print-escape*)    *print-escape*)
                     ((:radix  *print-radix*)     *print-radix*)
                     ((:base   *print-base*)      *print-base*)
                     ((:circle *print-circle*)    *print-circle*)
                     ((:pretty *print-pretty*)    *print-pretty*)
                     ((:level  *print-level*)     *print-level*)
                     ((:length *print-length*)    *print-length*)
                     ((:case   *print-case*)      *print-case*)
                     ((:gensym *print-gensym*)    *print-gensym*)
                     ((:array  *print-array*)     *print-array*))
  (case stream
    ((nil) (setq stream *standard-output*))
    ((T) (setq stream *terminal-io*)))
  (write2 object stream))

;;------------------------------------------------------------------------------
(defun print-integer (x stream)
  (cond
    ((eql 0 x) (write-char #\0 stream))
    (T (labels
           ((f (x)
              (cond
                ((eql 0 x) nil)
                (T (f (truncate x *print-base*))
                   (write-char (digit-char (rem x *print-base*)) stream)))))
         
         (cond
           ((minusp x)
            (write-char #\- stream)
            
            ;; '(f (- x))' geht nicht, da evtl. Overflow bei '(- x)'
            ;;------------------------------------------------------
            (f (- (truncate x *print-base*)))
            (write-char (digit-char (- (rem x *print-base*))) stream))
           (T (f x)))))))

;;------------------------------------------------------------------------------
;; cf. "How to Print Floating-Point Numbers Accurately"
;; in Proceedings of the ACM SIGPLAN '90 Conference on
;; Prog. Lang. Design and Implementation
;;------------------------------------------------------------------------------
(defun print-float (x stream)

  (when (minusp x)
    (write-char #\- stream)
    (setq x (- x)))

  (if (= x 0)

    (write-string "0.0" stream) 

    (let* ((exp (floor (log x 10)))     ; 10 --> 1, 433 --> 2, 0.05 --> -2
           (x (* x (expt 10.0 (- -1 exp)))) ; normiert in der Form 0.xyz
           dot
           (m short-float-epsilon)
           digit)

      (when (>= x (- 1 m))
        
        ;; 0.99999999999... wird zu 1
        (setq x 0.1
              exp (1+ exp)))
         
      (if (or (> exp 7) (< exp -3))

        ;; Den Dezimalpunkt hinter der 1. Ziffer, falls ein expliziter
        ;; Exponent ausgegeben wird.
        (setq dot 0)

        ;; Keinen expliziten Exponenten ausgeben.
        ;; Falls exp >= 1 ist, dann den Dezimalpunkt hinter der exp-ten
        ;; Ziffer, sonst Darstellung als 0.0..0xyz mit exp Nullen hinter
        ;; dem Dezimalpunkt
        (setq dot exp
              exp 0))
         
      (when (< dot 0)
        (write-char #\0 stream)
        (write-char #\. stream)
        (dotimes (i (1- (- dot)))
          (write-char #\0 stream))
        (setq dot -1))
         
      (loop
        (setq m (* m 10))
        (multiple-value-setq (digit x)
          (truncate (* 10 x)))
        (when (or (>= x (- 1 m)) (<= x m))
          (return))
        (write-char (digit-char digit) stream)
        (when (= dot 0)
          (write-char #\. stream))
        (decf dot))

      ;; evtl. aufrunden
      (when (>= x 0.5)
        (incf digit))
      (write-char (digit-char digit) stream)
      
      (when (>= dot 0)
        (dotimes (i dot)
          (write-char #\0 stream))
        (write-char #\. stream)
        (write-char #\0 stream))
      (unless (= 0 exp)
        (write-char #\E stream)
        (print-integer exp stream))
      nil)))

;;------------------------------------------------------------------------------
(defun write2 (object stream)
  (cond

    ((integerp object) (print-integer object stream))
    
    ((characterp object)
     (if *print-escape*
       (let ((name (char-name object)))
         (write-string "#\\" stream)
         (if name
           (write-string name stream)
           (write-char object stream)))
       (write-char object stream)))

    ((symbolp object)
     (let ((name (symbol-name object)))
       (if (not *print-escape*)
         (case *print-case*
           (:upcase (write-string name stream))
           (:downcase (write-string (string-downcase name) stream))
           (:capitalize (write-string (string-capitalize name) stream)))

         (labels
             ;;-----------------------------------------------------------------
             ((potential-number-p (name) ; name ist kein Leerstring
                (let (c)
                  (cond
                    ;; erstes Zeichen pruefen
                    ;;-----------------------
                    ((not (or (digit-char-p (setq c (char name 0))
                                            *print-base*)
                              (find c "-.+^_")))
                     nil)
                    ;; letztes Zeichen pruefen
                    ;;------------------------
                    ((or (eql #\- (setq c (char name (1- (length name)))))
                         (eql #\+ c))
                     nil)
                    ;; alle Zeichen pruefen
                    ;;---------------------
                    (T (let ((found-digits nil))
                         (dotimes (i (length name))
                           (setq c (char name i))
                           (cond
                             ((digit-char-p c *print-base*)
                              (setq found-digits t))
                             ((find c "-.eEfFdDlLsS+/^_") nil)
                             (T (return-from potential-number-p nil))))
                         found-digits)))))
                           
              ;;----------------------------------------------------------------
              (write-symbol-name (name)
                (let ((len (length name)))
                  (if (= 0 len)
                    (write-string "||" stream)
                    (let ((need-escape nil))
                      (dotimes (i len)

                        ;; PROBLEM: RUBOUT und BACKSPACE sind auch
                        ;; CONSTITUENT
                        ;;------------
                        (when (or (lower-case-p (char name i))
                                  (not (eq 'CONSTITUENT
                                           (get-syntax (char name i)))))
                          (setq need-escape t)
                          (return)))
                      (unless need-escape
                        (setq need-escape (potential-number-p name)))

                      ;; pruefen ob das Symbol von der Form '|...| ist.
                      ;;-----------------------------------------------
                      (unless need-escape
                        (do ((i 0 (1+ i)))
                            ((>= i len) (setq need-escape t))
                          (unless (eql #\. (char name i)) (return))))

                      (if need-escape
                        (write-char #\| stream)
                        (case *print-case*
                          (:upcase nil)
                          (:downcase (setq name (string-downcase name)))
                          (:capitalize (setq name
                                             (string-capitalize name)))))

                      ;; Zeichen von symbol-name ausgeben
                      ;;---------------------------------
                      (write-string name stream)

                      (when need-escape (write-char #\| stream)))))))

           ;; evtl. Package-Prefix ausgeben
           ;;------------------------------
           (cond
             ;; Keyword
             ;;--------
             ((keywordp object) (write-char #\: stream))

             ;; uninterned Symbol
             ;;------------------
             ((null (symbol-package object))
              (when *print-gensym* (write-string "#:" stream)))

             (T (multiple-value-bind (symbol where)
                    (find-symbol name)
                    
                  ;; nicht im aktuellen Package erreichbar
                  ;;--------------------------------------
                  (when (or (null where) (not (eq object symbol)))
                    (let ((package (symbol-package object)))
                      (write-symbol-name (package-name package))
                      (write-char #\: stream)
                      (multiple-value-bind (found where)
                          (find-symbol name package)
                        (declare (ignore found))
                        (when (eq where :internal)
                          (write-char #\: stream))))))))
           (write-symbol-name name)))))

    ((stringp object) (cond
                        ((null *print-escape*) (write-string object stream))
                        (T (write-char #\" stream)
                           (dotimes (i (length object))
                             (let ((c (char object i)))
                               (when (or (eql #\\ c) (eql #\" c))
                                 (write-char #\\ stream))
                               (write-char c stream)))
                           (write-char #\" stream))))
    ((consp object)
     (cond
       ((eql *print-level* 0) (write-char #\# stream))
       (T (let ((*print-level*
                 (if *print-level* (1- *print-level*) *print-level*)))
            (write-char #\( stream)
            (cond
              ((eql *print-length* 0) (write-string "..." stream))
              (T (write2 (car object) stream)
                 (do ((cons (cdr object) (cdr cons))
                      (length 1 (1+ length)))
                     ((cond
                        ((atom cons)
                         (when cons
                           (write-string " . " stream)
                           (write2 cons stream))
                         T)
                      
                        ((and *print-length* (>= length *print-length*))
                         (write-string " ..." stream) T)
                        (T nil)))
                   (write-char #\Space stream)
                   (write2 (car cons) stream))))
            (write-char #\) stream)))))

    ((vectorp object)
     (cond
       ((null *print-array*) (write-string "#<vector>" stream))
       ((eql *print-level* 0) (write-char #\# stream))
       ((eql 0 (length object)) (write-string "#()" stream))
       (T (write-string "#(" stream)
          (cond
            ((eql *print-length* 0) (write-string "..." stream))
            (T (let ((*print-level*
                      (if *print-level* (1- *print-level*) *print-level*))
                     (i 1)
                     (len (length object)))
                 (write2 (aref object 0) stream)
                 (loop
                  (when (>= i len) (return))
                  (when (and *print-length* (>= i *print-length*))
                    (write-string " ..." stream)
                    (return))
                  (write-char #\Space stream)
                  (write2 (aref object i) stream)
                  (incf i)))))
          (write-char #\) stream))))

    ((arrayp object) (write-string "#<array>" stream))
    ((floatp object) (print-float object stream))
    ((functionp object) (write-string "#<function>" stream)) 
    ((rt::instancep object)
     (write-string "#<INSTANCE of " stream)
     (write2 (rt::instance-ref (rt::instance-ref object -1) 0) stream)
     (write-string ">" stream))
    ((rt::structp object)
     (cond
       ((eql *print-level* 0) (write-char #\# stream))
       (T (let* ((*print-level*
                  (if *print-level* (1- *print-level*) *print-level*))
                 (type (rt::struct-type object))
                 (print-function (rt:struct-printer type)))
            (cond
              (print-function (funcall print-function object stream 0))
              (T
               (write-string "#S(" stream)
               (write2 type stream)
               (dotimes (i (rt::struct-size object))
                 (when (and *print-length* (>= i *print-length*))
                   (write-string " ..." stream)
                   (return))
                 (write-string " #<slot> " stream)
                 (write2 (rt::struct-ref object i type) stream))
               (write-char #\) stream)))))))

    ((or (ffi::c-char-p object)
         (ffi::c-unsigned-char-p object))
     (write-string "#<C-CHARACTER @ " stream)
     (write-char (ffi::lisp-character object) stream)
     (write-string ">" stream))
    ((or (ffi::c-long-p object)
         (ffi::c-unsigned-long-p object))
     (write-string "#<C-INTEGER @ " stream)
     (print-integer (ffi::lisp-integer object) stream)
     (write-string ">" stream))
    ((ffi::c-long-double-p object)
     (write-string "#<C-FLOAT @ " stream)
     (print-float (ffi::lisp-float object) stream)
     (write-string ">" stream))

    ((or (rt::c-struct-p object)
         (rt::c-union-p object)
         (rt::c-handle-p object)
         (rt::c-array-p object))
     (let* ((*print-level* 0))
       (write-string "#<" stream)
       (write2 (rt::internal-get-symbol object) stream)
       (write-string " @ " stream)
       (write2 (rt::internal-get-address object) stream)
       (write-string ">" stream)))
    ((ffi::c-string-p object)
     (write-string "#<C-STRING @ " stream)
     (write-string (ffi::make-lisp-string object) stream)
     (write-string ">" stream))

    ((rt::unbound-value-p object)
     (write-string "#<Unbound>" stream))

    (T (write-string "#<!!! unknown type !!!!>" stream)))
  object)
    
  
;;------------------------------------------------------------------------------
(defun prin1 (object &optional stream)
  (let ((*print-escape* t))
    (case stream
      ((nil) (setq stream *standard-output*))
      ((T) (setq stream *terminal-io*)))
    (write2 object stream)))

;;------------------------------------------------------------------------------
(defun print (object &optional stream)
  (let ((*print-escape* t))
    (case stream
      ((nil) (setq stream *standard-output*))
      ((T) (setq stream *terminal-io*)))
    (terpri stream)
    (write2 object stream)
    (write-char #\Space stream)
    object))

;;------------------------------------------------------------------------------
(defun pprint (object &optional stream)
  (let ((*print-escape* t) (*print-pretty* t))
    (case stream
      ((nil) (setq stream *standard-output*))
      ((T) (setq stream *terminal-io*)))
    (terpri stream)
    (write2 object stream)
    (values)))

;;------------------------------------------------------------------------------
(defun princ (object &optional stream)
  (let ((*print-escape* nil))
    (case stream
      ((nil) (setq stream *standard-output*))
      ((T) (setq stream *terminal-io*)))
    (write2 object stream)))

;;------------------------------------------------------------------------------
(defun write-to-string (object &key
                               ((:escape *print-escape*)    *print-escape*)
                               ((:radix  *print-radix*)     *print-radix*)
                               ((:base   *print-base*)      *print-base*)
                               ((:circle *print-circle*)    *print-circle*)
                               ((:pretty *print-pretty*)    *print-pretty*)
                               ((:level  *print-level*)     *print-level*)
                               ((:length *print-length*)    *print-length*)
                               ((:case   *print-case*)      *print-case*)
                               ((:gensym *print-gensym*)    *print-gensym*)
                               ((:array  *print-array*)     *print-array*))
  (let ((stream (make-string-output-stream)))
    (write2 object stream)
    (get-output-stream-string stream)))

;;------------------------------------------------------------------------------
(defun prin1-to-string (object)
  (let ((stream (make-string-output-stream)))
    (prin1 object stream)
    (get-output-stream-string stream)))

;;------------------------------------------------------------------------------
(defun princ-to-string (object)
  (let ((stream (make-string-output-stream)))
    (princ object stream)
    (get-output-stream-string stream)))
  
;;------------------------------------------------------------------------------
(defun write-string (string &optional stream
                            &key (start 0) (end (length string)))
  (case stream
    ((nil) (setq stream *standard-output*))
    ((T) (setq stream *terminal-io*)))
  (do ()
      ((>= start end))
    (write-char (char string start) stream)
    (incf start))
  string)

;;------------------------------------------------------------------------------
(defun write-line (&rest args)
  (prog1 (apply #'write-string args)
    (terpri)))

;;------------------------------------------------------------------------------
(defun terpri (&optional stream)
  (case stream
    ((nil) (setq stream *standard-output*))
    ((T) (setq stream *terminal-io*)))
  (write-char #\Newline stream)
  nil)

;;------------------------------------------------------------------------------
(defun fresh-line (&optional stream)
  (case stream
    ((nil) (setq stream *standard-output*))
    ((T) (setq stream *terminal-io*)))

  (unless (eql 0 (funcall (stream-column stream)))
    (write-char #\Newline stream)))
