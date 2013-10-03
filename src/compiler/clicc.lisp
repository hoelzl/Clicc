;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Hauptprogramm des Compilers
;;;
;;; $Revision: 1.16 $
;;; $Log: clicc.lisp,v $
;;; Revision 1.16  1994/05/22  15:07:49  sma
;;; Neuer Kommandoswitch -R zum Einstellen der Datenrepräsentation.
;;;
;;; Revision 1.15  1994/05/19  07:57:02  pm
;;; Fehler behoben
;;;
;;; Revision 1.14  1994/05/17  08:21:28  pm
;;; Abgleich der FFI-Exports mit clcload.lisp
;;;
;;; Revision 1.13  1994/04/22  14:09:54  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Ueberfluessiges exportiertes Symbol entfernt
;;;
;;; Revision 1.12  1994/04/18  12:05:56  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Liste der exportierten Symbole ergaenzt
;;;
;;; Revision 1.11  1993/12/21  09:29:59  hk
;;; Schreibfehler in Usage behoben.
;;;
;;; Revision 1.10  1993/12/06  16:42:15  hk
;;; Flag geändert und ergänzt: -v verbose, -V version, -O opt cycles.
;;; Aufgeräumt. Gemischte Flags -vVo <name> gehen nun. Initialisierung
;;; globaler Variablen nach clcdef.
;;;
;;; Revision 1.9  1993/10/26  11:32:34  pm
;;; Abgleich der FFI-Exports mit clcload.lisp
;;;
;;; Revision 1.8  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.7  1993/06/09  12:45:09  hk
;;; An den Modulcompiler angepasst, Packages mit clcload konsistent
;;; gemacht.
;;;
;;; Revision 1.6  1993/04/06  09:59:11  hk
;;; Nickname L.
;;;
;;; Revision 1.5  1993/02/19  13:58:46  hk
;;; :use () beim LISP Package.
;;;
;;; Revision 1.4  1993/02/16  15:27:00  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.3  1993/01/11  16:48:37  hk
;;; argv -> c-argv
;;;
;;; Revision 1.2  1992/08/07  11:40:36  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;; 
;;; Changes  : 29.10.91 Bedienung durch Kommandozeilenparameter 
;;;-----------------------------------------------------------------------------

(in-package "LISP" :nicknames '("CLICC-LISP" "L") :use ())
(in-package "USER")                     ; Damit es dieses Package wirklich gibt
(lisp:in-package "RT" :nicknames '("RUNTIME") :use ())
(lisp:in-package "FFI" :use ())
(lisp:export
 '(c-array c-char c-char-p c-double c-double-p c-enum c-float c-float-p c-fun
   c-handle c-int c-int-p c-long c-long-double c-long-double-p c-long-p c-ptr
   c-short c-short-p c-string c-string-p c-struct c-union c-unsigned-char
   c-unsigned-char-p c-unsigned-int c-unsigned-int-p c-unsigned-long
   c-unsigned-long-p c-unsigned-short c-unsigned-short-p c-vararg c-void
   copy-c-cstring def-c-type def-call-in def-call-out foreign-package-name
   free lisp-character lisp-float lisp-integer load-foreign make-c-string
   make-lisp-string)
 "FFI")

(lisp:in-package "CLICC" :use '("LISP"))

;;------------------------------------------------------------------------------
(require "clcmain")

(defconstant *COMMAND-OPTIONS* '((#\V . *SHOW-VERSION*)
                                 (#\v . *CLICC-PRINT*)
                                 (#\m . *MODULE-COMPILER*)
                                 (#\l . *LISP-MODULE*)
                                 (#\i . *INLINE-MODULE*)
                                 (#\s . *SPLIT-FILES*)
                                 (#\f . *FLAT-IFS*)
                                 (#\t *TI-LEVEL*)
                                 (#\R *OBREP*)
                                 (#\O *ITERATIONS*)
                                 (#\c *C-max-line-count*)
                                 (#\o *OUT-FILENAME*)))



(defun clicc-main ()
  (labels
      ((usage ()
         (format
          t
"Usage: clicc [-lmsvV] [-c count] [-t 0-3] [-O level] [-R 1-3] [-o name] name~%~
                  ~A-c: Maximum line count of C file~%~
                  ~A-f: Flat IFs~%~
                  ~A-i: Inline Module~%~
                  ~A-l: Lisp Module~%~
                  ~A-m: Module-Compiler~%~
                  ~A-O: Number of optimization cycles~%~
                  ~A-s: Split Files~%~
                  ~A-t: Type Inference Level 0, 1, 2 or 3~%~
                  ~A-R: Data Representation schema 1, 2 or 3~%~
                  ~A-v: Verbose~%~
                  ~A-V: Show Version~%~
                  ~A-o: Output file~%~
                  ~Aname: The Lisp File~%"
#\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
         (return-from clicc-main)))
       
    (setq *MODULE-COMPILER* nil
          *LISP-MODULE* nil
          *INLINE-MODULE* nil
          *OUT-FILENAME* nil
          *FILENAME* nil)

    (let ((options (cdr (rt::c-argv))))
      (when (null options)
        (usage))
      (do ((opt (car options) (car options)))
          ((endp options)
           (unless *FILENAME* (usage))
           (when (stringp *ITERATIONS*)
             (setq *ITERATIONS* (read-from-string *ITERATIONS*))
             (unless (integerp *ITERATIONS*) (usage))
             (when (zerop *ITERATIONS*) (setq *optimize* nil)))
           (when (stringp *TI-LEVEL*)
             (let ((l (digit-char-p (character *TI-LEVEL*))))
               (if l (setq *TI-LEVEL* l) (usage))))
           (when (stringp *OBREP*)
             (let ((o (digit-char-p (character *OBREP*))))
               (if (and o (<= 1 o 3)) (setq *OBREP* o) (usage))))
           (when (stringp *C-max-line-count*)
             (setq *C-max-line-count* (read-from-string *C-max-line-count*))
             (unless (integerp *C-max-line-count*) (usage)))
           (do-clicc))
        (pop options)
        (cond
          ((eql #\- (char  opt 0))
           (dotimes (i (1- (length opt)))
             (let ((var (cdr (assoc (char opt (1+ i)) *COMMAND-OPTIONS*)))) 
               (if var
                   (if (atom var)
                       (set var t)
                       (if (consp options)
                           (setf (symbol-value (car var)) (pop options))
                           (usage)))
                   (usage)))))
          ((not *FILENAME*) (setq *FILENAME* opt))
          (T (usage)))))))

(clicc-main)
