;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Konfigurierung von CLICC
;;;
;;; $Revision: 1.19 $
;;; $Log: config.lisp,v $
;;; Revision 1.19  1993/12/06  16:32:45  hk
;;; Definition der Variablen *warn-unbound-vars* und *ansi-c* nach clcdef.
;;;
;;; Revision 1.18  1993/11/29  10:04:56  uho
;;; Initialisierung der Pfadnamen wird nun in der Funktion
;;; 'setup-clicc-pathenames' vorgenommen, diese auf top-level aufgerufen.
;;;
;;; Revision 1.17  1993/11/08  11:10:54  hk
;;; Sonderbehandlung für *CLICC-PATH-STRING* in CLISP entfernt.
;;;
;;; Revision 1.16  1993/08/31  09:32:43  uho
;;; Aenderungen fuer die 22Aug93 Version von CLISP eingefuegt.
;;;
;;; Revision 1.15  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.14  1993/06/07  11:10:05  kl
;;; Kommentar zum neuen Fixnumtest erweitert.
;;;
;;; Revision 1.13  1993/06/07  10:27:04  kl
;;; Test auf fixnum im Zielsystem eingebaut.
;;;
;;; Revision 1.12  1993/06/04  14:04:37  hk
;;; Neue Funktion calc-next-C-filename, Funktion calc-fun-filename
;;; veraendert.
;;;
;;; Revision 1.11  1993/04/22  12:26:24  hk
;;; Neue Variablen *lisp-def* und *sys-def*,
;;; *SYSTEM-HEADER-FILES* und *SYS-FUN-DEF* gestrichen.
;;;
;;; Revision 1.10  1993/02/16  15:26:46  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.8  1993/01/25  15:10:27  sma
;;; C-Zielsystem-Konstanten
;;;
;;; Revision 1.7  1993/01/11  16:48:01  hk
;;; getenv -> c-getenv
;;;
;;; Revision 1.6  1992/10/02  14:19:00  hk
;;; *warn-unbound-vars* neu, in get-var-bind benutzt.
;;;
;;; Revision 1.5  1992/08/05  13:36:50  kl
;;; Dateiname "src/sys-fun-def" in "src/compiler/sys-fun.def" geaendert.
;;;
;;; Revision 1.4  1992/08/05  13:22:30  kl
;;; *** empty log message ***
;;;
;;; Revision 1.3  1992/08/05  13:07:35  kl
;;; Fehlende schliessende Klammer eingefuegt.
;;;
;;; Revision 1.2  1992/08/05  09:43:21  hk
;;; *SYS-FUN-DEF* definiert.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Eine Liste der LISP-Dateien, die Definitionen enthalten, die vor einer
;; Uebersetzung geladen werden muessen. Sie werden in setup-clicc-pathnames
;; gesetzt.
;;------------------------------------------------------------------------------
(defvar *LISP-DEF*)
(defvar *SYS-DEF*)

;;------------------------------------------------------------------------------
;; Pfadnamen fuer CLiCC-Directories werden gesetzt.
;; Dies sind die Wurzel des CLiCC-Filebaums und die Stellen wo lisp.def/syntax
;; und sys.def/syntax zu finden sind. Die Pfadnamen fuer die Verzeichnisse des
;; Laufzeitsystems und den Quellcode des Compilers werden in clcload.lisp in
;; der Funktion setup-clcload-pathnames gesetzt.
;;------------------------------------------------------------------------------
(defun setup-clicc-pathnames () 
  ;; Den Pfad bekanntgeben, an dem CLICC installiert ist. Zur Zeit wird der
  ;; Pfad aus der Environment-Variablen CLICCROOT ausgelesen.  Man koennte an
  ;; dieser Stelle auch direkt einen Pfadnamen angeben.
  ;; ---------------------------------------------------------------------------
  (setq *CLICC-PATH-STRING*
        (let ((str (rt::c-getenv "CLICCROOT"))
              len)
          (unless str (error "Environment variable CLICCROOT is not set"))
          (setq len (length str))
          (when (or (zerop len)
                    (not (eql #\/ (elt str (1- len)))))
            (setq str (concatenate 'string str "/")))
          str))
  (setq *LISP-DEF* (concatenate 'string *CLICC-PATH-STRING* "lib/lisp"))
  (setq *SYS-DEF* (concatenate 'string *CLICC-PATH-STRING* "lib/sys")))

(setup-clicc-pathnames)

;;------------------------------------------------------------------------------
;; Funktion zum Berechnen des Dateinamens, in die der Code fuer einzelne
;; Funktionen geschrieben wird.
;;------------------------------------------------------------------------------
(defconstant *MAX-ARCHIVE-NAME* 13)
(defun calc-fun-filename (fun-name base-name path count)
  (concatenate
   'string
   path
   (if (<= (length fun-name) *MAX-ARCHIVE-NAME*)
       fun-name
       (let* ((str (princ-to-string count))
              (max-len (- *MAX-ARCHIVE-NAME* (length str))))
         (when (> (length base-name) max-len)
           (setq base-name (subseq base-name 0 max-len)))
         (concatenate 'string base-name str)))
   ".c"))

;;------------------------------------------------------------------------------
;; Funktion zum Berechnen des Dateinamens, wenn der Code auf mehrere Dateien
;; verteilt wird
;;------------------------------------------------------------------------------
(defconstant *MAX-FILE-NAME* 8)
(defun calc-next-C-filename (file-name count)
  (concatenate
   'string
   (if (zerop count)
       file-name
       (let* ((base-name (strip-path file-name))
              (path (get-path file-name))
              (str (princ-to-string count))
              (len (length str)))
         (when (> (length base-name) (- *MAX-FILE-NAME* len))
           (setq base-name (subseq base-name 0 (- *MAX-FILE-NAME* len))))
         (concatenate 'string path base-name str)))
   ".c"))

;;------------------------------------------------------------------------------
;; Konstanten des C-Zielsystems (aus <limits.h>)
;;------------------------------------------------------------------------------
(defconstant C-most-negative-fixnum -2147483648)
(defconstant C-most-positive-fixnum  2147483647)


;;------------------------------------------------------------------------------
;; Fixnum-Test des Zielsystems
;; Dieser Test wird in der Funktion zs-type-of (tipass1.lisp) von der 
;; Typinferenz verwendet.
;;------------------------------------------------------------------------------
(defun target-fixnump (anything)
  (and (integerp anything)
       (>= anything C-most-negative-fixnum)
       (<= anything C-most-positive-fixnum)))


;;------------------------------------------------------------------------------
(provide "config")
