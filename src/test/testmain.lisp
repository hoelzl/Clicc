;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Funktionen fuer die Durchfuehrung der Tests und 
;;;            ein einfaches Menue zur Auswahl der einzelnen Tests.
;;;
;;; $Revision: 1.20 $
;;; $Log: testmain.lisp,v $
;;; Revision 1.20  1993/12/12  18:03:23  sma
;;; Tests für Arrays eingebunden (neue Datei arrays.lisp).
;;;
;;; Revision 1.19  1993/05/11  14:20:43  kl
;;; Test fuer die Seiteneffektanalyse wieder eingebunden.
;;;
;;; Revision 1.18  1993/05/04  06:11:58  ft
;;; clos-test und search wieder enabled.
;;;
;;; Revision 1.17  1993/04/23  10:13:41  ft
;;; side-effect, clos-test und search disabled, bis die entspr Fehler
;;; behoben sind.
;;;
;;; Revision 1.16  1993/04/20  15:29:24  kl
;;; Tests fuer die Seiteneffektanalyse eingebunden.
;;;
;;; Revision 1.15  1993/03/25  09:53:51  ft
;;; Die neuen 'search'-Tests eingebunden.
;;;
;;; Revision 1.14  1993/02/16  17:16:36  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.13  1993/01/25  10:49:19  kl
;;; Unter Lucid wird das Symbol USER::? uninterned.
;;;
;;; Revision 1.12  1993/01/12  09:40:39  ft
;;; CLOS Tests eingebunden.
;;;
;;; Revision 1.11  1993/01/06  17:12:51  kl
;;; clicc-test- in CLICC-TEST- umbenannt.
;;;
;;; Revision 1.10  1992/09/09  13:21:56  kl
;;; Ausgabeformat fuer unerwartete Resultate geaendert.
;;;
;;; Revision 1.9  1992/09/09  13:07:41  kl
;;; Testmacro um optionale Strings zu einzelnen Testaufrufen erweitert.
;;;
;;; Revision 1.8  1992/09/08  15:14:11  kl
;;; Neue Testdateien lambda und bindings eingehaengt.
;;;
;;; Revision 1.7  1992/09/02  08:51:19  kl
;;; Funktionalitaet erweitert und Menue verbessert.
;;;
;;; Revision 1.6  1992/08/27  12:06:08  kl
;;; Unnoetige Variablen entfernt und Kode verkuerzt.
;;;
;;; Revision 1.5  1992/08/26  12:30:21  kl
;;; *CLICC-TEST-ERRORS* eingefuehrt, Menue verschoenert, expert eingebunden.
;;;
;;; Revision 1.4  1992/08/26  09:50:25  kl
;;; Test-Makro erweitert, *CLICC-TESTS* und ein Testmenue eingefuehrt.
;;;
;;; Revision 1.3  1992/08/20  16:37:06  kl
;;; Neues Makro
;;;
;;; Revision 1.2  1992/08/16  16:29:38  kl
;;; *CLICC-TEST-PRINT-HEADERS* eingefuehrt.
;;;
;;; Revision 1.1  1992/08/16  16:26:21  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "USER")


;;------------------------------------------------------------------------------
;; Variablen fuer die CLiCC-Tests. 
;;------------------------------------------------------------------------------
(defvar *CLICC-TESTS*        nil)          ;Liste aller Tests
(defvar *CLICC-TEST-VERBOSE* nil)          ;Aufruf und Resultate ausgeben?
(defvar *CLICC-FAILED-TESTS* nil)          ;Liste der fehlgeschlagenen Tests



;;------------------------------------------------------------------------------
;; Makro fuer die CLiCC-Tests:
;;
;; `name' enthaelt den Namen des Tests, 
;; `description-format-string' einen Formattext zur Beschreibung des Tests, 
;; `function-call_expected-result_pairlist' eine Liste von Paaren von 
;; Funktionsaufrufen und zugehoerigen erwarteten Funktionsergebnissen. 
;; Als optionale dritte Komponente dieser Liste kann ein String zur naeheren 
;; Beschreibung des Testaufrufes angegeben werden.
;; Mit dem Schluesselwort `test' wird die Testfunktion zum Vergleich von
;; tatsaechlichem und erwartetem Funktionsergebnis angegeben. Der Standardtest
;; ist #'equal.
;;
;; Beispiele:
;; (clicc-test "Partitionen" 
;;             ";;; Returns the set of all partitions of a nonempty set."
;;             (((partitionen '())    ((())))
;;              ((partitionen '(a b)) (((A B)) ((A) (B))))
;;             )
;;             )
;;
;; (clicc-test "Fakultaet" 
;;             ";;; Liefert die Fakultaet zu einer natuerlichen Zahl."
;;             (((fakultaet 3)    6)
;;              ((fakultaet 5)  120) "optionaler zusaetzlicher Text")
;;             )
;;             :test #'eql)
;;------------------------------------------------------------------------------
(defmacro clicc-test (name 
                      description-format-string                  
                      function-call_expected-result_pairlist
                      &key (test '#'equal))
  (let ((function-name (intern (concatenate 'string "CLICC-TEST-" name))))
    `(progn
      (defun ,function-name ()
        (print-header ,name ,description-format-string)
        ,@(funcall #'make-tests 
                   name 
                   function-call_expected-result_pairlist
                   test))
      (pushnew (cons ,name #',function-name) *CLICC-TESTS*))))



;;------------------------------------------------------------------------------
;; Hilfsfunktion fuer das `clicc-test'-Makro
;;------------------------------------------------------------------------------
(defun make-tests (name function-call_expected-result_pairlist test)
  (let ((testno 0))
    (mapcar #'(lambda (function-call_expected-result)
                (incf testno)
                (let* ((raw-test-name   (format nil "~a ~2d" name testno))
                       (function-call   (first function-call_expected-result))
                       (function-name   (first function-call))
                       (quoted-args     (rest  function-call))
                       (expected-result 
                        (second function-call_expected-result))
                       (optional-test-description
                        (third function-call_expected-result))
                       (test-name
                        (if optional-test-description
                            (concatenate 'string raw-test-name " ("
                                         optional-test-description ")")
                            raw-test-name)))

                  `(progn 
                    (when *CLICC-TEST-VERBOSE*
                      (format T "~&;;; Testing ~a ... " ,test-name))
                    (do-test 
                        ,test-name 
                      ',function-call
                      ',expected-result
                      (funcall #',function-name ,@quoted-args)
                      ,test))))
            function-call_expected-result_pairlist)))



;;------------------------------------------------------------------------------
;; In `do-test' wird der eigentliche Test ausgewertet.
;;------------------------------------------------------------------------------
(defun do-test (name function-call expected-result result test)
  (let ((test-is-ok  (funcall test result expected-result)))

    (when *CLICC-TEST-VERBOSE*
      (format T "~A~%" (if test-is-ok "ok" "failed")))

    (unless test-is-ok
      (pushnew name *CLICC-FAILED-TESTS*)

      (format T "~&~%~
                 ;;; Function Call:   ~S~%~
                 ;;; Expected Result: ~S~%~
                 ;;; Result:          ~S~%~%"
              function-call
              expected-result
              result))))



;;------------------------------------------------------------------------------
;; `print-header' gibt den Kopf eines Tests aus, wenn *CLICC-TEST-PRINT-HEADERS*
;; ungleich nil ist.
;;------------------------------------------------------------------------------
(defun print-header (name description-format-string)
  (when *CLICC-TEST-VERBOSE*
    (format T "~&~%~%;;; ---------------------------------------------~
                         -------------------------------~%~
                     ;;; ~a test:~%~
                     ;;;~%"
            name)
    (format T description-format-string)
    (format T "~&;;; ---------------------------------------------------~
                     -------------------------~%")))
 



;;------------------------------------------------------------------------------
;; `print-test-result' gibt das Ergebnis eines oder mehrerer Tests aus.
;;------------------------------------------------------------------------------
(defun print-test-result ()
  (format t "~&~%;;; ---------------------------------------------~
                     -------------------------------~%~
                 ;;; Failed tests: ")

  (if (null *CLICC-FAILED-TESTS*)
      (format t "none~%")
      (dolist (name (reverse *CLICC-FAILED-TESTS*))
        (format t "~&;;;   ~A~%" name)))

  (format t "~&;;; ---------------------------------------------~
                   -------------------------------~%"))
 

             
;;------------------------------------------------------------------------------
;; `print-help' gibt eine kleine Hilfe fuer das Testmenue aus.
;;------------------------------------------------------------------------------
(defun print-help ()
  (format t "~&~%;;; ---------------------------------------------~
                     -------------------------------~%~
                 ;;; Type:~%~
                 ;;;   a number <n> to start the n-th test,~%~
                 ;;;   <A> to start all tests,~%~
                 ;;;   <V> to toggle the verbose mode,~%~
                 ;;;   <D> to display a list of all tests,~%~
                 ;;;   <H> for this help, or~%~
                 ;;;   <Q> to quit.~%~
                 ;;; ---------------------------------------------~
                     -------------------------------~%"))


   
;;------------------------------------------------------------------------------
;; Ein kleines Menue zum Starten ausgewaehlter oder aller Test.
;;------------------------------------------------------------------------------
(defun run-tests ()
  (let ((tests (reverse *CLICC-TESTS*))
        input)

    (flet ((display-tests ()
             (format t "~&~%~%  Available tests:~%~%")
             (dotimes (i (length tests))
               (format t "~&  Test ~2D:   ~A~%" (1+ i) (first (nth i tests))))))
   
      (display-tests)

      (loop
       (format t "~&~%  <number>  A)ll  V)erbose is ~A  D)isplay tests  H)elp  ~
                  Q)uit  choose: "
               (if *CLICC-TEST-VERBOSE* "on" "off"))

       (setf *CLICC-FAILED-TESTS* nil input (read))

       (case input
         (Q (return))
         (D (display-tests))
         (V (setf *CLICC-TEST-VERBOSE* (not *CLICC-TEST-VERBOSE*)))
         (A (dolist (test tests)
              (funcall (rest test)))
            (print-test-result))
         (H (print-help))
         (T (when (and (integerp input) 
                       (>= input 1) 
                       (<= input (length tests)))
              (funcall (rest (nth (1- input) tests))))))

       ))))


#+Lucid 
(unintern 'user::?)

;;------------------------------------------------------------------------------
;; Hier werden die einzelnen Testdateien geladen.
;;------------------------------------------------------------------------------
(require "lambda")
(require "bindings")
(require "side-effect")
(require "ai-course")
(require "expert")        ;Der expert-Test benoetigt Funktionen aus "ai-course".
(require "y-fac")
(require "clos-test")
(require "search")
(require "arrays")

;;------------------------------------------------------------------------------
;; Aufruf des Menues
;;------------------------------------------------------------------------------
(run-tests)
