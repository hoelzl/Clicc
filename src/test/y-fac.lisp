;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Eine Implementation der Fakultaetsfunktion.
;;;            Verwendet den Y-Kombinator, um Rekursion auszudruecken.
;;;
;;; $Revision: 1.7 $
;;; $Log: y-fac.lisp,v $
;;; Revision 1.7  1994/05/19  14:45:21  hk
;;; Fakult"at mit Zahlen als (Rest)-Listen implementiert hinzugef"ugt
;;;
;;; Revision 1.6  1993/02/16  17:19:06  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.5  1992/09/04  12:20:39  kl
;;; Einige zusaetzliche Tests und in-package eingefuegt.
;;;
;;; Revision 1.4  1992/08/20  16:33:27  kl
;;; Aufruf des clicc-test eingebaut.
;;;
;;; Revision 1.3  1992/08/14  10:37:09  uho
;;; und wieder entfernt.
;;;
;;; Revision 1.2  1992/08/14  10:35:12  uho
;;; leere Zeile eingefuegt.
;;;
;;; Revision 1.1  1992/08/11  10:49:50  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; Implementation der Fakultaetsfunktions mittels des Y-Kombinators
;;------------------------------------------------------------------------------
(defun fak (n)
  (let* ((Y #'(lambda (f)
                (funcall #'(lambda (g) (funcall g g))
                         #'(lambda (x)
                             (funcall f #'(lambda ()
                                            (funcall x x)))))))
         (fac (funcall Y #'(lambda (fg)
                             #'(lambda (n)
                                 (if (= n 0)
                                     1
                                     (* n (funcall (funcall fg) (- n 1)))))))))
    (funcall fac n)))


;;------------------------------------------------------------------------------
;; Einige Testaufrufe dazu:
;;------------------------------------------------------------------------------
(clicc-test "Y-fac" 
            ";;; Here fac is implemented with the Y Combinator."
            (((fak  0)         1)
             ((fak  1)         1)
             ((fak  3)         6)
             ((fak  4)        24)
             ((fak  5)       120)
             ((fak  6)       720)
             ((fak  7)      5040)
             ((fak  8)     40320)
             ((fak  9)    362880)
             ((fak 10)   3628800)
             ((fak 11)  39916800)
             ((fak 12) 479001600)
             )
            :test #'eql
            )
                  
;;------------------------------------------------------------------------------
;; Implementation der Fakultaetsfunktions mittels des Y-Kombinators
;; wobei die Zahl n als Liste der L"ange n dargestellt wird.
;;------------------------------------------------------------------------------
(defun incl   (l) (cons 1 l))
(defun decl   (l) (cdr l))
(defun addl   (m n) (if (zerolp m) n (addl (decl m) (incl n))))
(defun multl  (m n) (if (onelp m) n (addl n (multl (decl m) n))))
(defun zerolp (l) (null l))
(defun onelp  (l) (zerolp (decl l)))
(defconstant zerol '())

(defun fakl (n)
  (let* ((Y #'(lambda (f)
                (funcall #'(lambda (g) (funcall g g))
                         #'(lambda (x)
                             (funcall f #'(lambda ()
                                            (funcall x x)))))))
         (fac (funcall Y #'(lambda (fg)
                             #'(lambda (&rest n)
                                 (if (zerolp n)
                                     (incl zerol)
                                     (multl n (apply (funcall fg) (decl n)))))))))
    (apply fac n)))

;;------------------------------------------------------------------------------
;; Einige Testaufrufe dazu:
;;------------------------------------------------------------------------------
(clicc-test "Y-fac-list" 
";;; Here fac is implemented with the Y Combinator and numbers are ~%
implemented as lists"
            (((fakl '())      (1))
             ((fakl '(1))     (1))
             ((fakl '(1 1 1)) (1 1 1 1 1 1))
             )
            :test #'equal
            )
                       
;;------------------------------------------------------------------------------

(provide "y-fac")
