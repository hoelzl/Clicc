;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Tests zu Lambda-Listen in unbenannten und benannten Funktionen
;;;            Makrodefinitionen.
;;;            In Zukunft sollen hier auch die Lambda-Listen generischer von
;;;            Funktionen und Methodendefinitionen getestet werden.
;;;
;;; $Revision: 1.4 $
;;; $Log: lambda.lisp,v $
;;; Revision 1.4  1993/02/16  17:15:49  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.3  1992/09/25  08:05:53  hk
;;; Neuer Test mit special und closure Variabalen.
;;;
;;; Revision 1.2  1992/09/09  13:12:04  kl
;;; Macrotests eingefuegt.
;;;
;;; Revision 1.1  1992/09/04  16:30:40  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; Definition einer benannten Funktion mit allen Parameterarten:
;;------------------------------------------------------------------------------
(defun f1 (a b &optional c (d 44) (e 55 e-supplied-p)
               &rest rest
               &key f (g 77 g-supplied-p) ((:h h) 88 h-supplied-p)
               &allow-other-keys
               &aux (i 99) (j (list e (not (null e-supplied-p)) f g h i)))
  (list a b c d e (not (null e-supplied-p)) rest
        f g (not (null g-supplied-p)) h (not (null h-supplied-p)) i j))
  

;;------------------------------------------------------------------------------
;; Aufruf der Tests zu f1:
;;------------------------------------------------------------------------------
(clicc-test "Lambda f1" 
            ";;; Tests lambda-lists in function definitions."
            (((f1 1 2)
              (1 2 nil 44 55 nil nil nil 77 nil 88 nil 99 (55 nil nil 77 88 99)))
             ((f1 1 2 3 4 5 :f 6 :g 7 :h 8 :i 9)
              (1 2 3 4 5 T (:F 6 :G 7 :H 8 :I 9) 6 7 T 8 T 99 (5 T 6 7 8 99)))
             ((f1 1 2 3 4)
              (1 2 3 4 55 NIL NIL NIL 77 nil 88 nil 99 (55 NIL NIL 77 88 99)))
             ((f1 1 2 3 4 5 :i 9 :f 6)
              (1 2 3 4 5 T (:I 9 :F 6) 6 77 nil 88 nil 99 (5 T 6 77 88 99)))
             )
            )

;;------------------------------------------------------------------------------
;; Definition einer benannten Funktion mit allen Parameterarten, wobei die
;; Argumente nach der Uebergabe auf dem Stack teilweise in den Heap oder in
;; die Wertzelle von Symbolen kopiert werden.
;;------------------------------------------------------------------------------
(defun f2 (a b &optional c (d 44) (e 55 e-supplied-p)
             &rest rest
             &key f (g 77 g-supplied-p) ((:h h) 88 h-supplied-p)
             &allow-other-keys
             &aux (i 99) (j (list a c e g (not (null g-supplied-p)) i)))
  (declare (special a c e g g-supplied-p i))
  #'(lambda () 
      (list b d (not (null e-supplied-p)) rest
            f h (not (null h-supplied-p)) j)))

;;------------------------------------------------------------------------------
;; Aufruf der Tests zu f2:
;;------------------------------------------------------------------------------
(clicc-test "Lambda f2" 
            ";;; Tests lambda-lists with special and closure variables."
            (((funcall (f2 1 2))
              (2 44 nil nil nil 88 nil (1 nil 55 77 nil 99)))
             ((funcall (f2 1 2 3 4 5 :g 7 :i 9 :f 6 :h 8))
              (2 4 T (:G 7 :I 9 :F 6 :H 8) 6 8 T (1 3 5 7 T 99)))
             ((funcall (f2 1 2 3 4))
              (2 4 nil nil nil 88 nil (1 3 55 77 nil 99)))
             ((funcall (f2 1 2 3 4 5 :f 6 :XX (cons 'x 'x) :i 9
                           :allow-other-keys t))
              (2 4 T (:F 6 :XX (x . x) :I 9 :allow-other-keys t)
                 6 88 nil (1 3 5 77 nil 99)))))



;;------------------------------------------------------------------------------
;; Makrodefinition mit allen Parameterarten:
;;------------------------------------------------------------------------------
(defmacro m1 (name a b &optional c (d 44) (e 55 e-supplied-p)
                   &rest rest
                   &key f (g 77) ((:h h) 88)
                   &allow-other-keys
                   &aux (i 99) (j (list e (not (null e-supplied-p)) f g h i)))
  `(defun ,name ()
     (list ',a ',b ',c ',d ',e ,(not (null e-supplied-p)) 
      ',rest ',f ',g ',h ',i ',j)))


(m1 fm1 1 2)
(m1 fm2 1 2 3 4 5 :f 6 :g 7 :h 8 :i 9)
(m1 fm3 1 2 3 4)
(m1 fm4 1 2 3 4 5 :i 9 :f 6)

;;------------------------------------------------------------------------------
;; Aufruf der Tests zu f1:
;;------------------------------------------------------------------------------
(clicc-test "Lambda m1" 
            ";;; Tests lambda-lists in macro definitions."
            (((fm1)
              (1 2 nil 44 55 nil nil nil 77 88 99 (55 nil nil 77 88 99)))
             ((fm2)
              (1 2 3 4 5 T (:F 6 :G 7 :H 8 :I 9) 6 7 8 99 (5 T 6 7 8 99)))
             ((fm3)
              (1 2 3 4 55 NIL NIL NIL 77 88 99 (55 NIL NIL 77 88 99)))
             ((fm4)
              (1 2 3 4 5 T (:I 9 :F 6) 6 77 88 99 (5 T 6 77 88 99)))
             )
            )

;;------------------------------------------------------------------------------

(provide "lambda")
