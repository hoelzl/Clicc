;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Tests zu Seiteneffekten.
;;;
;;; $Revision: 1.6 $
;;; $Log: side-effect.lisp,v $
;;; Revision 1.6  1993/10/08  19:56:49  kl
;;; Test fuer Seiteneffekt in einer von maphash applizierten lokalen Funktion.
;;;
;;; Revision 1.5  1993/10/05  16:09:25  hk
;;; Test auf Kontrollfluss-Seiteneffekt
;;;
;;; Revision 1.4  1993/05/22  12:26:29  ft
;;; Test se6 wieder enabled.
;;;
;;; Revision 1.3  1993/05/19  07:21:38  kl
;;; Einen weiteren Test zugefuegt. Der Test zu Seiteneffekten in Slot-
;;; Initialisierungsausdruecken bleibt auskommentiert, weil sonst ein Fehler
;;; in der Codegenerierung auftritt.
;;;
;;; Revision 1.2  1993/05/11  14:24:32  kl
;;; Zwei Tests, fuer die falsche Code generiert wird auskommentiert.
;;;
;;; Revision 1.1  1993/04/20  15:27:51  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; Definitionen zu den Bindungstests:
;;------------------------------------------------------------------------------

(defvar *global*)

;;------------------------------------------------------------------------------
;; Seiteneffekt auf globale Variable in globaler Funktion.
;;------------------------------------------------------------------------------
(defun se1a (a b)
  (setf *global* 'wow)
  (+ a b))

(defun se1b ()
  (let ((*global* 1))
    (se1a 2 3)
    (if (numberp *global*)
        'wrong
        'right)))


;;------------------------------------------------------------------------------
;; Seiteneffekt auf globale Variable in lokaler Funktion.
;;------------------------------------------------------------------------------
(defun se2a (a b)
  (labels ((se2aa (l) 
             (dolist (e l)
               (setf *global* 'wow)
               (+ a b))))

    (se2aa '(1 2 3))))

(defun se2b ()
  (setf *global* 1)
  (se2a 1 2)
  (if (numberp *global*)
      'wrong
      'right))

;;------------------------------------------------------------------------------
;; Seiteneffekt auf lokale Variable in lokaler Funktion
;;------------------------------------------------------------------------------
(defun se3 ()
  (let ((local '(1 2))
        (sum   0))

    (labels ((se3a ()
               (dolist (e local)
                 (unless (eq e 'a)
                   (return (setf local 'wow)))
                 (incf sum))))

      (se3a)
      (if (listp local)
          'wrong
          'right))))

             

;;------------------------------------------------------------------------------
;; Seiteneffekt auf globale Variable im Initialisierungsausdruck eines 
;; optionalen Parameters.
;;------------------------------------------------------------------------------
(defun se4a (a &optional (b (progn (setf *global* 'wow) 3)))
  (cons a b))

(defun se4b ()
  (let ((*global* 1))
    (se4a 1)
    (if (numberp *global*)
        'wrong
        'right)))


;;------------------------------------------------------------------------------
;; Destruktiver Seiteneffekt auf eine Liste.
;;------------------------------------------------------------------------------
(defun se5a (l)
  (setf (first l) 'wow))


(defun se5b (a)
  (let ((b (cons '3 a)))
    (se5a a)
    (if (numberp (second b))
        'wrong
        'right)))

;;------------------------------------------------------------------------------
;; Seiteneffekt auf eine globale Variable im Initialisierungsausdruck des
;; Slots einer Klasse.
;;------------------------------------------------------------------------------
(defclass se6-class ()
  ((slot1 :initform (progn (se1a 1 2) 3))))


(defun se6 ()
  (let ((*global* 1))
    (make-instance 'se6-class)
    (if (numberp *global*)
      'wrong
        'right)))

;;------------------------------------------------------------------------------
;; Kontrollfluss-Seiteneffekt
;;------------------------------------------------------------------------------
(defun se7 ()
  (let ((v 'right))
   (tagbody
      (let ((x (go end)))
        (setq v 'wrong)
        (setq v x))
    end)
   v))


;;------------------------------------------------------------------------------
;; Seiteneffekt in einer von maphash applizierten Funktion
;;------------------------------------------------------------------------------
(defun se8 ()
  (let ((ht (make-hash-table))
        (a nil))
    (setf (gethash 's ht) 'linear)
    (maphash #'(lambda (var value)
                 (IF (eq value 'linear)
                     (setq a (cons var a))))
             ht)
    (if a
        'right
        'wrong)))


;;------------------------------------------------------------------------------
;; Aufrufe der Seiteneffekttests:
;;------------------------------------------------------------------------------
(clicc-test 
 "Side effect" 
 ";;; Tests the combination of side effect analysis, type inference~%~
  ;;; and optimizations using the infered informations.~%~
  ;;; The functions contain different forms of side effects and ~%~
  ;;; type tests."
 (((se1b)        right "side effect in global function")
  ((se2b)        right "side effect in local function")
  ((se3)         right "side effect in local function")
  ((se4b)        right "side effect in initform")
  ((se5b '(2 1)) right "destructive side effect")
  ((se6)         right "side effect in slot-initform")
  ((se7)         right "side effect of go")
  ((se8)         right "side effect in maphash")
  ))
              
;;------------------------------------------------------------------------------

(provide "side-effect")
