;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Tests fuer das Objectsystem
;;;
;;; $Revision: 1.5 $
;;; $Log: clos-test.lisp,v $
;;; Revision 1.5  1993/04/22  07:18:25  kl
;;; Test fuer Behandlung der Parameter in spez. Lambda-Listen eingefuegt.
;;;
;;; Revision 1.4  1993/02/16  17:12:31  hk
;;; Revision Keyword eingefuegt, #+, #- entfernt.
;;;
;;; Revision 1.3  1993/01/13  15:19:43  ft
;;; Notloesung fuer das 'Package-Problem' installiert.
;;;
;;; Revision 1.2  1993/01/12  10:05:50  ft
;;; (in-package USER) eingefuegt.
;;;
;;; Revision 1.1  1993/01/12  09:44:04  ft
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;------------------------------------------------------------------------------
;; Klassendefinitionen
;;------------------------------------------------------------------------------
(defclass c1 () ((s1 :accessor ?s1 :initarg :s1)))

(defclass c2 (c1) ())

(defclass c3 (c1) ())

(defclass c4 (c3) ())

(defclass c5 (c2) ())

;;------------------------------------------------------------------------------
;; Methodendefinitionen ...
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ... fuer den Test der Slot-Verwaltung
;;------------------------------------------------------------------------------
(defmethod m ((a1 c1)) (?s1 a1))

;;------------------------------------------------------------------------------
;; ... fuer den Test von CALL-NEXT-METHOD
;;------------------------------------------------------------------------------
(defmethod n ((a c1)) 'c1)
(defmethod n ((a c2)) (cons 'c2 (call-next-method)))
(defmethod n ((a c3)) (cons 'c3 (call-next-method)))
(defmethod n ((a c4)) (cons 'c4 (call-next-method)))

;;------------------------------------------------------------------------------
;; ... fuer die Behandlung der Parameter in spezialisierten Lambda-Listen
;;------------------------------------------------------------------------------
(defmethod o ((p1 c1) &optional p2) 
  (if p2
      (cons p2 (list 'b))
      (list 'b 'a)))

(defmethod o ((p1 c2) &optional p2) 
  (if p2
      (cons p2 (list 'd))
      (list 'd 'c)))

;;------------------------------------------------------------------------------
(defmethod p ((p1 c1) &key key1 key2) (list (n p1) key1 key2))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(clicc-test "clos"
            ";;; Some Tests for the Object-System."
            (((m (make-instance 'c1 :s1 'INITIAL))      INITIAL)
             ((n (make-instance 'c4))                   (C4 C3 . C1))
             ((o (make-instance 'c3))                   (b a))
             ((o (make-instance 'c3) 'a)                (a b))
             ((o (make-instance 'c5))                   (d c))
             ((o (make-instance 'c5) 'c)                (c d))
             ((p (make-instance 'c1) :key2 'b :key1 'a) (c1 a b))
             ))

;;------------------------------------------------------------------------------

(provide "clos-test")

