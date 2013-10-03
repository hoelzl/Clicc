;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Einfache Routinen zum Testen von Arrays.
;;;
;;; $Revision: 1.3 $
;;; $Log: arrays.lisp,v $
;;; Revision 1.3  1994/01/23  18:13:15  sma
;;; predicate --> (not (null predicate)), damit statt einem Wahrheitswert
;;; true wirklich das Symbol T geliefert wird.
;;;
;;; Revision 1.2  1993/12/15  11:56:49  sma
;;; Englische Testbeschreibungen.
;;;
;;; Revision 1.1  1993/12/12  18:04:54  sma
;;; Initial revision
;;;

(in-package "USER")

;;------------------------------------------------------------------------------
;; Anlegen, Displacen, Ajdusten und Zugriff auf Arrays.
;;------------------------------------------------------------------------------

(defun displace-arrays ()
  (let (a1 a2 c)
    (setq a1 (make-array 6 :adjustable t
                         :initial-contents '"Drache"))
    (setq a2 (make-array '(1 1 2) :displaced-to a1
                         :displaced-index-offset 3))
    (setf c (aref a2 0 0 0))
    (setf (aref a2 0 0 0) (aref a2 0 0 1))
    (setf (aref a2 0 0 1) c)
    (coerce (adjust-array a1 5) 'string)))

;;------------------------------------------------------------------------------
;; Fillpointer.
;;------------------------------------------------------------------------------

(defun fptest ()
  (let* ((v (make-array 3 :fill-pointer 1))
         (r (list (not (null (array-has-fill-pointer-p v))) (fill-pointer v))))
    (setf (fill-pointer v) 0)
    (setq r (append r (list (length v) (array-total-size v))))
    (setf (fill-pointer v) 2)
    (append r (list (length v) (fill-pointer v)))))


;;------------------------------------------------------------------------------
;; In Testmain einbinden
;;------------------------------------------------------------------------------
(clicc-test "Array1"
            ";;; Tests creation, displacing, adjusting and accessing of arrays"
            (((displace-arrays) "Drahc")))

(clicc-test "Vector1"
            ";;; Tests the handling of fill-pointers"
            (((fptest) (t 1 0 3 2 2))))

(provide "arrays")
