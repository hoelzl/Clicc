;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Allgemeine Operatoren fuer die Lisp nahe Zwischensprache
;;;
;;; $Revision: 1.6 $
;;; $Log: zsops.lisp,v $
;;; Revision 1.6  1993/10/28  08:25:37  kl
;;; has-no-side-effect umbenannt.
;;;
;;; Revision 1.5  1993/10/18  08:12:10  hk
;;; Neue Funktion mapc-tagged-form-list.
;;;
;;; Revision 1.4  1993/10/14  11:50:59  hk
;;; Kommentar zu mapc-tagged-form-list ergänzt
;;;
;;; Revision 1.3  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.2  1993/02/16  16:08:38  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.1  1992/09/25  16:37:15  kl
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; self-evaluating-p
;;------------------------------------------------------------------------------
(defun self-evaluating-p (v)
  (or (simple-literal-p v)
      (structured-literal-p v)
      (sym-p v)
      (fun-p v)
      (class-def-p v)
      (cont-p v)))

;;------------------------------------------------------------------------------
;; Stellt fest, ob ein Ausdruck kopiert werden kann.
;;------------------------------------------------------------------------------
(defun may-be-copied (form)
  (or (self-evaluating-p form) (var-ref-p form) (named-const-p form)))

;;------------------------------------------------------------------------------
;; Ueberprueft, ob es sich um eine einfache Konstante handelt, d.h. ob die
;; Konstante dupliziert werden darf und trotzdem im Sinne von EQ (EQL)
;; identisch bleibt.
;;------------------------------------------------------------------------------
(defun simple-constant-p (object)
  (or (and (atom object)
           (or (integerp object) (characterp object)))
      (and (consp object)
           (eq (first object) 'QUOTE)
           (symbolp (second object)))))

;;------------------------------------------------------------------------------
;; Iteriert ueber die form-list einer Progn-Form, und appliziert fun bzw. 
;; fun-for-last auf die Elemente.
;; Resultat: Result der Applikation von fun-for-last auf das letzte Element.
;;------------------------------------------------------------------------------
(defun mapc-progn-form-list (form-list fun fun-for-last-element)
  (cond
    ((atom (cdr form-list)) 
     (funcall fun-for-last-element (car form-list)))
    (t 
     (funcall fun (car form-list))
     (mapc-progn-form-list (cdr form-list) fun fun-for-last-element))))

;;------------------------------------------------------------------------------
;; Iteriert ueber die tagged-form-list einer Tagbody-Form, und appliziert fun
;; bzw. fun-for-last auf den Slot form der Elemente.  Resultat: Result der
;; Applikation von fun-for-last auf die form des letzten Elements.
;;------------------------------------------------------------------------------
(defun mapc-tagged-form-list (tagged-form-list fun fun-for-last-element)
  (cond
    ((atom (cdr tagged-form-list)) 
     (funcall fun-for-last-element (?form (car tagged-form-list))))
    (t 
     (funcall fun (?form (car tagged-form-list)))
     (mapc-tagged-form-list (cdr tagged-form-list) fun fun-for-last-element))))

;;------------------------------------------------------------------------------
(provide "zsops")

