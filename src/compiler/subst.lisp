;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Ersetzung von Konstanten und Variablen durch ihre Werte.
;;;
;;; $Revision: 1.7 $
;;; $Log: subst.lisp,v $
;;; Revision 1.7  1994/03/03  13:53:44  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.6  1994/02/08  14:48:14  hk
;;; used Slot nur in defined-fun/sym/class erhöhen.
;;;
;;; Revision 1.5  1993/09/21  12:52:14  jh
;;; Voreilig eingecheckten Fehler beseitigt.
;;;
;;; Revision 1.4  1993/09/21  12:21:20  jh
;;; Statt inc-used-slot zu verwenden, werden die used- und read-slots jetzt
;;; direkt veraendert, da inc-used-slot mit Analysemarken arbeitet, die nicht
;;; mehr aktuell sein muessen.
;;;
;;; Revision 1.3  1993/09/20  14:30:23  jh
;;; Die used-Slots werden jetzt richtig verwaltet.
;;;
;;; Revision 1.2  1993/09/17  13:57:26  jh
;;; Test #'eq bei assoc angegeben.
;;;
;;; Revision 1.1  1993/06/30  15:23:03  jh
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "delete")

;;------------------------------------------------------------------------------
;; A-Liste, die Variablen einen Wert zuordnet, durch den Vorkommen der
;; Variablen im Quelltext unter geeigneten Umstaenden ersetzt werden duerfen
;;------------------------------------------------------------------------------
(defvar *SUBSTITUTION* ())

;;------------------------------------------------------------------------------
;; m1, m2 sind partielle Abbildungen, dargestellt als A-Listen,
;; m1 wird auf den Definitionsbereich von m2 eingeschraenkt
;;------------------------------------------------------------------------------
(defun restrict-map-to (m1 m2)
  (collect-if #'(lambda (pair) (member (car pair) m2 :key #'car)) m1))

;;------------------------------------------------------------------------------
;; Die akt. Substitution wird auf die total undef. Fkt. gesetzt.
;;------------------------------------------------------------------------------
(defmacro clear-substitution ()
  `(setq *SUBSTITUTION* ()))

;;------------------------------------------------------------------------------
(defun copy-is-eq-p (x)
  (or (and (simple-literal-p x) (not (float-form-p x)))
      (sym-p x)
      (fun-p x)
      (cont-p x)
      (class-def-p x)))


;;------------------------------------------------------------------------------
;; inc-uses erhoeht die used- bzw. read-Slots der eingesetzten Werte
;;------------------------------------------------------------------------------
(defun inc-uses (x)
  (if (cont-p x)
      (incf (?read x))
      (when (or (defined-sym-p x) (defined-fun-p x) (defined-class-p x))
        (incf (?used x)))))

;;------------------------------------------------------------------------------
;; subst-1form substituiert unter geeignten Umstaenden die Vorkommen von
;; Konstanten und Variablen mittels *SUBSTITUTION* in einem Zwischensprach-
;; knoten.
;;------------------------------------------------------------------------------

(defmethod subst-1form ((a-form form))
  a-form)

;;------------------------------------------------------------------------------
;; Variablen Referenz
;;------------------------------------------------------------------------------
(defmethod subst-1form ((a-var-ref var-ref))
  (let ((var-subst (subst-1form (?var a-var-ref))))
    (if (var-p var-subst)
        (progn
          (setf (?var a-var-ref) var-subst)
          a-var-ref)
        var-subst)))

(defmethod subst-1form ((a-dynamic dynamic))
  (let* ((value (?constant-value (?sym a-dynamic))))
    (if (copy-is-eq-p value)
        (progn
          (decf (?read a-dynamic))
          (inc-uses value)
          value)
        a-dynamic)))

(defmethod subst-1form ((a-static static))
  (let ((subst-bind (assoc a-static *SUBSTITUTION* :test #'eq)))
    (if subst-bind
        (progn
          (decf (?read a-static))
          (let ((value (cdr subst-bind)))
            (inc-uses value)
            value))
        a-static)))

;;------------------------------------------------------------------------------
;; angewandtes Vorkommen von named-const
;;------------------------------------------------------------------------------
(defmethod subst-1form ((a-defined-named-const defined-named-const))
  (let ((value (?value a-defined-named-const)))
    (if (copy-is-eq-p value)
        (progn
          (decf (?read a-defined-named-const))
          (inc-uses value)
          value)
        a-defined-named-const)))

;;------------------------------------------------------------------------------
;; setq
;;------------------------------------------------------------------------------
(defmethod subst-1form ((a-setq-form setq-form))
  (let ((var (?var (?location a-setq-form))))
    (when (static-p var)
      ;; Die Variable darf nach Ausfuehrung dieser Zuweisung nicht mehr
      ;; durch ihren initialen Wert ersetzt werden.
      ;;------------------------------------------------------------------
      (setq *SUBSTITUTION* (remove var *SUBSTITUTION* :key #'car))))
  a-setq-form)

;;------------------------------------------------------------------------------
(provide "subst")
