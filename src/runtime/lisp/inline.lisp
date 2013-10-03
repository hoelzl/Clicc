;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Definition der Lisp Funktionen, deren Aufrufe vom Compiler
;;;            direkt in entsprechenden C-Code ueberfuehrt werden. Nur wenn
;;;            diese Funktionen als funktionale Objekte verwendet werden, dann
;;;            kommen die Definitionen in dieser Datei zur Anwendung.
;;;
;;; $Revision: 1.9 $
;;; $Log: inline.lisp,v $
;;; Revision 1.9  1994/01/27  16:21:43  kl
;;; rt::fixnump eingetragen.
;;;
;;; Revision 1.8  1994/01/24  16:23:05  sma
;;; Not kann nicht mehr inline-compiliert werden. Siehe Log-Msg von pred.lisp.
;;;
;;; Revision 1.7  1993/12/16  16:42:31  pm
;;; Inlining der Foreign-Function-typtests herausgenommen.
;;;
;;; Revision 1.6  1993/12/12  16:18:18  sma
;;; symbolp war irrtümlich gelöscht. Wieder eingefügt.
;;;
;;; Revision 1.5  1993/12/09  17:05:41  sma
;;; Änderungen entsprechend compiler/cginline.lisp. stringp, arrayp,
;;; vectorp raus, simple-bit-vector-p rein.
;;;
;;; Revision 1.4  1993/09/28  15:20:04  pm
;;; C-Typtests werden jetzt inline-compiliert
;;;
;;; Revision 1.3  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.2  1993/04/22  08:53:16  hk
;;; (in-package RUNTIME) gestrichen.
;;;
;;; Revision 1.1  1993/04/07  09:12:26  hk
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(defun atom (x) (atom x))
(defun characterp (x) (characterp x))
(defun cons (x y) (cons x y))
(defun consp (x) (consp x))
(defun eq (x1 x2) (eq x1 x2))
(defun eql (x1 x2) (eql x1 x2))
(defun floatp (x) (floatp x))
(defun functionp (x) (functionp x))
(defun integerp (x) (integerp x))
(defun rt::fixnump (x) (rt::fixnump x))
(defun listp (x) (listp x))
(defun numberp (x) (numberp x))
(defun simple-string-p (x) (simple-string-p x))
(defun simple-vector-p (x) (simple-vector-p x))
(defun simple-bit-vector-p (x) (simple-bit-vector-p x))
(defun symbolp (x) (symbolp x))

