;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (4. Type Specifiers)
;;;
;;; $Revision: 1.7 $
;;; $Log: typspc.lisp,v $
;;; Revision 1.7  1993/09/14  13:36:41  ft
;;; rt::type-error in rt::the-type-error umbenannt, da der Name in CLtL2
;;; schon vergeben ist.
;;;
;;; Revision 1.6  1993/09/13  15:09:14  ft
;;; Neue Funktion TYPE-ERROR hinzugefuegt.
;;;
;;; Revision 1.5  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.4  1993/04/22  10:36:43  hk
;;; (in-package "RUNTIME") -> (in-package "LISP").
;;;
;;; Revision 1.3  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.7 $ eingefuegt
;;;
;;; Revision 1.2  1992/06/11  10:21:35  hk
;;; Vorlaeufig string-char in character umbenannt.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export '(rt::the-type-error) "RT")

;;------------------------------------------------------------------------------
(defun normalize-type (type)
  (if (atom type)
    (case type
      ((array simple-array vector simple-vector string simple-string)
       (normalize-type (list type)) )
      (t type) )
    (let (type-specifier
          element-type
          dimensions
          (args (rest type)) )
      (tagbody
         (case (car type)
           ((string simple-string)
            (setq type-specifier
                  (if (eq (car type) 'string)
                    'array
                    'simple-array ))
;;; !!! gibts nicht mehr in CLTL2            
;;;-------------------------------------------
;;;            (setq element-type 'string-char)
            (setq element-type 'character)
            (setq dimensions
                  (list (if (atom args)
                          '*
                          (pop args) )))
            (when (not (null args)) (go INVALID-SEQTYPE)) )

           (simple-vector
            (setq type-specifier 'simple-array)
            (setq element-type t)
            (setq dimensions
                  (list (if (atom args)
                          '*
                          (pop args) )))
            (when (not (null args)) (go INVALID-SEQTYPE)) )

           (vector
            (setq type-specifier 'array)
            (setq element-type
                  (if (atom args)
                    '*
                    (pop args) ))
            (setq dimensions
                  (list (if (atom args)
                          '*
                          (pop args) )))
            (when (not (null args)) (go INVALID-SEQTYPE)))

           ((array simple-array)
            (setq type-specifier (car type))
            (setq element-type
                  (if (atom args)
                    '*
                    (pop args) ))
            (setq dimensions
                  (if (atom args)
                    '*
                    (pop args) ))
            (when (not (null args)) (go INVALID-SEQTYPE)) )

           (t (go INVALID-SEQTYPE)) )

         (return-from normalize-type
           (list type-specifier element-type dimensions) )
       INVALID-SEQTYPE
         (error "~S is an invalid sequence typespecifier." type) ))))

;;------------------------------------------------------------------------------
;; THE-TYPE-ERROR value type
;; Die Funktion wird vom dem aus THE expandierten Code im Fehlerfall aufgerufen
;;------------------------------------------------------------------------------
(defun rt:the-type-error (value type)
  (error "type error: ~S is not of type ~S" value type))

;;------------------------------------------------------------------------------


