;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung: Continuations, Spruenge
;;;
;;; $Revision: 1.18 $
;;; $Log: cgblock.lisp,v $
;;; Revision 1.18  1994/01/07  11:36:41  hk
;;; In cg-app(cont) für das Zwischenergebnis bei einem nichtlokalen Sprung
;;; (stacktop-result-location) statt (stacktop-location) verwendet.
;;;
;;; Revision 1.17  1993/09/09  09:59:50  uho
;;; Funktion 'CC-caller-stack' nach cg-code verschoben. Dadurch entfaellt
;;; auch die Sonderbehandlung fuer CMU-CL.
;;;
;;; Revision 1.16  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.15  1993/04/07  16:16:21  hk
;;; Fehlermeldung verschoenert.
;;;
;;; Revision 1.14  1993/02/16  16:08:21  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.13  1992/11/05  09:04:19  kl
;;; Labels fuer CMU-Lisp herausgezogen.
;;;
;;; Revision 1.12  1992/10/02  14:07:22  hk
;;; Fehlerbehandlung jetzt lokal
;;;
;;; Revision 1.11  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.10  1992/08/11  12:30:18  hk
;;; C-Ln --> C-Decl, falls lokale variablen deklariert werden.
;;;
;;; Revision 1.9  1992/08/10  11:55:15  hk
;;; *mv-spec* und *mv-produced* gestrichen, da Analyse komplett in Pass3.
;;;
;;; Revision 1.8  1992/08/10  10:29:27  hk
;;; Labels herausgezogen fuer CMU-Lisp.
;;;
;;; Revision 1.7  1992/07/21  13:58:25  hk
;;; Semikolon hinter "goto Marke" vergessen.
;;;
;;; Revision 1.6  1992/06/11  11:14:39  hk
;;; cg-error -> error.
;;;
;;; Revision 1.5  1992/06/10  14:56:46  hk
;;; Schreibfehler.
;;;
;;; Revision 1.4  1992/06/04  15:40:49  hk
;;; Namen der Marken bei Continuations und Tagbody vor deren Anwendung
;;; bekanntgeben.
;;;
;;; Revision 1.3  1992/06/04  14:50:15  hk
;;; In cg-app (cont) die Ueberpruefung der Anzahl der Arg. korrigiert.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; let/cc
;;------------------------------------------------------------------------------
(defmethod cg-form ((form let/cc-form))
  (let ((cont (?cont form))
        (C-new_cont "new_cont")
        (C-caller_base "caller_base")
        (C-last_cont "last_cont"))
    (unless (?only-local cont)
      (C-blockstart)
      (C-VarDecl "CONTENV" C-new_cont)
      (C-PtrDecl "CL_FORM" C-caller_base)

      (C-Assign (CC-StructKomp C-new_cont "bind_top") "bind_top")
      (C-Assign (CC-StructKomp C-new_cont "last") C-last_cont)
      (C-MacroCall "LOAD_UNIQUE_TAG" (CC-StackTop))
      (cg-var-bind cont *stack-top*)
      (incf *stack-top*)

      (C-Assign C-caller_base
                (CC-Cast "CL_FORM *"
                         (CC-MacroCall "SETJMP"
                                       (CC-StructKomp C-new_cont "jmp_buf"))))
      (C-if (CC-op== C-caller_base "NULL"))
      (C-Blockstart)

      ;; Nicht von call_cont
      ;;--------------------
      (C-Assign C-last_cont (CC-Address C-new_cont)))

    ;; Die aktuelle Hoehe des Binding-Stacks merken, damit spaeter bei einem
    ;; lokalen Sprung bekannt ist, ob bzw. wieweit er abgearbeitet werden muss.
    ;; Merken, an welcher Stelle der lokale Sprung sein Resultat erzeugen soll.
    ;;-------------------------------------------------------------------------
    (setf (?binding-stack-level cont) *special-count*)
    (setf (?result-spec cont) *result-spec*)

    ;; Namen der Marke fuer lokale Spruenge vor
    ;; der Bearbeitung des Rumpfes bekanntgeben.
    ;;------------------------------------------
    (setf (?adr cont) (CC-NameConc "RETURN" (incf *cont-counter*)))

    ;; den Rumpf des Blocks uebersetzen
    ;;---------------------------------
    (cg-form (?body form))
    (when (eq *result-spec* 'C-bool)
      (to-bool-result))

    ;; Marke fuer lokale Spruenge generieren.
    ;;---------------------------------------
    (C-Label (?adr cont))

    (unless (?only-local cont)
      (C-Assign C-last_cont (CC-StructKomp C-new_cont "last"))
      (C-blockend)
      (C-else)
      (C-blockstart)
      (C-Assign C-last_cont (CC-StructKomp C-new_cont "last"))

      ;; Nicht lokaler Kontroll Transfer ?
      ;;---------------------------------
      (C-if (CC-MacroCall "EQ" (CC-caller-stack 0 C-caller_base) 
                          (CC-static cont)))
      (C-blockstart)

      ;; Passende Continuation gefunden,
      ;; Resultat an die gewuenschte Position kopieren.
      ;;-----------------------------------------------
      (case *result-spec*
        ((nil) nil)
        (C-BOOL (C-Assign C-bool_result (CC-make-bool 
                                         (CC-caller-stack 1 C-caller_base)))
                (setq *C-bool* C-bool_result))
        (T (C-copy (CC-caller-stack 1 C-caller_base) 
                   (CC-dest *result-spec*))))
      (C-blockend)
      (C-else)
      (C-blockstart)
      (C-Call "call_cont" C-caller_base)
      (C-blockend)
      (C-blockend)

      (decf *stack-top*)
      (C-blockend))))

;;------------------------------------------------------------------------------
;; Aufruf einer Continuation
;;------------------------------------------------------------------------------
(defmethod cg-app ((cont cont) args app)
  (when (?downfun-list app)
    (error "downfuns in continuation calls are not implemented"))

  (cond    
    ((eql *level* (?level cont))

     (let ((*result-spec* (?result-spec cont)))

       ;; Das Resultat gleich an der Resultatsposition der Cont. erzeugen.
       ;;-----------------------------------------------------------------
       (when (not (eql (length args) 1))
         (cg-error "continuation has been called with ~a arguments"
                   (length args))
         (when (null args)
           (push empty-list args)))
       (cg-form (first args))
       (when (eq *result-spec* 'C-bool)
         (to-bool-result)))
     (C-restore-special2 (?binding-stack-level cont))

     ;; An das Ende des Rumpfes von let/cc springen.
     ;;---------------------------------------------
     (C-goto (?adr cont)))

    (t

     ;; Kennung fuer nicht lokalen Sprung erzeugen
     ;;---------------------------------------
     (C-Copy (CC-static cont) (CC-StackTop))
     (incf *stack-top*)

     ;; Resultat erzeugen.
     ;;-------------------
     (let ((*result-spec* (stacktop-result-location)))
       (cg-form (first args)))
     (decf *stack-top*)

     (C-Call "call_cont" (CC-StackTop)))))

;;------------------------------------------------------------------------------
;; Verwendung einer Continuation als funktionales Argument
;;------------------------------------------------------------------------------
(defmethod cg-form ((form cont))
  (to-result-loc form))

;;------------------------------------------------------------------------------
;; tagbody (nur lokale Spruenge)
;;------------------------------------------------------------------------------
#+CMU
(defun gen-tag (tagged-form)
  (C-Label (?adr tagged-form)))

;;------------------------------------------------------------------------------
(defmethod cg-form ((form tagbody-form))

  ;; *special-count* : aktuelle Anzahl der special Variablen muss in GO
  ;; bekannt sein, um die neu gebundenen special Variablen restaurieren
  ;; zu koennen.
  ;;-------------------------------------------------------------------
  (setf (?binding-stack-level form) *special-count*)

  ;; Die Namen der Marken vor der Uebersetzung des Rumpfes bekanntgeben
  ;;-------------------------------------------------------------------
  (incf *tagbody-counter*)
  (let ((tag-counter 0))
    (dolist (tagged-form (?tagged-form-list form))
      (setf (?adr tagged-form)
            (CC-NameConc "M" *tagbody-counter* "_" (incf tag-counter)))))
    
  (let ((*result-spec* nil))
    (cg-form (?first-form form)))

  (labels (#-CMU
           (gen-tag (tagged-form)
             (C-Label (?adr tagged-form))))
              
    (mapc-progn-form-list (?tagged-form-list form)
                          #'(lambda (tagged-form)
                              (gen-tag tagged-form)
                              (let ((*result-spec* nil))
                                (cg-form (?form tagged-form))))

                          ;; Die letzte Tagged-Form liefert das Resultat,
                          ;; im Frontend gewoehnlich auf nil gesetzt.
                          ;;-----------------------------------------
                          #'(lambda (tagged-form)
                              (gen-tag tagged-form)
                              (cg-form (?form tagged-form))))))

;;------------------------------------------------------------------------------
;; go (nur lokale Spruenge)
;;------------------------------------------------------------------------------
(defmethod cg-form ((form tagged-form))
  (cond
    ((eql *level* (?level (?tagbody form)))

     ;; lokales GO
     ;; Vor dem Sprung die Eintraege auf dem Binding-Stack abarbeiten, die
     ;; seit der Aktivierung des Tagbody angelegt wurden.
     ;;-------------------------------------------------------------------
     (C-restore-special2 (?binding-stack-level (?tagbody form)))
     (C-goto (?adr form)))

    (t (error "go out of function should have been transformed ~
                  into call of continuation"))))

;;------------------------------------------------------------------------------
(provide "cgblock")
     
