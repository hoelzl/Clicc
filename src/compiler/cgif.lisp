;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung, Special Form IF
;;;
;;; $Revision: 1.11 $
;;; $Log: cgif.lisp,v $
;;; Revision 1.11  1994/05/20  08:42:51  hk
;;; type-Slot ist nur g"ultig, wenn *optimize* = true.
;;;
;;; Revision 1.10  1994/02/17  10:13:11  hk
;;; Wenn das Pr"aikat eines if-Ausdrucks wiederum einen oder mehrere
;;; if-Ausdr"ucke enthält, dann wird in den "Asten der inneren
;;; if-Ausdr"ucke gleich zu dem entsprechenden then- oder else-Teil
;;; gesprungen, wenn bekannt ist, da"s der Wert True oder False ist.
;;;
;;; Keine eigene Funktion mehr f"ur *FLAT-IFS*.
;;;
;;; Revision 1.9  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.8  1993/02/16  15:50:38  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.7  1993/01/21  13:49:12  uho
;;; Optionale Generierung flacher ifs eingefuehrt (Funktion cd-if-flat)
;;;
;;; Revision 1.6  1992/09/25  17:20:12  kl
;;; Auf die neue Repraesentation der einfachen Literale umgestellt.
;;;
;;; Revision 1.5  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.4  1992/08/10  11:58:10  hk
;;; *mv-spec* und *mv-produced* gestrichen, da Analyse komplett in Pass3.
;;;
;;; Revision 1.3  1992/08/07  11:55:20  hk
;;; Dateikopf verschoenert.
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
(defvar *then-label*)
(defvar *else-label*)

;;------------------------------------------------------------------------------
(defmacro ensure-label (var prefix)
  `(unless ,var
    (setq ,var
     (CC-NameConc ,prefix (incf *if-counter*)))))

;;------------------------------------------------------------------------------
;; if test then [else]
;; Wenn pred wiederum einen oder mehrere if-Ausdr"ucke enthält, dann wird in
;; den "Asten der inneren if-Ausdr"ucke gleich zu dem entsprechenden then-
;; oder else-Teil gesprungen, wenn bekannt ist, da"s der Wert True oder False
;; ist.
;; Wenn zu beiden Zweigen nur gesprungen wird, dann wird kein C-if mehr
;; generiert, sondern am Ende des then-Teils wird ein Sprung hinter den
;; Else-Teil generiert, falls dieser nicht leer ist.
;;
;; Die Variable *FLAT-IFS* bestimmt, ob das C-else verwendet wird:
;; if <pred>        if !(<pred>) goto ELSE
;; {                {
;;    <then>           <then> goto ENDIF
;; }                }
;; else             ELSE:
;; {                {
;;    <else>          <else>
;; }                }
;;                  ENDIF:
;;------------------------------------------------------------------------------
(defmethod cg-form ((form if-form))
  (let ((then-label nil)
        (else-label nil)
        (endif-label nil)
        (need-if t)
        then-has-bool-result
        else-has-bool-result)

    (let ((*result-spec* 'C-bool)
          (*then-label* nil)
          (*else-label* nil)
          (*C-bool* 'none))
      (cg-form (?pred form))
      (setq then-label *then-label*
            else-label *else-label*)

      ;; Wenn *C-bool* nach der Übersetzung des Prädikats immer noch den
      ;; Wert 'none hat, dann wird kein Wert zuückgegeben und somit kein if
      ;; benötigt
      ;;--------------------------------------------------------------------
      (when (eq *C-bool* 'none)
        (setq need-if nil))

      (when need-if
        (if *FLAT-IFS*
            (C-flat-if *C-bool* (ensure-label else-label "ELSE"))
            (C-if *C-bool*))))

    (labels ((cg-then/else (form copy-to-bool-result)
               (cond
                 ((not (eq *result-spec* 'C-bool))
                  (cg-form form))
                 ((and *optimize* (zs-subtypep (?type form) null-t))
                  (let ((*result-spec* nil)) (cg-form form))
                  (unless *else-label*
                    (setq *else-label*
                          (CC-NameConc "ELSE" (incf *if-counter*))))
                  (C-goto *else-label*)
                  nil)
                 ((and *optimize* (not (zs-subtypep null-t (?type form))))
                  (let ((*result-spec* nil)) (cg-form form))
                  (unless *then-label*
                    (setq *then-label*
                          (CC-NameConc "THEN" (incf *if-counter*))))
                  (C-goto *then-label*)
                  nil)
                 (t (cg-form form)
                    (when copy-to-bool-result
                      (to-bool-result))
                    t))))

      (C-blockstart)
      (when then-label (C-label then-label))
      (setq else-has-bool-result
            (and (eq *result-spec* 'C-bool)
                 (not (and *optimize*
                           (or (zs-subtypep (?type (?else form)) null-t)
                               (not (zs-subtypep null-t
                                                 (?type (?else form)))))))))
      (setq then-has-bool-result
            (cg-then/else (?then form) else-has-bool-result))
      (C-blockend)

      (cond

        ;; Leerer Else-Teil: evtl. Label generieren
        ;;-----------------------------------------
        ((and (null *result-spec*) (null-form-p (?else form)))
         (when else-label (C-label else-label)))

        (T (if (and need-if (not *FLAT-IFS*))
               (C-else)
               (C-goto (ensure-label endif-label "ENDIF")))

           (C-blockstart)
           (when else-label (C-label else-label))
           (cg-then/else (?else form) then-has-bool-result)
           (C-blockend)
           (unless (and need-if (not *FLAT-IFS*)) (C-label endif-label))))

      ;; Wenn mindestens ein Zweig ein Resultat generiert hat und ein
      ;; Boolesches Resultat erwartet wird
      ;;----------------------------------
      (when (and (eq *result-spec* 'C-bool)
                 then-has-bool-result
                 else-has-bool-result)
        (setq *C-bool* C-bool_result)))))

;;------------------------------------------------------------------------------
;; Hilfsfunktion, weist der C-Variablen 'bool_result' den Wert des Ausdrucks,
;; der in *C-bool* angegeben ist zu.
;; Die Zuweisung "bool_result = bool_result;" wird unterdrueckt.
;; Wenn der gerade "ubersetzte Ausdruck kein Resultat generiert hat, dann wird
;; die Zuweisung auch unterdr"uckt.
;;------------------------------------------------------------------------------
(defun to-bool-result ()
  (unless (or (string= C-bool_result *C-bool*) (eq 'none *C-bool*))
    (C-Assign C-bool_result *C-bool*)
    (setq *C-bool* C-bool_result)))

;;------------------------------------------------------------------------------
(provide "cgif")
