;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung,
;;;            - mv-lambda
;;;            - VALUES (inline)
;;;
;;; $Revision: 1.20 $
;;; $Log: cgvalues.lisp,v $
;;; Revision 1.20  1994/05/25  14:07:05  sma
;;; Aufruf der Restlistenoptimierung aus cg-params herausgezogen.
;;; Restlistenoptimierung fuer mv-lambda korrigiert, d.h. jetzt gehts
;;; erst.
;;;
;;; Revision 1.19  1994/02/08  15:40:47  sma
;;; Zusätzlichen Block um cg-params-Aufruf herum eingefügt.
;;;
;;; Revision 1.18  1994/01/07  11:56:33  hk
;;; opt-args wird nun auch in cg-values verwendet.
;;;
;;; Revision 1.17  1993/09/10  10:06:29  hk
;;; Fehler beim Aufruf von cg-params behoben.
;;;
;;; Revision 1.16  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.15  1993/04/07  16:22:56  hk
;;; Schreibfehler behoben.
;;;
;;; Revision 1.14  1993/04/07  16:21:42  hk
;;; Fehlermeldung verschoenert.
;;;
;;; Revision 1.13  1993/04/05  10:06:59  hk
;;; Ueberzaehlige Argumente von values werden im Fehlerfall ignoriert.
;;;
;;; Revision 1.12  1993/04/05  10:02:38  hk
;;; Fehlerbehandlung verbessert.
;;;
;;; Revision 1.11  1993/04/05  09:48:33  hk
;;; Code verschoenert.
;;;
;;; Revision 1.10  1993/02/16  15:48:55  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.9  1992/09/26  16:00:43  hk
;;; In cg-values nil durch empty-list ersetzt.
;;;
;;; Revision 1.8  1992/09/23  14:34:58  hk
;;; In einem Fall ILLEGAL_ARGS durch TOO_FEW_ARGS ersetzt.
;;;
;;; Revision 1.7  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.6  1992/08/11  12:45:40  hk
;;; C-Ln --> C-Decl, fuer Deklarationen.
;;;
;;; Revision 1.5  1992/08/10  16:53:19  hk
;;; Keine globalen Variablen *mv-spec* und *mv-produced* mehr, sondern
;;; neue Slots mv-used in app und mv-spec und mv-calls in mv-lambda.
;;;
;;; Revision 1.4  1992/07/29  11:16:40  hk
;;; Kleiner Fehler.
;;;
;;; Revision 1.3  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.2  1992/05/22  09:11:25  hk
;;; cg-mv-args laeuft mit beliebig vielen Argumenten, auch wenn
;;; die Anzahl der generierten MV nicht zur Uebersetzungszeit bekannt ist.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC") 

;;------------------------------------------------------------------------------
(defconstant MV-LIMIT 20)

;;------------------------------------------------------------------------------
;; mv-lambda
;;------------------------------------------------------------------------------
(defmethod cg-form ((form mv-lambda))
  (let* ((old-stack *stack-top*)
         (params (?params form))
         (par-spec (calc-par-spec params))
         (mv-spec (?mv-spec form)))
     
    ;; MV generieren
    ;;--------------
    (let ((*result-spec* (stacktop-result-location)))
      (cg-form (?arg form)))

    (cond
        
      ;; bekannte Anzahl von MV
      ;;-----------------------
      ((numberp mv-spec)
       (unless (= 0 mv-spec)
         (incf *stack-top*)
         (dotimes (i (1- mv-spec))
           (C-Copy (CC-mv_buf i) (CC-StackTop))
           (incf *stack-top*)))
       (cond
         ((>= par-spec 0)
          (unless (eql mv-spec par-spec)
            (cg-error "MV-LAMBDA: the number of arguments ~a ~
                       should be ~a" mv-spec par-spec)))
         (t (unless (>= mv-spec (1- (- par-spec)))
              (cg-error "MV-LAMBDA: the number of arguments ~a ~
                         should be >= ~a" mv-spec (1- (- par-spec))))
            (C-Blockstart)
            (C-VarDecl "int" C-nargs)
            (C-Assign C-nargs mv-spec)))
         
       (unless (= 1 mv-spec)
         (C-resetMV)))

      ;; unbekannte Anzahl von MV 
      ;;--------------------------
      (t (incf *stack-top*)
         (C-MVToStack C-mv_count *stack-top*)
         (cond
           ((>= par-spec 0)
            (C-if (CC-op!= C-mv_count par-spec))
            (C-Blockstart)
            (C-Abort "ILLEGAL_ARGS")
            (C-Blockend))
           (t (C-Blockstart)
              (C-VarDecl "int" C-nargs)
              (C-Assign C-nargs C-mv_count)
              (let ((min (1- (- par-spec))))
                (when (> min 0)
                  (C-if (CC-op< C-mv_count min))
                  (C-Blockstart)
                  (C-Abort "TOO_FEW_ARGS")
                  (C-Blockend)))))
         (C-resetMV)))
    (setq *stack-top* old-stack)

    (C-blockstart)
    (let ((*rest-optimization* *rest-optimization*))
      (cg-params params (>= (calc-par-spec params) 0)
                 (rest-optimization-p (?params form) (?body form) nil))
      (cg-form (?body form)))
    (C-blockend)
    (setq *stack-top* old-stack)

    (unless (>= par-spec 0)
      (C-Blockend))))
    

;;------------------------------------------------------------------------------
;; Erzeugt Inline-code fuer die System-Fkt. VALUES.
;;------------------------------------------------------------------------------
(defun cg-values (arg-list app)
  (let ((mv-count (length arg-list))
        (*stack-top* *stack-top*))
    
    (if (or (not (?mv-used app)) (eql mv-count 1))
    
        ;; keine Multiple Values erzeugen
        ;;-------------------------------
        (case mv-count
          (0 (cg-form empty-list))
          (1 (cg-form (first arg-list)))
          (T (opt-args arg-list)        ; veraendert *stack-top* !
             (cg-args arg-list -1)
             (stacktop-to-result-loc)))
    
        ;; Multiple Values erzeugen
        ;;-------------------------
        (case mv-count
          (0 (cg-form empty-list)
             (C-SetMV 0))
      
          ;; Alle MV zunaechst auf dem Stack erzeugen, weil beim Evaluieren der
          ;; Argumente evtl. mv_buf uebertschrieben wird.
          ;;-Beachte:
          ;; Resultat nicht auf fun-result Position erzeugen, weil dadurch
          ;; evtl.  eine lokale Variable ueberschrieben wird, die fuer MV noch
          ;; benoetigt wird.
          ;;--------------------------------------------------------------------
          (t (when (> mv-count MV-LIMIT)
               (cg-error "VALUES is called with ~a arguments, ~
                        but only ~a are allowed"
                         mv-count MV-LIMIT)
               (setq mv-count MV-LIMIT))
             (opt-args arg-list)        ; veraendert *stack-top* !
             (cg-args arg-list -1)
             (stacktop-to-result-loc)
             (dotimes (i (1- mv-count))
               (C-copy (CC-Stack (+ *stack-top* i 1)) (CC-mv_buf i)))
             (C-SetMV mv-count))))))

;;------------------------------------------------------------------------------
(provide "cgvalues")

