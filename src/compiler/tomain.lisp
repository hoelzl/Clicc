;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Funktionen zur Optimierungen, die auf den Typinformationen
;;;            aufbauen.
;;;
;;; $Revision: 1.29 $
;;; $Log: tomain.lisp,v $
;;; Revision 1.29  1994/04/06  11:46:20  jh
;;; Einfachen Spezialfall des if-Splittings eingebaut.
;;;
;;; Revision 1.28  1994/02/18  14:00:53  hk
;;; Nur noch If-Ausdr"ucke, deren Pr"adikat den Typ NULL oder not NULL
;;; haben werden optimiert. Let*, Progn, setq etc., die Teilausdr"ucke des
;;; Typs bottom haben werden nicht mehr optimiert, da diese als Vorkommen
;;; von Typfehlern angesehen werden, die in look-for-type-errors gemeldet
;;; werden.
;;;
;;; Revision 1.27  1993/10/28  07:29:11  kl
;;; Verwendung von has-no-side-effect gestrichen. Soll in seo... erledigt werden.
;;;
;;; Revision 1.26  1993/09/21  15:04:36  jh
;;; dec-used-slot eingebaut.
;;;
;;; Revision 1.25  1993/09/16  17:39:29  jh
;;; Schreibfehler behoben.
;;;
;;; Revision 1.24  1993/08/27  15:51:40  hk
;;; Kleinen Fehler in (to-1form app) behoben.
;;;
;;; Revision 1.23  1993/08/26  14:48:30  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.22  1993/07/23  09:40:08  hk
;;; Neues Macro eliminating-msg fuer Meldung, dass Ausdrucke eliminiert
;;; werden. Benutzt *optimize-verbosity*.
;;;
;;; Revision 1.21  1993/06/30  13:46:41  jh
;;; An optimain.lisp angepasst.
;;;
;;; Revision 1.20  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.19  1993/06/10  09:54:18  kl
;;; Bindung der Variable *current-fun* korrigiert.
;;;
;;; Revision 1.18  1993/06/08  13:13:37  jh
;;; Variable *no-to* zum Ausschalten der Optimierungen eingebaut.
;;;
;;; Revision 1.17  1993/05/27  12:49:40  jh
;;; Optimierung fuer bottom-getypte Form einer Applikation eingebaut.
;;;
;;; Revision 1.16  1993/05/25  15:12:52  jh
;;; bottom-Optimierung fuer mv-lambda eingebaut.
;;;
;;; Revision 1.15  1993/05/19  14:40:12  jh
;;; Einige bottom-Optimierungen eingebaut.
;;;
;;; Revision 1.14  1993/05/13  13:19:26  jh
;;; Durch Optimierung entstandene forms werden jetzt ebenfalls optimiert.
;;;
;;; Revision 1.13  1993/05/13  10:15:23  jh
;;; In switch-forms wird jetzt auch otherwise optimiert.
;;;
;;; Revision 1.12  1993/05/12  08:47:43  kl
;;; Schreibfehler korrigiert.
;;;
;;; Revision 1.11  1993/05/09  16:26:42  kl
;;; Ausgabe erweitert.
;;;
;;; Revision 1.10  1993/05/09  13:05:40  kl
;;; Fehler in to-1form (if-form) behoben.
;;;
;;; Revision 1.9  1993/05/07  10:06:26  hk
;;; Schreibfehler in (to-parts switch-form) behoben.
;;;
;;; Revision 1.8  1993/05/06  13:31:06  kl
;;; Verwendung von has-no-side-effect eingebaut und Ausgabe erweitert.
;;;
;;; Revision 1.7  1993/05/05  11:04:22  jh
;;; Spezialbehandlung fuer bottom-t bei if-Optimierung eingebaut.
;;;
;;; Revision 1.6  1993/04/30  14:32:02  hk
;;; do-to deaktiviert, da Fehler in Typinferenz oder Typoptimierungen.
;;;
;;; Revision 1.5  1993/04/30  13:26:32  hk
;;; Hier wurde bisher JEDES (mv-lambda args body) zu args optimiert.
;;;
;;; Revision 1.4  1993/04/21  11:35:30  jh
;;; mv-lambda eingebaut.
;;;
;;; Revision 1.3  1993/02/16  16:09:13  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.2  1993/01/07  12:59:31  jh
;;; Statistik fuer die if-Optimierungen eingebaut.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; to-1form optimiert if-Ausdr"ucke, bei denen der Wahrheitswert des
;; Pr"adikats bereits bekannt ist.
;; F"ur Ausdr"ucke, die den Typ bottom haben, wird eine Warnung ausgegeben.
;; Diese Optimierung wird erst f"ur "au"sere und danach f"ur innere Ausdr"ucke
;; vorgeommen, so da"s nicht ereichbare Zweige von If-Ausdr"ucken eliminiert
;; werden, bevor eine Warnung "uber den Bottom Typ ausgegeben werden kann.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
(defmethod to-1form ((form form)) form)

;;------------------------------------------------------------------------------
(defmethod to-1form ((an-if-form if-form))
  (let* ((pred (?pred an-if-form))
         (pred-type (?type pred))
         (then (?then an-if-form))
         (else (?else an-if-form))
         eliminated-case
         (result 
          (cond
            ;; Typfehler gefunden, Warnung erfolgt sp"ater
            ;;--------------------------------------------
            ((is-bottom-t pred-type) an-if-form)
            
            ((zs-subtypep pred-type null-t)
             (inc-stats 'then-optis)
             (dec-used-slot then)
             (setq eliminated-case "then")
             (make-instance 'progn-form 
                            :form-list (list pred else)
                            :type (?type else)))
            ((not (zs-subtypep null-t pred-type))
             (inc-stats 'else-optis)
             (dec-used-slot else)
             (setq eliminated-case "else")
             (make-instance 'progn-form
                            :form-list (list pred then)
                            :type (?type then)))
            ((if-form-p pred) (split an-if-form))
            (T an-if-form))))
    (when (and eliminated-case (> *optimize-verbosity* 1))
      (clicc-message "(in ~A) Eliminating unreachable ~A-case."
                     *current-fun*
                     eliminated-case))
    result))

(defun split (an-if-form)
  (let* ((inner-if (?pred an-if-form))
         (then (?then an-if-form))
         (else (?else an-if-form))
         (inner-then (?then inner-if))
         (inner-else (?else inner-if))
         (inner-then-type (?type inner-then))
         (inner-else-type (?type inner-else)))
    (cond
      ((and (not (zs-subtypep null-t inner-then-type))
            (zs-subtypep inner-else-type null-t))
       (inc-stats 'if-if-split)
       (setf (?then inner-if)
             (optimize-1form (make-progn-form :form-list (list inner-then then)
                                              :type (?type then))))
       (setf (?else inner-if)
             (optimize-1form (make-progn-form :form-list (list inner-else else)
                                              :type (?type else))))
       (optimize-1form inner-if))
      ((and (zs-subtypep inner-then-type null-t)
            (not (zs-subtypep null-t inner-else-type)))
       (inc-stats 'if-if-split)
       (setf (?then inner-if)
             (optimize-1form (make-progn-form :form-list (list inner-then else)
                                              :type (?type else))))
       (setf (?else inner-if)
             (optimize-1form (make-progn-form :form-list (list inner-else then)
                                         :type (?type then))))
       (optimize-1form inner-if))
      (T an-if-form))))

;;------------------------------------------------------------------------------
(provide "tomain")
