;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Auf die Seiteneffektanalyse beruhende Optimierungen.
;;;
;;; $Revision: 1.17 $
;;; $Log: seomain.lisp,v $
;;; Revision 1.17  1994/01/27  18:42:06  kl
;;; Aufruf von (type-eq bottom-t ..) durch (is-bottom-t ..) ersetzt.
;;;
;;; Revision 1.16  1993/11/15  12:18:39  jh
;;; seo-1form fuer progn verbessert.
;;;
;;; Revision 1.15  1993/10/18  12:51:08  jh
;;; Es wird jetzt die Funktion is-side-effect-free verwendet.
;;;
;;; Revision 1.14  1993/10/15  10:48:16  hk
;;; seo-1form (progn-form) stellt nun sicher, daß keine progn-form mit
;;; leerer form-list entsteht.
;;;
;;; Revision 1.13  1993/09/21  15:03:56  jh
;;; dec-used-slot eingebaut.
;;;
;;; Revision 1.12  1993/09/09  08:30:17  jh
;;; Da sich der Fehler von dynamischen auch auf statische Variablen
;;; uebertraegt, werden var-refs jetzt ganz uebergangen.
;;;
;;; Revision 1.11  1993/09/06  14:17:31  jh
;;; Umgehung eines Fehlers der Typinferenz bei Referenzen auf dynamische
;;; Variablen eingebaut.
;;;
;;; Revision 1.10  1993/08/31  16:45:13  jh
;;; Fehler in der Statistik behoben.
;;;
;;; Revision 1.9  1993/08/30  14:07:07  jh
;;; Statistik weiter verfeinert.
;;;
;;; Revision 1.8  1993/08/27  10:06:42  jh
;;; Benutzung von is-side-effect-free wieder entfernt, da es noch Probleme
;;; mit den Seiteneffekten auf den Kontrollfluss gibt.
;;;
;;; Revision 1.7  1993/08/26  13:03:17  jh
;;; seo-1form fuer progn-form vereinfacht.
;;;
;;; Revision 1.6  1993/08/26  11:28:46  jh
;;; Es werden nur noch seiteneffektfreie Formen entfernt, die nicht den Typ
;;; bottom-t haben, da dieser eine Kontrollflussaenderung anzeigt.
;;;
;;; Revision 1.5  1993/08/25  14:46:42  jh
;;; is-side-effect-free eingebaut.
;;;
;;; Revision 1.4  1993/07/15  15:55:36  jh
;;; Schreibfehler beseitigt.
;;;
;;; Revision 1.3  1993/07/09  11:30:49  jh
;;; Fehler behoben.
;;;
;;; Revision 1.2  1993/07/08  11:16:39  jh
;;; Spezialbehandlung fuer einen Fall von progn-form, der aus dolist entsteht,
;;; eingebaut.
;;;
;;; Revision 1.1  1993/07/08  10:42:06  jh
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")
(require "titypes")

;;------------------------------------------------------------------------------
;; Nur wenn die seiteneffektfreie Form kein Literal ist, wird die Beseitigung
;; gezaehlt.
;;------------------------------------------------------------------------------

(defun inc-seo-stats (form id)
  (unless (literal-p form)
    (inc-stats id)))

(defun inc-eval-stats (form)
  (if (and (app-p form) (fun-p (?form form)))
      (case (?symbol (?form form))
        (L::eq (inc-stats 'eq-elis))
        (L::not (inc-stats 'not-elis))
        (otherwise (inc-seo-stats form 'fun-evals)))
      (inc-seo-stats form 'fun-evals)))

;;------------------------------------------------------------------------------
;; eliminate-it-p ueberprueft, ob eine Form eliminiert werden darf. Das ist der
;; Fall, wenn die Form keine Seiteneffekte hat und den Kontrollfluss nicht
;; aendert (kein Sprung oder Programmabbruch).
;;------------------------------------------------------------------------------

(defun eliminate-it-p (form)
  (and (is-side-effect-free form)
       (not (is-bottom-t (?type form)))))

;;------------------------------------------------------------------------------
;; An var-refs, named-const etc. kann nichts optimiert werden.
;;------------------------------------------------------------------------------

(defmethod seo-1form ((a-form form))
  a-form)

;;------------------------------------------------------------------------------
;; Seiteneffektfreie Ausdruecke werden zu NIL vereinfacht, wenn das Resultat
;; nicht benoetigt wird. Seiteneffektfreie Ausdruecke, deren Resultat benoetigt
;; wird, werden zu NIL oder T vereinfacht, wenn sie mit NULL bzw. T-SYMBOL
;; getypt sind.
;;------------------------------------------------------------------------------

(defmethod seo-1form :around ((a-form form))
           (if *result-used*
               (if (eliminate-it-p a-form)
                   (let ((type (?type a-form)))
                     (cond
                       ((type-eq null-t type)
                        (inc-eval-stats a-form)
                        (dec-used-slot a-form)
                        empty-list)
                       ((type-eq t-symbol-t type)
                        (inc-eval-stats a-form)
                        (dec-used-slot a-form)
                        (get-symbol-bind 'L::T))
                       (T (call-next-method))))
                   (call-next-method))
               (if (eliminate-it-p a-form)
                   (progn
                     (inc-seo-stats a-form 'seo-optis)
                     (dec-used-slot a-form)
                     empty-list)
                   (call-next-method))))
                        
;;------------------------------------------------------------------------------
(defmethod seo-1form ((a-progn-form progn-form))
  (let ((form-queue (empty-queue))
        (forms (?form-list a-progn-form)))
    (dolist (a-form (butlast forms))
      (unless (eliminate-it-p a-form)
        (add-q a-form form-queue)))
    (let ((a-form (first (last forms))))
      (unless (and (not *result-used*) (eliminate-it-p a-form))
        (add-q a-form form-queue)))
    
    (if (empty-queue-p form-queue)
        empty-list
        (let* ((new-forms (queue2list form-queue))
               (form-n-1 (first (last (butlast new-forms))))
               (form-n (last-q form-queue)))
          (when (and form-n-1           ; Dieser Fall entsteht aus 'dolist'.
                     (null-form-p form-n)
                     (type-eq null-t (?type form-n-1)))
            (setq new-forms (butlast new-forms)))
          (setf (?form-list a-progn-form) new-forms)
          a-progn-form))))

;;------------------------------------------------------------------------------

(provide "seomain")
