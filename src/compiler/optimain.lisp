;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Funktionen zur Steuerung der Optimierungsdurchlaeufe.
;;;            Optimierungen eines einzelnen Ausdrucks oder rekursiv eines
;;;            Ausdrucks und aller seiner Komponenten.
;;;
;;; $Revision: 1.23 $
;;; $Log: optimain.lisp,v $
;;; Revision 1.23  1994/05/02  11:34:37  pm
;;; Fehler behoben
;;; *optimize -> *optimize*
;;;
;;; Revision 1.22  1994/05/01  22:46:18  hk
;;; Kein Gewicht berechnen, wenn *OPTIMIZE* = nil
;;;
;;; Revision 1.21  1994/04/06  11:45:31  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.20  1994/02/18  13:57:49  hk
;;; Typ-Optimierung der If-Ausdr"ucke erfolgt nun von au"sen nach innen,
;;; die zu eliminierenden Zweigen sollen nicht optimiert werden, um die
;;; Statistik nicht zu verf"alschen und keine "uberfl"ussigen Warnungen zu
;;; erhalten.
;;;
;;; Revision 1.19  1994/02/02  09:33:36  hk
;;; Statistik an die Änderungen in simplifier angepaßt.
;;; Weigt wird nur bestimmt, wenn Inlining angeschaltet ist.
;;;
;;; Revision 1.18  1994/01/26  14:22:55  sma
;;; Änderung (von jh) damit auf ohne Optimierungen die Funcalls von
;;; inline-compilierten setf-funs aufgelöst werden.
;;;
;;; Revision 1.17  1994/01/15  22:01:46  kl
;;; Liste der Statistikmeldungen erweitert.
;;;
;;; Revision 1.16  1994/01/14  14:32:43  sma
;;; Neuer Text für Optimierung von mehr- in 2-stellige Funktionen ergänzt.
;;;
;;; Revision 1.15  1993/11/26  12:24:38  jh
;;; Statistik erweitert (equal-to-eql).
;;;
;;; Revision 1.14  1993/11/15  12:19:13  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.13  1993/10/20  15:43:30  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.12  1993/10/15  10:40:42  hk
;;; optimize-parts (progn-form) geht nun davon aus, daß form-list nicht
;;; leer ist.
;;;
;;; Revision 1.11  1993/10/08  18:16:42  kl
;;; Tagged-forms behalten nun ihren Typ (bottom).
;;;
;;; Revision 1.10  1993/09/20  14:15:54  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.9  1993/09/01  15:24:51  jh
;;; Fehler in optimize-parts fuer let/cc-form behoben.
;;;
;;; Revision 1.8  1993/08/30  14:06:31  jh
;;; Nach dem Optimieren werden die Typen neu gesetzt. Dadurch werden teilweise
;;; weitere Optimierungen bereits im selben Durchlauf moeglich.
;;;
;;; Revision 1.7  1993/08/26  14:48:58  jh
;;; Statistik erweitert.
;;;
;;; Revision 1.6  1993/08/26  13:01:27  jh
;;; Fehler in optimize-parts fuer progn-form behoben.
;;;
;;; Revision 1.5  1993/08/25  14:48:14  jh
;;; Statistik fuer die Optimierungen, die auf der Seiteneffektanalyse beruhen,
;;; eingebaut.
;;;
;;; Revision 1.4  1993/07/23  09:39:07  hk
;;; *optimize-verbosity* auf 1 (wg. tomain)
;;;
;;; Revision 1.3  1993/07/08  10:48:30  jh
;;; Einfache Optimierungen, die auf der Seiteneffektanalyse beruhen (seomain)
;;; eingebaut.
;;;
;;; Revision 1.2  1993/06/30  16:41:15  hk
;;; Schreibfehler in (optimize-parts mv-lambda) behoben.
;;;
;;; Revision 1.1  1993/06/30  15:22:53  jh
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; *result-used* gibt an, ob das Ergebnis des Zwsichensprachausdrucks benoetigt
;; wird.
;;------------------------------------------------------------------------------

(defvar *result-used*)

;;------------------------------------------------------------------------------

(require "optimize")
(require "subst")
(require "tomain")
(require "simplifier")
(require "seomain")
(require "weight")

;;------------------------------------------------------------------------------
;; Globale Variablen zum Abschalten einzelner Optimierungen.
;;------------------------------------------------------------------------------

(defvar *no-subst* nil)
(defvar *no-to* nil)
(defvar *no-seo* nil)
(defvar *no-simp* nil)

;;------------------------------------------------------------------------------
;; Globale Variablen, Konstanten und Funktionen fuer die Statistik.
;;------------------------------------------------------------------------------

(defvar *optimize-verbosity* 1)
(defvar *optimize-statistics* nil)

(defconstant statistics-output
  '((then-optis     . "~D times then case eliminated.")
    (else-optis     . "~D times else case eliminated.")
    (seo-optis      . "~D side-effect-free forms eliminated.")
    (bt-setq        . "~D bottom-typed setq-forms eliminated.")
    (bt-progn       . "~D times unreachable code in progn-forms eliminated.")
    (bt-app         . "~D times unreachable code in app-forms eliminated.")
    (bt-let*        . "~D times unreachable code in let*-forms eliminated.")
    (bt-mv-lambda   . "~D times unreachable code in mv-lambda-forms ~
                       eliminated.")
    (setq-elis      . "~D times useless setq-forms eliminated.")
    (progn-elis     . "~D useless progn-forms eliminated.")
    (if-not-elis    . "~D times not-function in if-form-predicates eliminated.")
    (let*-elis      . "~D useless let*-forms eliminated.")
    (let-var-elis   . "~D unused local variables eliminated.")
    (labels-elis    . "~D useless labels-forms eliminated.")
    (let/cc-elis    . "~D useless let/cc-forms eliminated.")
    (tag-elis       . "~D useless tags eliminated.")
    (tagbody-elis   . "~D useless tagbody-forms eliminated.")
    (mv-lambda-elis . "~D useless mv-lambda-forms eliminated.")
    (if-if-split    . "~D times if-form on predicate position splitted.")
    (to-n-arg-fun   . "~D times function simplified in call with n arguments.")
    (to-noresult-fun .
     "~D times function simplified in call with unused result.")
    (fun-to-%fun    .
     "~D times function simplified in call with nth argument of type cons.")
    (equal-to-eql   .
     "~D times function simplified in call with some argument not of type 
cons, pathname, string or bitvector.")
    (eql-to-eq      .
     "~D times function simplified in call with some argument not of type
number or character.")
    (only-test-optis .
     "~D times function simplified in call with only one specified 
keyword arg.")
    (test-fun-optis .
     "~D times function simplified in call with test function applied
to nth argument.")
    (funcall-elis   . "~D funcall applications eliminated.")
    (apply-elis     . "~D apply applications eliminated.")
    (eq-elis        . "~D times eq evaluated.")
    (eq-to-not      . "~D times (eq nil x) substituted by (not x).")
    (not-elis       . "~D times not evaluated.")
    (fun-evals      . "~D forms evaluated.")
    (set-elis       . "~D times (set sym x) substituted by (setq dyn x).")
    (sym-val-elis   . "~D times (symbol-value x) substituted by a reference to ~
                       the dynamic variable x.")
    (=-to-zerop     . "~D times (= 0 x) substituted by (zerop x).")
    (=-to-eql       . "~D times = substituted by eql.")
    (<-to-plusp     . "~D times (< 0 x) substituted by (plusp x).")
    (>-to-minusp    . "~D times (> 0 x) substituted by (minusp x).")
    (+-to-1+        . "~D times (+ 1 x) substituted by (1+ x).")
    (+-evals        . "~D times + evaluated.")
    (--to-1-        . "~D times (- x 1) substituted by (1- x).")
    ))

(defun write-optimize-statistics ()
  (when (> *optimize-verbosity* 0)
    (setq *optimize-statistics* (sort *optimize-statistics* #'> :key #'cdr))
    (dolist (counter *optimize-statistics*)
      (clicc-message (cdr (assoc (car counter) statistics-output :test #'eq))
                     (cdr counter)))
    (unless (or *no-inlining* (not *optimize*))
      (clicc-message "The weight of the module is ~D" (weight-module)))))

(defun inc-stats (stats-id)
  (let ((assoc (assoc stats-id *optimize-statistics* :test #'eq)))
    (if assoc
        (incf (cdr assoc))
        (push (cons stats-id 1) *optimize-statistics*))))

;;------------------------------------------------------------------------------
;; optimize-field optimiert einen Ausdruck und schreibt das Ergebnis an dieselbe
;; Stelle zurueck.
;;------------------------------------------------------------------------------
(defmacro optimize-field (field)
  `(setf ,field (optimize-form ,field)))

;;------------------------------------------------------------------------------
;; optimize-module optimiert die im Modul definierten Funktionen sowie die
;; toplevel-forms.
;;------------------------------------------------------------------------------
(defun optimize-module (a-module)
  (optimize-fun-def-list (?all-global-funs a-module)))

(defun do-optimization ()
  (let (*optimize-statistics*)
    (clicc-message "Optimization")
    (optimize-module *module*)
    (write-optimize-statistics)))

;;------------------------------------------------------------------------------

(defmethod optimize-fun-def ((a-simple-fun simple-fun))
  (let ((*current-function* a-simple-fun)
        (*current-fun* (?symbol a-simple-fun))
        (*result-used* t))
    (optimize-params (?params a-simple-fun))
    (optimize-field (?body a-simple-fun))))

(defun optimize-fun-def-list (fun-def-list)
  (mapc #'optimize-fun-def fun-def-list))

(defun optimize-form-list (form-list)
  (unless (endp form-list)
    (optimize-field (first form-list))
    (optimize-form-list (rest form-list))))

(defun optimize-params (params)
  (mapc #'optimize-opt/key (?opt-list params))
  (mapc #'optimize-opt/key (?key-list params)))

(defun optimize-opt/key (opt/key)
  (optimize-field (?init opt/key)))

;;------------------------------------------------------------------------------
;; optimize-form optimiert einen Zwischensprachausdruck sowie rekursiv seine
;; Teilausdruecke.
;;------------------------------------------------------------------------------

(defmethod optimize-form ((a-form form))

  ;; Die If-Ausdr"ucke von Au"sen nach innen optimieren, da evtl. schon viele
  ;; Zweige eliminiert werden.
  ;;--------------------------
  (unless *no-to*
    (setq a-form (to-1form a-form)))
  
  ;; Wegen der Ersetzungen muessen die Bestandteile eines Zwischensprachknotens
  ;; vor dem Zwischensprachknoten selbst analysiert werden. (Da noch Ersetzungen
  ;; innerhalb der form einer setq-form mit den alten Bindungen moeglich sind,
  ;; waehrend dies nachher fuer die betroffene Variable nicht mehr erlaubt ist.)
  (optimize-1form (optimize-parts a-form)))

(defmethod optimize-form ((a-cont cont))
  a-cont)

;;------------------------------------------------------------------------------
;; optimize-1form optimiert einen Zwischensprachausdruck, ohne seine Teil-
;; ausdruecke zu optimieren. optimize-parts optimiert die Teilausdruecke eines
;; Zwischensprachausdrucks. (Aendert sich durch eine der Teiloptimierungen die
;; Identitaet des Zwischensprachobjekts, so ist diese Teiloptimierung dafuer
;; verantwortlich, dass ein vollstaendig optimierter Zwischensprachausdruck
;; zurueckgeliefert wird.
;;------------------------------------------------------------------------------

(defmethod optimize-1form ((a-form form))
  (let ((new-form a-form))
    (unless *no-subst*
      (setq new-form (subst-1form a-form))
      ;; Hat sich die Identitaet geaendert, so wird der erhaltene
      ;; Zwischensprachausdruck zurueckgeliefert.
      (unless (eq new-form a-form)
        (return-from optimize-1form new-form)))
    (unless *no-seo*
      (setq new-form (seo-1form a-form))
      (unless (eq new-form a-form)
        (return-from optimize-1form new-form)))
    (if *no-simp*
        (setq new-form
              (if (and (app-p a-form)
                       (eq (?form a-form) (get-global-fun 'L::funcall)))
                  (let ((arg-list (?arg-list a-form)))
                    (setf (?form a-form) (first arg-list))
                    (setf (?arg-list a-form) (rest arg-list))
                    a-form)
                  a-form))
        (setq new-form (simplify-1form a-form)))
    new-form))

(defmethod optimize-parts ((a-form form))
  a-form)

(defmethod optimize-parts ((a-setq-form setq-form))
  (let ((*result-used* t))
    (optimize-field (?form a-setq-form)))
  (setf (?type a-setq-form) (?type (?form a-setq-form)))
  a-setq-form)

(defmethod optimize-parts ((a-progn-form progn-form))
  (let* ((forms (?form-list a-progn-form))
         (forms-1 (let ((*result-used* nil)) 
                    (mapcar #'optimize-form (butlast forms))))
         (last (last forms)))
    (optimize-field (first last))
    (setf (?type a-progn-form) (?type (first last))
          (?form-list a-progn-form) (nconc forms-1 last)))
  a-progn-form)

(defmethod optimize-parts ((an-if-form if-form))
  (let ((*result-used* t))
    (optimize-field (?pred an-if-form)))
  (let ((subst-pred *SUBSTITUTION*)
        subst-then
        subst-else)
    (optimize-field (?then an-if-form))
    (setq subst-then *SUBSTITUTION*
          *SUBSTITUTION* subst-pred)
    (optimize-field (?else an-if-form))
    (setq subst-else *SUBSTITUTION*)
    (setq *SUBSTITUTION*
          (restrict-map-to subst-pred
                           (intersection subst-then subst-else :key #'car))))
  (setf (?type an-if-form) (type-join (?type (?then an-if-form))
                                      (?type (?else an-if-form))))
  an-if-form)

(defmethod optimize-parts ((an-app app))
  (let ((*result-used* t))
    (optimize-field (?form an-app))
    (optimize-form-list (?arg-list an-app)))
  an-app)

(defmethod optimize-parts ((a-switch-form switch-form))
  (let ((*result-used* t))
    (optimize-field (?form a-switch-form)))
  (optimize-form-list (?case-list a-switch-form))
  (optimize-field (?otherwise a-switch-form))
  a-switch-form)

(defmethod optimize-parts ((a-let*-form let*-form))
  
  ;; Hier wird ermittelt, welche der in der let*-form gebundenen Variablen
  ;; durch ihren Wert ersetzt werden sollen. Dies muss leider zwischen den
  ;; Optimierungen der init-forms stattfinden und kann daher nicht in
  ;; subst.lisp stehen.
  ;;----------------------------------------------------------------------
  (let (actual-subst)
    (let ((*SUBSTITUTION* *SUBSTITUTION*))
      (let ((*result-used* t))
        (setf (?init-list a-let*-form)
              (mapcar #'(lambda (var init-form)
                          (setq init-form (optimize-form init-form))
                          (when (and (local-static-p var)
                                     (copy-is-eq-p init-form))
                            (push (cons var init-form) *SUBSTITUTION*))
                          init-form)
                      (?var-list a-let*-form) (?init-list a-let*-form))))
      (optimize-field (?body a-let*-form))
      (setq actual-subst *SUBSTITUTION*))

    ;; Nach dem Optimieren der let*-form ist wieder die alte Substitution
    ;; gueltig, vermindert um die inzwischen geloeschten Bindungen.
    ;;-------------------------------------------------------------------
    (setq *SUBSTITUTION* (restrict-map-to *SUBSTITUTION* actual-subst)))
  (setf (?type a-let*-form) (?type (?body a-let*-form)))
  a-let*-form)

(defmethod optimize-parts ((a-labels-form labels-form))
  ;; In und nach einer labels-form duerfen keine Ersetzungen vorgenommen werden.
  ;; (Bei Beachtung der Seiteneffekte kann hier genauer gearbeitet werden.)
  (clear-substitution)
  (optimize-fun-def-list (?fun-list a-labels-form))
  (optimize-field (?body a-labels-form))
  (setf (?type a-labels-form) (?type (?body a-labels-form)))
  a-labels-form)


(defmethod optimize-parts ((a-let/cc-form let/cc-form))
  (optimize-field (?body a-let/cc-form))
  a-let/cc-form)

(defmethod optimize-parts ((a-tagbody-form tagbody-form))
  (let ((tagged-forms (?tagged-form-list a-tagbody-form)))
    (when (?first-form a-tagbody-form)
      (let ((*result-used* (and (null tagged-forms) *result-used*)))
        (optimize-field (?first-form a-tagbody-form))))
  
    ;; Bis zur ersten Sprungmarke durfte optimiert werden.
    ;;----------------------------------------------------
    (clear-substitution)
    (let ((*result-used* nil))
      (mapc #'(lambda (a-tagged-form)
                (progn
                  (optimize-field (?form a-tagged-form))
                  ;;(setf (?type a-tagged-form) (?type (?form a-tagged-form)))
                  ))
            (butlast tagged-forms)))
    (if tagged-forms
        (let ((tagged-form (first (last tagged-forms))))
          (optimize-field (?form tagged-form))
          (let ((type (?type (?form tagged-form))))
            ;;(setf (?type tagged-form) type)
            (setf (?type a-tagbody-form) type)))
        (setf (?type a-tagbody-form) (?type (?first-form a-tagbody-form)))))
  a-tagbody-form)

(defmethod optimize-parts ((a-tagged-form tagged-form))
  a-tagged-form)

(defmethod optimize-parts ((a-mv-lambda mv-lambda))
  (let ((*result-used* t))
    (optimize-field (?arg a-mv-lambda))
    (optimize-params (?params a-mv-lambda)))
  (optimize-field (?body a-mv-lambda))
  (setf (?type a-mv-lambda) (?type (?body a-mv-lambda)))
  a-mv-lambda)

;;------------------------------------------------------------------------------
(provide "optimain")
