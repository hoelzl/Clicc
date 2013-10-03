;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Funktionen zur Inline-Compilation von Funktionsapplikationen.
;;;
;;; $Revision: 1.46 $
;;; $Log: inline.lisp,v $
;;; Revision 1.46  1994/06/11  00:10:17  hk
;;; In kleinen Funktinen nur dann nicht inlinen, wenn sie exportiert
;;; werden. (diese Regel ist noch nicht optimal)
;;;
;;; Revision 1.45  1994/06/10  23:36:07  hk
;;; F"ur neue Namen globaler definierender Vorkommen (Funktionen, named
;;; const) wird jetzt nicht das Resultat von intern ber"ucksichtigt,
;;; sondern es wird geschaut, ob bereits ein Programmobjekt gleichen
;;; Namens im Modul definiert ist.
;;;
;;; Revision 1.44  1994/06/10  14:23:29  hk
;;; F"ur globale definierende Vorkommen (Funktionen, named const) d"urfen
;;; die Namen keine uninterned Symbols sein.
;;;
;;; Revision 1.43  1994/06/10  12:31:15  jh
;;; *max-inline-weight* wegen der Änderung in weight.lisp auf 30 gesetzt.
;;;
;;; Revision 1.42  1994/06/10  10:45:00  hk
;;; Kein gentemp mehr verwendet, um bei verschiedenen Compiler-L"aufen
;;; reproduzierbare Namen zu erhalten.
;;;
;;; Revision 1.41  1994/06/09  15:18:29  hk
;;; *max-inline-weight* auf 38 erh"oht, damit struct-ref inline compiliert
;;; wird. named-constants m"ussen doch einen mit gentemp generierten Namen haben.
;;;
;;; Revision 1.40  1994/06/09  07:40:20  hk
;;; Noch ein Schreibfehler. ?symbol nicht auf dynamic anwenden.
;;;
;;; Revision 1.39  1994/06/08  16:32:49  hk
;;; Schreibfehler
;;;
;;; Revision 1.38  1994/06/08  16:27:36  hk
;;; Fehler behoben: gentemp nicht auf Symbol anwenden. Viele gentemps
;;; gestrichen, und den Namen der kopierten Variablen genommen.
;;;
;;; Revision 1.37  1994/06/08  15:06:24  jh
;;; Imported-funs sind immer copyable.
;;;
;;; Revision 1.36  1994/06/08  11:59:27  hk
;;; In dynamic-var-problems:test-opt&key explizites Block eingef"ugt (f"ur
;;; CLISP)
;;;
;;; Revision 1.35  1994/06/07  17:28:06  jh
;;; Funktionen zur Parametervereinfachung eingebaut und Inlining
;;; verbessert.
;;;
;;; Revision 1.34  1994/04/29  11:02:37  hk
;;; Slots mv-position und fn-on-mv-pos von Continuations werden von zs-cp
;;; nicht kopiert, da diese Slots bei importierten Funktionen nicht
;;; gesetzt sind. Level nur bei lokalen Funktionen abfragen.
;;;
;;; Revision 1.33  1994/04/05  15:11:25  jh
;;; Inlining importierter Funktionen eingebaut.
;;;
;;; Revision 1.32  1994/02/08  11:25:28  sma
;;; Überflüssiges `"' gelöscht.
;;;
;;; Revision 1.31  1994/02/08  11:09:22  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.30  1994/02/01  15:20:21  hk
;;; make-instance 'xxx --> make-xxx
;;;
;;; Revision 1.29  1994/02/01  11:32:36  hk
;;; zs-cp (fun): nur bei defined-fun den used Slot erhöhen.
;;;
;;; Revision 1.28  1994/01/31  13:53:23  hk
;;; inline-app-p ist nur dann wahr, wenn eine definierte Funktion
;;; angewendet wird. Bei importierten Funktionen wird der Body in einem
;;; :raw Slot stecken.
;;;
;;; Revision 1.27  1994/01/21  13:23:12  sma
;;; max-inline-weight von 25 auf 35 erhöht.
;;;
;;; Revision 1.26  1994/01/10  08:52:41  hk
;;; write-inline-statistics: (apply #'+ (mapcar #'cdr *inline-statistics*))
;;; führt zu Problemen, wenn *inline-statistics* mehr als
;;; lambda-paramerts-limit Elemente enthält.  Stattdessen reduce verwendet.
;;;
;;; Revision 1.25  1993/09/29  08:09:57  jh
;;; Fehler in keywords-ok-p behoben.
;;;
;;; Revision 1.24  1993/09/20  14:35:41  jh
;;; An die neue Version der weight-Funktion angepasst. Bei Funktionen mit nur
;;; einer Anwendung wird der Rumpf nicht kopiert, sondern das Original weiter-
;;; verwendet.
;;;
;;; Revision 1.23  1993/09/10  15:36:15  hk
;;; apply '+ --> apply #'+, weil das erste kein CL_0 ist.
;;;
;;; Revision 1.22  1993/08/05  11:31:38  jh
;;; foreign-funs eingebaut.
;;;
;;; Revision 1.21  1993/08/04  17:10:00  hk
;;; Unbekannter Seiteneffekt in read- und write-list wird jetzt durch -1
;;; und nicht mehr durch :UNKNOWN ausgedrückt.
;;;
;;; Revision 1.20  1993/07/26  14:35:39  wg
;;; Lokale Funktionen in Methoden fuer CMU global definiert.
;;;
;;; Revision 1.19  1993/07/23  13:18:49  hk
;;; Neue Variabale *inline-verbosity*.
;;;
;;; Revision 1.18  1993/07/22  12:46:45  jh
;;; Fehler in mc-form behoben.
;;;
;;; Revision 1.17  1993/07/21  12:45:06  jh
;;; structured-literals in Funktionen, die inline kompiliert werden sollen,
;;; werden in defined-named-const verpackt.
;;;
;;; Revision 1.16  1993/07/20  13:49:22  jh
;;; Die Funktion error wird nicht mehr inline kompiliert.
;;;
;;; Revision 1.15  1993/07/19  09:53:29  jh
;;; structured-literals werden wieder inline kompiliert, da sie jetzt in
;;; named-const verpackt werden.
;;;
;;; Revision 1.14  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.13  1993/06/07  08:31:15  jh
;;; Kommentar in bind-arguments eingeguegt.
;;;
;;; Revision 1.12  1993/06/05  22:27:34  hk
;;; structured-literals werden zunaechst nicht inline kompiliert.
;;;
;;; Revision 1.11  1993/06/05  18:57:28  hk
;;; In bind-arguments bei der Bearbeitung von Keyword Parametern
;;; (push a-key processed-keys) eingefuegt.
;;;
;;; Revision 1.10  1993/05/27  12:51:22  jh
;;; Inlining von tagbody-forms und let/cc-forms eingebaut.
;;;
;;; Revision 1.9  1993/05/25  13:36:36  jh
;;; Testmeldungen entfernt.
;;;
;;; Revision 1.8  1993/05/25  13:29:26  jh
;;; Inlining von Funktionen mit &key- und &rest-Parametern eingebaut.
;;;
;;; Revision 1.7  1993/05/19  11:37:24  jh
;;; *max-inline-weight* von 20 auf 10 verkleinert.
;;;
;;; Revision 1.6  1993/05/18  09:16:55  jh
;;; Die Reihenfolge der neuen Parametervariablen korrigiert.
;;;
;;; Revision 1.5  1993/05/11  09:03:04  hk
;;; inline-app, arguments: Fehler bei optionalen Paramtern behoben.
;;;
;;; Revision 1.4  1993/05/10  12:24:15  jh
;;; Schalter *no-inlining* eingebaut. mv-lambda wird jetzt unterstuetzt.
;;;
;;; Revision 1.3  1993/02/16  15:16:47  hk
;;; $ eingefuegt.
;;;
;;; Revision 1.2  1993/01/28  12:06:07  jh
;;; Fehler beseitigt.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "weight")
(require "p3main")

;;------------------------------------------------------------------------------
;; Variable zur Steuerung der Gespr"achigkeit
;; 0: nichts
;; 1: Gesamtzahl
;; 2: Fuer jede Funktion
;;------------------------------------------------------------------------------
(defvar *inline-verbosity* 1)

;;------------------------------------------------------------------------------
;; *max-inline-weight* gibt das maximale Gewicht an, bis zu dem eine Funktion
;; inline-compiliert werden soll.
;;------------------------------------------------------------------------------

(defvar *max-inline-weight* 30)

(defun set-max-inline-weight (weight)
  (setf *max-inline-weight* weight))

(defvar *only-application* nil)

;;------------------------------------------------------------------------------
;; Makro zur besseren Lesbarkeit.
;;------------------------------------------------------------------------------

(defmacro inline-field (field &optional history)
  `(setf ,field (inline-form ,field ,history)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Find unique names for generated named-constants and global functions 
;;------------------------------------------------------------------------------
(defvar *named-const-gen-counter*)

(defun named-const-name ()
  (let ((*package* (?package *module*)))
    (loop
     (incf *named-const-gen-counter*)
     (let ((name (intern-postnum "NC" *named-const-gen-counter*)))
       (unless (find name (?named-const-list *module*) :key #'?symbol)
         (return name))))))

(defun clone-fun-name (name)
  (let ((*package* (?package *module*))
        (i 0))
    (loop
     (incf i)
     (let ((new (if (consp name)
                    (list (first name) (intern-postnum (second name) i))
                    (intern-postnum name i))))
       (unless (find new (?fun-list *module*) :key #'?symbol :test #'equal)
         (return new))))))

;;------------------------------------------------------------------------------
;; inline-module untersucht die im Modul definierten Funktionen sowie die
;; toplevel-forms nach Applikationen, die inline-compiliert werden sollen.
;;------------------------------------------------------------------------------

(defvar *no-inlining* nil)

(defun inline-module (a-module)
  (unless *no-inlining*
    (setq *named-const-gen-counter* 0)
    (mapc #'(lambda (fun) (when (<= (export-weight fun) *max-inline-weight*)
                            (setf (?inline fun) t)))
          (?fun-list a-module))
    (inline-fun-def-list (?fun-list a-module))
    (inline-fun-def (?toplevel-forms a-module))))

(defun do-inline ()
  (clicc-message "Inlining ...")
  (init-inline-statistics)
  (inline-module *module*)
  (write-inline-statistics))

;;------------------------------------------------------------------------------
;; Variablen und Funktionen fuer die Inline-Statistik.
;;------------------------------------------------------------------------------

;; Tabelle zur Inline-Statistik bestehend aus Paaren
;; (Funktionsname . Anzahl der Inline-Compilationen)
(defvar *inline-statistics*)

(defun init-inline-statistics ()
  (setf *inline-statistics* nil))

(defun inc-inline-counter (fun-name)
  (let ((pair (assoc fun-name *inline-statistics* :test #'eq)))
    (if pair
        (incf (cdr pair))
        (push (cons fun-name 1) *inline-statistics*))))

(defun write-inline-statistics ()
  (when (> *inline-verbosity* 0)
    (let ((total-number-of-inline-compilations
           (reduce #'(lambda (x y) (+ x (cdr y)))
		   *inline-statistics*
		   :initial-value 0)))
      (setf *inline-statistics*
            (sort *inline-statistics* #'(lambda (e1 e2)
                                          (> (cdr e1) (cdr e2)))))
      (clicc-message-line)
      (clicc-message "~D application~:p inline compiled." 
                     total-number-of-inline-compilations)
      (when (> *inline-verbosity* 1)
        (dolist (entry *inline-statistics*)
          (let ((fun-name (car entry))
                (number-of-inline-compilations (cdr entry)))
            (clicc-message "~A ~D time~:P inline compiled."
                           fun-name
                           number-of-inline-compilations)))
        (clicc-message-line)))))

;;------------------------------------------------------------------------------

(defmethod inline-fun-def ((a-simple-fun simple-fun) &optional history)
  (let ((*current-fun* a-simple-fun))
    ;; In Funktionen, die selbst inline kompiliert werden sollen und die
    ;; exportiert werden, wird nicht inline kompiliert.
    (unless (and (?inline a-simple-fun) (?exported a-simple-fun))
      (inline-field (?body a-simple-fun) (cons a-simple-fun history)))
    a-simple-fun))

(defmethod inline-fun-def ((a-local-fun local-fun) &optional history)
  (inline-field (?body a-local-fun) (cons a-local-fun history))
  a-local-fun)

(defun inline-fun-def-list (fun-def-list &optional history)
  (mapc #'(lambda (fun-def) (inline-fun-def fun-def history)) fun-def-list))

(defun inline-form-list (form-list &optional history)
  (unless (endp form-list)
    (inline-field (first form-list) history)
    (inline-form-list (rest form-list) history)))

;;------------------------------------------------------------------------------
;; inline-form versucht Applikationen innerhalb eines Zwischensprachausdrucks
;; inline zu compilieren.
;;------------------------------------------------------------------------------

(defmethod inline-form ((a-form form) &optional history)
  (declare (ignore history))
  a-form)

(defmethod inline-form ((a-cont cont) &optional history)
  (declare (ignore history))
  (clicc-message "In ~S Continuation outside app-form: ~S"
                 (?symbol *current-fun*)
                 a-cont)
  a-cont)

(defmethod inline-form ((an-app app) &optional history)
  (inline-form-list (?arg-list an-app) history)
  (let ((form (?form an-app)))
    (if (inline-app-p an-app history)
        (progn
          (inc-inline-counter (?symbol form))
          (inline-form
           (let ((*current-fun* form)
                 (*only-application* (and (= (?used form) 1)
                                          (not (and (global-fun-p form)
                                                    (?exported form))))))
             (unless (or (eq (?inline form) 'copyable)
                         *only-application*)
               (make-copyable form))
             (inline-app an-app))
           (cons form history)))
        (progn
          (unless (cont-p form)
            (inline-field (?form an-app)))
          an-app))))

(defmethod ?inline ((an-imported-fun imported-fun))
  'copyable)

(defmethod inline-form ((a-setq-form setq-form) &optional history)
  (inline-field (?form a-setq-form) history)
  a-setq-form)

(defmethod inline-form ((a-progn-form progn-form) &optional history)
  (inline-form-list (?form-list a-progn-form) history)
  a-progn-form)

(defmethod inline-form ((an-if-form if-form) &optional history)
  (inline-field (?pred an-if-form) history)
  (inline-field (?then an-if-form) history)
  (inline-field (?else an-if-form) history)
  an-if-form)

(defmethod inline-form ((a-switch-form switch-form) &optional history)
  (inline-field (?form a-switch-form) history)
  (inline-form-list (?case-list a-switch-form) history)
  a-switch-form)

(defmethod inline-form ((a-labeled-form labeled-form) &optional history)
  (inline-field (?form a-labeled-form) history)
  a-labeled-form)

(defmethod inline-form ((a-let*-form let*-form) &optional history)
  (inline-form-list (?init-list a-let*-form) history)
  (inline-field (?body a-let*-form) history)
  a-let*-form)

(defmethod inline-form ((a-labels-form labels-form) &optional history)
  (inline-field (?body a-labels-form) history)
  (inline-fun-def-list (?fun-list a-labels-form) history)
  a-labels-form)

(defmethod inline-form ((a-let/cc-form let/cc-form) &optional history)
  (inline-field (?body a-let/cc-form) history)
  a-let/cc-form)

(defmethod inline-form ((a-tagbody-form tagbody-form) &optional history)
  (when (?first-form a-tagbody-form)
    (inline-field (?first-form a-tagbody-form) history))
  (mapc #'(lambda (a-tagged-form) (inline-field (?form a-tagged-form) history))
        (?tagged-form-list a-tagbody-form))
  a-tagbody-form)

(defmethod inline-form ((a-tagged-form tagged-form) &optional history)
  (declare (ignore history))
  a-tagged-form)

(defmethod inline-form ((a-mv-lambda mv-lambda) &optional history)
  (inline-field (?arg a-mv-lambda) history)
  (inline-field (?body a-mv-lambda) history)
  a-mv-lambda)

;;------------------------------------------------------------------------------
;; inline-app-p entscheidet, ob eine Funktionsapplikation inline compiliert
;; werden soll.
;;------------------------------------------------------------------------------

(defun inline-app-p (an-app history)
  (let ((app-form (?form an-app)))
    (and (fun-p app-form)
         (?body app-form)
         (not (member app-form history))
         (null (?local-funs app-form))
         (keywords-ok-p (?params app-form) (?arg-list an-app))
         (or
          (and (= (?used app-form) 1)
               (not (and (global-fun-p app-form)
                         (?exported app-form))))
          (<= (inline-delta *current-fun* an-app) *max-inline-weight*)))))

(defun keywords-ok-p (params args)
  (or (null (?key-list params))
      (let* ((number-of-args (length args))
             (number-of-simple-and-opt-params (+ (length (?var-list params))
                                                 (length (?opt-list params))))
             (key-args (when (> number-of-args number-of-simple-and-opt-params)
                         (subseq args number-of-simple-and-opt-params))))
        (or (null key-args)
            (when (evenp (length key-args))
              (subsetp (split-args key-args)
                       (mapcar #'?sym (?key-list params))))))))

;;------------------------------------------------------------------------------
;; make-copyable steckt jedes structured-literal in eine defined-named-const,
;; damit das structured-literal weiterhin nur ein angewandtes Vorkommen hat.
;;------------------------------------------------------------------------------

(defun make-copyable (a-fun)
  (setf (?inline a-fun) 'copyable)
  (mc-form (?params a-fun))
  (setf (?body a-fun) (mc-form (?body a-fun))))

(defmethod mc-form ((a-form form))
  a-form)

#+CMU(defun mc-opt/key (opt/key)
       (setf (?init opt/key) (mc-form (?init opt/key))))

(defmethod mc-form ((parameters params))
  (labels (#-CMU(mc-opt/key (opt/key)
                  (setf (?init opt/key) (mc-form (?init opt/key)))))
    (mapc #'mc-opt/key (?opt-list parameters))
    (mapc #'mc-opt/key (?key-list parameters))))

(defmethod mc-form ((a-structured-literal structured-literal))
  (let ((const (make-defined-named-const :value a-structured-literal
                                         :symbol (named-const-name))))
    (push const (?named-const-list *module*))
    const))

(defmethod mc-form ((an-app app))
  (setf (?form an-app) (mc-form (?form an-app))
        (?arg-list an-app) (mapcar #'mc-form (?arg-list an-app)))
  an-app)

(defmethod mc-form ((a-cont cont))
  a-cont)

(defmethod mc-form ((a-setq-form setq-form))
  (setf (?form a-setq-form) (mc-form (?form a-setq-form)))
  a-setq-form)

(defmethod mc-form ((a-progn-form progn-form))
  (setf (?form-list a-progn-form) (mapcar #'mc-form (?form-list a-progn-form)))
  a-progn-form)

(defmethod mc-form ((an-if-form if-form))
  (setf (?pred an-if-form) (mc-form (?pred an-if-form))
        (?then an-if-form) (mc-form (?then an-if-form))
        (?else an-if-form) (mc-form (?else an-if-form)))
  an-if-form)

(defmethod mc-form ((a-switch-form switch-form))
  (setf (?form a-switch-form) (mc-form (?form a-switch-form))
        (?otherwise a-switch-form) (mc-form (?otherwise a-switch-form)))
  (mapc #'mc-form (?case-list a-switch-form))
  a-switch-form)

(defmethod mc-form ((a-labeled-form labeled-form))
  (setf (?form a-labeled-form) (mc-form (?form a-labeled-form)))
  a-labeled-form)

(defmethod mc-form ((a-let*-form let*-form))
  (setf (?init-list a-let*-form) (mapcar #'mc-form (?init-list a-let*-form))
        (?body a-let*-form) (mc-form (?body a-let*-form)))
  a-let*-form)

(defmethod mc-form ((a-labels-form labels-form))
  (mapc #'make-copyable (?fun-list a-labels-form))
  (setf (?body a-labels-form) (mc-form (?body a-labels-form)))
  a-labels-form)

(defmethod mc-form ((a-let/cc-form let/cc-form))
  (setf (?body a-let/cc-form) (mc-form (?body a-let/cc-form)))
  a-let/cc-form)

(defmethod mc-form ((a-tagbody-form tagbody-form))
  (when (?first-form a-tagbody-form)
    (setf (?first-form a-tagbody-form) (mc-form (?first-form a-tagbody-form))))
  (mapc #'(lambda (a-tagged-form)
            (setf (?form a-tagged-form) (mc-form (?form a-tagged-form))))
        (?tagged-form-list a-tagbody-form))
  a-tagbody-form)

(defmethod mc-form ((a-tagged-form tagged-form))
  a-tagged-form)

(defmethod mc-form ((a-mv-lambda mv-lambda))
  (mc-form (?params a-mv-lambda))
  (setf (?body a-mv-lambda) (mc-form (?body a-mv-lambda))
        (?arg a-mv-lambda) (mc-form (?arg a-mv-lambda)))
  a-mv-lambda)

;;------------------------------------------------------------------------------
;; inline-app bildet eine let*-form zur Bindung der Argumente an neue lokale
;; Variablen und kopiert den Funktionsrumpf unter Beachtung der neuen Bindungen.
;;------------------------------------------------------------------------------
(defmethod inline-app ((an-app app))
  (multiple-value-bind (var-list init-list subst-map)
      (bind-arguments (?params (?form an-app)) (?arg-list an-app))
    (let ((new-body (zs-copy (?body (?form an-app)) subst-map)))
      (if (null subst-map)
          new-body
          (make-let*-form :var-list var-list
                          :init-list init-list
                          :body new-body)))))

(defun split-args (args)
  (if (null args)
      (values () ())
      (multiple-value-bind (rest-keys rest-args)
          (split-args (rest (rest args)))
        (values (cons (first args) rest-keys)
                (cons (first (rest args)) rest-args)))))

(defun bind-arguments (params args)
  (let ((new-var-queue (empty-queue))
        (init-queue (empty-queue))
        (subst-map ()))
    
    ;; Zuerst werden die benoetigten Parameter versorgt.
    ;;-------------------------------------------------- 
    (dolist (a-var (?var-list params))
      (let ((new-var (new-variable a-var)))
        (add-q new-var new-var-queue)
        (push (cons a-var new-var) subst-map)
        (add-q (pop args) init-queue)))
    
    ;; Dann sind die optionalen Parameter dran.
    ;;----------------------------------------- 
    (dolist (an-opt (?opt-list params))
      (let ((new-var (new-variable (?var an-opt))))
        (add-q new-var new-var-queue)
        (push (cons (?var an-opt) new-var) subst-map)
        (add-q (if args
                   (first args)
                   (zs-copy (?init an-opt) subst-map))
               init-queue)
        (when (?suppl an-opt)
          (let ((new-suppl-var (new-variable (?suppl an-opt))))
            (add-q new-suppl-var new-var-queue)
            (push (cons (?suppl an-opt) new-suppl-var) subst-map)
            (add-q (if args
                       (get-symbol-bind t)
                       empty-list)
                   init-queue)))
        (pop args)))
    
    ;; Und jetzt kriegen's die Keyword-Parameter.
    ;;-------------------------------------------
    (if (?key-list params)
        (multiple-value-bind (keys key-args) (split-args args)
          (let ((key-params (?key-list params))
                (processed-keys ())
                (make-rest-var (and (?rest params) (is-used (?rest params))))
                (rest-queue (empty-queue)))
            (dolist (a-key keys)
              (let* ((key-processed (member a-key processed-keys))
                     (key-param (find a-key key-params :key #'?sym))
                     (arg (first key-args))
                     (new-var (if key-processed 
                                  ;; Da der Key bereits abgearbeitet wurde, wird
                                  ;; die neue Variable nur zur Aufbewahrung des
                                  ;; berechneten Wertes fuer einen moeglichen
                                  ;; Restparameter benutzt.
                                  (make-local-static :read 0
                                                     :write 1
                                                     :symbol 'key
                                                     :type (?type arg))
                                  (new-variable (?var key-param)))))
                (add-q new-var new-var-queue)
                (add-q arg init-queue)
                (unless key-processed
                  (push a-key processed-keys)
                  (push (cons (?var key-param) new-var) subst-map)
                  (when (?suppl key-param)
                    (let ((new-suppl-var (new-variable (?suppl key-param))))
                      (add-q new-suppl-var new-var-queue)
                      (push (cons (?suppl key-param) new-suppl-var)
                            subst-map)
                      (add-q (get-symbol-bind t) init-queue))))
                (when make-rest-var
                  (incf (?read new-var))
                  (add-q a-key rest-queue)
                  (add-q (make-var-ref :var new-var
                                       :type (?type arg))
                         rest-queue))
                (pop key-args)))
            (dolist (a-key-param key-params)
              (unless (member (?sym a-key-param) processed-keys)
                (let ((new-var (new-variable (?var a-key-param))))
                  (add-q new-var new-var-queue)
                  (push (cons (?var a-key-param) new-var) subst-map)
                  (add-q (zs-copy (?init a-key-param) subst-map) init-queue)
                  (when (?suppl a-key-param)
                    (let ((new-suppl-var (new-variable (?suppl a-key-param))))
                      (add-q new-suppl-var new-var-queue)
                      (push (cons (?suppl a-key-param) new-suppl-var)
                            subst-map)
                      (add-q empty-list init-queue))))))
            
            ;; Neben den Keyword-Param. gibt's evtl. noch einen Rest-Parameter.
            ;;-----------------------------------------------------------------
            (when make-rest-var
              (let ((new-rest-var (new-variable (?rest params)))
                    (list-fun (get-global-fun 'L::list)))
                (add-q new-rest-var new-var-queue)
                (push (cons (?rest params) new-rest-var) subst-map)
                (add-q (if (empty-queue-p rest-queue)
                           empty-list
                           (make-app :form list-fun
                                     :arg-list (queue2list rest-queue)
                                     :read-list -1
                                     :write-list -1
                                     :called-funs (list list-fun)
                                     :downfun-list ()
                                     :other-funs nil))
                       init-queue)))))
        
        ;; Nun noch der Rest-Parameter solo.
        ;;----------------------------------
        (when (?rest params)
          (let ((new-rest-var (new-variable (?rest params)))
                (list-fun (get-global-fun 'L::list)))
            (add-q new-rest-var new-var-queue)
            (push (cons (?rest params) new-rest-var) subst-map)
            (add-q (if args
                       (if (is-used (?rest params))
                           (make-app :form list-fun
                                     :arg-list args
                                     :read-list -1
                                     :write-list -1
                                     :called-funs (list list-fun)
                                     :downfun-list ()
                                     :other-funs nil)
                           (make-progn-form :form-list args))
                       empty-list)
                   init-queue))))

    (values (queue2list new-var-queue)
            (queue2list init-queue)
            subst-map)))

(defmethod new-variable ((a-local-static local-static))
  (if *only-application*
      a-local-static
      (let ((new-local-static (make-local-static
                               :read (?read a-local-static)
                               :write (?write a-local-static)
                               :symbol (?symbol a-local-static)
                               :type (?type a-local-static))))
        (when (slot-boundp a-local-static 'level)
          (setf (?level new-local-static) (?level a-local-static)))
        new-local-static)))

(defmethod new-variable ((a-var var))
  a-var)

(defun new-local-var-list (var-list subst-map)
  (let ((new-var-queue (empty-queue)))
    (dolist (old-var var-list)
      (let ((new-var (new-variable old-var)))
        (add-q new-var new-var-queue)
        (push (cons old-var new-var) subst-map)))
    (values (queue2list new-var-queue) subst-map)))

(defun new-parameters (params subst-map)
  (multiple-value-bind (new-all-vars new-subst-map)
      (new-local-var-list (?all-vars params) subst-map)
    (labels ((new-internal (var)
               (new-param-var var new-subst-map)))
      (values
       (make-params :var-list (mapcar #'new-internal (?var-list params))
                    :opt-list (mapcar #'new-internal (?opt-list params))
                    :rest (if (?rest params)
                              (new-internal (?rest params))
                              nil)
                    :key-list (mapcar #'new-internal (?key-list params))
                    :allow-other-keys (?allow-other-keys params)
                    :all-vars new-all-vars)
       new-subst-map))))

(defmethod new-param-var ((a-var var) subst-map)
  (zs-copy a-var subst-map))

(defmethod new-param-var ((an-opt opt) subst-map)
  (make-opt :var (zs-copy (?var an-opt) subst-map)
            :init (zs-copy (?init an-opt) subst-map)
            :suppl (if (?suppl an-opt)
                       (zs-copy (?suppl an-opt) subst-map)
                       nil)))

(defmethod new-param-var ((a-key key) subst-map)
  (incf (?used (?sym a-key)))
  (make-key :var (zs-copy (?var a-key) subst-map)
            :init (zs-copy (?init a-key) subst-map)
            :suppl (if (?suppl a-key)
                       (zs-copy (?suppl a-key) subst-map)
                       nil)
            :sym (?sym a-key)))

;;------------------------------------------------------------------------------
;; zs-copy kopiert einen Zwischensprachausdruck, wobei die neuen Variablen-
;; bindungen beachtet werden.
;;------------------------------------------------------------------------------

(defun zs-copy (form subst-map)
  (if *only-application*
      form
      (zs-cp form subst-map)))

(defmethod zs-cp ((a-local-static local-static) subst-map)
  (let ((entry (assoc a-local-static subst-map :test #'eq)))
    (if entry
        (cdr entry)
        a-local-static)))

(defmethod zs-cp ((a-global-static global-static) subst-map)
  (declare (ignore subst-map))
  a-global-static)

(defmethod zs-cp ((an-imported-static imported-static) subst-map)
  (declare (ignore subst-map))
  an-imported-static)

(defmethod zs-cp ((a-dynamic dynamic) subst-map)
  (declare (ignore subst-map))
  a-dynamic)

(defmethod zs-cp ((a-var-ref var-ref) subst-map)
  (make-var-ref :var (zs-cp (?var a-var-ref) subst-map)))

(defmethod zs-cp ((a-named-const named-const) subst-map)
  (declare (ignore subst-map))
  a-named-const)

(defmethod zs-cp ((a-literal literal) subst-map)
  (declare (ignore subst-map))
  a-literal)

(defmethod zs-cp ((a-class-def class-def) subst-map)
  (declare (ignore subst-map))
  a-class-def)

(defmethod zs-cp ((a-fun fun) subst-map)
  (declare (ignore subst-map))
  (when (defined-fun-p a-fun)
    (incf (?used a-fun)))
  a-fun)

(defmethod zs-cp ((a-foreign-fun foreign-fun) subst-map)
  (declare (ignore subst-map))
  a-foreign-fun)

(defmethod zs-cp ((an-app app) subst-map)
  (make-app :form (zs-cp (?form an-app) subst-map)
            :arg-list (mapcar #'(lambda (a-form)
                                  (zs-cp a-form subst-map))
                              (?arg-list an-app))
            :read-list (?read-list an-app)
            :write-list (?write-list an-app)
            :mv-used (?mv-used an-app)
            :downfun-list (if (slot-boundp an-app 'downfun-list)
                              (?downfun-list an-app)
                              nil)
            :called-funs (?called-funs an-app)
            :other-funs (?other-funs an-app)
            :type (?type an-app)))

(defmethod zs-cp ((a-setq-form setq-form) subst-map)
  (make-setq-form :location (zs-cp (?location a-setq-form) subst-map)
                  :form (zs-cp (?form a-setq-form) subst-map)
                  :type (?type a-setq-form)))

(defmethod zs-cp ((a-progn-form progn-form) subst-map)
  (make-progn-form :form-list (mapcar #'(lambda (a-form)
                                          (zs-cp a-form subst-map))
                                      (?form-list a-progn-form))
                   :type (?type a-progn-form)))

(defmethod zs-cp ((an-if-form if-form) subst-map)
  (make-if-form :pred (zs-cp (?pred an-if-form) subst-map)
                :then (zs-cp (?then an-if-form) subst-map)
                :else (zs-cp (?else an-if-form) subst-map)
                :type (?type an-if-form)))

(defmethod zs-cp ((a-switch-form switch-form) subst-map)
  (make-switch-form :form (zs-cp (?form a-switch-form) subst-map)
                    :case-list (mapcar #'(lambda (a-form)
                                           (zs-cp a-form subst-map))
                                       (?case-list a-switch-form))
                    :otherwise (zs-cp (?otherwise a-switch-form)
                                      subst-map)
                    :type (?type a-switch-form)))

(defmethod zs-cp ((a-labeled-form labeled-form) subst-map)
  (make-labeled-form :value (zs-cp (?value a-labeled-form) subst-map)
                     :form (zs-cp (?form a-labeled-form) subst-map)
                     :type (?type a-labeled-form)))

(defmethod zs-cp ((a-let*-form let*-form) subst-map)
  (multiple-value-bind (new-var-list new-subst-map)
      (new-local-var-list (?var-list a-let*-form) subst-map)
    (make-let*-form :var-list new-var-list
                    :init-list (mapcar #'(lambda (a-form)
                                           (zs-cp a-form new-subst-map))
                                       (?init-list a-let*-form))
                    :body (zs-cp (?body a-let*-form) new-subst-map)
                    :type (?type a-let*-form))))

(defmethod zs-cp ((a-let/cc-form let/cc-form) subst-map)
  (let* ((cont (?cont a-let/cc-form))
         (new-cont
          (make-cont :read (?read cont)
                     :write (?write cont)
                     :type (?type cont)
                     :binding-stack-level (?binding-stack-level cont)
                     :mv-spec (?mv-spec cont)
                     :mv-calls (?mv-calls cont)
                     :only-local (?only-local cont)
                     :unknown-caller (?unknown-caller cont)
                     :result-type (?result-type cont)
                     :type-env (?type-env cont)
                     :result-spec (?result-spec cont)
                     :level (if (not (local-fun-p *current-fun*))
                                0
                                (?level *current-fun*)))))
    (push (cons cont new-cont) subst-map)
    (make-let/cc-form :cont new-cont
                      :body (zs-cp (?body a-let/cc-form) subst-map))))

(defmethod zs-cp ((a-tagbody-form tagbody-form) subst-map)
  (dolist (a-tagged-form (?tagged-form-list a-tagbody-form))
    (push (cons a-tagged-form
                (make-tagged-form :used (?used a-tagged-form)))
          subst-map))
  (let ((new-tagbody-form
         (make-tagbody-form :first-form (zs-cp (?first-form a-tagbody-form)
                                               subst-map)
                            :tagged-form-list
                            (mapcar #'(lambda (a-tagged-form)
                                        (zs-cp a-tagged-form subst-map))
                                    (?tagged-form-list a-tagbody-form)))))
    (dolist (old-tagged-form (?tagged-form-list a-tagbody-form))
      (let ((new-tagged-form (zs-cp old-tagged-form subst-map)))
        (setf (?form new-tagged-form)
              (zs-cp (?form old-tagged-form) subst-map))
        (setf (?tagbody new-tagged-form)
              new-tagbody-form)))
    new-tagbody-form))

(defmethod zs-cp ((a-tagged-form tagged-form) subst-map)
  (cdr (assoc a-tagged-form subst-map :test #'eq)))

(defmethod zs-cp ((a-mv-lambda mv-lambda) subst-map)
  (multiple-value-bind (new-params new-subst-map)
      (new-parameters (?params a-mv-lambda) subst-map)
    (make-mv-lambda :params new-params
                    :arg (zs-cp (?arg a-mv-lambda) subst-map)
                    :body (zs-cp (?body a-mv-lambda) new-subst-map)
                    :mv-spec (?mv-spec a-mv-lambda)
                    :mv-calls (?mv-calls a-mv-lambda))))

;; labels-forms werden noch nicht unterstuetzt.

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------

(defvar *simplified-params* 0)

(defun simplify-params (used-dynamic-vars-fun)
  (let ((*simplified-params* 0))
    (clicc-message "Simplifying lambda-lists.")
    (mapc #'(lambda (fun) (simplify-params1 fun used-dynamic-vars-fun))
          (?fun-list *module*))
    (clicc-message "~d lambda-lists simplified."
                   *simplified-params*)))
    

(defun simplify-params1 (fun used-dynamic-vars-fun)
  (let* ((*only-application* nil)
         (params (?params fun))
         (opt-list (?opt-list params))
         (key-list (?key-list params)))
    (labels ((varname (var)
               (if (dynamic-p var)
                   (?symbol (?sym var))
                   (?symbol var))))
      (when (and (or opt-list key-list)
                 (not (dynamic-var-problems params used-dynamic-vars-fun)))
        (incf *simplified-params*)
        (let ((var-list (?var-list params))
              (rest (?rest params))
              (var-queue (empty-queue))
              (new-var-queue (empty-queue))
              (arg-queue (empty-queue))
              (subst-map nil)
              (offset -1))
          (dolist (var var-list)
            (let ((new-var (make-local-static :symbol (varname var)
                                              :offset (incf offset))))
              (push (cons var new-var) subst-map)
              (add-q new-var new-var-queue)
              (add-q var var-queue)
              (add-q new-var arg-queue)))
          (setf (?var-list params) (queue2list new-var-queue))
          (dolist (opt opt-list)
            (let ((suppl (?suppl opt)))
              (when suppl
                (let ((new-var (make-local-static :symbol (varname suppl)
                                                  :offset (incf offset))))
                  (push (cons suppl new-var) subst-map)
                  (setf (?suppl opt) new-var)
                  (add-q suppl var-queue)
                  (add-q new-var arg-queue)))))
          (dolist (opt opt-list)
            (let* ((var (?var opt))
                   (new-var (make-local-static :symbol (varname var)
                                               :offset (incf offset))))
              (push (cons var new-var) subst-map)
              (setf (?var opt) new-var)
              (add-q var var-queue)
              (add-q new-var arg-queue)))
          (dolist (key key-list)
            (let* ((var (?var key))
                   (new-var (make-local-static :symbol (varname var)
                                               :offset (incf offset))))
              (push (cons var new-var) subst-map)
              (setf (?var key) new-var)
              (add-q var var-queue)
              (add-q new-var arg-queue)))
          (when rest
            (let ((new-var (make-local-static :symbol (varname rest)
                                              :offset (incf offset))))
              (push (cons rest new-var) subst-map)
              (setf (?rest params) new-var)
              (add-q rest var-queue)
              (add-q new-var arg-queue)))
          (dolist (key key-list)
            (let ((suppl (?suppl key)))
              (when suppl
                (let ((new-var (make-local-static :symbol (varname suppl)
                                                  :offset (incf offset))))
                  (push (cons suppl new-var) subst-map)
                  (setf (?suppl key) new-var)
                  (add-q suppl var-queue)
                  (add-q new-var arg-queue)))))
          (dolist (opt opt-list)
            (setf (?init opt) (zs-copy (?init opt) subst-map)))
          (dolist (key key-list)
            (setf (?init key) (zs-copy (?init key) subst-map)))
          (setf (?all-vars params) (queue2list arg-queue))
          (let* ((vars (queue2list var-queue))
                 (args (mapcar #'(lambda (var)
                                   (make-var-ref :var var))
                               (queue2list arg-queue)))
                 (inner-fun (make-global-fun
                             :params (make-params 
                                      :var-list vars
                                      :opt-list nil
                                      :rest nil
                                      :key-list nil
                                      :allow-other-keys
                                      (?allow-other-keys params)
                                      :all-vars vars)
                             :symbol (clone-fun-name (?symbol fun))
                             :body (?body fun)
                             :par-spec (list-length vars)
                             :used 1)))
            (push inner-fun (?fun-list *module*))
            (setf (?inline fun) t
                  (?body fun) (make-app :form inner-fun
                                        :arg-list args))))))))

(defun used-dynamic-vars-with-side-effects (form)
  (let ((effect (empty-effect))
        (local-effect (make-instance 'local-effect)))
    (effect-of-form form effect local-effect)
    (unify-lists 
     (unify-lists (?read-list effect)
                  (?write-list effect))
     (unify-lists (?read-list local-effect)
                  (?write-list effect)))))

(defun used-dynamic-vars-simple1 (form)
  (typecase form
    (app (if (cont-p (?form form))
             (mapcan #'used-dynamic-vars-simple1 (?arg-list form))
             '(unknown)))
    (setq-form (append (used-dynamic-vars-simple1 (?location form))
                       (used-dynamic-vars-simple1 (?form form))))
    (progn-form (mapcan #'used-dynamic-vars-simple1 (?form-list form)))
    (if-form (append (used-dynamic-vars-simple1 (?pred form))
                     (used-dynamic-vars-simple1 (?then form))
                     (used-dynamic-vars-simple1 (?else form))))
    (switch-form (append (used-dynamic-vars-simple1 (?form form))
                         (mapcan #'used-dynamic-vars-simple1 (?case-list form))
                         (used-dynamic-vars-simple1 (?otherwise form))))
    (labeled-form (used-dynamic-vars-simple1 (?form form)))
    (let*-form (append (remove-if-not #'dynamic-p (?var-list form))
                       (mapcan #'used-dynamic-vars-simple1 (?init-list form))
                       (used-dynamic-vars-simple1 (?body form))))
    (labels-form '(unknown))
    (let/cc-form (used-dynamic-vars-simple1 (?body form)))
    (tagbody-form (append (used-dynamic-vars-simple1 (?first-form form))
                          (mapcan #'(lambda (tagged-form)
                                       (used-dynamic-vars-simple1
                                        (?form tagged-form)))
                                   (?tagged-form-list form))))
    (tagged-form nil)
    (mv-lambda (append (remove-if-not #'dynamic-p (?all-vars (?params form)))
                       (mapcan #'used-dynamic-vars-simple1
                                (mapcar #'?init
                                        (append (?opt-list (?params form))
                                                (?key-list (?params form)))))
                       (used-dynamic-vars-simple1 (?body form))
                       (used-dynamic-vars-simple1 (?arg form))))
    (var-ref (if (dynamic-p (?var form)) (list (?var form)) nil))))

(defun used-dynamic-vars-simple (form)
  (let ((used-dynamic-vars (used-dynamic-vars-simple1 form)))
    (if (member 'unknown used-dynamic-vars :test #'eq)
        -1
        used-dynamic-vars)))

(defun dynamic-var-problems (params used-dynamic-vars-fun)
  (let* ((var-list (?var-list params))
         (opt-list (?opt-list params))
         (rest (?rest params))
         (key-list (?key-list params))
         (dyn-vars (remove-if-not #'dynamic-p var-list)))
    (labels ((test-opt&key (opt/key-list)
               (block test-opt&key      ; CLISP compatibility
                 (dolist (opt/key opt/key-list)
                   (let* ((var (?var opt/key))
                          (suppl (?suppl opt/key)))
                     (let ((used-vars (funcall used-dynamic-vars-fun
                                               (?init opt/key))))
                       (if (integerp used-vars)
                           (return-from test-opt&key dyn-vars)
                           (dolist (dyn-var dyn-vars)
                             (when (member dyn-var used-vars :test #'eq)
                               (return-from test-opt&key t))))
                       (when (dynamic-p var)
                         (pushnew var dyn-vars))
                       (when (dynamic-p suppl)
                         (pushnew suppl dyn-vars)))))
                 nil)))
      (or (test-opt&key opt-list)
          (progn
            (when (dynamic-p rest)
              (pushnew rest dyn-vars))
            (test-opt&key key-list))))))
          
;;------------------------------------------------------------------------------
(provide "inline")
