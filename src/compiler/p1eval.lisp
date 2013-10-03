;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Ein Interpretierer fuer Teile der Zwischensprache.
;;;
;;; $Revision: 1.70 $
;;; $Log: p1eval.lisp,v $
;;; Revision 1.70  1994/04/28  13:40:44  hk
;;; Fehler in zw-eval (mv-lambda t) behoben.
;;;
;;; Revision 1.69  1994/03/11  14:44:40  hk
;;; break kann interpretiert werden, damit man Fehler in
;;; Makro-Expansionsfunktionen eingrenzen kann.
;;;
;;; Revision 1.68  1994/03/08  16:26:32  hk
;;; zw-eval f"ur let* bricht ab, wenn dynamische Variablen gebunden werden
;;; sollen.
;;;
;;; Revision 1.67  1994/03/03  13:48:27  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.66  1994/02/09  15:30:38  hk
;;; p1-eval kennt nun rt::set-prop und rt::remf-internal, die aus der
;;; Makroexpansion von (setf getf) und remf entstehen.
;;;
;;; Revision 1.65  1994/02/08  14:49:09  hk
;;; eval fÜR mv-lambda eingebaut.
;;;
;;; Revision 1.64  1993/12/09  10:12:53  hk
;;; (method zw-apply imported-fun): (declare (ignore env)) gestrichen.
;;;
;;; Revision 1.63  1993/12/09  10:07:07  hk
;;; zw-apply auf imported-fun verwendet nun ggf. die Funktion im Slot
;;; syntax.
;;;
;;; Revision 1.62  1993/12/01  11:49:36  ft
;;; Beim Aufruf von make-hash-table in init-zw-sym-fun-hash-table die
;;; Groesse angegeben, da *zw-sym-fun-hash-table* konstant.
;;;
;;; Revision 1.61  1993/08/20  10:53:02  hk
;;; In p1-eval (array character) -> (array standard-char), da in vielen
;;; Implementierungen gilt: (array character) = (array T)
;;;
;;; Revision 1.60  1993/07/26  14:42:43  hk
;;; Klammerfehler behoben
;;;
;;; Revision 1.59  1993/07/26  13:48:06  hk
;;; Fehler in eval-structured-literal-value behoben: die Konstanten
;;; duerfen nicht destruktiv veraendert werden, da der urspruengliche Wert
;;; evt. der Wert einer Konstanten ist, der weiterhin verwendet werden
;;; kann.
;;;
;;; Revision 1.58  1993/07/19  15:21:32  hk
;;; Neu: eval-structured-literal-value
;;;
;;; Revision 1.57  1993/07/16  09:47:39  uho
;;; dito fuer #-(or CLISP CMU) auf #-PCL
;;;
;;; Revision 1.56  1993/07/16  09:43:54  uho
;;; Fuer Function als Specializer von #+(or CLISP CMU) auf #+PCL
;;; umgestellt.
;;;
;;; Revision 1.55  1993/07/13  14:22:40  hk
;;; Hash table fuer zw-symbol-fun hat nun test #'equal wg. (setf car)
;;;
;;; Revision 1.54  1993/07/02  11:27:59  hk
;;; (setf ..) Funktionen in init-zw-sym-fun-hash-table eingetragen
;;;
;;; Revision 1.53  1993/06/23  13:20:29  uho
;;; In p1-eval noch einen Fall fuer Symbol eingeschoben, da in CLtL1
;;; (functionp 'sym) immer T ist.
;;;
;;; Revision 1.52  1993/06/23  10:25:16  uho
;;; Inkompatibilitaet von LUCID und CLISP umgangen:
;;;     (typep hallo '(array character)) => ???
;;;
;;; Revision 1.51  1993/06/23  08:23:29  hk
;;; p1-eval: map-array nicht auf strings anwenden.
;;;
;;; Revision 1.50  1993/06/22  13:59:51  hk
;;; Fixed a typo
;;;
;;; Revision 1.49  1993/06/22  13:29:02  hk
;;; Resultat von p1-eval darf keine funktionalen Objekte enthalten.
;;;
;;; Revision 1.48  1993/06/22  08:28:45  uho
;;; Fehler in CLISP (sym-fun) umgangen.
;;;
;;; Revision 1.47  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.46  1993/05/19  15:14:23  hk
;;; *zw-sym-fun-hash-table* wird in init-zw-sym-fun-hash-table
;;; mit einer Hashtable initialisiert.
;;;
;;; Revision 1.45  1993/05/19  13:47:28  ft
;;; Auf Wunsch von Karsten einen Fehlerabbruch durch einen Abbruch des
;;; Evaluationsversuchs ersetzt.
;;;
;;; Revision 1.44  1993/05/13  13:41:51  hk
;;; Anpassung an CMU und CLISP.
;;;
;;; Revision 1.43  1993/04/30  13:48:28  hk
;;; init-zw-sym-fun-hash-table vervollstaendigt.
;;;
;;; Revision 1.42  1993/04/20  14:31:26  ft
;;; Auf besonderen Wunsch von Karsten wird bei Special-Variablen die
;;; Evaluation ohne Fehler abgebrochen.
;;;
;;; Revision 1.41  1993/04/15  07:29:33  hk
;;; init-zw-sym-fun-hash-table: Number Funktionen hinzugefuegt.
;;;
;;; Revision 1.40  1993/04/15  06:43:59  hk
;;; In init-zw-sym-fun-hash-table Sequence Funktionen eingefuegt.
;;;
;;; Revision 1.39  1993/04/07  14:03:58  hk
;;; Methode (zw-eval function) hinzugefuegt; fuer die Ausfuehrung von
;;; Lisp Systemfunktionen bei der Makroexpansion.
;;;
;;; Revision 1.38  1993/04/06  09:48:59  hk
;;; Predicates on Numbers hinzugefuegt.
;;;
;;; Revision 1.37  1993/04/06  09:40:34  hk
;;; init-zw-sym-fun-hash-table um some, every, notany und notevery erw.
;;;
;;; Revision 1.36  1993/04/05  14:32:53  hk
;;; Integerp in init-zw-sym-fun-hash-table.
;;;
;;; Revision 1.35  1993/03/22  17:36:12  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.34  1993/03/18  14:51:33  ft
;;; zw-eval um eine Methode ueber Klassen erweitert.
;;;
;;; Revision 1.33  1993/02/18  15:07:30  kl
;;; Liste der waehrend der Makroexpansion genutzten Funktionen erweitert.
;;;
;;; Revision 1.32  1993/02/17  11:04:28  hk
;;; Fehler in init-zw-sym-fun-hash-table behoben, anderes Makro.
;;;
;;; Revision 1.31  1993/02/16  17:31:11  hk
;;; Fehler in init-zw-sym-fun-hash-table behoben: CLICC-LISP -> LISP.
;;;
;;; Revision 1.30  1993/02/16  16:59:57  hk
;;; Revision Keyword eingefuegt, in init-zw-sym-fun-hash-table wird eine
;;; Zuordnung von Symbolen im clicc-lisp Package zu Funktionen im lisp Package
;;; hergestellt.
;;;
;;; Revision 1.29  1993/01/27  13:00:43  kl
;;; last und butlast koennen jetzt waehrend der Uebersetzungszeit ausgewertet
;;; werden.
;;;
;;; Revision 1.28  1993/01/19  12:57:15  hk
;;; Symbole werden nur noch evaluiert, wenn sie Konstanten sind, deren
;;; Wert bekannt ist.
;;;
;;; Revision 1.27  1993/01/14  14:40:07  hk
;;; Neue Funktionen in init-zw-sym-fun-hash-table.
;;;
;;; Revision 1.26  1993/01/06  13:19:01  hk
;;; init-zw-sym-fun-hash-table von p0init nach hier kopiert.
;;;
;;; Revision 1.25  1992/11/26  08:23:44  ft
;;; Abbruch der Auswertung, statt Fehler, bei Forwaerts-referenzierter
;;; Konstante.
;;;
;;; Revision 1.24  1992/11/24  13:14:01  ft
;;; Fehlermeldung aus zw-symbol-fun entfernt.
;;;
;;; Revision 1.23  1992/11/20  13:49:57  ft
;;; Abbruchmoeglichkeit fuer zw-eval geschaffen und Funktionsauswertung
;;; begrenzt.
;;;
;;; Revision 1.22  1992/09/29  12:15:56  hk
;;; Fehlermeldungstext.
;;;
;;; Revision 1.21  1992/09/25  17:14:16  kl
;;; Umstellung auf die neue Repraesentation der einfachen Literale.
;;;
;;; Revision 1.20  1992/09/14  13:56:12  kl
;;; constant-value-p nach zsdef.lisp verlegt.
;;;
;;; Revision 1.19  1992/09/04  12:02:35  kl
;;; Log message korrigiert.
;;;
;;; Revision 1.18  1992/09/04  12:01:45  kl
;;; In zw-eval werden benannte Konstanten, die nicht forward als Wert
;;; enthalten zu der Auswertung der values-Komponente evaluiert.
;;;
;;; Revision 1.17  1992/08/11  16:25:49  hk
;;; Declare Ignore.
;;;
;;; Revision 1.16  1992/08/10  11:01:10  hk
;;; Labels-Form herausgezogen fuer CMU-Lisp.
;;;
;;; Revision 1.15  1992/08/06  13:05:32  hk
;;; Schreibfehler.
;;;
;;; Revision 1.14  1992/08/06  13:00:17  hk
;;; Eval fuer tagbody und go geschrieben, fuer let/cc und cont korrigiert.
;;;
;;; Revision 1.13  1992/07/31  14:10:00  hk
;;; Interpretation von Funktionen umgestellt, jetzt auch lokal-funs.
;;;
;;; Revision 1.12  1992/07/28  11:38:30  hk
;;; p1-eval benutzt nun in-compile-time-env, Konstante listen werden beim
;;; evaluieren nicht mehr destruktiv veraendert, Schreibfehler beseitigt.
;;;
;;; Revision 1.11  1992/07/23  08:42:13  hk
;;; var-ref --> location.
;;;
;;; Revision 1.10  1992/07/09  15:06:35  hk
;;; Keine Warnung, wenn in p1-eval mit symbol-function aufruft.
;;;
;;; Revision 1.9  1992/07/02  13:20:51  hk
;;; Fehlermeldung bei (error) in zw-eval (structure-literal) eingefuegt.
;;;
;;; Revision 1.8  1992/06/11  08:46:03  hk
;;; Warning, wenn in zw-eval eine Fkt. des Wirts-Lisps aufgerufen wird.
;;;
;;; Revision 1.7  1992/06/10  16:25:31  hk
;;; Schreibfehler.
;;;
;;; Revision 1.6  1992/06/10  16:22:32  hk
;;; Wert eines Symbols ist ggf. konstanter Wert des Symbols.
;;;
;;; Revision 1.5  1992/06/10  16:11:16  hk
;;; Fehler in zw-eval (progn-form), Resultat des letzten Ausdrucks verwenden.
;;;
;;; Revision 1.4  1992/06/05  12:41:23  hk
;;; Continuations eingebaut; in app werden fun nicht evaluiert.
;;;
;;; Revision 1.3  1992/06/05  11:56:08  hk
;;; Abbruchbedingung in zw-eval (let*-form) eingefuegt.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial RCS-revision
;;;
;;; Revision 1.0 24.07.91 
;;; In CLICC-GET-BQ-FUNCTIONS wird MACROEXPAND benutzt, denn evtl. enthalten 
;;; die aus Backquote-Ausdruecken erzeugten internen Ausdruecke Makros.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(defun p1-eval (form)
  (catch 'cant-eval
    (let ((v (zw-eval (in-compile-time-env (p1-form form)) (zw-empty-env))))
      (labels ((chk-no-fun (v)
                 (typecase v
                   (symbol nil)         ; CLtL1 (!) symbol < function
                   (cons (chk-no-fun (car v)) (chk-no-fun (cdr v)))
                   (function (unable-to-eval v))
                   #+(OR CLISP LUCID) (string nil)        
                   ((array standard-char) nil)
                   (array (map-array #'chk-no-fun v))
                   (t nil))))
        (chk-no-fun v)
        (values v t)))))

;;------------------------------------------------------------------------------
(defun unable-to-eval (form)
  (throw 'cant-eval
    (values form NIL)))

;;------------------------------------------------------------------------------
;; Funktion, die ein leeres Environment generiert.
;;------------------------------------------------------------------------------
(defun zw-empty-env () ())

;;------------------------------------------------------------------------------
;; erweitert das aktuelle Environment um eine Bindung fuer var mit dem initialen
;; Wert value.
;;------------------------------------------------------------------------------
(defmacro zw-bind (var value env)
  `(push (cons ,var ,value) ,env))

;;------------------------------------------------------------------------------
;; Setzt den Wert von var in env destruktiv auf value.
;;------------------------------------------------------------------------------
(defmacro zw-setq (var value env)
  `(setf (cdr (assoc ,var ,env)) ,value))

;;------------------------------------------------------------------------------
;; Bestimmt den aktuellen Wert von var in env.
;;------------------------------------------------------------------------------
(defmacro zw-get-bind (var env)
  `(cdr (assoc ,var ,env)))


;;------------------------------------------------------------------------------
;; Fehlerfall
;;------------------------------------------------------------------------------
(defmethod zw-eval ((anything t) env)
  (declare (ignore env))
  #+PCL (when (functionp anything)
          (return-from zw-eval anything))
  (error "can't eval ~A" anything))

;;------------------------------------------------------------------------------
;; Auswertung von Literalen
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Die als strukturierte Literale repraesentierten Listen enthalten mit Ausnahme
;; von Symbolen und Strukturen Literale in ihrer Lisp-Repraesentation.
;; Der Interpretierer ruft in diesem Fall eval-structure-literal-list auf.
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form structured-literal) env)
  (let ((value (?value form)))
    (etypecase value
      ((or list array) (eval-structured-literal-value value env)))))

;;------------------------------------------------------------------------------
;; Die in der Liste enthaltenen Symbole werden ausgewertet, alle anderen
;; Literale bleiben erhalten.
;;------------------------------------------------------------------------------
(defun eval-structured-literal-value (x &optional env)
  (labels
      ((f (x)
         (typecase x
           (cons (cons (f (car x)) (f (cdr x))))
           (string x)
           (array (copy-array #'f x))
           (sym  (zw-eval x env))
           (literal-instance (zw-eval x env))
           (t x))))
    (f x)))

;;------------------------------------------------------------------------------
(defmethod zw-eval ((a-simple-literal simple-literal) env)
  (declare (ignore env))
  (?value a-simple-literal))

(defmethod zw-eval ((a-null-form null-form) env)
  (declare (ignore env))
  '())

;;------------------------------------------------------------------------------
;; literal-instance
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form literal-instance) env)
  (declare (ignore env))
  (unable-to-eval form))

;;------------------------------------------------------------------------------
;; Symbole
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form sym) env)
  (declare (ignore env))
  (?symbol form))

;;------------------------------------------------------------------------------
;; Klassen
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form class-def) env)
  (declare (ignore env))
  form)

;;------------------------------------------------------------------------------
;; Auswertung eines lesenden Zugriffs auf eine Variable
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form var-ref) env)
  (setq form (?var form))
  (cond
    ((local-static-p form)
     (zw-get-bind form env))
    (t (setq form (?sym form))
       (cond
         ((constant-value-p form) (zw-eval (?constant-value form) env))
         (t (unable-to-eval form))))))

;;------------------------------------------------------------------------------
;; Auswertung einer benannten Konstante
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form defined-named-const) env)
  (if (member (?value form) '(:unknown :forward))
      (unable-to-eval form)
      (zw-eval (?value form) env)))

(defmethod zw-eval ((form imported-named-const) env)
  (declare (ignore env))
  (unable-to-eval form))

;;------------------------------------------------------------------------------
;; Zuweisung an eine Variable
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form setq-form) env)
  (let ((var (?var (?location form))))
    (cond
      ((local-static-p var)
        (zw-setq var (zw-eval (?form form) env) env))
      (t (unable-to-eval form)))))

;;------------------------------------------------------------------------------
;; Hintereinanderausfuehrung
;;------------------------------------------------------------------------------
(defmethod zw-eval ((a-progn-form progn-form) env)
  (let ((evaluator #'(lambda (form)
                       (zw-eval form env))))
    (mapc-progn-form-list (?form-list a-progn-form) evaluator evaluator)))

;;------------------------------------------------------------------------------
;; Konditional
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form if-form) env)
  (if (zw-not-null-p (zw-eval (?pred form) env))
      (zw-eval (?then form) env)
      (zw-eval (?else form) env)))

;;------------------------------------------------------------------------------
;; Hinzufuegen neuer Variablenbindungen
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form let*-form) env)
  (mapc #'(lambda (variable init-form)
            (if (local-static-p variable)
                (zw-bind variable (zw-eval init-form env) env)
                (unable-to-eval form)))
        (?var-list form)
        (?init-list form))
  (zw-eval (?body form) env))

;;------------------------------------------------------------------------------
;; Multiple Values
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form mv-lambda) env)
  (multiple-value-call
      (zw-eval (make-defined-fun :symbol 'mv-lambda
                                 :params (?params form)
                                 :body (?body form))
               env)
    (zw-eval (?arg form) env)))
  
;;------------------------------------------------------------------------------
;; Tagbody und Go
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form tagbody-form) env)
  (let ((secret (gensym))
        (next-tagged-forms (?tagged-form-list form))
        target)
    (zw-bind form secret env)
    (setq target (catch secret (zw-eval (?first-form form) env)))
    (loop
     (if (tagged-form-p target)
         (setq next-tagged-forms (member target (?tagged-form-list form)))
         (unless next-tagged-forms (return)))
     (setq target
           (catch secret (zw-eval (?form (pop next-tagged-forms)) env))))))

;;------------------------------------------------------------------------------
(defmethod zw-eval ((form tagged-form) env)
  (throw (zw-get-bind (?tagbody form) env) form))

;------------------------------------------------------------------------------
;; Continuations
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form let/cc-form) env)
  (let ((secret (gensym)))
    (zw-bind (?cont form) #'(lambda (x) (throw secret x)) env)
    (catch secret
      (zw-eval (?body form) env))))

;;------------------------------------------------------------------------------
(defmethod zw-eval ((cont cont) env)
  (zw-get-bind cont env))

;;------------------------------------------------------------------------------
;; Funktionales Objekt
;; Globale und importierte Funktionen greifen nicht auf env zu
;;------------------------------------------------------------------------------
(defmethod zw-eval ((fun fun) env)
  #'(lambda (&rest arg-list)
      (zw-apply fun arg-list env)))

;;------------------------------------------------------------------------------
;; Funktion des Wirts Lispsystems.
;; Kommt nur im Rumpf von Makroexpansionsfunktionen vor und nur fuer Lisp
;; Systemfunktionen.
;;------------------------------------------------------------------------------
#-PCL                        ; siehe (zw-eval T T)
(defmethod zw-eval ((function function) env)
  (declare (ignore env))
  function)

;;------------------------------------------------------------------------------
;; Funktionsanwendung
;;------------------------------------------------------------------------------
(defmethod zw-eval ((app app) env)
  (apply (zw-eval (?form app) env)
         (mapcar #'(lambda (arg) (zw-eval arg env)) (?arg-list app))))

;;------------------------------------------------------------------------------
;; Definition lokaler Funktionen
;; Die Definitionen brauchen nicht betrachtet zu werden, da die angewandten
;; Vorkommen alle relevanten Informationen enthalten.
;;------------------------------------------------------------------------------
(defmethod zw-eval ((form labels-form) env)
  (zw-eval (?body form) env))

;;------------------------------------------------------------------------------
(defmethod zw-apply ((fun defined-fun) arg-list env)
  (let ((params (?params fun)))
    (dolist (var (?var-list params))
      (zw-bind var (pop arg-list) env))
    (dolist (opt (?opt-list params))
      (zw-bind (?var opt)
               (cond
                 (arg-list
                  (when (?suppl opt) (zw-bind (?suppl opt) (zw-t) env))
                  (pop arg-list))
                 (t (when (?suppl opt) (zw-bind (?suppl opt) nil env))
                    (zw-eval (?init opt) env)))
               env))
    (when (?rest params)
      (zw-bind (?rest params) arg-list env))
    (when (?key-list params)
      (unable-to-eval (?symbol fun))))
  (zw-eval (?body fun) env))

;;------------------------------------------------------------------------------
(defmethod zw-apply ((fun imported-fun) arg-list env)
  (let ((app-fun (zw-symbol-fun (?symbol fun))))
    (cond
      (app-fun (apply app-fun arg-list))
      ((slot-boundp fun 'syntax) (zw-apply (?syntax fun) arg-list env))
      (T (unable-to-eval (?symbol fun))))))

;;------------------------------------------------------------------------------
(defun zw-t ()
  (get-symbol-bind t))
  
;;------------------------------------------------------------------------------
(defun zw-not-null-p (x) x)

;;------------------------------------------------------------------------------
(defun zw-error (&rest args) (apply #'clicc-error args))

;;------------------------------------------------------------------------------
;; Unser eigenes symbol-function
;;------------------------------------------------------------------------------
(defun zw-symbol-fun (fun-sym)
  (gethash fun-sym *zw-sym-fun-hash-table*))

;;------------------------------------------------------------------------------
;; Initialisierung der Hash-Table fuer zw-symbol-fun
;;------------------------------------------------------------------------------
(defun init-zw-sym-fun-hash-table ()
  (setq *zw-sym-fun-hash-table* (make-hash-table :test #'equal :size 377))
  (macrolet
    #-CLISP
    ((sym-fun (name fun)
              `(setf (gethash ',name *zw-sym-fun-hash-table*) #',fun)))
    #+CLISP
    ((sym-fun (name fun)
              `(let (dummy-to-avoid-error)
                (setq dummy-to-avoid-error #',fun)
                (setf (gethash ',name *zw-sym-fun-hash-table*)
                 dummy-to-avoid-error))))
    
    ;; Predicates
    ;; typep und subtypep des Wirts-Lispsystems nicht benutzen
    (sym-fun L::null null)
    (sym-fun L::symbolp symbolp)
    (sym-fun L::atom atom)
    (sym-fun L::consp consp)
    (sym-fun L::listp listp)
    (sym-fun L::numberp numberp)
    (sym-fun L::integerp integerp)
    (sym-fun L::rationalp rationalp)
    (sym-fun L::floatp floatp)
    ;; .. real, complex
    (sym-fun L::characterp characterp)
    (sym-fun L::stringp stringp)
    ;; bit-vector-p, vectorp, simple-vector-p
    (sym-fun L::simple-string-p simple-string-p)
    ;; simple-bit-vector-p, arrayp, packagep, functionp, compiled-function-p
    ;; common
    (sym-fun L::eq eq)
    (sym-fun L::eql eql)
    (sym-fun L::equal equal)
    (sym-fun L::not not)

    ;; Control Structure
    ;; kein symbol-value, symbol-function, boundp, fboundp, special-form-p, set
    ;; kein makunbound, fmakunbound
    ;; kein get-setf-method, get-setf-method-multiple-value
    (sym-fun
     L::apply
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'apply fn args)
           (error "apply: first argument ~a must be a function" fn))))
    (sym-fun
     L::funcall
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply fn args)
           (error "funcall: first argument ~a must be a function" fn))))
    (sym-fun
     L::mapcar
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'mapcar fn args)
           (error "mapcar: first argument ~a must be a function" fn))))
    (sym-fun
     L::maplist
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'maplist fn args)
           (error "maplist: first argument ~a must be a function" fn))))
    (sym-fun
     L::mapc
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'mapc fn args)
           (error "mapc: first argument ~a must be a function" fn))))
    (sym-fun
     L::mapl
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'mapl fn args)
           (error "mapl: first argument ~a must be a function" fn))))
    (sym-fun
     L::mapcan
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'mapcan fn args)
           (error "mapcan: first argument ~a must be a function" fn))))
    (sym-fun
     L::mapcon
     (lambda (fn &rest args)
       (if (functionp fn)
           (apply #'mapcon fn args)
           (error "mapcon: first argument ~a must be a function" fn))))
    (sym-fun L::values values)

    ;; Symbols
    ;; keine P-Listen von Symbolen bearbeiten !
    (sym-fun L::getf getf)
    (sym-fun
     rt::set-prop
     (lambda (plist indicator value)
       (labels ((get-prop (list indicator)
                  (cond
                    ((atom list) nil) 
                    ((eq (car list) indicator) list)
                    (t (get-prop (cddr list) indicator)))))
         (let ((list (get-prop plist indicator)))
           (cond
             (list
              (rplaca (cdr list) value)
              plist)
             (t (cons indicator (cons value plist))))))))
    
    (sym-fun
     rt::remf-internal
     (lambda (list indicator)
       (cond
         ((atom list) (values nil nil))
         ((eq (car list) indicator) (values (cddr list) t))
         (t (do ((list1 list (cddr list1))
                 (list2 (cddr list) (cddr list2)))
                ((atom list2) (values nil nil)) ;end test
              (when (eq (car list2) indicator)
                (rplacd (cdr list1) (cddr list2))
                (return (values list t))))))))

    (sym-fun L::get-properties get-properties)
    (sym-fun L::symbol-name symbol-name)
    (sym-fun L::make-symbol make-symbol)
    (sym-fun L::copy-symbol copy-symbol)
    (sym-fun L::gensym gensym)
    (sym-fun L::gentemp gentemp)
    (sym-fun L::symbol-package symbol-package)
    (sym-fun L::keywordp keywordp)

    ;; Packages
    (sym-fun L::intern intern)
    (sym-fun L::find-package find-package)

    ;; Numbers
    (sym-fun L::zerop zerop)
    (sym-fun L::plusp plusp)
    (sym-fun L::minusp minusp)
    (sym-fun L::oddp oddp)
    (sym-fun L::evenp evenp)
    (sym-fun L::= =)
    (sym-fun L::/= /=)
    (sym-fun L::< <)
    (sym-fun L::> >)
    (sym-fun L::<= <=)
    (sym-fun L::>= >=)
    (sym-fun L::max max)
    (sym-fun L::min min)
    (sym-fun L::+ +)
    (sym-fun L::- -)
    (sym-fun L::* *)
    (sym-fun L::/ /)
    (sym-fun L::1+ 1+)
    (sym-fun L::1- 1-)
    (sym-fun L::exp exp)
    (sym-fun L::expt expt)
    (sym-fun L::log log)
    (sym-fun L::sqrt sqrt)
    (sym-fun L::isqrt isqrt)
    (sym-fun L::abs abs)
    (sym-fun L::signum signum)
    (sym-fun L::float float)
    (sym-fun L::floor floor)
    (sym-fun L::ceiling ceiling)
    (sym-fun L::truncate truncate)
    (sym-fun L::round round)
    (sym-fun L::mod mod)
    (sym-fun L::rem rem)
    (sym-fun L::logior logior)
    (sym-fun L::logxor logxor)
    (sym-fun L::logand logand)
    (sym-fun L::logeqv logeqv)
    (sym-fun L::lognand lognand)
    (sym-fun L::lognor lognor)
    (sym-fun L::logandc1 logandc1)
    (sym-fun L::logandc2 logandc2)
    (sym-fun L::logorc1 logorc1)
    (sym-fun L::logorc2 logorc2)
    (sym-fun L::ash ash)

    ;; Characters
    (sym-fun L::standard-char-p standard-char-p)
    (sym-fun L::graphic-char-p graphic-char-p)
    (sym-fun L::alpha-char-p alpha-char-p)
    (sym-fun L::upper-case-p upper-case-p)
    (sym-fun L::lower-case-p lower-case-p)
    (sym-fun L::both-case-p both-case-p)
    (sym-fun L::digit-char-p digit-char-p)
    (sym-fun L::alphanumericp alphanumericp)
    (sym-fun L::char= char=)
    (sym-fun L::char\= char\=)
    (sym-fun L::char< char<)
    (sym-fun L::char> char>)
    (sym-fun L::char<= char<=)
    (sym-fun L::char>= char>=)
    (sym-fun L::char-equal char-equal)
    (sym-fun L::char-not-equal char-not-equal)
    (sym-fun L::char-lessp char-lessp)
    (sym-fun L::char-greaterp char-greaterp)
    (sym-fun L::char-not-greaterp char-not-greaterp)
    (sym-fun L::char-not-lessp char-not-lessp)
    (sym-fun L::char-code char-code)
    (sym-fun L::code-char code-char)
    (sym-fun L::character character)
    (sym-fun L::char-upcase char-upcase)
    (sym-fun L::char-downcase char-downcase)
    (sym-fun L::digit-char digit-char)
    (sym-fun L::char-name char-name)
    (sym-fun L::name-char name-char)
    
    ;; Sequences
    (sym-fun L::elt elt)
    (sym-fun (L::setf L::elt) (lambda (v s i)
                                (setf (elt s i) v)))
    (sym-fun L::subseq subseq)
    (sym-fun L::copy-seq copy-seq)
    (sym-fun L::length length)
    (sym-fun L::reverse reverse)
    (sym-fun L::nreverse nreverse)
    (sym-fun L::make-sequence make-sequence)
    (sym-fun L::concatenate concatenate)
    (sym-fun L::map map)
    (sym-fun L::some some)
    (sym-fun L::every every)
    (sym-fun L::notany notany)
    (sym-fun L::notevery notevery)
    (sym-fun L::reduce reduce)
    (sym-fun L::fill fill)
    (sym-fun L::replace replace)
    (sym-fun L::remove remove)
    (sym-fun L::remove-if remove-if)
    (sym-fun L::remove-if-not remove-if-not)
    (sym-fun L::delete delete)
    (sym-fun L::delete-if delete-if)
    (sym-fun L::delete-if-not delete-if-not)
    (sym-fun L::remove-duplicates remove-duplicates)
    (sym-fun L::delete-duplicates delete-duplicates)
    (sym-fun L::substitute substitute)
    (sym-fun L::substitute-if substitute-if)
    (sym-fun L::substitute-if-not substitute-if-not)
    (sym-fun L::nsubstitute nsubstitute)
    (sym-fun L::nsubstitute-if nsubstitute-if)
    (sym-fun L::nsubstitute-if-not nsubstitute-if-not)
    (sym-fun L::find find)
    (sym-fun L::find-if find-if)
    (sym-fun L::find-if-not find-if-not)
    (sym-fun L::position position)
    (sym-fun L::position-if position-if)
    (sym-fun L::position-if-not position-if-not)
    (sym-fun L::count count)
    (sym-fun L::count-if count-if)
    (sym-fun L::count-if-not count-if-not)
    (sym-fun L::mismatch mismatch)
    (sym-fun L::search search)
    (sym-fun L::sort sort)
    (sym-fun L::stable-sort stable-sort)
    (sym-fun L::merge merge)

    ;; Lists
    (sym-fun L::car car)
    (sym-fun (L::setf L::car) (lambda (v c)
                                (setf (car c) v)))
    (sym-fun L::cdr cdr)
    (sym-fun (L::setf L::cdr) (lambda (v c)
                                (setf (cdr c) v)))
    (sym-fun L::caar caar)
    (sym-fun (L::setf L::caar) (lambda (v c)
                                 (setf (caar c) v)))
    (sym-fun L::cadr cadr)
    (sym-fun (L::setf L::cadr) (lambda (v c)
                                 (setf (cadr c) v)))
    (sym-fun L::cdar cdar)
    (sym-fun (L::setf L::cdar) (lambda (v c)
                                 (setf (cdar c) v)))
    (sym-fun L::cddr cddr)
    (sym-fun (L::setf L::cddr) (lambda (v c)
                                 (setf (cddr c) v)))
    (sym-fun L::caaar caaar)
    (sym-fun (L::setf L::caaar) (lambda (v c)
                                  (setf (caaar c) v)))
    (sym-fun L::caadr caadr)
    (sym-fun (L::setf L::caadr) (lambda (v c)
                                  (setf (caadr c) v)))
    (sym-fun L::cadar cadar)
    (sym-fun (L::setf L::cadar) (lambda (v c)
                                  (setf (cadar c) v)))
    (sym-fun L::caddr caddr)
    (sym-fun (L::setf L::caddr) (lambda (v c)
                                  (setf (caddr c) v)))
    (sym-fun L::cadar cadar)
    (sym-fun (L::setf L::cadar) (lambda (v c)
                                  (setf (cadar c) v)))
    (sym-fun L::caddr caddr)
    (sym-fun (L::setf L::caddr) (lambda (v c)
                                  (setf (caddr c) v)))
    (sym-fun L::cdaar cdaar)
    (sym-fun (L::setf L::cdaar) (lambda (v c)
                                  (setf (cdaar c) v)))
    (sym-fun L::cdadr cdadr)
    (sym-fun (L::setf L::cdadr) (lambda (v c)
                                  (setf (cdadr c) v)))
    (sym-fun L::cddar cddar)
    (sym-fun (L::setf L::cddar) (lambda (v c)
                                  (setf (cddar c) v)))
    (sym-fun L::cdddr cdddr)
    (sym-fun (L::setf L::cdddr) (lambda (v c)
                                  (setf (cdddr c) v)))
    (sym-fun L::caaaar caaaar)
    (sym-fun (L::setf L::caaaar) (lambda (v c)
                                   (setf (caaaar c) v)))
    (sym-fun L::caaadr caaadr)
    (sym-fun (L::setf L::caaadr) (lambda (v c)
                                   (setf (caaadr c) v)))
    (sym-fun L::caadar caadar)
    (sym-fun (L::setf L::caadar) (lambda (v c)
                                   (setf (caadar c) v)))
    (sym-fun L::caaddr caaddr)
    (sym-fun (L::setf L::caaddr) (lambda (v c)
                                   (setf (caaddr c) v)))
    (sym-fun L::cadaar cadaar)
    (sym-fun (L::setf L::cadaar) (lambda (v c)
                                   (setf (cadaar c) v)))
    (sym-fun L::cadadr cadadr)
    (sym-fun (L::setf L::cadadr) (lambda (v c)
                                   (setf (cadadr c) v)))
    (sym-fun L::caddar caddar)
    (sym-fun (L::setf L::caddar) (lambda (v c)
                                   (setf (caddar c) v)))
    (sym-fun L::cadddr cadddr)
    (sym-fun (L::setf L::cadddr) (lambda (v c)
                                   (setf (cadddr c) v)))
    (sym-fun L::cdaaar cdaaar)
    (sym-fun (L::setf L::cdaaar) (lambda (v c)
                                   (setf (cdaaar c) v)))
    (sym-fun L::cdaadr cdaadr)
    (sym-fun (L::setf L::cdaadr) (lambda (v c)
                                   (setf (cdaadr c) v)))
    (sym-fun L::cdadar cdadar)
    (sym-fun (L::setf L::cdadar) (lambda (v c)
                                   (setf (cdadar c) v)))
    (sym-fun L::cdaddr cdaddr)
    (sym-fun (L::setf L::cdaddr) (lambda (v c)
                                   (setf (cdaddr c) v)))
    (sym-fun L::cddaar cddaar)
    (sym-fun (L::setf L::cddaar) (lambda (v c)
                                   (setf (cddaar c) v)))
    (sym-fun L::cddadr cddadr)
    (sym-fun (L::setf L::cddadr) (lambda (v c)
                                   (setf (cddadr c) v)))
    (sym-fun L::cdddar cdddar)
    (sym-fun (L::setf L::cdddar) (lambda (v c)
                                   (setf (cdddar c) v)))
    (sym-fun L::cddddr cddddr)
    (sym-fun (L::setf L::cddddr) (lambda (v c)
                                   (setf (cddddr c) v)))
    (sym-fun L::cons cons)
    (sym-fun L::tree-equal tree-equal)
    (sym-fun L::endp endp)
    (sym-fun L::list-length list-length)
    (sym-fun L::nth nth)
    (sym-fun (L::setf L::nth) (lambda (v l i)
                                (setf (nth l i) v)))
    (sym-fun L::first first)
    (sym-fun (L::setf L::first) (lambda (v c)
                                  (setf (first c) v)))
    (sym-fun L::second second)
    (sym-fun (L::setf L::second) (lambda (v c)
                                   (setf (second c) v)))
    (sym-fun L::third third)
    (sym-fun (L::setf L::third) (lambda (v c)
                                  (setf (third c) v)))
    (sym-fun L::fourth fourth)
    (sym-fun (L::setf L::fourth) (lambda (v c)
                                   (setf (fourth c) v)))
    (sym-fun L::fifth fifth)
    (sym-fun (L::setf L::fifth) (lambda (v c)
                                  (setf (fifth c) v)))
    (sym-fun L::sixth sixth)
    (sym-fun (L::setf L::sixth) (lambda (v c)
                                  (setf (sixth c) v)))
    (sym-fun L::seventh seventh)
    (sym-fun (L::setf L::seventh) (lambda (v c)
                                    (setf (seventh c) v)))
    (sym-fun L::eighth eighth)
    (sym-fun (L::setf L::eighth) (lambda (v c)
                                   (setf (eighth c) v)))
    (sym-fun L::ninth ninth)
    (sym-fun (L::setf L::ninth) (lambda (v c)
                                  (setf (ninth c) v)))
    (sym-fun L::tenth tenth)
    (sym-fun (L::setf L::tenth) (lambda (v c)
                                  (setf (tenth c) v)))
    (sym-fun L::rest rest)
    (sym-fun (L::setf L::rest) (lambda (v c)
                                 (setf (rest c) v)))
    (sym-fun L::nthcdr nthcdr)
    (sym-fun L::last last)
    (sym-fun L::list list)
    (sym-fun L::list* list*)
    (sym-fun L::make-list make-list)
    (sym-fun L::append append)
    (sym-fun L::copy-list copy-list)
    (sym-fun L::copy-alist copy-alist)
    (sym-fun L::copy-tree copy-tree)
    (sym-fun L::revappend revappend)
    (sym-fun L::nconc nconc)
    (sym-fun L::nreconc nreconc)
    (sym-fun L::butlast butlast)
    (sym-fun L::nbutlast nbutlast)
    (sym-fun L::ldiff ldiff)
    (sym-fun L::rplaca rplaca)
    (sym-fun L::rplacd rplacd)
    (sym-fun L::subst subst)
    (sym-fun L::subst-if subst-if)
    (sym-fun L::subst-if-not subst-if-not)
    (sym-fun L::nsubst nsubst)
    (sym-fun L::nsubst-if nsubst-if)
    (sym-fun L::nsubst-if-not nsubst-if-not)
    (sym-fun L::sublis sublis)
    (sym-fun L::nsublis nsublis)
    (sym-fun L::member member)
    (sym-fun L::member-if member-if)
    (sym-fun L::member-if-not member-if-not)
    (sym-fun L::tailp tailp)
    (sym-fun L::adjoin adjoin)
    (sym-fun L::union union)
    (sym-fun L::nunion nunion)
    (sym-fun L::intersection intersection)
    (sym-fun L::set-difference set-difference)
    (sym-fun L::subsetp subsetp)
    (sym-fun L::acons acons)
    (sym-fun L::pairlis pairlis)
    (sym-fun L::assoc assoc)
    (sym-fun L::assoc-if assoc-if)
    (sym-fun L::assoc-if-not assoc-if-not)
    (sym-fun L::rassoc rassoc)
    (sym-fun L::rassoc-if rassoc-if)
    (sym-fun L::rassoc-if-not rassoc-if-not)

    ;; Arrays
    ;; ..
    (sym-fun L::aref aref)
    (sym-fun (L::setf L::aref) (lambda (v &rest l)
                                 (setf (apply #'aref l) v)))
    (sym-fun L::svref svref)

;;; is not required to be supported, in ANSI-CL use #'(setf svref) instead    
;;;    (sym-fun (L::setf L::svref) (lambda (v l)
;;;                                  (setf (apply #'svref l) v)))
    ;; ..
    
    ;; Strings
    (sym-fun L::char char)
;;;    (sym-fun (L::setf L::char) (lambda (v l)
;;;                                 (setf (apply #'char l) v)))
    (sym-fun L::schar schar)
;;;    (sym-fun (L::setf L::schar) (lambda (v l)
;;;                                  (setf (apply #'schar l) v)))
    (sym-fun L::string= string=)
    (sym-fun L::string-equal string-equal)
    (sym-fun L::string< string<)
    (sym-fun L::string> string>)
    (sym-fun L::string<= string<=)
    (sym-fun L::string>= string>=)
    (sym-fun L::string/= string/=)
    (sym-fun L::string-lessp string-lessp)
    (sym-fun L::string-greaterp string-greaterp)
    (sym-fun L::string-not-greaterp string-not-greaterp)
    (sym-fun L::string-not-lessp string-not-lessp)
    (sym-fun L::string-not-equal string-not-equal)
    (sym-fun L::make-string make-string)
    (sym-fun L::string-trim string-trim)
    (sym-fun L::string-left-trim string-left-trim)
    (sym-fun L::string-right-trim string-right-trim)
    (sym-fun L::string-upcase string-upcase)
    (sym-fun L::string-downcase string-downcase)
    (sym-fun L::string-capitalize string-capitalize)
    (sym-fun L::nstring-upcase nstring-upcase)
    (sym-fun L::nstring-downcase nstring-downcase)
    (sym-fun L::nstring-capitalize nstring-capitalize)
    (sym-fun L::string string)

    ;; I/O
    ;; ..
    (sym-fun L::write-to-string write-to-string)
    (sym-fun L::prin1-to-string prin1-to-string)
    (sym-fun L::princ-to-string princ-to-string)
    ;; ..
    (sym-fun L::format format)

    ;; Errors
    (sym-fun L::error error)
    (sym-fun L::warn warn)
    (sym-fun L::break break)
    ;; ..
    ))

;;------------------------------------------------------------------------------
(provide "p1eval")
