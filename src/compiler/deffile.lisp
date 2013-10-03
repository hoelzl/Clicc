;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Bearbeiten der Interface-Beschreibungen fuer Modulkompilation
;;;            (.def Dateien)
;;;            - exportierte Funktionen
;;;            - exportierte Symbole
;;;
;;; $Revision: 1.42 $
;;; $Log: deffile.lisp,v $
;;; Revision 1.42  1994/06/13  13:52:57  hk
;;; export-body darf nur aufgerufen werden, wenn *optimize* nicht nil ist
;;;
;;; Revision 1.41  1994/06/07  14:46:45  jh
;;; *max-export-weight* eingefuehrt. Symbole lokaler Variablen werden
;;; nicht rausgeschrieben. prepare-export-write nach clcmain.lisp verlegt.
;;;
;;; Revision 1.40  1994/05/05  15:15:46  hk
;;; Raw-Slots deaktiviert, damit sich CLiCC wieder selbst "ubersetzten
;;; kann.
;;;
;;; Revision 1.39  1994/05/05  14:39:04  hk
;;; - Fehler behoben in export-cont-def: level wird geschrieben, read und
;;;   adr wurden falsch geschrieben, obwohl sie gar nicht ben"otigt wurden.
;;; - find-class auf Symbole angewendet, da apply #'make-instance nicht mit
;;;   Symbolen umgehen kann.
;;; - Funktionsr"umpfe werden nur in die .def Datei eingetragen, wenn
;;;   Inlining angeschaltet ist.
;;;
;;; Revision 1.38  1994/04/05  15:17:56  jh
;;; Export von Funktionsruempfen und benannten Konstanten eingebaut.
;;;
;;; Revision 1.37  1994/02/10  09:54:45  sma
;;; my-last-arg-may-be-rest-var wird jetzt auch exportiert.
;;;
;;; Revision 1.36  1994/02/08  13:30:09  hk
;;; Aufrufe von get-global-fun durch name2fun ersetzt.
;;;
;;; Revision 1.35  1994/02/08  13:15:06  sma
;;; Annotation my-last-arg-may-be-rest-var bei funs, die angibt, daﬂ das
;;; letzte Argument der hiermit annotierten Funktion auch ein rest-listen
;;; Parameter sein kann. Die Annotation enth‰lt in diesem Falle den Namen
;;; (als Keyword) ihrer Funktion.
;;;
;;; Revision 1.34  1994/02/03  07:53:55  hk
;;; Fehler beim Einlesen von :has-funs-as-args behoben.
;;;
;;; Revision 1.33  1994/02/02  09:17:35  hk
;;; import- und export-fun lesen und schreiben zus‰tzliche Annotationen.
;;;
;;; Revision 1.32  1993/12/22  09:22:14  hk
;;; F¸r CMU17 m¸ssen bei make-instance Symbole statt Klassen verwendet
;;; werden.
;;;
;;; Revision 1.31  1993/12/19  14:08:46  hk
;;; In import-fun stellt nun sicher, daﬂ wirklich alle :mv-spec :t zu
;;; :mv-spec T werden.
;;;
;;; Revision 1.30  1993/12/16  09:36:53  hk
;;; In das .def File wird nicht das Symbol T sondern das Keyword :T
;;; geschrieben, um Probleme mit packages zu vermeiden. Beim Einlesen wird
;;; wieder zu T ¸bergegangen.
;;;
;;; Revision 1.29  1993/12/09  14:36:42  hk
;;; Beim Lesen eines .def Files mittels import-read wird sichergestellt,
;;; daﬂ das verwendete Package bei Bedarf mit make-package generiert wird.
;;;
;;; Revision 1.28  1993/12/03  09:59:37  ft
;;; Aufrufe von make-instance in import-fun und import-sym optimiert.
;;;
;;; Revision 1.27  1993/08/19  15:22:11  hk
;;; get-symbol-bind statt eines expliziten find in *GLOBAL-ENVIRONMENT*
;;; verwendet.
;;;
;;; Revision 1.26  1993/08/19  10:34:16  hk
;;; Auch Funktionen mit Namen (setf xxx) kˆnne special-sys-fun sein.
;;;
;;; Revision 1.25  1993/07/27  14:12:14  atr
;;; Import-fun leicht geaendert.
;;;
;;; Revision 1.24  1993/07/20  13:26:49  uho
;;; 'import-read' auf 'imported-module'-Zwischensprachkonstrukt
;;; umgestellt.
;;;
;;; Revision 1.23  1993/07/20  12:09:58  atr
;;; In der Beschreibung einer Funktion stehen unter dem Key
;;; :has-funs-as-args nicht defined-syms sondern die Symbole (lisp symbole).
;;; Beim Importieren werden die defined-syms in der *global-environment* gesucht.
;;;
;;; Revision 1.22  1993/07/19  15:04:22  uho
;;; In .def-Files wird jetzt unter dem Schluesselwort :NAME zusaetzlich
;;; zum Namen des Moduls auch der Namen seiner Initialisierungsfunktion
;;; und die Symbol-base (der Name der Tabelle fuer die Symbole)
;;; abgelegt. 'import-read' liefert diese drei Namen nun als multiplen
;;; Wert und der Modul-Slot 'loaded-modules' enthaelt nun eine Liste
;;; dreielementiger Listen der Form: (Modulname, Initfunktionsname, Symbolbase).
;;;
;;; Revision 1.21  1993/07/19  11:28:26  atr
;;; Slot HAS-FUNS-AS-ARGS steht in der fun-descr einer
;;; exportierten Funktion. Der Slot wird bei der Seiteneffektanalyse
;;; gebraucht.
;;;
;;; Revision 1.20  1993/07/14  17:17:26  atr
;;; In der .def Datei werden die Symbole zunaechst ausgeschrieben,
;;; und dann die Funktionen.
;;; Einlesen der Beschreibungen von Funktionen korrigiert.
;;;
;;; Revision 1.19  1993/06/27  12:14:33  atr
;;; READ und WRITE-LIST bei imported-fun sind jetzt
;;; Listen von Variablen und nicht mehr von Symbolen der Variablen.
;;;
;;; Revision 1.18  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.17  1993/06/12  15:57:01  atr
;;; Tippfehler korrigiert data-effect --> data-effects.
;;;
;;; Revision 1.16  1993/06/09  12:16:56  ft
;;; Fehlende find-class bei make-instance eingef¸gt.
;;;
;;; Revision 1.15  1993/06/07  07:24:25  hk
;;; Schreibfehler.
;;;
;;; Revision 1.14  1993/06/05  21:38:54  hk
;;; Fehler in export-fun behoben.
;;;
;;; Revision 1.13  1993/06/04  14:49:53  hk
;;; :unknown von read- und write-list kann unveraendert geschrieben werden.
;;;
;;; Revision 1.12  1993/06/04  11:10:43  hk
;;; In :read-list und :write-list werden nun Symbole und nicht
;;; Zwischensprachausdruecke angegeben.
;;;
;;; Revision 1.11  1993/06/04  08:32:42  hk
;;; In export-fun: '@ -> ,@ und ' -> ,
;;;
;;; Revision 1.10  1993/05/30  13:59:57  atr
;;; Jetzt werden die Slots read-list , write-list und
;;; data-effects auch ausgeschrieben.
;;;
;;; Revision 1.9  1993/05/14  12:15:59  hk
;;; In .def Files wird ein 2-zeiliger Kommentar geschrieben.
;;;
;;; Revision 1.8  1993/05/14  09:11:00  hk
;;; *package* nicht mit NIL initialisieren.
;;;
;;; Revision 1.7  1993/04/22  12:20:40  hk
;;; export-write beruecksichtigt *OUT-FILENAME*.
;;; Verschiedenartige Eintraege in .def-Files werden durch Keywords
;;; eingeleitet. import-fun beruecksichtigt special-sys-funs.
;;; Bei importierten Symbolen wird Slot base gesetzt.
;;; Neuer Eintrag :sys fuer sys.def: keinen Aufruf von
;;; Initialisierungsfunktion generieren. Neuer Eintrag :package, der
;;; den Namen des package des Moduls angibt.
;;;
;;; Revision 1.6  1993/04/15  16:09:56  hk
;;; Fehler behoben.
;;;
;;; Revision 1.5  1993/04/14  12:20:46  hk
;;; Nur Funktionen mit ?exported = T werden geschrieben.
;;;
;;; Revision 1.4  1993/04/14  10:26:12  hk
;;; import-fun: beruecksichtigt (setf f).
;;;
;;; Revision 1.3  1993/04/08  07:37:39  hk
;;; (provide DEFFILE) -> (provide deffile).
;;;
;;; Revision 1.2  1993/04/03  09:53:08  hk
;;; Alten Dateinamen entfernt.
;;;
;;; Revision 1.1  1993/04/03  09:49:12  hk
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")     

;; Exportierte importierte Funktionen sind in waehrend der Codegenerierung gar
;; nicht zugreifbar. Ihre Spezifikation muesste also nach dem Einlesen in
;; Pass1 bereits wieder herausgeschrieben werden. Die Export-Anweisung erfolgt
;; evtl. erst nach dem Einlesen der Spezifikation, also kann das Schreiben der
;; Spezifikation erst am Ende von Pass1 erfolgen. Dann sind die importierten
;; Funktionen noch im global Environment zugreifbar.  Da (load ..) transitiv
;; ist, werden die exportierten Funktionen des Moduls, das vom aktuellen Modul
;; geladen wird, automatisch bekanntgegeben. Es muss nur durch geeignete
;; Packageoperationen sichergestellt werde, dass diese Funktionen mit beiden
;; Packagenamen qualifiziert werden darf.

;; Das gleiche gilt fuer exportierte importierte Symbole.

;; C-Namen von Funktionen aus verschiedenen Modulen: Wenn in zwei Modulen
;; Funktionen mit gleichem Namen definiert werden, dann ist es schwierig, zwei
;; eindeutige C-namen daraus zu generieren. Als Alternativen bieten sich an:
;; 1) den Packagenamen voranstellen, oder den kuerzesten Nickname, fuer Lisp
;; evtl. kein Praefix, 2) einen eindeutigen Praefix voranstellen, das
;; erfordert jedoch Benutzerinteraktion, da verschiedene Praefixe bei
;; getrennter Modulkompilation nicht automatisch bestimmt werden koennen, 3)
;; generieren des C-namens direkt aus dem Lisp-Namen unter der Annahme, dass
;; keine Namenskonflikte auftreten, diese Annahme kann beim Importieren der
;; Module automatisch geprueft werden.

;;------------------------------------------------------------------------------
;; Berechnet den Namen der .def Datei aus dem Namen eines Moduls 
;;------------------------------------------------------------------------------
(defun calc-def-filename (name)
  (concatenate 'string name ".def"))

;;------------------------------------------------------------------------------
;; Kodiert die Liste der Variablen im Slot has-funs-as-args einer exported-fun.
;; Keyword Parameter werden durch das Keyword Symbol ersetzt , required
;; und optional Parameter werden durch ihre Position in der Parameterliste 
;; ersetzt.
;;------------------------------------------------------------------------------
(defun update-and-encode (function)
  (when (?has-funs-as-args function)
    (let ((code-list nil)
          (all-vars (?all-vars (?params function)))
          (key-var-list (mapcar #'?var (?key-list (?params function)))))
      (dolist (one-spec-var (?has-funs-as-args function))
        (when (and (static-p one-spec-var)
                   (member one-spec-var all-vars
                           :test #'eq)
                   (not (eq (?rest (?params function)) one-spec-var)))
          (if (member one-spec-var key-var-list :test #'eq)
              (push (?symbol (?sym (find one-spec-var (?key-list 
                                              (?params function))
                                         :key #'?var))) code-list)
              (push (position one-spec-var all-vars :test #'eq) code-list))))
      (setf (?has-funs-as-args function) code-list))))

;;------------------------------------------------------------------------------
;; Beschreibung einer exportierten Funktion in die .def Datei schreiben
;; Keine Symbole auﬂer Keywords schreiben, um Probleme mit Packages zu vermeiden
;;------------------------------------------------------------------------------
(defun export-fun (fun)
  (when (?exported fun)
    (update-and-encode fun)
    (print `(,(?symbol fun) :par-spec ,(?par-spec fun) :adr ,(?adr fun)
             ,@(unless (eql (?mv-spec fun) 1)
                       `(:mv-spec ,(if (eq (?mv-spec fun) 'T)
                                       :T
                                       (?mv-spec fun))))
             ,@(when (?read-list fun) 
                     `(:read-list ,(if (atom (?read-list fun))
                                       (?read-list fun)
                                       (mapcar #'(lambda (x) (?symbol (?sym x)))
                                               (?read-list fun)))))
             ,@(when (?write-list fun)
                     `(:write-list ,(if (atom (?write-list fun))
                                        (?write-list fun)
                                        (mapcar #'(lambda (x)
                                                    (?symbol (?sym x)))
                                                (?write-list fun)))))
             ,@(when (?data-effects fun)
                     `(:data-effects ,(?data-effects fun)))
             ,@(when (?has-funs-as-args fun)
                     `(:has-funs-as-args ,(?has-funs-as-args fun)))
             ,@(when (?simp-when-n-args fun)
                     `(:simp-when-n-args
                       (,(first (?simp-when-n-args fun))
                        ,(?symbol (second (?simp-when-n-args fun))))))
             ,@(when (?simp-when-no-result fun)
                     `(:simp-when-no-result
                       ,(?symbol (?simp-when-no-result fun))))
             ,@(when (?simp-when-arg-n=cons fun)
                     `(:simp-when-arg-n=cons
                       (,(first (?simp-when-arg-n=cons fun))
                        ,(?symbol (second (?simp-when-arg-n=cons fun))))))
             ,@(when (?simp-when-some-arg-not-cons/pathn/string/bitv fun)
                     `(:simp-when-some-arg-not-cons/pathn/string/bitv
                       ,(?symbol
                         (?simp-when-some-arg-not-cons/pathn/string/bitv fun))))
             ,@(when (?simp-when-some-arg-not-num/char fun)
                     `(:simp-when-some-arg-not-num/char
                       ,(?symbol (?simp-when-some-arg-not-num/char fun))))
             ,@(when (?simp-test-fun-when-not-testnot fun)
                     (let ((annotation (?simp-test-fun-when-not-testnot fun)))
                       `(:simp-test-fun-when-not-testnot
                         (,(pop annotation)
                          ,(pop annotation)
                          ,(?symbol (pop annotation))
                          ,(?symbol (pop annotation))
                          ,(?symbol (pop annotation))))))
             ,@(when (?simp-when-only-test=value fun)
                     (let ((annotation (?simp-when-only-test=value fun)))
                     `(:simp-when-only-test=value
                       (,(pop annotation)
                        ,(?symbol (pop annotation))
                        ,(?symbol (pop annotation))
                        ,(?symbol (pop annotation))))))
             ,@(when (?my-last-arg-may-be-rest-var fun)
                     `(:my-last-arg-may-be-rest-var 
                       ,(?my-last-arg-may-be-rest-var fun)))
             ))))

;;------------------------------------------------------------------------------
;; Beschreibung eines exportierten Symbols in die .def Datei schreiben
;;------------------------------------------------------------------------------
(defun export-sym (sym)
  (when (?exported sym)
    (print `(,(?symbol sym) :adr ,(?adr sym)
             ,@(unless (eq :no-const (?constant-value sym))
                 '(:constant-value :unknown))))))

;;------------------------------------------------------------------------------
;; Beschreibung der Parameter und des Rumpfes der Funktionen, die ueber Modul-
;; grenzen inline compiliert werden sollen, in die .def Datei schreiben.
;;------------------------------------------------------------------------------

(defvar *export-id-counter* 0)

(defun next-export-id ()
  (prog1
      *export-id-counter*
    (incf *export-id-counter*)))

(defun set-export-id (obj)
  (setf (?export-id obj) (next-export-id)))

(defvar *max-export-weight* *max-inline-weight*)

(defun export-body (fun)
  (when (and (?exported fun) (<= (export-weight fun) *max-export-weight*))
    (setq *export-id-counter* 0)
    (print `(,(?symbol fun)
             ,(export-params (?params fun))
             ,(export-form (?body fun))
             ,@(when (?local-funs fun)
                 (list (mapcar #'export-form (?local-funs fun))))))))

(defmethod export-var ((a-local-static local-static))
  `(:local-static ,(?export-id a-local-static)))

(defmethod export-var ((a-dynamic dynamic))
  `(:dynamic ,(?symbol (?sym a-dynamic))))

(defmethod export-form ((a-var-ref var-ref))
  `(:var-ref ,(export-var (?var a-var-ref))))

(defmethod export-form ((a-named-const named-const))
  `(:named-const ,(?symbol a-named-const)))

(defmethod export-form ((a-sym sym))
  `(:sym ,(?symbol a-sym)))

(defmethod export-form ((empty-list null-form))
  '(:empty-list))

(defmethod export-form ((a-character-form character-form))
  `(:character-form
    :value ,(?value a-character-form)))

(defmethod export-form ((an-int int))
  `(:int
    :value ,(?value an-int)))

(defmethod export-form ((a-float-form float-form))
  `(:float-form
    :value ,(?value a-float-form)))

;; Structured-literals und literal-instances werden in named-constants verpackt
;; und nicht exportiert.

(defmethod export-form ((a-class-def class-def))
  `(:class-def ,(?symbol a-class-def)))

(defun export-params (params)
  `(,(mapcar #'export-var-def (?var-list params))
    ,(mapcar #'export-opt (?opt-list params))
    ,(when (?rest params) (let ((rest (?rest params)))
                            (append (export-var-def rest)
                                    `(:read ,(?read rest)
                                      :write ,(?write rest)))))
    ,(mapcar #'export-key (?key-list params))
    ,(?allow-other-keys params)))

(defun export-opt (an-opt)
  `(,(export-var-def (?var an-opt))
    ,(export-form (?init an-opt))
    ,(when (?suppl an-opt) (export-var-def (?suppl an-opt)))))

(defun export-key (a-key)
  `(,(export-var-def (?var a-key))
    ,(export-form (?init a-key))
    ,(when (?suppl a-key) (export-var-def (?suppl a-key)))
    ,(export-form (?sym a-key))))

(defmethod export-form ((a-global-fun global-fun))
  `(:global-fun ,(?symbol a-global-fun)))

(defmethod export-form ((a-local-fun local-fun))
  `(:local-fun ,(?export-id a-local-fun)))

(defmethod export-form ((an-imported-fun imported-fun))
  `(:imported-fun ,(?symbol an-imported-fun)))

(defmethod export-form ((an-app app))
  `(:app
    :form ,(export-form (?form an-app))
    :arg-list ,(mapcar #'export-form (?arg-list an-app))
    ,@(when (?downfun-list an-app)
        `(:downfun-list ,(mapcar #'export-form (?downfun-list an-app))))))

(defmethod export-form ((a-setq-form setq-form))
  `(:setq-form
    :location ,(export-form (?location a-setq-form))
    :form ,(export-form (?form a-setq-form))))

(defmethod export-form ((a-progn-form progn-form))
  `(:progn-form
    :form-list ,(mapcar #'export-form (?form-list a-progn-form))))

(defmethod export-form ((an-if-form if-form))
  `(:if-form
    :pred ,(export-form (?pred an-if-form))
    :then ,(export-form (?then an-if-form))
    :else ,(export-form (?else an-if-form))))

(defmethod export-form ((a-switch-form switch-form))
  `(:switch-form
    :form ,(export-form (?form a-switch-form))
    :case-list ,(mapcar #'export-form (?case-list a-switch-form))
    :otherwise ,(export-form (?otherwise a-switch-form))))

(defmethod export-form ((a-labeled-form labeled-form))
  `(:labeled-form
    :value ,(export-form (?value a-labeled-form))
    :form ,(export-form (?form a-labeled-form))))

(defmethod export-var-def ((a-local-static local-static))
  `(:local-static ,(set-export-id a-local-static)
    :level ,(?level a-local-static)))

(defmethod export-var-def ((a-dynamic dynamic))
  (export-var a-dynamic))

(defmethod export-form ((a-let*-form let*-form))
  `(:let*-form ,(mapcar #'export-var-def (?var-list a-let*-form))
    :init-list ,(mapcar #'export-form (?init-list a-let*-form))
    :body ,(export-form (?body a-let*-form))))

(defun export-local-fun-def (a-local-fun)
  `(,(set-export-id a-local-fun)
    ,(?symbol a-local-fun)
    ,(?adr a-local-fun)
    ,(?par-spec a-local-fun)
    ,(export-params (?params a-local-fun))
    ,(export-form (?body a-local-fun))
    :used ,(?used a-local-fun)
    :level ,(?level a-local-fun)
    ,@(when (?local-funs a-local-fun)
        `(:local-funs ,(mapcar #'export-form (?local-funs a-local-fun))))))

(defmethod export-form ((a-labels-form labels-form))
  `(:labels-form ,(mapcar #'export-local-fun-def (?fun-list a-labels-form))
    :body ,(export-form (?body a-labels-form))))

(defmethod export-form ((a-let/cc-form let/cc-form))
  `(:let/cc-form ,(export-cont-def (?cont a-let/cc-form))
    :body ,(export-form (?body a-let/cc-form))))

(defun export-cont-def (a-cont)
  `(,(set-export-id a-cont)
    :level ,(?level a-cont)))

(defmethod export-form ((a-cont cont))
  `(:cont ,(?export-id a-cont)))

(defmethod export-form ((a-tagbody-form tagbody-form))
  (let ((tagged-form-list (?tagged-form-list a-tagbody-form)))
    (progn
      (mapc #'set-export-id tagged-form-list)
      `(:tagbody-form ,(set-export-id a-tagbody-form)
        ,(export-form (?first-form a-tagbody-form))
        ,(mapcar #'export-tagged-form-def tagged-form-list)))))

(defun export-tagged-form-def (a-tagged-form)
  `(,(?export-id a-tagged-form)
    ,(export-form (?form a-tagged-form))
    ,(?export-id (?tagbody a-tagged-form))
    :adr ,(?adr a-tagged-form)))

(defmethod export-form ((a-tagged-form tagged-form))
  `(:tagged-form ,(?export-id a-tagged-form)))

(defmethod export-form ((a-mv-lambda mv-lambda))
  `(:mv-lambda
    ,(export-params (?params a-mv-lambda))
    :body ,(export-form (?body a-mv-lambda))
    :arg ,(export-form (?arg a-mv-lambda))
    :mv-spec ,(?mv-spec a-mv-lambda)))

;;------------------------------------------------------------------------------
;; Beschreibung einer benannten Konstante in die .def Datei schreiben.
;;------------------------------------------------------------------------------
(defun export-named-const (named-const)
  (when (?exported named-const)
    (let ((value (?value named-const)))
      (if (structured-literal-p value)
          (print `(,(?symbol named-const)
                   :value-zs-type ,(typecase (?value value)
                                     (cons :cons)
                                     (string :string)
                                     (vector :vector)
                                     (array :array)
                                     (literal-instance :literal-instance))
                   :adr ,(?adr value)))
          (internal-error
           'export-named-const
           "The value of the named constant ~S is not a structured literal."
           (?symbol named-const))))))

;;------------------------------------------------------------------------------
;; Vorbereitung des Exports von Funktionsruempfen.
;;------------------------------------------------------------------------------
(defmethod mark-for-export ((object T)))

(defmethod mark-for-export ((a-global-fun global-fun))
  (unless (?exported a-global-fun)
    (setf (?exported a-global-fun) 'inline)))

(defmethod mark-for-export ((a-sym sym))
  (unless (?exported a-sym)
    (setf (?exported a-sym) 'inline)))

(defmethod mark-for-export ((a-var-ref var-ref))
  (mark-for-export (?var a-var-ref)))

(defmethod mark-for-export ((a-dynamic dynamic))
  (mark-for-export (?sym a-dynamic)))

(defmethod mark-for-export ((a-defined-named-const defined-named-const))
  (unless (?exported a-defined-named-const)
    (setf (?exported a-defined-named-const) 'inline)))

(defvar *new-exported-fun* nil)

(defun prepare-export-write ()
  (unless *no-inlining*

    (mapc #'(lambda (a-fun)
              (when (?exported a-fun) (make-copyable a-fun)))
          (?fun-list *module*))
    (traverse-module
     *module*
     :before-funs (list #'(lambda (object)
                            (setf *new-exported-fun*
                                  (and (global-fun-p object)
                                       (is-defined-fun-without-recursion-p
                                        object)
                                       (not (?exported object))
                                       (<= (export-weight object)
                                           *max-inline-weight*)))
                            (when *new-exported-fun*
                              (make-copyable object))))
     :after-funs (list #'mark-for-export)
     :fun-selector #'(lambda (a-fun)
                       (or (local-fun-p a-fun)
                           (and (global-fun-p a-fun) (eq (?exported a-fun) T))))
     :tr-fun-body-p #'(lambda (a-fun)
                        (declare (ignore a-fun))
                        *new-exported-fun*))))

;;------------------------------------------------------------------------------
;; Schreiben der .def Datei
;; Die Symbole zur Benennung der Beschreibungen werden im aktuellen Package,
;; also ohne Package-Qualifizierer ausgegeben.
;;------------------------------------------------------------------------------
(defun export-write ()
  (with-open-file (*standard-output* (calc-def-filename *OUT-FILENAME*)
                                     :direction :output
                                     :if-exists :supersede)
    (let ((*package* (?package *module*))
          (*print-circle* nil)
          (*print-length* nil)
          (*print-level* nil))
      (format T ";;; This is a CLICC-generated definition header file.~%")
      (format T ";;; It contains exports of module ~A~%"
              (package-name *package*))
      (print :name)
      (print (?name *module*))
      (print (?init-fun-name *module*))
      (print (?symbol-base *module*))
      (print :package)
      (print (package-name (?package *module*)))
      (print :sym)
      (mapc #'export-sym (?sym-list *module*))
      (terpri)
      (print :named-const)
      (mapc #'export-named-const (?named-const-list *module*))
      (terpri)
      (print :fun)
      (mapc #'export-fun (?fun-list *module*))
      (terpri)
      (when (and *optimize* (not *no-inlining*))
        (print :body)
        (mapc #'export-body (?fun-list *module*))
        (terpri))
      (print :eof))))

;;------------------------------------------------------------------------------
;; Die Beschreibung einer Funktion enthaelt, in den Slots READ und WRITE
;; sowohl imported-symbols als auch defined-symbols.
;; Bei imported-symbols liefert "get-global-dynamic" die entsprechende 
;; globale Variable zurueck.
;; Bei defined-symbols liefert "imported-get-global-dynamic" eine
;; neue dynamische Variable zurueck, die nur in der Beschreibung
;; der Funktion referenziert wird.
;;------------------------------------------------------------------------------
(defun imported-get-global-dynamic (symbol)
  (let ((sym (make-instance 'imported-sym
                            :symbol symbol 
                            :constant-value :unknown)))
    (make-instance 'dynamic :sym sym))) 

;;------------------------------------------------------------------------------
;; Liefert die dem Symbol entsprechende dynamische Variable , wenn das 
;; Symbol imported-sym ist, sonst eine neue dynamische Variable 
;; die einen Zeiger auf das Symbol enthaelt.
;;------------------------------------------------------------------------------
(defun get-dyn (sym)
  (let ((bind (find-global-dynamic sym)))
    (if bind
        (cdr bind)
        (imported-get-global-dynamic sym))))

;;------------------------------------------------------------------------------
;; Beschreibung einer importierten Funktion bearbeiten
;;------------------------------------------------------------------------------
(defun import-fun (fun-descr)
  (let ((args fun-descr) (raw ()))

    (labels ((read-list2dyn-list (read-list)
               (if (atom read-list)
                   read-list
                   (mapcar #'get-dyn read-list))))
    
      (macrolet
          ((raw-case (&optional (value-transformer '(lambda (x) x)))
#|             `(progn
               (push (cons (second args) (,value-transformer (third args))) raw)
               (setf (cdr args) (cdddr args)))
|#
           `(transform ,value-transformer)
)
           (transform (value-transformer)
             `(progn
               (setf (third args) (,value-transformer (third args)))
               (setq args (cddr args)))))
        (loop
         (when (null (cdr args))
#|
           (setf (cdr args) `(:raw ,raw))
|#
           (return))
         (case (second args)
           (:read-list (transform read-list2dyn-list))
           (:write-list (transform read-list2dyn-list))
           (:need-no-stack (raw-case))
           (:has-funs-as-args
            (raw-case
             (lambda (list)
               (mapcar #'(lambda (elem)
                           (if (integerp elem) elem (get-symbol-bind elem)))
                       list))))
           (:mv-spec (raw-case (lambda (v) (if (eq v :T) 'T v))))
           (:simp-when-n-args
            (raw-case
             (lambda (n/fun)
               (setf (second n/fun) (name2fun (second n/fun))) n/fun)))
           (:simp-when-no-result (raw-case name2fun))
           (:simp-when-arg-n=cons
            (raw-case
             (lambda (n/fun)
               (setf (second n/fun) (name2fun (second n/fun))) n/fun)))
           (:simp-when-some-arg-not-cons/pathn/string/bitv
            (raw-case name2fun))
           (:simp-when-some-arg-not-num/char (raw-case name2fun))
           (:simp-test-fun-when-not-testnot
            (raw-case
             (lambda (pos/nkargs/tkeyw/deflt/tnkeyw)
               (setf (third pos/nkargs/tkeyw/deflt/tnkeyw)
                     (get-symbol-bind (third pos/nkargs/tkeyw/deflt/tnkeyw)))
               (setf (fourth pos/nkargs/tkeyw/deflt/tnkeyw)
                     (name2fun (fourth pos/nkargs/tkeyw/deflt/tnkeyw)))
               (setf (fifth pos/nkargs/tkeyw/deflt/tnkeyw)
                     (get-symbol-bind (fifth pos/nkargs/tkeyw/deflt/tnkeyw)))
               pos/nkargs/tkeyw/deflt/tnkeyw)))
           (:simp-when-only-test=value
            (raw-case
             (lambda (nkargs/keyw/val/fun)
               (setf (second nkargs/keyw/val/fun)
                     (get-symbol-bind (second nkargs/keyw/val/fun)))
               (setf (third nkargs/keyw/val/fun)
                     (name2fun (third nkargs/keyw/val/fun)))
               (setf (fourth nkargs/keyw/val/fun)
                     (name2fun (fourth nkargs/keyw/val/fun)))
               nkargs/keyw/val/fun)))
           (:my-last-arg-may-be-rest-var (raw-case))
           (otherwise (setq args (cddr args))))))))

  (let* ((name (car fun-descr))
         (fun (apply #'make-instance
                     (get (if (consp name) (second name) name)
                          'zws-type
                          #-CMU17 (find-class 'imported-fun)
                          #+CMU17 'imported-fun)
                     :symbol name
                     (cdr fun-descr))))
    (when (not (slot-boundp fun 'adr))
      (cg-set-C-name fun))
    (if (consp name)
        (set-imported-setf-fun name fun)
        (set-imported-fun name fun))))

;;------------------------------------------------------------------------------
;; Beschreibung eines importierten Symbols bearbeiten
;;------------------------------------------------------------------------------
(defun import-sym (sym-descr sym-base)
  (let ((sym
         (apply #'make-instance
                #-CMU17 (find-class 'imported-sym) #+CMU17 'imported-sym
                :symbol (car sym-descr)
                :base sym-base
                (cdr sym-descr))))
    (bind-symbol sym)))

;;------------------------------------------------------------------------------
;; Beschreibung der Parameter und des Rumpfes einer importierten Funktion
;; bearbeiten.
;;------------------------------------------------------------------------------
(defvar *import-array* nil)

(defun import-body (fun-descr)
  (let* ((*import-array* (make-array 20 :adjustable t))
         (fun-name (pop fun-descr))
         (fun (name2fun fun-name)))
    (setf (?params fun) (import-params (pop fun-descr)))
    (setf (?body fun) (import-form (pop fun-descr)))
    (when fun-descr
      (setf (?local-funs fun) (mapcar #'import-form fun-descr)))))

(defun import-var (var-descr)
  (case (first var-descr)
    (:local-static (aref *import-array* (second var-descr)))
    (:dynamic (get-global-dynamic (second var-descr)))))

(defun import-form (form-descr)
  (if (atom form-descr)
      form-descr
      (case (first form-descr)
        (:var-ref (apply #'make-instance (find-class 'var-ref)
                         :var (import-var (second form-descr))
                         (map-import-form (rest form-descr))))
        (:named-const (get-imported-named-const (second form-descr)))
        (:sym (get-symbol-bind (second form-descr)))
        (:empty-list empty-list)
        (:character-form (apply #'make-instance (find-class 'character-form)
                                (map-import-form form-descr)))
        (:int (apply #'make-instance (find-class 'int)
                     (map-import-form form-descr)))
        (:float-form (apply #'make-instance (find-class 'float-form)
                            (map-import-form form-descr)))
        (:class-def (get-class-def (second form-descr))) ; ?? ft ??
        (:global-fun (name2fun (second form-descr)))
        (:local-fun (aref *import-array* (second form-descr)))
        (:imported-fun (name2fun (second form-descr)))
        (:app (apply #'make-instance (find-class 'app)
                     (let ((slots (map-import-form form-descr)))
                       (if (find :downfun-list slots)
                           slots
                           (cons :downfun-list (cons nil slots))))))
        (:setq-form (apply #'make-instance (find-class 'setq-form)
                           (map-import-form form-descr)))
        (:progn-form (apply #'make-instance (find-class 'progn-form)
                            (map-import-form form-descr)))
        (:if-form (apply #'make-instance (find-class 'if-form)
                         (map-import-form form-descr)))
        (:switch-form (apply #'make-instance (find-class 'switch-form)
                             (map-import-form form-descr)))
        (:labeled-form (apply #'make-instance (find-class 'labeled-form)
                              (map-import-form form-descr)))
        (:let*-form (apply #'make-instance (find-class 'let*-form)
                           :var-list (mapcar #'import-var-def
                                             (second form-descr))
                           (map-import-form (rest form-descr))))
        (:labels-form (apply #'make-instance (find-class 'labels-form)
                             :fun-list (mapcar #'import-local-fun-def
                                               (second form-descr))
                             (map-import-form (rest form-descr))))
        (:let/cc-form (apply #'make-instance (find-class 'let/cc-form)
                             :cont (import-cont-def (second form-descr))
                             (map-import-form (rest form-descr))))
        (:cont (aref *import-array* (second form-descr)))
        (:tagbody-form (import-tagbody-form (rest form-descr)))
        (:tagged-form (aref *import-array* (second form-descr)))
        (:mv-lambda (apply #'make-instance (find-class 'mv-lambda)
                           :params (import-params (second form-descr))
                           (map-import-form (rest form-descr))))
        (otherwise (mapcar #'import-form form-descr)))))

(defun import-tagbody-form (tagbody-descr)
  (let* ((tagbody-id (pop tagbody-descr))
         (first-form-descr (pop tagbody-descr))
         (tagged-form-list-descr (pop tagbody-descr))
         (tagbody (fill-import-array
                   tagbody-id
                   (apply #'make-instance (find-class 'tagbody-form)
                          (mapcar #'import-form tagbody-descr)))))
    (setf (?tagged-form-list tagbody)
          (mapcar #'(lambda (tagged-form-descr)
                      (fill-import-array
                       (first tagged-form-descr)
                       (apply #'make-instance (find-class 'tagged-form)
                              (map-import-form (cddr tagged-form-descr)))))
                  tagged-form-list-descr))
    (setf (?first-form tagbody) (import-form first-form-descr))
    (mapcar #'import-tagged-form-def tagged-form-list-descr)
    tagbody))

(defun import-tagged-form-def (tagged-form-descr)
  (let ((tagged-form (aref *import-array* (pop tagged-form-descr))))
    (setf (?form tagged-form) (import-form (pop tagged-form-descr)))
    (setf (?tagbody tagged-form) (aref *import-array*
                                       (first tagged-form-descr)))))

(defun import-cont-def (cont-descr)
  (fill-import-array (first cont-descr)
                     (apply #'make-instance (find-class 'cont)
                            (map-import-form cont-descr))))

(defun map-import-form (form-descr)
  (mapcar #'import-form (rest form-descr)))

(defun import-var-def (var-descr)
  (case (first var-descr)
    (:local-static
     (fill-import-array (second var-descr)
                        (apply #'make-instance (find-class 'local-static)
                               :symbol
                               'inline-var
                               (map-import-form (cdr var-descr)))))
    (:dynamic (import-var var-descr))))

(defun import-local-fun-def (fun-descr)
  (fill-import-array (pop fun-descr)
                     (apply #'make-instance (find-class 'local-fun)
                      :symbol (pop fun-descr)
                      :adr (pop fun-descr)
                      :par-spec (pop fun-descr)
                      :params (import-params (pop fun-descr))
                      :body (import-form (first fun-descr))
                      (map-import-form fun-descr))))

(defvar *all-vars* ())

(defun import-params (param-descr)
  (let ((*all-vars* ()))
    (make-params
     :var-list (mapcar #'import-param (pop param-descr))
     :opt-list (mapcar #'import-opt (pop param-descr))
     :rest (let ((rest (pop param-descr)))
             (when rest
               (import-param rest)))
     :key-list (mapcar #'import-key (pop param-descr))
     :allow-other-keys (first param-descr)
     :all-vars *all-vars*)))

(defun import-param (var-descr)
  (let ((var (import-var-def var-descr)))
    (push var *all-vars*)
    var))

(defun import-opt (opt-descr)
  (make-opt
   :var (import-param (pop opt-descr))
   :init (import-form (pop opt-descr))
   :suppl (let ((suppl (first opt-descr)))
            (when suppl
              (import-param suppl)))))

(defun import-key (key-descr)
  (make-key
   :var (import-param (pop key-descr))
   :init (import-form (pop key-descr))
   :suppl (let ((suppl (pop key-descr)))
            (when suppl
              (import-param suppl)))
   :sym (import-form (first key-descr))))

(defun fill-import-array (index item)
  (unless (array-in-bounds-p *import-array* index)
    (adjust-array *import-array* (min (+ (array-total-size *import-array*) 20)
                                      (+ index 5))))
  (setf (aref *import-array* index) item))

;;------------------------------------------------------------------------------
;; Beschreibung einer importierten benannten Konstante bearbeiten.
;;------------------------------------------------------------------------------
(defvar *imported-named-consts* ())

(defun import-named-const (named-const-descr)
  (push (apply #'make-instance (find-class 'imported-named-const)
               :symbol (first named-const-descr)
               (map-import-form named-const-descr))
        *imported-named-consts*))

(defun get-imported-named-const (name)
  (let* ((named-const (find name *imported-named-consts*
                            :test #'eq :key #'?symbol)))
    (if named-const
        named-const
        (internal-error 'get-imported-named-const
                        "Unknown imported named const referenced: ~S"
                        name))))

;;------------------------------------------------------------------------------
;; Lesen der .def Datei
;; Die Symbole zur Benennung der Beschreibungen werden in das Package des Moduls
;; eingelesen.
;;------------------------------------------------------------------------------
(defun import-read (module-name)
  (with-open-file (*standard-input* (calc-def-filename module-name)
                                    :direction :input)
    
    ;; Package wird spaeter durch :package spezifiziert, waehle zunaechst
    ;; *keyword-package* als unschaedlichen Wert
    ;;-------------------------------------------------
    (let ((*package* *keyword-package*)
          (name nil)
          (init-fun-name nil)
          (symbol-base nil)
          (sys nil)
          (type (read))
          (new-module nil))
                      
      (loop
       (case type
         (:sys  (setq sys (read))
                (setq type (read)))
         (:name (setq name (read))
                (setq init-fun-name (read))
                (setq symbol-base (read))
                (setq type (read)))
         (:package (let ((name (read)))
                     (setq *package* (or (find-package name)
                                         (make-package name :use ()))))
                   (setq type (read)))
         (:fun (do ((fun-descr (read) (read)))
                   ((atom fun-descr) (setq type fun-descr))
                 (import-fun fun-descr)))
         (:sym (if (null symbol-base)
                   (internal-error
                    "import-read"
                    ":NAME must preceede :SYM in ~A~%"
                    (calc-def-filename module-name))
                   (do ((sym-descr (read) (read)))
                       ((atom sym-descr) (setq type sym-descr))
                     (import-sym sym-descr symbol-base))))
         (:named-const (do ((named-const-descr (read) (read)))
                           ((atom named-const-descr)
                            (setq type named-const-descr))
                         (import-named-const named-const-descr)))
         (:body (do ((body-descr (read) (read)))
                    ((atom body-descr) (setq type body-descr))
                  (import-body body-descr)))
         (:eof (return))
         (T (internal-error "import-read" "Undefined keyword: ~A" type))))

      ;; Importiertes Modul erzeugen
      ;;----------------------------
      (setq new-module
            (make-imported-module :name name
                                  :init-fun-name init-fun-name
                                  :symbol-base symbol-base))

      ;; Falls kein Name angegeben ist, dann ist es sys.def
      ;;---------------------------------------------------
      (unless sys
        (addnew-q new-module (?loaded-modules *module*)))

      new-module)))

;;------------------------------------------------------------------------------
(provide "deffile")
