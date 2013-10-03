;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Erzeugung von Lisp-Code aus der Zwischensprache
;;;
;;; $Revision: 1.13 $
;;; $Log: zs2lisp.lisp,v $
;;; Revision 1.13  1993/10/12  18:39:18  kl
;;; Fehler in der Darstellung von tagbody-Ausdruecken behoben.
;;;
;;; Revision 1.12  1993/09/17  13:56:29  jh
;;; Test #'eq bei assoc angegeben.
;;;
;;; Revision 1.11  1993/07/22  13:10:57  jh
;;; Code uebersichtlicher gestaltet.
;;;
;;; Revision 1.10  1993/07/08  10:37:17  jh
;;; Fehler in zs2lisp-opt/key beseitigt.
;;;
;;; Revision 1.9  1993/06/24  13:17:55  jh
;;; Umwandlung von Klassendefinitionen eingebaut.
;;;
;;; Revision 1.8  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.7  1993/06/17  07:39:53  jh
;;; Conse werden jetzt korrekt ausgegeben.
;;;
;;; Revision 1.6  1993/06/11  11:59:45  jh
;;; Namenskonflikte lokaler Variablen mit dynamischen Variablen wird jetzt
;;; mit Hilfe von symbol-value und set geloest.
;;;
;;; Revision 1.5  1993/06/09  12:10:21  jh
;;; Namen lokaler Variablen und Funktionen, sowie Continuations und Tags werden
;;; jetzt viel sparsamer mit Postfixes versehen. (Annotation id wird nicht mehr
;;; benoetigt.
;;;
;;; Revision 1.4  1993/06/08  14:00:06  jh
;;; Fehler bei Applikationen behoben.
;;;
;;; Revision 1.3  1993/06/08  13:02:37  jh
;;; Einige Fehler beseitigt und progn-Einsparungen eingebaut.
;;;
;;; Revision 1.2  1993/06/04  11:43:10  jh
;;; Symbole und Listen werden jetzt gequotet.
;;;
;;; Revision 1.1  1993/06/03  13:48:36  jh
;;; Initial revision
;;;
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; zs2lisp liefert zu einem Zwischensprachausdruck den entsprechenden Lisp-Code.
;;------------------------------------------------------------------------------

(defvar *name-table*)
(defvar *is-quoted* nil)

(defun zs2lisp-fun (a-fun)
  (let (*name-table*)
    (append `(defun ,(?symbol a-fun) ,(zs2lisp-params (?params a-fun)))
            (remove-progn (zs2lisp-form (?body a-fun))))))

(defmethod get-symbol ((a-zws-object zws-object))
  (if (slot-boundp a-zws-object 'symbol)
      (?symbol a-zws-object)
      'clicc))

(defmethod get-symbol ((a-dynamic dynamic))
  (get-symbol (?sym a-dynamic)))

(defmethod get-symbol ((a-class-def class-def))
  (get-symbol (?symbol a-class-def)))

(defmethod get-symbol ((a-slot-desc slot-desc))
  (get-symbol (?symbol a-slot-desc)))

(defmethod get-symbol ((a-cont cont))
  (if (slot-boundp a-cont 'symbol)
      (?symbol a-cont)
      'cont))

(defmethod get-symbol ((a-tagged-form tagged-form))
  'tag)

(defun make-unique-name (a-zws-object)
  (labels ((build-name (name postfix)
             (intern (concatenate 'string name postfix)))
           (get-unique-postfix (name object)
             (let ((assoc (assoc name *name-table* :test #'eq)))
               (if assoc
                   (let* ((object-queue (rest assoc))
                          (pos (position object (queue2list object-queue))))
                     (if pos
                         (if (zerop pos) "" (concatenate 'string
                                                         "-"
                                                         (write-to-string pos)))
                         (progn
                           (add-q object object-queue)
                           (concatenate
                            'string "-"
                            (write-to-string
                             (1- (length (queue2list object-queue))))))))
                   (progn
                     (push (cons name (add-q object (empty-queue)))
                           *name-table*)
                     "")))))
    (let ((old-name (get-symbol a-zws-object)))
      (if (symbolp old-name)
          (build-name (symbol-name old-name)
                      (get-unique-postfix old-name a-zws-object))
          (list (first old-name) 
                (build-name (symbol-name (second old-name))
                            (get-unique-postfix (second old-name)
                                                a-zws-object)))))))

(defun zs2lisp-class-def (a-class-def)
  `(defclass ,(get-symbol a-class-def)
    ,(mapcar #'get-symbol (?super-list a-class-def))
    ,(mapcan #'zs2lisp-slot-desc (?slot-descr-list a-class-def))))

(defun zs2lisp-slot-desc (a-slot-desc)
  (append
   `(,(get-symbol a-slot-desc))
   (zs2lisp-allocation (?allocation a-slot-desc))
   (zs2lisp-initargs (?initargs a-slot-desc))
   (zs2lisp-initform (?initform a-slot-desc))
   (let ((type (?declared-type a-slot-desc)))
     (unless (eq type T)
       `(:type ,type)))))

(defun zs2lisp-allocation (an-allocation)
  (when (eq an-allocation 'class)
    '(:allocation class)))

(defun zs2lisp-initargs (initarg-list)
  (mapcan #'(lambda (initarg) `(:initarg ,(get-symbol initarg))) initarg-list))

(defun zs2lisp-initform (an-initform)
  (let ((lisp-initform (zs2lisp-form an-initform)))
    (unless (equal "SECRET-UNBOUND-SLOT-VALUE" lisp-initform)
      `(:initform ,(if (fun-p an-initform)
                       (zs2lisp-form (?body an-initform))
                       ;;`(,(second lisp-initform))
                       lisp-initform)))))

(defun zs2lisp-params (params)
  (append
   (zs2lisp-required (?var-list params))
   (zs2lisp-optional (?opt-list params))
   (zs2lisp-rest (?rest params))
   (zs2lisp-key (?key-list params))
   (if (?allow-other-keys params)
       '(&allow-other-keys)
       ())))

(defun zs2lisp-required (var-list)
  (mapcar #'zs2lisp-var var-list))

(defun zs2lisp-1optional (an-opt)
  (zs2lisp-opt/key (zs2lisp-var (?var an-opt)) (?init an-opt) (?suppl an-opt)))

(defun zs2lisp-opt/key (opt/key init svar)
  (if svar
      `(,opt/key ,(zs2lisp-form init) ,(zs2lisp-var svar))
      (if (eq init empty-list)
          (if (symbolp opt/key)
              opt/key
              (list opt/key))
          `(,opt/key ,(zs2lisp-form init)))))

(defun zs2lisp-optional (opt-list)
  (when opt-list
    (cons '&optional (mapcar #'zs2lisp-1optional opt-list))))

(defun zs2lisp-1key (a-key)
  (zs2lisp-opt/key `(,(zs2lisp-form (?sym a-key)) ,(zs2lisp-var (?var a-key)))
                   (?init a-key) (?suppl a-key)))

(defun zs2lisp-key (key-list)
  (when key-list
    (cons '&key (mapcar #'zs2lisp-1key key-list))))

(defun zs2lisp-rest (rest)
  (when rest
    `(&rest ,(zs2lisp-var rest))))

(defmethod zs2lisp-var ((a-local-static local-static))
  (make-unique-name a-local-static))

(defmethod zs2lisp-var ((a-global-static global-static))
  (get-symbol a-global-static))

(defmethod zs2lisp-var ((a-dynamic dynamic))
  (let ((name (get-symbol a-dynamic)))
    (if (assoc name *name-table* :test #'eq)
        ;; Der Name der dynamischen Variablen kollidiert mit dem Namen einer
        ;; lokalen Variablen.
        `(symbol-value (quote ,name))
        name)))

(defun zs2lisp-form-list (form-list)
  (mapcar #'zs2lisp-form form-list))

(defmethod zs2lisp-form ((anything T))
  anything)

(defmethod zs2lisp-form ((a-var-ref var-ref))
  (zs2lisp-var (?var a-var-ref)))

(defmethod zs2lisp-form ((a-named-const named-const))
  (get-symbol a-named-const))

(defmethod zs2lisp-form ((a-sym sym))
  (if (or *is-quoted*
          (eq a-sym (?constant-value a-sym)))
      ;; Das Symbol befindet sich in einem gequoteten Ausdruck oder wertet
      ;; zu sich selbst aus.
      (get-symbol a-sym)
      `(quote ,(get-symbol a-sym))))

(defmethod zs2lisp-form ((a-null-form null-form))
  ())

(defmethod zs2lisp-form ((a-simple-literal simple-literal))
  (?value a-simple-literal))

(defmethod zs2lisp-form ((a-structured-literal structured-literal))
  (zs2lisp-form (?value a-structured-literal)))

(defmethod zs2lisp-form ((a-cons cons))
  (if *is-quoted*
      (cons (zs2lisp-form (first a-cons))
            (zs2lisp-form (rest a-cons)))
      (let ((*is-quoted* t))
        `(quote ,(zs2lisp-form a-cons)))))

(defmethod zs2lisp-form ((a-literal-instance literal-instance))
  'hier-gehoert-eine-Instanz-her)

(defmethod zs2lisp-form ((a-class-def class-def))
  (get-symbol a-class-def))

(defmethod zs2lisp-form ((a-fun fun))
  `(function ,(get-symbol a-fun)))

(defmethod zs2lisp-form ((a-local-fun local-fun))
  `(function ,(make-unique-name a-local-fun)))

(defmethod zs2lisp-form ((an-app app))
  (let ((form (?form an-app))
        (args (zs2lisp-form-list (?arg-list an-app))))
    (if (and (fun-p form) (symbolp (get-symbol form)))
        (cons (second (zs2lisp-form form)) args)
        (append `(funcall ,(zs2lisp-form form))
                args))))

(defmethod zs2lisp-form ((a-setq-form setq-form))
  (let ((location (?location a-setq-form))
        name)
    (if (and (var-ref-p location)
             (dynamic-p (?var location))
             (assoc (setq name (get-symbol (?var location)))
                    *name-table* :test #'eq))
        ;; Der Name der dynamischen Variablen kollidiert mit dem Namen einer
        ;; lokalen Variablen.
        `(set (quote ,name) ,(zs2lisp-form (?form a-setq-form)))
        `(setq
          ,(zs2lisp-form (?location a-setq-form))
          ,(zs2lisp-form (?form a-setq-form))))))

(defmethod zs2lisp-form ((a-progn-form progn-form))
  (cons 'progn (zs2lisp-form-list (?form-list a-progn-form))))

(defmethod zs2lisp-form ((an-if-form if-form))
  (let ((pred (zs2lisp-form (?pred an-if-form)))
        (then (zs2lisp-form (?then an-if-form)))
        (else (zs2lisp-form (?else an-if-form))))
    (cond ((null then) (append `(unless ,pred) (remove-progn else)))
          ((null else) (append `(when ,pred) (remove-progn then)))
          (T `(if ,pred ,then ,else)))))

(defmethod zs2lisp-form ((a-switch-form switch-form))
  (append
   `(case ,(zs2lisp-form (?form a-switch-form)))
   (zs2lisp-form-list (?case-list a-switch-form))
   (when (?otherwise a-switch-form)
     (list (cons 'otherwise
                 (remove-progn (zs2lisp-form (?otherwise a-switch-form))))))))

(defmethod zs2lisp-form ((a-labeled-form labeled-form))
  (cons (zs2lisp-form (?value a-labeled-form))
        (remove-progn (zs2lisp-form (?form a-labeled-form)))))

(defmethod zs2lisp-form ((a-let*-form let*-form))
  (append `(let* ,(make-var-init-list
                   (mapcar #'zs2lisp-var (?var-list a-let*-form))
                   (zs2lisp-form-list (?init-list a-let*-form))))
          (remove-progn (zs2lisp-form (?body a-let*-form)))))

(defun make-var-init-list (var-list init-list)
  (mapcar #'(lambda (a-var init-form) (if init-form
                                          (list a-var init-form)
                                          a-var))
          var-list init-list))

(defmethod zs2lisp-form ((a-labels-form labels-form))
  (append `(labels ,(mapcar #'zs2lisp-local-fun (?fun-list a-labels-form)))
          (remove-progn (zs2lisp-form (?body a-labels-form)))))

(defun zs2lisp-local-fun (a-local-fun)
  (append `(,(make-unique-name a-local-fun)
            ,(zs2lisp-params (?params a-local-fun)))
    (remove-progn (zs2lisp-form (?body a-local-fun)))))

(defmethod zs2lisp-form ((a-let/cc-form let/cc-form))
  (append `(let/cc ,(make-unique-name (?cont a-let/cc-form)))
          (remove-progn (zs2lisp-form (?body a-let/cc-form)))))

(defmethod zs2lisp-form ((a-cont cont))
  (make-unique-name a-cont))

(defmethod zs2lisp-form ((a-tagbody-form tagbody-form))
  (append
   `(tagbody)
   (when (?first-form a-tagbody-form)
       (remove-progn (zs2lisp-form (?first-form a-tagbody-form))))
   
   (mapcan #'zs2lisp-tagged-form (?tagged-form-list a-tagbody-form))))

(defun zs2lisp-tagged-form (a-tagged-form)
  (cons (make-unique-name a-tagged-form)
        (remove-progn (zs2lisp-form (?form a-tagged-form)))))

(defmethod zs2lisp-form ((a-tagged-form tagged-form))
  `(go ,(make-unique-name a-tagged-form)))

(defmethod zs2lisp-form ((a-mv-lambda mv-lambda))
  `(mv-lambda ,(zs2lisp-params (?params a-mv-lambda))
    ,(zs2lisp-form (?body a-mv-lambda))
    ,(zs2lisp-form (?arg a-mv-lambda))))

(defun remove-progn (lisp-form)
  (if (and (listp lisp-form) (eq (first lisp-form) 'progn))
      (rest lisp-form)
      (list lisp-form)))
