;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Funktionen zum Aufspueren und Beseitigen von unbenutzten
;;;            Funktionen, Symbolen und benannten Konstanten.
;;;
;;; $Revision: 1.33 $
;;; $Log: delete.lisp,v $
;;; Revision 1.33  1994/04/05  15:07:54  jh
;;; ?used-Methode fuer importierte Funktionen definiert.
;;;
;;; Revision 1.32  1994/03/03  13:47:57  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.31  1994/02/08  11:27:11  sma
;;; Klammer eingefügt (hätte ich bloß nix geändert...)
;;;
;;; Revision 1.30  1994/02/08  11:07:31  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.29  1994/02/04  14:25:06  kl
;;; Formatstring zur Meldung über unbenutzte Klassen korrigiert.
;;;
;;; Revision 1.28  1994/02/01  12:10:22  hk
;;; dec-used-slot nur auf definierte, nicht auf importierte Funktionen
;;; anwenden.
;;;
;;; Revision 1.27  1994/01/31  13:49:19  hk
;;; inc-used-slot nur auf definierte, nicht auf importierte Funktionen
;;; anwenden.
;;;
;;; Revision 1.26  1993/09/21  15:02:53  jh
;;; dec-used-slot hinzugefuegt.
;;;
;;; Revision 1.25  1993/09/20  14:17:58  jh
;;; is-used verbessert.
;;;
;;; Revision 1.24  1993/09/06  13:03:37  jh
;;; Jetzt werden in set-used-slots-for-cg wirklich alle Funktionen (auch
;;; die lokalen) analysiert.
;;;
;;; Revision 1.23  1993/09/06  10:00:29  jh
;;; set-used-slots-for-cg analysiert alle Funktionen, ob benutzt oder nicht.
;;; Dies wird benoetigt, damit der Codegenerator bei ausgeschalteter Optimierung
;;; mit den noetigen Informationen versorgt werden kann.
;;;
;;; Revision 1.22  1993/09/02  08:13:09  jh
;;; Die class-precedence-list ist ein structured-literal und enthaelt die
;;; Klasse T nicht. Der Aufruf von inc-used-slot ist jetzt entsprechend
;;; angepasst.
;;;
;;; Revision 1.21  1993/09/01  11:50:05  ft
;;; Beim erhoehen der used-slots von Elementen der class-precedence-list
;;; einer Klasse die erste (die Klasse selbst) und das letzte (die Klasse
;;; T) Element ausgenommen.
;;;
;;; Revision 1.20  1993/09/01  11:45:45  ft
;;; inc-used-slot zaehlt jetzt den used-slot aller Elemente der
;;; class-precedence-list einer Klasse rauf, statt denen der super-list.
;;;
;;; Revision 1.19  1993/08/04  09:39:03  ft
;;; Die Meldung ueber unbenutzte Klassen bildet jetzt den richtigen
;;; Plural von 'class'.
;;;
;;; Revision 1.18  1993/07/24  07:59:56  ft
;;; Erweiterung um das Löschen ungenutzter Klassen.
;;;
;;; Revision 1.17  1993/07/15  12:27:37  hk
;;; ~:*~[s~; ~:;s~] durch ~:p ersetzt
;;;
;;; Revision 1.16  1993/06/29  11:20:54  jh
;;; Schreibfehler beseitigt.
;;;
;;; Revision 1.15  1993/06/29  10:55:38  jh
;;; Das Entfernen von let/cc-forms mit unbenutzter Continuation und labels-forms
;;; mit leerer fun-list nach simplifier.lisp verlegt.
;;; Generische Funktion is-used eingebaut.
;;;
;;; Revision 1.14  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.13  1993/06/14  10:28:12  jh
;;; Nicht benutzte continuations werden entfernt.
;;;
;;; Revision 1.12  1993/03/19  08:46:42  ft
;;; sym aus *objects-to-delete* entfernt, da noch Konflikte mit dem
;;; Modul-System bestehen.
;;;
;;; Revision 1.11  1993/03/18  15:18:58  jh
;;; Fehler bei vorhandenen importierten Symbolen etc. beseitigt.
;;;
;;; Revision 1.10  1993/03/18  13:11:22  jh
;;; Fehler in inc-used-slot(let*-form) behoben.
;;;
;;; Revision 1.9  1993/03/18  11:05:02  hk
;;; Fehler in inc-used-slot(var-ref) behoben.
;;;
;;; Revision 1.8  1993/03/16  16:52:49  jh
;;; Fehler beseitigt.
;;;
;;; Revision 1.7  1993/03/16  14:14:24  jh
;;; set-used-slots benutzt jetzt eigenes Traversierverfahren.
;;;
;;; Revision 1.6  1993/02/25  13:16:44  jh
;;; Lokale Funktionen werden jetzt ebenfalls entfernt.
;;;
;;; Revision 1.5  1993/02/16  15:24:21  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.4  1993/02/16  12:44:03  jh
;;; Fehler bei structured-literal beseitigt.
;;;
;;; Revision 1.3  1993/02/11  13:27:41  jh
;;; mv-lambda eingebaut.
;;;
;;; Revision 1.2  1993/02/10  13:32:31  jh
;;; Fehler beseitigt und Ausgabe geaendert.
;;;
;;; Revision 1.1  1993/01/27  13:25:04  jh
;;; Initial revision
;;------------------------------------------------------------------------------

(in-package "CLICC")

(require "traverse")

;;------------------------------------------------------------------------------
(defvar *objects-to-delete* '(fun local-fun named-constant class))
(defvar *delete-verbosity* 1)
(defvar *unused-local-funs* nil)
(defvar *delete-path* nil)

;;------------------------------------------------------------------------------
;; Die Funktion search-and-delete-unused-objects ermittelt unbenutzte Objekte
;; im aktuellen Modul, gibt diese aus und entfernt sie.
;;------------------------------------------------------------------------------

(defun search-and-delete-unused-objects ()
  (let ((*unused-local-funs* '()))
    (set-used-slots *module*)
    (write-unused-objects *module*)
    (delete-unused-objects *module*)))

;;------------------------------------------------------------------------------
;; clear-all-used-slots setzt die used slots von Funktionen, Symbolen und
;; benannten Konstanten auf 0.
;;------------------------------------------------------------------------------

(defmethod clear-used-slot ((a-zws-object zws-object))
  (setf (?used a-zws-object) 0))

(defmethod clear-read-slot ((a-defined-named-const defined-named-const))
  (setf (?read a-defined-named-const) 0))

(defmethod clear-read-and-write-slot ((a-var var))
  (setf (?read a-var) 0
        (?write a-var) 0))

(defun clear-all-used-slots (a-module)
  (mapc #'clear-used-slot (?fun-list a-module))
  (when (?toplevel-forms a-module)
    (setf (?used (?toplevel-forms a-module)) 1))
  (mapc #'clear-read-slot (?named-const-list a-module))
  (mapc #'clear-used-slot (?sym-list a-module))
  (mapc #'clear-used-slot (?class-def-list a-module))
  (mapc #'clear-read-and-write-slot (?var-list a-module))
  (mapc #'clear-read-and-write-slot (?dyn-var-list a-module)))

;;------------------------------------------------------------------------------
;; is-used ermittelt, ob ein Zwischensprachobjekt benutzt wird.
;;------------------------------------------------------------------------------

(defmethod is-used ((a-zws-object zws-object))
  (plusp (?used a-zws-object)))

(defmethod is-used ((a-global-fun global-fun))
  (or (call-next-method)
      (?exported a-global-fun)
      (?call-in a-global-fun)))

(defmethod is-used ((a-sym sym))
  (or (call-next-method)
      (?exported a-sym)
      (constant-value-p a-sym)))

(defmethod is-used ((a-defined-named-const defined-named-const))
  (or (plusp (?read a-defined-named-const))
      (?exported a-defined-named-const)))

(defmethod is-used ((a-var var))
  (or (plusp (?write a-var))
      (plusp (?read a-var))))

(defmethod is-used ((a-local-static local-static))
  (or (> (?write a-local-static) 1)
      (plusp (?read a-local-static))))

(defmethod is-used ((a-global-static global-static))
  (or (call-next-method)
      (?exported a-global-static)))

(defmethod is-used ((a-imported-static imported-static))
  (or (call-next-method)
      (?exported a-imported-static)))

(defmethod ?used ((an-imported-fun imported-fun))
  most-positive-fixnum)

;;------------------------------------------------------------------------------
;; inc-used-slot erhoeht den Inhalt der used slots von Funktionen, Symbolen und
;; benannten Konstanten, wenn diese beim traversieren der toplevel-forms und der
;; exportierten Funktionen erreicht werden. Werden local-funs oder continuations
;; als unbenutzt erkannt, werden die entsprechenden labels- und let/cc-forms
;; entfernt. Deshalb muss jede Methode das resultierende Zwischensprachobjekt
;; zurueckliefern.
;;------------------------------------------------------------------------------

(defmethod inc-used-slot ((anything T) &optional write)
  (declare (ignore write)))

(defmethod inc-used-slot ((a-var var) &optional write)
  (unless (analysed-p a-var)
    (mark-as-analysed a-var)
    (clear-read-and-write-slot a-var))
  (if write
      (incf (?write a-var))
      (incf (?read a-var))))

(defmethod inc-used-slot ((a-dynamic dynamic) &optional write)
  (declare (ignore write))
  (call-next-method)
  (inc-used-slot (?sym a-dynamic)))

(defmethod inc-used-slot ((a-var-ref var-ref) &optional write)
  (inc-used-slot (?var a-var-ref) write))

(defmethod inc-used-slot ((a-defined-named-const defined-named-const)
                          &optional write)
  (unless (analysed-p a-defined-named-const)
    (mark-as-analysed a-defined-named-const)
    (inc-used-slot (?value a-defined-named-const)))
  (unless write
    (incf (?read a-defined-named-const))))

(defmethod inc-used-slot ((a-sym sym) &optional write)
  (declare (ignore write))
  (unless (analysed-p a-sym)
    (mark-as-analysed a-sym)
    (clear-used-slot a-sym)
    (when (constant-value-p a-sym)
      (inc-used-slot (?constant-value a-sym))))
  (incf (?used a-sym)))

(defmethod inc-used-slot ((a-structured-literal structured-literal)
                          &optional write)
  (declare (ignore write))
  (inc-used-slot (?value a-structured-literal))) ; Kommt genau einmal vor!

(defmethod inc-used-slot ((a-string string) &optional write)
  (declare (ignore write)))

(defmethod inc-used-slot ((an-array array) &optional write)
  (declare (ignore write))
  (let* ((total-size (array-total-size an-array))
         (flat-array (make-array total-size
                                 :displaced-to an-array
                                 :element-type (array-element-type an-array))))
    (dotimes (index total-size)
      (inc-used-slot (aref flat-array index)))))

(defmethod inc-used-slot ((a-cons cons) &optional write)
  (declare (ignore write))
  (inc-used-slot (car a-cons))
  (inc-used-slot (cdr a-cons)))

(defmethod inc-used-slot ((a-literal-instance literal-instance) &optional write)
  (declare (ignore write))
  (inc-used-slot (?class a-literal-instance))
  (mapc #'inc-used-slot (?value-list a-literal-instance)))

(defmethod inc-used-slot ((a-class-def class-def) &optional write)
  (declare (ignore write))
  (unless (analysed-p a-class-def)
    (mark-as-analysed a-class-def)
    (clear-used-slot a-class-def)
    (mapc #'inc-used-slot (rest (?value (?class-precedence-list a-class-def))))
    (mapc #'inc-used-slot (?slot-descr-list a-class-def))
    (inc-used-slot (?symbol a-class-def)))
  (incf (?used a-class-def)))

(defmethod inc-used-slot ((a-slot-desc slot-desc) &optional write)
  (declare (ignore write))
  (inc-used-slot (?symbol a-slot-desc))
  (inc-used-slot (?initform a-slot-desc))
  (mapc #'inc-used-slot (?initargs a-slot-desc)))

(defmethod inc-used-slot ((parameters params) &optional write)
  (declare (ignore write))
  (dolist (a-var (?var-list parameters))
    (inc-used-slot a-var 'write))
  (mapc #'inc-used-slot (?opt-list parameters))
  (when (?rest parameters)
    (inc-used-slot (?rest parameters) 'write))
  (mapc #'inc-used-slot (?key-list parameters)))

(defmethod inc-used-slot ((an-opt opt) &optional write)
  (declare (ignore write))
  (inc-used-slot (?var an-opt) 'write)
  (inc-used-slot (?init an-opt))
  (inc-used-slot (?suppl an-opt) 'write))

(defmethod inc-used-slot ((a-key key) &optional write)
  (declare (ignore write))
  (call-next-method)
  (inc-used-slot (?sym a-key)))

(defmethod inc-used-slot ((a-fun defined-fun) &optional write)
  (declare (ignore write))
  (unless (analysed-p a-fun)
    (mark-as-analysed a-fun)
    (clear-used-slot a-fun)
    (inc-used-slot (?params a-fun))
    (let ((*delete-path* (cons a-fun *delete-path*)))
      (inc-used-slot (?body a-fun)))
    (when (member 'local-fun *objects-to-delete*)
      (setf (?local-funs a-fun)
            (remove-if-not #'is-used (?local-funs a-fun)))))
  (incf (?used a-fun)))

(defmethod inc-used-slot ((an-app app) &optional write)
  (declare (ignore write))
  (inc-used-slot (?form an-app))
  (mapc #'inc-used-slot (?arg-list an-app)))

(defmethod inc-used-slot ((a-setq-form setq-form) &optional write)
  (declare (ignore write))
  (inc-used-slot (?location a-setq-form) 'write)
  (inc-used-slot (?form a-setq-form)))

(defmethod inc-used-slot ((a-progn-form progn-form) &optional write)
  (declare (ignore write))
  (mapc #'inc-used-slot (?form-list a-progn-form)))

(defmethod inc-used-slot ((an-if-form if-form) &optional write)
  (declare (ignore write))
  (inc-used-slot (?pred an-if-form))
  (inc-used-slot (?then an-if-form))
  (inc-used-slot (?else an-if-form)))

(defmethod inc-used-slot ((a-switch-form switch-form) &optional write)
  (declare (ignore write))
  (inc-used-slot (?form a-switch-form))
  (mapc #'inc-used-slot (?case-list a-switch-form))
  (inc-used-slot (?otherwise a-switch-form)))

(defmethod inc-used-slot ((a-labeled-form labeled-form) &optional write)
  (declare (ignore write))
  (inc-used-slot (?value a-labeled-form))
  (inc-used-slot (?form a-labeled-form)))

(defmethod inc-used-slot ((a-let*-form let*-form) &optional write)
  (declare (ignore write))
  (dolist (a-var (?var-list a-let*-form))
    (inc-used-slot a-var 'write))
  (mapc #'inc-used-slot (?init-list a-let*-form))
  (inc-used-slot (?body a-let*-form)))

(defmethod inc-used-slot ((a-labels-form labels-form) &optional write)
  (declare (ignore write))
  (dolist (a-local-fun (?fun-list a-labels-form))
    (clear-used-slot a-local-fun))
  (inc-used-slot (?body a-labels-form))
  (let ((used-local-funs '()))
    (dolist (a-local-fun (?fun-list a-labels-form))
      (if (is-used a-local-fun)
          (push a-local-fun used-local-funs)
          (push (describe-location a-local-fun) *unused-local-funs*)))
    (when (member 'local-fun *objects-to-delete*)
      (setf (?fun-list a-labels-form) used-local-funs))))

(defmethod inc-used-slot ((a-let/cc-form let/cc-form) &optional write)
  (declare (ignore write))
  (let ((cont (?cont a-let/cc-form)))
    (inc-used-slot cont 'write)
    (inc-used-slot (?body a-let/cc-form))))

(defmethod inc-used-slot ((a-tagbody-form tagbody-form) &optional write)
  (declare (ignore write))
  (dolist (a-tagged-form (?tagged-form-list a-tagbody-form))
    (clear-used-slot a-tagged-form))
  (inc-used-slot (?first-form a-tagbody-form))
  (dolist (a-tagged-form (?tagged-form-list a-tagbody-form))
    (inc-used-slot (?form a-tagged-form))))

(defmethod inc-used-slot ((a-tagged-form tagged-form) &optional write)
  (declare (ignore write))
  (incf (?used a-tagged-form)))

(defmethod inc-used-slot ((a-mv-lambda mv-lambda) &optional write)
  (declare (ignore write))
  (inc-used-slot (?params a-mv-lambda))
  (inc-used-slot (?arg a-mv-lambda))
  (inc-used-slot (?body a-mv-lambda)))

;;------------------------------------------------------------------------------
;; dec-used-slots aktualisiert den Inhalt der used-slots der Bestandteile von
;; Zwischensprachausdruecken, die wegoptimiert werden.
;;------------------------------------------------------------------------------

(defmethod dec-used-slot ((anything T) &optional write)
  (declare (ignore write)))

(defmethod dec-used-slot ((a-var var) &optional write)
  (if write
      (decf (?write a-var))
      (decf (?read a-var))))

(defmethod dec-used-slot ((a-dynamic dynamic) &optional write)
  (declare (ignore write))
  (call-next-method)
  (dec-used-slot (?sym a-dynamic)))

(defmethod dec-used-slot ((a-var-ref var-ref) &optional write)
  (dec-used-slot (?var a-var-ref) write))

(defmethod dec-used-slot ((a-defined-named-const defined-named-const)
                          &optional write)
  (unless write
    (decf (?read a-defined-named-const))))

(defmethod dec-used-slot ((a-sym sym) &optional write)
  (declare (ignore write))
  (decf (?used a-sym)))

(defmethod dec-used-slot ((a-structured-literal structured-literal)
                          &optional write)
  (declare (ignore write))
  (dec-used-slot (?value a-structured-literal)))

(defmethod dec-used-slot ((a-string string) &optional write)
  (declare (ignore write)))

(defmethod dec-used-slot ((an-array array) &optional write)
  (declare (ignore write))
  (let* ((total-size (array-total-size an-array))
         (flat-array (make-array total-size
                                 :displaced-to an-array
                                 :element-type (array-element-type an-array))))
    (dotimes (index total-size)
      (dec-used-slot (aref flat-array index)))))

(defmethod dec-used-slot ((a-cons cons) &optional write)
  (declare (ignore write))
  (dec-used-slot (car a-cons))
  (dec-used-slot (cdr a-cons)))

(defmethod dec-used-slot ((a-literal-instance literal-instance) &optional write)
  (declare (ignore write))
  (dec-used-slot (?class a-literal-instance))
  (mapc #'dec-used-slot (?value-list a-literal-instance)))

(defmethod dec-used-slot ((a-class-def class-def) &optional write)
  (declare (ignore write))
  (decf (?used a-class-def)))

(defmethod dec-used-slot ((a-slot-desc slot-desc) &optional write)
  (declare (ignore write))
  (dec-used-slot (?symbol a-slot-desc))
  (dec-used-slot (?initform a-slot-desc))
  (mapc #'dec-used-slot (?initargs a-slot-desc)))

(defmethod dec-used-slot ((parameters params) &optional write)
  (declare (ignore write))
  (dolist (a-var (?var-list parameters))
    (dec-used-slot a-var 'write))
  (mapc #'dec-used-slot (?opt-list parameters))
  (when (?rest parameters)
    (dec-used-slot (?rest parameters) 'write))
  (mapc #'dec-used-slot (?key-list parameters)))

(defmethod dec-used-slot ((an-opt opt) &optional write)
  (declare (ignore write))
  (dec-used-slot (?var an-opt) 'write)
  (dec-used-slot (?init an-opt))
  (dec-used-slot (?suppl an-opt) 'write))

(defmethod dec-used-slot ((a-key key) &optional write)
  (declare (ignore write))
  (call-next-method)
  (dec-used-slot (?sym a-key)))

(defmethod dec-used-slot ((a-fun defined-fun) &optional write)
  (declare (ignore write))
  (decf (?used a-fun)))

(defmethod dec-used-slot ((an-app app) &optional write)
  (declare (ignore write))
  (dec-used-slot (?form an-app))
  (mapc #'dec-used-slot (?arg-list an-app)))

(defmethod dec-used-slot ((a-setq-form setq-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?location a-setq-form) 'write)
  (dec-used-slot (?form a-setq-form)))

(defmethod dec-used-slot ((a-progn-form progn-form) &optional write)
  (declare (ignore write))
  (mapc #'dec-used-slot (?form-list a-progn-form)))

(defmethod dec-used-slot ((an-if-form if-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?pred an-if-form))
  (dec-used-slot (?then an-if-form))
  (dec-used-slot (?else an-if-form)))

(defmethod dec-used-slot ((a-switch-form switch-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?form a-switch-form))
  (mapc #'dec-used-slot (?case-list a-switch-form))
  (dec-used-slot (?otherwise a-switch-form)))

(defmethod dec-used-slot ((a-labeled-form labeled-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?value a-labeled-form))
  (dec-used-slot (?form a-labeled-form)))

(defmethod dec-used-slot ((a-let*-form let*-form) &optional write)
  (declare (ignore write))
  (dolist (a-var (?var-list a-let*-form))
    (dec-used-slot a-var 'write))
  (mapc #'dec-used-slot (?init-list a-let*-form))
  (dec-used-slot (?body a-let*-form)))

(defmethod dec-used-slot ((a-labels-form labels-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?body a-labels-form)))

(defmethod dec-used-slot ((a-let/cc-form let/cc-form) &optional write)
  (declare (ignore write))
  (let ((cont (?cont a-let/cc-form)))
    (dec-used-slot cont 'write)
    (dec-used-slot (?body a-let/cc-form))))

(defmethod dec-used-slot ((a-tagbody-form tagbody-form) &optional write)
  (declare (ignore write))
  (dec-used-slot (?first-form a-tagbody-form))
  (dolist (a-tagged-form (?tagged-form-list a-tagbody-form))
    (dec-used-slot (?form a-tagged-form))))

(defmethod dec-used-slot ((a-tagged-form tagged-form) &optional write)
  (declare (ignore write))
  (decf (?used a-tagged-form)))

(defmethod dec-used-slot ((a-mv-lambda mv-lambda) &optional write)
  (declare (ignore write))
  (dec-used-slot (?params a-mv-lambda))
  (dec-used-slot (?arg a-mv-lambda))
  (dec-used-slot (?body a-mv-lambda)))

;;------------------------------------------------------------------------------
;; Die Funktion set-used-slots zaehlt die angewandten Vorkommen von Funktionen,
;; Symbolen und benannten Konstanten, die von den toplevel-forms und den
;; exportierten Funktionen aus erreichbar sind.
;;------------------------------------------------------------------------------

(defun set-used-slots (a-module)
  (new-analyse-mark)
  (clear-all-used-slots a-module)
  ;; Bei Symbolen, die einen konstanten Wert enthalten, muss dieser ebenfalls
  ;; analysiert werden, auch wenn das Symbol nicht benutzt wird. (Wegen der
  ;; moeglichen Anwendung von 'symbol-value'.)
  (dolist (a-sym (?sym-list a-module))
    (when (constant-value-p a-sym)
      (mark-as-analysed a-sym)
      (inc-used-slot (?constant-value a-sym))))
  ;; Traversierung, ausgehend von den toplevel-forms:
  (inc-used-slot (?toplevel-forms a-module))
  ;; Traversierung, ausgehend von den exportierten Funktionen:
  (dolist (a-global-fun (?fun-list a-module))
    (when (?exported a-global-fun)
      (inc-used-slot a-global-fun))))

(defun set-used-slots-for-cg ()
  (new-analyse-mark)
  (clear-all-used-slots *module*)
  (dolist (a-sym (?sym-list *module*))
    (when (constant-value-p a-sym)
      (mark-as-analysed a-sym)
      (inc-used-slot (?constant-value a-sym))))
  ;; Alle Funktionen werden analysiert, ob benutzt oder nicht.
  (dolist (a-fun (?all-funs *module*))
    (inc-used-slot a-fun)))

;;------------------------------------------------------------------------------
;; Die Funktion describe-location gibt eine Liste zurueck, die die Namen der
;; Funktion a-local-fun und der Funktionen, in der sie lokal definiert ist, in
;; der Schachtelungsreihenfolge von innen nach aussen enthaelt.
;;------------------------------------------------------------------------------

(defun describe-location (a-local-fun)
  (labels ((describe-location-internal (fun-path)
             (if (local-fun-p (first fun-path))
                 (cons (?symbol (first fun-path))
                       (describe-location-internal (rest fun-path)))
                 (list (?symbol (first fun-path))))))
    (cons (?symbol a-local-fun)
          (describe-location-internal *delete-path*))))
  
;;------------------------------------------------------------------------------
;; Die folgenden Funktionen dienen zum Loeschen der als unbenutzt erkannten
;; Funktionen, Symbolen und benannten Konstanten.
;;------------------------------------------------------------------------------

(defun delete-unused-funs (a-module)
  (when (member 'fun *objects-to-delete*)
    (setf (?fun-list a-module)
          (remove-if-not #'is-used (?fun-list a-module)))))

(defun delete-unused-syms (a-module)
  (when (member 'sym *objects-to-delete*)
    (setf (?sym-list a-module)
          (remove-if-not #'is-used (?sym-list a-module)))))

(defun delete-unused-named-consts (a-module)
  (when (member 'named-constant *objects-to-delete*)
    (setf (?named-const-list a-module)
          (remove-if-not #'is-used (?named-const-list a-module)))))

(defun delete-unused-classes (a-module)
  (when (member 'class *objects-to-delete*)
    (setf (?class-def-list a-module)
          (remove-if-not #'is-used (?class-def-list a-module)))))

(defun delete-unused-objects (a-module)
  (delete-unused-funs a-module)
  (delete-unused-syms a-module)
  (delete-unused-named-consts a-module)
  (delete-unused-classes a-module))

;;------------------------------------------------------------------------------
;; write-unused-objects gibt die Liste der unbenutzten Funktionen, Symbolen und
;; benannten Konstanten aus.
;;------------------------------------------------------------------------------
(defun list-names (objects)
  (mapcar #'get-object-name objects))

(defmethod get-object-name ((a-class class-def))
  (?symbol (?symbol a-class)))

(defmethod get-object-name ((an-object T))
  (?symbol an-object))

(defun list-unused-funs (a-module)
  (remove-if #'is-used (?fun-list a-module)))

(defun list-unused-syms (a-module)
  (remove-if #'is-used (?sym-list a-module)))

(defun list-unused-named-consts (a-module)
  (remove-if #'is-used (?named-const-list a-module)))

(defun list-unused-classes (a-module)
  (remove-if #'is-used (?class-def-list a-module)))

(defun write-unused-objects (a-module)
  (clicc-message-line)
  (let ((unused-funs (list-unused-funs a-module))
        (unused-syms (list-unused-syms a-module))
        (unused-named-consts (list-unused-named-consts a-module))
        (unused-classes (list-unused-classes a-module)))
    
    (when (> *delete-verbosity* 1)
      (when unused-syms
        (clicc-message "The unused symbols are:~%~S"
                       (list-names unused-syms)))
      (when unused-named-consts
        (clicc-message "The unused named constants are:~%~S"
                       (list-names unused-named-consts)))
      (when unused-funs
        (clicc-message "The unused functions are:~%~S"
                       (list-names unused-funs)))
      (when *unused-local-funs*
        (clicc-message "The unused local functions are:~%~S"
                       *unused-local-funs*))
      (when unused-classes
        (clicc-message "The unused classes are:~%~S"
                       (list-names unused-classes))))

    (when (> *delete-verbosity* 0)
      (clicc-message "~D unused symbol~:p found"
                     (length unused-syms))
      (clicc-message "~D unused named constant~:p found"
                     (length unused-named-consts))
      (clicc-message "~D unused function~:p found"
                     (length unused-funs))
      (clicc-message "~D unused local function~:p found"
                     (length *unused-local-funs*))
      (clicc-message "~D unused~:* ~[classes~;class~:;classes~] found"
                     (length unused-classes))))

  (clicc-message-line))

;;------------------------------------------------------------------------------
(provide "delete")
