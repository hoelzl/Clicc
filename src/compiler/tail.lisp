;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;--------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Tail-rekursion.
;;;            Tail-rekursive Aufrufe werden in geeigneter Form in 
;;;            Spruenge umgewandelt.
;;; Autor    : Anouar trigui
;;; $Revision: 1.15 $
;;; $Log: tail.lisp,v $
;;; Revision 1.15  1994/01/04  16:14:44  atr
;;; Die Funktion tail-rec-fun geändert. Nun werden werden die Level-Slots
;;; bei den lokalen Funktionen während der Tail-rekursion gesetzt, damit
;;; die Level-Slots der neu erzeugten Tagbody's auch gesetzt werden
;;; können.
;;;
;;; Revision 1.14  1994/01/03  11:58:13  atr
;;; Tail-rekursive Aufrufe, die innerhalb eines Let-Konstruktes sind, das
;;; dynamische Variablen bindet werden nicht mehr in Spr"unge umgewandelt,
;;; denn dies kann zu Fehlern führen.
;;;
;;; Revision 1.13  1993/12/22  11:13:01  atr
;;; Die Tail-Rekursion kann jetzt Funktionen mit Multiplen Werte richtig
;;; umwandeln.
;;;
;;; Revision 1.12  1993/12/02  12:33:48  atr
;;; Der Slot level soll natuerlich bei dem neu erzeugten Tagbody gesetzt
;;; werden und nicht bei der nei erzeugten tagged-form !!!
;;;
;;; Revision 1.11  1993/12/02  12:00:33  atr
;;; Der Slot Level bei der neu erzeugten Tagged-form *body-tag* gesetzt.
;;;
;;; Revision 1.10  1993/11/09  16:40:25  atr
;;; Die globalen Variablen stehen jetzt in se-init.lisp.
;;;
;;; Revision 1.9  1993/10/15  12:37:10  hk
;;; In den used-slot der generierten tagged-forms wird nun die Anzahl der
;;; angwandten Vorkommen eingetragen (statt 1), damit beim Wegoptimieren
;;; unbenutzter tagged-forms die Zählung durcheinander..
;;; In transform-form (switch-form) wurde vergessen, den Slot otherwise zu
;;; bearbeiten.
;;;
;;; Revision 1.8  1993/10/13  14:46:25  atr
;;; Methode transform-form f"ur CONT eingef"ugt.
;;;
;;; Revision 1.7  1993/10/13  11:23:41  atr
;;; Transform-form bei APP korrigiert.
;;; (any-thing-else T) --> (any-thing form)
;;;
;;; Revision 1.6  1993/09/14  13:15:24  atr
;;; Die Funktion make-a-twin-var erzeugt jetzt auch fuer eine dynamisch
;;; gebundene Variable eine  lokale lexikalisch gebundene Variable.
;;;
;;; Revision 1.5  1993/09/06  11:41:47  atr
;;; Jetzt werden lokale Funktionen analysiert, und eventuell die
;;; Tail-rekursion eliminiert.
;;;
;;; Revision 1.4  1993/08/16  16:46:16  hk
;;; (provide "tail") eingefügt, damit compile-clicc funktioniert
;;;
;;; Revision 1.3  1993/08/16  14:57:47  atr
;;; Nun entsteht nicht, wie bisher, eine setq-form fuer jede
;;; Resultatsform im Rumpf der Funktion, sondern nur eine Setq-form fuer den
;;; optimierten Rumpf der Funktion.
;;;
;;; Revision 1.2  1993/08/04  15:42:59  hk
;;; make-setq-form in assign-to-result umbenannt, da make-setq-form schon
;;; in zsdef definiert wurde.
;;;
;;; Revision 1.1  1993/08/04  09:55:56  atr
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------


(in-package "CLICC")

;;;--------------------------------------------------------------------------
(defun tail-rec-module ()
  (let ((*app-counter*    0)
        (*optimized-funs* 0))
    (clicc-message "Tail recursion elimination ...")
    (clicc-message "------------------------------")
    (mapc #'tail-rec-fun (?fun-list *module*))
    (clicc-message "~s tail recursive calls are eliminated"
                   *app-counter*)))
;;;--------------------------------------------------------------------------
;;; Die Tail-rekursion wird nur auf Funktionen angewendet, die 
;;; Tail-rekursive Aufrufe enthalten, keine Closures bilden und 
;;; keine Rest-Parameter haben.
;;;--------------------------------------------------------------------------
(defun tail-rec-fun (function)
  (let ((*current-function* function)
        (*static-level*     (if (global-fun-p function)
                                0
                                (?level function))))
    
    (when (and (not (generates-closures function))
               (has-tail-rec-calls function)
               (null (?rest (?params function))))
      (incf *optimized-funs*)
      (clicc-message "optimizing function ~s " (?symbol function))
      (setf (?body function) (transform-tail-rec-calls function)))
    (when (?local-funs *current-function*)

      ;; Die Level-Slots der lokalen Funktionen werden zunächst gesetzt, 
      ;; dann werden die Funktionen behandelt.
      ;;----------------------------------------------------------------
      (dolist (one-local-fun (?local-funs *current-function*))
        (setf (?level one-local-fun) (1+ *static-level*)))
      (mapc #'tail-rec-fun (?local-funs *current-function*)))))
    
  


;;;--------------------------------------------------------------------------
;;; Wenn die Funktion sich selbst aufruft, und zwar steht der Aufruf 
;;; auf Ergebisposition, dann kann der Aufruf zu einem Sprung 
;;; umgewandelt weden.
;;;--------------------------------------------------------------------------
(defun has-tail-rec-calls (function)
  (member function (get-applied-functions (?body function)) :test #'eq))


;;;--------------------------------------------------------------------------
;;; Der Rumpf der Funktion wird in ein Let/cc-Konstrukt umgewandelt.
;;; Dieses Let/cc-Konstrukt enth"alt einen Tagbody mit nur einem Tag. 
;;; In diesem Tag wird die Continuation auf den neuen Rumpf der Funktion,
;;; indem Tail-rec-Aufrufe durch Sprünge ersetzt werden, aufgerufen.
;;;--------------------------------------------------------------------------
(defun transform-tail-rec-calls (function)
  (let* ((new-body     (make-instance 'let/cc-form
                                      :cont (make-instance 'cont 
                                                           :read 1)))
         
         (new-tagbody  (make-instance 'tagbody-form
                                      :first-form empty-list
                                      :level (if (global-fun-p function)
                                                 0
                                                 (?level function))
                                      :tagged-form-list
                                      nil))
         (*body-tag*   (make-instance 'tagged-form 
                                      :form nil
                                      :tagbody  new-tagbody
                                      :used 0)))
    
    (incf *app-counter*)
    
    (setf (?form *body-tag*) (make-instance 'app
                                            :form (?cont new-body)
                                            :arg-list (list (transform-form 
                                                             (?body function)))))
    (setf (?tagged-form-list new-tagbody)
          (list *body-tag*))
    (setf (?body new-body) new-tagbody) 
    new-body))


;;;-------------------------------------------------------------------------
;;; Diese Funktion erzeugt fuer die Variable "VAR"eine neue lokale Variable, 
;;; die dafuer dient den Wert des Arguments, an dem die Variable "VAR" 
;;; gebunden ist aufzunehmen.
;;;-------------------------------------------------------------------------
(defun make-a-twin-var (var)
  (make-instance 'local-static 
                 :symbol (if (dynamic-p var)
                             (?symbol (?sym var))
                             (?symbol var))
                 :read 1
                 :write 1
                 :level (if (global-fun-p *current-function*)
                            0
                            (?level *current-function*))))

;;;-------------------------------------------------------------------------
;;; Fuer jeden formalen Parameter wird eine lokale Variable erzeugt.
;;; Die Liste der Paare (var . new-var) wird zurueckgegeben.
;;;-------------------------------------------------------------------------
(defun make-new-bindings-var ()
  (let ((var-list      (?all-vars (?params *current-function*)))
        (old-new-list  nil))
    (dolist (one-var var-list)
      (push (cons one-var (make-a-twin-var one-var)) old-new-list))
    (reverse old-new-list)))

;;;--------------------------------------------------------------------------
;;; Ein Tail-rekursiver Aufruf der Form (f arg1 ... argN) wird umgeformt
;;; in  (let ((local-arg1 arg1)
;;;                ...
;;;           (local-argN argN))
;;;         (setq P1 local-arg1)
;;;                ...
;;;         (setq PN local-argN))
;;;--------------------------------------------------------------------------
(defun make-a-tail-rec-call (a-app)
  (let ((init-list nil)
        (old-new-list (make-new-bindings-var))
        one-init-form
        one-par)
    
    
    ;; Abbilden der Argumentliste auf die Parameterliste.
    ;;----------------------------------------------------
    (dolist (one-pair old-new-list)
      (setq one-par (car one-pair))
      (setq one-init-form (get-arg-from-param one-par  a-app))
      (if  (and (var-ref-p one-init-form)
                (eql (?var one-init-form) one-par))
           (setf old-new-list (remove one-pair  old-new-list))
           (push one-init-form init-list)))
    (setf init-list (reverse init-list))
    (if (and (eql 1 (length old-new-list))
             (eql 1 (length init-list)))
        
        ;; Hier wird keine Let-form erzeugt, sonder nur eine Setq-form
        ;;------------------------------------------------------------
        (let* ((location (make-instance 'var-ref 
                                        :var (caar old-new-list)))
               (progn-f 
                (make-instance 'progn-form
                               :form-list 
                               (list
                                (make-instance 'setq-form
                                               :location location
                                               :form (car init-list))
                                *body-tag*))))
          (incf (?used *body-tag*))
          progn-f)
        ;; Erzeugen der Let-form.
        ;;-----------------------
        (let ((let-form (make-instance 'let*-form)))
          (setf (?var-list let-form)  (mapcar #'cdr old-new-list))
          (setf (?init-list let-form) init-list)
          
          ;; Der Rumpf der Funktion ist dann eine Sequenz von 
          ;; (SETQ varI local-varI) gefolgt von einem Sprung zum Anfang
          ;; der Funktion.
          ;;-----------------------------------------------------------
          (setf (?body let-form) 
                (make-instance 'progn-form 
                               :form-list 
                               (append 
                                (mapcar #'update-old-with-new old-new-list)
                                (list *body-tag*))))
          (incf (?used *body-tag*))
          let-form))))

;;;------------------------------------------------------------------------
;;; Bestimme ob das Argument geschlossen ist oder nicht.
;;; Also ob im Argument nur der entsp  Parameter gelesen wird .
;;;------------------------------------------------------------------------
(defun is-closed (one-par form)
  (let ((effect     (empty-effect))
        (loc-effect (make-instance 'local-effect)))
    (effect-of-form form effect loc-effect)
    (and (null (?write-list effect))
         (subsetp (?read-list loc-effect) (list one-par)))))
;;;------------------------------------------------------------------------
;;; Diese Funktion liefert fuer ein Paar (var . local-var) eine 
;;; setq-form (setq old var-ref.local-var).
;;;------------------------------------------------------------------------
(defun update-old-with-new (pair-of-twin-vars)
  (let* ((old     (make-instance 'var-ref 
                                 :var (car pair-of-twin-vars)))
         (new     (make-instance 'var-ref  
                                 :var (cdr pair-of-twin-vars))))
    
    (make-instance 'setq-form 
                   :location old
                   :form new)))

;;;------------------------------------------------------------------------
;;; dynamic-var-bound überprüft ob eine Liste von Variablen eine dynamische 
;;; Variable enthält.
;;;------------------------------------------------------------------------
(defun dynamic-var-bound (list-of-var)
  (if (null list-of-var)
      nil
      (if (dynamic-p (car list-of-var))
          T
          (dynamic-var-bound (cdr list-of-var)))))
;;;------------------------------------------------------------------------
;;; transform-Form traversiert den Rumpf der Funktion, und ersetzt 
;;; alle Ergebnis-formen durch Sequenzen von 
;;; 1 :einer Zuweisung an die  *result-var* (mit der result-form) 
;;; 2 :einem Sprung zum *body-tag*
;;; in dem Fall wo die Form kein rekursiver Aufruf ist.
;;; wenn die Result-form ein rekursiver Aufruf ist, dann wird er
;;; ersetzt durch eine Sequenz von Zuweisungen zum updaten der 
;;; formalen Parameter.
;;;-------------------------------------------------------------------------
(defmethod transform-form ((a-progn-form progn-form))
  (setf (car (last (?form-list a-progn-form)))
        (transform-form (car (last (?form-list a-progn-form)))))
  a-progn-form)

(defmethod transform-form ((a-let-form let*-form))
  (if (dynamic-var-bound (?var-list a-let-form))
      a-let-form
      (progn 
        (setf (?body a-let-form) (transform-form (?body a-let-form)))
        a-let-form)))

;;;--------------------------------------------------------------------------
;;; Bei der (?form a-setq) können keine rekursive Aufrufe durch Sprünge 
;;; ersetzt werden, weil die letzte Berechnung die Zuweisung an die Variable, 
;;; und nicht die Auswertung der (?form a-setq) ist.
;;;--------------------------------------------------------------------------
(defmethod transform-form ((a-setq-form setq-form))
  a-setq-form)

(defmethod transform-form ((a-switch-form switch-form))
  (dolist (one-case (?case-list a-switch-form))
    (setf (?form one-case)
          (transform-form (?form one-case))))
  (setf (?otherwise a-switch-form) (transform-form (?otherwise a-switch-form)))
  a-switch-form)

(defmethod transform-form ((a-let/cc-form let/cc-form))
  (setf (?body a-let/cc-form )
        (transform-form (?body a-let/cc-form )))
  a-let/cc-form)

(defmethod transform-form ((a-if-form if-form))
  (setf (?then a-if-form) (transform-form (?then a-if-form)))
  (setf (?else a-if-form) (transform-form (?else a-if-form)))
  a-if-form)

(defmethod transform-form ((a-app app))
  (let ((functional (?form a-app)))
    (if (fun-p functional)
        (if (eq functional *current-function*)
            (make-a-tail-rec-call a-app)
            a-app)
        a-app)))
            

(defmethod transform-form ((a-mv-lambda mv-lambda))
  (setf (?body a-mv-lambda)
        (transform-form (?body a-mv-lambda)))
  a-mv-lambda)

(defmethod transform-form ((a-labels labels-form))
  (setf (?body a-labels)
        (transform-form (?body a-labels)))
  a-labels) 

;;;------------------------------------------------------------------------
;;; die naechsten Formen liefern keine resultatswerte zuerueck 
;;;------------------------------------------------------------------------

(defmethod transform-form ((tagbody tagbody-form))
  tagbody)

(defmethod transform-form ((a-tagged-form tagged-form))
  a-tagged-form)

(defmethod transform-form ((a-labeled-form labeled-form))
  a-labeled-form)

(defmethod transform-form ((a-class-def class-def))
  a-class-def)

(defmethod transform-form ((a-cont cont))
  a-cont)

;;;------------------------------------------------------------------------
;;; Alle anderen Formen , die auf Ergebnisposition stehen, muessen durch
;;; eine Sequenz ersetzt werden :
;;; Kopieren dieser Form in *RESULT-VAR*.
;;; ein Sprung zum Ende der Funktion (also zum TAG "*body-tag*".
;;;-------------------------------------------------------------------------
(defmethod transform-form ((any-thing-else form))
  any-thing-else)

(defmethod transform-form ((a-var-ref var-ref))
  a-var-ref)

;;------------------------------------------------------------------------------
(provide "tail")
