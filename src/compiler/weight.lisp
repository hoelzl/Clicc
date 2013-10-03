;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Abschaetzung der zu erwartenden Codelaenge
;;;
;;; $Revision: 1.13 $
;;; $Log: weight.lisp,v $
;;; Revision 1.13  1994/06/22  07:47:38  hk
;;; Fehler in weight-eq und weight-eql behoben.
;;;
;;; Revision 1.12  1994/06/12  19:12:32  hk
;;; Fehler in weight-gen-classes behoben: vertauschte Zweige in einem
;;; if-Ausdruck bewirkten, da"s ein unzul"assiges strukturiertes Literal
;;; mit value = NIL entstand.
;;;
;;; Revision 1.11  1994/06/10  12:34:43  jh
;;; Für einfache Literale und Symbole wird jetzt auch auf die
;;; let-Optimierung vertraut.
;;;
;;; Revision 1.10  1994/06/09  16:02:09  jh
;;; Abschätzung, die auf die let-Optimierung hofft, eingebaut.
;;;
;;; Revision 1.9  1994/06/09  10:36:20  hk
;;; Anpassungen an cginline vorgenommen
;;;
;;; Revision 1.8  1994/04/05  15:13:01  jh
;;; Funktion export-weight definiert.
;;;
;;; Revision 1.7  1994/03/03  13:55:33  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.6  1994/01/06  17:29:35  sma
;;; Aufruf von opt-args um den Paramter fun ergänzt.
;;;
;;; Revision 1.5  1993/12/23  12:03:03  hk
;;; Variable *current-fun* wird gebunden wegen einer Änderung in opt-args.
;;;
;;; Revision 1.4  1993/12/16  18:14:22  pm
;;; Eigenen Fehler in weight-fun-def in weight-fun-def behoben
;;;
;;; Revision 1.3  1993/12/06  11:28:04  hk
;;; Fehler in weight-gen-classes behoben
;;;
;;; Revision 1.2  1993/11/18  14:40:50  pm
;;; Methode fuer weight-form ueber foreign-funs.
;;;
;;; Revision 1.1  1993/09/20  14:29:13  jh
;;; Initial revision
;;;

(in-package "CLICC")

(require "cgdefs")
(require "cgcode")
(require "cgvalues")
(require "p3main")

;;------------------------------------------------------------------------------
;; Hauptfunktionen zur Codelaengenabschaetzung
;;------------------------------------------------------------------------------

(defun weight-module ()
  (weight-codegen))

(defun weight-function (fun)
  (reset-weight-vars)
  (prog1
      (weight-defun fun)
    (make-closure-offset-slot-unbound fun)))

(defvar *weight-for-inlining* nil)

(defun inline-delta (fun app)
  (let* ((*weight-for-inlining* t)
         (inline-fun (?form app))
         (nargs (length (?arg-list app)))
         (simple-args (count-if #'(lambda (arg)
                                    (or (simple-literal-p arg)
                                        (sym-p arg)
                                        (and (var-ref-p arg)
                                             (local-static-p (?var arg)))))
                            (?arg-list app)))
         (params (?params inline-fun))
         (rest (?rest params))
         (nvars (length (?var-list params)))
         (nopt (length (?opt-list params)))
         (nparams (length (?all-vars params)))
         (nspecial (count-if #'dynamic-p (?all-vars params))))
    (reset-weight-vars)
    (+
     (if (and rest (local-static-p rest) (is-used rest))
         (let ((rest-length (- nargs (+ nvars nopt))))
           (cond
             ((zerop rest-length)
              3)                        ; rest an nil binden
             ((plusp rest-length)
              (+ 3 (* 4 rest-length)))  ; rest berechnen
             (t 0)))
         0)
     (- (* 4 (- nparams (if rest 1 0))) ; Parameter im neuen let binden
        (* 4 nargs)                     ; Argumente auf den Stack vorm Aufruf
        (* 4 (min simple-args (+ nopt nvars)))) ; Statische Variablen können
                                        ; meist aus dem let entfernt werden.
     (* 28 nspecial)                    ; Specialvars binden und restaurieren
     -3                                 ; Aufruf
     (let* ((global (or (global-fun-p fun)
                        (?as-global-fun fun)))
            (closure (and (not global)
                          (eq :closure (?closure fun))))
            (*stack-top* nparams)
            (*level* (if (local-fun-p fun) (?level fun) 0))
            (*special-count* 0)
            (*result-spec* (stacktop-result-location)))
       (weight-params params 0)
       (flet ((weight-gen-fun ()
                (let ((weight 0))
                  (incf weight (weight-form (?body inline-fun)))
                  (dolist (local-fun (?local-funs inline-fun))
                    (incf weight (weight-fun-def local-fun)))
                  weight)))
         (cond
           (closure (let ((*closure* fun)
                          (*cl-level* *level*))
                      (weight-gen-fun)))
           (global (let ((*cl-level* *level*))
                     (weight-gen-fun)))
           (t (weight-gen-fun))))))))

(defun export-weight (fun)
  (let* ((*current-fun* fun)
         (*weight-for-inlining* t)
         (params (?params fun))
         (nspecial (count-if #'dynamic-p (?all-vars params)))
         (*stack-top* (length (?all-vars params)))
         (*level* 0)
         (*special-count* 0)
         (*result-spec* (stacktop-result-location))
         (*cl-level* 0))
    (reset-weight-vars)
    (+
     (* 28 nspecial)                    ; Specialvars binden und restaurieren
     -3                                 ; Aufruf
     (- (* 4 (length (?var-list params))))
     (let ((weight 0))
       (weight-params params 0)
       (incf weight (weight-form (?body fun)))
       (dolist (local-fun (?local-funs fun))
         (incf weight (weight-fun-def local-fun)))
       weight))))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgmain
;;------------------------------------------------------------------------------

(defun reset-weight-vars ()
  (setq *copy-source* nil)
  (setq *special-count* 0)
  (setq *do-not-restore* ())
  (setq *C-bool* nil)
  (setq *cl-level* 0)
  (setq *closure* nil))

(defun weight-codegen ()
  (let ((weight 0))
    (reset-weight-vars)
    (unless *module-compiler*
      (incf weight (weight-C-DefMemSizes)))
    (incf weight (weight-gen-named-constants))
    (dolist (fun (?fun-list *module*))
      (incf weight (weight-defun fun)))
    (unless *inline-module*
      (incf weight (weight-defun (?toplevel-forms *module*)))
      (unless *module-compiler*
        (incf weight (weight-GC-function)))
      (incf weight (weight-gen-symbols))
      (when (?class-def-list *module*)
        (incf weight (weight-gen-classes))))
    (dolist (global-fun (?fun-list *module*))
      (make-closure-offset-slot-unbound global-fun))
    (make-closure-offset-slot-unbound (?toplevel-forms *module*))
    weight))

(defun make-closure-offset-slot-unbound (fun)
  ;; Die closure-offset-Slots der lokalen Funktionen muessen vor der Code-
  ;; generierung unbound sein.
  (dolist (local-fun (?local-funs fun))
    (slot-makunbound local-fun 'closure-offset)))

(defun weight-GC-function ()
  ;; Stackpointer retten, Register restaurieren, Ruecksprung und
  ;; call gc_symbols fuer die Symbole des Moduls
  (+ 6
     ;; call gc_symbols fuer die Symbole der geladenen Module
     (* 3 (length (?loaded-modules *module*)))))

(defmethod weight-form ((form progn-form))
  (let ((weight 0))
    (mapc-progn-form-list (?form-list form)
                          #'(lambda (form)
                              (let ((*result-spec* nil))
                                (incf weight (weight-form form))))
                          #'(lambda (form)
                              (incf weight (weight-form form))))
    weight))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgcode
;;------------------------------------------------------------------------------

(defun weight-C-Abort ()
  3)

(defun weight-C-Lispcall (nargs &optional display)
  (+ 2
     (if nargs 1 0)
     (if display 1 0)))

(defun weight-C-copy (w-source w-dest)
  (+ w-source w-dest 2))

(defun weight-C-goto ()
  1)

(defun weight-C-break ()
  1)

(defun weight-C-SetMV ()
  3)                                    ; gilt auch fuer C-ResetMV

(defun weight-C-if (w-pred)             ; gilt auch fuer C-flat-if
  (+ 2 w-pred))

(defun weight-C-else ()
  1)

(defun weight-C-switch (w-pred)
  w-pred)

(defun weight-C-case ()
  2)

(defun weight-C-MemUpMove ()
  15)

(defun weight-C-MVToStack ()
  21)

(defun weight-C-init-CL_FORM ()
  2)

(defun weight-CC-make-bool (w-loc)
  (1+ w-loc))

(defun weight-CC-bool ()
  1)

(defun weight-CC-Stack ()
  1)

(defun weight-CC-mv_buf ()
  2)

(defun weight-C-DefMemSizes ()
  8)

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgif
;;------------------------------------------------------------------------------

(defmethod weight-form ((form if-form))
  (if *FLAT-IFS*
      (weight-if-flat form)
      (weight-if      form)))

(defun weight-if-flat (form)
  (let ((weight 0))
    (let ((*result-spec* 'C-bool))
      (incf weight (weight-form (?pred form))))
    (let ((else-absent (and (null *result-spec*) (null-form-p (?else form)))))
      (if else-absent
          ;; Das Gewicht des Praedikats wurde bereits gezaehlt, deshalb 0.
          (incf weight (weight-C-if 0))
          (incf weight (weight-C-if 0)))
      (incf weight (weight-form (?then form)))
      (when (eq *result-spec* 'C-bool)
        (incf weight (weight-to-bool-result)))
      (unless else-absent
        (incf weight (weight-C-goto))
        (incf weight (weight-form (?else form)))
        (when (eq *result-spec* 'C-bool)
          (incf weight (weight-to-bool-result)))))
    (setq *C-bool* C-bool_result)
    weight))

(defun weight-if (form)
  (let ((weight 0))
    (let ((*result-spec* 'C-bool))
      (incf weight (weight-form (?pred form))))
    ;; Das Gewicht des Praedikats wurde bereits gezaehlt, deshalb 0.
    (incf weight (weight-C-if 0))
    (incf weight (weight-form (?then form)))
    (when (eq *result-spec* 'C-bool)
      (incf weight (weight-to-bool-result)))
    (unless (and (null *result-spec*)
                 (or (null-form-p (?then form)) (null-form-p (?else form))))
      (incf weight (weight-C-else)))
    (incf weight (weight-form (?else form)))
    (when (eq *result-spec* 'C-bool)
      (incf weight (weight-to-bool-result)))
    (setq *C-bool* C-bool_result)
    weight))
    
(defun weight-to-bool-result ()
  (if (not (string= C-bool_result *C-bool*))
      (progn
        (setq *C-bool* C-bool_result)
        1)
      0))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgconst
;;------------------------------------------------------------------------------

(defmethod weight-form ((form sym))
  (case *result-spec*
    ((nil) 0)
    (C-BOOL (setq *C-bool* nil) 0)
    (t (weight-C-symbol (weight-CC-dest *result-spec*)))))

(defmethod weight-form ((form class-def))
  (case *result-spec*
    ((nil) 0)
    (C-BOOL (setq *C-bool* nil) 0)
    (t (weight-C-class (weight-CC-dest *result-spec*)))))

(defmethod weight-form ((form simple-literal))
  (case *result-spec*
    ((nil) 0)
    (C-BOOL (setq *C-bool* nil) 0)
    (t (let ((w-dest (weight-CC-dest *result-spec*)))
         (etypecase form
           (null-form      (weight-C-nil w-dest))
           (character-form (weight-C-character w-dest))
           (int            (weight-C-integer w-dest))
           ;; Wenn der adr-Slot der float-form bereits gesetzt ist, wird
           ;; hier ein Wort zuviel gezaehlt.
           (float-form     (weight-C-float w-dest)))))))

(defmethod weight-form ((obj structured-literal))
  (case *result-spec*
    ((nil) 0)
    (C-BOOL (setq *C-bool* nil) 0)
    ;; LOAD_CONS/SMSTR/SMVEC_T/SMAR_T/STRUCT sind Zugriffe auf eine
    ;; Konstantentabelle und somit von der gleichen Qualitaet wie ein
    ;; Zugriff auf eine Symboltabelle LOAD_SYMBOL.
    (t (+ 4 (weight-CC-dest *result-spec*)))))

(defun weight-gen-literals (literals)
  (let (float-table
        fixnum-table
        (weight 0))
    (labels
        ((weight-gen-const-vector (elt)
           (let ((weight 0))
             (dotimes (i (length elt))
               (incf weight (weight-gen-immed (aref elt i))))
             (dotimes (i (length elt))
               (incf weight (weight-gen-const (aref elt i))))
             weight))
         
         (weight-gen-const (elt)
           (typecase elt
             (cons
              (let ((weight 0))
                (incf weight (weight-gen-immed (car elt)))
                (incf weight (weight-gen-immed (cdr elt)))
                (incf weight (weight-gen-const (car elt)))
                (incf weight (weight-gen-const (cdr elt)))
                weight))
             (string
              (weight-C-const-string elt))
             (vector
              (+ 2                      ; MAKE_VECTOR
                 (weight-gen-const-vector elt)))
             (array
              (let ((vector (make-array (array-total-size elt)
                                        :displaced-to elt
                                        :element-type
                                        (array-element-type elt))))
                (dolist (dim (array-dimensions elt))
                  (vector-push-extend dim fixnum-table))
                (+ 4                    ; MAKE_ARRAY
                   (weight-gen-const-vector vector))))
             (literal-instance
              (+ 4                      ; MAKE_INSTANCE
                 (weight-gen-const-vector (?value-list elt))))
             (t 0)))
         
         (weight-gen-immed (elt)
           (typecase elt
             (null-form      (weight-C-const-nil))
             (character-form (weight-C-const-character))
             (int            (weight-C-const-fixnum))
             (float-form     (float-address (?value elt))
                             (weight-C-const-float))
             (null           (weight-C-const-nil))
             (character      (weight-C-const-character))
             (sym            (weight-C-const-symbol))
             (integer        (weight-C-const-fixnum))
             (float          (float-address elt) (weight-C-const-float))
             (structured-literal
              (if (equalp (?value elt) "SECRET-UNBOUND-SLOT-VALUE")
                  (weight-C-const-unbound)
                  2))                   ; MAKE_VECREF/ARREF
             (class-def      (weight-C-const-class))
             (global-fun     (weight-C-const-fun))
             (T 2)))                    ; MAKE_??

         ;; float-address wird nur fuer die Pflege der float-table benoetigt.
         (float-address (float)
           (unless (position float float-table)
             (vector-push-extend float float-table)))
         (weight-floats ()
           (if (and float-table (> (length float-table) 0))
               (* 2 (length float-table))
               0))
         (weight-fixnums ()
           (if (and fixnum-table (> (length fixnum-table) 0))
               (length fixnum-table)
               0)))
      (when literals
        (when (dolist (const literals nil)
                (when (and (structured-literal-p const)
                           (?needs-fixnum-array const))
                  (return t)))
          (setq fixnum-table (make-array 1 :fill-pointer 0 :adjustable t)))
        (when (dolist (const literals nil)
                (when (or (float-form-p const)
                           (?needs-float-array const))
                  (return t)))
          (setq float-table (make-array 1 :fill-pointer 0 :adjustable t)))
        (dolist (struct literals)
          (etypecase struct
            (float-form (float-address (?value struct)))
            (structured-literal
             (let ((elt (?value struct)))
               (incf weight (weight-gen-const elt))))))
        (incf weight (weight-floats))
        (incf weight (weight-fixnums))))
    weight))
        


(defun weight-C-const-nil ()
  2)

(defun weight-C-const-t ()
  2)

(defun weight-C-const-unbound ()
  2)

(defun weight-C-const-fixnum ()
  2)

(defun weight-C-const-float ()
  2)

(defun weight-C-const-character ()
  2)

(defun weight-C-const-symbol ()
  2)
 
(defun weight-C-const-string (string)
  (+ 4 (ceiling (length string) 4)))
   
(defun weight-C-float (w-dest)
  (+ 2                                  ; float
     3 w-dest))                         ; LOAD_FLOAT

(defun weight-C-string (string w-dest)
  (+ 2 (ceiling (length string) 4)      ; CL_FORM + string
     3 w-dest))                         ; LOAD_SMSTR

(defun weight-CC-symbol ()
  2)

(defun weight-CC-special ()
  3)

(defun weight-C-symbol (w-dest)
  (+ 4 w-dest))

(defun weight-C-nil (w-dest)
  (+ 2 w-dest))

(defun weight-C-t (w-dest)
  (+ 4 w-dest))

(defun weight-C-integer (w-dest)
  (+ 3 w-dest))

(defun weight-C-character (w-dest)
  (+ 3 w-dest))

(defun weight-C-const-class ()
  2)

(defun weight-C-class (w-dest)
  (+ 4 w-dest))

(defun weight-C-const-fun ()
  2)

(defun weight-gen-symbols ()
  (+
   (weight-gen-literals
    (mapcan #'(lambda (sym)
                (let ((value (?constant-value sym)))
                  (when (constant-value-p sym)
                    (typecase value
                      ((or float-form structured-literal) (list value))
                      (T nil)))))
            (?sym-list *module*)))
   ;; Berechnung des Gewichts der Namenstrings
   (let ((weight 0))
     (dolist (sym (?sym-list *module*))
       (incf weight (ceiling (length (?name sym)) 4)))
     weight)
   ;; Eine Symbolbeschreibung besteht aus 6 CL_FORM, also 12 Worten.
   (* 12 (length (?sym-list *module*)))
   1))                                  ; die abschliessende 0 fuer den GC.

(defun weight-gen-classes ()
  (+
   (weight-gen-literals
    (mapcan #'(lambda (class)
                (if (?slot-descr-list class)
                    `(,(?class-precedence-list class)
                      ,(make-instance 'structured-literal
                        :value (mapcar #'get-rt-slot-info
                                (?slot-descr-list class))
                        :needs-fixnum-array nil
                        :needs-float-array nil))
                    `(,(?class-precedence-list class))))
            (?class-def-list *module*)))
   ;; Eine Klassenbeschreibung ist besteht aus 6 CL_FORM, also 12 Worten.
   (* 12 (length (?class-def-list *module*)))))

(defun weight-gen-named-constants ()
  (weight-gen-literals
   (mapcan #'(lambda (const)
               (let ((value (?value const)))
                 (unless (eq :unknown value)
                   (typecase value
                     ((or float-form structured-literal) (list value))
                     (T nil)))))
           (?named-const-list *module*))))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cg-vars
;;------------------------------------------------------------------------------

(defun weight-params (params known-number-of-args)
  (let* ((nsimple      (length (?var-list params)))
         (nopt         (length (?opt-list params)))
         (nkey         (length (?key-list params)))
         (nsimpopt     (+ nsimple nopt))
         (rest         (?rest params))
         nopt-supplied
         suppl-index
         first-key
         w-C-stacktop
         (weight 0))
    (setq w-C-stacktop (if (not known-number-of-args)
                           1
                           0))
    
    ;; normale Parameter
    (dolist (simple (?var-list params))
      (incf weight (weight-var-bind simple *stack-top* w-C-stacktop))
      (incf *stack-top*))

    ;; optionale und opt-suppliedp Parameter
    (when (> nopt 0)
      (setq nopt-supplied
            (count-if-not #'null (?opt-list params)
                          :key #'?suppl))
      (when (> nopt-supplied 0)
        (incf weight (weight-C-MemUpMove))
        (dotimes (i nopt-supplied)
          (incf weight (weight-C-nil (weight-CC-Stack)))
          (incf *stack-top*))
        (incf weight)                   ; nargs = nargs + nopt-supplied
        (incf nsimple nopt-supplied)
        (incf nsimpopt nopt-supplied))
      (when (or (> nopt-supplied 0)
                (find-if #'(lambda (x) (or (dynamic-p x)
                                           (and (local-static-p x)
                                                (?closure x))))
                         (?opt-list params)
                         :key #'?var))
        ;; if (nargs>nsimpopt) goto ALL_OPT_SUPPLIED
        (incf weight 3)
        (setq suppl-index *stack-top*)
        (incf weight (weight-C-Switch 0))
        (let ((i nopt)
              (*special-count* *special-count*)
              (*do-not-restore* *do-not-restore*))
          (dolist (opt-spec (reverse (?opt-list params)))
            (when (or (?suppl opt-spec)
                      (dynamic-p (?var opt-spec))
                      (and (local-static-p (?var opt-spec))
                           (?closure (?var opt-spec))))
              (incf weight (weight-C-case))
              (incf weight (weight-var-bind (?var opt-spec) (+ *stack-top* i -1)
                                            w-C-stacktop))
              (when (?suppl opt-spec)
                (decf suppl-index)
                (incf weight (weight-C-t (weight-CC-Stack)))
                (incf weight (weight-var-bind (?suppl opt-spec) suppl-index
                                              w-C-stacktop))))
            (decf i))))
      (incf weight (weight-C-Switch 0))
      (let ((*result-spec* (stacktop-result-location))
            (i 0))
        (dolist (opt-spec (?opt-list params))
          (incf weight (weight-C-case))
          (incf weight (weight-form (?init opt-spec)))
          (incf weight (weight-var-bind (?var opt-spec) *stack-top*))
          (incf *stack-top*)
          (incf (?offset *result-spec*))
          (when (?suppl opt-spec)
            (incf weight (weight-var-bind (?suppl opt-spec) suppl-index))
            (incf suppl-index))
          (incf i)))
      (cond
        ((or (> nkey 0) rest)
         (incf weight 1))
        (T
         (incf weight (weight-C-case))
         (incf weight (weight-C-break))
         (incf weight (weight-C-case))  ; defaultcase
         (incf weight (weight-C-Abort)))))
    (when (> nkey 0)
      (incf weight (length (?key-list params))) ; keylist[]
      (incf weight 8)                   ; call keysort
      (setq first-key *stack-top*)
      (incf *stack-top* nkey))
    (when (and rest (or (plusp (?write rest)) (plusp (?read rest))))
      ;; Aufruf von FList (ein Aufruf unter Angabe von nargs)
      (incf weight (weight-C-Lispcall t))
      (incf weight (weight-var-bind rest *stack-top*))
      (incf *stack-top*))
    (when (> nkey 0)
      (let ((*result-spec* (stack-location first-key *level*))
            (i 0))
        (dolist (key-spec (?key-list params))
          (when (or (?init key-spec) (?suppl key-spec))
            (incf weight (weight-C-if 1))
            (when (?init key-spec)
              (incf weight (weight-form (?init key-spec))))
            (when (?suppl key-spec)
              (incf weight (weight-C-nil (weight-CC-Stack)))
              (incf weight (weight-C-else))
              (incf weight (weight-C-t (weight-CC-Stack)))
              (incf weight (weight-var-bind (?suppl key-spec) *stack-top*))
              (incf *stack-top*)))
          (incf weight (weight-var-bind (?var key-spec)
                                        (?offset *result-spec*)))
          (incf (?offset *result-spec*))
          (incf i))))
    weight))

(defmethod weight-form ((form let*-form))
  (let ((old-stack *stack-top*)
        (old-special *special-count*)
        (vars (?var-list form))
        (forms (?init-list form))
        (*result-spec* (stacktop-result-location))
        (forms-weight 0))
    (loop (unless vars (return))
          (incf forms-weight (weight-form (pop forms)))
          (incf forms-weight (weight-var-bind (pop vars) *stack-top*))
          (incf (?offset *result-spec*))
          (incf *stack-top*))
    (+ forms-weight
       (prog1
           (weight-form (?body form))
         (setq *stack-top* old-stack))
       (weight-C-restore-special old-special))))
    
(defmethod weight-form ((form var-ref))
  (let ((var (?var form)))
    (cond
      ((and (eq *result-spec* 'C-bool) (dynamic-p var) (> *special-count* 0))
       (weight-CC-make-bool (weight-CC-special)))
      (t (weight-to-result-loc var)))))

(defmethod weight-form ((form setq-form))
  (let ((var (?var (?location form))))
    (+
     (let ((*result-spec*
            (if (and (local-static-p var)
                     (eql (?offset var) (1- *stack-top*))
                     (if (>= (?level var) *level*)
                         (progn ;(clicc-message "setq-levels: ~S ~D ~D"
                                ;               *weight-for-inlining*
                                ;               (?level var)
                                ;               *level*)
                                t)
                         nil)
                     (not (?closure var)))
                (stacktop-result-location (?offset var))
                var)))
       (weight-form (?form form)))
     (weight-to-result-loc var))))

(defmethod weight-form ((form defined-named-const))
  (let ((value (?value form)))
    (if (eq :unknown value)
        (progn
          (internal-error
           'weight-form
           "Named constants with unknown values are not implemented: ~s"
           (?symbol form))
          0)
        (if (or (simple-literal-p value)
            (sym-p value)
            (structured-literal-p value))
            (weight-form value)
            0))))

(defmethod weight-form ((form imported-named-const))
  (case *result-spec*
    ((nil) 0)
    (C-BOOL (setq *C-bool* nil) 0)
    ;; LOAD_CONS/SMSTR/SMVEC_T/SMAR_T/STRUCT sind Zugriffe auf eine
    ;; Konstantentabelle und somit von der gleichen Qualitaet wie ein
    ;; Zugriff auf eine Symboltabelle LOAD_SYMBOL.
    (t (+ 4 (weight-CC-dest *result-spec*)))))

(defmethod weight-var-bind ((var local-static) offset
                            &optional (w-C-stacktop 0))
  (setf (?offset var) offset)
  (if (?closure var)
      (+ 11 w-C-stacktop)               ; fuer GEN_HEAPVAR
      0))

(defmethod weight-var-bind ((var dynamic) offset &optional (w-C-stacktop 0))
  (declare (ignore offset))
  (declare (ignore w-C-stacktop))
  (when (and *result-spec* (equal-loc *result-spec* var))
    (push *special-count* *do-not-restore*))
  (incf *special-count*)
  20)                                   ; fuer BIND-SPECIAL

(defun weight-C-restore-special (old-special)
  (let ((weight 0))
    (do ()
        ((= *special-count* old-special))
      (decf *special-count*)
      (cond
        ((eql (car *do-not-restore*) *special-count*)
         (pop *do-not-restore*)
         (incf weight 4))               ; fuer POP_SPECIAL
        (t (incf weight 9))))          ; fuer RESTORE-SPECIAL
    weight))

(defun weight-C-restore-special2 (old-special)
  (let ((weight 0))
    (do ((i *special-count*)
         (l *do-not-restore*))
        ((= i old-special))
      (decf i)
      (cond
        ((eql (car l) i)
         (pop l)
         (incf weight 4))               ; fuer POP_SPECIAL
        (t (incf weight 9))))          ; fuer RESTORE_SPECIAL
    weight))

(defun weight-CC-static (static)
  (let ((level (?level static)))
    (cond
      ((not (?closure static)) (weight-CC-frame-access level))
      ((< level *cl-level*) (weight-CC-clos-heap))
      (t (weight-CC-stack-heap level)))))

(defun weight-CC-frame-access (level)
  (if (if (<= *level* level)
          (progn
            ;(clicc-message "frame-access: ~S ~D ~D"
            ;               *weight-for-inlining*
            ;               *level*
            ;               level)
            t)
          nil)
      (weight-CC-Stack)
      2))                               ; Zugriff auf das display-array

(defun weight-CC-stack-heap (level)
  (1+ (weight-CC-frame-access level)))

(defun weight-CC-clos-heap ()
  (1+ (weight-CC-heapenv)))

(defun weight-CC-closure-access (w-closure)
  (1+ w-closure))

(defun weight-CC-heapenv ()
  (weight-CC-closure-access (weight-CC-frame-access *cl-level*)))

(defun weight-stacktop-to-result-loc ()
  (weight-to-result-loc (stacktop-location)))

(defun weight-to-result-loc (loc)
  (case *result-spec*
    ((NIL) 0)
    (C-BOOL (setq *C-bool* nil) (weight-CC-make-bool (weight-CC-dest loc)))
    (T (weight-copy loc *result-spec*))))

(defun weight-CC-dest (loc)
  (typecase loc
    (static (weight-CC-static loc))
    (dynamic (weight-CC-special))
    (mv-buf (weight-CC-mv_buf))
    (T (error "unexpected location ~S" loc))))

(defun weight-copy (source dest)
  (if (or (equal-loc source dest)
          (and *copy-source*
               (equal-loc source *copy-dest*)
               (equal-loc dest *copy-source*)))
      0                                 ; nicht kopieren
      (progn
        (setq *copy-source* source *copy-dest* dest)
        (weight-C-copy (weight-CC-dest source) (weight-CC-dest dest)))))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgfuns
;;------------------------------------------------------------------------------

(defun weight-defun (fun)
  (+
   (if (?closure fun)
       (weight-C-global-closure)
       0)
   (weight-gen-literals (?const-list fun))
   (weight-fun-def fun)))

(defun weight-fun-def (fun)
  (let* ((*current-fun* fun)            ; für opt-args
         (global (or (global-fun-p fun)
                     (?as-global-fun fun)))
         (closure (and (not global)
                       (eq :CLOSURE (?closure fun))))
         (*stack-top* 0)
         (*level* (if (local-fun-p fun) (?level fun) 0))
         (old-special *special-count*)
         (*result-spec* (cond
                          ((eq :JUMP (?mv-spec fun)) nil)
                          (closure (stacktop-location))
                          (t (stacktop-result-location)))))
    (flet ((weight-gen-fun ()
             (let ((weight 0))
               (when (?local-funs fun)
                 (dolist (local (?local-funs fun))
                   (when (and (?as-global-fun local)
                              (?closure local))
                     (incf weight (weight-C-global-closure)))))
               (incf weight (weight-C-fun-entry))
               (when closure
                 (incf *stack-top*))
               (incf weight (weight-params (?params fun)
                                           (>= (?par-spec fun) 0)))
               (incf weight (weight-form (?body fun)))
               (incf weight (weight-C-restore-special old-special))
               (dolist (local (?local-funs fun))
                 (incf weight (weight-fun-def local)))
               weight)))
      (cond
        (closure (let ((*closure* fun)
                       (*cl-level* *level*))
                   (weight-gen-fun)))
        (global
         (+ (weight-gen-fun)
            (if (and (global-fun-p fun)
                     (?call-in fun))
                (weight-call-in-interface fun)
                0)))
        (t (weight-gen-fun))))))

(defun weight-C-global-closure ()
  2)

;; Stackpointer retten, Register restaurieren und Ruecksprung
(defun weight-C-fun-entry ()
  3)

(defmethod weight-form ((form labels-form))
  (let ((old-stack *stack-top*)
        (funs (?fun-list form))
        (weight 0))
    (dolist (fun funs)
      (unless (?as-global-fun fun)
        (when (eq :CLOSURE (?closure fun))
          (setf (?num-free-lex-vars fun) (length (?free-lex-vars fun)))
          (incf weight (weight-C-gen-closure fun (weight-CC-Stack)))
          (setf (?closure-offset fun) *stack-top*)
          (incf *stack-top*))))
    (dolist (fun funs)
      (unless (?as-global-fun fun)
        (when (eq :CLOSURE (?closure fun))
          (dolist (free-fun (?free-local-funs fun))
            (when (and (= (?level free-fun)
                          (?level fun))
                       (> (?closure-offset free-fun)
                          (?closure-offset fun)))
              (incf weight
                    (weight-C-copy
                     (weight-CC-closure fun)
                     (weight-CC-closure-access (weight-CC-closure fun)))))))))
    (incf weight (weight-form (?body form)))
    (setq *stack-top* old-stack)
    weight))

(defun weight-C-gen-closure (fun w-dest)
  (let ((weight 0))
    (setf (?free-local-funs fun)
          (delete-if #'?as-global-fun
                     (?free-local-funs fun)))
    (incf weight 3)                     ; call form_alloc
    (incf weight (weight-C-integer 1))
    (incf weight 5)                     ; LOAD_CODE
    (incf weight (weight-C-integer 1))
    (dolist (var (?free-lex-vars fun))
      (incf weight (weight-C-copy
                    (if (< (?level var) *cl-level*)
                        (weight-CC-heapenv)
                        (weight-CC-frame-access (?level var)))
                    1)))
    (dolist (fun (?free-local-funs fun))
      (incf weight
            (if (slot-boundp fun 'closure-offset)
                (weight-C-copy (weight-CC-closure fun) 1)
                (weight-C-nil 1))))
    (incf weight (+ 2 w-dest))          ; LOAD_CLOSURE
    weight))

(defun weight-CC-closure (fun)
  (cond
    ((eq fun *closure*)
     (weight-CC-Stack))
    ((<= (?level fun) *cl-level*)
     (weight-CC-heapenv))
    (t
     (weight-CC-frame-access (1- (?level fun))))))

(defmethod weight-form ((fun imported-fun))
  (case *result-spec*
    ((nil) 0)
    (C-bool (setq *C-bool* nil) 0)
    (t (let ((w-dest (weight-CC-dest *result-spec*)))
         (+ 2                           ; C-Static-GLOBAL_FUNARG-Init
            4 w-dest)))))               ; LOAD_GLOBFUN

;;------------------------------------------------------------------------------
;; *** Welcher Wert muss hier hin??? ***
;;------------------------------------------------------------------------------
(defmethod weight-form ((fun foreign-fun))
  4)

;;------------------------------------------------------------------------------
(defmethod weight-form ((fun defined-fun))
  (case *result-spec*
    ((nil) 0)
    (C-bool (setq *C-bool* nil) 0)
    (t (let ((w-dest (weight-CC-dest *result-spec*)))
         (if (or (global-fun-p fun)
                 (?as-global-fun fun))
             (+ 4 w-dest)               ; LOAD_GLOBFUN
             (case (?closure fun)
               (:closure (weight-C-copy (weight-CC-closure fun) w-dest))
               (:downfun (incf *downfun-count*)
                         (+ 4 w-dest))  ; LOAD_DOWNFUN
               (t 0)))))))

(defun weight-args (args par-spec)
  (let ((*result-spec* (stacktop-result-location))
        (old-stack *stack-top*)
        (weight 0))
    (dolist (arg args)
      (incf weight (weight-form arg))
      (incf *stack-top*)
      (incf (?offset *result-spec*)))
    (multiple-value-prog1
        (values weight (if (>= par-spec 0) nil (- *stack-top* old-stack)))
      (setq *stack-top* old-stack))))

(defmethod weight-form ((app app))
  (weight-app (?form app) (?arg-list app) app))

(defmethod weight-app ((fun special-sys-fun-with-mv) args app)
  (if (?weight-c-inline fun)
      (let ((*stack-top* *stack-top*))
        (funcall (?weight-c-inline fun) args app))
      (call-next-method)))

(defmethod weight-app ((fun special-sys-fun) args app)
  (declare (ignore app))
  (if (?weight-c-inline fun)
      (let ((*stack-top* *stack-top*))
        (apply (?weight-c-inline fun) args))
      (call-next-method)))

(defun weight-C-save-base ()
  1)

(defmethod weight-app ((fun imported-fun) args app)
  (let* ((weight 0)
         w-args
         nargs
         (old-stack *stack-top*)
         (*downfun-count* 0))
    (multiple-value-bind (w-downfuns save-base) (weight-downfuns app)
      (incf weight w-downfuns)
      (opt-args args fun)               ; veraendert *stack-top* !
      (multiple-value-setq (w-args nargs) (weight-args args (?par-spec fun)))
      (incf weight w-args)
      (when save-base
        (incf weight (weight-C-save-base)))
      (incf weight (weight-C-Lispcall nargs))
      (unless (eq (?mv-spec fun) :JUMP)
        (unless (or (eql (?mv-spec fun) 1) (?mv-used app))
          (incf weight (weight-C-SetMV)))
        (incf weight (weight-stacktop-to-result-loc)))
      (setq *stack-top* old-stack))
    weight))

(defmethod weight-app ((fun defined-fun) args app)
  (let* ((weight 0)
         w-args
         nargs
         level
         (old-stack *stack-top*)
         (closure (and (local-fun-p fun)
                       (eq :CLOSURE (?closure fun))
                       (not (?as-global-fun fun))))
         (*downfun-count* 0))
    (multiple-value-bind (w-downfuns save-base) (weight-downfuns app)
      (incf weight w-downfuns)
      (opt-args args fun)               ; veraendert *stack-top* !
      (when closure
        (incf weight (weight-C-copy (weight-CC-closure fun) (weight-CC-Stack)))
        (incf *stack-top*))
      (multiple-value-setq (w-args nargs) (weight-args args (?par-spec fun)))
      (incf weight w-args)
      (when save-base
        (incf weight (weight-C-save-base)))
      (cond
        ((or (not (local-fun-p fun))
             (?as-global-fun fun)
             closure)
         (incf weight (weight-C-Lispcall nargs))
         (when closure
           (decf *stack-top*)))
        ((> (setq level (?level fun)) *level*)
         (incf weight (weight-C-save-base))
         (incf weight (weight-C-Lispcall nargs C-display)))
        ((or (= level *level*)
             (and (eql 0 (?max-level fun)) (< level *level*)))
         (incf weight (weight-C-Lispcall nargs C-display)))
        ((< level *level*)
         ;; von display nach new_display kopieren
         (incf weight (* (- level *cl-level*) 2))
         (incf weight (weight-C-Lispcall nargs C-new_display))))
      (unless (eq (?mv-spec fun) :JUMP)
        (unless (or (eql (?mv-spec fun) 1) (?mv-used app))
          (incf weight (weight-C-SetMV)))
        (incf weight (weight-stacktop-to-result-loc)))
      (setq *stack-top* old-stack))
    weight))

(defmethod weight-app (exp args app)
  (let* ((weight 0)
         w-args
         nargs
         (old-stack *stack-top*)
         (*downfun-count* 0))
    (multiple-value-bind (w-downfuns save-base) (weight-downfuns app)
      (incf weight w-downfuns)
      (push exp args)
      (opt-args args)
      (multiple-value-setq (w-args nargs) (weight-args args -2))
      (incf weight w-args)
      (when save-base (incf weight (weight-C-save-base)))
      (incf weight (weight-C-Lispcall nargs)) ; funcall
      (unless (?mv-used app)
        (incf weight (weight-C-SetMV)))
      (incf weight (weight-stacktop-to-result-loc))
      (setq *stack-top* old-stack))
    weight))

(defun weight-downfuns (app)
  (let ((weight 0)
        (downfuns (?downfun-list app))
        (save-base nil))
    (when downfuns
      (dolist (downfun downfuns)
        (when (and (not (?as-global-fun downfun))
                   (eq :DOWNFUN (?closure downfun)))
          (incf *downfun-count*)))
      (when (> *downfun-count* 0)
        (dolist (downfun downfuns)
          (when (and (not (?as-global-fun downfun))
                   (eq :DOWNFUN (?closure downfun)))
            (let ((level (?level downfun)))
              (incf weight 4)           ; 2 * C-Assign
              (cond
                ((> level *level*)
                 (incf weight)          ; C-Assign
                 (setq save-base t))
                ((or (= level *level*)
                     (and (eql 0 (?max-level downfun))
                          (< level *level*)))
                 (incf weight))         ; C-Assign
                ((< level *level*)
                 ;; von display nach new_display? kopieren
                 (incf weight (* 2 (- level *cl-level*)))
                 (incf weight))))))     ; C-Assign
        (setq *downfun-count* 0)))
    (values weight save-base)))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgblock
;;------------------------------------------------------------------------------

(defmethod weight-form ((form let/cc-form))
  (let ((weight 0)
        (cont (?cont form)))
    (unless (?only-local cont)
      (incf weight 6)                   ; 2 * C-Assign
      (incf weight 7)                   ; LOAD_UNIQUE_TAG
      (incf weight (weight-var-bind cont *stack-top*))
      (incf *stack-top*)
      (incf weight 2)                   ; C-Assign
      (incf weight (weight-C-if 0))
      (incf weight 3))                  ; C-Assign
    (setf (?binding-stack-level cont) *special-count*)
    (setf (?result-spec cont) *result-spec*)
    (incf weight (weight-form (?body form)))
    (when (eq *result-spec* 'C-bool)
      (incf weight (weight-to-bool-result)))
    (unless (?only-local cont)
      (incf weight 2)                   ; C-Assign
      (incf weight (weight-C-else))
      (incf weight 3)                   ; C-Assign
      ;; EQ macht zusaetzlich zwei Lade-, einen Vergleichs- und einen
      ;; Sprungbefehl notwendig.
      (incf weight (weight-C-if (+ 4 (weight-CC-Stack)
                                   (weight-CC-static cont))))
      (incf weight
            (case *result-spec*
              ((nil) 0)
              (C-bool (setq *C-bool* C-bool_result)
                      (weight-CC-make-bool (weight-CC-Stack)))
              (t (weight-C-copy (weight-CC-Stack)
                                (weight-CC-dest *result-spec*)))))
      (incf weight (weight-C-else))
      (incf weight 2)                   ; call_cont
      (decf *stack-top*))
    weight))

(defmethod weight-app ((cont cont) args app)
  (declare (ignore app))
  (let ((weight 0))
    (cond
      ((eql *level* (?level cont))
       (let ((*result-spec* (?result-spec cont)))
         (when (not (eql (length args) 1))
           (when (null args)
             (push empty-list args)))
         (incf weight (weight-form (first args)))
         (when (eq *result-spec* 'C-bool)
           (incf weight (weight-to-bool-result))))
       (incf weight (weight-C-restore-special2 (?binding-stack-level cont)))
       (incf weight (weight-C-goto)))
      (t
       (incf weight (weight-C-copy (weight-CC-static cont) (weight-CC-stack)))
       (incf *stack-top*)
       (let ((*result-spec* (stacktop-location)))
         (incf weight (weight-form (first args))))
       (decf *stack-top*)
       (incf weight 2)))                ; call_cont
    weight))

(defmethod weight-form ((form cont))
  (weight-to-result-loc form))

(defmethod weight-form ((form tagbody-form))
  (let ((weight 0))
    (setf (?binding-stack-level form) *special-count*)
    (let ((*result-spec* nil))
      (incf weight (weight-form (?first-form form))))
    (mapc-progn-form-list (?tagged-form-list form)
                          #'(lambda (tagged-form)
                              (let ((*result-spec* nil))
                                (incf weight (weight-form
                                              (?form tagged-form)))))
                          #'(lambda (tagged-form)
                              (incf weight (weight-form (?form tagged-form)))))
    weight))

(defmethod weight-form ((form tagged-form))
  (+ (weight-C-restore-special2 (?binding-stack-level (?tagbody form)))
     (weight-C-goto)))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgvalues
;;------------------------------------------------------------------------------

(defmethod weight-form ((form mv-lambda))
  (let* ((weight 0)
         (old-stack *stack-top*)
         (params (?params form))
         (par-spec (calc-par-spec params))
         (mv-spec (?mv-spec form)))
    (let ((*result-spec* (stacktop-result-location)))
      (incf weight (weight-form (?arg form))))
    (cond
      ((numberp mv-spec)
       (unless (= 0 mv-spec)
         (incf *stack-top*)
         (incf weight (* (1- mv-spec) (weight-C-copy (weight-CC-mv_buf)
                                                     (weight-CC-Stack))))
         (incf *stack-top* (1- mv-spec)))
       (cond
         ((>= par-spec 0) nil)
         (t (incf weight)))             ; C-Assign
       (unless (= 1 mv-spec)
         (incf weight (weight-C-SetMV))))
      (t (incf *stack-top*)
         (incf weight (weight-C-MVToStack))
         (cond
           ((>= par-spec 0)
            (incf weight (weight-C-if 1))
            (incf weight (weight-C-Abort)))
           (t
            (incf weight 2)             ; C-Assign
            (let ((min (1- (- par-spec))))
              (when (> min 0)
                (incf weight (weight-C-if 1))
                (incf weight (weight-C-Abort))))))
         (incf weight (weight-C-SetMV))))
    (setq *stack-top* old-stack)
    (incf weight (weight-params params (>= (calc-par-spec params) 0)))
    (incf weight (weight-form (?body form)))
    (setq *stack-top* old-stack)
    weight))
    
(defun weight-values (arg-list app &aux (mv-count (length arg-list)))
  (let ((weight 0))
    (if (or (not (?mv-used app)) (eql mv-count 1))
        (case mv-count
          (0 (incf weight (weight-form empty-list)))
          (1 (incf weight (weight-form (first arg-list))))
          (t (incf weight (weight-args arg-list -1))
             (incf weight (weight-stacktop-to-result-loc))))
        (case mv-count
          (0 (incf weight (weight-form empty-list))
             (incf weight (weight-C-SetMV)))
          (t (when (> mv-count MV-LIMIT)
               (setq mv-count MV-LIMIT))
             (incf weight (weight-args arg-list -1))
             (incf weight (weight-stacktop-to-result-loc))
             (incf weight (* (1- mv-count)
                             (weight-C-copy (weight-CC-Stack)
                                            (weight-CC-mv_buf))))
             (incf weight (weight-C-SetMV)))))
    weight))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cginline
;;------------------------------------------------------------------------------

(p0-special-funs
 (?weight-c-inline "WEIGHT")
 clicc-lisp::eq
 clicc-lisp::eql
 clicc-lisp::integerp
 rt::fixnum-low-p
 rt::fixnum-high-p
 clicc-lisp::consp
 clicc-lisp::characterp
 rt::plain-vector-p
 clicc-lisp::simple-vector-p
 clicc-lisp::simple-string-p
 clicc-lisp::simple-bit-vector-p
 clicc-lisp::floatp
 clicc-lisp::atom
 clicc-lisp::symbolp
 rt::symp
 clicc-lisp::listp
 clicc-lisp::numberp
 clicc-lisp::functionp
 clicc-lisp::values
 clicc-lisp::cons
 rt::%car
 rt::%cdr
 rt::%rplaca
 rt::%rplacd
 rt::%logior
 rt::%logxor
 rt::%logand
 rt::%lognot
 rt::%shift-right
 rt::%shift-left
 rt::instancep
 rt::instance-ref
 rt::instance-set
 rt::plain-vector-length
 rt::svref-internal
 rt::set-svref-internal
 rt::symbol-value
 rt::symbol-plist
 rt::symbol-package
 (L::setf rt::symbol-value)
 (L::setf rt::symbol-plist)
 (L::setf rt::symbol-package)
 rt::structp
 rt::struct-size
 rt::structure-ref
 (L::setf rt::structure-ref))

(defun weight-get-arg-loc (form)
  (if (var-ref-p form)
      (values (?var form) 0)
      (let ((*result-spec* (stacktop-result-location))
            (weight (weight-form form)))
        (incf *stack-top*)
        (values *result-spec* weight))))

(defun weight-CC-get-arg (form)
  (multiple-value-bind (arg-loc weight) (weight-get-arg-loc form)
    (+ (weight-CC-dest arg-loc)
       weight)))

(defun weight-pred-result (w-pred)
  (case *result-spec*
    ((nil) 0)
    (C-bool (setq *C-bool* nil) w-pred)
    (t (+ (weight-C-if w-pred)
          (weight-C-t (weight-CC-dest *result-spec*))
          (weight-C-else)
          (weight-C-nil (weight-CC-dest *result-spec*))))))

(defun weight-C-result (w-source)
  (case *result-spec*
    ((nil) 0)
    (C-bool (setq *C-bool* nil) (weight-CC-make-bool w-source))
    (t (weight-C-copy w-source (weight-CC-dest *result-spec*)))))

(defun weight-eq (form1 form2)
  (weight-pred-result
   (let ((*stack-top* *stack-top*)
         (const-arg 0))
     (if (or (simple-literal-p form1) (sym-p form1))
         (incf const-arg 1)
         (setq form1 (weight-CC-get-arg form1)))
     (if (or (simple-literal-p form2) (sym-p form2))
         (incf const-arg 2)
         (setq form2 (weight-CC-get-arg form2)))
     (case const-arg
       ((1 2) (when (= const-arg 2)
                (rotatef form1 form2))
        (unless (numberp form2)
          (setq form2 (weight-form form2)))
        (typecase form1
          (null-form form2)
          (int (+ 4 form2))
          (character-form (+ 4 form2))
          (sym (+ 6 form2))
          (t 0)))
       (3 0)
       (t (+ 5 form1 form2))))))

(defun weight-eql (form1 form2)
  (weight-pred-result
   (let ((*stack-top* *stack-top*)
         (floatconst 0))
     (if (float-form-p form1)
         (incf floatconst 1)
         (setq form1 (weight-CC-get-arg form1)))
     (if (float-form-p form2)
         (incf floatconst 2)
         (setq form2 (weight-CC-get-arg form2)))
     (case floatconst
       (0 (+ 13 form1 form2))
       (3 0)
       (t (when (= floatconst 2)
            (rotatef form1 form2))
          (unless (numberp form2)
            (setq form2 (weight-form form2)))
          (+ 8 form2))))))

(defun weight-integerp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-fixnum-low-p (val low)
  (setq val (weight-CC-get-arg val))
  (setq low (weight-CC-get-arg low))
  (weight-pred-result (+ 4 val low)))

(defun weight-fixnum-high-p (val high)
  (setq val (weight-CC-get-arg val))
  (setq high (weight-CC-get-arg high))
  (weight-pred-result (+ 4 val high)))

(defun weight-consp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-characterp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-plain-vector-p (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-simple-vector-p (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-simple-string-p (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-simple-bit-vector-p (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-floatp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-atom (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-symbolp (x-loc)
  (setq x-loc (weight-CC-get-arg x-loc))
  (weight-pred-result (+ 5 x-loc)))

(defun weight-symp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-listp (x-loc)
  (setq x-loc (weight-CC-get-arg x-loc))
  (weight-pred-result (+ 5 x-loc)))

(defun weight-numberp (x-loc)
  (setq x-loc (weight-CC-get-arg x-loc))
  (weight-pred-result (+ 5 x-loc)))

(defun weight-functionp (x-loc)
  (setq x-loc (weight-CC-get-arg x-loc))
  (weight-pred-result (+ 8 x-loc)))

(defun weight-%car (cell)
  (setq cell (weight-CC-get-arg cell))
  (weight-C-result (1+ cell)))

(defun weight-%cdr (cell)
  (setq cell (weight-CC-get-arg cell))
  (weight-C-result (1+ cell)))

(defun weight-cons (x y)
  (setq x (weight-CC-get-arg x))
  (setq y (weight-CC-get-arg y))
  (case *result-spec*
    ((nil) 0)
    (C-bool (setq *C-bool* nil) 0)
    (t (+ 3                             ; call form_alloc
          (weight-C-copy x 1)
          (weight-C-copy y 1)
          2 (weight-CC-dest *result-spec*))))) ; LOAD_CONS

(defun weight-%rplaca (x y)
  (multiple-value-bind (x weight-x) (weight-get-arg-loc x)
    (multiple-value-bind (y weight-y) (weight-get-arg-loc y)
      (+ (weight-C-copy (weight-CC-dest y) (1+ (weight-CC-dest x)))
         (weight-to-result-loc x)
         weight-x
         weight-y))))

(defun weight-%rplacd (x y)
  (multiple-value-bind (x weight-x) (weight-get-arg-loc x)
    (multiple-value-bind (y weight-y) (weight-get-arg-loc y)
      (+ (weight-C-copy (weight-CC-dest y) (1+ (weight-CC-dest x)))
         (weight-to-result-loc x)
         weight-x
         weight-y))))

(defun weight-%logior (x y)
  (setq x (weight-CC-get-arg x))
  (setq y (weight-CC-get-arg y))
  (+ 3 x y (weight-CC-dest *result-spec*)))

(defun weight-%logxor (x y)
  (setq x (weight-CC-get-arg x))
  (setq y (weight-CC-get-arg y))
  (+ 3 x y (weight-CC-dest *result-spec*)))

(defun weight-%logand (x y)
  (setq x (weight-CC-get-arg x))
  (setq y (weight-CC-get-arg y))
  (+ 3 x y (weight-CC-dest *result-spec*)))

(defun weight-%lognot (x)
  (setq x (weight-CC-get-arg x))
  (+ 3 x (weight-CC-dest *result-spec*)))

(defun weight-%shift-right (i c)
  (setq i (weight-CC-get-arg i))
  (setq c (weight-CC-get-arg c))
  (+ 3 i c (weight-CC-dest *result-spec*)))

(defun weight-%shift-left (i c)
  (setq i (weight-CC-get-arg i))
  (setq c (weight-CC-get-arg c))
  (+ 3 i c (weight-CC-dest *result-spec*)))

(defun weight-instancep (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-instance-ref (instance offset)
  (weight-svref-internal instance offset))

(defun weight-instance-set (new-value instance offset)
  (weight-set-svref-internal new-value instance offset))

(defun weight-plain-vector-length (x)
  (setq x (weight-CC-get-arg x))
  (+ 3 x (weight-CC-dest *result-spec*)))

(defun weight-svref-internal (vector index)
  (weight-C-result (+ 3 (weight-CC-get-arg vector) (weight-CC-get-arg index))))

(defun weight-set-svref-internal (new-value vector index)
  (weight-C-copy (weight-CC-get-arg new-value)
                 (+ 3 (weight-CC-get-arg vector) (weight-CC-get-arg index))))

(defun weight-symbol-value (sym)
  (weight-C-result (+ 3 (weight-CC-get-arg sym))))

(defun weight-set-symbol-value (value sym)
  (weight-C-copy (weight-CC-get-arg value)
                 (+ 3 (weight-CC-get-arg sym))))

(defun weight-symbol-plist (sym)
  (weight-symbol-value sym))

(defun weight-set-symbol-plist (value sym)
  (weight-set-symbol-value value sym))

(defun weight-symbol-package (sym)
  (weight-symbol-value sym))

(defun weight-set-symbol-package (value sym)
  (weight-set-symbol-value value sym))

(defun weight-structp (x-loc)
  (weight-pred-result (weight-CC-get-arg x-loc)))

(defun weight-struct-size (struct)
  (weight-plain-vector-length struct))
  
(defun weight-structure-ref (struct index)
  (weight-svref-internal struct index))

(defun weight-set-structure-ref (new-value instance offset)
  (weight-instance-set new-value instance offset))

;;------------------------------------------------------------------------------
;; weight-Funktionen und -Konstanten fuer cgforeign
;;------------------------------------------------------------------------------

;; weight ist fuer foreign-funs noch nicht implementiert.

(defun weight-call-in-interface (fun)
  (declare (ignore fun))
  0)

;;------------------------------------------------------------------------------

(provide "weight")
