;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Test eines regelbasierten Expertensystems mit Vorvaertsverkettung
;;;
;;; $Revision: 1.6 $
;;; $Log: expert.lisp,v $
;;; Revision 1.6  1993/06/07  11:12:03  kl
;;; Anpassung an die zwei Versionen des Patternmatchers.
;;;
;;; Revision 1.5  1993/02/16  17:15:08  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.4  1992/09/04  12:52:14  kl
;;; Package auf User geaendert.
;;;
;;; Revision 1.3  1992/08/26  14:24:00  kl
;;; in-package eingefuegt.
;;;
;;; Revision 1.2  1992/08/26  13:38:11  kl
;;; Textausgaben entfernt oder auskommentiert.
;;;
;;; Revision 1.1  1992/08/26  13:02:40  kl
;;; Initial revision
;;;
;;;
;;;-----------------------------------------------------------------------------

(in-package "USER")

;;**************************** S T R E A M S ***********************************

;;; Strom-Praedikat und -Zugriffsfunktionen:

(defun stream-endp (stream) (eq stream 'empty-stream))
(defun stream-first (stream) (first stream))
(defun stream-rest (stream) (second stream))
(defun stream-cons (object stream) (list object stream))

;;------------------------------------------------------------------------------

;;; Stream-append haengt stream2 an stream1.
(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
    stream2
    (stream-cons (stream-first stream1)
                 (stream-append (stream-rest stream1)
                                stream2))))

;;------------------------------------------------------------------------------

;;; Stream-concatenate liefert zu einem Strom von Stroemen (s1, ..., sn) einen
;;; Strom mit den Inhalten von s1 bis sn.
(defun stream-concatenate (streams)
  (if (stream-endp streams)
    'empty-stream
    (stream-append (stream-first streams)
                   (stream-concatenate (stream-rest streams)))))

;;------------------------------------------------------------------------------

;;; Stream-transform liefert eine Strom von Resultaten der Anwendung von 
;;; procedure auf die Elemente von stream.
(defun stream-transform (procedure stream)
  (if (stream-endp stream)
    'empty-stream
    (stream-cons (funcall procedure (stream-first stream))
                 (stream-transform procedure
                                   (stream-rest stream)))))

;;------------------------------------------------------------------------------

;;; Stream-member liefert, ob object ein Element von stream ist oder nicht.
(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
        ((equal object (stream-first stream)) t)
        (t (stream-member object (stream-rest stream)))))

;;------------------------------------------------------------------------------

;;; Das Makro stream-remember setzt variable auf den Strom der durch Anhaengen
;;; des Elementes object an den alten Inhalt (ein Strom) von variable entsteht.
(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
     (setf ,variable
           (stream-append ,variable
                          (stream-cons ,object
                                       'empty-stream)))))

;;*********** R U L E  -  B A S E D   E X P E R T  -  S Y S T E M **************

;;------------------------------------------------------------------------------
;; Globale Variablen:
;;------------------------------------------------------------------------------
(defvar *assertions*)
(defvar *new-assertions*)
(defvar *rules*)

;;------------------------------------------------------------------------------
(defun try-assertion (pattern assertion bindings)
  (let ((result (match1 pattern assertion bindings)))
    (if (eq 'fail result)
      'empty-stream
      (stream-cons result 'empty-stream))))

;;------------------------------------------------------------------------------

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
   (stream-transform
    #'(lambda (assertion) (try-assertion (the list pattern)
                                         (the list assertion)
                                         (the list bindings)))
    *assertions*)))

;;------------------------------------------------------------------------------

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
   (stream-transform
    #'(lambda (bindings)
        (match-pattern-to-assertions pattern bindings))
    stream)))

;;------------------------------------------------------------------------------

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
    initial-input-stream
    (apply-filters (rest patterns)
                   (filter-binding-stream (first patterns)
                                          initial-input-stream))))

;;------------------------------------------------------------------------------

(defun instantiate-variables (pattern a-list)
  (cond ((atom pattern) pattern)
        ((variable-p pattern)
         (first (extract-value (find-binding pattern a-list))))
        (T (cons (instantiate-variables (first pattern) a-list)
                 (instantiate-variables (rest pattern) a-list)))))

;;------------------------------------------------------------------------------

(defun use-rule (rule)
  (let ((binding-stream
         (apply-filters (rule-ifs rule)
                        (stream-cons nil 'empty-stream))))
    (do ((binding-stream binding-stream
                         (stream-rest binding-stream))
         (success-switch nil))
        ((stream-endp binding-stream) success-switch)
      (let ((result (instantiate-variables
                     (rule-then rule)
                     (stream-first binding-stream))))
        (when (remember-assertion result)
          (push (cons (rule-name rule) result) *new-assertions* )
;         (format t "~%;;; Rule ~a indicates ~a."
;                 (rule-name rule) result)
          (setf success-switch t))))))

;;------------------------------------------------------------------------------

(defun forward-chain ()
  (do ((rule-stream *rules* (stream-rest rule-stream))
       (repeat-switch nil))
      ((stream-endp rule-stream)
       (if repeat-switch
           ;; Test the rules again.
           (forward-chain)
           ;; Otherwise You are ready.
           *new-assertions*))
    (when (use-rule (stream-first rule-stream))
      (setf repeat-switch t))))

;;******************************* R U L E S ************************************

(defun remember-rule (rule)
  (stream-remember rule *rules*))

;;------------------------------------------------------------------------------

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule ) (rest (second rule)))

(defun rule-then (rule) (second (third rule)))

;;------------------------------------------------------------------------------

(defun set-rules ()
  (setf *rules* 'empty-stream)

  (remember-rule '(identify1
                   (AND ((? animal) has hair))
                   (CONCLUDE ((? animal) is a mammal))))

  (remember-rule '(identify2
                   (AND ((? animal) gives milk))
                   (CONCLUDE ((? animal) is a mammal))))

  (remember-rule '(identify3
                   (AND ((? animal) has feathers))
                   (CONCLUDE ((? animal) is a bird))))

  (remember-rule '(identify4
                   (AND ((? animal) flies)
                    ((? animal) lays eggs))  
                   (CONCLUDE ((? animal) is a bird))))

  (remember-rule '(identify5
                   (AND ((? animal) eats meat))
                   (CONCLUDE ((? animal) is a carnivore))))

  (remember-rule '(identify6
                   (AND ((? animal) has pointed teeth)
                    ((? animal) has claws)
                    ((? animal) has forward eyes))
                   (CONCLUDE ((? animal) is a carnivore))))

  (remember-rule '(identify16
                   (AND ((? animal) is a (? species))
                    ((? animal) is a parent of (? child)))
                   (CONCLUDE ((? child) is a (? species)))))
  )

;;*************************** A S S E R T I O N S ******************************
 
(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

;;------------------------------------------------------------------------------

(defun set-assertions ()
  (setf *assertions* 'empty-stream)

  (remember-assertion '(mary is a parent of fury))

  (remember-assertion '(bozo is a dog))

  (remember-assertion '(lisa is a parent of mary))

  (remember-assertion '(lisa has hair))

  (remember-assertion '(deedee is a horse))

  (remember-assertion '(deedee is a parent of sugar))

  (remember-assertion '(deedee is a parent of brassy))

  (remember-assertion '(heinz is a human))

  (remember-assertion '(the more we test the more errors we will find))

  (remember-assertion '(eve is a parent of mike))

  (remember-assertion '(steve is a parent of mike)))

;;******************************************************************************

(defun test-rule-based-expertsystem ()
  (set-rules)
  (set-assertions)
  (setf *new-assertions* nil)
  (forward-chain))


(clicc-test "rule based expertsystem"
            ";;; Does forward chaining on a small rule base."
            (((test-rule-based-expertsystem) 
              ((IDENTIFY16 FURY IS A MAMMAL) (IDENTIFY16 MARY IS A MAMMAL)
               (IDENTIFY16 BRASSY IS A HORSE) (IDENTIFY16 SUGAR IS A HORSE)
               (IDENTIFY1 LISA IS A MAMMAL)))
             ))

(provide "expert")
;eof
