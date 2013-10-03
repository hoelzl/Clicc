;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion  : Laufzeitsystem (14. Sequences)                             
;;;
;;; $Revision: 1.36 $
;;; $Log: seq.lisp,v $
;;; Revision 1.36  1994/02/09  14:16:27  hk
;;; Keyword Parameter in {remove|delete}-if-[not] gestrichen, da die
;;; Rest-Liste an eine Funktion weitergereicht wird, die die Keyword
;;; Parameter Zuordnung und Prüfung vornimmt.
;;;
;;; Revision 1.35  1994/02/08  13:19:25  sma
;;; :my-last-arg-may-be-rest-var bei length.
;;;
;;; Revision 1.34  1993/12/14  12:40:42  sma
;;; Aufruf von rt::%vector-length aus length entfernt. Alle (besser)
;;; internen Funktionen nach vector-length aus array.lisp verlagert.
;;;
;;; Revision 1.33  1993/12/09  16:53:41  sma
;;; length verändert, C-funktion raw-list-length wird nicht mehr benutzt.
;;;
;;; Revision 1.32  1993/12/07  15:36:53  ft
;;; fehlende lokale Variable result in list-find hinzugefuegt
;;;
;;; Revision 1.31  1993/12/03  10:20:31  ft
;;; Austausch von list-position und list-find gegen korrektere und
;;; effizientere Versionen.
;;;
;;; Revision 1.30  1993/10/15  13:01:42  ft
;;; REMOVE optimiert.
;;;
;;; Revision 1.29  1993/09/30  09:44:01  wg
;;; Ueberfluessige Klammer-Zu in subseq geloescht.
;;;
;;; Revision 1.28  1993/09/28  16:25:23  hk
;;; subseq für Listen optimiert: verwendet nicht mehr length, prüft die
;;; Argumente nicht mehr so genau auf Zulässigkeit.
;;;
;;; Revision 1.27  1993/09/16  14:24:48  sma
;;; raw-list-length ist jetzt eine C-Funktion ist list.c
;;;
;;; Revision 1.26  1993/08/20  08:48:53  hk
;;; check-seq-test ist nun ein Macro
;;;
;;; Revision 1.25  1993/07/15  10:52:30  hk
;;; In check-seq-test eql statt eq als Defaultwert.
;;;
;;; Revision 1.24  1993/07/13  13:46:01  hk
;;; Fehler in list-position behoben: Resultat war um 1 zu gross.
;;;
;;; Revision 1.23  1993/07/13  06:52:09  ft
;;; MAP, SOME und EVERY optimiert.
;;;
;;; Revision 1.22  1993/07/12  04:29:26  ft
;;; Parameter in list-position richtig benannt.
;;;
;;; Revision 1.21  1993/07/11  07:30:18  ft
;;; Erste Optimierungen an MAP vorgenommen.
;;;
;;; Revision 1.20  1993/07/08  09:16:49  ft
;;; CONCATENATE optimiert.
;;;
;;; Revision 1.19  1993/07/07  06:47:55  ft
;;; Falsche Klammerung korrigiert.
;;;
;;; Revision 1.18  1993/07/06  15:39:59  ft
;;; POSITION optimiert.
;;;
;;; Revision 1.17  1993/07/06  15:30:28  ft
;;; NREVERSE optimiert.
;;;
;;; Revision 1.16  1993/07/06  13:17:37  ft
;;; Rückwärts-find auf Listen optimiert, die Optimierungen arbeiten
;;; jetzt auch auf NIL.
;;;
;;; Revision 1.15  1993/07/06  08:03:59  ft
;;; Abstrakten Datentyp Queue in das Lisp-Modul integriert.
;;;
;;; Revision 1.14  1993/07/06  07:57:33  ft
;;; REVERSE optimiert.
;;;
;;; Revision 1.13  1993/07/06  07:45:19  ft
;;; Abstrakten Datentyp Queue lokal in list-subseq eingefügt.
;;;
;;; Revision 1.12  1993/07/06  07:25:52  ft
;;; SUBSEQ optimiert.
;;;
;;; Revision 1.11  1993/07/02  12:45:31  ft
;;; Debug-Ausgabe entfernt.
;;;
;;; Revision 1.10  1993/07/01  14:18:49  ft
;;; find optimiert.
;;;
;;; Revision 1.9  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.8  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.7  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.36 $ eingefuegt
;;;
;;; Revision 1.6  1993/01/15  08:45:46  ft
;;; Erweiterung um MISMATCH und SEARCH.
;;;
;;; Revision 1.5  1992/11/26  17:02:13  hk
;;; length von sequence.c nach hier.
;;;
;;; Revision 1.4  1992/08/11  14:31:42  kl
;;; Substitute implementiert.
;;;
;;; Revision 1.3  1992/07/28  14:39:17  hk
;;; Aufgeraeumt, umbenannt.
;;;
;;; Revision 1.2  1992/07/06  11:44:54  hk
;;; Vorlaeufig string-char in character umbenannt.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(complement elt subseq copy-seq length reverse nreverse make-sequence
   concatenate map some every notany notevery reduce fill replace remove
   remove-if remove-if-not delete delete-if delete-if-not remove-duplicates
   delete-duplicates find find-if find-if-not position position-if
   position-if-not count count-if count-if-not mismatch search substitute sort))

;;------------------------------------------------------------------------------
;; Fuer Fehlermeldungen
;;------------------------------------------------------------------------------
(defconstant NOT_IN_RANGE
  "The value ~S is not an integer in the interval [0,~S]")

;;------------------------------------------------------------------------------
(defun sequence-type (sequence)
  (cond
    ((listp   sequence) 'list)
    ((vectorp sequence) `(vector ,(array-element-type sequence)))
    (t (error "Unexpected error"))))

;;------------------------------------------------------------------------------
(defmacro check-seq-test (test test-not)
  `(cond
    (,test ,test)
    (,test-not #'(lambda (&rest arguments)
                   (not (apply ,test-not arguments))))
    (t #'eql)))

;;------------------------------------------------------------------------------
(defun check-seq-start-end (start end length)
  (when (not (check-integer start 0 length))
    (error NOT_IN_RANGE start length))
  (when (null end)
    (setq end length))
  (when (not (check-integer end 0 length))
    (error NOT_IN_RANGE end length))
  (when (> start end)
    (error "The START value ~S is greater than the END value ~S" start end))
  end)

;;------------------------------------------------------------------------------
(defun check-seq-count (count length)
  (cond
    ((integerp count)
     (if (plusp count) count 0))
    ((null count) length)
    (t (error "~S should be an INTEGER at least 0 and no more than 16777214"
              count))))

;;------------------------------------------------------------------------------
;; Abstrakter Datentyp Queue
;; Eine Cons-Zelle enthaelt im Car eine Liste und im Cdr den letzten Cons-Knoten
;; dieser Liste.
;; Eine leere Queue wird durch einen Cons-Knoten, der im Car und Cdr Nil
;; enthaelt, dargestellt. 
;;------------------------------------------------------------------------------

(defun empty-queue ()
  (cons nil nil))

(defun empty-queue-p (q)
  (null (car q)))

(defun list2queue (l)
  (cons l (last l)))                   ; (last ()) = ()

(defun queue2list (q)
  (car q))

(defun add-q (e q)
  (let ((new-cons (cons e nil)))
    (if (null (cdr q))
        (rplaca q new-cons)
        (rplacd (cdr q) new-cons))
    (rplacd q new-cons)))

(defun addnew-q (e q)
  (unless (member e (queue2list q))
    (add-q e q)))

;;------------------------------------------------------------------------------
;; Letztes Element der Queue
;;------------------------------------------------------------------------------
(defun last-q (q)
  (cadr q))

(defun set-last-q (e q)
  (rplaca (cdr q) e))

;;-----------------------------------------------------------------------------
;; COMPLEMENT fn                                              [CL2]
;;-----------------------------------------------------------------------------
(defun complement (fn)
  #'(lambda (&rest arguments)
      (not (apply fn arguments))))

;;-----------------------------------------------------------------------------
;; 14.1. Simple Sequence Functions                                            
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ELT sequence index
;;-----------------------------------------------------------------------------
(defun elt (sequence index)
  (typecase sequence
    (cons (nth index sequence))
    (vector (row-major-aref sequence index))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

;;-----------------------------------------------------------------------------
;; (setf ELT) newvalue sequence index
;;-----------------------------------------------------------------------------
(defun (setf elt) (newvalue sequence index)
  (typecase sequence
    (cons (setf (nth index sequence) newvalue))
    (vector (setf (row-major-aref sequence index) newvalue))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

;;-----------------------------------------------------------------------------
;; SUBSEQ sequence start &OPTIONAL end
;;-----------------------------------------------------------------------------
(defun subseq (sequence start &optional end)
    (typecase sequence
      (null nil)
      (cons (list-subseq sequence start end))
      (vector (vector-subseq sequence start end))
      (T (error WRONG_TYPE sequence 'SEQUENCE))))

(defun vector-subseq (sequence start end)
  (let ((length (length sequence)))
    (setq end (check-seq-start-end start end length))
    (let ((subseq (make-sequence `(vector ,(array-element-type sequence))
                                 (- end start))))
      (do ((i start (1+ i))
           (j 0     (1+ j)))
          ((= i end) subseq)
        (setf (elt subseq j) (elt sequence i))))))

(defun list-subseq (sequence start end)
  (let ((subseq (empty-queue)))
    (setq sequence (nthcdr start sequence))
    (cond
      (end
       (decf end start)
       (dotimes (i end)
         (when (atom sequence)
           (error "end of sequence reached"))
         (add-q (pop sequence) subseq)))
      (t
       (loop
        (when (null sequence)
          (return))
        (add-q (pop sequence) subseq))))
    (queue2list subseq)))

;;------------------------------------------------------------------------------
;; (SETF SUBSEQ)
;;------------------------------------------------------------------------------
(defsetf SUBSEQ (sequence start &optional end) (new-sequence)
  `(PROGN (REPLACE ,sequence ,new-sequence :START1 ,start :END1 ,end)
    ,new-sequence))

;;-----------------------------------------------------------------------------
;; COPY-SEQ sequence
;;-----------------------------------------------------------------------------
(defun copy-seq (sequence)
  (subseq sequence 0))

;;------------------------------------------------------------------------------
;; length sequence
;;------------------------------------------------------------------------------
(defun length (sequence)
  (declare (:my-last-arg-may-be-rest-var :length))
  (typecase sequence
    (null 0)
    (cons (let ((n 0)) 
            (dolist (s sequence) (incf n))
            n))
    ;;Statt doppeltem vectorp-Test erzeugt vector-length ggf Laufzeitfehler
    (T (vector-length sequence))))

;;-----------------------------------------------------------------------------
;; REVERSE sequence
;;-----------------------------------------------------------------------------
(defun reverse (sequence)
  (typecase sequence
    (null nil)
    (cons (list-reverse sequence))
    (vector (vector-reverse sequence))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))
  
(defun vector-reverse (sequence)
  (let* ((length (length sequence))
         (reverse (make-sequence `(vector ,(array-element-type sequence))
                                 length)))
    (do ((i 0 (1+ i))
         (j (1- length) (1- j)))
        ((= i length) reverse)
      (setf (elt reverse j) (elt sequence i)))))

(defun list-reverse (sequence)
  (let ((reverse nil))
    (dolist (elem sequence)
      (setf reverse (cons elem reverse)))
    reverse))

;;-----------------------------------------------------------------------------
;; NREVERSE sequence
;;-----------------------------------------------------------------------------
(defun nreverse (sequence)
  (typecase sequence
    (null nil)
    (cons (list-nreverse sequence))
    (vector (vector-nreverse sequence))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

(defun vector-nreverse (sequence)
  (do ((i 0 (1+ i))
       (j (1- (length sequence)) (1- j))
       seq-elem-i)
      ((>= i j) sequence)
    (setq seq-elem-i (elt sequence i))
    (setf (elt sequence i) (elt sequence j))
    (setf (elt sequence j) seq-elem-i)))

(defun list-nreverse (sequence)
  (labels ((doit (head tail)
	     (let ((rest (cdr tail)))
	       (setf (cdr tail) head)
	       (if (null rest)
		   tail
		   (doit tail rest)))))
    (if (null sequence)
	sequence
	(doit nil sequence))))

;;-----------------------------------------------------------------------------
;; MAKE-SEQUENCE type size &KEY :initial-element
;;-----------------------------------------------------------------------------

;; type ::= LIST |
;;          (SIMPLE-STRING size)              | (STRING size) |
;;          (SIMPLE-VECTOR element-type size) | (VECTOR element-type size)

;; (declaim (lisp::fun-spec normalize-type :par-spec 1))

;;------------------------------------------------------------------------------
(defun make-sequence (type size &key (initial-element nil init-elem-sp))
  (let ((result-type (normalize-type type))
        size-of-type)
    (tagbody
       (cond
         ((atom result-type)
          (if (eq result-type 'list)
            (return-from make-sequence
              (if init-elem-sp
                (make-list size :initial-element initial-element)
                (make-list size)))
            (go INVALID-SEQTYPE)))
         (t (case (car result-type)
              ((simple-array array)
               (let ((element-type (second result-type))
                     (dimensions   (third  result-type)))
                 (when (rest dimensions) (go INVALID-SEQTYPE))
                 (setq size-of-type (first dimensions))
                 (when (and (not (eq size-of-type '*))
                            (/= size size-of-type))
                   (go INCONSISTENT-SIZE))
                 (return-from make-sequence
                   (if (eq element-type 'character)
;;;                    (eq element-type 'string-char)
                     (if init-elem-sp
                       (make-string size :initial-element initial-element)
                       (make-string size))
                     (apply #'make-array size
                            (append
                             (if init-elem-sp
                               `(:initial-element ,initial-element)
                               ())
                             (if (eq element-type '*)
                               ()
                               `(:element-type ,element-type))))))))
              (t (go INVALID-SEQTYPE)))))
     INVALID-SEQTYPE
       (error "~S is an invalid sequence typespecifier." type)
     INCONSISTENT-SIZE
       (error "The size ~S is inconsistent with the specified size ~S in type."
              size size-of-type))))

;;-----------------------------------------------------------------------------
;; 14.2. Concatenating, Mapping, and Reducing Sequences                       
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; CONCATENATE result-type &REST sequences
;;-----------------------------------------------------------------------------
(defun concatenate (result-type &rest sequences)
  (if (eq result-type 'list)
    (apply #'concatenate-to-list sequences)
    (apply #'concatenate-to-non-list result-type sequences)))

(defun concatenate-to-non-list (result-type &rest sequences)
  (let (new-sequence
        (size  0)
        (index 0))
    (dolist (sequence sequences)
      (incf size (length sequence)))
    (setq new-sequence (make-sequence result-type size))
    (dolist (sequence sequences)
      (if (listp sequence)
          (dolist (elem sequence)
            (setf (elt new-sequence index) elem)
            (incf index))
          (dotimes (i (length sequence))
            (setf (elt new-sequence index)
                  (elt sequence     i))
            (incf index))))
    new-sequence))

(defun concatenate-to-list (&rest sequences)
  (let ((new-sequence (empty-queue)))
    (dolist (sequence sequences)
      (if (listp sequence)
          (dolist (elem sequence)
            (add-q elem new-sequence))
          (dotimes (i (length sequence))
            (add-q (elt sequence i) new-sequence))))
    (queue2list new-sequence)))

;;------------------------------------------------------------------------------
;; MAP result-type function sequence &REST more-sequences
;;------------------------------------------------------------------------------
(defun map (result-type function sequence &rest more-sequences)
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences)))
        (i 0))
    (declare (fixnum i l))
    (labels ((get-elem (rest-sequences)
               (if (listp (car rest-sequences))
                   (pop (car rest-sequences))
                   (elt (car rest-sequences) i))))
      (cond
        ((null result-type)
         (loop
          (when (>= i l) (return nil))
          (apply function (maplist #'get-elem more-sequences))
          (incf i)))
        ((eq 'list result-type)
         (let ((x (empty-queue)))
           (loop
            (when (>= i l) (return (queue2list x)))
            (add-q (apply function (maplist #'get-elem more-sequences)) x)
            (incf i))))
        (T
         (let ((x (make-sequence result-type l)))
           (loop
            (when (>= i l) (return x))
            (setf (elt x i) (apply function
                                   (maplist #'get-elem more-sequences)))
            (incf i))))))))



;;------------------------------------------------------------------------------
;; SOME predicate sequence &REST more-sequences
;;------------------------------------------------------------------------------
(defun some (predicate sequence &rest more-sequences)
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences)))
        (i 0))
    (declare (fixnum i l))
    (labels ((get-elem (rest-sequences)
               (if (listp (car rest-sequences))
                   (pop (car rest-sequences))
                   (elt (car rest-sequences) i))))
      (loop
       (when (>= i l) (return NIL))
       (let ((that-value
              (apply predicate (maplist #'get-elem more-sequences))))
         (when that-value (return that-value)))
       (incf i)))))

;;------------------------------------------------------------------------------
;; EVERY predicate sequence &REST more-sequences
;;------------------------------------------------------------------------------
(defun every (predicate sequence &rest more-sequences)
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences)))
        (i 0))
    (declare (fixnum i l))
    (labels ((get-elem (rest-sequences)
               (if (listp (car rest-sequences))
                   (pop (car rest-sequences))
                   (elt (car rest-sequences) i))))
      (loop
       (when (>= i l) (return T))
       (unless (apply predicate (maplist #'get-elem more-sequences))
         (return NIL))
       (incf i)))))

;;------------------------------------------------------------------------------
;; NOTANY predicate sequence &REST more-sequences
;;------------------------------------------------------------------------------
(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))

;;------------------------------------------------------------------------------
;; NOTEVERY predicate sequence &REST more-sequences
;;------------------------------------------------------------------------------
(defun notevery (predicate sequence &rest more-sequences)
  (not (apply #'every predicate sequence more-sequences)))

;;-----------------------------------------------------------------------------
;; REDUCE function sequence &KEY :from-end :start :end :initial-value
;;-----------------------------------------------------------------------------
(defun reduce (function sequence
                        &key  from-end (start 0) end
                        (initial-value nil init-val-sp))
  (let ((length (length sequence)))
    (setq end (check-seq-start-end start end length))
    (cond
      ((zerop length) (if init-val-sp
                          initial-value
                          (funcall function)))
      ((and (= length 1) (not init-val-sp)) (elt sequence 0))
      ((not from-end)
       (when (not init-val-sp)
         (setq initial-value (elt sequence start))
         (incf start))
       (do ((result initial-value))
           ((>= start end) result)
         (setq result (funcall function result (elt sequence start)))
         (incf start)))
      (t (decf end)
         (when (not init-val-sp)
           (setq initial-value (elt sequence end))
           (decf end))
         (do ((result initial-value))
             ((>= start end) result)
           (setq result (funcall function (elt sequence end) result))
           (decf end))))))

;;-----------------------------------------------------------------------------
;; FILL sequence item &KEY :start :end
;;-----------------------------------------------------------------------------
(defun fill (sequence item &key (start 0) end)
  (setq end (check-seq-start-end start end (length sequence)))
  (do ((i start (1+ i)))
      ((>= i end) sequence)
    (setf (elt sequence i) item)))

;;-----------------------------------------------------------------------------
;; REPLACE sequence1 sequence2 &KEY :start1 :end1 :start2 :end2
;;-----------------------------------------------------------------------------
(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  (setq end1 (check-seq-start-end start1 end1 (length sequence1)))
  (setq end2 (check-seq-start-end start2 end2 (length sequence2)))
  (let ((length (min (- end1 start1) (- end2 start2))))
    (if (and (eq sequence1 sequence2)
             (> start1 start2))
      (do ((i 0 (1+ i))
           (i1 (+ start1 (1- length)) (1- i1))
           (i2 (+ start2 (1- length)) (1- i2)))
          ((>= i length))
        (setf (elt sequence1 i1) (elt sequence2 i2)))
      (do ((i 0 (1+ i))
           (i1 start1 (1+ i1))
           (i2 start2 (1+ i2)))
          ((>= i length))
        (setf (elt sequence1 i1) (elt sequence2 i2)))))
  sequence1)

;;------------------------------------------------------------------------------
(defun matched-items (item sequence test start end key)
  (do (matched-items
       (i start (1+ i)))
      ((>= i end) (reverse matched-items))
    (when (funcall test item (funcall key (elt sequence i)))
      (push i matched-items))))

;;-----------------------------------------------------------------------------
;; REMOVE item sequence &KEY :from-end :test :test-not :start :end :count :key
;;-----------------------------------------------------------------------------
(defun remove (item sequence &rest keys)
  (typecase sequence
    (null nil)
    (cons (apply #'list-remove item sequence keys))
    (vector (apply #'vector-remove item sequence keys))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

(defun list-remove (item sequence
                         &key from-end test test-not (start 0) end count
                         (key #'identity))
  (let ((length (length sequence))
        (matched-items (empty-queue))
        (num-matched 0)
        (new-sequence (empty-queue)))
    (setq test  (check-seq-test test test-not))
    (setq end   (check-seq-start-end start end length))
    (setq count (check-seq-count count length))
    (let ((i 0))
      (dolist (elem sequence)
        (when (>= i end) (return))
        (when (and (>= i start) (funcall test item (funcall key elem)))
          (add-q i matched-items)
          (incf num-matched))
        (incf i)))
    (setq matched-items (queue2list matched-items))
    (if (not from-end)
        (do ((i 0 (1+ i)))
            ((>= i length))
          (cond
            ((and (plusp count) (eql i (car matched-items)))
             (pop matched-items)
             (decf count)
             (pop sequence))
            (t (add-q (pop sequence) new-sequence))))
        (do ((i 0 (1+ i)))
            ((>= i length))
          (cond
            ((and (<= num-matched count) (eql i (car matched-items)))
             (pop matched-items)
             (decf num-matched)
             (pop sequence))
            (t (add-q (pop sequence) new-sequence)
               (when (eql i (car matched-items))
                 (pop matched-items)
                 (decf num-matched))))))
    (queue2list new-sequence)))
    
(defun vector-remove (item sequence
                           &key from-end test test-not (start 0) end count
                           (key #'identity))
  (let ((length (length sequence))
        matched-items
        num-matched
        new-sequence)
    (setq test  (check-seq-test test test-not))
    (setq end   (check-seq-start-end start end length))
    (setq count (check-seq-count count length))
    (setq matched-items (matched-items item sequence test start end key))
    (setq num-matched   (length matched-items))
    (setq new-sequence  (make-sequence (sequence-type sequence)
                                       (- length (min num-matched count))))
    (if (not from-end)
        (do ((i 0 (1+ i))
             (j 0))
            ((>= i length))
          (cond
            ((and (plusp count) (eql i (car matched-items)))
             (pop matched-items)
             (decf count))
            (t (setf (elt new-sequence j) (elt sequence i))
               (incf j))))
        (do ((i 0 (1+ i))
             (j 0))
            ((>= i length))
          (cond
            ((and (<= num-matched count) (eql i (car matched-items)))
             (pop matched-items)
             (decf num-matched))
            (t (setf (elt new-sequence j) (elt sequence i))
               (when (eql i (car matched-items))
                 (pop matched-items)
                 (decf num-matched))
               (incf j)))))
    new-sequence))

;;-----------------------------------------------------------------------------
;; REMOVE-IF predicate sequence &KEY :from-end :start :end :count :key
;;-----------------------------------------------------------------------------
(defun remove-if (predicate sequence &rest rest-args)
  (apply #'remove nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (funcall predicate seq-elem))
         rest-args))

;;-----------------------------------------------------------------------------
;; REMOVE-IF-NOT predicate sequence &KEY :from-end :start :end :count :key
;;-----------------------------------------------------------------------------
(defun remove-if-not (predicate sequence &rest rest-args)
  (apply #'remove nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (not (funcall predicate seq-elem)))
         rest-args))
 
;;-----------------------------------------------------------------------------
;; DELETE item sequence &KEY :from-end :test :test-not :start :end :count :key
;;-----------------------------------------------------------------------------
(defun delete (item sequence &rest rest-args)
  (apply #'remove item sequence rest-args))

;;-----------------------------------------------------------------------------
;; DELETE-IF predicate sequence &KEY :from-end :start :end :count :key
;;-----------------------------------------------------------------------------
(defun delete-if (predicate sequence &rest rest-args)
  (apply #'delete nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (funcall predicate seq-elem))
         rest-args))

;;-----------------------------------------------------------------------------
;; DELETE-IF-NOT predicate sequence &KEY :from-end :start :end :count :key
;;-----------------------------------------------------------------------------
(defun delete-if-not (predicate sequence &rest rest-args)
  (apply #'delete nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (not (funcall predicate seq-elem)))
         rest-args))

;;-----------------------------------------------------------------------------
;; REMOVE-DUPLICATES sequence &KEY :from-end :test :test-not :start :end :key
;;-----------------------------------------------------------------------------
(defun remove-duplicates (sequence
                          &key from-end test test-not (start 0) end
                          (key #'identity))
  (let ((length (length sequence))
        new-sequence
        new-length
        without-duplicates)
    
    (setq test (check-seq-test test test-not))
    (setq end (check-seq-start-end start end length))

    (if (not from-end)
      (do ((i start (1+ i)))
          ((>= i end)
           (setq without-duplicates (reverse without-duplicates)))
        (do ((j (1+ i) (1+ j)))
            ((>= j end)
             (push (elt sequence i) without-duplicates))
          (when (funcall test (funcall key (elt sequence i))
                         (funcall key (elt sequence j)))
            (return))))
      (do ((i (1- end) (1- i)))
          ((< i start))
        (do ((j (1- i) (1- j)))
            ((< j start)
             (push (elt sequence i) without-duplicates))
          (when (funcall test (funcall key (elt sequence i))
                         (funcall key (elt sequence j)))
            (return)))))

    (setq new-length (+ start (length without-duplicates) (- length end)))
    (setq new-sequence
          (make-sequence (sequence-type sequence)
                         new-length))
    (let ((i 0) (j end))
      (loop
        (when (>= i start) (return))
        (setf (elt new-sequence i) (elt sequence i))
        (incf i))
      (loop
        (when (null without-duplicates) (return))
        (setf (elt new-sequence i) (pop without-duplicates))
        (incf i))
      (loop
        (when (>= i new-length) (return))
        (setf (elt new-sequence i) (elt sequence j))
        (incf i)
        (incf j)))
    new-sequence))

;;-----------------------------------------------------------------------------
;; DELETE-DUPLICATES sequence &KEY :from-end :test :test-not :start :end :key
;;-----------------------------------------------------------------------------
(defun delete-duplicates (sequence &rest rest-args)
  (apply #'remove-duplicates sequence rest-args))

;;-----------------------------------------------------------------------------
;; FIND item sequence &KEY :from-end :test :test-not :start :end :key
;;-----------------------------------------------------------------------------
(defun find (item sequence &rest keys)
  (typecase sequence
    (null nil)
    (cons (apply #'list-find item sequence keys))
    (vector (apply #'vector-find item sequence keys))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

(defun vector-find (item vector
                         &key from-end test test-not (start 0) end
                         (key #'identity))
  (setq test (check-seq-test test test-not))
  (setq end  (check-seq-start-end start end (length vector)))
  (if (not from-end)
      (do ((i start (1+ i)))
          ((>= i end) nil)
        (let ((elem (elt vector i)))
          (when (funcall test item (funcall key elem)) (return elem))))
      (do ((i (1- end) (1- i)))
          ((< i start) nil)
        (let ((elem (elt vector i)))
          (when (funcall test item (funcall key elem)) (return elem))))))

(defun list-find (item
                  list
                  &key from-end test test-not (start 0) end (key #'identity))
  (let ((i start)
        (result nil)
        (length (length list)))
    (setq test (check-seq-test test test-not))
    (setq end  (check-seq-start-end start end length))
    (when from-end (decf end))
    (dolist (elem (nthcdr start list))
      (when (funcall test item (funcall key elem)) (if from-end
                                                       (setf result elem)
                                                       (return elem)))
      (when (= i end) (return result))      
      (incf i))))

;;-----------------------------------------------------------------------------
;; FIND-IF predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun find-if (predicate sequence &rest rest-args)
  (apply #'find nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (funcall predicate seq-elem))
         rest-args))

;;-----------------------------------------------------------------------------
;; FIND-IF-NOT predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun find-if-not (predicate sequence &rest rest-args)
  (apply #'find nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (not (funcall predicate seq-elem)))
         rest-args))

;;-----------------------------------------------------------------------------
;; POSITION item sequence &KEY :from-end :test :test-not :start :end :key
;;-----------------------------------------------------------------------------
(defun position (item sequence &rest keys)
  (typecase sequence
    (null nil)
    (cons (apply #'list-position item sequence keys))
    (vector (apply #'vector-position item sequence keys))
    (T (error WRONG_TYPE sequence 'SEQUENCE))))

(defun vector-position (item sequence
                             &key from-end test test-not (start 0) end
                             (key #'identity))
  (setq test (check-seq-test test test-not))
  (setq end  (check-seq-start-end start end (length sequence)))

  (if (not from-end)
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (when (funcall test item (funcall key (elt sequence i)))
        (return i)))
    (do ((i (1- end) (1- i)))
        ((< i start) nil)
      (when (funcall test item (funcall key (elt sequence i)))
        (return i)))))

(defun list-position (item list
                           &key from-end test test-not (start 0) end
                           (key #'identity))
  (let ((i start)
        (result nil)
        (length (length list)))
    (setq test (check-seq-test test test-not))
    (setq end  (check-seq-start-end start end length))
    (when from-end (decf end))
    (dolist (elem (nthcdr start list))
      (when (funcall test item (funcall key elem)) (if from-end
                                                       (setf result i)
                                                       (return i)))
      (when (= i end) (return result))      
      (incf i))))

;;-----------------------------------------------------------------------------
;; POSITION-IF predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun position-if (predicate sequence &rest rest-args)
  (apply #'position nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (funcall predicate seq-elem))
         rest-args))

;;-----------------------------------------------------------------------------
;; POSITION-IF-NOT predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun position-if-not (predicate sequence &rest rest-args)
  (apply #'position nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (not (funcall predicate seq-elem)))
         rest-args))

;;-----------------------------------------------------------------------------
;; COUNT item sequence &KEY :from-end :test :test-not :start :end :key
;;-----------------------------------------------------------------------------
(defun count (item sequence
                   &key from-end test test-not (start 0) end (key #'identity))
  (declare (ignore from-end))
  (setq test (check-seq-test test test-not))
  (setq end  (check-seq-start-end start end (length sequence)))

  (let ((count 0))
    (do ((i start (1+ i)))
        ((>= i end) nil)
      (when (funcall test item (funcall key (elt sequence i)))
        (incf count)))
    count))

;;-----------------------------------------------------------------------------
;; COUNT-IF predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun count-if (predicate sequence &rest rest-args)
  (apply #'count nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (funcall predicate seq-elem))
         rest-args))

;;-----------------------------------------------------------------------------
;; COUNT-IF-NOT predicate sequence &KEY :from-end :start :end :key
;;-----------------------------------------------------------------------------
(defun count-if-not (predicate sequence &rest rest-args)
  (apply #'count nil sequence
         :test #'(lambda (item seq-elem)
                   (declare (ignore item))
                   (not (funcall predicate seq-elem)))
         rest-args))

;;------------------------------------------------------------------------------
;; MISMATCH sequence1 sequence2 &KEY :from-end :test :test-not
;;          :key :start1 :start2 :end1 :end2
;;------------------------------------------------------------------------------
(defun mismatch (sequence1 sequence2 
                 &key from-end test test-not (key #'identity)
                      (start1 0) (start2 0) end1 end2)
  (setq test (check-seq-test test test-not))
  (setq end1  (check-seq-start-end start1 end1 (length sequence1)))
  (setq end2  (check-seq-start-end start2 end2 (length sequence2)))

  (block testing-sequences
    (if from-end
          
        ;; von rechts nach links
        ;;----------------------
        (do ((i1 (1- end1) (1- i1))
             (i2 (1- end2) (1- i2)))
            ((or (<= i1 start1) (<= i2 start2)) nil)
          (unless (funcall test 
                           (funcall key (elt sequence1 i1))
                           (funcall key (elt sequence2 i2)))
            (return-from testing-sequences (1+ i1))))
        
      ;; von links nach rechts
      ;;----------------------
      (do ((i1 start1 (1+ i1))
           (i2 start2 (1+ i2)))
          ((or (>= i1 end1) (>= i2 end2)) nil)
        (unless (funcall test 
                         (funcall key (elt sequence1 i1))
                         (funcall key (elt sequence2 i2)))
          (return-from testing-sequences i1))))
    nil))

;;------------------------------------------------------------------------------
;; SEARCH sequence1 sequence2 &KEY :from-end :test :test-not
;;        :key :start1 :start2 :end1 :end2
;;------------------------------------------------------------------------------
(defun search (sequence1 sequence2 
               &key from-end test test-not (key #'identity)
                    (start1 0) (start2 0) end1 end2)
  
  (setq test (check-seq-test test test-not))
  (setq end1  (check-seq-start-end start1 end1 (length sequence1)))
  (setq end2  (check-seq-start-end start2 end2 (length sequence2)))
  
  (let ((first1 (if from-end
                    (funcall key (elt sequence1 (1- end1)))
                  (funcall key (elt sequence1 start1)))))
    (block testing-sequences
      (if from-end
          
          ;; von rechts nach links
          ;;----------------------
          (do ((i (1- end2) (1- i)))
              ((<= i start2) nil)
            (block testing-subsequences
              (when (funcall test first1 (funcall key (elt sequence2 i)))
                (do ((i1 (- 2 end1) (1- i1))
                     (i2 (1- i) (1- i2)))
                    ((or (<= i1 start1) (<= i2 start2)) nil)
                  (unless (funcall test 
                                   (funcall key (elt sequence1 i1))
                                   (funcall key (elt sequence2 i2)))
                    (setf i (1+ i2))
                    (return-from testing-subsequences nil)))
                (return-from testing-sequences (- (1+ i) (- end1 start1))))))
        
        ;; von links nach rechts
        ;;----------------------
        (do ((i start2 (1+ i)))
              ((>= i end2) nil)
            (block testing-subsequences
              (when (funcall test first1 (funcall key (elt sequence2 i)))
                (do ((i1 (1+ start1) (1+ i1))
                     (i2 (1+ i) (1+ i2)))
                    ((or (>= i1 end1) (>= i2 end2)) nil)
                  (unless (funcall test 
                                   (funcall key (elt sequence1 i1))
                                   (funcall key (elt sequence2 i2)))
                    (setf i (1- i2))
                    (return-from testing-subsequences nil)))
                (return-from testing-sequences i)))))
      nil)))

;;------------------------------------------------------------------------------
;; SUBSTITUTE newitem olditem sequence &KEY :from-end :test :test-not :start
;;   :end :cout :key
;;------------------------------------------------------------------------------
(defun substitute (newitem olditem sequence &key from-end test test-not
                           (start 0) end count (key #'identity))
  (let ((length (length sequence)))
    (setq test (check-seq-test test test-not))
    (setq end  (check-seq-start-end start end length))
    (setq count (check-seq-count count length))
  
    (if (not from-end)
        (do ((newseq (make-sequence (sequence-type sequence) length))
             (i 0 (1+ i))
             (k 0))
            ((>= i length) newseq)
          (cond ((and (<= start i) (< i end) (< k count) 
                      (funcall test olditem 
                               (funcall key (elt sequence i))))
                 (setf (elt newseq i) newitem)
                 (incf k))
                (t (setf (elt newseq i) (elt sequence i)))))

        (do ((newseq (make-sequence (sequence-type sequence) length))
             (i (1- length) (1- i))
             (k 0))
            ((< i 0) newseq)
          (cond ((and (<= start i) (< i end) (< k count) 
                      (funcall test olditem 
                               (funcall key (elt sequence i))))
                 (setf (elt newseq i) newitem)
                 (incf k))
                (t (setf (elt newseq i) (elt sequence i))))))))
          



;;-----------------------------------------------------------------------------
;; SORT sequence predicate &KEY :key
;;-----------------------------------------------------------------------------
(defun sort (sequence predicate &key (key #'identity))
  (if (listp sequence)
    (list-merge-sort sequence predicate key)
    (quick-sort sequence 0 (length sequence) predicate key)))

;;------------------------------------------------------------------------------
(defun list-merge-sort (l predicate key)
  (labels
      ((sort (l)
         (let ((i (length l))
               left right l0 l1 key-left key-right)
           (cond ((< i 2) l)
                 ((= i 2)
                  (setq key-left (funcall key (car l)))
                  (setq key-right (funcall key (cadr l)))
                  (if (or (funcall predicate key-left key-right)
                          (not (funcall predicate key-right key-left)))
                      l
                      (nreverse l)))
                 (t (setq i (floor i 2))
                    (do ((j 1 (1+ j)) (l1 l (cdr l1)))
                        ((>= j i)
                         (setq left l)
                         (setq right (cdr l1))
                         (rplacd l1 nil)))
                    (setq left (sort left))
                    (setq right (sort right))
                    (cond ((endp left) right)
                          ((endp right) left)
                          (t (setq l0 (cons nil nil))
                             (setq l1 l0)
                             (setq key-left (funcall key (car left)))
                             (setq key-right (funcall key (car right)))

                             (loop
                               (cond
                                 ((or (funcall predicate key-left key-right)
                                      (not
                                       (funcall predicate key-right key-left)))
                                
                                  (rplacd l1 left)
                                  (setq l1 (cdr l1))
                                  (setq left (cdr left))
                                  (when (endp left)
                                    (rplacd l1 right)
                                    (return (cdr l0)))
                                  (setq key-left (funcall key (car left))))
                                 (t (rplacd l1 right)
                                    (setq l1 (cdr l1))
                                    (setq right (cdr right))
                                    (when (endp right)
                                      (rplacd l1 left)
                                      (return (cdr l0)))
                                    (setq key-right
                                          (funcall key (car right)))))))))))))
    (sort l)))

;;------------------------------------------------------------------------------
(defun quick-sort (sequence start end predicate key)
  (let ((j 0) (k 0))
    (when (<= end (1+ start))
      (return-from quick-sort sequence))
    (setq j start)
    (setq k (1- end))
    (do ((d (elt sequence start)))
        ((> j k))
      (do ()
          ((or (> j k)
               (funcall predicate
                        (funcall key (elt sequence k))
                        (funcall key d))))
        (decf k))
      (when (< k start)
        (quick-sort sequence (1+ start) end predicate key)
        (return-from quick-sort sequence))
      (do ()
          ((or (> j k)
               (not (funcall predicate
                             (funcall key (elt sequence j))
                             (funcall key d)))))
        (incf j))
      (when (> j k) (return))
      (rotatef (elt sequence j) (elt sequence k))
      (incf j)
      (decf k))
    (quick-sort sequence start j predicate key)
    (quick-sort sequence j end predicate key)
    sequence))
