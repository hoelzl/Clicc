;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : System-Funktionen (15. Lists)                                  
;;;
;;; $Revision: 1.24 $
;;; $Log: list.lisp,v $
;;; Revision 1.24  1994/02/08  13:18:55  sma
;;; :my-last-arg-may-be-rest-var bei car, cdr, endp, list-length, first
;;; und rest.
;;;
;;; Revision 1.23  1994/02/02  09:43:08  hk
;;; car, cdr, rplaca, replacd und assoc mit Deklarationen versehen, die
;;; Optimierungen erlauben. Definition von simple-assoc vor assoc gezogen,
;;; da es in der Deklaration verwendet wird.
;;;
;;; Revision 1.22  1994/01/13  13:04:40  ft
;;; Die Faelle CONS und NULL im TYPECASE von CAR und CDR vertauscht.
;;;
;;; Revision 1.21  1994/01/05  12:40:37  sma
;;; rt::simple-assoc jetzt (endgültig) in Lisp implementiert. assoc auf
;;; allgemeinen Fall reduziert. assoc wird von clicc zur compile-Zeit durch
;;; rt::simple-assoc ersetzt wenn möglich.
;;;
;;; Revision 1.20  1993/12/13  13:15:55  sma
;;; Auskommentierte Funktion raw-list-length gelöscht
;;;
;;; Revision 1.19  1993/09/20  08:07:36  sma
;;; unnötigen Test in assoc entfernt.
;;;
;;; Revision 1.18  1993/09/16  14:34:41  sma
;;; raw-list-length ist jetzt eine C-Funktion in list.c
;;; assoc hat jetzt eine Unterfunktion simple-assoc (in C geschrieben),
;;; welche aufgerufen wird, wenn der Test eq ist und weder test-not noch
;;; key angegeben wurde. Dies sollte assoc deutlich beschleunigen, wenn
;;; als test 'eq angegeben wird.
;;;
;;; Revision 1.17  1993/08/16  17:20:00  hk
;;; copy-list kennt nun auch die leere Liste
;;;
;;; Revision 1.16  1993/07/19  16:12:50  hk
;;; Klammerfehler in asoc behoben
;;;
;;; Revision 1.15  1993/07/15  11:56:47  hk
;;; Macro apply-key zum optimierten Zugriff bei :key #'identity.
;;; assoc und rassoc optimiert fuer :test #'eql.
;;;
;;; Revision 1.14  1993/07/15  10:14:07  hk
;;; (copy-alist nil) -> nil
;;;
;;; Revision 1.13  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.12  1993/05/11  10:59:52  hk
;;; Kommentar geaendert.
;;;
;;; Revision 1.11  1993/05/11  10:53:27  hk
;;; first, .., fourth, rest und (setf first), .. definiert.
;;;
;;; Revision 1.10  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.9  1993/04/07  09:13:23  hk
;;; cons -> inline.lisp
;;;
;;; Revision 1.8  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.24 $ eingefuegt
;;;
;;; Revision 1.7  1993/02/16  10:25:01  ft
;;; Erweiterung um eine 'dumme' Version von nunion.
;;;
;;; Revision 1.6  1992/12/18  07:43:22  ft
;;; Erweiterung um SUBSETP.
;;;
;;; Revision 1.5  1992/12/16  09:09:07  ft
;;; Optimierung von INTERSECTION und SET-DIFFERENCE.
;;;
;;; Revision 1.4  1992/12/15  10:25:02  ft
;;; Erweiterung um SET-DIFFERENCE.
;;;
;;; Revision 1.3  1992/11/26  17:03:27  hk
;;; Funktionen aus list.c nach hier, car, cdr, set-car, etc.
;;;
;;; Revision 1.2  1992/07/06  09:10:46  hk
;;; Neue Syntax fuer declaim fun-spec.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(car cdr caar cadr cdar cddr caaar caadr cadar caddr cadar caddr cdaar cdadr
   cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
   cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr tree-equal endp list-length
   nth fifth sixth seventh eighth ninth tenth nthcdr last make-list copy-list
   copy-alist copy-tree revappend nconc nreconc butlast nbutlast ldiff
   rplaca rplacd subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not
   sublis nsublis member member-if member-if-not tailp adjoin union nunion
   intersection set-difference subsetp acons pairlis assoc assoc-if assoc-if-not
   rassoc rassoc-if rassoc-if-not))

(export 
 '(rt::simple-assoc) "RT")

;;-----------------------------------------------------------------------------
;; 15.1. Conses                                                               
;;-----------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; CAR x
;;------------------------------------------------------------------------------
(defun car (x)
  (declare (:simp-when-arg-n=cons 0 rt::%car)
           (:my-last-arg-may-be-rest-var :car))
  (typecase x
    (cons (rt::%car x))
    (null x)
    (t (error "~a is not a list" x))))

;;------------------------------------------------------------------------------
;; CDR x
;;------------------------------------------------------------------------------
(defun cdr (x)
  (declare (:simp-when-arg-n=cons 0 rt::%cdr)
           (:my-last-arg-may-be-rest-var :cdr))
  (typecase x
    (cons (rt::%cdr x))
    (null x)
    (t (error "~a is not a list" x))))

;;------------------------------------------------------------------------------
;; (SETF CAR) new-value x
;;------------------------------------------------------------------------------
(defun (setf car) (new-value x)
  (typecase x
    (cons (rt::%rplaca x new-value)
          new-value)
    (t (error "~a is not a cons" x))))

;;------------------------------------------------------------------------------
;; (SETF CDR) new-value x
;;------------------------------------------------------------------------------
(defun (setf cdr) (new-value x)
  (typecase x
    (cons (rt::%rplacd x new-value)
          new-value)
    (t (error "~a is not a cons" x))))

(defun   caar (list)           (car (car list)))
(defun   cadr (list)           (car (cdr list)))
(defun   cdar (list)           (cdr (car list)))
(defun   cddr (list)           (cdr (cdr list)))
(defun  caaar (list)      (car (car (car list))))
(defun  caadr (list)      (car (car (cdr list))))
(defun  cadar (list)      (car (cdr (car list))))
(defun  caddr (list)      (car (cdr (cdr list))))
(defun  cdaar (list)      (cdr (car (car list))))
(defun  cdadr (list)      (cdr (car (cdr list))))
(defun  cddar (list)      (cdr (cdr (car list))))
(defun  cdddr (list)      (cdr (cdr (cdr list))))
(defun caaaar (list) (car (car (car (car list)))))
(defun caaadr (list) (car (car (car (cdr list)))))
(defun caadar (list) (car (car (cdr (car list)))))
(defun caaddr (list) (car (car (cdr (cdr list)))))
(defun cadaar (list) (car (cdr (car (car list)))))
(defun cadadr (list) (car (cdr (car (cdr list)))))
(defun caddar (list) (car (cdr (cdr (car list)))))
(defun cadddr (list) (car (cdr (cdr (cdr list)))))
(defun cdaaar (list) (cdr (car (car (car list)))))
(defun cdaadr (list) (cdr (car (car (cdr list)))))
(defun cdadar (list) (cdr (car (cdr (car list)))))
(defun cdaddr (list) (cdr (car (cdr (cdr list)))))
(defun cddaar (list) (cdr (cdr (car (car list)))))
(defun cddadr (list) (cdr (cdr (car (cdr list)))))
(defun cdddar (list) (cdr (cdr (cdr (car list)))))
(defun cddddr (list) (cdr (cdr (cdr (cdr list)))))

(defun   (setf caar) (new list)           (setf (car (car list)) new))
(defun   (setf cadr) (new list)           (setf (car (cdr list)) new))
(defun   (setf cdar) (new list)           (setf (cdr (car list)) new))
(defun   (setf cddr) (new list)           (setf (cdr (cdr list)) new))
(defun  (setf caaar) (new list)      (setf (car (car (car list))) new))
(defun  (setf caadr) (new list)      (setf (car (car (cdr list))) new))
(defun  (setf cadar) (new list)      (setf (car (cdr (car list))) new))
(defun  (setf caddr) (new list)      (setf (car (cdr (cdr list))) new))
(defun  (setf cdaar) (new list)      (setf (cdr (car (car list))) new))
(defun  (setf cdadr) (new list)      (setf (cdr (car (cdr list))) new))
(defun  (setf cddar) (new list)      (setf (cdr (cdr (car list))) new))
(defun  (setf cdddr) (new list)      (setf (cdr (cdr (cdr list))) new))
(defun (setf caaaar) (new list) (setf (car (car (car (car list)))) new))
(defun (setf caaadr) (new list) (setf (car (car (car (cdr list)))) new))
(defun (setf caadar) (new list) (setf (car (car (cdr (car list)))) new))
(defun (setf caaddr) (new list) (setf (car (car (cdr (cdr list)))) new))
(defun (setf cadaar) (new list) (setf (car (cdr (car (car list)))) new))
(defun (setf cadadr) (new list) (setf (car (cdr (car (cdr list)))) new))
(defun (setf caddar) (new list) (setf (car (cdr (cdr (car list)))) new))
(defun (setf cadddr) (new list) (setf (car (cdr (cdr (cdr list)))) new))
(defun (setf cdaaar) (new list) (setf (cdr (car (car (car list)))) new))
(defun (setf cdaadr) (new list) (setf (cdr (car (car (cdr list)))) new))
(defun (setf cdadar) (new list) (setf (cdr (car (cdr (car list)))) new))
(defun (setf cdaddr) (new list) (setf (cdr (car (cdr (cdr list)))) new))
(defun (setf cddaar) (new list) (setf (cdr (cdr (car (car list)))) new))
(defun (setf cddadr) (new list) (setf (cdr (cdr (car (cdr list)))) new))
(defun (setf cdddar) (new list) (setf (cdr (cdr (cdr (car list)))) new))
(defun (setf cddddr) (new list) (setf (cdr (cdr (cdr (cdr list)))) new))

;;------------------------------------------------------------------------------
;; CONS x y:  siehe inline.lisp
;;------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; TREE-EQUAL x y &key :test :test-not
;;-----------------------------------------------------------------------------
(defun tree-equal (x y &key test test-not)
  (setq test (check-seq-test test test-not))
  (labels ((tree-equal-internal (x y)
             (if (atom x)
               (if (atom y)
                 (funcall test x y)
                 nil)
               (if (atom y)
                 nil
                 (and (tree-equal-internal (car x) (car y))
                      (tree-equal-internal (cdr x) (cdr y)))))))
    (tree-equal-internal x y)))

;;-----------------------------------------------------------------------------
;; 15.2. Lists                                                                
;;-----------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ENDP object
;;------------------------------------------------------------------------------
(defun endp (x)
  (declare (:my-last-arg-may-be-rest-var :atom))
  (typecase x
    (null t)
    (cons nil)
    (t (error "~a is not a list" x))))

;;-----------------------------------------------------------------------------
;; LIST-LENGTH list
;; source-code from [CLtL2] p. 414
;;-----------------------------------------------------------------------------
(defun list-length (list)
  (declare (:my-last-arg-may-be-rest-var :length))
  (do ((n 0 (+ n 2))
       (fast list (cdr fast))
       (slow list (cdr slow)))
      (nil)
    (when (endp fast) (return n))
    (pop fast)
    (when (endp fast) (return (1+ n)))
    (when (eq fast slow) (return nil))))

;;-----------------------------------------------------------------------------
;; NTH n list
;;-----------------------------------------------------------------------------
(defun nth (n list)
  (car (nthcdr n list)))

;;-----------------------------------------------------------------------------
;; (SETF NTH) newvalue n list
;;-----------------------------------------------------------------------------
(defun (setf nth) (newvalue n list)
  (setf (car (nthcdr n list)) newvalue))

;;------------------------------------------------------------------------------
;; Besser waere:
;; (setf (fdefinition 'first) #'car)
;; (setf (fdefinition '(setf first)) #'(setf car)) ...
;;------------------------------------------------------------------------------
(defun first (list) (declare (:my-last-arg-may-be-rest-var :car)) (car list))
(defun rest  (list) (declare (:my-last-arg-may-be-rest-var :cdr)) (cdr list))
(defun second  (list) (cadr   list))
(defun third   (list) (caddr  list))
(defun fourth  (list) (cadddr list))
(defun fifth   (list) (nth 4 list))
(defun sixth   (list) (nth 5 list))
(defun seventh (list) (nth 6 list))
(defun eighth  (list) (nth 7 list))
(defun ninth   (list) (nth 8 list))
(defun tenth   (list) (nth 9 list))

(defun (setf first)   (new list) (setf (car    list) new))
(defun (setf rest)    (new list) (setf (cdr    list) new))
(defun (setf second)  (new list) (setf (cadr   list) new))
(defun (setf third)   (new list) (setf (caddr  list) new))
(defun (setf fourth)  (new list) (setf (cadddr list) new))
(defun (setf fifth)   (new list) (setf (nth 4 list) new))
(defun (setf sixth)   (new list) (setf (nth 5 list) new))
(defun (setf seventh) (new list) (setf (nth 6 list) new))
(defun (setf eighth)  (new list) (setf (nth 7 list) new))
(defun (setf ninth)   (new list) (setf (nth 8 list) new))
(defun (setf tenth)   (new list) (setf (nth 9 list) new))

;;-----------------------------------------------------------------------------
;; NTHCDR n list
;;-----------------------------------------------------------------------------
(defun nthcdr (n list)
  (loop
    (cond
      ((zerop n)
       (return list))
      ((null list)
       (return nil))
      (t (decf n)
         (pop list)))))
		
;;-----------------------------------------------------------------------------
;; LAST list &OPTIONAL (n 1)
;;-----------------------------------------------------------------------------
(defun last (list &optional (n 1))
  (when (not (typep n '(integer 0 *)))
    (error "~A is an illegal index" n))
  (let ((i (- (length list) n)))
    (cond
      ((<= i 0) list)
      (t (nthcdr i list)))))

;;-----------------------------------------------------------------------------
;; MAKE-LIST size &key :initial-element
;;-----------------------------------------------------------------------------
(defun make-list (size &key initial-element)
  (when (not (typep size '(integer 0 *)))
    (error "make-list: ~A is an illegal size" size))
  (labels ((f (size elt)
             (if (= size 0)
                 ()
                 (cons elt (f (1- size) elt)))))
    (f size initial-element)))

;;------------------------------------------------------------------------------
;; COPY-LIST list
;;------------------------------------------------------------------------------
(defun copy-list (list)
  (typecase list
    (null list)
    (atom (error "~a is not a list" list))
    (t (labels ((f (list)
                  (if (atom list)
                      list
                      (cons (car list) (f (cdr list))))))
         (f list)))))

;;-----------------------------------------------------------------------------
;; COPY-ALIST list
;;-----------------------------------------------------------------------------
(defun copy-alist (alist)
  (etypecase alist
    (null alist)
    (cons
     (labels ((f (alist)
                (if (atom alist)
                    alist
                    (cons (if (atom (car alist))
                              (car alist)
                              (cons (caar alist) (cdar alist)))
                          (f (cdr alist))))))
       (f alist)))))

;;-----------------------------------------------------------------------------
;; COPY-TREE object
;;-----------------------------------------------------------------------------
(defun copy-tree (object)
  (if (atom object)
      object
      (cons (copy-tree (car object)) (copy-tree (cdr object)))))

;;-----------------------------------------------------------------------------
;; REVAPPEND x y
;;-----------------------------------------------------------------------------
(defun revappend (x y)
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

;;-----------------------------------------------------------------------------
;; NCONC &rest lists
;;-----------------------------------------------------------------------------
(defun nconc (&rest lists)
  (if (null lists)
    ()
    (let ((last-list (pop lists)))
      (if (null last-list)
        (apply #'nconc lists)
        (do ((result last-list)
             next-list)
            ((null lists) result)
          (setq next-list (pop lists))
          (when next-list
            (rplacd (last last-list) next-list)
            (setq last-list next-list)))))))

;;-----------------------------------------------------------------------------
;; NRECONC x y
;;-----------------------------------------------------------------------------
(defun nreconc (x y)
  (nconc (reverse x) y))

;;-----------------------------------------------------------------------------
;; BUTLAST list &optional n
;;-----------------------------------------------------------------------------
(defun butlast (list &optional (n 1))
  (let ((size (- (length list) n)))
    (if (<= size 0)
      ()
      (subseq list 0 size))))

;;-----------------------------------------------------------------------------
;; NBUTLAST list &optional n
;;-----------------------------------------------------------------------------
(defun nbutlast (list &optional (n 1))
  (let ((size (- (length list) n))
        last-cons)
    (cond
      ((<= size 0) ())
      (t (setq last-cons (nthcdr (1- size) list))
         (rplacd last-cons nil)
         list))))

;;-----------------------------------------------------------------------------
;; LDIFF list sublist
;;-----------------------------------------------------------------------------
(defun ldiff (l subl)
  (cond
    ((or (null l) (eq l subl)) ())
    (t (cons (first l) (ldiff (rest l) subl)))))

;;------------------------------------------------------------------------------
;; 15.3. Alteration of List Structure
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; RPLACA x y
;;------------------------------------------------------------------------------
(defun rplaca (x y)
  (declare (:simp-when-arg-n=cons 0 rt::%rplaca))
  (typecase x
    (cons (rt::%rplaca x y))
    (t (error "~a is not a cons" x))))

;;------------------------------------------------------------------------------
;; RPLACD x y
;;------------------------------------------------------------------------------
(defun rplacd (x y)
  (declare (:simp-when-arg-n=cons 0 rt::%rplacd))
  (typecase x
    (cons (rt::%rplacd x y))
    (t (error "~a is not a cons" x))))

;;-----------------------------------------------------------------------------
;; 15.4. Substitution of Expressions                                          
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; APPLY-KEY saves us a function call sometimes.
;;-----------------------------------------------------------------------------
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

;;-----------------------------------------------------------------------------
;; SUBST new old tree &key :test :test-not :key
;;-----------------------------------------------------------------------------
(defun subst (new old tree &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((subst-internal (tree)
             (cond
               ((funcall test old (apply-key key tree))
                new)
               ((atom tree) tree)
               (t (let* ((old-car (car tree))
                         (old-cdr (cdr tree))
                         (new-car (subst-internal old-car))
                         (new-cdr (subst-internal old-cdr)))
                    (if (and (eql new-car old-car)
                             (eql new-cdr old-cdr))
                      tree
                      (cons new-car new-cdr)))))))
    (subst-internal tree)))

;;-----------------------------------------------------------------------------
;; SUBST-IF new test tree &key :key
;;-----------------------------------------------------------------------------
(defun subst-if (new test tree &key key)
  (labels ((subst-if-internal (tree)
             (cond
               ((funcall test (apply-key key tree))
                new)
               ((atom tree) tree)
               (t (let* ((old-car (car tree))
                         (old-cdr (cdr tree))
                         (new-car (subst-if-internal old-car))
                         (new-cdr (subst-if-internal old-cdr)))
                    (if (and (eql new-car old-car)
                             (eql new-cdr old-cdr))
                      tree
                      (cons new-car new-cdr)))))))
    (subst-if-internal tree)))

;;-----------------------------------------------------------------------------
;; SUBST-IF-NOT new test tree &key :key
;;-----------------------------------------------------------------------------
(defun subst-if-not (new test tree &key key)
  (subst-if new (complement test) tree :key key))

;;-----------------------------------------------------------------------------
;; NSUBST new old tree &key :test :test-not :key
;;-----------------------------------------------------------------------------
(defun nsubst (new old tree &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((nsubst-internal (tree)
             (cond
               ((funcall test old (apply-key key tree))
                new)
               ((atom tree) tree)
               (t (let ((new-car (nsubst-internal (car tree)))
                        (new-cdr (nsubst-internal (cdr tree))))
                    (rplaca tree new-car)
                    (rplacd tree new-cdr)
                    tree)))))
    (nsubst-internal tree)))

;;-----------------------------------------------------------------------------
;; NSUBST-IF new test tree &KEY :key
;;-----------------------------------------------------------------------------
(defun nsubst-if (new test tree &key key)
  (labels ((nsubst-if-internal (tree)
             (cond
               ((funcall test (apply-key key tree))
                new)
               ((atom tree) tree)
               (t (let ((new-car (nsubst-if-internal (car tree)))
                        (new-cdr (nsubst-if-internal (cdr tree))))
                    (rplaca tree new-car)
                    (rplacd tree new-cdr)
                    tree)))))
    (nsubst-if-internal tree)))

;;-----------------------------------------------------------------------------
;; NSUBST-IF-NOT new test tree &KEY :key
;;-----------------------------------------------------------------------------
(defun nsubst-if-not (new test tree &key key)
   (nsubst-if new (complement test) tree :key key))

;;-----------------------------------------------------------------------------
;; SUBLIS alist tree &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun sublis (alist tree &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((sublis-internal (tree)
             (let ((acons (assoc tree alist :test test :key key)))
               (if acons
                 (cdr acons)
                 (let* ((old-car (car tree))
                        (old-cdr (cdr tree))
                        (new-car (sublis-internal old-car))
                        (new-cdr (sublis-internal old-cdr)))
                   (if (and (eql new-car old-car)
                            (eql new-cdr old-cdr))
                     tree
                     (cons new-car new-cdr)))))))
    (sublis-internal tree)))

;;-----------------------------------------------------------------------------
;; NSUBLIS alist tree &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun nsublis (alist tree &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((nsublis-internal (tree)
             (let ((acons (assoc tree alist :test test :key key)))
               (if acons
                 (cdr acons)
                 (let ((new-car (nsublis-internal (car tree)))
                       (new-cdr (nsublis-internal (cdr tree))))
                   (rplaca tree new-car)
                   (rplacd tree new-cdr)
                   tree)))))
    (nsublis-internal tree)))

;;-----------------------------------------------------------------------------
;; 15.5. Using Lists as Sets                                                  
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; MEMBER item list &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun member (item list &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((member-internal (list)
             (cond
               ((null list) ())
               ((funcall test item (apply-key key (car list)))
                list)
               (t (member-internal (rest list))))))
    (member-internal list)))

;;-----------------------------------------------------------------------------
;; MEMBER-IF predicate list &KEY :key
;;-----------------------------------------------------------------------------
(defun member-if (predicate list &key key)
  (labels ((member-if-internal (list)
             (cond
               ((null list) ())
               ((funcall predicate (apply-key key (car list)))
                list)
               (t (member-if-internal (rest list))))))
    (member-if-internal list))) 

;;-----------------------------------------------------------------------------
;; MEMBER-IF-NOT predicate list &KEY :key
;;-----------------------------------------------------------------------------
(defun member-if-not (predicate list &key key)
   (member-if (complement predicate) list :key key))

;;-----------------------------------------------------------------------------
;; TAILP sublist list
;;-----------------------------------------------------------------------------
(defun tailp (sublist list)
  (cond
    ((eql sublist list) t)
    ((atom list) nil)
    (t (tailp sublist (cdr list)))))

;;-----------------------------------------------------------------------------
;; ADJOIN item list &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun adjoin (item list &key test test-not key)
  (setq test (check-seq-test test test-not))
  (if (member (apply-key key item) list :test test :key key)
    list
    (cons item list)))

;;-----------------------------------------------------------------------------
;; UNION list1 list2 &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun union (list1 list2 &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((union-internal (list1)
             (cond
               ((null list1) list2)
               ((member (apply-key key (car list1)) list2 :test test :key key)
                (union-internal (cdr list1)))
               (t (cons (car list1) (union-internal (cdr list1)))))))
    (union-internal list1)))

;;-----------------------------------------------------------------------------
;; NUNION list1 list2 &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun nunion (list1 list2 &key test test-not key)
  (setq test (check-seq-test test test-not))
  (labels ((union-internal (list1)
             (cond
               ((null list1) list2)
               ((member (apply-key key (car list1)) list2 :test test :key key)
                (union-internal (cdr list1)))
               (t (cons (car list1) (union-internal (cdr list1)))))))
    (union-internal list1)))

;;-----------------------------------------------------------------------------
;; INTERSECTION list1 list2 &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun intersection (list1 list2 &key test test-not key)
  (setq test (check-seq-test test test-not))
  (let (intersection)
    (dolist (elem1 list1)
      (when (member (apply-key key elem1) list2 :test test :key key)
          (push elem1 intersection)))
    intersection))

;;-----------------------------------------------------------------------------
;; SET-DIFFERENCE list1 list2 &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun set-difference (list1 list2 &key test test-not key)
  (setq test (check-seq-test test test-not))
  (let (set-difference)
    (dolist (elem1 list1)
      (unless (member (apply-key key elem1) list2 :test test :key key)
          (push elem1 set-difference)))
    set-difference))

;;------------------------------------------------------------------------------
;; SUBSETP list1 list2 &KEY :test :test-not :key
;;------------------------------------------------------------------------------
(defun subsetp (list1 list2 &key test test-not key)
  (setq test (check-seq-test test test-not))
  (dolist (elem1 list1 T)
    (unless (member (apply-key key elem1) list2 :test test :key key)
      (return nil))))

;;-----------------------------------------------------------------------------
;; 15.6. Association Lists                                                    
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ACONS key datum a-list
;;-----------------------------------------------------------------------------
(defun acons (key datum a-list)
  (cons (cons key datum) a-list))

;;-----------------------------------------------------------------------------
;; PAIRLIS keys data &OPTIONAL a-list
;;-----------------------------------------------------------------------------
(defun pairlis (keys data &optional a-list)
  (tagbody

   BEGIN
     (cond
       ((atom keys)
        (if (atom data)
          (return-from pairlis a-list)
          (go ERROR)))
       ((atom data) (go ERROR))
       (t (setq a-list (acons (first keys) (first data) a-list))
          (pop keys)
          (pop data)))
     (go BEGIN)
   ERROR
     (error "The lists of keys and data are of unequal length.")))

;;------------------------------------------------------------------------------
;; RT::SIMPLE-ASSOC item a-list
;;------------------------------------------------------------------------------
(defun rt:simple-assoc (item a-list)
  (dolist (pair a-list nil)
    (when (eq item (car pair))
      (return pair))))

;;-----------------------------------------------------------------------------
;; ASSOC item a-list &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun assoc (item a-list &key test test-not key)
  (declare
   (:simp-test-fun-when-not-testnot 0 2 :test eql :test-not)
   (:simp-when-only-test=value 2 :test eq rt::simple-assoc))
  (setq test (check-seq-test test test-not))
  (cond
    ((and (eq test #'eq) (not key))
     (rt::simple-assoc item a-list))
    ((eq test #'eql)
      (dolist (pair a-list nil)
        (when (eql item (apply-key key (car pair)))
          (return-from assoc pair))))
    (t 
     (dolist (pair a-list nil)
       (when (funcall test item (apply-key key (car pair)))
         (return-from assoc pair))))))

;;-----------------------------------------------------------------------------
;; ASSOC-IF predicate a-list &KEY :key
;;-----------------------------------------------------------------------------
(defun assoc-if (predicate a-list &key key)
  (dolist (pair a-list nil)
    (when (funcall predicate (car pair) (apply-key key (car pair)))
      (return-from assoc-if pair))))

;;-----------------------------------------------------------------------------
;; ASSOC-IF-NOT predicate a-list &KEY :key
;;-----------------------------------------------------------------------------
(defun assoc-if-not (predicate a-list &key key)
  (dolist (pair a-list nil)
    (when (not (funcall predicate (apply-key key (car pair))))
      (return-from assoc-if-not pair))))

;;-----------------------------------------------------------------------------
;; RASSOC item a-list &KEY :test :test-not :key
;;-----------------------------------------------------------------------------
(defun rassoc (item a-list &key test test-not key)
  (setq test (check-seq-test test test-not))
  (if (eq test #'eql)
      (dolist (pair a-list nil)
        (when (eql item (apply-key key (car pair)))
          (return-from rassoc pair)))
      (dolist (pair a-list nil)
        (when (funcall test item (apply-key key (cdr pair)))
          (return-from rassoc pair)))))

;;-----------------------------------------------------------------------------
;; RASSOC-IF predicate a-list &KEY :key
;;-----------------------------------------------------------------------------
(defun rassoc-if (predicate a-list &key key)
  (dolist (pair a-list nil)
    (when (funcall predicate (apply-key key (cdr pair)))
      (return-from rassoc-if pair))))

;;-----------------------------------------------------------------------------
;; RASSOC-IF-NOT predicate a-list &KEY :key
;;-----------------------------------------------------------------------------
(defun rassoc-if-not (predicate a-list &key key)
  (dolist (pair a-list nil)
    (when (not (funcall predicate (apply-key key (cdr pair))))
      (return-from rassoc-if-not pair)))) 
