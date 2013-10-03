;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem
;;;            - Definition von PACKAGE als LISP-Struktur
;;;            - alle Package-Funktionen
;;;            - Initialisierung der Packages beim Start des Programms
;;;
;;; $Revision: 1.17 $
;;; $Log: packg.lisp,v $
;;; Revision 1.17  1994/06/02  13:20:18  hk
;;; Print-Funktion f"ur package-Struktur von write2 nach hier.
;;;
;;; Revision 1.16  1994/01/21  13:26:34  sma
;;; set-symbol-package statt (setf rt::symbol-package).
;;;
;;; Revision 1.15  1994/01/13  16:44:57  sma
;;; rt::set-symbol-package -> setf rt::set-symbol-package. Sourcecode
;;; verschönert.
;;;
;;; Revision 1.14  1993/12/09  17:12:38  sma
;;; *package-array* und *keyword-package* jetzt statt in startup.lisp hier
;;; als toplevel-Form definiert. rt::string-hash -> string-hash. Teilweise
;;; neu eingerückt.
;;;
;;; Revision 1.13  1993/12/08  00:01:00  hk
;;; in-package nimmt nur dann den Defaultwert '("LISP") für die Use-List,
;;; wenn das Package neu generiert wird und kein anderer Wert angegeben
;;; ist.
;;;
;;; Revision 1.12  1993/12/08  00:00:00  wg
;;; unuse-package: package-name coerced to package.
;;;
;;; Revision 1.11  1993/10/12  14:48:43  hk
;;; Fehler in unexport behoben: find-package wurde nur mit 1 Argument
;;; aufgerufen, so daß der Wert von *package* statt von package gelesen
;;; wurde.
;;;
;;; Revision 1.10  1993/07/06  15:23:35  hk
;;; rt:ensure-package ruft nun make-package mit :use () auf.
;;;
;;; Revision 1.9  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.8  1993/05/19  11:47:47  hk
;;; *package-info* gestrichen.
;;;
;;; Revision 1.7  1993/05/12  11:35:39  hk
;;; rt:setup-symbol benutzt nun rt::symbol-package-index, statt symbol-package.
;;;
;;; Revision 1.6  1993/05/07  08:57:48  hk
;;; package exportiert.
;;;
;;; Revision 1.5  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; Revision-Keyword eingefuegt
;;;
;;; Revision 1.3  1992/09/25  13:40:43  hk
;;; Sichergestellt, dass string-hash nur Strings als Argument erhaelt.
;;;
;;; Revision 1.2  1992/07/06  09:15:38  hk
;;; Text anders formatiert.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(*package* package packagep make-package package-name package-nicknames
   in-package find-package rename-package package-use-list package-used-by-list
   package-shadowing-symbols list-all-packages intern find-symbol unintern
   export unexport import shadowing-import shadow use-package unuse-package
   find-all-symbols))
(export
 '(rt::ensure-package rt::do-symbols-iterator rt::do-external-symbols-iterator
   rt::do-all-symbols-iterator rt::setup-symbol) "RT")

;;------------------------------------------------------------------------------
(defconstant package-hashtab-size 101)

;;------------------------------------------------------------------------------
(defstruct (package (:copier nil)
                    (:predicate packagep)
                    (:conc-name "%PACKAGE-")
                    (:constructor raw-make-package)
                    (:print-function
                     (lambda (package stream depth)
                       (write-string "#<Package \"" stream)
                       (write-string (package-name package) stream)
                       (write-string "\">" stream))))
  (name "" :type string)
  (nicknames () :type list)
  (internal
   (make-array PACKAGE-HASHTAB-SIZE
               :initial-element nil
               :element-type 'list)
   :type (array list *))
  (external (make-array PACKAGE-HASHTAB-SIZE
                        :initial-element nil
                        :element-type 'list)
            :type (array list *))
  (shadowings () :type list)
  (use-list () :type list)
  (used-by-list () :type list))

;;------------------------------------------------------------------------------
(defvar *package*)
(defvar *package-array* (make-array 6 :fill-pointer 0 :adjustable t)) 
(defvar *keyword-package* (make-package "KEYWORD" :nicknames '("") :use ()))

;;--------------------------------------------------------------------------
;; packagep package
;;--------------------------------------------------------------------------
;; structure-predicate

;;--------------------------------------------------------------------------
;; package-name package
;;--------------------------------------------------------------------------
(defun package-name (package)
  (%package-name package))

;;--------------------------------------------------------------------------
;; package-nicknames package
;;--------------------------------------------------------------------------
(defun package-nicknames (package)
  (%package-nicknames package))

;;-----------------------------------------------------------------------------
(defun del-pack-sym (symbol pack-hash index)
  (setf (svref pack-hash index)
        (delete symbol (svref pack-hash index) :count 1)))

;;--------------------------------------------------------------------------
(defun coerce-to-package (p)
  (when (symbolp p)
    (setq p (symbol-name p)))
  (when (stringp p)
    (setq p (find-package p)))
  (unless (packagep p)
    (error "there is no Package with name ~S" p))
  p)

;;--------------------------------------------------------------------------
(defun add-nicknames (nicknames package)
  (dolist (nickname nicknames)
    (when (symbolp nickname)
      (setq nickname (symbol-name nickname)))
    (let ((p  (find-package nickname)))
      (cond
        ((eq p package) nil)
        (p (error "package with name ~S already exists"))
        (t (push nickname (%package-nicknames package)))))))

;;--------------------------------------------------------------------------
;; make-package package-name &key :nicknames :use
;;--------------------------------------------------------------------------
(defun make-package (name &key (nicknames ()) (use '("LISP")))
  (let (package)

    (when (symbolp name)
      (setq name (symbol-name name)))

    (when (find-package name)
      (error "package with name ~S already exists" name))

    (setq package (raw-make-package :name name))
    (vector-push-extend package *package-array*)

    (use-package use package)
    (add-nicknames nicknames package)

    package))

;;------------------------------------------------------------------------------
;; ensure-package name
;;------------------------------------------------------------------------------
(defun rt:ensure-package (name)
  (let ((package (find-package name)))
    (if package
        package
        (make-package name :use ()))))

;;--------------------------------------------------------------------------
;; in-package name &key nicknames use
;;--------------------------------------------------------------------------
(defun in-package (name &key (nicknames ()) (use '() supplied))
  (let ((package (find-package name)))
    (cond
      (package
       (add-nicknames nicknames package)
       (use-package use package)
       (setq *package* package))
      (T (setq *package*
               (if supplied
                   (make-package name :nicknames nicknames :use use)

                   ;; Den Defaultwert bei make-package bestimmen
                   ;;-------------------------------------------
                   (make-package name :nicknames nicknames)))))))

;;--------------------------------------------------------------------------
;; find-package name
;;--------------------------------------------------------------------------
(defun find-package (name)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (map nil #'(lambda (package)
               (when (string= name (%package-name package))
                 (return-from find-package package))
               (dolist (nickname (%package-nicknames package))
                 (when (string= nickname name)
                   (return-from find-package package))))
       *package-array*))

;;--------------------------------------------------------------------------
;; rename-package package new-name &optional new-nicknames
;;--------------------------------------------------------------------------
(defun rename-package (package new-name &optional new-nicknames)
  (when (symbolp new-name)
    (setq new-name (symbol-name new-name)))
  (setq package (coerce-to-package package))
  (let ((p2 (find-package new-name)))
    (when (and p2 (not (eq package p2)))
      (error "package with name ~S already exists")))
  (setf (%package-name package) new-name)
  (setf (%package-nicknames package) ())
  (add-nicknames new-nicknames package)
  package)

;;--------------------------------------------------------------------------
;; package-use-list package
;;--------------------------------------------------------------------------
(defun package-use-list (package)
  (%package-use-list (coerce-to-package package)))

;;--------------------------------------------------------------------------
;; package-use-list package
;;--------------------------------------------------------------------------
(defun package-used-by-list (package)
  (%package-used-by-list (coerce-to-package package)))

;;--------------------------------------------------------------------------
;; package-shadowing-symbols package
;;--------------------------------------------------------------------------
(defun package-shadowing-symbols (package)
  (%package-shadowings (coerce-to-package package)))

;;--------------------------------------------------------------------------
;; list-all-packages
;;--------------------------------------------------------------------------
(defun list-all-packages ()
  (concatenate 'list *package-array*))

;;--------------------------------------------------------------------------
;; intern string &optional package
;;--------------------------------------------------------------------------
(defun intern (string &optional (package *package*))
  (setq package (coerce-to-package package))
  
  (multiple-value-bind (symbol where) (find-symbol string package)
    (if where
        
        ;; Symbol ist schon bekannt
        ;;-------------------------
        (values symbol where)
        
        (progn
          (setq symbol (make-symbol string))
          (internal-import symbol package)
          
          ;; neues Symbol angelegt
          ;;----------------------
          (values symbol nil)))))

;;--------------------------------------------------------------------------
;; find-symbol string &optional package
;;--------------------------------------------------------------------------
(defun find-symbol (string &optional (package *package*))
  (unless (stringp string)
    (error "~a is not a string" string))
  (let ((hash-index (string-hash string PACKAGE-HASHTAB-SIZE)))
    
    (setq package (coerce-to-package package))
    
    (dolist (symbol (svref (%package-external package) hash-index))
      (when (string= string (symbol-name symbol))
        
        ;; external Symbol gefunden
        ;;-------------------------
        (return-from find-symbol (values symbol :external))))
    
    (dolist (symbol (svref (%package-internal package) hash-index))
      (when (string= string (symbol-name symbol))
        
        ;; internal Symbol gefunden
        ;;-------------------------
        (return-from find-symbol (values symbol :internal))))
    
    (dolist (package (%package-use-list package))
      (dolist (symbol (svref (%package-external package) hash-index))
        (when (string= string (symbol-name symbol))
          
          ;; inherited Symbol gefunden
          ;;--------------------------
          (return-from find-symbol (values symbol :inherited)))))
    
    ;; Symbol nicht gefunden
    ;;----------------------
    (values nil nil)))

;;--------------------------------------------------------------------------
;; unintern symbol &optional package
;;--------------------------------------------------------------------------
(defun unintern (symbol &optional (package *package*))
  (let (found
        where
        (string (symbol-name symbol))
        hash-table)
    
    (setq package (coerce-to-package package))
    
    (multiple-value-setq (found where)
      (find-symbol string package))
    (if (and (eq found symbol)
             (or (eq :internal where) (eq :external where)))
        (progn
          (setq hash-table (if (eq :external where)
                               (%package-external package)
                               (%package-internal package)))
          
          (when (member symbol (%package-shadowings package))
            
            ;; pruefen, ob nach dem Entfernen von SYMBOL ein
            ;; Names-Konflikt eintreten wuerde.
            ;;----------------------------------------------
            (let ((sym1 0))
              (dolist (used-package (%package-use-list package))
                (multiple-value-setq (found where)
                  (find-symbol string used-package))
                (when (eq where :external)
                  (if (eql sym1 0)
                      (setq sym1 found)
                      (unless (eq found sym1)
                        (error "~S and ~S will cause a name conflict"
                               sym1 found))))))
            (setf (%package-shadowings package)
                  (delete symbol (%package-shadowings package)
                          :count 1)))
      
          (del-pack-sym symbol hash-table
                        (string-hash string PACKAGE-HASHTAB-SIZE))
          (when (eq (symbol-package symbol) package)
            (set-symbol-package symbol nil))
        
          T)
      
        ;; Symbol nicht entfernt
        ;;----------------------
        nil)))

;;--------------------------------------------------------------------------
;; export symbols &optional package
;;--------------------------------------------------------------------------
(defun export (symbols &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (dolist (symbol symbols)
    (multiple-value-bind (found where)
        (find-symbol (symbol-name symbol) package)
      
      (when (or (eq nil where) (not (eq found symbol)))
        (error "~S is not accessible in ~S" symbol package))
      
      ;; ist schon extern, nichts zu tun
      ;;--------------------------------
      (when (eq :external where)
        (return))
      
      ;; werden Namens-Konfikte auftreten ?
      ;;-----------------------------------
      (dolist (using-package (%package-used-by-list package))
        (multiple-value-bind (found where)
            (find-symbol (symbol-name symbol) using-package) 
          (when (and where
                     (not (eq found symbol))
                     (not (member found (%package-shadowings
                                         using-package))))
            (error "~S will cause a name conflict in ~S"
                   symbol using-package))))

      ;; internal --> external
      ;;----------------------
      (let ((hash-index (string-hash (symbol-name symbol)
                                     PACKAGE-HASHTAB-SIZE)))
        (when (eq :internal where)
          (del-pack-sym symbol
                        (%package-internal package)
                        hash-index))
        (push symbol
              (svref (%package-external package) hash-index)))))
  t)

;;--------------------------------------------------------------------------
;; unexport symbols &optional package
;;--------------------------------------------------------------------------
(defun unexport (symbols &optional (package *package*))
  (setq package (coerce-to-package package))
  (when (eq *keyword-package* package)
    (error "can't unexport from ~S" package))
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (dolist (symbol symbols)
    (multiple-value-bind (found where)
        (find-symbol (symbol-name symbol) package)
      (when (or (eq nil where) (not (eq found symbol)))
        (error "~S is not accessible in ~S" symbol package))
      (when (eq :external where)

        ;; external --> internal
        ;;----------------------
        (let ((hash-index (string-hash (symbol-name symbol)
                                       PACKAGE-HASHTAB-SIZE)))
          (del-pack-sym symbol
                        (%package-external package)
                        hash-index)
          (push symbol
                (svref (%package-internal package) hash-index))))))
  t)

;;--------------------------------------------------------------------------
;; import symbols &optional package
;;--------------------------------------------------------------------------
(defun import (symbols &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (dolist (symbol symbols)
    (multiple-value-bind (found where)
        (find-symbol (symbol-name symbol) package) 
      (when where
        (when (not (eq symbol found))
          (error "a symbol ~S is already present in package ~S"
                 found package))
        (unless (eq :inherited where)
          (return-from import)))
      (internal-import symbol package)))
  t)

;;--------------------------------------------------------------------------
(defun internal-import (symbol package)
  (let ((hash-index (string-hash (symbol-name symbol)
                                 PACKAGE-HASHTAB-SIZE)))
    (unless (symbol-package symbol)
      (set-symbol-package symbol package))
    
    (if (eq package *keyword-package*)
        ;; Steele (S. 175): whenever a symbol is added to the keyword package
        ;; the symbol is always made external; the symbol is also automatically
        ;; declared to be a constant and made to have itself as its value.
        ;;-------------------------------------------------------------------
        (progn
        
          ;; Error, wenn eine Konstante in das Keyword-Package importiert
          ;; wird.
          ;;-------------------------------------------------------------
          (set symbol symbol)
          (rt::set-constant-flag symbol)
          (push symbol (svref (%package-external package) hash-index)))
        (push symbol (svref (%package-internal package) hash-index)))))

;;--------------------------------------------------------------------------
;; shadowing-import symbols &optional package
;;--------------------------------------------------------------------------
(defun shadowing-import (symbols &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (dolist (symbol symbols)
    (multiple-value-bind (found where)
        (find-symbol (symbol-name symbol) package)

      ;; evtl. schon vorhandenes Symbol gleichen Namens entfernen
      ;;---------------------------------------------------------
      (let ((hash-index (string-hash (symbol-name symbol)
                                     PACKAGE-HASHTAB-SIZE)))
        (case where
          (:internal (del-pack-sym found
                                   (%package-internal package)
                                   hash-index))
          (:external (del-pack-sym found
                                   (%package-external package)
                                   hash-index)))))
    (internal-import symbol package)
    (push symbol (%package-shadowings package)))
  t)

;;--------------------------------------------------------------------------
;; shadow symbols &optional package
;;--------------------------------------------------------------------------
(defun shadow (symbols &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (dolist (symbol symbols)
    (let ((name (symbol-name symbol)))
      (multiple-value-bind (found where)
          (find-symbol name package)
        (declare (ignore found))
        (case where
          ((:internal :external) nil)
          (T (setq symbol (make-symbol name))
             (internal-import symbol package)))
           
        (push symbol (%package-shadowings package)))))
  t)

;;--------------------------------------------------------------------------
;; use-package to-use &optional package
;;--------------------------------------------------------------------------
(defun use-package (to-use &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp to-use)
    (setq to-use (list to-use)))
  (dolist (use to-use)
    (setq use (coerce-to-package use))
    (when (eq use *keyword-package*)
      (error "can't use ~S" use))
    (unless (member use (%package-use-list package))
      (do-external-symbols (s use)
        (multiple-value-bind (found where)
            (find-symbol (symbol-name s) package)
          (when (and where
                     (not (eq s found))
                     (not (member found
                                  (%package-shadowings package))))
            (error "~S in ~S conflicts with ~S in ~S"
                   found package s use))))

      (push use (%package-use-list package))
      (push package (%package-used-by-list use))))
  t)

;;--------------------------------------------------------------------------
;; unuse-package to-unuse &optional package
;;--------------------------------------------------------------------------
(defun unuse-package (to-unuse &optional (package *package*))
  (setq package (coerce-to-package package))
  (unless (listp to-unuse)
    (setq to-unuse (list to-unuse)))
  (dolist (unuse to-unuse)
    (setq unuse (coerce-to-package unuse))
    (setf (%package-use-list package)
          (delete unuse (%package-use-list package) :count 1))
    (setf (%package-used-by-list unuse)
          (delete package (%package-use-list unuse) :count 1)))
  t)

;;--------------------------------------------------------------------------
;; find-all-symbols string-or-symbol
;;--------------------------------------------------------------------------
(defun find-all-symbols (name)
  (let ((symbols ()))
    (when (symbolp name)
      (setq name (symbol-name name)))
    (map 'nil
         #'(lambda (package)
      
             ;; hier werden unnoetigerweise auch :inherited Symbole gesucht !
             ;; (evtl. optimieren)
             ;;--------------------------------------------------------------
             (multiple-value-bind (found where)
                 (find-symbol name package)
               (when (member where '(:internal :external))
                 (pushnew found symbols))))
         *package-array*)
    symbols))

;;--------------------------------------------------------------------------
;; do-symbols-iterator function package
;;--------------------------------------------------------------------------
(defun rt:do-symbols-iterator (function package)
  (let ((hash-tab (%package-external package)))
    (dotimes (i PACKAGE-HASHTAB-SIZE)
      (dolist (s (svref hash-tab i))
        (funcall function s)))
    (setq hash-tab (%package-internal package))
    (dotimes (i PACKAGE-HASHTAB-SIZE)
      (dolist (s (svref hash-tab i))
        (funcall function s)))
    (dolist (use (%package-use-list package))
      (setq hash-tab (%package-internal use))
      (dotimes (i PACKAGE-HASHTAB-SIZE)
        (dolist (s (svref hash-tab i))
          (funcall function s))))))

;;--------------------------------------------------------------------------
;; do-external-symbols-iterator function package
;;--------------------------------------------------------------------------
(defun rt:do-external-symbols-iterator (function package)
  (let ((hash-tab (%package-external package)))
    (dotimes (i PACKAGE-HASHTAB-SIZE)
      (dolist (s (svref hash-tab i))
        (funcall function s)))))

;;--------------------------------------------------------------------------
;; do-all-symbols-iterator function package
;;--------------------------------------------------------------------------
(defun rt:do-all-symbols-iterator (function)
  (map nil #'(lambda (package)
               (let ((hash-tab (%package-external package)))
                 (dotimes (i PACKAGE-HASHTAB-SIZE)
                   (dolist (s (svref hash-tab i))
                     (funcall function s)))
                 (setq hash-tab (%package-internal package))
                 (dotimes (i PACKAGE-HASHTAB-SIZE)
                   (dolist (s (svref hash-tab i))
                     (funcall function s)))))
       *package-array*))


;;------------------------------------------------------------------------------
;; zur Übersetzungszeit bekanntes Symbol in Packages eintragen
;;------------------------------------------------------------------------------
(defun rt:setup-symbol (sym package-vector)
  
  ;; Package-Cell enthält:
  ;;  NIL = uninterned Symbol, nichts zu tun.
  ;;  0   = Keyword package
  ;;  -n  = external Symbol in Package n
  ;;  n   = internal Symbol in Package n
  ;;-----------------------------------------
  (let ((n (symbol-package-index sym)))
    (when n
      (let ((p (svref package-vector (abs n)))
            (hash-index (string-hash (symbol-name sym) PACKAGE-HASHTAB-SIZE)))
        
        (set-symbol-package sym p)
        
        ;; Symbol in Hash-Table des Package eintragen
        ;;-------------------------------------------
        (if (plusp n)
            ;; internal Symbol
            ;;----------------
            (push sym (svref (%package-internal p) hash-index))

            ;; external Symbol oder Keyword
            ;;-----------------------------
            (push sym (svref (%package-external p) hash-index)))))))
          
