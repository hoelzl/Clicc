;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Funktionen, die auf den C-Typen basieren
;;;
;;; $Revision: 1.17 $
;;; $Log: fftypes.lisp,v $
;;; Revision 1.17  1994/05/17  08:35:34  pm
;;; Funktion ohne Parameter hat in ANSI alsArgumentliste: (void)
;;;
;;; Revision 1.16  1994/04/22  14:13:11  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Packages korrigiert.
;;;   - Symbole werden gleich beim expandieren des C-Types exportiert.
;;; - C-Macros und C-Typdefinitionen werden erst spaeter herausgeschrieben.
;;;
;;; Revision 1.15  1994/04/18  12:09:56  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Alle Funktionen zur Expansion von C-Typen ueberarbeitet und strukturiert.
;;;
;;; Revision 1.14  1993/12/16  16:35:17  pm
;;; Fehler behoben.
;;; Funktionen ueberarbeitet.
;;;
;;; Revision 1.13  1993/11/03  11:43:29  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.12  1993/08/25  11:38:00  pm
;;; Alle Funktionen, die auf der Bearbeitung der C-Typen beruhen,
;;; wurden in diese Datei transferiert.
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;******************************************************************************
;; generieren der Funktionen zur Manipulation von C-Typen.
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Fuer Strings (Synonym)
;;------------------------------------------------------------------------------
(defun ff-string (name old-type)
  (ff-typ name)
  (ff-test-fun name old-type)
  (ff-convert-fun-string name)
  (ff-copy-fun-string name old-type)
  )

;;------------------------------------------------------------------------------
;; Fuer primitive Typen. (Syonym)
;;------------------------------------------------------------------------------
(defun ff-prim-type (name old-type)
  (ff-typ name)
  (ff-test-fun name old-type)
  (ff-convert-fun name old-type)
  )

;;------------------------------------------------------------------------------
;; Fuer Strukturen, die noch nicht bekannt sind.
;;------------------------------------------------------------------------------
(defun ff-new-struct (name type)
  (ff-typ name)
  (ff-test-fun-struct-or-union name name 'struct)
  (ff-construct-fun-struct-or-union-new name 'struct)
  (ff-copy-fun-struct-or-union-new name 'struct)
  (ff-access-fun-new name type name 'struct))

;;------------------------------------------------------------------------------
;; Fuer bekannte Strukturen.
;; 
;; Eine benannte Struktur stuetzt sich weitgehend auf die schon vorher
;; eingebundenen Funktionen.
;;------------------------------------------------------------------------------
(defun ff-old-struct (name old-name old-type)
  (ff-typ name)
  (ff-test-fun-struct-or-union name old-name 'struct)
  (ff-construct-fun-struct-or-union-old name old-name 'struct)
  (ff-copy-fun-struct-or-union-old name old-name 'struct)
  (ff-access-fun-old name old-type name old-name 'struct)
  )

;;------------------------------------------------------------------------------
;; Fuer Unionen, die noch nicht bekannt sind.
;;------------------------------------------------------------------------------
(defun ff-new-union (name type)
  (ff-typ name)
  (ff-test-fun-struct-or-union name name 'union)
  (ff-construct-fun-struct-or-union-new name 'union)
  (ff-copy-fun-struct-or-union-new name 'union)
  (ff-access-fun-new name type name 'union)
  )

;;------------------------------------------------------------------------------
;; Fuer bekannte Unionen.
;; 
;; Eine benannte Union stuetzt sich weitgehend auf die schon vorher
;; eingebundenen Funktionen.
;;------------------------------------------------------------------------------
(defun ff-old-union (name old-name old-type)
  (ff-typ name)
  (ff-test-fun-struct-or-union name old-name 'union)
  (ff-construct-fun-struct-or-union-old name old-name 'union)
  (ff-copy-fun-struct-or-union-old name old-name 'union)
  (ff-access-fun-old name old-type name old-name 'union)
  )

;;------------------------------------------------------------------------------
;; Fuer Handles
;;------------------------------------------------------------------------------
(defun ff-new-handle (name)
  (ff-typ name)
  (ff-test-fun-handle name name)
  )

;;------------------------------------------------------------------------------
;; Fuer Synonyme von handles
;;------------------------------------------------------------------------------
(defun ff-old-handle (name old-name)
  (ff-typ name)
  (ff-test-fun-handle name old-name)
  )

;;------------------------------------------------------------------------------
;; Fuer Arrays
;;------------------------------------------------------------------------------
(defun ff-new-array (name type)
  (ff-typ name)
  (ff-test-fun-array name name)
  (ff-construct-fun-array-new name type)
  (ff-copy-fun-array-new name)
  (ff-access-fun-array-new name type)
  )

;;------------------------------------------------------------------------------
;; Fuer Arrays (Synonym)
;;------------------------------------------------------------------------------
(defun ff-old-array (name old-name old-type)
  (ff-typ name)
  (ff-test-fun-array name old-name)
  (ff-construct-fun-array-old name old-name)
  (ff-copy-fun-array-old name old-name)
  (ff-access-fun-array-old name old-name old-type)
  )

;;******************************************************************************
;; Testfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Erzeugt die Testfunktion fuer elementare Typen und fuer Strings:
;;   (<name>-p <c-value>)
;;------------------------------------------------------------------------------
(defun ff-test-fun (name type)
  (let* ((test-name (intern-postfixed name "-P"))
         (test-type (intern-postfixed type "-P"))
         )
    (p1-defun `(,test-name (arg) (,test-type arg)))
    (export test-name *package*)))


;;------------------------------------------------------------------------------
;; Erzeuge die Testfunktion fuer Strukturen und Unionen:
;;   (<name>-p <c-value>)
;;------------------------------------------------------------------------------
(defun ff-test-fun-struct-or-union (name old-name struct-or-union)
  (let* ((typesymbol (get-typesymbol old-name))
         (test-name (intern-postfixed name "-P"))
         (internal-fun (if (eq struct-or-union 'struct)
                           'RT::internal-c-struct-p
                           'RT::internal-c-union-p))
         )
    (p1-defun `(,test-name (arg) (,internal-fun arg ',typesymbol)))
    (export test-name *package*)))

;;------------------------------------------------------------------------------
;; Erzeuge die Testfunktion fuer Handles:
;;   (<name>-p <c-value>)
;;------------------------------------------------------------------------------
(defun ff-test-fun-handle (name old-name)
  (let* ((typesymbol (get-typesymbol old-name))
         (test-name (intern-postfixed name "-P"))
         )
    (p1-defun `(,test-name (arg) (RT::internal-c-handle-p arg ',typesymbol)))
    (export test-name *package*)))

;;------------------------------------------------------------------------------
;; Erzeuge die Testfunktion fuer Arrays
;;   (<name>-p <c-value>)
;;------------------------------------------------------------------------------
(defun ff-test-fun-array (name old-name)
  (let* ((typesymbol (get-typesymbol old-name))
         (test-name (intern-postfixed name "-P"))
         )
    (p1-defun `(,test-name (arg) (RT::internal-c-array-p arg ',typesymbol)))
    (export test-name *package*)))

;;******************************************************************************
;; Typ erzeugen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Typ erzeugen.
;;------------------------------------------------------------------------------
(defun ff-typ (name)
  (let* ((test-name (intern-postfixed name "-P"))
         )
    (p1-deftype `(,name () (quote (satisfies ,test-name))))
    (export name *package*)))

;;******************************************************************************
;; Konvertierungsfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Konvertierungsfunktion fuer primitive Werte:
;;   (<name> <value>)
;;------------------------------------------------------------------------------
(defun ff-convert-fun (name old-type)
  (p1-defun `(,name (arg) (,old-type arg)))
  (export name *package*))

;;------------------------------------------------------------------------------
;; Konvertierungsfunktion fuer Strings:
;;   (make-<name> <value>)
;;------------------------------------------------------------------------------
(defun ff-convert-fun-string (name)
  (let* ((make-name (intern-prefixed "MAKE-" name)))
    (p1-defun `(,make-name (arg) (ffi:make-c-string arg)))
    (export make-name *package*)))

;;******************************************************************************
;; Konstruktorfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Konstruktor-Funktion fuer neue Strukturen und Unionen
;;   (make-<name>)
;;------------------------------------------------------------------------------
(defun ff-construct-fun-struct-or-union-new (name struct-or-union)
  (let* ((internal-fun (if (eq struct-or-union 'struct)
                           'RT::internal-make-c-struct
                           'RT::internal-make-c-union))
         (typesymbol (get-typesymbol name))
         (make-name (intern-prefixed "MAKE-" name))
         (malloc-name (intern-prefixed "MALLOC-" name))
         (define-name (c-macro-string malloc-name))
         )

    (p1-def-call-out
     `(,malloc-name 
       :arguments nil
       :return-type (ffi:c-ptr ,name)
       :name ,define-name)
     :intern t :macro t) 
    
    (p1-defun 
     `(,make-name () 
       (,internal-fun ',typesymbol (,malloc-name))))
    (export make-name *package*)))

;;------------------------------------------------------------------------------
;; Konstruktor-Funktion fuer Synonyme von Strukturen und Unionen
;;   (make-<name>)
;;------------------------------------------------------------------------------
(defun ff-construct-fun-struct-or-union-old (name old-name struct-or-union)
  (let* ((internal-fun (if (eq struct-or-union 'struct)
                           'RT::internal-make-c-struct
                           'RT::internal-make-c-union))
         (typesymbol (get-typesymbol old-name))
         (make-name (intern-prefixed "MAKE-" name))
         (malloc-name (intern-prefixed "MALLOC-" old-name))
         )
    
    (p1-defun 
     `(,make-name () 
       (,internal-fun ',typesymbol (,malloc-name))))
    (export make-name *package*)))

;;------------------------------------------------------------------------------
;; Konstruktor-Funktionen fuer neue Arrays
;;   (make-<name>)
;;------------------------------------------------------------------------------
(defun ff-construct-fun-array-new (name type)
  (let* ((typesymbol (get-typesymbol name))
         (malloc-name (intern-prefixed "MALLOC-" name))
         (define-name (c-macro-string malloc-name))
         (make-name (intern-prefixed "MAKE-" name))
         (array-type (second type))
         )

    (multiple-value-bind (key old-name old-type)
        (select-type name array-type array-type)
      (declare (ignore old-name old-type))

      (when (member key '(string primitive old-handle old-union old-array ptr))
        (p1-def-call-out
         `(,malloc-name :arguments () 
           :return-type (ffi:c-ptr ,name)
           :name ,define-name)
         :intern t :macro t) 
        
        (p1-defun 
         `(,make-name ()
           (RT::internal-make-c-array ',typesymbol (,malloc-name))))
        (export make-name *package*)))))

;;------------------------------------------------------------------------------
;; Konstruktor-Funktion fuer Arrays (Synonym)
;;   (make-<name>)
;;------------------------------------------------------------------------------
(defun ff-construct-fun-array-old (name old-name)
  (let* ((typesymbol (get-typesymbol old-name))
         (make-name (intern-prefixed "MAKE-" name))
         (malloc-name (intern-prefixed "MALLOC-" old-name))
         )
    
    (p1-defun 
     `(,make-name ()
       (RT::internal-make-c-array ',typesymbol (,malloc-name))))
    (export make-name *package*)))

;;******************************************************************************
;; Kopierfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Kopierfunktion fuer Strings
;;   (copy-<name> <arg>)
;;------------------------------------------------------------------------------
(defun ff-copy-fun-string (name old-type)
  (let* ((copy-name (intern-prefixed "COPY-" name))
         (copy-old-name (intern-prefixed "COPY-" old-type)))
    (p1-defun `(,copy-name (arg) (,copy-old-name arg)))
    (export copy-name *package*)))

;;------------------------------------------------------------------------------
;; Kopier-Funktion fuer neue Strukturen und Unionen
;;   (copy-<name> <arg>)
;;------------------------------------------------------------------------------
(defun ff-copy-fun-struct-or-union-new (name struct-or-union)
  (let* ((internal-fun (if (eq struct-or-union 'struct)
                           'RT::internal-copy-c-struct
                           'RT::internal-copy-c-union))
         (typesymbol (get-typesymbol name))
         (copy-name (intern-prefixed "COPY-" name))
         (test-name (intern-postfixed name "-P"))
         (malloc-name (intern-prefixed "MALLOC-" name))
         (size-of-name (intern-prefixed "SIZE-OF-" name))
         (define-name (c-macro-string size-of-name))
         )
    
    (p1-def-call-out
     `(,size-of-name :arguments () 
       :return-type ffi:c-unsigned-long
       :name ,define-name)
     :intern t :macro t) 
    
    (p1-defun `(,copy-name (arg)
                (if (,test-name arg)
                    (,internal-fun
                     ',typesymbol (,malloc-name) arg (,size-of-name))
                    (error "Runtime Error: not of right type: ~A" arg))))
    (export copy-name *package*)))

;;------------------------------------------------------------------------------
;; Kopier-Funktion fuer Strukturen und Unionen (Synonym)
;;   (copy-<name> <arg>)
;;------------------------------------------------------------------------------
(defun ff-copy-fun-struct-or-union-old (name old-name struct-or-union)
  (let* ((internal-fun (if (eq struct-or-union 'struct)
                           'RT::internal-copy-c-struct
                           'RT::internal-copy-c-union))
         (typesymbol (get-typesymbol old-name))
         (copy-name (intern-prefixed "COPY-" name))
         (test-name (intern-postfixed name "-P"))
         (malloc-name (intern-prefixed "MALLOC-" old-name))
         (size-of-name (intern-prefixed "SIZE-OF-" old-name))
         )
    
    (p1-defun `(,copy-name (arg)
                (if (,test-name arg)
                    (,internal-fun
                     ',typesymbol (,malloc-name) arg (,size-of-name))
                    (error "Runtime Error: not of right type: ~A" arg))))
    (export copy-name *package*)))

;;------------------------------------------------------------------------------
;; Kopier-Funktion fuer Arrays
;;   (copy-<name> <arg>)
;;------------------------------------------------------------------------------
(defun ff-copy-fun-array-new (name)
  (let* ((typesymbol (get-typesymbol name))
         (copy-name (intern-prefixed "COPY-" name))
         (test-name (intern-postfixed name "-P"))
         (malloc-name (intern-prefixed "MALLOC-" name))
         (size-of-name (intern-prefixed "SIZE-OF-" name))
         (define-name (c-macro-string size-of-name))
         )
    
    (p1-def-call-out
     `(,size-of-name :arguments () 
       :return-type ffi:c-unsigned-long
       :name ,define-name)
     :intern t :macro t) 
    
    (p1-defun `(,copy-name (arg)
                (if (,test-name arg)
                    (RT::internal-copy-c-array
                     ',typesymbol (,malloc-name) arg (,size-of-name))
                    (error "Runtime Error: not of right type: ~A" arg))))))

;;------------------------------------------------------------------------------
;; Kopier-funktion fuer Arrays (Synonyme)
;;   (copy-<name> <arg>)
;;------------------------------------------------------------------------------
(defun ff-copy-fun-array-old (name old-name)
  (let* ((typesymbol (get-typesymbol old-name))
         (copy-name (intern-prefixed "COPY-" name))
         (test-name (intern-postfixed name "-P"))
         (malloc-name (intern-prefixed "MALLOC-" old-name))
         (size-of-name (intern-prefixed "SIZE-OF-" old-name))
         )
    
    (p1-defun `(,copy-name (arg)
                (if (,test-name arg)
                    (RT::internal-copy-c-array
                     ',typesymbol (,malloc-name) arg (,size-of-name))
                    (error "Runtime Error: not of right type: ~A" arg))))
    (export copy-name *package*)))

;;******************************************************************************
;; Zugriffsfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Fuer Neue Strukturen und Unionen
;;------------------------------------------------------------------------------
(defun ff-access-fun-new (name type original-name struct-or-union
                               &optional (access ""))
  (dolist (slot (rest type))
    (let* ((internal-fun (if (eq struct-or-union 'struct)
                             'rt::internal-get-struct-pointer
                             'rt::internal-get-union-pointer))
           (slot-name (first slot))
           (slot-type (second slot))
           (slot-reader 
            (intern (concatenate 'string (string name) "-" (string slot-name))))
           (test-name (intern-postfixed original-name "-P"))
           (typesymbol (get-typesymbol original-name))
           )
      
      (multiple-value-bind (key old-name old-type)
          (select-type name slot-type slot-type)
        (declare (ignore old-type old-name))
        
        (case key
          ;; Strings, Primitive, Synonyme
          ;;-----------------------------
          ((string primitive old-handle old-struct old-union old-array ptr)
           (let* 
               ((retreive-name (intern-prefixed "RETREIVE-" slot-reader))
                (retreive-name-string (c-macro-string retreive-name))
                (give-name (intern-prefixed "GIVE-" slot-reader))
                (give-name-string (c-macro-string give-name))
                )
             
             ;; erzeuge die lesende Zugriffsfunktion
             ;; (<name>-<slot> objekt)
             ;;-----------------------
             (p1-def-call-out
              `(,retreive-name
                :arguments ((ffi:c-ptr ,original-name))
                :return-type ,slot-type 
                :name ,retreive-name-string)
              :macro t :intern t)
             
             (p1-defun
              `(,slot-reader (objekt)
                (if (,test-name objekt)
                    (,retreive-name
                     (,internal-fun ',typesymbol objekt))
                    (error "not a structured type: ~A" objekt))))
             (export slot-reader *package*)
             
             ;; erzeuge die schreibenden Zugriffsfunktionen
             ;; (setf (<name>-<slot> objekt) value)
             ;;------------------------------------
             (p1-def-call-out
              `(,give-name :arguments ((ffi:c-ptr ,original-name) ,slot-type)
                :return-type ,slot-type 
                :name ,give-name-string)
              :macro t :intern t)
             
             (p1-defun 
              `((L::setf ,slot-reader) (value objekt)
                (if (,test-name objekt)
                    (,give-name
                     (,internal-fun ',typesymbol objekt) value)
                    (error "not a structured type: ~A" objekt))))))
          
          ;; Unbenannte Handles
          ;;-------------------
          (new-handle
           ;; keine Zugriffsfunktion
           ;;-----------------------
           )
          
          ;; Unbenannte Strukturen
          ;;----------------------
          (new-struct
           ;; Keine Zugriffsfunktion, aber
           ;; Zugriffsfunktionen fuer die Komponenten
           ;;----------------------------------------
           (let* ((access-string 
                   (concatenate 'string access (string slot-name) ".")))
             (ff-access-fun-new 
              slot-reader slot-type original-name 'struct
              access-string)))
          
          ;; Unbenannte Unionen
          ;;-------------------
          (new-union
           ;; Keine Zugriffsfunktion, aber
           ;; Zugriffsfunktionen fuer die Komponenten
           ;;----------------------------------------
           (let* ((access-string 
                   (concatenate 'string access (string slot-name) ".")))
             (ff-access-fun-new 
              slot-reader slot-type original-name 'union
              access-string)))
    
          ;; Arrays
          ;;-------
          (new-array
           ;; keine Zugriffsfunktion
           ;;-----------------------
           ;; *** to do *** aref-zugriff.
           )
          
          )))))

;;------------------------------------------------------------------------------
;; Zugriffsfunktionen fuer Synonyme von Strukturen und Unionen
;;------------------------------------------------------------------------------
(defun ff-access-fun-old (name type original-name old-name struct-or-union)
  (dolist (slot (rest type))
    (let* ((typesymbol (get-typesymbol original-name))
           (internal-fun (if (eq struct-or-union 'struct)
                             'rt::internal-get-struct-pointer
                             'rt::internal-get-union-pointer))
           (slot-name (first slot))
           (slot-type (second slot))
           (slot-reader 
            (intern 
             (concatenate 'string (string name) "-" (string slot-name))))
           (old-slot-reader 
            (intern 
             (concatenate 'string (string old-name) "-" (string slot-name))))
           (test-name (intern-postfixed original-name "-P")))
      
      (multiple-value-bind (key old-name old-type)
          (select-type name slot-type slot-type)
        (declare (ignore old-type old-name))
        
        (case key
          ;; Strings, Primitive
          ;;-------------------
          ((string primitive old-handle old-struct old-union old-array ptr)
           (let* 
               ((retreive-name (intern-prefixed "RETREIVE-" old-slot-reader))
                (give-name (intern-prefixed "GIVE-" old-slot-reader)))
             
             ;; erzeuge die lesende Zugriffsfunktion
             ;; (<name>-<slot> objekt)
             ;;-----------------------
             (p1-defun
              `(,slot-reader (objekt)
                (if (,test-name objekt)
                    (,retreive-name
                     (,internal-fun ',typesymbol objekt))
                    (error "not a structured type: ~A" objekt))))
             (export slot-reader *package*)
             
             ;; erzeuge die schreibenden Zugriffsfunktionen
             ;; (setf (<name>-<slot> objekt) value)
             ;;------------------------------------
             (p1-defun 
              `((L::setf ,slot-reader) (value objekt)
                (if (,test-name objekt)
                    (,give-name
                     (,internal-fun ',typesymbol objekt) value)
                    (error "not a structured type: ~A" objekt))))))
          
          ;; Unbenannte Handles
          ;;-------------------
          (new-handle
           ;; keine Zugriffsfunktion
           ;;-----------------------
           )
          
          ;; Unbenannte Strukturen
          ;;----------------------
          (new-struct
           ;; Keine Zugriffsfunktion, aber
           ;; Zugriffsfunktionen fuer die Komponenten
           ;;----------------------------------------
           (ff-access-fun-old 
            slot-reader slot-type original-name old-slot-reader 'struct))
          
          ;; Unbenannte Unionen
          ;;-------------------
          (new-union
           ;; Keine Zugriffsfunktion, aber
           ;; Zugriffsfunktionen fuer die Komponenten
           ;;----------------------------------------
           (ff-access-fun-old 
            slot-reader slot-type original-name old-slot-reader 'union))
          
          ;; Arrays
          ;;-------
          (new-array
           ;; keine Zugriffsfunktion
           ;;-----------------------
           ;; *** to do *** aref-zugriff.
           ))))))

;;------------------------------------------------------------------------------
;; Zugriffsfunktionen fuer neue Arrays
;;------------------------------------------------------------------------------
(defun ff-access-fun-array-new (name type)
  (let* ((typesymbol (get-typesymbol name))
         (test-name (intern-postfixed name "-P"))
         (aref-name (intern-prefixed "AREF-" name))
         (retreive-name (intern-prefixed "RETREIVE-" aref-name))
         (retreive-name-string (c-macro-string retreive-name))
         (give-name (intern-prefixed "GIVE-" aref-name))
         (give-name-string (c-macro-string give-name))
         (array-type (second type))
         (dimension-list (third type))
         dimension-desc
         dimension-type-desc
         )

    (multiple-value-bind (key old-name old-type)
        (select-type name array-type array-type)
      (declare (ignore old-type old-name))

      (case key
        ((string primitive ptr old-handle old-struct old-union old-array)
         ;; Listen fuer die Zugriffe erzeugen
         ;;----------------------------------
         (dotimes (count (length dimension-list))
           (push (intern (format nil "x~A" count)) dimension-desc)
           (push 'ffi:c-long dimension-type-desc))
         
         (p1-def-call-out
          `(,retreive-name
            :arguments ((ffi:c-ptr ,name) ,@dimension-type-desc)
            :return-type ,array-type
            :name ,retreive-name-string)
          :macro t :intern t)
         
         (p1-defun
          `(,aref-name (objekt ,@dimension-desc)
            (if (,test-name objekt)
                (,retreive-name
                 (RT::internal-get-array-pointer ',typesymbol objekt)
                 ,@dimension-desc)
                (error "not a structured type: ~A" objekt))))
         (export aref-name *package*)
         
         ;; erzeuge die schreibende Zugriffsfunktion
         ;;-----------------------------------------
         (p1-def-call-out
          `(,give-name 
            :arguments 
            ((ffi:c-ptr ,name) ,@dimension-type-desc ,array-type)
            :return-type ,array-type 
            :name ,give-name-string)
          :macro t :intern t)
             
         (p1-defun 
          `((L::setf ,aref-name) (value objekt ,@dimension-desc)
                (if (,test-name objekt)
                    (,give-name
                     (RT::internal-get-array-pointer ',typesymbol objekt) 
                     ,@dimension-desc
                     value)
                    (error "not a structured type: ~A" objekt)))))
        
        ((new-handle new-struct new-union new-array)
         ;; Keine Zugriffsfunktionen
         ;;-------------------------
         )
        (otherwise
         (internal-error 'ff-access-fun-array-new "Not a valid type: ~A"
                         type))))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun ff-access-fun-array-old (name old-name old-type)
  (let* ((test-name (intern-postfixed old-name "-P"))
         (aref-name (intern-prefixed "AREF-" name))
         (old-aref-name (intern-prefixed "AREF-" old-name))
         (retreive-name (intern-prefixed "RETREIVE-" old-aref-name))
         (give-name (intern-prefixed "GIVE-" old-aref-name))
         (array-type (second old-type))
         (dimension-list (third old-type))
         dimension-desc
         (typesymbol (get-typesymbol name))
         )

    (multiple-value-bind (key old-name old-type)
        (select-type name array-type array-type)
      (declare (ignore old-name old-type))

      (case key
        ((string primitive ptr old-handle old-struct old-union old-array)
         ;; Listen fuer die Zugriffe erzeugen
         ;;----------------------------------
         (dotimes (count (length dimension-list))
           (push (intern (format nil "x~A" count)) dimension-desc))
         
         (p1-defun
          `(,aref-name (objekt ,@dimension-desc)
            (if (,test-name objekt)
                (,retreive-name
                 (RT::internal-get-array-pointer ',typesymbol objekt)
                 ,@dimension-desc)
                (error "not a structured type: ~A" objekt))))
         (export aref-name *package*)

         ;; erzeuge die schreibende Zugriffsfunktion
         ;;-----------------------------------------
         (p1-defun 
          `((L::setf ,aref-name) (value objekt ,@dimension-desc)
                (if (,test-name objekt)
                    (,give-name
                     (RT::internal-get-array-pointer ',typesymbol objekt) 
                     ,@dimension-desc
                     value)
                    (error "not a structured type: ~A" objekt)))))
        
        ((new-handle new-struct new-union new-array)
         ;; Keine Zugriffsfunktionen
         ;;-------------------------
         )
        (otherwise
         (internal-error 'ff-access-fun-array-new "Not a valid type: ~A"
                         array-type))))))

;;******************************************************************************
;; Hilfsfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Funktionssignatur herausschreiben.
;;------------------------------------------------------------------------------
(defun ff-signatur (foreign-fun)
  (let* ((argument-liste (?arguments foreign-fun))
         (c-name (?name foreign-fun))
         (return-type (?return-type foreign-fun))
         (*print-circle* nil)
         )
    (push 
     (format nil "~A ~A(~{~A~^,~});"
             (convert-c-type-to-string return-type)
             c-name
             (if (null argument-liste)
                 "void"
                 (mapcar #'convert-c-type-to-string argument-liste)))
     *ffi-signatures*)))

;;------------------------------------------------------------------------------
;; Bekommt einen C-Typ und macht daraus die C-Repraesentation dieses Types als
;; String.
;;------------------------------------------------------------------------------
(defun convert-c-type-to-string (type)
  (let* ((*print-circle* nil))
    (multiple-value-bind (key old-name old-type)
        (select-type nil type type)
      
      (case key
        (string "char *")
        (primitive (minus-to-space old-type))
        (void "void")
        (old-struct (c-name-string old-name))
        (old-union (c-name-string old-name))
        (old-array (c-name-string old-name))
        (ptr (format nil "~A *" (convert-c-type-to-string (second type))))
        (old-handle "void *")             ; oder "char *"
        (new-handle "void *")             ; oder "char *"
        (new-struct 
         (format nil "struct {~%~:{~T~A ~A;~%~}}" 
                 (mapcar #'make-c-struct-component (rest old-type))))
        (new-union
         (format nil "union {~%~:{~T~A ~A;~%~}}"
                 (mapcar #'make-c-union-component (rest old-type))))
        (new-array
         (format nil "~A~{[~A]~}"
                 (convert-c-type-to-string (second old-type))
                 (third old-type)))
        
        (otherwise
         (internal-error 'convert-c-type-to-string "Unknown Type: ~A" type))))))


;;------------------------------------------------------------------------------
;; Erzeugt eine Liste aus zwei Strings, die eine Komponente einer C-Struktur
;; enthaelt.
;;------------------------------------------------------------------------------
(defun make-c-struct-component (komponente)
  (let* ((k-slot (first komponente))
         (k-slot-name (string k-slot))
         (k-typ (second komponente)))
    (multiple-value-bind (key old-name old-type)
        (select-type nil k-typ k-typ)
      (declare (ignore old-name old-type))
      (case key
        (old-struct
         (list 
          (concatenate 'string "struct " (convert-c-type-to-string k-typ))
          k-slot-name))
        (old-union
         (list 
          (concatenate 'string "union " (convert-c-type-to-string k-typ))
          k-slot-name))
        (ptr
         (multiple-value-bind (key old-name old-type)
             (select-type nil (second k-typ) (second k-typ))
           (declare (ignore old-name old-type))
           (case key
             (old-struct
              (list 
               (concatenate 'string "struct " (convert-c-type-to-string k-typ))
               k-slot-name))
             (old-union
              (list 
               (concatenate 'string "union " (convert-c-type-to-string k-typ))
               k-slot-name))
             (otherwise
              (list
               (convert-c-type-to-string k-typ)
               k-slot-name)))))
        (otherwise
         (list 
          (convert-c-type-to-string k-typ)
          k-slot-name))))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun make-c-union-component (komponente)
  (let* ((k-slot (first komponente))
         (k-slot-name (string k-slot))
         (k-typ (second komponente)))
    (multiple-value-bind (key old-name old-type)
        (select-type nil k-typ k-typ)
      (declare (ignore old-name old-type))
      (case key
        (old-struct
         (list 
          (concatenate 'string "struct " (convert-c-type-to-string k-typ))
          k-slot-name))
        (old-union
         (list 
          (concatenate 'string "union " (convert-c-type-to-string k-typ))
          k-slot-name))
        (ptr
         (multiple-value-bind (key old-name old-type)
             (select-type nil (second k-typ) (second k-typ))
           (declare (ignore old-name old-type))
           (case key
             (old-struct
              (list 
               (concatenate 'string "struct " (convert-c-type-to-string k-typ))
               k-slot-name))
             (old-union
              (list 
               (concatenate 'string "union " (convert-c-type-to-string k-typ))
               k-slot-name))
             (otherwise
              (list
               (convert-c-type-to-string k-typ)
               k-slot-name)))))
        (otherwise
         (list 
          (convert-c-type-to-string k-typ)
          k-slot-name))))))

;;------------------------------------------------------------------------------
;; Erzeugt einen String, in dem alle Minuszeichen in Underscores gewandelt
;; wurden.
;;------------------------------------------------------------------------------
(defun minus-to-underscore (arg)
  (substitute #\_ #\- (string arg)))

;;------------------------------------------------------------------------------
;; Erzeugt einen String, in dem alle Minuszeichen in Spaces gewandelt wurden.
;; Desweiteren werden die ersten beiden Zeichen eliminiert. Wird verwendet, um
;; aus dem LISP-Namen eines C-types den C-Namen desselben Types zu generieren.
;; Nur fuer primitive Typen zu verwenden !!!
;; aus `c-unsigned-int' wird der typ `unsigned int'
;;------------------------------------------------------------------------------
(defun minus-to-space (arg)
  (string-downcase (subseq (substitute #\space #\- (string arg)) 2)))

;;------------------------------------------------------------------------------
;; Erzeugt einen String, in dem alle Minuszeichen durch Underscores ersetzt
;; wurden, und der dann in Kleinbuchstaben konvertiert wurde.
;;------------------------------------------------------------------------------
(defun c-name-string (name)
  (string-downcase (minus-to-underscore name)))

(defun c-macro-string (name)
  (string-upcase (minus-to-underscore name)))

;;------------------------------------------------------------------------------
;; Schreibt Daten in die Interface-Header-Datei.
;;------------------------------------------------------------------------------
(defun write-to-header-file (arg)
  (let* ((*print-circle* nil))
    (format *interface-file-stream* "~A~%" arg)))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun get-typesymbol (name)
  (values name))

;;------------------------------------------------------------------------------
(provide "fftypes")
