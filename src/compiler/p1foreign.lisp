;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Foreign Functions
;;;
;;; $Revision: 1.30 $
;;; $Log: p1foreign.lisp,v $
;;; Revision 1.30  1994/05/22  16:16:24  sma
;;; Erzeugen einer leeren '*-ffi.h'-Datei verhindert.
;;;
;;; Revision 1.29  1994/05/19  08:00:32  pm
;;; <file>-ffi.h wird nun nur noch bei Bedarf angelegt.
;;; Alle diese Relikte duerfen geloescht werden
;;;
;;; Revision 1.28  1994/04/27  16:37:03  pm
;;; Fehler behoben.
;;;
;;; Revision 1.27  1994/04/22  14:14:24  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Packages korrigiert.
;;;   - Symbole werden gleich beim expandieren des C-Types exportiert.
;;; - C-Macros und C-Typdefinitionen werden erst spaeter herausgeschrieben.
;;;
;;; Revision 1.26  1994/04/18  12:16:09  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Alle Allgemeinen Funktionen des FFI stehen hier
;;;
;;; Revision 1.25  1993/12/16  16:38:03  pm
;;; def-c-type an die neuen Typen angepasst.
;;;
;;; Revision 1.24  1993/11/03  11:40:19  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.23  1993/08/24  11:20:29  pm
;;; Erweiterungen um C-Pointer
;;;
;;; Revision 1.22  1993/07/26  16:45:56  pm
;;; Erweiterungen um C-Strukturen
;;;
;;; Revision 1.21  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.20  1993/06/10  11:02:38  pm
;;; Quelltext bereinigt
;;;
;;; Revision 1.19  1993/05/31  17:06:02  pm
;;; Abarbeiten von call-ins eingebaut
;;;
;;; Revision 1.18  1993/05/23  17:52:44  pm
;;; p1-def-c-type um das Argument package erweitert
;;;
;;; Revision 1.17  1993/05/21  13:58:02  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.16  1993/05/12  08:36:57  pm
;;; packages verstanden und ueberarbeitet
;;;
;;; Revision 1.15  1993/04/23  09:42:35  pm
;;; Aufruf von p1-foreign-fun-call optimiert
;;;
;;; Revision 1.14  1993/04/08  12:52:32  pm
;;; Tippfehler beseitigt
;;;
;;; Revision 1.13  1993/04/08  09:16:52  pm
;;; Angefangen aufs C-Typ-System umzustellen
;;; p1-call-foreign-fun in p1-foreign-fun-call umbenannt
;;;
;;; Revision 1.12  1993/03/18  07:40:57  ft
;;; Tippfehler beseitigt.
;;;
;;; Revision 1.11  1993/03/17  14:32:11  pm
;;; struct eingebaut
;;;
;;; Revision 1.10  1993/02/18  10:22:01  kl
;;; Fehler mit den im Package FFI unbekannten Funktionen export und
;;; in-package behoben.
;;;
;;; Revision 1.9  1993/02/17  16:41:37  hk
;;; Package FFI soll KEINE anderen Packages usen, auch nicht Lisp.
;;;
;;; Revision 1.8  1993/02/16  16:58:24  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.7  1992/12/01  15:11:11  pm
;;; c-char* eingebaut
;;;
;;; Revision 1.6  1992/11/10  10:24:00  pm
;;; Fluechtigkeitsfehler behoben
;;;
;;; Revision 1.5  1992/11/05  10:52:02  pm
;;; Ueberarbeitet
;;;
;;; Revision 1.4  1992/11/04  12:41:28  pm
;;; p1-call-foreign-fun
;;;
;;; Revision 1.3  1992/10/19  14:17:44  pm
;;; kleinere Aenderungen
;;;
;;; Revision 1.2  1992/10/19  12:00:50  pm
;;; parser fuer foreign-funs
;;;
;;; Revision 1.1  1992/10/13  14:28:39  pm
;;; Initial revision
;;;-----------------------------------------------------------------------------

;;------------------------------------------------------------------------------
(in-package "CLICC")

(require "fftypes")

;;------------------------------------------------------------------------------
;; Fehlermeldungen.
;;------------------------------------------------------------------------------
(defconstant NO_NAME_SPECIFIED 
  "You must specify a name in DEF-CALL-OUT")
(defconstant NO_FORWARD_REF 
  "You cannot use the call-out-function ~S before its deklaration.")
(defconstant ILLEGAL_ARGUMENT
  "Not enough or to many arguments for ~S in PARSE-C-TYPE")

;;------------------------------------------------------------------------------
;; Meldungen.
;;------------------------------------------------------------------------------
(defconstant ANALYSE-CALL-OUT   "Analyse Call-Out-Function ~S")
(defconstant ANALYSE-CALL-IN    "Analyse Call-In-Function  ~S")
(defconstant ANALYSE-DEF-C-TYPE "Analyse C-Type-Definition ~S")


;;******************************************************************************
;; DEF-CALL-OUT
;;******************************************************************************
;;------------------------------------------------------------------------------
;; DEF-CALL-OUT name {option}*
;;
;; option ::= 
;;     (:name <c-name>)
;;   | (:arguments ({<c-type>}*))
;;   | (:return-type <c-type>)
;;   | (:callback <boolean>)
;;
;;------------------------------------------------------------------------------
(defun p1-def-call-out 
    (name_options &key
                  macro   ; Keine Signatur herausschreiben
                  intern) ; Keine Meldung ausgeben
  
  (unless intern (clicc-message ANALYSE-CALL-OUT (first name_options)))
  
  (multiple-value-bind 
        (name c-name arguments return-type callback)
      (parse-foreign-fun-args name_options)
    
    (let ((operator-def (get-operator-def name)))
      
      (case (car operator-def)
        ;; Neue Definition eintragen
        ;;--------------------------
        ((nil)
         (let* ((argument-length (length-of-arguments arguments))
                (exp-arg-length (if (< argument-length 0)
                                    (1- argument-length)
                                    (1+ argument-length)))
                (gen-sym (gensym))
                (foreign-fun
                 (make-instance 'foreign-fun
                                :arguments arguments
                                :name c-name
                                :return-type return-type
                                :callback callback
                                :symbol name
                                :par-spec argument-length
                                :other-sym gen-sym))
                (expanded-fun
                 (make-instance 'foreign-fun
                                :arguments arguments
                                :name c-name
                                :return-type return-type
                                :callback callback
                                :symbol name
                                :par-spec exp-arg-length
                                :other-sym gen-sym)))
           (set-unexpanded-foreign-fun name foreign-fun)
           (set-foreign-fun gen-sym expanded-fun)
           
           
           ;; Signatur herausschreiben
           ;;-------------------------
           (unless macro (ff-signatur foreign-fun))))
        
        ;; keine Forwaertsreferenz erlaubt
        ;;--------------------------------
        (:FORWARD
         (internal-error 'p1-def-call-out NO_FORWARD_REF name))
        
        ;; keine Mehrfachdefinition erlaubt
        ;;---------------------------------
        (t (redef-op-error (car operator-def) name)))
      
      ;; Symbol exportieren
      ;;-------------------
      (export name *package*)

      (values))))

;;******************************************************************************
;; Hilfsfunktionen von def-call-out
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Holt den Funktionsname und die Optionen-Liste und testet einige
;; Fehlerquellen ab.
;;
;; Resultat:
;; (MV) <name> <c-name> <arguments> <return-type> <callback>
;;------------------------------------------------------------------------------
(defun parse-foreign-fun-args (name_options)
  (let (name
        options)
    
    ;; Kein Name angegeben
    ;;--------------------
    (when (null name_options)
      (internal-error 'parse-foreign-fun-args NO_NAME_SPECIFIED))
    
    (if (atom name_options)
        (setq name name_options
              options nil)              ; keine Options
        (setq name (first name_options)
              options (rest name_options)))
    
    ;; Kein Symbol als Name
    ;;---------------------
    (unless (symbolp name)
      (internal-error 'parse-foreign-fun-args NO_NAME name 'DEF-CALL-OUT))
    
    ;; Uebrige Werte generieren
    ;;-------------------------
    (multiple-value-bind (c-name arguments return-type callback)
        (parse-ff-key-list name options)
      (values name c-name arguments return-type callback))))

;;------------------------------------------------------------------------------
;; Parsed die Key-Liste einer Call-Out-Definition. Es werden
;; Default-Werte angelegt, wenn keine Keys angegeben sind.
;; Es werden Fehler in der Key-List abgefangen.
;;
;; Die Keyword-Liste wird von links nach rechts durchgegangen, wobei
;; das letzte Auftreten eines Keywords den endgueltigen Wert festlegt.
;; [entgegen den ueblichen LISP-Konventionen.]
;;
;; Resultat: (MV) <c-name> <arguments> <return-type> <callback>
;;------------------------------------------------------------------------------
(defun parse-ff-key-list (name options)
  (let ((c-name (c-name-string name))
        (arguments '())
        (callback nil)
        (return-type 'ffi:c-int))
    
    ;; Key-Word-Liste muss gerade sein
    ;;--------------------------------
    (unless (evenp (length options))
      (internal-error 'parse-ff-key-list ODD_LEN_KEYLIST))
    
    ;; Key-Word-Liste durchgehen
    ;;--------------------------
    (do* ((key-list options (cddr key-list))
          (key (first key-list) (first key-list))
          (arg (second key-list) (second key-list)))
         ((null key-list))
      
      ;; zuerst ein Key-Word
      ;;--------------------
      (unless (keywordp key)
        (internal-error 'parse-ff-key-list NO_KEYWORD key))
      
      (case key
        ;; C-Name
        ;;-------
        (:name 
         (if (stringp arg)
             (setq c-name arg)
             (internal-error 'parse-ff-key-list NO_STRING arg)))
        
        ;; Argument-Liste
        ;;---------------
        (:arguments
         (cond 
           ;; leere Argumentliste
           ;;--------------------
           ((null arg)
            (setq arguments nil))
           
           ;; ein Argument
           ;;-------------
           ((and (listp arg) (= (length arg) 1))
            (setq arguments (list (parse-c-type (first arg)
                                                :could-be-void t
                                                :could-be-vararg t
                                                :could-be-ptr t
                                                :could-be-fixnum t))))
           ;; mehr als ein Argument
           ;;----------------------
           ((listp arg)
            (setq arguments
                  (append (mapcar #'(lambda (r-l)
                                      (parse-c-type 
                                       r-l 
                                       :could-be-ptr t
                                       :could-be-fixnum t))
                                  (butlast arg))
                          (list (parse-c-type (car (last arg))
                                              :could-be-vararg t
                                              :could-be-ptr t
                                              :could-be-fixnum t)))))
           ;; Keine Liste ist ein Fehler
           ;;---------------------------
           (t
            (internal-error 'parse-ff-key-list NO_LIST arg))))
        
        ;; Rueckgabewert
        ;;--------------
        (:return-type
         (setq return-type (parse-c-type arg
                                         :could-be-ptr t
                                         :could-be-void t
                                         :could-be-fixnum t)))
        
        ;; Callback
        ;;---------
        (:callback
         (setq callback arg))
        
        ;; Ungueltiger Key
        ;;----------------
        (otherwise
         (internal-error 'parse-ff-key-list
          ILLEGAL_KEY '(:NAME :ARGUMENTS :RETURN-TYPE :CALLBACK) key))))
    
    ;; Tests:
    ;;-------
    (when (equal arguments '(ffi:c-void))
      (setq arguments '()))

    (values c-name arguments return-type callback)))

;;------------------------------------------------------------------------------
;; Liefert die Laenge einer Parameterlist entsprechend dem par-spec
;;------------------------------------------------------------------------------
(defun length-of-arguments (arguments)
  (cond 
    ((eq (first arguments) 'ffi:c-void) (values 0))
    ((eq (last arguments) 'ffi:c-vararg) (- 0 (length arguments)))
    (t (length arguments))))

;;******************************************************************************
;; DEF-CALL-IN
;;******************************************************************************
;;------------------------------------------------------------------------------
;; DEF-CALL-IN name {option}*
;;
;; option ::=
;;     (:name c-name)
;;   | (:arguments ({c-type}*))
;;   | (:return-type c-type)
;;------------------------------------------------------------------------------
(defun p1-def-call-in (name_options)
  
  (clicc-message ANALYSE-CALL-IN (first name_options))
  
  (multiple-value-bind (name c-name arg-list return-type)
      (parse-foreign-fun-args name_options)
    
    (let ((call-in-fun (get-call-in-fun name)))
      ;; keine Mehrfachdefinition erlaubt
      ;;---------------------------------
      (when call-in-fun
        (clicc-error "Call-In-Fun (~S) declared twice" call-in-fun))
      
      ;; Neue Definition eintragen
      ;;--------------------------
      (set-call-in-fun 
       name (make-instance 'call-in-fun
                           :arguments arg-list
                           :foreign-name c-name
                           :return-type return-type))
      
      (values name))))

;;------------------------------------------------------------------------------
;; finalize-call-in-funs
;;------------------------------------------------------------------------------
(defun finalize-call-in-funs ()
  (let ((call-in-funs (?call-in-funs *GLOBAL-ENVIRONMENT*)))
    (flet ((check-call-in (name_call-in-fun)
             (let* ((name (car name_call-in-fun))
                    (call-in-fun (cdr name_call-in-fun))
                    (operator (get-operator-def name))
                    (global-fun (cdr operator)))
               (unless (eq (car operator) :GLOBAL-FUN)
                 (clc-error "No global-fun declared as call-in-fun: ~A"
                              name))
               (setf (?call-in global-fun) call-in-fun))))

      (mapcar #'check-call-in call-in-funs))))


;;******************************************************************************
;; DEF-C-type
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Syntax einer C-Typ-Definition:
;;
;; DEF-C-TYPE name data-type
;;------------------------------------------------------------------------------
(defun p1-def-c-type (name_type)
  (let (name type)
    (clicc-message ANALYSE-DEF-C-TYPE (first name_type))

    ;; Es muessen immer Name und Typ angegeben werden
    ;;-----------------------------------------------
    (unless (>= (length name_type) 2)
      (clicc-error "You must specify a name and a type"))

    ;; Der Name muss ein Symbol sein
    ;;------------------------------
    (setq name (first name_type))
    (unless (symbolp name) (clicc-error NO_NAME name 'DEF-C-TYPE))

    ;; Der Typ darf nicht redefiniert werden
    ;;--------------------------------------
    (when (or (get-fftype name) (member-of-ffi name))
      (clicc-error "It is illegal to redefine the c-type ~S" name))      

    ;; Die Typdefinition parsen
    ;;-------------------------
    (setq type (parse-c-type (second name_type)))
 
    ;; Typ ins globale Environment eintragen.
    ;;---------------------------------------
    (set-fftype name type)

    ;; Die Lisp-Zugriffsfunktionen erzeugen.
    ;;--------------------------------------
    (multiple-value-bind (key old-name old-type)
        (select-type name type type)
      (case key
        (string     (ff-string name old-type))
        (primitive  (ff-prim-type name old-type))
        (new-array  (ff-new-array name type))
        (old-array  (ff-old-array name old-name old-type))
        (new-handle (ff-new-handle name))
        (old-handle (ff-old-handle name old-name))
        (new-struct (ff-new-struct name type))
        (old-struct (ff-old-struct name old-name old-type))
        (new-union  (ff-new-union name type))
        (old-union  (ff-old-union name old-name old-type))
        
        (otherwise  (clicc-error "Unknown Type: ~A" type))))))

;;------------------------------------------------------------------------------
;; Finalisieren der Typen, erzeugen der Funktionen (in fftypes.lisp)
;;------------------------------------------------------------------------------
(defun finalize-fftypes ()
  (let* ((fftypes (reverse (?fftypes *GLOBAL-ENVIRONMENT*))))

    ;; wenn diese beiden Listen leer sind, wird hier eh nix gemacht --sma
    ;;-------------------------------------------------------------------
    (unless (or fftypes *ffi-signatures*) (return-from finalize-fftypes))

    (setq *interface-file-stream* 
          (open (concatenate 'string (pathname-name *FILENAME*) "-ffi.h")
                :direction :output
                :if-exists :supersede
                :if-does-not-exist :create))
    
    (dolist (name_type fftypes)
      (let* ((name (first name_type))
             (type (rest name_type)))
        (multiple-value-bind (key old-name old-type)
            (select-type name type type)
          (declare (ignore old-name old-type))
          (case key
            ;; Strukturen
            ;;-----------
            (new-struct 
             (let* ((*print-circle* nil)
                    (c-type (convert-c-type-to-string name))
                    (malloc-name (intern-prefixed "MALLOC-" name))
                    (size-of-name (intern-prefixed "SIZE-OF-" name))
                    (define-name-malloc (c-macro-string malloc-name))
                    (define-name-size (c-macro-string size-of-name))
                    (slots (rest type)))

               ;; Typ definieren
               ;;---------------
               (write-to-header-file
                (format nil
                        "struct ~A {~%~:{~T~A ~A;~%~}};"
                        c-type (mapcar #'make-c-struct-component slots)))

               ;; Typedef auf den Typ
               ;;--------------------
               (write-to-header-file
                (format nil 
                        "typedef struct ~A ~A;" 
                        c-type c-type))

               ;; Alloziiere Speicher fuer die Struktur
               ;;--------------------------------------
               (write-to-header-file 
                (format nil
                        "#define ~A() malloc(sizeof(~A))" 
                        define-name-malloc c-type))

               ;; Groesse der Struktur
               ;;---------------------
               (write-to-header-file
                (format nil "#define ~A() sizeof(~A)"
                        define-name-size c-type))

               ;; Zugriffsfunktionen fuer die einzelnen slots
               ;;--------------------------------------------
               (finalise-access name type name)))
            
            ;; Unionen
            ;;--------
            (new-union  
             (let* ((*print-circle* nil)
                    (c-type (convert-c-type-to-string name))
                    (malloc-name (intern-prefixed "MALLOC-" name))
                    (size-of-name (intern-prefixed "SIZE-OF-" name))
                    (define-name-malloc (c-macro-string malloc-name))
                    (define-name-size (c-macro-string size-of-name))
                    (slots (rest type)))

               ;; Typ definieren
               ;;---------------
               (write-to-header-file
                (format nil
                        "union ~A {~%~:{~T~A ~A;~%~}};"
                        c-type (mapcar #'make-c-union-component slots)))

               ;; Typedef auf den Typ
               ;;--------------------
               (write-to-header-file
                (format nil 
                        "typedef union ~A ~A;" 
                        c-type c-type))

               ;; Alloziiere Speicher fuer die Struktur
               ;;--------------------------------------
               (write-to-header-file 
                (format nil
                        "#define ~A() malloc(sizeof(~A))" 
                        define-name-malloc c-type))

               ;; Groesse der Struktur
               ;;---------------------
               (write-to-header-file
                (format nil "#define ~A() sizeof(~A)"
                        define-name-size c-type))

               ;; Zugriffsfunktionen fuer die einzelnen slots
               ;;--------------------------------------------
               (finalise-access name type name)))
            
            ;; Arrays
            ;;-------
            (new-array  
             (let* ((*print-circle* nil)
                    (array-type (second type))
                    (dimension-list (third type))
                    (c-type (convert-c-type-to-string name))
                    (c-type-2 (convert-c-type-to-string array-type))
                    (malloc-name (intern-prefixed "MALLOC-" name))
                    (size-of-name (intern-prefixed "SIZE-OF-" name))
                    (define-name-malloc (c-macro-string malloc-name))
                    (define-name-size (c-macro-string size-of-name))
                    (aref-name (intern-prefixed "AREF-" name))
                    (retreive-name (intern-prefixed "RETREIVE-" aref-name))
                    (retreive-name-string (c-macro-string retreive-name))
                    (give-name (intern-prefixed "GIVE-" aref-name))
                    (give-name-string (c-macro-string give-name))
                    dimension-desc
                    )

               ;; Typ definieren
               ;;---------------
               (write-to-header-file
                (format nil "typedef ~A ~A~{[~A]~};" 
                        (convert-c-type-to-string array-type)
                        c-type dimension-list))
               
               ;; Alloziiere Speicher fuer das Array
               ;;-----------------------------------
               (write-to-header-file
                (format nil "#define ~A() malloc(sizeof(~A))" 
                        define-name-malloc c-type))

               ;; Groesse des Arrays
               ;;-------------------
               (write-to-header-file
                (format nil "#define ~A() sizeof(~A)" 
                        define-name-size c-type))

               ;; erzeuge die lesende Zugriffsfunktion
               ;;-------------------------------------
               (multiple-value-bind (key old-array-name old-array-type)
                   (select-type name array-type array-type)
                 (declare (ignore old-array-type old-array-name))
                 
                 (dotimes (count (length dimension-list))
                   (push (intern (format nil "x~A" count)) dimension-desc))
                 
                 (write-to-header-file 
                  (format nil 
                          "#define ~A(ptr~{,~A~}) (*(((~A *)(ptr)))~{[~A]~})"
                          retreive-name-string dimension-desc 
                          c-type dimension-desc))
                 
                 (case key
                   ((string primitive old-handle ptr)
                    (write-to-header-file
                     (format
                      nil
                      "#define ~A(ptr~{,~A~},value) ((*(((~A *)(ptr)))~{[~A]~}) = ((~A)(value)))"
                      give-name-string dimension-desc 
                      c-type dimension-desc c-type-2)))
                   (otherwise
                    (write-to-header-file
                     (format 
                      nil
                      "#define ~A(ptr~{,~A~},value) memcpy((void *)&((((~A *)(ptr))~{[~A]~})), (void *)(&((~A)(value))), sizeof(~A))"
                      give-name-string dimension-desc 
                      c-type dimension-desc c-type-2 c-type-2)))))))))))

    (dolist (sig *ffi-signatures*)
      (write-to-header-file sig))

    (close *interface-file-stream*)))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun finalise-access (name type original-name &optional (access ""))
  (dolist (slot (rest type))
    (let* ((*print-circle* nil)
           (c-type (convert-c-type-to-string original-name))
           (slot-name (first slot))
           (slot-type (second slot))
           (slot-reader 
            (intern (concatenate 'string (string name) "-" (string slot-name))))
           )
      
      (multiple-value-bind (key old-name old-type)
          (select-type name slot-type slot-type)
        (declare (ignore old-type old-name))
        
        (case key
          ((string primitive old-handle old-struct old-union old-array ptr)
           (let* 
               ((retreive-name (intern-prefixed "RETREIVE-" slot-reader))
                (retreive-name-string (c-macro-string retreive-name))
                (give-name (intern-prefixed "GIVE-" slot-reader))
                (give-name-string (c-macro-string give-name))
                (c-type-2 (convert-c-type-to-string slot-type))
                )
             
             (write-to-header-file
              (format nil "#define ~A(ptr) (((~A *)(ptr))->~A~A)"
                      retreive-name-string c-type access slot-name))
             
             (case key
               ((string primitive old-handle ptr)
                (write-to-header-file
                 (format
                  nil
                  "#define ~A(ptr, value) ((((~A *)(ptr))->~A~A) = ((~A)(value)))"
                  give-name-string c-type access slot-name c-type-2)))
               (otherwise
                (write-to-header-file
                 (format 
                  nil
                  "#define ~A(ptr, value) memcpy(&(((~A *)(ptr))->~A~A), &((~A)(value)), sizeof(~A))"
                  give-name-string c-type 
                  access slot-name c-type-2 c-type-2))))))
          
          ((new-struct new-union)
           ;; Keine Zugriffsfunktion, aber
           ;; Zugriffsfunktionen fuer die Komponenten
           ;;----------------------------------------
           (let* ((access-string 
                   (concatenate 'string access (string slot-name) ".")))
             (finalise-access
              slot-reader slot-type original-name access-string))))))))

;;******************************************************************************
;; C-Typen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Parsen eine C-Typspezifikation
;;------------------------------------------------------------------------------
(defun parse-c-type (type-spec &key 
                               could-be-void 
                               could-be-ptr
                               could-be-vararg
                               could-be-fixnum
                               called-by-ptr)
  (cond 
    ;; String
    ;;-------
    ((eq type-spec 'ffi:c-string)
     (values type-spec))
    
    ;; vordefinierter Typ
    ;;-------------------
    ((or (eq type-spec 'ffi:c-char) 
         (eq type-spec 'ffi:c-short)
         (eq type-spec 'ffi:c-int) 
         (eq type-spec 'ffi:c-long)
         (eq type-spec 'ffi:c-unsigned-char) 
         (eq type-spec 'ffi:c-unsigned-short)
         (eq type-spec 'ffi:c-unsigned-int) 
         (eq type-spec 'ffi:c-unsigned-long)
         (eq type-spec 'ffi:c-float)
         (eq type-spec 'ffi:c-double)
         (eq type-spec 'ffi:c-long-double))
     (values type-spec))

    ;; Handle
    ;;-------
    ((eq type-spec 'ffi:c-handle)
     (values type-spec))
    
    ;; Speziell behandelter Typ
    ;;-------------------------
    ((eq type-spec 'ffi:c-void)
     (if (or could-be-void 
             called-by-ptr)
         (values 'ffi:c-void)
         (clicc-error "Misuse of c-void")))
    
    ((eq type-spec 'ffi:c-vararg)
     (if could-be-vararg
         (values 'ffi:c-vararg)
         (clicc-error "Misuse of c-vararg")))
    
    ((eq type-spec :fixnum)
     (if could-be-fixnum
         (values :fixnum)
         (clicc-error "Misuse of :fixnum")))
    
    ;; Synonymtyp
    ;;-----------
    ((atom type-spec)
     (unless (or (get-fftype type-spec) 
                 (member-of-ffi type-spec)
                 called-by-ptr)
       (clicc-error "Unknown type-specifier ~S" type-spec))
     (values type-spec))

    ;; Struktur  (ffi:c-struct (<bezeichner> <typ>)+)
    ;;---------
    ((and (listp type-spec) 
          (eq (first type-spec) 'ffi:c-struct))
     (if (>= (length type-spec) 2) 
         (values 
          `(ffi:c-struct 
            ,@(maplist
               ;; slot-description
               ;;-----------------
               #'(lambda (r-l)
                   (if (member (caar r-l) (cdr r-l) :key #'car)
                       ;;     <bezeichner>
                       (clicc-error "Identifier used twice")
                       (if (= (length (car r-l)) 2)
                           (values `(,(caar r-l)
                                     ,(parse-c-type (cadar r-l)
                                                    :could-be-ptr t)))
                           (clicc-error ILLEGAL_ARGUMENT 'c-struct))))
               (rest type-spec))))
         (clicc-error ILLEGAL_ARGUMENT 'c-struct)))
    
    ;; Union  (ffi:c-union (<bezeichner> <typ>)+)
    ;;------
    ((and (listp type-spec) 
          (eq (first type-spec) 'ffi:c-union))
     (if (>= (length type-spec) 2) 
         (values 
          `(ffi:c-union
            ,@(maplist
               ;; slot-description
               ;;-----------------
               #'(lambda (r-l)
                   (if (member (caar r-l) (cdr r-l) :key #'car)
                       ;;     <bezeichner>
                       (clicc-error "Identifier used twice")
                       (if (= (length (car r-l)) 2)
                           (values `(,(caar r-l)
                                     ,(parse-c-type (cadar r-l)
                                                    :could-be-ptr t)))
                           (clicc-error ILLEGAL_ARGUMENT 'c-union))))
               (rest type-spec))))
         (clicc-error ILLEGAL_ARGUMENT 'c-union)))
    
    ;; Array  (ffi:c-array <type> <dimensions>)
    ;;------
    ((and (listp type-spec)
          (eq (first type-spec) 'ffi:c-array))
     (if (= (length type-spec) 3)
         (flet ((parse-dimensions (number-or-list)
                  (cond ((typep number-or-list 'fixnum)
                         (values (list number-or-list)))
                        ((listp number-or-list)
                         (mapcar 
                          #'(lambda (a-num) 
                              (if (typep a-num 'fixnum)
                                  (values a-num)
                                  (clicc-error "~A must be a fixnum"
                                               a-num)))
                          number-or-list))
                        (t
                         (clicc-error "~A must be a fixnum" 
                                      number-or-list)))))
           (values
            `(ffi:c-array
              ,(parse-c-type (second type-spec) 
                             :could-be-ptr t)
              ,(parse-dimensions (third type-spec)))))
         (clicc-error ILLEGAL_ARGUMENT 'c-array)))
    
    ;; Pointer
    ;;--------
    ((and (listp type-spec)
          could-be-ptr
          (eq (first type-spec) 'ffi:c-ptr))
     (if (= (length type-spec) 2)
         (values
          `(ffi:c-ptr ,(parse-c-type (second type-spec) 
                                     :called-by-ptr t)))
         (clicc-error ILLEGAL_ARGUMENT 'c-pointer)))
    
    ;; Keine andere Moeglichkeit
    ;;--------------------------
    (t
     (clicc-error "Unknown type-spec ~S." type-spec))))


;;------------------------------------------------------------------------------
;; Liefert zuereuck:
;; (MV) <key> <name> <type>
;;  - string
;;  - primitive
;;  - old-struct
;;  - old-union
;;  - old-array
;;  - old-handle
;;  - new-struct
;;  - new-union
;;  - new-array
;;  - new-handle
;;  - ptr
;;  - void
;;  - *** noch andere ***
;;  sonst Fehler
;;------------------------------------------------------------------------------
(defun select-type (name type ref-type &key iterate)
  (cond
    ;; Kein typ
    ;;---------
    ((eq type nil)
     (internal-error 'select-type "Not a valid c-type: ~A" ref-type))

    ;; String
    ;;-------
    ((eq type 'ffi:c-string) 
     (values 'string name type))

    ;; Handle
    ;;-------
    ((eq type 'ffi:c-handle)
     (if iterate
         (values 'old-handle name type)
         (values 'new-handle name type)))

    ;; Elementarer Type
    ;;-----------------
    ((or (eq type 'ffi:c-char) 
         (eq type 'ffi:c-short)
         (eq type 'ffi:c-int) 
         (eq type 'ffi:c-long)
         (eq type 'ffi:c-unsigned-char) 
         (eq type 'ffi:c-unsigned-short)
         (eq type 'ffi:c-unsigned-int) 
         (eq type 'ffi:c-unsigned-long)
         (eq type 'ffi:c-float)
         (eq type 'ffi:c-double)
         (eq type 'ffi:c-long-double))
     (values 'primitive name type))

    ;; Void
    ;;-----
    ((eq type 'ffi:c-void)
     (values 'void name type))
          
    ;; Synonym
    ;;--------
    ((atom type)
      (select-type type (get-fftype type) ref-type :iterate t))

    ;; Struktur
    ;;---------
    ((eq (first type) 'ffi:c-struct)
     (if iterate
         (values 'old-struct name type)
         (values 'new-struct name type)))
    
    ;; Union
    ;;------
    ((eq (first type) 'ffi:c-union)
     (if iterate
         (values 'old-union name type)
         (values 'new-union name type)))
        
    ;; Array
    ;;------
    ((eq (first type) 'ffi:c-array) 
     (if iterate
         (values 'old-array name type)
         (values 'new-array name type)))

    ;; Pointer
    ;;--------
    ((eq (first type) 'ffi:c-ptr)
     (values 'ptr name type))

    ;; Weitere:
    ;; c-void, c-vararg
    ;;-----------------
     
    ;; Alles andere ist ein Fehler
    ;;----------------------------
    (t (internal-error 'select-type "Not a valid c-type: ~A" ref-type))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun member-of-ffi (type)
  (member type 
          '(ffi:c-char ffi:c-short ffi:c-int ffi:c-long 
            ffi:c-unsigned-char ffi:c-unsigned-short 
            ffi:c-unsigned-int ffi:c-unsigned-long 
            ffi:c-float ffi:c-double ffi:c-long-double 
            ffi:c-handle ffi:c-string ffi:c-void ffi:c-vararg)))

;;******************************************************************************
;; 
;;******************************************************************************
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun p1-foreign-fun-call (operator form)
  (let* ((form-arg-list (rest form))    ; Arg-List der Applikation
         (let-arg-list '())             ; Liste der mit gensym erz. Namen
         (type-list 
          (?arguments operator))    ; Liste der Foreign-Types
         )
    
    ;; Lokale Funktionen zum Zusammenbasteln des Aufrufes
    ;;---------------------------------------------------
    (labels (
             ;; Deklarationsteil des let-Konstruktes
             ;;-------------------------------------
             (make-let-arg-list ()
               (let (liste)
                 (dolist (arg form-arg-list liste)
                   (let ((generated-sym (gensym)))
                     (setq let-arg-list `(,@let-arg-list ,generated-sym))
                     (setq liste `(,@liste (,generated-sym ,arg)))))))
             
             ;; Typtests
             ;;---------
             (make-type-test (var type)
               (multiple-value-bind (key old-name old-type)
                   (select-type type type type)
                 (case key
                   ((new-handle new-union new-struct new-array)
                    'nil)
                   (ptr
                    (let* ((ptr-type (second old-type)))
                      (multiple-value-bind (key old-ptr-name old-ptr-type)
                          (select-type ptr-type ptr-type ptr-type)
                        (declare (ignore old-ptr-type))
                        (case key
                          ((new-handle new-union new-struct new-array)
                           'nil)
                          (otherwise
                           (let* ((package (symbol-package old-ptr-name))
                                  (test-name
                                   (intern-postfixed-package old-ptr-name
                                                             "-P" package)))
                             `(,test-name ,var)))))))
                   (otherwise
                    (let* ((package (symbol-package old-name))
                           (test-name
                            (intern-postfixed-package old-name "-P" package)))
                      `(,test-name ,var))))))
             
             ;; Liste fuer die Typueberpruefung durch AND
             ;;------------------------------------------
             (make-and-list ()
               (let (liste)
                 (dotimes (count (length form-arg-list) liste)
                   (setq liste `(,@liste ,(make-type-test
                                           (nth count let-arg-list)
                                           (nth count type-list)))))))
             
             ;; Aufruf der C-Funktion
             ;;----------------------
             (foreign-call (let-arg-list)
               (let* ((return-type (?return-type operator))
                      (new-name (?other-sym operator)))
                 (multiple-value-bind (key old-name old-type)
                     (select-type return-type return-type return-type)
                   (case key
                     ((new-struct new-union new-handle new-array)
                      `(,new-name ',(gensym) ,@let-arg-list))
                     
                     (ptr
                      (let* ((ptr-type (second old-type)))
                        (multiple-value-bind (key old-ptr-name old-ptr-type)
                            (select-type ptr-type ptr-type ptr-type)
                          (declare (ignore old-ptr-type))
                          (case key
                            ((new-struct new-union new-handle new-array)
                             `(,new-name ',(gensym) ,@let-arg-list))
                            (otherwise 
                             `(,new-name ',old-ptr-name ,@let-arg-list))))))
                     
                     (otherwise 
                      `(,new-name ',old-name ,@let-arg-list)))))))


      ;; Den Aufruf zusammenbasteln
      ;;---------------------------
      (if (null form-arg-list)
          ;; Keine Argumente
          ;;----------------
          (foreign-call let-arg-list)

          ;;Hat Argumente
          ;;-------------
          `(let* ,(make-let-arg-list)
            (if (and ,@(make-and-list))
                ,(foreign-call let-arg-list)
                (error "illegal argument-type")))))))


;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun intern-postfixed-package (symbol-or-string postfix package)
  (intern (concatenate 'string (string symbol-or-string) postfix) package))


;;------------------------------------------------------------------------------
(provide "p1foreign")
