;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Structures
;;;
;;; $Revision: 1.26 $
;;; $Log: p1struct.lisp,v $
;;; Revision 1.26  1994/06/03  14:09:33  hk
;;; :PRINT-FUNCTION wird ber"ucksichtigt
;;;
;;; Revision 1.25  1994/01/13  16:36:23  sma
;;; Statt set-struct-ref/set-struct-constructor werden jetzt setf-Methoden
;;; benutzt.
;;;
;;; Revision 1.24  1993/06/29  10:22:53  wg
;;; Einige (clicc-warning ...) in (clicc-message ...) geaendert.
;;;
;;; Revision 1.23  1993/06/25  09:02:11  wg
;;; BOA-Konstruktoren (by order of arguments) mit einfachen
;;; Parameterlisten fuer (DEFSTRUCT ...) eingebaut. (Steele S.482f).
;;; Mehrfach-Definition der Option (:CONSTRUCTOR ...) und
;;; (:CONSTRUCTOR nil) sind jetzt moeglich.
;;;
;;; Revision 1.22  1993/06/22  12:54:15  uho
;;; Typ und default-Wert von name in Struktur SLOT auf Symbol/NIL geandert.
;;;
;;; Revision 1.21  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.20  1993/04/22  11:32:41  hk
;;; Sonderbehandlung bei *CLICC-MODULE*, *CLICC-LISP-PROGRAM* gestrichen.
;;;
;;; Revision 1.19  1993/04/14  12:12:31  hk
;;; L:: eingefuegt, Inline Deklarationen entfernt,
;;; (defun (setf f) ..) fuer Slot-Update Funktionen verwendet.
;;;
;;; Revision 1.18  1993/03/18  12:13:33  ft
;;; Abbruch fuer nicht implementierte Struktur Optionen hinzugefuegt.
;;;
;;; Revision 1.17  1993/02/17  07:10:15  kl
;;; Falsche Klammerung in p1-defstruct behoben.
;;;
;;; Revision 1.16  1993/02/16  16:39:13  hk
;;; Revision Keyword eingefuegt, Zugriffe auf *CLICC-PACKAGE* gestrichen.
;;;
;;; Revision 1.15  1993/01/22  15:50:10  ft
;;; Vorkommen von STRING-CHAR in p1-defstruct-options durch STANDARD-CHAR
;;; ersetzt.
;;;
;;; Revision 1.14  1993/01/11  15:35:12  hk
;;; Schreibfehler
;;;
;;; Revision 1.13  1993/01/11  15:34:28  hk
;;; Schreibfehler
;;;
;;; Revision 1.12  1993/01/11  14:36:18  hk
;;; structure -> struct
;;;
;;; Revision 1.11  1993/01/11  11:11:18  hk
;;; Defaultwerte von :type und :read-only bei included Slots korrigiert.
;;;
;;; Revision 1.10  1993/01/08  15:45:35  hk
;;; Fehler bei :include + :readonly behoben.
;;;
;;; Revision 1.9  1993/01/07  14:01:51  hk
;;; Abfrage gestrichen, die verhinderte, einen Slot mit dem Namen einer
;;; benannten Konstanten in defstruct zu definieren. Die Abfrage war motiviert
;;; durch eine nicht verstandene Fehlermeldung in Lucid CL 4.0.
;;;
;;; Revision 1.8  1992/08/11  16:19:13  hk
;;; Fehler mit (go ..) in p1-defstruct-options beseitigt.
;;;
;;; Revision 1.7  1992/08/05  10:01:36  hk
;;; Nur syntaktische Aenderungen.
;;;
;;; Revision 1.6  1992/08/05  09:59:07  hk
;;; Nur syntaktische Aenderungen.
;;;
;;; Revision 1.5  1992/07/06  11:10:17  hk
;;; Komponente init-value in Slot in init-form umbenannt.
;;; Init-form wird nicht mehr in Pass1 evaluiert, bzw. nur bei literal
;;; Instances.
;;;
;;; Revision 1.4  1992/07/02  13:56:52  hk
;;; structdescr-slots enthaelt Vector und muss entsprechend bearbeitet werden.
;;;
;;; Revision 1.3  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.2  1992/06/01  10:24:34  hk
;;; defstructs fuer structdescr und slot aus clcdef eingefuegt.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;; Changes  : 03.07.91 CLICC-SUBTYPEP gibt Warnung, falls es keine exakte
;;;                     Aussage ueber die angegebenen Typen machen kann.
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Jeder Strukturdefinition des Programms wird eine Struktur vom Typ
;; STRUCTDESCR zugeordnet, in die die spezifizierten Eigenschaften
;; der definierten Struktur eingetragen werden.
;;------------------------------------------------------------------------------
(defstruct STRUCTDESCR
   (symbol         nil :type (or symbol sym))
   (conc-name      "" :type string)
   (constructor    nil :type (or symbol list))
   (copier         nil :type symbol)
   (predicate      nil :type symbol)
   (include        nil :type symbol)
   (print-function nil :type t)
   (slots          nil :type (or null vector)))

;;------------------------------------------------------------------------------
;; Jedem SLOT einer im Programm definierten Struktur wird ein SLOT-Deskriptor
;; zugeordnet, der die Informationen ueber die Attribute des SLOT's enthaelt.
;;------------------------------------------------------------------------------
(defstruct SLOT
  (name        nil  :type symbol)
  (index          0 :type fixnum)
  (init-form   nil :type t)
  (type           t :type t)
  (read-only    nil :type (member t nil)))

;;------------------------------------------------------------------------------
;; DEFSTRUCT <name-and-options> [doc-string] { <slot-description> }*
;;     s. 19. Structures  S. 305
;;
;; <name-and-options> ::= <struct-name> |
;;                        ( <struct-name> { <option> }*)
;;
;; <option> ::= :CONC-NAME   | (:CONC-NAME   [<generalized-string>]) |
;;              :CONSTRUCTOR | (:CONSTRUCTOR [<symbol>] [<lambda-list>]) |
;;              :COPIER      | (:COPIER      [<symbol>]) |
;;              :PREDICATE   | (:PREDICATE   [<symbol>]) |
;;              :INCLUDE     |
;;              (:INCLUDE <struct-name> { <slot-description> }*)
;;              :PRINT-FUNCTION | (:PRINT-FUNCTION [<function>])
;;
;; <slot-description> ::= <slot-name> |
;;                        ( <slot-name> [<default-init> { <slot-option> }*])
;;
;; <slot-option> ::= (:TYPE <type>) || (:READ-ONLY <form>)
;;------------------------------------------------------------------------------
(defun p1-defstruct (name-and-options_rest)
  (when (atom name-and-options_rest)
    (clicc-error ILLEGAL_CALL "DEFSTRUCT"
                 "(NAME-AND-OPTIONS &BODY SLOT-DESCRIPTIONS)"))
  (let ((name-and-options   (first name-and-options_rest))
        ([doc-string]_slots (rest  name-and-options_rest))
        struct-name                     ; Name der Struktur als Symbol
        (doc-string "")
        slots
        
        struct-info                     ; Struktur vom Typ STRUCTDESCR
        num-slots                       ; Anzahl der Slot's
        slot-descriptors                ; Vektor aus SLOT-Deskriptoren
        
        ;; Fuer die :INCLUDE Option wird benoetigt:
        ;;-----------------------------------------
        include-slots                   ; Vektor der INCLUDE-Slot-Deskriptoren
        new-slot-descriptors-for-included-slots

        ;; Fuer den Structure-Reader mindestens einen Keyword-Constructor:
        ;;----------------------------------------------------------------
        reader-constructor              ; (gensym), falls nur BOA Konstruktoren
        
        ;; Code, der automatisch bei einer Strukturdefinition erzeugt wird.
        ;;-----------------------------------------------------------------
        (defstruct-code ()))
    ;;---------------------------------
    ;; Den Namen der Struktur bestimmen
    ;;---------------------------------
    (when (atom name-and-options)
      (setq name-and-options (list name-and-options)))
    (setq struct-name (first name-and-options))
    (unless (symbolp struct-name)
      (clicc-error "~A should be a defstruct name" struct-name))
    (when (get-struct-def struct-name)
      (clicc-error "Structure ~A declared twice." struct-name))
    
    (clicc-message "Analyse DEFSTRUCT    ~A" struct-name)
    
    ;;------------------------------------------------------------------
    ;; Fuer jede Strukturdefinition wird eine Beschreibung in Form einer
    ;; Struktur vom Typ STRUCTDESCR in der Uebersetzungsumgebung abgelegt.
    ;; Zunaechst werden die Default-Werte fuer die DEFSTRUCT-Optionen
    ;; eingetragen.
    ;;------------------------------------------------------------------
    (setq struct-info
          (make-structdescr
           :symbol      struct-name
           :conc-name   (concatenate 'string (string struct-name) "-")
;;;           :constructor (list (intern-prefixed "MAKE-" struct-name))
           :constructor nil
           :copier      (intern-prefixed "COPY-" struct-name)
           :predicate   (intern-postfixed struct-name "-P")
           :print-function 'std-struct-printer))
    
    ;;-----------------------------------
    ;; Analysieren der DEFSTRUCT-Optionen
    ;;-----------------------------------
    (multiple-value-setq (include-slots new-slot-descriptors-for-included-slots)
      (p1-defstruct-options (rest name-and-options) struct-info))

    ;;-------------------------------------------------
    ;; [doc-string] { <slot-description> }* analysieren
    ;;-------------------------------------------------

    ;; Dokumentations-String vorhanden ?
    ;; ---------------------------------
    (cond
      ((p1-endp [doc-string]_slots)
       (setq slots ()))
      ((stringp (first [doc-string]_slots))
       (setq doc-string (first [doc-string]_slots)
             slots      (rest  [doc-string]_slots)))
      (t (setq slots      [doc-string]_slots)))
    
    ;; Eintragen der SLOT-Deskriptoren
    ;; -------------------------------
    (let ((initial-offset (length include-slots)) index slot-descriptor)
      
      ;; Anzahl der SLOTs berechnen und Vektor fuer SLOT-Deskriptoren anlegen
      ;; --------------------------------------------------------------------
      (setq num-slots        (+ initial-offset (length slots)))
      (setq slot-descriptors (make-array num-slots))
      (setf (structdescr-slots struct-info) slot-descriptors)
      
      ;; :INCLUDE-Slot's eintragen
      ;; -------------------------
      ;; Zuerst werden die Default-Werte aus der INCLUDE-Struktur uebernommen,
      ;; bevor die explizit spezifizierten Default-Werte eingetragen werden.
      
      (replace slot-descriptors include-slots)
      (dolist (include-slot new-slot-descriptors-for-included-slots)
        (setq index (slot-index include-slot))
        (setf (elt slot-descriptors index) include-slot))
      
      ;; Die restlichen Slot's analysieren
      ;; ---------------------------------
      (setq index initial-offset)
      (dolist (slot-description slots)

        ;; Slot-Beschreibung analysieren und ueberpruefen, ob der Slot-Name
        ;; bereits in der Strukturdefinition benutzt wird.
        ;;------------------------------------------------
        (setq slot-descriptor (p1-slot-description slot-description))
        (let ((slot-name (slot-name slot-descriptor)))
          (dotimes (i (1- index))
            (when (string= slot-name (slot-name (elt slot-descriptors i)))
              (clicc-error "More than one slot is named ~A" slot-name))))
        
        ;; Index in den Deskriptor eintragen
        ;;----------------------------------
        (setf (slot-index slot-descriptor) index)
        
        ;; Eintragen des Deskriptors in das Array fuer die Slot-Beschreibungen
        ;;--------------------------------------------------------------------
        (setf (elt slot-descriptors index) slot-descriptor)
        (incf index)))
    
    ;;--------------------------------
    ;; 0. Neuen Datentyp bekannt geben
    ;;--------------------------------
    (let ((predicate (structdescr-predicate struct-info)))
      
      ;; Soll keine Praedikatsfunktion fuer diese Struktur erzeugt werden,
      ;; wird die Praedikatsfunktion unter einem neu erzeugten Namen definiert.
      ;;-----------------------------------------------------------------------
      (when (not predicate)
        (setq predicate (gensym))
        (setf (structdescr-predicate struct-info) predicate))
      
      (push
       `(L::DEFTYPE ,struct-name () '(L::SATISFIES ,predicate))
       defstruct-code))
    
    ;;---------------------------------
    ;; 1. Konstruktor-Funktionen erzeugen
    ;;---------------------------------
    (if (structdescr-constructor struct-info)
        (let ((slot_init-list
               (map 'list #'(lambda (s)
                              (list (slot-name s) (slot-init-form s)))
                    slot-descriptors))
              (slot-list
               (map 'list #'(lambda (s) (slot-name s)) slot-descriptors)))
          (dolist (constr (structdescr-constructor struct-info))
            (if (consp constr)

                ;; Erzeuge BOA-Constructor.
                ;;-------------------------
                (let ((cname (car constr)) (cparams (cadr constr)))
                  (unless (subsetp cparams slot-list)
                    (clicc-error "Constructor parameter is not a slot in ~A."
                                 (cons :CONSTRUCTOR constr)))
                  (let ((arguments
                          (mapcar #'(lambda (slot_init)
                                      (if (member (car slot_init) cparams)
                                          (car slot_init)
                                          (cadr slot_init)))
                                  slot_init-list)))
                    (push
                      `(L::DEFUN ,cname ,cparams
                         (rt::MAKE-STRUCT (L::QUOTE ,struct-name) ,@arguments))
                     defstruct-code)))
                  
                ;; Erzeuge Keyword-Parameter-Constructor.
                ;;---------------------------------------
                (unless (null constr)
                  (setq reader-constructor constr)
                  (push
                    `(L::DEFUN ,constr
                     (L::&KEY ,@slot_init-list)
                     (rt::MAKE-STRUCT (L::QUOTE ,struct-name) ,@slot-list))
                   defstruct-code))))))

    ;;----------------------------
    ;; 2. Kopier-Funktion erzeugen
    ;;----------------------------
    (let ((copier (structdescr-copier struct-info)))
      (when copier
        (push
         `(L::DEFUN ,copier (struct)
           (RT::COPY-STRUCT struct (L::QUOTE ,struct-name)))
         defstruct-code)))

    ;;--------------------------------
    ;; 3. Praedikats-Funktion erzeugen
    ;;--------------------------------
    (let ((predicate (structdescr-predicate struct-info)))
      (when predicate
        (push
         `(L::DEFUN ,predicate (object)
           (rt::STRUCT-TYPEP object (L::QUOTE ,struct-name)))
         defstruct-code)))

    ;;--------------------------------
    ;; 4. Zugriffs-Funktionen erzeugen
    ;;--------------------------------
    (let ((conc-name (structdescr-conc-name struct-info)))
      (flet ((p1-make-struct-access-fn (slot-descriptor)
               (let ((conc-name-slot
                      (intern-prefixed conc-name (slot-name slot-descriptor)))
                     (index (slot-index slot-descriptor)))
                 (push
                  `(L::DEFUN ,conc-name-slot (struct)
                    (rt::STRUCT-REF struct ,index (L::QUOTE ,struct-name)))
                  defstruct-code)
                 (push 
                  `(L::DEFUN (L::SETF ,conc-name-slot) (newvalue struct)
                    (L::SETF (rt::STRUCT-REF
                              struct ,index (L::QUOTE ,struct-name)) newvalue))
                  defstruct-code))))
        
        ;; Body of FLET
        ;;-------------
        (map nil #'p1-make-struct-access-fn slot-descriptors)))

    ;;-------------------------------------------------------------------
    ;; die Uebersetzungsumgebung wird um die Strukturdefinition erweitert
    ;;-------------------------------------------------------------------
    (set-struct-def struct-name struct-info)

    ;;-------------------------------------------------
    ;; Uebersetzen der automatisch erzeugten Funktionen
    ;;-------------------------------------------------
    (let ((*SDF* T))                    ; System-Defined-Function
      (p1-top-level-form
       `(L::PROGN ,@(reverse defstruct-code)))
      
      ;; Ist die Struktur SUBTYPE einer anderen Struktur,
      ;; muss dies in der Laufzeitumgebung vermerkt werden.
      ;; STRUCT-TYPEP benoetigt diese Information, um feststellen zu koennen,
      ;; ob ein Objekt eine Struktur vom spezifizierten Typ ist.
      ;;--------------------------------------------------------
      (when (structdescr-include struct-info)
        (p1-top-level-form
         `(L::SETF (L::GET ',struct-name 'rt::INCLUDED-STRUCT)
           ',(structdescr-include struct-info))))

      ;; Dem PRINTER wird die print-function bekanntgegeben:
      ;;----------------------------------------------------
      (let ((print-function (structdescr-print-function struct-info)))
        (unless (eq print-function 'std-struct-printer)
          (p1-top-level-form
           `(L::SETF (rt::struct-printer ',struct-name)
             ,(if (eq 'include print-function)
                  `(rt::struct-printer
                    ',(structdescr-include struct-info))
                  `(L::FUNCTION ,print-function))))))

      ;; Dem READER wird der reader-constructor bekanntgegeben:
      ;;-------------------------------------------------------
      (if reader-constructor
          (p1-top-level-form
           `(L::SETF (rt::struct-constructor ',struct-name)
             (L::FUNCTION ,reader-constructor)))
          (clicc-message "No structure reader function for ~A." struct-name)))))

;;------------------------------------------------------------------------------
;; Analysiert die Optionen, die bei der Definition einer Struktur
;; mit DEFSTRUCT angegeben wurden.
;; Rueckgabewert: 2 Werte:
;;                - Vektor der 'included'-Slots
;;                - Liste der Slot-Deskriptoren der 'included'-Slots,
;;                  fuer die neue Slot-Beschreibungen spezifiziert wurden.
;;------------------------------------------------------------------------------
(defun p1-defstruct-options (options struct-info)
   (let ((*CURRENT-FORM* options)       ; Fuer Fehlerroutine
         argument                       ; Argument der Option
         argument-p                     ; Supplied-p fuer das Argument
         (options-supplied (make-array 6 :initial-element nil))
         include-slots                  ; Vektor der Slot-Deskriptoren
         (new-slot-descriptors-for-included-slots ()))
      (dolist (option options)
        (when (atom option)
          (setq option (list option)))
        (labels
            
            ;; Ermittelt zu einer DEFSTRUCT-Option das Argument.
            ;;--------------------------------------------------
            ((get-defstruct-option-arg (option-index)
               
                   ;; Wurde die Option bereits spezifiert ?
                   ;;--------------------------------------

                   (when (elt options-supplied option-index)
                     (if (= option-index 1)

                         ;; :CONSTRUCTOR-Option ist mehrfach erlaubt. (S.483)
                         ;;--------------------------------------------------
                         (clicc-message "More than one ~S option specified."
                                        :CONSTRUCTOR)

                         (clicc-error "~A option has already been specified."
                                      option)))

                   (setf (elt options-supplied option-index) t)

                   ;; Argument ermitteln
                   ;;-------------------
                   (let ((argument_rest (rest option)))
                     (if (p1-endp argument_rest)
                         (setq argument-p nil)
                         (progn
                           (when (and (/= option-index 4) (/= option-index 1))
                             (when (rest argument_rest)
                               (clicc-error "~A option has too many arguments."
                                            option))
                             (setq argument (first argument_rest)))
                           (when (= option-index 1)
                             (if (rest argument_rest)
                                 (if (rest (rest argument_rest))
                                     (clicc-error "~A option has too many arguments."
                                            option)
                                     (setq argument argument_rest))
                                 (setq argument (first argument_rest))))
                           (setq argument-p t)))))
          
             (arg_no_symbol ()
               (clicc-error "The argument ~A of the ~A option must be a symbol."
                            argument (first option))))
          
          (case (first option)
            (:CONC-NAME
             (get-defstruct-option-arg 0)
             (when argument-p
               (when (not (typep argument 
                                 '(or null symbol string standard-char)))
                 (clicc-error
                  "The argument ~A of :CONC-NAME is not acceptable for STRING"
                  argument))
               (setf (structdescr-conc-name struct-info)
                      (if (null argument) "" (string argument)))))

            (:CONSTRUCTOR
             (get-defstruct-option-arg 1)
             (when argument-p

               ;; Pruefe auf einfache Lambda-Liste, falls BOA-Constructor.
               ;;---------------------------------------------------------
               (when (consp argument)
                 (dolist (param (cadr argument))
                   (unless (and (symbolp param) 
                                (not (lambda-list-keyword-p param)))
                     (clicc-error "Illegal parameter ~A in ~A." param option))))

               ;; Constructor-Name bereits vorhanden ?
               ;;-------------------------------------
               (dolist (constr (structdescr-constructor struct-info))
                 (let ((cname (if (consp constr) (car constr) constr))
                       (nname (if (consp argument) (car argument) argument)))
                   (when (eq cname nname)
                     (clicc-error "Constructor ~A defined twice." nname))))
                           
        
               ;; Fuege Option an die Constructor-Liste an.
               ;;------------------------------------------
;;;               (setf (structdescr-constructor struct-info)
;;;                 (cons argument (structdescr-constructor struct-info)))))
               (push argument (structdescr-constructor struct-info))))
                   
            (:COPIER
             (get-defstruct-option-arg 2)
             (when argument-p
               (when (not (symbolp argument)) (arg_no_symbol))
               (setf (structdescr-copier struct-info) argument)))

            (:PREDICATE
             (get-defstruct-option-arg 3)
             (when argument-p
               (when (not (symbolp argument)) (arg_no_symbol))
               (setf (structdescr-predicate struct-info) argument)))

            (:INCLUDE
             (get-defstruct-option-arg 4)
             (when argument-p
               (let ((include-name (second option))
                     include-struct-info
                     (include-slot-names ()))
                 (when (not (symbolp include-name))
                   (clicc-error NO_SYMBOL include-name))
                 (setq include-struct-info (get-struct-def include-name))
                 (when (null include-struct-info)
                   (clicc-error
                    ":INCLUDE must name a DEFSTRUCT that is declared before."))
                 (setf (structdescr-include struct-info) include-name)

                 (unless (elt options-supplied 5)
                   ;; :print-function noch nicht angegeben: vererben
                   (setf (structdescr-print-function struct-info) 'include))
                 (setq include-slots (structdescr-slots include-struct-info))

                 (dolist (new-slot-description (cddr option))
                   (let* ((slot-name (if (atom new-slot-description)
                                         new-slot-description
                                         (car new-slot-description)))
                          (include-slot-descriptor (find slot-name
                                                         include-slots
                                                         :key  #'slot-name
                                                         :test #'string=))
                          new-slot-descriptor)
                     (when (null include-slot-descriptor)
                       (clicc-error "No slot named ~A in DEFSTRUCT ~A"
                                    slot-name include-name))
                     (when (member slot-name include-slot-names
                                   :test #'string=)
                       (clicc-error "Duplicate include slot description in ~A"
                                    new-slot-description))
                     (push slot-name include-slot-names)
                     (setq new-slot-descriptor
                           (p1-slot-description
                            new-slot-description
                            :default-type (slot-type include-slot-descriptor)
                            :default-read-only (slot-read-only
                                                include-slot-descriptor)))

                     ;; Index des Slots in den Deskriptor eintragen
                     ;;--------------------------------------------
                     (setf (slot-index new-slot-descriptor)
                           (slot-index include-slot-descriptor))

                     (push new-slot-descriptor
                           new-slot-descriptors-for-included-slots))))))

            (:PRINT-FUNCTION
             (get-defstruct-option-arg 5)
             (setf (structdescr-print-function struct-info)
                   (if argument-p
                       argument
                       'std-struct-printer)))
            ((:TYPE :NAMED :INITIAL-OFFSET)
             (clicc-error
              "The defstruct option ~S has not been implemented yet."
              (first option)))
            (t (clicc-error ILLEGAL_KEY
                            '(:CONC-NAME :CONSTRUCTOR :COPIER :PREDICATE
                              :INCLUDE :PRINT-FUNCTION)
                            (first option))))))

   (unless (elt options-supplied 1)
     (setf (structdescr-constructor struct-info) 
           (list (intern-prefixed "MAKE-" (structdescr-symbol struct-info)))))
      
   (values include-slots new-slot-descriptors-for-included-slots)))

;;------------------------------------------------------------------------------
;; Prueft, ob 'type1' im Compilerkontext ein subtype
;; von 'type2' ist.
;; Bemerkung    : Da noch keine Typanalyse unterstuetzt wird, wird T
;;                zurueckgegeben.
;;------------------------------------------------------------------------------
(defun clicc-subtypep (type1 type2)
  (cond
    ((eq type2 t) t)
    ((equal type1 type2) t)
    (T (clicc-warning "Assuming that ~A is subtype of ~A" type1 type2) t)))

;;------------------------------------------------------------------------------
;; Analysiert eine Beschreibung fuer einen Slot.
;; Rueckgabewert: Ein Slot-Deskriptor in Form einer Struktur vom Typ SLOT
;;------------------------------------------------------------------------------
(defun p1-slot-description (slot-description &key
                                             (default-type t)
                                             (default-read-only nil))
  (let (name default-init type read-only)
    (when (atom slot-description)
      (setq slot-description (list slot-description)))

    (setq name (pop slot-description))
    (setq default-init
          (if slot-description
              (if (atom slot-description)
                  (clicc-error "illegal slot description")
                  (pop slot-description))
              nil))
    (when (oddp (length slot-description))
      (clicc-error "Unpaired item in keyword portion of an argument list"))
    (setq type (getf slot-description :type default-type))
    (setq read-only (getf slot-description :read-only default-read-only))

    ;; Ueberpruefen, ob die neuen :TYPE und :READ-ONLY Optionen
    ;; konsistent mit den alten Optionen sind.
    ;;----------------------------------------
    (when (not (clicc-subtypep type default-type))
      (clicc-error "~A is not a subtype of ~A, in slot ~A"
                   type default-type name))
    (when (and default-read-only (not read-only))
      (clicc-error "Slot ~A cannot be made writable." name))
    
    (unless (symbolp name)
      (clicc-error "~A should be a defstruct slot name." name))

    ;; SLOT-Deskriptor erzeugen
    ;;-------------------------
    (make-slot :name       name
               :init-form  default-init
               :type       type
               :read-only  read-only)))

;;------------------------------------------------------------------------------
;; Liest konstante Strukturen des zu uebersetzenden Programms.
;; Rueckgabewert: Eine Struktur vom Typ STRUCTURE
;;------------------------------------------------------------------------------
(defun p1-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (catch 'CLICC-ERROR
    (let* ((const-struct    (read stream))
           (*CURRENT-FORM*  const-struct)
           struct-name                  ; Name der konstanten Struktur
           name-value-list              ; Slot-Namen und Slot-Werte
           struct-info)                 ; Die Struktur vom Typ STRUCTDESCR

      (when arg
        (clicc-error "The extra argument ~A was supplied for the '#S readmacro."
                     arg))
      (when (atom const-struct)
        (clicc-error "READ encountered '#S~A, but a list must follow the '#S."
                     const-struct))
      (setq struct-name (first const-struct))
      (when (not (symbolp struct-name))
        (clicc-error NO_SYMBOL struct-name))
      (setq struct-info (get-struct-def struct-name))
      (unless struct-info
        (clicc-error "~S is not a structure." struct-name))
      (setq name-value-list (rest  const-struct))
      (when (oddp (length name-value-list))
        (clicc-error "odd number of slot-value entries in constant structure"))
      
      ;; Bestimmen der Slot-Werte
      ;;-------------------------
      (let* ((secret (gensym))
             (value-list (map 'list
                              #'(lambda (slot)
                                  (let ((value (getf name-value-list
                                                     (slot-name slot)
                                                     secret)))
                                    (if (eq secret value)
                                        (p1-eval (slot-init-form slot))
                                        value)))
                              (structdescr-slots struct-info))))

  
        ;; Die erzeugte Struktur als Ergebnis liefern
        ;;-------------------------------------------
        (make-instance 'literal-instance
                       :class struct-name
                       :value-list value-list)))))

;;------------------------------------------------------------------------------
(provide "p1struct")
