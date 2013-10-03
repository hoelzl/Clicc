;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Pass 1 der Klassenverarbeitung, sowie der Instantierung
;;;
;;; $Revision: 1.63 $
;;; $Log: p1class.lisp,v $
;;; Revision 1.63  1994/02/21  10:04:42  ft
;;; Parameterprüfung in p1-defclass erweitert; Neue Funktion
;;; export-classes zur Vorbereitung von Klassen auf das Exportieren hin
;;; zugefügt.
;;;
;;; Revision 1.62  1994/01/26  13:36:53  ft
;;; Änderung der Darstellung von ungebundenen Slots.
;;;
;;; Revision 1.61  1994/01/21  16:51:15  ft
;;; Zweite Behelfskorrektur an *SECRET-UNBOUND-SLOT-VALUE*.
;;;
;;; Revision 1.60  1994/01/21  16:48:51  ft
;;; Behelfskorrektur an *SECRET-UNBOUND-SLOT-VALUE*.
;;;
;;; Revision 1.59  1994/01/21  08:25:00  ft
;;; Änderung der Zwischensprachrepr. des Werts unbound für Slots von einem
;;; String zu einem Symbol (unintern).
;;;
;;; Revision 1.58  1993/12/14  12:27:25  hk
;;; In parse-slot-specifier: 'setf -> 'L::setf
;;;
;;; Revision 1.57  1993/11/22  13:13:14  ft
;;; compute-class-precedence-list in compute-c-p-l wegen Konflikten mit
;;; gleichnamiger Funktion in Allegro CL umbenannt.
;;;
;;; Revision 1.56  1993/11/10  09:47:52  ft
;;; Parameteranalyse in p1-defclass erweitert. Vorkommen von T in
;;; Superklassenlisten werden gestrichen, Bei mehrfacher Nennung einer
;;; Klasse wird ein Fehler ausgegeben.
;;;
;;; Revision 1.55  1993/10/13  14:15:28  ft
;;; Fehler beim Umwandeln der Klassensymbole behoben.
;;;
;;; Revision 1.54  1993/10/13  12:26:22  ft
;;; Falls der Fehler bei einer referenzierten aber nicht definierten
;;; Klasse fortgesetzt wird, werden jetzt Default-Belegungen f"ur die
;;; Klasse ermittelt und diese in die class-def-list des aktuellen Moduls
;;; eingetragen.
;;;
;;; Revision 1.53  1993/09/30  09:35:31  ft
;;; Einbau eines forsetzbaren Fehlers, falls mehr als ein initarg
;;; fuer einen Slot angegeben wird.
;;;
;;; Revision 1.52  1993/09/17  13:53:14  ft
;;; Explizite Angabe des Tests bei allen Aufrufen von assoc die nur eq
;;; benoetigen.
;;;
;;; Revision 1.51  1993/09/09  09:27:10  ft
;;; Bei einfacher Vererbung werden die Positionen von Slots erhalten.
;;;
;;; Revision 1.50  1993/09/09  09:05:47  ft
;;; Aenderung der durch den order Slot festgelegten Reihenfolge.
;;;
;;; Revision 1.49  1993/09/07  09:48:27  ft
;;; Verwaltung der Annotation order an definierten Klassen implementiert.
;;;
;;; Revision 1.48  1993/09/06  08:43:29  ft
;;; Das Compiler-Flag *OPTIMIZE* wird jetzt beachtet.
;;;
;;; Revision 1.47  1993/07/14  08:52:50  ft
;;; Anpassung an die geänderten Parameter von instance-ref/set.
;;;
;;; Revision 1.46  1993/06/29  11:40:48  ft
;;; Ungenutzte Variable in compute-class-precedence-list entfernt.
;;;
;;; Revision 1.45  1993/06/26  14:22:55  ft
;;; Kommentare und Fehlermeldungen verschönt.
;;;
;;; Revision 1.44  1993/06/26  14:18:00  ft
;;; Meldung beim Finalisieren von built-in's,
;;; Optimierung der Position von Slots,
;;; Fehler beim Linearisieren der class-precedence-list behoben.
;;;
;;; Revision 1.43  1993/06/23  09:18:41  uho
;;; Explizite Bloecke fuer CLISP um die Funktionen topological-sort
;;; und select-other-choice gelegt.
;;;
;;; Revision 1.42  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.41  1993/06/16  15:04:53  ft
;;; an diversen Stellen L:: eingefügt.
;;;
;;; Revision 1.40  1993/06/16  11:31:45  ft
;;; parser-function für defclass-parameter überarbeitet.
;;;
;;; Revision 1.39  1993/05/22  11:59:55  ft
;;; Neue bessere und viel richtigere Verarbeitung von Slot-Initforms.
;;;
;;; Revision 1.38  1993/05/19  13:45:22  ft
;;; Zugriffe auf den metaclass-slot gestrichen.
;;;
;;; Revision 1.37  1993/05/13  13:19:34  ft
;;; lokale Funktion extract-slot-info in finalize-classes gestrichen, da
;;; sie nicht mehr benutzt wird
;;;
;;; Revision 1.36  1993/04/20  14:28:33  ft
;;; Erweiterung um das Finalisieren der Built-In-Klassen.
;;;
;;; Revision 1.35  1993/04/15  13:04:38  ft
;;; symbolize-class-names wandelt jezt auch die Slot-Bezeichner um.
;;;
;;; Revision 1.34  1993/04/13  09:19:30  ft
;;; Neue initform fuer ungebundene Slots.
;;;
;;; Revision 1.33  1993/04/08  10:13:16  ft
;;; Ausgabe der Finalisierungsmeldung korrigiert, und neu Pruefung auf
;;; doppelte Slot-Definitionen.
;;;
;;; Revision 1.32  1993/04/06  15:38:51  ft
;;; Fehler bei falscher Lambda-liste fuer Defclass.
;;;
;;; Revision 1.31  1993/04/06  14:02:47  ft
;;; Ueberfuehren der c-p-l in Zwischensprache jetzt 'von Hand'.
;;;
;;; Revision 1.30  1993/04/03  10:06:22  hk
;;; p1-make-instance als Quelltext transform. compiler-macro implementiert.
;;;
;;; Revision 1.29  1993/03/30  12:27:59  ft
;;; Verarbeitung der special-form find-class.
;;;
;;; Revision 1.28  1993/03/25  10:14:46  ft
;;; Klassename wird waehrend des Finalisierens in ein 'defined-sym' umgewandelt.
;;;
;;; Revision 1.27  1993/03/23  07:34:00  ft
;;; Verarbeitung der Slot-Initforms korrigiert,
;;; p1-make-instance an die Veraenderung im Laufzeitsystem angepasst,
;;;
;;; Revision 1.26  1993/03/19  08:54:26  ft
;;; Slot-Initforms enthalten jetzt Zwischensprachkonstrukte.
;;;
;;; Revision 1.25  1993/03/12  09:43:29  ft
;;; Erweiterung um p1-make-instance; Anpassung der Werte von 'class-def'-Slots
;;; an Codeerzeugung fuer Klassen; geaenerte Repraesentation des Unbound-Werts
;;; fuer Slots.
;;;
;;; Revision 1.24  1993/02/22  16:07:12  ft
;;; Ausgabe einer 'clicc-message' waehrend des Finalisierens.
;;;
;;; Revision 1.23  1993/02/16  17:02:19  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.22  1993/02/10  09:40:43  ft
;;; Erweiterung der Spezialisierungsmoeglichkeit auf einen Parameter
;;; an beliebiger Position.
;;;
;;; Revision 1.21  1993/01/22  14:54:35  ft
;;; Es werden jetzt die erweiterten Funktionsnamen genutzt.
;;;
;;; Revision 1.20  1993/01/15  17:08:02  kl
;;; Fehler in p1-defclass behoben. In einem if-Ausdruck im Fall :CLASS standen
;;;
;;; Revision 1.19  1993/01/15  11:52:28  ft
;;; Fast alle clicc-error in clc-error umgewandelt und
;;; ein catch fuer clicc-error in finalize-class installiert.
;;;
;;; Revision 1.18  1993/01/14  15:51:35  ft
;;; Unterdruecken der lokalen Funktionen bei Slot-Zugriffs-Methoden
;;; und Auswerten der definierten Slot-Typen.
;;;
;;; Revision 1.17  1993/01/13  12:19:43  ft
;;; Setzen von *CURRENT-FORM* fuer Aufrufe von clicc-error
;;;
;;; Revision 1.16  1992/12/21  07:39:37  ft
;;; Message-String verschoenert.
;;;
;;; Revision 1.15  1992/12/18  09:17:44  ft
;;; UNBOUND als Slot-Wert implementiert.
;;;
;;; Revision 1.14  1992/12/10  11:02:53  ft
;;; Erzeugen einer Laufzeit-Initialisierungsfkt.
;;;
;;; Revision 1.13  1992/12/03  15:36:49  ft
;;; Zwischenstand der Fehlerbeseitigung.
;;;
;;; Revision 1.12  1992/11/17  14:47:25  ft
;;; Fehler im Rueckgabewert von generate-slot-desc korrigiert.
;;;
;;; Revision 1.11  1992/11/17  13:22:32  ft
;;; Verarbeitung der Writer-Methoden modifiziert.
;;;
;;; Revision 1.10  1992/11/12  10:55:14  ft
;;; parse-class-options liefert jetzt auch documentation zurueck.
;;;
;;; Revision 1.9  1992/11/11  10:36:34  ft
;;; Fehler in parse-slot-options beseitigt.
;;;
;;; Revision 1.8  1992/10/28  09:55:03  ft
;;; Umbenennungen
;;;
;;; Revision 1.7  1992/10/28  09:50:28  ft
;;; Aufruf von clicc-error korrigiert.
;;;
;;; Revision 1.6  1992/10/16  11:58:07  ft
;;; Korrekturen in p1-defclass und Erweiterungen in finalize-classes.
;;;
;;; Revision 1.5  1992/10/13  09:27:29  ft
;;; multiple-value-bind's so geaendert, dass Warnings entfallen.
;;;
;;; Revision 1.4  1992/10/13  09:11:02  ft
;;; Umstellung auf die Aenderungen im *GLOBAL-ENVIRONMENT*.
;;; finalize-classes hinzugefuegt.
;;;
;;; Revision 1.3  1992/09/01  14:32:57  ft
;;; Datei-Header hinzugefuegt und Kommentare verschoenert
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Die Expansionsfunktion fuer das Compiler-Macro zu make-instance.
;; (make-instance 'constant &rest args) ->
;; (make-instance (find-class 'constant) &rest args)
;; sonst unveraendert.
;;------------------------------------------------------------------------------
(defun p1-make-instance (form)
  (match-args
   form MAKE-INSTANCE (class-specifier . keys)
   (if (quote-form-p class-specifier)
       `(L::make-instance
         (L::find-class ,class-specifier) . ,keys)
       form)))

(defun bind-forward-class (class-name)
  (let ((class (make-instance 'defined-class :symbol class-name)))
    (set-forward-class-entry class-name class)
    (get-class-entry class-name)))

;;------------------------------------------------------------------------------
;; p1-find-class expandiert den Aufruf zu einer Klasse
;;------------------------------------------------------------------------------
(defun p1-find-class (args)
  (let ((class-specifier (first args)))
    (if (quote-form-p class-specifier)
        (let ((class-entry 
               (or (get-class-entry (second class-specifier))
                   (bind-forward-class (second class-specifier)))))
          (if class-entry
              (?class-def (cdr class-entry))
              (clc-error "~A is not a valid argument for FIND-CLASS." 
                         class-specifier)))
        (clc-error "~A is not a valid argument for FIND-CLASS." 
                   class-specifier))))

;;------------------------------------------------------------------------------
;; p1-defclass legt das entsprechende Zwischensprachkonstrukt an
;; die Vererbung wird erst am Ende von Pass 1 durchgefuehrt
;;------------------------------------------------------------------------------
(defun p1-defclass (all-args)
  (block define-class
  
    ;; erste Parameterpruefung
    ;;------------------------
    (unless (and (listp all-args) (> (length all-args) 2)
                 (listp (second all-args)) (listp (third all-args)))
      (clicc-error
       ILLEGAL_CALL "DEFCLASS"
       "CLASS-NAME ({SUPERCLASS-NAME}*) ({SLOT-SPECIFIER}*) [CLASS-OPTION]"))

    (multiple-value-bind
        (class-name 
         superclass-names
         slot-specifier)
        (parse-defclass-args all-args)
    
      (let ((class-entry                ; Klasseninfo aus dem global-env
             (get-class-entry class-name)) 
            class                       ; Instanz von class-def
            superclasses                ; Superklassen-Referenzen
            slot-descriptions           ; Liste von slot-desc Instanzen
            (*CURRENT-FORM* class-name))
      
        ;; gebe Meldung aus
        ;;-----------------
        (clicc-message "Analyse DEFCLASS     ~A" class-name)

        ;; Pruefen ob die Klasse schon definiert/referenziert  wurde
        ;; sonst neue Instanz von defined-class erzeugen
        ;;----------------------------------------------
        (case (car class-entry)
          ((:TYPE :STRUCT)              ; Name schon vergeben
           (clc-error REDEF_CLASS class-name)
           (return-from define-class))
          (:CLASS                       ; Klasse schon referenziert/definiert
           (cond ((?forward (cdr class-entry))
                  (setf (?forward (cdr class-entry)) nil
                        class (?class-def (cdr class-entry))))
                 (T (clc-error REDEF_CLASS class-name)
                    (return-from define-class))))
          (otherwise                    ; kein Eintrag im global-env!
           (setf class (make-instance 'defined-class :symbol class-name))
           (set-unfinalized-class-entry class-name class nil nil)
           (setf class-entry (get-class-entry class-name))))

        ;; Parameteranalyse z.B Ermittlung der Metaklasse
        ;;-----------------------------------------------
        (setf superclass-names (remove 'T superclass-names))
        (let ((red-superclass-names (remove-duplicates superclass-names)))
          (when (< (length red-superclass-names)
                   (length superclass-names))
            (clc-error "Some classes mentioned multiple times in ~
                        superclass-list. They will be ignored.")
            (setf superclass-names red-superclass-names)))
        (setf superclasses              ; suche def. Klassen/erzeuge Vorw.-Ref.
          (mapcar #'(lambda (superclass-name)
                      (let ((superclass-def (get-class-entry superclass-name))
                            superclass)
                        (ecase (car superclass-def)
                          ((:TYPE :STRUCT :BUILT-IN)
                           (clc-error 
                            "~S is not a legal superclass, using T instead" 
                            superclass-name)
                           (setf superclass 
                             (?class-def (cdr (get-class-entry 'T)))))
                          (:CLASS
                           (setf superclass 
                             (?class-def (cdr superclass-def))))
                          ((nil)
                           (setf superclass
                             (make-instance 'defined-class
                               :symbol superclass-name))
                           (set-forward-class-entry superclass-name
                                                    superclass)))
                        superclass))
                  superclass-names))
       
        ;; hier slot-descriptions aus slot-specifier erzeugen
        ;; spielen hier auch die default-initargs rein ???
        ;;------------------------------------------------
        (setf slot-descriptions (generate-slot-descs slot-specifier))
        (maplist #'(lambda (list) 
                     (when (member 
                            (?symbol (first list)) (rest list) :key #'?symbol)
                       (clc-error 
                        "The slot ~S has been declared twice in class ~S."
                        (?symbol (first list)) class-name)))
                (mapcar #'car slot-descriptions))

        ;; trage die Zugriffsmethoden im class-entry ein
        ;;----------------------------------------------
        (setf (?reader (cdr class-entry))
          (mapcar #'(lambda (slot-description)
                      (cons (?symbol (first slot-description)) 
                            (second slot-description)))
                  slot-descriptions))
        (setf (?writer (cdr class-entry))
          (mapcar #'(lambda (slot-description)
                      (cons (?symbol (first slot-description))
                            (third slot-description)))
                  slot-descriptions))

        ;; fuelle Slots der Instanz
        ;;-------------------------
        (setf (?super-list class)      superclasses
              (?slot-descr-list class) (mapcar #'car slot-descriptions))
      
        ;; Rueckgabewert=Instanz...kein einheitlicher Rueckgabewert fuer def... 
        ;;---------------------------------------------------------------------
        class
        ))))

;;------------------------------------------------------------------------------
;; parse-defclass-args: Parser Funktion fuer die Parameter des macros defclass
;;------------------------------------------------------------------------------

(defun parse-defclass-args (all-args)
  (let ((class-name                     ; Name der Klasse als Symbol
         (first all-args))
        (defined-superclasses           ; Liste definierter Superklassen
            (second all-args))
        ;; Die folgenden drei koennen auch nil enthalten
        (superclasses nil)              ; Liste aller Superklassen
        (slot-specifier                 ; definierte Slot Spezifizierer
         (third all-args))                              
        (default-initargs nil))
    (setf superclasses defined-superclasses) ; vorlaeufig !!!
    (do ((options (cadddr all-args) (cddr options)))
        ((null options))
      (case (first options)
        (:default-initargs
            (setf default-initargs (cadr options)))
        (:documentation
         nil)                           ; kann ignoriert werden
        (:metaclass
         (clc-error "The class-option ~A is not a part of the ~
                         implemented language ~%and  will be ignored."
                    (first options)))                          
        (otherwise
         (clc-error "The ~A is not a valid class-option and will be ignored."
                    (first options)))))
    (values
     class-name
     superclasses
     slot-specifier
     default-initargs)))

;;------------------------------------------------------------------------------
;; Funktionen zum Erzeugen von Instanzen von slot-desc aus einer
;; slot-specifier Liste
;;------------------------------------------------------------------------------
(defun generate-slot-descs (slot-specifier-list)
  (mapcar #'generate-slot-desc slot-specifier-list))

(defun generate-slot-desc (slot-specifier)
  (labels ((s-fun-p (f) (and (consp f) (eq (first f) 'function))))
    (multiple-value-bind 
          (symbol allocation initargs initform type reader writer)
                                        ;other-options!
        (parse-slot-specifier (if (consp slot-specifier) 
                                  slot-specifier 
                                  (list slot-specifier)))
      (multiple-value-bind
            (evaluated-initform evaluated)
          (cond
            ((eq initform :unbound) (values initform T))
            ((s-fun-p initform)                        (values initform nil))
            (T                                         (p1-eval initform)))
        (let* ((*SDF* T)
               (initfunction-name (intern (concatenate 'string 
                                                       (string symbol)
                                                       "-initfunction" 
                                                       (string (gensym)))))
               (processed-initform
                (cond
                  ((eq evaluated-initform :unbound)
                   evaluated-initform)
                  (evaluated 
                   (p1-form
                    `(L::QUOTE ,evaluated-initform)))
                  (T
                   (progn 
                     (p1-defun `(,initfunction-name () ,initform))
                     (p1-form  `#',initfunction-name)))))
               (slot-description (make-instance 'slot-desc
                                                :symbol symbol
                                                :initform processed-initform
                                                :initargs 
                                                (mapcar #'p1-make-symbol
                                                        initargs)
                                                :allocation allocation
                                                :declared-type type)))
          (list slot-description reader writer))))))

;;------------------------------------------------------------------------------
;; parse-slot-specifier: Parser fuer den Slot-Specifier eines DEFCLASS
;;------------------------------------------------------------------------------
(defun parse-slot-specifier (slot-specifier)
  (let ((symbol (car slot-specifier))
        (type T)
        (initform :unbound)
        (allocation ':instance)
        ;; fuer ^^^ koennte man noch einen Fehler bei mehrfacher Angabe ausgeben
        (reader (empty-queue))
        (writer (empty-queue))
        (initargs (empty-queue))
        (other-options (empty-queue))
        (last-key nil))
    (do ((options (cdr slot-specifier) (cddr options)))
        ((null options))
      (case (car options)
        (:reader
         (add-q (cadr options) reader)
         (setf last-key ':reader))
        (:writer
         (add-q (cadr options) writer)
         (setf last-key ':writer)) 
        (:accessor
         (add-q (cadr options) reader)
         (add-q (list 'L::setf (cadr options)) writer)
         (setf last-key ':accessor))
        (:allocation
         (setf allocation (cadr options))
         (setf last-key nil))
        (:initarg
         (add-q (cadr options) initargs)
         (setf last-key ':initarg))
        (:initform
         (setf initform (cadr options))
         (setf last-key nil))
        (:type
         (setf type (cadr options))
         (setf last-key nil))
        (otherwise
         (case last-key
           (:reader
            (add-q (cadr options) reader))
           (:writer
            (add-q (cadr options) writer))
           (:accessor
            (add-q (cadr options) reader)
            (add-q (list 'L::setf (cadr options)) writer))
           (:initarg
            (add-q (cadr options) initargs))
           (otherwise
            (add-q (car options) other-options)
            (add-q (cadr options) other-options))))))
    (when (> (length (queue2list initargs)) 1)
      (clc-error "There is more than one initarg specified for slot ~A, ~%~
                 continue to ignore all but the first." symbol))
    (values
     symbol 
     allocation 
     (queue2list initargs) 
     initform 
     type
     (queue2list reader) 
     (queue2list writer) 
     (queue2list other-options))))

;;------------------------------------------------------------------------------
;; finalize-class(es): Berechnung d. class-precedence-lists, Vererbung
;; Der Aufruf muss am Anfang von p1-end stattfinden, da indirekt p1-defun
;; aufgerufen wird.
;;------------------------------------------------------------------------------

(defun finalize-classes ()
  (let ((all-Rc '())                    ; fuer die lokalen Ordnungen Rc
        (class-message-flag nil)
        (built-in-message-flag nil))
    (labels

        ;;----------------------------------------------------------------------
        ;; finalize-class: Finalisiert eine Klasse 
        ;;----------------------------------------------------------------------
        ((finalize-class (class)

           ;; gebe eine Meldung aus (einmal)
           ;;-------------------------------
           (unless class-message-flag
             (setf class-message-flag T)
             (clicc-message "Finalizing classes ..."))
                     
           (catch 'CLICC-ERROR
             (let* ((class-name (?symbol class))
                    (class-entry (cdr (get-class-entry class-name)))
                    (*CURRENT-FORM* class-name))    
             
               ;; pruefe ob diese Klasse nicht schon finalized ist
               ;;-------------------------------------------------
               (if (?forward class-entry)
                   (progn
                     (clc-error "Class ~S is referenced, but not defined." 
                                class-name)

                     ;; falls obiger Fehler fortgesetzt wird muessen Defaults
                     ;; fuer die Klasse angenommen werden
                     ;;----------------------------------
                     (setf (?forward class-entry) nil)
                     (setf (?finalized class-entry) nil)
                     (setf (?class-precedence-list (?class-def class-entry))
                           (make-instance 'structured-literal
                                          :value
                                          (list (?class-def class-entry))
                                          :needs-fixnum-array nil
                                          :needs-float-array nil))
                     (setf (?slot-descr-list (?class-def class-entry)) nil)
                     (add-q (?class-def class-entry)
                            (?class-def-list *module*)))
                   
                   (unless (?finalized class-entry)

                     ;; finalisiere die Oberklassen
                     ;;----------------------------
                     (dolist (superclass (?super-list class))
                       (finalize-class superclass))
               
                     ;; Berechnung der class-precedence-list
                     ;;-------------------------------------
                     (setf (?class-precedence-list class)
                           (compute-c-p-l class))
                               
                     ;; vererbe die Slots aus den Oberklassen
                     ;;--------------------------------------
                     (dolist (superclass 
                               (butlast (cdr (?class-precedence-list class))))
                       (dolist (slot-desc (reverse
                                           (?slot-descr-list superclass)))
                         (unless (member (?symbol slot-desc) 
                                         (mapcar #'?symbol 
                                                 (?slot-descr-list class)))
                           (let ((superclass-entry 
                                  (cdr (get-class-entry 
                                        (?symbol superclass))))
                                 (new-slot (copy-slot-desc slot-desc)))
                              (setf (?inherited-from new-slot)
                                    (?class-def superclass-entry))
                              (setf (?slot-descr-list class)
                                    (cons new-slot (?slot-descr-list class)))
                              (push 
                               (cons (?symbol slot-desc) 
                                     (copy-list 
                                      (cdr (assoc (?symbol slot-desc) 
                                                  (?reader 
                                                   superclass-entry)
                                                  :test #'eq))))
                               (?reader class-entry))
                              (push 
                               (cons (?symbol slot-desc) 
                                     (copy-list 
                                      (cdr (assoc (?symbol slot-desc) 
                                                  (?writer 
                                                   superclass-entry)
                                                  :test #'eq))))
                               (?writer class-entry))))))
                     
                     ;; Vergabe der offsets fuer die Slots
                     ;;-----------------------------------
                     (let ((offset 0))
                       (dolist (slot-desc (?slot-descr-list class))
                         (setf (?offset slot-desc) offset)
                         (setf offset (1+ offset))))
               
                     ;; ueberfuehre die Komponenten in Zwischensprache
                     ;; dabei kann es nicht geschehen, dass der Aufruf
                     ;; BUTLAST NIL liefert, da das Resultat mind. eine
                     ;; Liste mit der Klasse selbst ist
                     ;;-----------------------------------------------
                     (setf (?class-precedence-list class)
                           (make-instance 'structured-literal
                                          :value 
                                          (butlast 
                                           (?class-precedence-list class))
                                          :needs-fixnum-array nil
                                          :needs-float-array nil))

                     ;; markiere die Klasse als finalized
                     ;;----------------------------------
                     (setf (?finalized class-entry) T)
               
                     ;; Trage die Klasse in class-def-list des akt. Moduls ein
                     ;;-------------------------------------------------------
                     (add-q class (?class-def-list *module*))))

               class)))

         ;;---------------------------------------------------------------------
         ;;finalize-built-in: Finalisiert eine 'built-in'-Klasse 
         ;;---------------------------------------------------------------------
         (finalize-built-in (class)
           (let ((*CURRENT-FORM* (?symbol class)))
             
          
             ;; pruefe ob diese Klasse nicht schon finalized ist
             ;;-------------------------------------------------
             (unless (?class-precedence-list class)

               ;; gebe eine Meldung aus (einmal)
               ;;-------------------------------
               (unless built-in-message-flag
                 (setf built-in-message-flag T)
                 (clicc-message "Finalizing built-in-classes ..."))
               
               ;; finalize die Oberklassen
               ;;-------------------------
               (dolist (superclass (?super-list class))
                 (finalize-built-in superclass))
               
               ;; Berechnung der class-precedence-list
               ;;-------------------------------------
               (setf (?class-precedence-list class)
                     (compute-c-p-l class)))))

         ;;---------------------------------------------------------------------
         ;; optimize-slot-positions: Optimiere die Positionen von Slots, so dass
         ;;                          weniger Zugriffsfunkt. benoetigt werden
         ;;---------------------------------------------------------------------
         (optimize-slot-positions ()
           (let ((slot-counts ()))

             ;; Zaehle die Vorkommen von Slot-Bezeichnern
             ;;------------------------------------------
             (dolist (class (queue2list (?class-def-list *module*)))
               (dolist (slot (?slot-descr-list class))
                 (let ((slot-count (assoc (?symbol slot) slot-counts
                                          :test #'eq)))
                   (if slot-count (rplacd slot-count
                                          (cons (1+ (cadr slot-count))
                                                (cons class (cddr slot-count))))
                       (setf slot-counts (cons (cons (?symbol slot)
                                                     (cons 1 (list class)))
                                               slot-counts))))))
             (setf slot-counts (sort slot-counts #'> :key #'cadr))
             (when slot-counts
               (clicc-message "Optimizing slot positions ... ")

               ;; sortiere die Klassen
               ;;---------------------
               (dolist (slot-count slot-counts)
                 (setf (cddr slot-count) (reverse
                                          (sort (cddr slot-count)
                                                #'preceding-class-p))))

               ;; optimiere Positionierung der Slots
               ;;-----------------------------------
               (dolist (slot-count slot-counts)
                 (let* ((slot-name (first slot-count))
                        (new-pos (?offset
                                  (find slot-name (?slot-descr-list
                                                   (first (cddr slot-count)))
                                        :key #'?symbol))))
                   (dolist (class (cddr slot-count))
                     (let ((orig-slot (find slot-name (?slot-descr-list class)
                                            :key #'?symbol))
                           (dest-slot (find new-pos (?slot-descr-list class)
                                            :key #'?offset)))
                       (if (or (null dest-slot) (?moved dest-slot)
                               (eq (?offset orig-slot) (?offset dest-slot)))
                           (setf new-pos (?offset orig-slot))
                           (progn
                             (setf (?offset dest-slot) (?offset orig-slot))
                             (setf (?offset orig-slot) new-pos)
                             (setf (?moved orig-slot) T)))))))

               ;; sortiere die Slots entsrechend ihrem offset
               ;;--------------------------------------------
               (dolist (class (queue2list (?class-def-list *module*)))
                 (when class
                   (setf (?slot-descr-list class)
                         (sort (?slot-descr-list class) #'<
                               :key #'?offset)))))))

         ;;---------------------------------------------------------------------
         ;; generate-accessor-methods: Erzeuge die Slot-Zugriffs-Methoden
         ;;                            Diese müssen auch Typtests enthalten
         ;;                            (mittels THE) damit die primitiven
         ;;                            Zugriffsf. darauf verzichten können.
         ;;---------------------------------------------------------------------
         (generate-accessor-methods ()
           (when (queue2list (?class-def-list *module*))
             (clicc-message "Generating slot accessors ..."))
           (dolist (class (queue2list (?class-def-list *module*)))

             ;; erzeuge die Slot-Zugriffsmethoden
             ;;----------------------------------
             (let* ((*SDF* T)           ; System-Defined-Function
                    (class-name (?symbol class))
                    (class-entry (cdr (get-class-entry class-name))))
               (dolist (slot-desc (?slot-descr-list class))
                 (let* ((type (?declared-type slot-desc))
                        (offset (?offset slot-desc))
                        (dont-generate-flag (if (?inherited-from slot-desc)
                                                (eq offset
                                                    (?offset
                                                     (find (?symbol slot-desc)
                                                           (?slot-descr-list
                                                            (?inherited-from
                                                             slot-desc))
                                                           :key #'?symbol)))
                                                nil)))
                   (unless dont-generate-flag
                     (dolist (reader-name (cdr (assoc (?symbol slot-desc)
                                                      (?reader class-entry)
                                                      :test #'eq)))
                       (when reader-name
                         (p1-defmethod 
                          `(,reader-name ((instance ,class-name))
                            ,(if (eq type T)
                                 `(rt::instance-ref instance ,offset)
                                 `(L::the ,type 
                                   (rt::instance-ref instance ,offset))))
                          :dont-generate-local-funs T)))
                     (dolist (writer-name 
                               (cdr (assoc (?symbol slot-desc)
                                           (?writer class-entry)
                                           :test #'eq)))
                       (when writer-name
                         (p1-defmethod 
                          `(,writer-name (new-value (instance ,class-name))
                            ,(if (eq type T)
                                 `(rt::instance-set new-value instance ,offset)
                                 `(rt::instance-set 
                                   (L::the ,type new-value) instance ,offset)))
                          :dont-generate-local-funs T)))))))))
           
         ;;---------------------------------------------------------------------
         ;; compute-c-p-l: Berechnung der class-precedence-list 
         ;;---------------------------------------------------------------------
         (compute-c-p-l (class)
           (compute-local-precedence (cons class (?super-list class)))
           (let* ((Sc                   ;erweiterte Superklassenliste
                   (cons class
                         (remove-duplicates (collect-superclasses class))))
                  (R                    ;Ordnung auf class
                   (mapappend #'(lambda (a-class)
                                  (cdr (assoc (?symbol a-class) 
                                              all-Rc
                                              :test #'eq)))
                                         Sc)))
             (append (topological-sort Sc R class)
                     (list (?class-def (rest (get-class-entry `T)))))))

         ;;---------------------------------------------------------------------
         ;; compute-local-precedence: vgl. CLtL p. 782
         ;;---------------------------------------------------------------------
         (compute-local-precedence (cl)
           (let ((lc (mapcar #'list (butlast cl) (rest cl))))
             (push (cons (?symbol (first cl)) lc) all-Rc)
             lc))
         
         ;;---------------------------------------------------------------------
         ;; collect-superclasses: Berechne die Menge aller Superklassen
         ;;---------------------------------------------------------------------
         (collect-superclasses (class)
           (let ((result (?super-list class))
                 (others (mapcar #'collect-superclasses (?super-list class))))
             (mapcar #'(lambda (other)
                         (setf result (append result other)))
                     others)
             result))

         ;;---------------------------------------------------------------------
         ;; topological-sort: vgl. CLtL p. 784
         ;;---------------------------------------------------------------------
         (topological-sort (Sc R class)
           (block topological-sort
             (let ((result '()))
               (loop
                (let ((choices (remove-if
                                #'(lambda (a-class)
                                    (member a-class R :key #'cadr))
                                Sc))
                      choice)
                  (when (null choices) 
                    (if (null Sc)
                        (return-from topological-sort result)
                        (clicc-error 
                         "Inconsistent precedence graph for class ~S.~% ~
                          Unable to find positions for ~S"
                         (?symbol class) (mapcar #'?symbol Sc))))
                  (setf choice (if (null (cdr choices))
                                   (car choices)
                                   (select-other-choice choices result class)))
                  (setf result (append result (list choice)))
                  (setf Sc (remove choice Sc))
                  (setf R (remove choice R :test #'member)))))))
         
         ;;---------------------------------------------------------------------
         ;; select-other-choice: vgl. CLtL p. 784
         ;;---------------------------------------------------------------------
         (select-other-choice (choices result class)
           (block select-other-choice
             (dolist (a-result (reverse result))
               (dolist (possible-choice (?super-list a-result))
                 (when (member possible-choice choices) 
                   (return-from select-other-choice possible-choice))))
             (clicc-error "Inconsistent precedence graph for class ~S.~% ~
                           Unable to choose one of ~S" 
                          (?symbol class) (mapcar #'?symbol choices))))
         
         ;;---------------------------------------------------------------------
         ;; copy-slot-desc: Erzeugen einer Kopie von einer Instanz der Klasse
         ;;                 slot-desc
         ;;---------------------------------------------------------------------
         (copy-slot-desc (old-slot-desc)
           (make-instance 'slot-desc 
                          :symbol (?symbol old-slot-desc)
                          :initform (?initform old-slot-desc)
                          :initargs (?initargs old-slot-desc)
                          :allocation (?allocation old-slot-desc)
                          :offset (?offset old-slot-desc)
                          :declared-type (?declared-type old-slot-desc))))
         
      ;; Rumpf von finalize-classes
      ;;---------------------------
      (dolist (a-type (?types *GLOBAL-ENVIRONMENT*))
        (case (cadr a-type)
          (:CLASS
           (finalize-class (?class-def (cddr a-type))))
          (:BUILT-IN
           (finalize-built-in (cddr a-type)))))
      (when *OPTIMIZE* (optimize-slot-positions))
      (let* ((cgol (queue2list (?class-def-list *module*)))
             (order (+ (length cgol) 1)))
        (setf cgol (class-precedence-sort cgol))
        (dolist (class cgol)
          (decf order)
          (setf (?order class) order))
        (setf (?class-def-list *module*) (list2queue cgol)))
      (generate-accessor-methods))))

;;------------------------------------------------------------------------------
;; symbolize-class-names: wandelt die Klassen- und Slotsymbole in sym's um
;;------------------------------------------------------------------------------
(defun symbolize-class-names ()
  (dolist (a-class (queue2list (?class-def-list *module*)))
    (unless (sym-p (?symbol a-class))
      (setf (?symbol a-class) (p1-constant (?symbol a-class))))
    (dolist (a-slot (?slot-descr-list a-class)) 
      (setf (?symbol a-slot) (p1-constant (?symbol a-slot))))))

;;------------------------------------------------------------------------------
;; export-classes: Setzt das exported-flag der Klassen und veranlasst ggf. das
;;                 schattierte exportieren von Oberklassen
;;------------------------------------------------------------------------------
(defun export-classes ()
  (dolist (class (?class-def-list *module*))
    (unless (?exported class)
      (let ((symbol (?symbol (?symbol class))))
        (when (consp symbol) (setq symbol (second symbol)))
        (when (symbol-package symbol)
          (multiple-value-bind (s status)
              (find-symbol (symbol-name symbol) (symbol-package symbol))
            (declare (ignore s))
            (when (eq :external status)
              (dolist (superclass
                        (rest (?value (?class-precedence-list class))))
                (unless (?exported superclass)
                  (setf (?exported superclass) t)
                  (unless (member :full-specializable
                                  (?export-goals superclass))
                    (push :shadow-specializable
                          (?export-goals superclass)))))
              (setf (?exported class) t)
              (unless (?export-goals class)
                (setf (?export-goals class)
                      (list :full-subclassable
                            :full-instanceable
                            :full-specializable))))))))))

;;------------------------------------------------------------------------------
;; class-precedence-sort: sortiert eine Liste von Klassen gemaess der durch
;;                        die c-p-l's vorgegeben Reihenfolgen
;;------------------------------------------------------------------------------
(defun class-precedence-sort (class-list)
  (let ((todo class-list)
        (work nil)
        (result nil))
    (loop
     (when (null todo) (return-from class-precedence-sort result))
     (setf work todo)
     (setf todo nil)
     (dolist (class work)
       (if (null (set-difference (cdr (?value (?class-precedence-list class)))
                                 result))
           (push class result)
           (push class todo))))))
     
;;------------------------------------------------------------------------------
(provide "p1class")




