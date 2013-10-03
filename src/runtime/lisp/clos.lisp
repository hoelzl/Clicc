;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Laufzeitfunktionen des Objektsystems
;;;
;;; $Revision: 1.34 $
;;; $Log: clos.lisp,v $
;;; Revision 1.34  1994/01/21  13:25:26  sma
;;; rt::set-slot-unbound gelöscht und Lisp-Code ersetzt.
;;;
;;; Revision 1.33  1994/01/13  12:20:04  ft
;;; rt:typep-class optimiert, indem statt T der erste Parameter
;;; zurueckgeliefert wird.
;;;
;;; Revision 1.32  1994/01/05  12:37:36  sma
;;; Namensänderung: rt::make-instance-internal -> rt::make-instance
;;;
;;; Revision 1.31  1993/09/27  12:14:54  hk
;;; Argumentreihenfolge beim Aufruf von rt::set-slot-unbound korrigiert.
;;;
;;; Revision 1.30  1993/09/06  06:24:24  ft
;;; Tippfehler entfernt: set-direct-class-of.
;;;
;;; Revision 1.29  1993/09/02  13:37:59  ft
;;; Erweiterung um CLASS-OF.
;;;
;;; Revision 1.28  1993/08/30  07:46:12  ft
;;; REINITIALIZE-INSTANCE hinzugefuegt und MAKE-INSTANCE an das
;;; veraenderte rt::make-instance-internal angepasst.
;;;
;;; Revision 1.27  1993/07/14  08:51:54  ft
;;; Anpassung an die geänderten Parameter von instance-ref/set.
;;;
;;; Revision 1.26  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.25  1993/06/11  09:01:02  ft
;;; Mal wieder 'ne Klammer vergessen.
;;;
;;; Revision 1.24  1993/06/10  14:15:05  ft
;;; Fehlermeldung verfeinert und Erläuterungen eingefügt.
;;;
;;; Revision 1.23  1993/06/09  16:26:21  hk
;;; Fehler in make-instance provisorisch behoben.
;;;
;;; Revision 1.22  1993/05/22  10:33:11  ft
;;; Erweiterung um die Auswertung von Slot-Init-Funktionen.
;;;
;;; Revision 1.21  1993/05/11  08:38:37  ft
;;; Erweiterung um (setf slot-value).
;;;
;;; Revision 1.20  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.19  1993/04/21  14:12:56  ft
;;; Spezifischere Fehlermeldung fuer make-instance mit einem Symbol als
;;; Klassen-Parameter.
;;;
;;; Revision 1.18  1993/04/15  13:00:07  ft
;;; Neue Funktionen fuer die Handhabung von Slots.
;;;
;;; Revision 1.17  1993/04/02  11:21:48  ft
;;; simple-member optimiert.
;;;
;;; Revision 1.16  1993/04/02  06:31:40  ft
;;; type-class verwendet jetzt ein einfaches member um schneller zu sein.
;;;
;;; Revision 1.15  1993/04/01  08:23:50  ft
;;; Macros zur besseren Lesbarkeit eingefuehrt.
;;;
;;; Revision 1.14  1993/03/30  12:37:33  ft
;;; slot-boundp an die geaenderte Darst. des Zustands 'unbound' angepasst.
;;;
;;; Revision 1.13  1993/03/25  10:17:58  ft
;;; Test des ersten Parameters von make-instance, korrigierter Zugriff
;;; auf Klassen.
;;;
;;; Revision 1.12  1993/03/23  14:38:18  ft
;;; Make-instance prueft jetzt ob der erste Parameter eine Klasse ist.
;;;
;;; Revision 1.11  1993/03/23  07:40:39  ft
;;; make-instance-using-class in make-instance umbenannt, damit die
;;; Anwendungen als funktionales Object richtig gebunden werden.
;;;
;;; Revision 1.10  1993/03/15  15:15:41  ft
;;; Fehler beim Zugriff auf Klassen beseitigt.
;;;
;;; Revision 1.9  1993/03/12  09:58:57  ft
;;; Indirektion beim Zugriff auf Instanzen beseitigt; unbenoetigte Funktionen
;;; geloescht; Instanziierung grundlegend geaendert.
;;;
;;; Revision 1.8  1993/02/26  14:57:44  ft
;;; make-instance und typep-class optimiert.
;;;
;;; Revision 1.7  1993/02/26  10:09:32  ft
;;; no-applicable-method mit Parametern versehen.
;;;
;;; Revision 1.6  1993/02/23  15:36:47  ft
;;; Verarbeitung des initial-value nil bei make-instance korrigiert.
;;;
;;; Revision 1.5  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.34 $ eingefuegt
;;;
;;; Revision 1.4  1993/01/25  09:59:15  kl
;;; Funktion no-applicable-method, die eine entsprechende Meldung ausgibt
;;; eingefuehrt.
;;;
;;; Revision 1.3  1993/01/21  14:54:18  ft
;;; Erweiterung um no-next-method.
;;;
;;; Revision 1.2  1992/12/18  09:23:36  ft
;;; Erweiterung um SLOT-BOUNDP.
;;;
;;; Revision 1.1  1992/12/11  07:21:22  ft
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(make-instance slot-value slot-boundp slot-makunbound no-next-method
   no-applicable-method slot-missing slot-unbound))
(export '(rt::typep-class) "RT")

;;------------------------------------------------------------------------------
(defmacro direct-class-of (instance-or-class)
  `(rt::instance-ref ,instance-or-class -1))

(defmacro set-direct-class-of (instance new-value)
    `(rt::instance-set ,new-value ,instance -1))

(defmacro name-of (class)
    `(rt::instance-ref ,class 0))

(defmacro class-precedence-list (class)
    `(rt::instance-ref ,class 1))

(defmacro num-o-slots (class)
    `(rt::instance-ref ,class 2))

(defmacro slot-infos (class)
    `(rt::instance-ref ,class 3))

(defmacro slot-key (slot-info)
    `(first ,slot-info))

(defmacro slot-init (slot-info)
    `(second ,slot-info))

(defmacro slot-name (slot-info)
    `(third ,slot-info))

;;------------------------------------------------------------------------------
;; AUFBAU EINES KLASSENOBJEKTS:
;; 
;; --------------------------
;;|Typ : CL_INSTANCE         |
;;|--------------------------|
;;|-1 : NIL                  | eine Klasse ist also nicht Instanz einer Klasse
;;|--------------------------|
;;| 0 : name-of              |
;;|--------------------------|
;;| 1 : class-precedence-list|
;;|--------------------------|
;;| 2 : num-o-slots          | Anzahl der Slots in Instanzen dieser Klasse
;;|--------------------------|
;;| 3 : slot-infos           | Liste von Slot-Beschreibungen
;; --------------------------
;;
;; 
;; SLOT-BESCHREIBUNGEN:
;; 
;; (key init-val-or-fun name)
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; MAKE-INSTANCE class &REST keys
;;------------------------------------------------------------------------------
(defun make-instance (class &rest keys)
  (unless (and (rt::instancep class) (not (direct-class-of class)))
    (error-in 'make-instance 
              (if (symbolp class)
                  "The computed symbol ~S is not a valid class-argument, ~
                   these have been restricted to classes."
                  "~S is not a valid class-argument.")
              class))
  (let ((instance (rt::make-instance
                   (+ (num-o-slots class) 1)))
        (slot-pos 0)) 
    (set-direct-class-of instance class)
    (dolist (slot-info (slot-infos class))
      (let ((init-val-or-fun
             (getf keys (slot-key slot-info) (slot-init slot-info))))
        (if (and (functionp init-val-or-fun)
                 (eq init-val-or-fun (slot-init slot-info)))
            (rt::instance-set (funcall init-val-or-fun) instance slot-pos)
            (rt::instance-set init-val-or-fun instance slot-pos)))
      (incf slot-pos))
    instance))

;;------------------------------------------------------------------------------
;; REINITIALIZE-INSTANCE instance slot-names &REST initargs
;;------------------------------------------------------------------------------
(defun reinitialize-instance (instance slot-names &rest initargs)
  (unless (or (not (rt::instancep instance)) (direct-class-of instance))
    (error-in 'reinitialize-instance "~S is not an instance." instance))
  (let ((slot-pos 0))
    (dolist (slot-info (slot-infos instance))
      (let ((init-val-or-fun
             (getf initargs (slot-key slot-info) (slot-init slot-info))))
        (if (and (eq init-val-or-fun (slot-init slot-info)))
            (when (or (eq T slot-names)
                      (member (slot-name slot-info) initargs))
              (if (and (functionp init-val-or-fun)
                       (eq init-val-or-fun (slot-init slot-info)))
                  (rt::instance-set (funcall init-val-or-fun) instance slot-pos)
                  (rt::instance-set init-val-or-fun instance slot-pos)))
            (if (and (functionp init-val-or-fun)
                     (eq init-val-or-fun (slot-init slot-info)))
                (rt::instance-set (funcall init-val-or-fun) instance slot-pos)
                (rt::instance-set init-val-or-fun instance slot-pos))))
      (incf slot-pos))))

;;------------------------------------------------------------------------------
;; TYPEP-CLASS object class
;;------------------------------------------------------------------------------
(defun rt:typep-class (object class)
  (if (rt::instancep object)
      (if (eq class (direct-class-of object))
          object
          (simple-member class
                         (class-precedence-list (direct-class-of object))))
      nil))

(defun simple-member (item list)
  (cond
    ((null list)
     nil)
    ((eq item (car list))
     item)
    (T
     (simple-member item (cdr list)))))

;;------------------------------------------------------------------------------
;; SLOT-VALUE object slot-name
;;------------------------------------------------------------------------------
(defun slot-value (object slot-name)
  (let* 
      ((slot-infos (slot-infos (class-of object)))
       (slot-pos   (position slot-name slot-infos :key #'third)))
    (if slot-pos
        (if (rt::unbound-value-p (rt::instance-ref object slot-pos))
            (slot-unbound (class-of object) object slot-name)
            (rt::instance-ref object slot-pos))
        (slot-missing (class-of object) object slot-name 'SLOT-VALUE))))

;;------------------------------------------------------------------------------
;; (SETF SLOT-VALUE) object slot-name
;;------------------------------------------------------------------------------
(defun (setf slot-value) (new-value object slot-name)
  (let* 
      ((slot-infos (slot-infos (class-of object)))
       (slot-pos   (position slot-name slot-infos :key #'third)))
    (if slot-pos
        (rt::instance-set new-value object slot-pos))
        (slot-missing (class-of object) object slot-name '(SETF SLOT-VALUE))))

;;------------------------------------------------------------------------------
;; SLOT-BOUNDP instance slot-name
;;------------------------------------------------------------------------------
(defun slot-boundp (instance slot-name)
  (let* 
      ((slot-infos (slot-infos (class-of instance)))
       (slot-pos   (position slot-name slot-infos :key #'third)))
    (if slot-pos
        (not (rt::unbound-value-p (rt::instance-ref instance slot-pos)))
        (slot-missing (class-of instance) instance slot-name 'SLOT-BOUNDP))))

;;------------------------------------------------------------------------------
;; SLOT-MAKUNBOUND instance slot-name
;;------------------------------------------------------------------------------
(defun slot-makunbound (instance slot-name)
  (let* 
      ((slot-infos (slot-infos (class-of instance)))
       (slot-pos   (position slot-name slot-infos :key #'third)))
    (if slot-pos
        (rt::instance-set (rt::unbound-value) instance slot-pos)
        (slot-missing (class-of instance) instance 
                      slot-name 'SLOT-MAKUNBOUND)))
  instance)

;;------------------------------------------------------------------------------
;; CLASS-OF instance
;;------------------------------------------------------------------------------
(defun class-of (instance)
  (if (rt::instancep instance)
      (direct-class-of instance)
      (error "~S ist not a valid argument for CLASS-OF, ~
              these have been restricted to instances of user-defined-classes."
             instance)))

;;------------------------------------------------------------------------------
;; Funktionen zur Fehlerbehandlung der von CLOS erzeugten Laufzeitfehler:
;; NO-NEXT-METHOD
;; NO-APPLICABLE-METHOD gf-name &REST gf-args
;; SLOT-MISSING class object slot-name operation
;; SLOT-UNBOUND class instance slot-name
;;------------------------------------------------------------------------------
(defun no-next-method ()
  (error "No next method."))

(defun no-applicable-method (gf-name &rest gf-args)
  (error "No applicable method for generic function ~s with args ~s." 
         gf-name gf-args))

(defun slot-missing (class object slot-name operation)
  (error "~S: The slot ~s is missing from the object ~s of class ~s."
         operation slot-name object class))

(defun slot-unbound (class instance slot-name)
  (error "The slot ~s is unbound in the object ~s of class ~s."
         slot-name instance class))

;;------------------------------------------------------------------------------
