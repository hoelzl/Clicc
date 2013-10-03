;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Methoden fuer Print-Object, spezialisiert fuer die Lisp nahe
;;;            Zwischensprache
;;;
;;; $Revision: 1.26 $
;;; $Log: printzs.lisp,v $
;;; Revision 1.26  1994/03/03  13:52:36  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.25  1994/02/21  09:55:07  ft
;;; Anpassung von PO-STANDARD an die Änderungen an Klassen und ge.
;;; Funktionen.
;;;
;;; Revision 1.24  1994/01/05  10:13:02  kl
;;; Funktionen types-on und types-off aus timain.lisp nach hier verlegt.
;;;
;;; Revision 1.23  1993/12/22  09:21:00  hk
;;; Verwendung des xp Package gestrichen, um Probleme mit vordefinieren
;;; Pretty-Printern zu vermeiden.
;;;
;;; Revision 1.22  1993/11/08  11:17:31  hk
;;; Das package von class-direct-superclasses wird nur noch anhand des
;;; Features PCL bzw. CLOS bestimmt.
;;;
;;; Revision 1.21  1993/08/31  09:33:44  uho
;;; Aenderungen fuer die 22Aug93 Version von CLISP eingebaut.
;;;
;;; Revision 1.20  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.19  1993/05/28  11:47:20  ft
;;; Anpassung an die Aenderungen in der Zwischensprachdefinition.
;;;
;;; Revision 1.18  1993/05/19  14:42:48  uho
;;; Aenderungen fuer CLISP eingebaut.
;;;
;;; Revision 1.17  1993/02/16  16:14:19  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.16  1992/11/22  16:35:09  kl
;;; Aufbau und Ausgabe fuer die verschiedenen Implementationen vereinheitlicht.
;;; Namenskonflikte mit dem XP-Package beseitigt.
;;;
;;; Revision 1.15  1992/11/05  10:43:33  kl
;;; Aufbau und Ausgabe verbessert. Pretty-printer fuer Lucid eingebunden.
;;;
;;; Revision 1.14  1992/10/01  17:47:19  kl
;;; PO-Standard an den geaenderten Klassenbaum angepasst.
;;;
;;; Revision 1.13  1992/09/28  12:15:41  hk
;;; Die tagbody-form in tagged-form wird nicht ausgegeben, um Rekursion
;;; zu vermeiden.
;;;
;;; Revision 1.12  1992/09/25  17:45:38  kl
;;; Umstellung auf die neue Repraesentation der einfachen Literale.
;;;
;;; Revision 1.11  1992/08/11  10:37:14  ft
;;; Die Belegung fuer PO-ALL-SLOTS wird jetzt berechnet.
;;;
;;; Revision 1.10  1992/08/05  15:47:51  kl
;;; `union' durch `append' ersetzt, Umbenennungen durchgefuehrt und Fehler 
;;; beseitigt.
;;;
;;; Revision 1.9  1992/08/05  10:38:51  ft
;;; Symbol Annotationen in Standard-Tabelle eingetragen, All-Slots Tabelle 
;;; neu erstellt.
;;;
;;; Revision 1.8  1992/08/05  08:58:50  ft
;;; print-object:zws-object kann jetzt pretty printen.
;;;
;;; Revision 1.7  1992/08/04  15:20:03  ft
;;; Slot constant-value in sym auskommentiert bis zur Beachtung der Rekursion.
;;;
;;; Revision 1.6  1992/08/04  13:12:12  ft
;;; PO_STANDARD erstellt, Vererbung der Eigenschaft "Slot wird gedruckt" 
;;; eingebaut.
;;;
;;; Revision 1.5  1992/08/03  10:54:54  ft
;;; erste lauff"ahige, tabellengesteuerte Version; mit einer Tabelle
;;;
;;; Revision 1.4  1992/07/31  09:09:24  ft
;;; Frank's neuer (zweiter) Ansatz mit nur ein Meth. und vielen Listen
;;;
;;; Revision 1.3  1992/07/30  09:44:30  ft
;;; Frank's erster Ansatz.
;;;
;;; Revision 1.2  1992/07/30  08:38:35  apply
;;; Karstens erste Version.
;;;
;;; Revision 1.1  1992/07/29  08:53:27  apply
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC") 

;;------------------------------------------------------------------------------
;; Symbol fuer ungebundene Slots:
;;------------------------------------------------------------------------------
(defconstant unbound-slot-value '---)


;;------------------------------------------------------------------------------
;; *po-slot-list* enth"alt eine Tabelle f"ur die spezialisierte Print-Object
;; Methode, in der zu jedem Zwischensprachkonstrukt eine Liste von Komponenten
;; abgelegt ist, die gedruckt werden sollen.
;;------------------------------------------------------------------------------
(defvar *po-slot-list* '())


;;------------------------------------------------------------------------------
;; Hilfsfunktion fuer den Umgang mit Klassen.
;;------------------------------------------------------------------------------
(defun direct-superclasses (class)
  #+PCL           (pcl::class-direct-superclasses class)
  #+CLOS          (clos::class-direct-superclasses class)
  #-(or PCL CLOS) (class-direct-superclasses class))

;;------------------------------------------------------------------------------
;; Hilfsfunktion fuer die print-object Methode
;;------------------------------------------------------------------------------
(defun slots-to-print (class-name)
  (if (eq class-name 'zws-object)
      nil
      (append (slots-to-print   
               (class-name (first (direct-superclasses 
                                   (find-class class-name)))))
              (rest (assoc class-name *po-slot-list*)))))


;;------------------------------------------------------------------------------
;; print-object-Methoden fuer die Zwischensprachkonstrukte.
;;------------------------------------------------------------------------------
(defmethod print-object ((an-empty-list null-form) a-stream)
  (princ "[emtpy-list]" a-stream))

;;------------------------------------------------------------------------------
(defmethod print-object ((an-object zws-object) a-stream)
  (let* ((name-of-class 
          (class-name (class-of an-object)))
         (prefix "[")
         (suffix "]")
         (slot-values
          (mapcar #'(lambda (slot) 
                      (if (slot-boundp an-object slot)
                          (slot-value an-object slot)
                          unbound-slot-value))
                  (slots-to-print name-of-class))))
    #+allegro
    (pprint-logical-block (a-stream slot-values :prefix prefix :suffix suffix)
                          (write name-of-class :stream a-stream)
                          (pprint-indent :block 1 a-stream)
                          (loop
                           (pprint-exit-if-list-exhausted)
                           (write-char #\space a-stream)
                           (pprint-newline :fill a-stream)
                           (pprint-fill a-stream (pprint-pop))))


    #-allegro
    (progn
      (format a-stream "~S~S" prefix name-of-class)
      (dolist (slot-value slot-values) 
        (format a-stream " ~s" slot-value))
      (write suffix :stream a-stream))
    
    an-object))


;;------------------------------------------------------------------------------
;; PO-STANDARD Default-Belegung fuer die Variable *po-slot-list*
;;------------------------------------------------------------------------------
(defconstant PO-STANDARD '((module 
                            fun-list
                            class-def-list
                            named-const-list
                            var-list
                            sym-list
                            toplevel-forms)
                           (var)
                           (static symbol)
                           (defined-static)
                           (local-static)
                           (global-static exported)
                           (imported-static exported)
                           (dynamic . (sym))
                           (form)
                           (var-ref . (var))
                           (named-const . (symbol))
                           (defined-named-const . (exported value))
                           (imported-named-const . (value-zs-type))
                           (literal)
                           (sym . (symbol exported))
                           (defined-sym . (name package))
                           (imported-sym)
                           (simple-literal)
                           (character-form .(value))
                           (num . (value))
                           (structured-literal . (value))
                           (literal-instance . (class value-list))
                           (class-def . (symbol 
                                         super-list 
                                         slot-descr-list))
                           (defined-class . (exported))
                           (imported-class)
                           (built-in-class-def . (symbol 
                                                  super-list 
                                                  slot-descr-list
                                                  type-expander))
                           (slot-desc . (symbol initform initargs allocation))
                           (params . (var-list 
                                      opt-list 
                                      rest 
                                      key-list 
                                      allow-other-keys))
                           (opt . (var init suppl))
                           (key . (sym))
                           (fun . (symbol params))
                           (simple-fun . (body))
                           (defined-fun . ())
                           (global-fun . (exported))
                           (local-fun . ())
                           (imported-fun . (exported))
                           (special-sys-fun . ())
                           (generic-fun . (method-list
                                           argument-precedence-order
                                           method-combination))
                           (defined-generic-fun . (exported))
                           (imported-generic-fun . ())
                           (method-def . (fun specializer-list qualifier-list))
                           (app . (form arg-list))
                           (setq-form . (location form))
                           (progn-form . (form-list))
                           (if-form . (pred then else))
                           (switch-form . (form case-list otherwise))
                           (labeled-form . (value form))
                           (let*-form . (var-list init-list body))
                           (labels-form . (fun-list body))
                           (let/cc-form . (cont body))
                           (cont)
                           (tagbody-form . (first-form tagged-form-list))
                           (tagged-form . (form))
                           (mv-lambda . (params body arg))))


;;------------------------------------------------------------------------------
;; Weitere vorgegebene m"ogliche Belegungen f"ur *po-slot-list*
;;------------------------------------------------------------------------------
(setf *po-slot-list* PO-STANDARD)


;;------------------------------------------------------------------------------
;; Funktionen zum Ein- und Ausschalten der Anzeige des Typslots der ZS-Elemente.
;;------------------------------------------------------------------------------
(defmacro find-po-slot-list-element (a-zs-class-name)
  `(rest (assoc ',a-zs-class-name *po-slot-list*)))

(defun types-off ()
  (setf (find-po-slot-list-element form) 
        (remove 'type (find-po-slot-list-element form)))
  (setf (find-po-slot-list-element var) 
        (remove 'type (find-po-slot-list-element var))))

(defun types-on ()
  (push 'type (find-po-slot-list-element form))
  (push 'type (find-po-slot-list-element var)))


;;------------------------------------------------------------------------------
(provide "printzs")
