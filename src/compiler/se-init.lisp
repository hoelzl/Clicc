;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;--------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Die Globalen Variablen der Voranalyse, der 
;;;            Seiteneffektanalyse, der Let-optimierung und der 
;;;            Tail-Rekursion.
;;; $Revision: 1.1 $
;;; $Log: se-init.lisp,v $
;;; Revision 1.1  1993/11/09  15:36:01  atr
;;; Initial revision
;;;
;;;-------------------------------------------------------------------------

;;;-------------------------------------------------------------------------
;;; Die globalen Variablen der Voranalyse.
;;;-------------------------------------------------------------------------
(in-package "CLICC")

(defvar *upward-fun-args* nil )    
                                   
(defvar *down-fun-args* nil)       
(defvar *se-var-env* nil)
(defvar *se-vars-to-funs* )
(defvar *se-fun-counter*)

;;;--------------------------------------------------------------------------
;;; Globale Variablen der Seiteneffektanalyse .
;;;--------------------------------------------------------------------------
(defvar *current-function* )  ;; Die gerade analysierte Funktion.

(defvar *error-function* nil) ;; Die Funktion clicc-lisp::error wird 
                              ;; gesondert behandelt.

(defvar *effect-work-set* )   ;; enthaelt die zu analysierenden Funktionen 
                              ;; waehrend der Fixpunktiteration.

(defvar *jump-level* )        ;; Zu welchem Niveau wird es gesprungen ??.

(defvar *static-level* 0)     ;; aktuelles statisches Niveau.

;;;--------------------------------------------------------------------------
;;; Schalter f"ur die Analyse, die Let-Optimierung und die Tail-Rekursion.
;;;--------------------------------------------------------------------------

(defvar *Side-effect-info-level* 1)
(defvar *no-pre-analysis* nil)
(defvar *no-let-optimizing* nil)    
(defvar *no-tail-recursion* nil)

(defvar *eliminated-vars* 0)
(defvar *eliminated-lets* 0)
(defvar *subst-number*    0)
(defvar *let-effect* )
(defvar *local-effect*)
(defvar *vars-bound-but-not-used* 0)

;;;--------------------------------------------------------------------------
;;; Die globale Variablen der Tail-Rekursion.
;;;--------------------------------------------------------------------------
(defvar *new-body*)           ;; Der neue Rumpf einer Funktion nach der
                              ;; Tail-Rekursion ist ein Tagbody-konstrukt.

(defvar *body-tag*)           ;; Der Anfang des Rumpfes.

(defvar *result-var*)         ;; Der Resultatswert der Funktion wird nun
                              ;; an den ersten Parameter der Funktion
                              ;; zugewiesen, und der Parameter wird dann
                              ;; als Resultat der Funktion zurueckgegeben.

(defvar *app-counter*)        ;; Diese Variablen sind nur momentan fuer mich
(defvar *optimized-funs*)     ;; da.

;;;--------------------------------------------------------------------------
;;; Bei der Analyse der ZWS-Formen werden die gelesenen Variablen bzw
;;; die vera"nderten Variablen in den read-list-Slot bzw in den 
;;; write-list-Slot eingetragen.
;;;--------------------------------------------------------------------------
(defclass1 effect  ()
  (read-list   :initform nil :type (or integer list ))
  (write-list  :initform nil :type (or integer list ))
  (data-effects :initform nil :type (member nil :alloc :dest :jump
                                            :alloc-jump :alloc-dest :dest-jump
                                            :alloc-dest-jump)))

  
;;;-------------------------------------------------------------------------
;;; Diese Strukture dient zum Abspeichern der textuell sichtbaren 
;;; Effekte durch ein SETQ oder Referenzen von Variablen.
;;;-------------------------------------------------------------------------
(defclass1 local-effect ()
  (read-list  :initform nil :type list)
  (write-list :initform nil :type list))

;;;-------------------------------------------------------------------------
(provide "se-init")    
