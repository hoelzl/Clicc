;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Lader-Parser fuer FFI-Beschreibungsdateien.
;;; 
;;; $Revision: 1.25 $
;;; $Log: ffload.lisp,v $
;;; Revision 1.25  1994/06/08  12:04:21  hk
;;; use-package und unuse-package in einem LET versteckt, damit CLISP es
;;; nicht bereits zur "Ubersetzungszeit auswertet.
;;;
;;; Revision 1.24  1994/05/17  08:33:17  pm
;;; Fehler korrigiert
;;;
;;; Revision 1.23  1994/04/27  16:36:10  pm
;;; Merkwuerdigen Fehler behoben.
;;; Koennte ein Fehler in Allegro sein.
;;;
;;; Revision 1.22  1994/04/22  14:10:53  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Packages korrigiert.
;;;
;;; Revision 1.21  1994/04/18  12:07:48  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - NOCH ZU UEBERARBEITEN: Package immer noch nicht richtig durchdacht.
;;;
;;; Revision 1.20  1993/12/17  11:28:59  pm
;;; Member wird jetzt auf eine liste und nicht auf eine queue angewendet.
;;;
;;; Revision 1.19  1993/12/16  16:32:27  pm
;;; Kommentare eingefuegt.
;;;
;;; Revision 1.18  1993/11/03  11:45:30  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.17  1993/07/26  16:43:50  pm
;;; Anlegen der Interface-datei eingefügt
;;;
;;; Revision 1.16  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.15  1993/06/10  09:32:09  pm
;;; Quelltext aufgeraeumt
;;;
;;; Revision 1.14  1993/05/31  17:02:28  pm
;;; Bearbeitung von def-call-in eingefuegt
;;;
;;; Revision 1.13  1993/05/23  17:49:59  pm
;;; Aufruf von def-c-type um das Argument package erweitert
;;;
;;; Revision 1.12  1993/05/13  12:42:54  pm
;;; Gleichen Fehler nochmal korrigiert.
;;;
;;; Revision 1.11  1993/05/13  12:00:51  pm
;;; Fehler behoben
;;;
;;; Revision 1.10  1993/05/12  08:38:05  pm
;;; packages korrigiert
;;;
;;; Revision 1.9  1993/05/06  14:32:58  pm
;;; (use-package FFI) herausgenommen
;;;
;;; Revision 1.8  1993/03/10  12:47:24  pm
;;; Kleinigkeiten geaendert
;;;
;;; Revision 1.7  1993/02/17  11:02:19  hk
;;; *clicc-package* durch *package* ersetzt.
;;;
;;; Revision 1.6  1993/02/16  15:23:33  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.5  1993/01/18  10:56:02  pm
;;; Inkonsistens behoben
;;;
;;; Revision 1.4  1992/12/01  15:10:04  pm
;;; kleine Aenderungen
;;;
;;; Revision 1.3  1992/11/10  10:24:35  pm
;;; Fehler ausgemaerzt
;;;
;;; Revision 1.2  1992/11/05  10:45:44  pm
;;; p1-load-foreign eingebaut
;;;
;;; Revision 1.1  1992/10/19  09:29:46  pm
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Globale Variablen
;;------------------------------------------------------------------------------
(defvar *interface-file-stream*)
(defvar *interface-file-queue* (empty-queue)) 
                                        ; Liste der geladenen Definitionsdateien
(defvar *ffi-signatures* '())
;;------------------------------------------------------------------------------
;; p1-load-foreign
;; Analysiert eine Beschreibungsdatei.
;; Folgende Konstrukte werden erkannt:
;;  - def-call-out           Definition einer Call-Out-Funktion,
;;  - def-call-in            Definition einer Call-In-Funktion,
;;  - def-c-type             Definition eines C-Types.
;;------------------------------------------------------------------------------
(defun p1-load-foreign (all-args)
  (let* ((old-package-use-list (package-use-list *package*))
         (name-of-file (first all-args))
         (file-name (pathname-name name-of-file))
         (extension (pathname-type name-of-file))
         (directory (directory-namestring name-of-file))
         (*package* *package*)          ; Wird lokal fuer diese F. geaendert
         definition-file
         form)

    ;;-------------------------------------------
    #+CLISP(let ((x "FFI")) (use-package x)) ; do not execute at compile time
    #-CLISP(use-package "FFI")

    ;; Den Filenamen bei Bedarf mit der Endung ".def" versehen
    ;;--------------------------------------------------------
    (unless (or (equal extension "def") (equal extension 'nil))
      (clicc-error "Wrong extension for LOAD-FOREIGN: ~A." extension))
    
    (setq definition-file (concatenate 'string directory file-name ".def"))
    
    ;; Ist die Beschreibungsdatei nicht vorhanden, Fehler ausgeben
    ;;------------------------------------------------------------
    (unless (probe-file definition-file)
      (clicc-error "Unknown file: ~A." definition-file))

    ;; Die Definitionsdatei nur bearbeiten, wenn sie nicht schon einmal
    ;; bearbeitet wurde.
    ;;------------------
    (unless 
        (member definition-file
                (queue2list *interface-file-queue*) 
                :test #'string=)
      
      ;; File oeffnen und bearbeiten
      ;;----------------------------
      (with-open-file (def-file definition-file :direction :input)
        
        (clicc-message "Load definition-file ~A" definition-file)
        
        ;; Solange S-Ausdruecke lesen, bis Datei zuende
        ;;---------------------------------------------
        (loop
         (setq form (clicc-read def-file))
         (if (or (eq '|eof| form)
                 ;; Kleiner Fehler in Allegro
                 ;; Manchmal wird kein '|eof| gesendet
                 ;;-----------------------------------
                 #+allegro-v4.1 (eq 'EOF form)
                 )
             (return)
             
             ;; Die Eintraege abarbeiten.
             ;;--------------------------
             (case (first form)

               (ffi:foreign-package-name
                (let* ((package-name (second form)))
                  (setq *package* 
                        (let ((package (find-package package-name)))
                          (if package
                              package
                              (make-package package-name
                                            :use '("FFI")))))))
               
               (ffi:def-call-out
                   (p1-def-call-out (rest form)))
               
               (ffi:def-call-in
                   (p1-def-call-in (rest form)))
               
               (ffi:def-c-type
                   (p1-def-c-type (rest form)))
               
               (t
                (clicc-error "Unknown identifier in Signature: ~A" 
                             form)))))
        
        ;; Definitionsdatei in die Queue einhaengen, und damit
        ;; fuer eine weitere Benutzung sperren.
        ;;-------------------------------------
        (add-q definition-file *interface-file-queue*)))

    #+CLISP(let ((x "FFI")) (unuse-package x)) ; do not execute at compile time
    #-CLISP(unuse-package "FFI")
    (use-package old-package-use-list)))

;;------------------------------------------------------------------------------
(provide "ffload")
