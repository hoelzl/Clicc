;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Laufzeitfunktionen des FFI
;;;
;;; $Revision: 1.11 $
;;; $Log: foreign.lisp,v $
;;; Revision 1.11  1994/04/18  12:21:33  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Laufzeitsystemfunktionen des FFI hinzugefuegt bzw.ueberarbeitet.
;;;
;;; Revision 1.10  1993/12/16  16:41:43  pm
;;; FFI-Funktionen vom rt:: Package ins FFI: Package geschoben.
;;;
;;; Revision 1.9  1993/11/03  12:32:30  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.8  1993/09/19  15:12:36  pm
;;; Erweiterungen fuer C-Strings
;;;
;;; Revision 1.7  1993/08/24  11:21:59  pm
;;; Erweiterungen um C-Pointer
;;;
;;; Revision 1.6  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.5  1993/05/23  17:56:08  pm
;;; Alle in Lisp geschriebenen Konstruktor- und Konvertierungs-
;;; Funktionen fuer die primitiven C-Typen implementiert
;;;
;;; Revision 1.4  1993/05/21  13:59:37  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.3  1993/05/06  14:38:36  pm
;;; erste Versuche fuer int's
;;;
;;; Revision 1.2  1993/04/28  09:10:27  pm
;;; initial revision
;;;
;;------------------------------------------------------------------------------

(in-package "LISP")


(export
 '(ffi:c-char ffi:c-short ffi:c-int ffi:c-long
   ffi:c-unsigned-char ffi:c-unsigned-short 
   ffi:c-unsigned-int ffi:c-unsigned-long
   ffi:c-float ffi:c-double ffi:c-long-double
   ffi:c-handle ffi:c-string
   ffi:lisp-integer ffi:lisp-character ffi:lisp-float ffi:make-lisp-string
   ffi:make-c-string ffi:copy-c-string)
 "FFI")

;;------------------------------------------------------------------------------
;; Die C-Typen anlegen
;;------------------------------------------------------------------------------
(deftype ffi:c-char ()  `(satisfies ffi::c-char-p))
(deftype ffi:c-short () `(satisfies ffi::c-short-p))
(deftype ffi:c-int ()   `(satisfies ffi::c-int-p))
(deftype ffi:c-long ()  `(satisfies ffi::c-long-p))
(deftype ffi:c-unsigned-char ()  `(satisfies ffi::c-unsigned-char-p))
(deftype ffi:c-unsigned-short () `(satisfies ffi::c-unsigned-short-p))
(deftype ffi:c-unsigned-int ()   `(satisfies ffi::c-unsigned-int-p))
(deftype ffi:c-unsigned-long ()  `(satisfies ffi::c-unsigned-long-p))
(deftype ffi:c-float ()        `(satisfies ffi::c-float-p))
(deftype ffi:c-double ()       `(satisfies ffi::c-double-p))
(deftype ffi:c-long-double ()  `(satisfies ffi::c-long-double-p))

(deftype ffi:c-string () '(satisfies ffi::c-string-p))

;;------------------------------------------------------------------------------
;; Die Konstruktor-Funktionen zum Anlegen von C-Daten
;;------------------------------------------------------------------------------
(defun ffi:c-char (value)
  (cond 
    ((typep value 'character)
      (rt::make-c-char value))
    ((ffi:c-char-p value)
     (rt::cast-c-char value))
    (t (error-in "C-CHAR"
                "The evaluated value ~S is not of type character." value))))

(defun ffi:c-short (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-short value))
    ((ffi:c-short-p value)
     (rt::cast-c-short value))
    (t (error-in "C-SHORT"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-int (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-int value))
    ((ffi:c-int-p value)
     (rt::cast-c-int value))
    (t (error-in "C-INT"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-long (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-long value))
    ((ffi:c-long-p value)
     (rt::cast-c-long value))
    (t (error-in "C-LONG"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-unsigned-char (value)
  (cond 
    ((typep value 'character)
      (rt::make-c-unsigned-char value))
    ((ffi:c-unsigned-char-p value)
     (rt::cast-c-unsigned-char value))
    (t (error-in "C-UNSIGNED-CHAR"
                "The evaluated value ~S is not of type character." value))))

(defun ffi:c-unsigned-short (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-unsigned-short value))
    ((ffi:c-unsigned-short-p value)
     (rt::cast-c-unsigned-short value))
    (t (error-in "C-UNSIGNED-SHORT"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-unsigned-int (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-unsigned-int value))
    ((ffi:c-unsigned-int-p value)
     (rt::cast-c-unsigned-int value))
    (t (error-in "C-UNSIGNED-INT"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-unsigned-long (value)
  (cond 
    ((typep value 'integer)
      (rt::make-c-unsigned-long value))
    ((ffi:c-unsigned-long-p value)
     (rt::cast-c-unsigned-long value))
    (t (error-in "C-UNSIGNED-LONG"
                 "The evaluated value ~S is not of type fixnum." value))))
    
(defun ffi:c-float (value)
  (cond 
    ((typep value 'float)
      (rt::make-c-float value))
    ((ffi:c-float-p value)
     (rt::cast-c-float value))
    (t (error-in "C-FLOAT"
                 "The evaluated value ~S is not of type float." value))))
    
(defun ffi:c-double (value)
  (cond 
    ((typep value 'float)
      (rt::make-c-double value))
    ((ffi:c-double-p value)
     (rt::cast-c-double value))
    (t (error-in "C-DOUBLE"
                 "The evaluated value ~S is not of type float." value))))
    
(defun ffi:c-long-double (value)
  (cond 
    ((typep value 'float)
      (rt::make-c-long-double value))
    ((ffi:c-long-double-p value)
     (rt::cast-c-long-double value))
    (t (error-in "C-LONG-DOUBLE"
                 "The evaluated value ~S is not of type float." value))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun ffi:make-c-string (value)
  (if (typep value 'string)
      (rt::internal-make-c-string value)
      (error-in "MAKE-C-STRING"
                "The evaluated value ~S is not of type string." value)))

(defun ffi:copy-c-string (c-value)
  (if (ffi::c-string-p c-value)
      (rt::internal-copy-c-string c-value)
      (error-in "MAKE-C-STRING"
                "The evaluated value ~S is not of type c-string." c-value)))
  
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun ffi:lisp-character (c-value)
  (if (or (ffi::c-char-p c-value)
          (ffi::c-unsigned-char-p c-value))
      (rt::make-lisp-character c-value)
      (error-in 
       "LISP-CHARACTER"
       "The evaluated value ~S is not of type c-<char>." c-value)))

(defun ffi:lisp-integer (c-value)
  (if (or (ffi::c-long-p c-value)
          (ffi::c-unsigned-long-p c-value))
      (rt::make-lisp-integer c-value)
      (error-in
       "LISP-INTEGER"
       "The evaluated value ~S is not of type c-<integer>." c-value)))

(defun ffi:lisp-float (c-value)
  (if (ffi::c-long-double-p c-value)
      (rt::make-lisp-float c-value)
      (error-in
       "LISP-FLOAT"
       "The evaluated value ~S is not of type c-<float>." c-value)))

(defun ffi:make-lisp-string (c-value)
  (if (ffi::c-string-p c-value)
      (rt::internal-make-lisp-string c-value)
      (error-in
       "MAKE-LISP-STRING"
       "The evaluated value ~S is not of type c-string." c-value)))
