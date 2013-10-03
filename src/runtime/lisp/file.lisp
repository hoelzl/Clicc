;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem
;;;            OPEN, FILE-POSITION, FILE-LENGTH,
;;;            READ-CHAR, UNREAD-CHAR WRITE-CHAR
;;;
;;; $Revision: 1.14 $
;;; $Log: file.lisp,v $
;;; Revision 1.14  1993/11/10  17:34:45  hk
;;; Schreibfehler in der letzten revision behoben
;;;
;;; Revision 1.13  1993/11/10  16:44:23  hk
;;; Fehler in write-char behoben: stream t -> *standard-ouput*
;;;
;;; Revision 1.12  1993/11/10  16:06:59  hk
;;; In read-char den Defaultwert von eof-error-p zu T korrigiert.
;;;
;;; Revision 1.11  1993/11/01  16:05:01  hk
;;; Falschen Kommentar gelöscht.
;;;
;;; Revision 1.10  1993/08/17  10:25:22  hk
;;; Open benutzt nun namestring, um das Argument in einen String zu
;;; wandeln
;;;
;;; Revision 1.9  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.8  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.7  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.14 $ eingefuegt
;;;
;;; Revision 1.6  1993/01/14  16:17:13  hk
;;; probe-file geloescht, da in pathname definiert.
;;;
;;; Revision 1.5  1993/01/07  12:23:11  hk
;;; C-fclose deklariert.
;;;
;;; Revision 1.4  1993/01/06  16:30:46  hk
;;; Aufruf von C-fopen vereinfacht.
;;;
;;; Revision 1.3  1993/01/05  15:37:48  hk
;;; Zusaetzliches Argument fuer make-file-...-stream.
;;;
;;; Revision 1.2  1992/07/06  15:24:43  hk
;;; 'runtime --> "RUNTIME"
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(open file-position file-length read-char unread-char write-char))

;;------------------------------------------------------------------------------
;; Einschraenkungen:
;; - :ELEMENT-TYPE wird nicht unterstuetzt
;; - :RENAME, :RENAME-AND-DELETE werden nicht unterstuetzt
;; - :SUPERSEDE, :NEW-VERSION ueberschreiben eine evtl. bestehende Datei
;;------------------------------------------------------------------------------
(defun open (name &key (direction :input)
                  (if-exists :error)
                  (if-does-not-exist
                   (case direction
                     (:input :error)
                     ((:output :io)
                      (case if-exists
                        ((:overwrite :append) :error)
                        (T :create)))
                     (:probe nil)
                     (T (error "illegal option ~s" direction)))))
  (let (in-file out-file)

    (setq name (namestring name))

    ;; IF-DOES-NOT-EXIST bearbeiten
    ;;-----------------------------
    (setq in-file (rt::C-fopen name "r"))
    (unless in-file
      (case if-does-not-exist
        (:error (error "~s does not exist" name))
        ((nil) (return-from open nil))
        (:create
         (setq out-file (rt::C-fopen name "w+"))
         (when (null out-file) (error "can not create ~S" name)))
        (t (error "illegal option ~s" if-does-not-exist))))

    (labels
        ((get-io-file ()
           (cond
             ;; die Datei wurde neu angelegt
             ;;-----------------------------
             ((null in-file) out-file)

             ;; die Datei existiert schon
             ;;--------------------------
             (t (rt::C-fclose in-file)
                (case if-exists
                  (:error (error "file already exists ~s" name))
                  (:overwrite (rt::C-fopen name "r+"))
                  (:append (rt::C-fopen name "a+"))
                  ((:supersede :new-version) (rt::C-fopen name "w+"))
                  ((nil) (return-from open nil))
                  (T (error "unimplemented option ~S" if-exists)))))))
      
      (case direction
        ((:input :probe)
         (when out-file
           (close out-file)
           (setq in-file (rt::C-fopen name "r+")))
         (when (null in-file) (error "can not open ~S" name))
         (let ((stream (make-file-input-stream in-file name)))
           (when (eq direction :probe) (close stream))
           stream))

        (:output
         (make-file-output-stream (get-io-file) name))

        (:io
         (make-file-io-stream (get-io-file) name))))))

;;------------------------------------------------------------------------------
(defun file-position (stream &optional (position nil pos-supplied))
  (cond
    (pos-supplied (setq position
                        (typecase position
                          (integer
                           (unless (<= 0 position (file-length stream))
                             (error "illegal file-position")))
                          ((member :start) 0)
                          ((member :end) (file-length stream))
                          (T (error "illegal file-position: ~S" position))))
                   (funcall (stream-seek stream) position))
    (T (funcall (stream-tell stream)))))

;;------------------------------------------------------------------------------
(defun file-length (stream)
  (funcall (stream-length stream)))

;;------------------------------------------------------------------------------
(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (case stream
    ((nil) (setq stream *standard-input*))
    ((t) (setq stream *terminal-io*))
    (T nil))
  (let ((c (funcall (stream-readc stream))))
    (cond
      (c c)
      (T (when (or eof-error-p recursive-p)
           (error "unexpected end-of-stream"))
         eof-value))))

;;------------------------------------------------------------------------------
(defun unread-char (c &optional stream)
  (case stream
    ((nil) (setq stream *standard-input*))
    ((t) (setq stream *terminal-io*))
    (T nil))
  
  (funcall (stream-unreadc stream) c))

;;------------------------------------------------------------------------------
(defun write-char (c &optional stream)
  (case stream
    ((nil) (setq stream *standard-output*))
    ((t) (setq stream *terminal-io*))
    (T nil))
  
  (funcall (stream-writec stream) c))
