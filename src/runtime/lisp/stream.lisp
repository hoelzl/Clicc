;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem
;;;            - STREAM als LISP-Struktur
;;;            - SYNONYM-, BROADCAST-, CONCATENATED-, TWO-WAY-, ECHO-,
;;;              STRING-INPUT-, STRING-OUTPUT-STREAM
;;;            - FILE-...-STREAM
;;;            - CLOSE
;;;
;;; $Revision: 1.13 $
;;; $Log: stream.lisp,v $
;;; Revision 1.13  1994/06/02  13:43:44  hk
;;; Print-Funktion f"ur stream-Struktur von write2 nach hier.
;;;
;;; Revision 1.12  1993/12/14  15:38:48  uho
;;; lisp::make-stream -> exportiertes rt::make-stream
;;;
;;; Revision 1.11  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.10  1993/06/05  19:25:49  hk
;;; Funktion file-name ins Package RT.
;;;
;;; Revision 1.9  1993/05/07  08:56:00  hk
;;; stream exportiert.
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
;;; $Revision: 1.13 $ eingefuegt
;;;
;;; Revision 1.6  1993/01/06  16:30:22  hk
;;; Spezifikationen von C-fclose, C-fgetc, ... nach hier.
;;;
;;; Revision 1.5  1993/01/05  17:02:42  hk
;;; Neue Funktion file-name.
;;;
;;; Revision 1.4  1993/01/05  15:39:39  hk
;;; Zusaetzlicher Parameter 'name' fuer make-file-..-stream.
;;;
;;; Revision 1.3  1992/12/21  11:47:36  hk
;;; Schreibfehler in output-stream-p behoben.
;;;
;;; Revision 1.2  1992/07/06  11:35:49  hk
;;; string-char --> character.
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(*terminal-io* *standard-input* *standard-output* *error-output* *query-io*
   stream streamp
   make-synonym-stream make-broadcast-stream make-concatenated-stream
   make-two-way-stream make-echo-stream make-string-input-stream
   make-string-output-stream get-output-stream-string input-stream-p
   output-stream-p stream-element-type close))

(export '(rt::file-name rt::make-stream) "RT")

;;------------------------------------------------------------------------------
(defparameter *terminal-io*     (make-terminal-io))
(defparameter *standard-input*  (make-synonym-stream '*terminal-io*))
(defparameter *standard-output* *standard-input*)
(defparameter *error-output*    *standard-input*)
(defparameter *query-io*        *standard-input*)

;;------------------------------------------------------------------------------
(defstruct (stream (:copier nil)
                   (:predicate streamp)
                   (:constructor rt::make-stream)
                   (:print-function print-stream))
  type

  ;; zusaetzliche Informationen:
  ;; - File-name bei File-Streams
  ;; - Symbol bei Synonym-Streams
  ;; - get-output-stream-string bei string-output-streams
  ;;-----------------------------------------------------
  extra
  (readc #'undef-stream-op)
  (unreadc #'undef-stream-op)
;;; (listen #'undef-stream-op)
  (writec #'undef-stream-op)
  (column #'nil-fun)
  (tell #'nil-fun)
  (seek #'undef-stream-op)
  (length #'undef-stream-op)
  close)

;;------------------------------------------------------------------------------
(defun print-stream (stream to-stream depth)
  (write-string "#<" to-stream)
  (princ (stream-type stream) to-stream)
  (write-string "-STREAM" to-stream)
  (when (member (stream-type stream) '(FILE-INPUT FILE-OUTPUT FILE-IO))
    (write-char #\Space to-stream)
    (write-string (stream-extra stream) to-stream))
  (write-char #\> to-stream))
  
;;------------------------------------------------------------------------------
(defun undef-stream-op (&rest x)
  (declare (ignore x))
  (error "undefined Stream Operation"))

;;------------------------------------------------------------------------------
(defun nil-fun () nil)

;;------------------------------------------------------------------------------
(defun close-err (&rest x)
  (declare (ignore x))
  (error "stream is closed"))

;;------------------------------------------------------------------------------
;; verbietet weitere E/A-Operationen auf dem Stream
;; wiederholtes Close bleibt ohne Wirkung
;;------------------------------------------------------------------------------
;; CLtL,2.Ed: Closing a composite stream has no effect on its contents
;;------------------------------------------------------------------------------
(defun close1 (stream)
  (setf (stream-readc stream) #'close-err)
  (setf (stream-unreadc stream) #'close-err)
;;;  (setf (stream-listen stream) #'close-err)
  (setf (stream-writec stream) #'close-err)
  (setf (stream-tell stream) #'close-err)
  (setf (stream-seek stream) #'close-err)
  (setf (stream-close stream)
        ;; Identitaet
        ;;-----------
        #'(lambda (stream) (declare (ignore stream)) "already closed")))

;;------------------------------------------------------------------------------
;; Resultat von readc ist nil, wenn End-of-File. !!!
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
(defun make-synonym-stream (symbol)
  (rt::make-stream :type 'SYNONYM
                   :extra symbol
                   :readc
                   #'(lambda () (funcall (stream-readc (symbol-value symbol))))
                   :unreadc
                   #'(lambda (c)
                       (funcall (stream-unreadc (symbol-value symbol)) c))
                   :writec
                   #'(lambda (c)
                       (funcall (stream-writec (symbol-value symbol)) c))
;;;                :listen
;;;                #'(lambda () (funcall (stream-listen (symbol-value symbol))))
                   :column #'(lambda ()
                               (funcall (stream-column (symbol-value symbol))))
                   :tell
                   #'(lambda () (funcall (stream-tell (symbol-value symbol))))
                   :seek
                   #'(lambda () (funcall (stream-seek (symbol-value symbol))))
                   :length
                   #'(lambda () (funcall (stream-length (symbol-value symbol))))
                   :close
                   #'close1))

;;------------------------------------------------------------------------------
(defun make-broadcast-stream (&rest streams)
  (rt::make-stream :type 'BROADCAST
                   :writec
                   #'(lambda (c) 
                       (dolist (s streams)
                         (funcall (stream-writec s) c)))
                   :close #'close1))

;;------------------------------------------------------------------------------
(defun make-concatenated-stream (&rest streams)
  (rt::make-stream
   :type 'CONCATENATED
   :readc
   (labels
       ((readc ()
          (if (null streams)
              nil
              (progn
                (let ((c (funcall (stream-readc (car streams)))))
                  (if c
                      c
                      (progn
                        (pop streams)
                        (readc))))))))
     #'readc)
   :unreadc
               
   ;; Error, wenn streams == NIL
   ;;---------------------------
   #'(lambda (c) (funcall (stream-unreadc (car streams)) c))
;;;            :listen
;;;            #'(lambda () (if (null streams)
;;;                           nil
;;;                           (funcall (stream-listen (car streams)))))
   :close #'close1))

;;------------------------------------------------------------------------------
(defun make-two-way-stream (in out)
  (rt::make-stream :type 'TWO-WAY
                   :readc #'(lambda () (funcall (stream-readc in)))
                   :unreadc #'(lambda () (funcall (stream-unreadc in)))
;;;                :listen #'(lambda () (funcall (stream-listen in)))
                   :writec #'(lambda () (funcall (stream-writec out)))
                   :column #'(lambda () (funcall (stream-column out)))
                   :close #'close1))
               
;;------------------------------------------------------------------------------
(defun make-echo-stream (in out)
  (let ((unread-count 0))
    (rt::make-stream
     :type 'ECHO
     :readc
     #'(lambda()
         (let ((c (funcall (stream-readc in))))
           (if c
               ;; echo, wenn nicht EOF und nicht unread-char
               (if (eql unread-count 0)
                   (progn
                     (funcall (stream-writec out) c)
                     c)
                   (decf unread-count))
               ;; EOF, kein Echo
               nil)))
     :unreadc #'(lambda (c)
                  (funcall (stream-unreadc in) c) 
                  (incf unread-count))
;;;              :listen #'(lambda () (funcall (stream-listen in)))
     :writec #'(lambda (c) (funcall (stream-writec out) c))
     :column #'(lambda () (funcall (stream-column out)))
     :close #'close1)))
               
;;------------------------------------------------------------------------------
(defun make-string-input-stream (string &optional
                                        (start 0) (end (length string)))
  (let ((index start))
    (rt::make-stream
     :type 'STRING-INPUT
     :readc
     #'(lambda ()
         (if (< index end)
             (prog1 (char string index)
               (incf index))
                     
             ;; EOF
             ;;----
             nil))
     :unreadc
     #'(lambda (c)
         (when (<= index start) (error "reached start of stream"))
         (decf index)
         (unless (eql c (char string index))
           (error "%s should be eql to %s" c (char string index)))
         nil)
;;;            :listen #'(lambda () (< index end))
     :tell #'(lambda () index)
     :seek #'(lambda (pos) (if (<= pos end)
                               (setq index pos)
                               (error "illegal position")))
     :length #'(lambda () end)
     :close #'close1)))

;;------------------------------------------------------------------------------
;; zusaetzlicher Parameter STRING, damit WITH-OUTPUT-TO-STRING implementiert
;; werden kann.
;;------------------------------------------------------------------------------
(defun make-string-output-stream
  (&optional (string (make-array 10 :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0))
   &aux (column 0))
  (rt::make-stream
   :type 'STRING-OUTPUT
   :writec
   #'(lambda (c)
       (if (eql c #\Newline)
           (setq column 0)
           (incf column))
       (vector-push-extend c string))
   :column #'(lambda () column)
   :tell #'(lambda () (length string))
   :length #'(lambda () (length string))

   ;; fuer GET-OUTPUT-STREAM-STRING
   ;;------------------------------
   :extra
   #'(lambda () (prog1 (make-array (length string)
                                   :element-type 'character
                                   :initial-contents string)
                  (setf (fill-pointer string) 0)))
   :close #'close1))

;;------------------------------------------------------------------------------
(defun get-output-stream-string (stream)
  (unless (eq `STRING-OUTPUT (stream-type stream))
    (error "string-output-stream expected"))
  (funcall (stream-extra stream)))

;;------------------------------------------------------------------------------
(defun input-stream-p (stream)
  (if (eq 'SYNONYM (stream-type stream))
    (input-stream-p (symbol-value (stream-extra stream)))
    (member (stream-type stream)
            `(FILE-INPUT FILE-IO CONCATENATED TWO-WAY ECHO STRING-INPUT))))

;;------------------------------------------------------------------------------
(defun output-stream-p (stream)
  (if (eq 'SYNONYM (stream-type stream))
    (output-stream-p (symbol-value (stream-extra stream)))
    (member (stream-type stream)
            `(FILE-OUTPUT FILE-IO BROADCAST TWO-WAY ECHO STRING-OUTPUT))))

;;------------------------------------------------------------------------------
(defun stream-element-type (stream)
  (if (streamp stream) (error "stream expected") 'character))

;;------------------------------------------------------------------------------
;; rt:file-name stream &optional new-name
;;------------------------------------------------------------------------------
(defun rt:file-name (stream  &optional new-name)
  (unless (member (stream-type stream) '(FILE-INPUT FILE-OUTPUT FILE-IO))
    (error "there is no filename associated with stream ~a" stream))
  (when new-name
    (setf (stream-extra stream) new-name))
  (stream-extra stream))

;;----------------------------------------------------------------------------
(defun make-file-input-stream (C-file name)
  (rt::make-stream :type 'FILE-INPUT
                   :extra name
                   :readc #'(lambda () (rt::C-fgetc C-file))
                   :unreadc #'(lambda (c) (rt::C-ungetc c C-file))
;;;                :listen #'(lambda () (rt::C-listen C-file))
                   :tell #'(lambda () (rt::C-ftell C-file))
                   :seek #'(lambda (pos) (rt::C-fseek C-file pos))
                   :length #'(lambda () (rt::C-file-length C-file))
                   :close #'(lambda (stream)
                              (rt::C-fclose C-file) (close1 stream))))

;;------------------------------------------------------------------------------
(defun make-file-output-stream (C-file name &aux (column nil))
  (rt::make-stream
   :type 'FILE-OUTPUT
   :extra name
   :writec #'(lambda (c)
               (if (eql c #\Newline)
                   (setq column 0)
                   (when column (incf column)))
               (rt::C-fputc c C-file))
   :column #'(lambda () column)
   :tell #'(lambda () (rt::C-ftell C-file))
   :seek #'(lambda (pos) (setq column nil) (rt::C-fseek C-file pos))
   :length #'(lambda () (rt::C-file-length C-file))
   :close #'(lambda (stream)
              (rt::C-fclose C-file) (close1 stream))))

;;----------------------------------------------------------------------------
(defun make-file-io-stream (C-file name)
  (rt::make-stream
   :type 'FILE-IO
   :extra name
   :readc #'(lambda () (rt::C-fgetc C-file))
   :unreadc #'(lambda (c) (rt::C-ungetc c C-file))
;;;            :listen #'(lambda () (rt::C-listen C-file))
   :tell #'(lambda () (rt::C-ftell C-file))
   :seek #'(lambda (pos) (rt::C-fseek C-file pos))
   :length #'(lambda () (rt::C-file-length C-file))
   :writec #'(lambda (c) (rt::C-fputc c C-file))
   :close #'(lambda (stream)
              (rt::C-fclose C-file) (close1 stream))))

;;------------------------------------------------------------------------------
(defun make-terminal-io ()
  "zum Erzeugen von *terminal-io* mittels stdin, stdout"
  (let ((stdin (rt::C-stdin))
        (stdout (rt::C-stdout))
        (column 0))
    (rt::make-stream
     :type 'FILE-IO
     :extra "stdin / stdout"
     :readc #'(lambda () (rt::C-fgetc stdin))
     :unreadc #'(lambda (c) (rt::C-ungetc c stdin))
;;;              :listen #'(lambda () (rt::C-listen stdin))
     :writec #'(lambda (c)
                 (if (eql c #\newline)
                     (setq column 0)
                     (incf column))
                 (rt::C-fputc c stdout))
     :column #'(lambda () column)
     :close #'(lambda (stream)
                (declare (ignore stream))
                "can't close *terminal-io*"))))

;;------------------------------------------------------------------------------
(defun close (stream &key abort)
  (funcall (stream-close stream) stream))
