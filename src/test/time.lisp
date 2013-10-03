;;;-----------------------------------------------------------------------------
;;; Project  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Content  : Definition of the time macro, which is not supported by CLiCC
;;;            because it is considered to be part of the development
;;;            environment
;;;
;;; $Revision: 1.1 $
;;; $Log: time.lisp,v $
;;; Revision 1.1  1994/06/23  12:52:52  hk
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------
(in-package "USER" :use '("LISP" "FFI"))

(load-foreign "time.def")

(defun ticks ()
  (let* ((tmsbuf (make-tms))
         (current-time (times tmsbuf))
         (utime (tms-utime tmsbuf))
         (stime (tms-stime tmsbuf)))
    (free tmsbuf)
    (values (lisp-integer utime) (lisp-integer stime))))
  
(defun print-time (u0 u1 s0 s1)
  (labels
      ((rounded (x)
         (/ (round (* 10 x)) 10)))
    (let* ((hz 60)
           (ut (rounded (/ (- u1 u0) hz)))
           (st (rounded (/ (- s1 s0) hz)))
           (tot (+ ut st)))
      (format T "~%user ~A sec, system ~A sec, sum ~A sec~%"
              ut st tot))))

(defmacro time (&rest form)
  (let ((u0 (gensym))
        (s0 (gensym))
        (rslt (gensym))
        (u1 (gensym))
        (s1 (gensym)))
    `(multiple-value-bind (,u0 ,s0)
      (ticks)
      (let ((,rslt (progn ,@form)))
        (multiple-value-bind (,u1 ,s1)
            (ticks)
          (print-time ,u0 ,u1 ,s0 ,s1))
          ,rslt))))
