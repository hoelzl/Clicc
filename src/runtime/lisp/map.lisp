;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Laufzeitsystem: MAPCAR, MAPLIST, MAPC, MAPL, MAPCAN, MAPCON
;;;
;;; $Revision: 1.7 $
;;; $Log: map.lisp,v $
;;; Revision 1.7  1994/02/02  09:45:16  hk
;;; mapcar, maplist, mapcan, mapcon mit der Deklaration
;;; simp-when-no-result versehen. Definition von mapc und mapl nach vorn
;;; gezogen, da sie in den Deklarationen verwendet werden..
;;;
;;; Revision 1.6  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.5  1993/04/22  10:47:09  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Exports eingefuegt.
;;;
;;; Revision 1.4  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.7 $ eingefuegt
;;;
;;; Revision 1.3  1992/07/06  15:16:14  hk
;;; Schreibfehler.
;;;
;;; Revision 1.2  1992/07/06  15:15:49  hk
;;; 'runtime --> "RUNTIME"
;;;
;;; Revision 1.1  1992/03/24  17:12:55  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "LISP")

(export '(mapcar maplist mapc mapl mapcan mapcon))

;;------------------------------------------------------------------------------
(defun mapc (f list &rest lists)
  (dolist (x list)
    (labels
        ((get-rest-args (lists)
           (cond
             ((atom lists) nil)
             (T
              ;; eine der Rest-Listen ist vollstaendig abgearbeitet
              ;;---------------------------------------------------
              (when (atom (car lists))
                (return))
              (cons (pop (car lists)) 
                    (get-rest-args (cdr lists)))))))

      (apply f x (get-rest-args lists))))
  list)

;;------------------------------------------------------------------------------
(defun mapl (f list &rest lists)
  (loop
    (labels
        ((get-rest-args (lists)
           (cond
             ((atom lists) nil)
             (T
              ;; eine der Rest-Listen ist vollstaendig abgearbeitet
              ;;---------------------------------------------------
              (when (atom (car lists))
                (return))
              (cons (prog1 (car lists) (pop (car lists)))
                    (get-rest-args (cdr lists)))))))
      (if (atom list)
          (return)
          (apply f list (get-rest-args lists)))
      (pop list)))
  list)

;;------------------------------------------------------------------------------
(defun mapcar (f list &rest lists)
  (declare (:simp-when-no-result mapc))
  (labels
      ((mapcar-internal ()
         (labels
             ((get-rest-args (lists)
                (cond
                  ((atom lists) nil)
                  (T
                   ;; eine der Rest-Listen ist vollstaendig abgearbeitet
                   ;;---------------------------------------------------
                   (when (atom (car lists))
                     (return-from mapcar-internal nil))
                   (cons (pop (car lists)) 
                         (get-rest-args (cdr lists)))))))

           (if (atom list)

               ;; die erste Liste ist vollstaendig abgearbeitet
               ;;----------------------------------------------
               nil

               (cons (apply f (pop list) (get-rest-args lists))
                     (mapcar-internal))))))

    (mapcar-internal)))

;;------------------------------------------------------------------------------
(defun maplist (f list &rest lists)
  (declare (:simp-when-no-result mapl))
  (labels
      ((maplist-internal ()
         (labels
             ((get-rest-args (lists)
                (cond
                  ((atom lists) nil)
                  (T
                   ;; eine der Rest-Listen ist vollstaendig abgearbeitet
                   ;;---------------------------------------------------
                   (when (atom (car lists))
                     (return-from maplist-internal nil))
                   (cons (prog1 list (pop (car lists)))
                         (get-rest-args (cdr lists)))))))

           (if (atom list)
               ;; die erste Liste ist vollstaendig abgearbeitet
               ;;----------------------------------------------
               nil

               (cons (apply f (prog1 list (pop list)) (get-rest-args lists))
                     (maplist-internal))))))

    (maplist-internal)))

;;------------------------------------------------------------------------------
(defun mapcan (f list &rest lists)
  (declare (:simp-when-no-result mapc))
  (labels
      ((mapcan-internal ()
         (labels
             ((get-rest-args (lists)
                (cond
                  ((atom lists) nil)
                  (T
                   ;; eine der Rest-Listen ist vollstaendig abgearbeitet
                   ;;---------------------------------------------------
                   (when (atom (car lists))
                     (return-from mapcan-internal nil))
                   (cons (pop (car lists)) 
                         (get-rest-args (cdr lists)))))))

           (if (atom list)

               ;; die erste Liste ist vollstaendig abgearbeitet
               ;;----------------------------------------------
               nil

               (nconc (apply f (pop list) (get-rest-args lists))
                      (mapcan-internal))))))

    (mapcan-internal)))

;;------------------------------------------------------------------------------
(defun mapcon (f list &rest lists)
  (declare (:simp-when-no-result mapl))
  (labels
      ((mapcon-internal ()
         (labels
             ((get-rest-args (lists)
                (cond
                  ((atom lists) nil)
                  (T
                   ;; eine der Rest-Listen ist vollstaendig abgearbeitet
                   ;;---------------------------------------------------
                   (when (atom (car lists))
                     (return-from mapcon-internal nil))
                   (cons (prog1 list (pop (car lists)))
                         (get-rest-args (cdr lists)))))))

           (if (atom list)

               ;; die erste Liste ist vollstaendig abgearbeitet
               ;;----------------------------------------------
               nil

               (nconc (apply f (prog1 list (pop list)) (get-rest-args lists))
                      (mapcon-internal))))))

    (mapcon-internal)))
