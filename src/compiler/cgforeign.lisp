;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Foreign Functions - codegeneration
;;;
;;; $Revision: 1.24 $
;;; $Log: cgforeign.lisp,v $
;;; Revision 1.24  1994/05/17  08:20:15  pm
;;; type-of in typecase umgewandelt, da CLiCC kein type-of kennt.
;;; Codegenerierung an Aenderungen angepasst.
;;;
;;; Revision 1.23  1994/04/26  12:34:42  sma
;;; Direkte Benutzung von C-MacroCall LOAD_NIL bzw LOAD_INTEGER durch
;;; Funktionen C-nil bzw C-integer ersetzt.
;;;
;;; Revision 1.22  1994/04/22  14:08:22  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Fehlende Funktionalitaet eingebaut.
;;;
;;; Revision 1.21  1994/04/18  12:02:15  pm
;;; Foreign Function Interface voellig ueberarbeitet.
;;; - Alle Funktionen neu ueberarbeitet
;;; - SICHERHEITS-CHECK: noch nicht die ganze Funktionalitaet eingebaut!
;;;
;;; Revision 1.20  1994/01/05  13:15:30  hk
;;; In cg-call-in-interface #+CMU17(declare (notinline C-blockend))
;;; eingefügt, um einen Fehler in CMU-CL 17.c zu umgehen
;;;
;;; Revision 1.19  1993/12/17  11:16:41  pm
;;; Fehler behoben: get_c_string statt GET_C_STRING aufrufen
;;;
;;; Revision 1.18  1993/12/16  16:28:40  pm
;;; Codegenerator bereinigt.
;;; Call-In -Codegenerator.
;;;
;;; Revision 1.16  1993/11/03  11:44:22  pm
;;; Inkonsistenzen in den Symbolnamen behoben.
;;;
;;; Revision 1.15  1993/08/24  11:18:54  pm
;;; Erweiterungen um C-Pointer
;;;
;;; Revision 1.14  1993/07/26  16:49:36  pm
;;; Erweiterungen um C_Strukturen.
;;;
;;; Revision 1.13  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.12  1993/06/04  13:44:10  pm
;;; cg-call-in-interface eingebaut
;;;
;;; Revision 1.11  1993/05/31  17:01:13  pm
;;; Schreibfehler beseitigt
;;;
;;; Revision 1.10  1993/05/23  17:46:06  pm
;;; Codegenerierung fuer die C-Funktionen basierend auf den
;;; primitven C-Typen implementiert
;;;
;;; Revision 1.9  1993/05/21  13:55:49  pm
;;; c-int in int umbenannt
;;;
;;; Revision 1.8  1993/05/12  14:11:29  pm
;;; packages verstanden und korrigiert.
;;;
;;; Revision 1.7  1993/03/10  12:46:56  pm
;;; Kleinigkeiten geaendert
;;;
;;; Revision 1.6  1993/02/16  16:07:31  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.5  1993/01/12  14:09:45  pm
;;; Fehler behoben
;;;
;;; Revision 1.4  1992/12/01  15:11:33  pm
;;; c-char* eingebaut
;;;
;;; Revision 1.3  1992/11/25  12:36:55  pm
;;; Codegenerator fuer das Foreign Function Interface
;;;
;;; Revision 1.2  1992/11/05  12:55:19  pm
;;; initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
(defconstant NOT-IMPLEMENTED-YET 
  "Not implemented yet: ~A")

;;******************************************************************************
;; Call-Out
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Erzeuge die Applikation einer Foreign Function
;;------------------------------------------------------------------------------
(defmethod cg-app ((fun foreign-fun) args app)
  (declare (ignore app))

  (let* ((arg-list (cg-ffi-args (cdr args) fun))
         (typesymbol (first args)))

    ;; Aktuellen stack-top retten fuer eventuelle Call-Ins
    ;;----------------------------------------------------
    (C-assign "save_stack" (CC-Stack *stack-top*))

    (C-ForeignCall fun *stack-top* arg-list typesymbol)
    (stacktop-to-result-loc)))

;;------------------------------------------------------------------------------
;; Pendant zu cg-args. 
;;------------------------------------------------------------------------------
(defun cg-ffi-args (args fun)
  (let ((*result-spec* (stacktop-result-location))
        (old-stack *stack-top*)
        (arglist-queue (empty-queue)))
    
    (do ((arg args (cdr arg))
         (type (?arguments fun) 
               (if (equalp type '(ffi:c-vararg)) type (cdr type))))
        ((null arg))
      (let ((arg (first arg))
            (type (first type)))
        (multiple-value-bind (key old-name old-type)
            (select-type type type type)
          (declare (ignore old-name))
          (case key
            (string     (cg-ff-string arg arglist-queue))
            (old-handle (cg-ff-handle-old arg arglist-queue))
            (primitive  (cg-ff-primitive old-type arg arglist-queue))
            (old-struct (cg-ff-struct-old arg arglist-queue))
            (old-union  (cg-ff-union-old arg arglist-queue))
            (old-array  (cg-ff-array-old arg arglist-queue))
            (ptr        (cg-ff-ptr old-type arg arglist-queue))
            
            (otherwise  (cg-ff-error 'cg-ffi-args old-type))))))

    (setq *stack-top* old-stack)
    (values (queue2list arglist-queue))))

;;------------------------------------------------------------------------------
;; erzeugt:
;;   LOAD_C_... (... , <base,+0>);
;;------------------------------------------------------------------------------
(defun C-ForeignCall (fun base arg-list typesymbol)
  (let* ((return-type (?return-type fun))
         (stack (CC-Stack base)))
    (multiple-value-bind (key old-name old-type)
        (select-type return-type return-type return-type)
      (declare (ignore old-name))
      (case key
        (void
         (C-Cmd (CC-ForeignCallArgs fun arg-list))
         (C-nil stack))

        (string
         (C-MacroCall "LOAD_C_STRING"
                      (CC-ForeignCallArgs fun arg-list)
                      stack))
        (primitive
         (C-MacroCall (concatenate 'string "LOAD_" (c-macro-string old-type))
                      (CC-ForeignCallArgs fun arg-list)
                      stack))

        (old-array
         (C-Call "_make_c_array_ptr"
                 stack
                 (CC-Symbol typesymbol)
                 (CC-Cast "char *" (CC-ForeignCallArgs fun arg-list))))

        (old-handle
         (C-Call "_make_c_handle"
                 stack
                 (CC-Symbol typesymbol)
                 (CC-Cast "char *" (CC-ForeignCallArgs fun arg-list))))

        (old-struct
         (let* ((c-type (convert-c-type-to-string return-type)))
           (C-blockstart)
           (C-VarDeclInit (format nil "~A *" c-type) "__ptr"
                          (format nil "malloc(sizeof(~A))" c-type))
           (C-Call "_make_c_struct_ptr"
                 stack
                 (CC-Symbol typesymbol)
                 "(char *)__ptr")
           (C-Cmd (format nil "*__ptr = ~A" (CC-ForeignCallArgs fun arg-list)))
           (C-blockend)))

        (old-union
         (let* ((c-type (convert-c-type-to-string return-type)))
           (C-blockstart)
           (C-VarDeclInit (format nil "~A *" c-type) "__ptr"
                          (format nil "malloc(sizeof(~A))" c-type))
           (C-Call "_make_c_union_ptr"
                 stack
                 (CC-Symbol typesymbol)
                 "(char *)__ptr")
           (C-Cmd (format nil "*__ptr = ~A" (CC-ForeignCallArgs fun arg-list)))
           (C-blockend)))

        (old-array
         (let* ((c-type (convert-c-type-to-string return-type)))
           (C-blockstart)
           (C-VarDeclInit (format nil "~A *" c-type) "__ptr"
                          (format nil "malloc(sizeof(~A))" c-type))
           (C-Call "_make_c_array_ptr"
                 stack
                 (CC-Symbol typesymbol)
                 "(char *)__ptr")
           (C-Cmd (format nil "*__ptr = ~A" (CC-ForeignCallArgs fun arg-list)))
           (C-blockend)))

        (ptr
         (let* ((ptr-type (second old-type)))
           (multiple-value-bind (key old-ptr-name old-ptr-type)
               (select-type ptr-type ptr-type ptr-type)
             (declare (ignore old-ptr-type old-ptr-name))
             (case key
               (string
                (C-MacroCall "LOAD_C_STRING"
                             (concatenate 
                              'string "*" (CC-ForeignCallArgs fun arg-list))
                             stack))
               (old-struct
                (C-Call "_make_c_struct_ptr"
                        stack
                        (CC-Symbol typesymbol)
                        (CC-Cast "char *" (CC-ForeignCallArgs fun arg-list))))
               (old-union
                (C-Call "_make_c_union_ptr"
                        stack
                        (CC-Symbol typesymbol)
                        (CC-Cast "char *" (CC-ForeignCallArgs fun arg-list))))
               (old-array
                (C-Call "_make_c_array_ptr"
                        stack
                        (CC-Symbol typesymbol)
                        (CC-Cast "char *" 
                                  (CC-ForeignCallArgs fun arg-list))))
               (otherwise
                (internal-error 
                 'C-ForeignCall "FATAL ERROR: unknown Pointer-type: ~A"
                 return-type))))))

        (otherwise
         (internal-error 'C-ForeignCall "FATAL ERROR: unknown Pointer-type: ~A"
                         return-type))))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun CC-ForeignCallArgs (fun arg-list)
  (apply #'CC-Call (?name fun) arg-list))

;;******************************************************************************
;; Hilfsfunktionen
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Fehlermeldung
;;------------------------------------------------------------------------------
(defun cg-ff-error (where type)
  (internal-error 
   where "FATAL ERROR: Unknown Type (~A)" type))
  
;;******************************************************************************
;; Routinen zur Erzeugung von Funktionsargumenten
;;******************************************************************************
;;------------------------------------------------------------------------------
;; Fuer Strings
;;------------------------------------------------------------------------------
(defun cg-ff-string (arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*)))
    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall "GET_C_STRING" (cc-dest (?var arg))) queue))

      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall "GET_C_STRING" stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))

;;------------------------------------------------------------------------------
;; Primitive Typen
;;------------------------------------------------------------------------------
(defun cg-ff-primitive (type arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*))
         (macro (format nil "GET_~A" (c-macro-string type))))
    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall macro (cc-dest (?var arg))) queue))
      
      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall macro stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))

;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun cg-ff-handle-old (arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*)))

    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall "GET_C_HANDLE" (cc-dest (?var arg))) queue))
      
      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall "GET_C_HANDLE" stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))
    
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun cg-ff-struct-old (arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*)))

    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall "GET_C_STRUCT" (cc-dest (?var arg))) queue))
      
      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall "GET_C_STRUCT" stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))
  
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun cg-ff-union-old (arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*)))

    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall "GET_C_UNION" (cc-dest (?var arg))) queue))
      
      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall "GET_C_UNION" stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))
  
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun cg-ff-array-old (arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*)))

    (typecase arg
      (var-ref 
       (add-q (CC-MacroCall "GET_C_ARRAY" (cc-dest (?var arg))) queue))
      
      (otherwise
       (cg-form arg)
       (add-q (CC-MacroCall "GET_C_ARRAY" stack) queue)
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))
  
;;------------------------------------------------------------------------------
;; 
;;------------------------------------------------------------------------------
(defun cg-ff-ptr (type arg queue)
  (let* ((*result-spec* (stacktop-result-location))
         (stack (CC-Stack *stack-top*))
         (c-type (convert-c-type-to-string type)))

    (typecase arg
      (var-ref
       (multiple-value-bind (key old-name old-type)
           (select-type (second type) (second type) (second type))
         (declare (ignore old-name old-type))
         (case key
           (old-struct
            (add-q
             (CC-Cast c-type
                      (CC-MacroCall "GET_C_STRUCT_PTR" (cc-dest (?var arg))))
             queue))
           
           (old-union
            (add-q 
             (CC-Cast c-type
                      (CC-MacroCall "GET_C_UNION_PTR" (cc-dest (?var arg))))
             queue))
           
           (old-array
            (add-q 
             (CC-Cast
              c-type
              (CC-MacroCall "GET_C_ARRAY_PTR" (cc-dest (?var arg))))
             queue))
           
           (otherwise
            (internal-error 'cg-ff-ptr
                            "Illegal Pointer Type: ~A" type)))))
      
      (otherwise
       (cg-form arg)
       (multiple-value-bind (key old-name old-type)
           (select-type (second type) (second type) (second type))
         (declare (ignore old-name old-type))
         (case key
           (old-struct
            (add-q 
             (CC-Cast
              c-type
              (CC-MacroCall "GET_C_STRUCT_PTR" stack))
             queue))
           
           (old-union
            (add-q 
             (CC-Cast
              c-type
              (CC-MacroCall "GET_C_UNION_PTR" stack))
             queue))
           
           (old-array
            (add-q
             (CC-Cast
              c-type
              (CC-MacroCall "GET_C_ARRAY_PTR" stack))
             queue))
           
           (otherwise
            (internal-error 'cg-ff-ptr
                            "Illegal Pointer Type: ~A" type))))
       (incf *stack-top*)
       (incf (?offset *result-spec*))))))

;;******************************************************************************
;; Call-In
;;******************************************************************************
;;------------------------------------------------------------------------------
;; cg-call-in-interface 
;;------------------------------------------------------------------------------
(defun cg-call-in-interface (fun)
  #+CMU17(declare (notinline C-blockend)) ; by-pass a bug
  (let* ((fun-name (?foreign-name (?call-in fun)))
         (gen-fun-name (?adr fun))
         (return-type (?return-type (?call-in fun)))
         (arguments (?arguments (?call-in fun))))
    
    (labels
        ((make-type ()
           (C-Out (convert-c-type-to-string return-type) " "))
         
         (make-fun-name ()
           (C-Out fun-name))
         
         (make-ansi-or-kr-params ()
           (let* ((laenge (length arguments)))
             (when (> laenge 0)
               (cond
                 (*ANSI-C*
                  (dotimes (i (- laenge 1))
                    (C-Out (convert-c-type-to-string (nth i arguments))
                           (format nil " arg~A, " i)))
                  (C-Out (convert-c-type-to-string (car (last arguments)))
                         (format nil " arg~A" (- laenge 1))))
                 (t
                  (dotimes (i (- laenge 1))
                    (C-Out (format nil "arg~A, " i)))
                  (C-Out (format nil "arg~A" (- laenge 1))))))))
         
         (make-kr-spec ()
           (unless *ANSI-C*
             (let* ((laenge (length arguments)))
               (when (> laenge 0)
                 (dotimes (i laenge)
                   (C-Ln (convert-c-type-to-string (nth i arguments))
                         (format nil " arg~A;" i)))))))
         
         (make-args ()
           (let* ((count 0))
             (dolist (arg arguments)
               (make-one-arg arg count)
               (incf count))))
         
         (make-one-arg (type count)
           (multiple-value-bind (key old-name old-type)
               (select-type type type type)
             (case key
               (string
                (C-MacroCall "LOAD_C_STRING"
                              (format nil "arg~A" count)
                              (CC-Stack count)))
               
               (primitive
                (C-MacroCall 
                 (concatenate 'string "LOAD_" (c-macro-string type)) 
                 (format nil "arg~A" count)
                 (CC-Stack count)))
               
               (void
                ;; Do nothing!
                ;;------------
                )
               
               (old-struct
                (let* ((typesymbol old-name))
                  (C-Call 
                   "_make_c_struct_ptr"
                   (CC-Stack count)
                   (CC-Symbol typesymbol)
                   (format nil "(char *)&arg~A" count))))
               
               (old-union
                (let* ((typesymbol old-name))
                  (C-Call 
                   "_make_c_union_ptr"
                   (CC-Stack count)
                   (CC-Symbol typesymbol)
                   (format nil "(char *)&arg~A" count))))
               
               (old-array
                (let* ((typesymbol old-name))
                  (C-Call 
                   "_make_c_union_ptr"
                   (CC-Stack count)
                   (CC-Symbol typesymbol)
                   (format nil "(char *)&arg~A" count))))
               
               (old-handle
                (let* ((typesymbol old-name))
                  (C-Call
                   "_make_c_handle"
                   (CC-Stack count)
                   (CC-Symbol typesymbol)
                   (format nil "(char *)arg~A" count))))
               
               (ptr
                (let* ((ptr-type (second old-type))
                       (typesymbol old-name))
                  (multiple-value-bind (key old-ptr-name old-ptr-type)
                      (select-type ptr-type ptr-type ptr-type)
                    (declare (ignore old-ptr-name old-ptr-type))
                    (case key
                      (old-struct
                       (C-Call 
                        "_make_c_struct_ptr"
                        (CC-Stack count)
                        (CC-Symbol typesymbol)
                        (format nil "(char *)arg~A" count)))
                      
                      (old-union
                       (C-Call 
                        "_make_c_union_ptr"
                        (CC-Stack count)
                        (CC-Symbol typesymbol)
                        (format nil "(char *)arg~A" count)))
                    
                      (old-array
                       (C-Call 
                        "_make_c_union_ptr"
                        (CC-Stack count)
                        (CC-Symbol typesymbol)
                        (format nil "(char *)arg~A" count)))))))
               
               ((new-struct new-union new-array new-handle)
                ;; *** to do ***
                )
               
               (t 
                (internal-error 'C-ForeignCall
                                NOT-IMPLEMENTED-YET type)))))
             
         (make-call ()
           (if (eq (first arguments) 'ffi:c-void)
               (C-Call gen-fun-name)
               (C-Lispcall gen-fun-name (CC-Stack 0) nil)))
         
         (make-return-value (type)
           (multiple-value-bind (key old-name old-type)
               (select-type type type type)
             (declare (ignore old-name))
             (case key
               (primitive
                (C-Call
                 "return"                
                 (CC-MacroCall 
                  (concatenate 'string "GET_" (c-macro-string type))
                  (CC-Stack 0))))
               
               (string
                (C-Call "return" (CC-MacroCall "GET_C_STRING" (CC-Stack 0))))
               
               (old-struct
                (C-Call "return" (CC-MacroCall "GET_C_STRUCT" (CC-Stack 0))))
               
               (old-union
                (C-Call "return" (CC-MacroCall "GET_C_UNION" (CC-Stack 0))))
               
               (old-array
                (C-Call "return" (CC-MacroCall "GET_C_ARRAY" (CC-Stack 0))))
               
               (old-handle
                (C-Call "return" (CC-MacroCall "GET_C_HANDLE" (CC-Stack 0))))
               
               (ptr
                (let* ((ptr-type (second old-type)))
                  (multiple-value-bind (key old-ptr-name old-ptr-type)
                      (select-type ptr-type ptr-type ptr-type)
                    (declare (ignore old-ptr-name old-ptr-type))
                    (case key
                      (old-struct
                       (C-Call "return" 
                               (CC-MacroCall "GET_C_STRUCT_PTR" (CC-Stack 0))))
                      
                      (old-union
                       (C-Call "return"
                               (CC-MacroCall "GET_C_UNION_PTR" (CC-Stack 0))))
                      
                      (old-array
                       (C-Call "return" 
                               (CC-MacroCall "GET_C_ARRAY_PTR"
                                             (CC-Stack 0))))))))
               
               (void
                ;; do nothing!
                ;;------------
                )
               
               ((new-struct new-union new-array new-handle)
                ;; *** to do ***
                )
               
               (t 
                (internal-error 'C-ForeignCall
                                NOT-IMPLEMENTED-YET type))))))
      
      (C-newline)
      (make-type)
      (make-fun-name)
      (C-Out "(")
      (make-ansi-or-kr-params)
      (C-Out ")")
      (C-newline)
      (make-kr-spec)
      (C-blockstart)
      (C-VarDeclInit "CL_FORM *" "base" "save_stack")
      (C-Newline)
      (make-args)
      (make-call)
      (make-return-value return-type)
      (C-blockend))))

;;------------------------------------------------------------------------------
(provide "cgforeign")
