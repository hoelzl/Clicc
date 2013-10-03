;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Systemunabhaengige Pathname Funktionen
;;;
;;; $Revision: 1.10 $
;;; $Log: pathname.lisp,v $
;;; Revision 1.10  1994/06/02  13:15:53  hk
;;; Print-Funktion f"ur pathname-Struktur definiert.
;;;
;;; Revision 1.9  1994/05/05  14:15:31  hk
;;; Constructor f"ur pattern wieder mit BOA versehen.
;;;
;;; Revision 1.8  1993/07/06  10:40:02  hk
;;; Queue Funktionen nach seq.lisp
;;;
;;; Revision 1.7  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.6  1993/06/05  19:26:34  hk
;;; Benutzung der Funktion rt::file-name eingeschaltet.
;;;
;;; Revision 1.5  1993/05/07  08:59:26  hk
;;; pathname exportiert.
;;;
;;; Revision 1.4  1993/04/22  10:48:21  hk
;;; (in-package "RUNTIME") -> (in-package "LISP"),
;;; Definitionen exportiert, defvar, defconstant, defmacro aus
;;; clicc/lib/lisp.lisp einkopiert. rt::set-xxx in (setf xxx) umgeschrieben.
;;; Definitionen und Anwendungen von/aus Package Runtime mit rt: gekennzeichnet.
;;; declaim fun-spec und declaim top-level-form gestrichen.
;;;
;;; Revision 1.3  1993/02/16  14:34:20  hk
;;; clicc::declaim -> declaim, clicc::fun-spec (etc.) -> lisp::fun-spec (etc.)
;;; $Revision: 1.10 $ eingefuegt
;;;
;;; Revision 1.2  1993/01/20  11:09:03  uho
;;; Aufrufe von %make-pathname mit keywords versehen.
;;;
;;; Revision 1.1  1993/01/19  13:18:14  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export
 '(*default-pathname-defaults*
   pathname pathnamep parse-namestring pathname merge-pathnames make-pathname
   pathname-host pathname-device pathname-directory pathname-name pathname-type
   pathname-version namestring host-namestring directory-namestring
   file-namestring enough-namestring))

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;;;
;;; **********************************************************************
;;;
;;; Machine/filesystem independent pathname functions for CMU Common Lisp.
;;;
;;; Written by William Lott
;;; Earlier version written by Jim Large and Rob MacLachlan
;;;
;;; **********************************************************************

(defvar *default-pathname-defaults*)
(defun required-argument () nil)

;;;; Patterns
(defstruct (pattern
	    (:constructor make-pattern (pieces))
            (:copier nil))
  (pieces nil :type list))

(defun pattern= (pattern1 pattern2)
  (declare (type pattern pattern1 pattern2))
  (let ((pieces1 (pattern-pieces pattern1))
	(pieces2 (pattern-pieces pattern2)))
    (and (= (length pieces1) (length pieces2))
	 (every #'(lambda (piece1 piece2)
		    (typecase piece1
		      (simple-string
		       (and (simple-string-p piece2)
			    (string= piece1 piece2)))
		      (cons
		       (and (consp piece2)
			    (eq (car piece1) (car piece2))
			    (string= (cdr piece1) (cdr piece2))))
		      (t
		       (eq piece1 piece2))))
		pieces1
		pieces2))))



;;;; Utilities.

(defun compare-component (this that)
  (or (eql this that)
      (typecase this
	(simple-string
	 (and (simple-string-p that)
	      (string= this that)))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (compare-component (car this) (car that))
	      (compare-component (cdr this) (cdr that)))))))


;;;; Structures and types.
(defstruct (pathname
             (:copier nil)
             (:conc-name %pathname-)
             (:print-function %print-pathname)
             (:constructor
              %make-pathname #|(host device directory name type version)|#)
             (:predicate pathnamep)
             ;;	    (:make-load-form-fun :just-dump-it-normally)
             )
  "Pathname is the structure of the file pathname.  It consists of a
   host, a device, a directory, a name, and a type."
  (host nil :type (or host null))
  (device nil :type (member nil :unspecific))
  (directory nil :type list)
  (name nil :type (or simple-string pattern null))
  (type nil :type (or simple-string pattern null (member :unspecific)))
  (version nil :type (or integer null (member :newest :wild))))

(defun %print-pathname (pathname stream depth)
  (declare (ignore depth))
     (write-string "#P\"" stream)
     (write-string (namestring pathname) stream)
     (write-char #\" stream))

(defstruct (host
             (:copier nil)
             #|(:print-function %print-host)|#)
  (parse (required-argument) :type function)
  (unparse (required-argument) :type function)
  (unparse-host (required-argument) :type function)
  (unparse-directory (required-argument) :type function)
  (unparse-file (required-argument) :type function)
  (unparse-enough (required-argument) :type function)
  (customary-case (required-argument) :type (member :upper :lower)))


;;;; Pathname functions.

(defun pathname= (pathname1 pathname2)
  (and (eq (%pathname-host pathname1)
	   (%pathname-host pathname2))
       (compare-component (%pathname-device pathname1)
			  (%pathname-device pathname2))
       (compare-component (%pathname-directory pathname1)
			  (%pathname-directory pathname2))
       (compare-component (%pathname-name pathname1)
			  (%pathname-name pathname2))
       (compare-component (%pathname-type pathname1)
			  (%pathname-type pathname2))
       (compare-component (%pathname-version pathname1)
			  (%pathname-version pathname2))))

(defmacro with-pathname ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (etypecase ,var
		   (pathname ,var)
		   (string (parse-namestring ,var))
		   (stream (parse-namestring (rt::file-name ,var)))
                   ))))
    ,@body))

(defun %parse-namestring (namestr start end host junk-allowed)
  (declare (type string namestr)
	   (type index start end)
	   (type host host))
  (cond (junk-allowed
         (warn ":junk-allowed will be ignored")
;;	 (handler-case (%parse-namestring namestr start end host nil)
;;	   (namestring-parse-error (condition)
;;	     (values nil
;;		     (namestring-parse-error-offset condition))))
         )
	((simple-string-p namestr)
	 (multiple-value-bind
	     (new-host device directory file type version)
	     (funcall (host-parse host) namestr start end)
	   (values (%make-pathname :host (or new-host host)
				   :device device
                                   :directory directory
                                   :name file
                                   :type type
                                   :version version)
		   end)))
	(t
	 (%parse-namestring (coerce namestr 'simple-string)
			    start end host nil))))

(defun parse-namestring (thing
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  (declare (type pathnamelike thing)
	   (type (or null host) host)
	   (type pathnamelike defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or null (not null)) junk-allowed))
  (if (stringp thing)
      (%parse-namestring thing start (or end (length thing))
			 (or host
			     (with-pathname (defaults defaults)
			       (%pathname-host defaults)))
			 junk-allowed)
      (with-pathname (pathname thing)
	(when host
	  (unless (eq host (%pathname-host pathname))
	    (error "Hosts do not match: ~S and ~S."
		   host
		   (%pathname-host pathname))))
	(values pathname start))))

(defun pathname (thing)
  (declare (type pathnamelike thing))
  (with-pathname (pathname thing)
    pathname))

(defun maybe-diddle-case (thing diddle-p)
  (declare (type (or list pattern simple-string (member :unspecific))
		 thing))
  (if diddle-p
      (labels ((check-for (pred in)
		 (etypecase in
		   (pattern
		    (dolist (piece (pattern-pieces in))
		      (when (typecase piece
			      (simple-string
			       (check-for pred piece))
			      (cons
			       (case (car in)
				 (:character-set
				  (check-for pred (cdr in))))))
			(return t))))
		   (list
		    (dolist (x in)
		      (when (check-for pred x)
			(return t))))
		   (simple-string
		    (dotimes (i (length in))
		      (when (funcall pred (schar in i))
			(return t))))
		   ((member :unspecific)
		    nil)))
	       (diddle-with (fun thing)
		 (etypecase thing
		   (pattern
		    (make-pattern
		     (mapcar #'(lambda (piece)
				 (typecase piece
				   (simple-string
				    (funcall fun thing))
				   (cons
				    (case (car piece)
				      (:character-set
				       (cons :character-set
					     (funcall fun (cdr piece))))
				      (t
				       piece)))
				   (t
				    piece)))
			     (pattern-pieces thing))))
		   (list
		    (mapcar fun thing))
		   (simple-string
		    (funcall fun thing)))))
	(let ((any-uppers (check-for #'upper-case-p thing))
	      (any-lowers (check-for #'lower-case-p thing)))
	  (cond ((and any-uppers any-lowers)
		 ;; Mixed case, stays the same.
		 thing)
	    (any-uppers
	     ;; All uppercase, becomes all lower case.
	     (diddle-with #'string-downcase thing))
	    (any-lowers
	     ;; All lowercase, becomes all upper case.
	     (diddle-with #'string-upcase thing))
	    (t
	     ;; No letters?  I guess just leave it.
	     thing))))
      thing))

(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
	  (null dir2))
      dir1
      (let ((results nil))
	(flet ((add (dir)
		 (if (and (eq dir :back)
			  results
			  (not (eq (car results) :back)))
		     (pop results)
		     (push dir results))))
	  (dolist (dir (maybe-diddle-case dir2 diddle-case))
	    (add dir))
	  (dolist (dir (cdr dir1))
	    (add dir)))
	(reverse results))))

(defun merge-pathnames (pathname
			&optional
			(defaults *default-pathname-defaults*)
			(default-version :newest))
  (with-pathname (defaults defaults)
    (let ((pathname (let ((*default-pathname-defaults* defaults))
		      (pathname pathname))))
      (let* ((default-host (%pathname-host defaults))
	     (pathname-host (%pathname-host pathname))
	     (diddle-case
	      (and default-host pathname-host
		   (not (eq (host-customary-case default-host)
			    (host-customary-case pathname-host))))))
	(%make-pathname
         :host (or pathname-host default-host)
         :device (or (%pathname-device pathname)
                     (maybe-diddle-case (%pathname-device defaults)
                                        diddle-case))
         :directory (merge-directories (%pathname-directory pathname)
                                      (%pathname-directory defaults)
                                      diddle-case)
	 :name		(or (%pathname-name pathname)
			    (maybe-diddle-case (%pathname-name defaults)
					       diddle-case))
	 :type		(or (%pathname-type pathname)
			    (maybe-diddle-case (%pathname-type defaults)
					       diddle-case))
	 :version	(or (%pathname-version pathname)
			    default-version))))))

(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    (list
     (let ((results (empty-queue)))
       (ecase (pop directory)
	 (:absolute
	  (add-q :absolute results)
;;	  (when (search-list-p (car directory))
;;	    (add-q (pop directory) results))
          )
	 (:relative
	  (add-q :relative results)))
       (dolist (piece directory)
	 (cond ((eq piece :wild)
		(add-q (make-pattern (list :multi-char-wild)) results))
	       ((eq piece :wild-inferiors)
		(error ":WILD-INFERIORS not supported."))
	       ((member piece '(:up :back))
		(add-q piece results))
	       ((or (simple-string-p piece) (pattern-p piece))
		(add-q (maybe-diddle-case piece diddle-case) results))
	       ((stringp piece)
		(add-q (maybe-diddle-case (coerce piece 'simple-string)
                                          diddle-case)
                       results))
	       (t
		(error "~S is not allowed as a directory component." piece))))
       (queue2list results)))
    (simple-string
     `(:absolute
       ,(maybe-diddle-case directory diddle-case)))
    (string
     `(:absolute
       ,(maybe-diddle-case (coerce directory 'simple-string)
			   diddle-case)))))

(defun make-pathname (&key (host nil hostp)
			   (device nil devp)
			   (directory nil dirp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults (case :local))
  (let* ((defaults (if defaults
		       (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 (host (if hostp host default-host))
	 (diddle-args (and (eq case :common)
			   (eq (host-customary-case host) :lower)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host)))))
    (macrolet ((pick (var varp field)
		 `(cond ((eq ,var :wild)
			 (make-pattern (list :multi-char-wild)))
			((or (simple-string-p ,var)
			     (pattern-p ,var))
			 (maybe-diddle-case ,var diddle-args))
			((stringp ,var)
			 (maybe-diddle-case (coerce ,var 'simple-string)
					    diddle-args))
			(,varp
			 (maybe-diddle-case ,var diddle-args))
			(defaults
			 (maybe-diddle-case (,field defaults)
					    diddle-defaults))
			(t
			 nil))))
      (%make-pathname
       :host host
       :device (if devp device (if defaults (%pathname-device defaults)))
       :directory (let ((dir (import-directory directory diddle-args)))
                    (if (and defaults (not dirp))
                        (merge-directories dir
                                           (%pathname-directory defaults)
                                           diddle-defaults)
                        dir))
       :name (pick name namep %pathname-name)
       :type (pick type typep %pathname-type)
       :version (cond
                  (versionp version)
                  (defaults (%pathname-version defaults))
                  (t nil))))))

(defun pathname-host (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

(defun pathname-device (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-device pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-directory (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-directory pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-name (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-name pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-type (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-type pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-version (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))

(defun namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun host-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-host host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun directory-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-directory host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun file-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-file host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun enough-namestring (pathname
			  &optional (defaults *default-pathname-defaults*))
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (with-pathname (defaults defaults)
	    (funcall (host-unparse-enough host) pathname defaults))
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))


;;;; Wild pathnames.

(defun wild-pathname-p (pathname &optional field-key)
  (declare (type pathnamelike pathname)
	   (type (member nil :host :device :directory :name :type :version)
		 field-key))
  (with-pathname (pathname pathname)
    (ecase field-key
      ((nil)
       (or (wild-pathname-p pathname :host)
	   (wild-pathname-p pathname :device)
	   (wild-pathname-p pathname :directory)
	   (wild-pathname-p pathname :name)
	   (wild-pathname-p pathname :type)
	   (wild-pathname-p pathname :version)))
      (:host
       (pattern-p (%pathname-host pathname)))
      (:device
       (pattern-p (%pathname-host pathname)))
      (:directory
       (some #'pattern-p (%pathname-directory pathname)))
      (:name
       (pattern-p (%pathname-name pathname)))
      (:type
       (pattern-p (%pathname-type pathname)))
      (:version
       (eq (%pathname-version pathname) :wild)))))

(defun pattern-matches (pattern string)
  (declare (type pattern pattern)
	   (type simple-string string))
  (let ((len (length string)))
    (labels ((maybe-prepend (subs cur-sub chars)
	       (if cur-sub
		   (let* ((len (length chars))
			  (new (make-string len))
			  (index len))
		     (dolist (char chars)
		       (setf (schar new (decf index)) char))
		     (cons new subs))
		   subs))
	     (matches (pieces start subs cur-sub chars)
	       (if (null pieces)
		   (if (= start len)
		       (values t (maybe-prepend subs cur-sub chars))
		       (values nil nil))
		   (let ((piece (car pieces)))
		     (etypecase piece
		       (simple-string
			(let ((end (+ start (length piece))))
			  (and (<= end len)
			       (string= piece string
					:start2 start :end2 end)
			       (matches (cdr pieces) end
					(maybe-prepend subs cur-sub chars)
					nil nil))))
		       (list
			(ecase (car piece)
			  (:character-set
			   (and (< start len)
				(let ((char (schar string start)))
				  (if (find char (cdr piece) :test #'char=)
				      (matches (cdr pieces) (1+ start) subs t
					       (cons char chars))))))))
		       ((member :single-char-wild)
			(and (< start len)
			     (matches (cdr pieces) (1+ start) subs t
				      (cons (schar string start) chars))))
		       ((member :multi-char-wild)
			(multiple-value-bind
			    (won new-subs)
			    (matches (cdr pieces) start subs t chars)
			  (if won
			      (values t new-subs)
			      (and (< start len)
				   (matches pieces (1+ start) subs t
					    (cons (schar string start)
						  chars)))))))))))
      (multiple-value-bind
	  (won subs)
	  (matches (pattern-pieces pattern) 0 nil nil nil)
	(values won (reverse subs))))))

(defun components-match (this that)
  (or (eq this that)
      (typecase this
	(simple-string
	 (typecase that
	   (pattern
	    (values (pattern-matches that this)))
	   (simple-string
	    (string= this that))))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (components-match (car this) (car that))
	      (components-match (cdr this) (cdr that))))
	((member :back :up :unspecific nil)
	 (and (pattern-p that)
	      (equal (pattern-pieces that) '(:multi-char-wild)))))))

(defun pathname-match-p (pathname wildname)
  (with-pathname (pathname pathname)
    (with-pathname (wildname wildname)
      (macrolet ((frob (field)
		   `(or (null (,field wildname))
			(components-match (,field pathname)
					  (,field wildname)))))
	(and (frob %pathname-host)
	     (frob %pathname-device)
	     (frob %pathname-directory)
	     (frob %pathname-name)
	     (frob %pathname-type)
	     (or (null (%pathname-version wildname))
	       (eq (%pathname-version wildname) :wild)
	       (eql (%pathname-version pathname)
		    (%pathname-version wildname))))))))

(defun substitute-into (pattern subs)
  (declare (type pattern pattern)
	   (type list subs))
  (let ((in-wildcard nil)
	(pieces nil)
	(strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
	     (push piece strings)
	     (setf in-wildcard nil))
	    (in-wildcard)
	    ((null subs))
	    (t
	     (let ((sub (pop subs)))
	       (etypecase sub
		 (pattern
		  (when strings
		    (push (apply #'concatenate 'simple-string
				 (nreverse strings))
			  pieces))
		  (dolist (piece (pattern-pieces sub))
		    (push piece pieces)))
		 (simple-string
		  (push sub strings))))
	     (setf in-wildcard t))))
    (when strings
      (push (apply #'concatenate 'simple-string
		   (nreverse strings))
	    pieces))
    (if (and pieces
	     (simple-string-p (car pieces))
	     (null (cdr pieces)))
	(car pieces)
	(make-pattern (nreverse pieces)))))

(defun translate-component (source from to)
  (typecase to
    (pattern
     (if (pattern-p from)
	 (typecase source
	   (pattern
	    (if (pattern= from source)
		source
		:error))
	   (simple-string
	    (multiple-value-bind
		(won subs)
		(pattern-matches from source)
	      (if won
		  (values (substitute-into to subs))
		  :error)))
	   (t
	    :error))
	 source))
    ((member nil :wild)
     source)
    (t
     (if (components-match source from)
	 to
	 :error))))
