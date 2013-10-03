;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Inhalt   : Laufzeitfunktionen zur Bearbeitung von UNIX Pathnames
;;;
;;; $Revision: 1.9 $
;;; $Log: filesys.lisp,v $
;;; Revision 1.9  1993/12/09  16:59:54  sma
;;; Parameter der Aufrufe von error korrigiert. rt::shrink-vector durch
;;; shrink-simple-string ersetzt.
;;;
;;; Revision 1.8  1993/06/16  15:20:38  hk
;;;  Copyright Notiz eingefuegt.
;;;
;;; Revision 1.7  1993/06/05  19:26:57  hk
;;; Funktion file-name ins Package RT.
;;;
;;; Revision 1.6  1993/05/13  13:24:53  hk
;;; rt:: vor unix-current-directory.
;;;
;;; Revision 1.5  1993/05/03  14:35:47  hk
;;; declaim top-level-form gestrichen.
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
;;; $Revision: 1.9 $ eingefuegt
;;;
;;; Revision 1.2  1993/01/19  16:46:25  uho
;;; Beim Aufruf von %make-pathname keywords eingefuegt.
;;;
;;; Revision 1.1  1993/01/19  13:17:07  hk
;;; Initial revision
;;;----------------------------------------------------------------------------

(in-package "LISP")

(export '(truename probe-file directory))

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;;;
;;; **********************************************************************
;;;
;;; File system interface functions.  This file is pretty UNIX specific.
;;;
;;; Written by William Lott
;;;
;;; **********************************************************************



(defparameter *ignore-wildcards* nil)
(defparameter *unix-host* (make-host :parse #'parse-unix-namestring
                                     :unparse #'unparse-unix-namestring
                                     :unparse-host #'unparse-unix-host
                                     :unparse-directory #'unparse-unix-directory
                                     :unparse-file #'unparse-unix-file
                                     :unparse-enough #'unparse-unix-enough
                                     :customary-case :lower)
  #|(make-unix-host)|#)
(defconstant s-ifmt 61440)
(defconstant s-ifdir 16384)
(defconstant s-ifreg 32768)
(defconstant s-iflnk 40960)
(defconstant *unix-errors* 
  #("Successful"
    "Not owner"
    "No such file or directory"
    "No such process"
    "Interrupted system call"
    "I/O error"
    "No such device or address"
    "Arg list too long"
    "Exec format error"
    "Bad file number"
    "No children"
    "No more processes"
    "Not enough core"
    "Permission denied"
    "Bad address"
    "Block device required"
    "Mount device busy"
    "File exists"
    "Cross-device link"
    "No such device"
    "Not a director"
    "Is a directory"
    "Invalid argument"
    "File table overflow"
    "Too many open files"
    "Not a typewriter"
    "Text file busy"
    "File too large"
    "No space left on device"
    "Illegal seek"
    "Read-only file system"
    "Too many links"
    "Broken pipe"
    "Argument too large"
    "Result too large"
    "Operation would block"
    "Operation now in progress"
    "Operation already in progress"
    "Socket operation on non-socket"
    "Destination address required"
    "Message too long"
    "Protocol wrong type for socket"
    "Protocol not available"
    "Protocol not supported"
    "Socket type not supported"
    "Operation not supported on socket"
    "Protocol family not supported"
    "Address family not supported by protocol family"
    "Address already in use"
    "Can't assign requested address"
    "Network is down"
    "Network is unreachable"
    "Network dropped connection on reset"
    "Software caused connection abort"
    "Connection reset by peer"
    "No buffer space available"
    "Socket is already connected"
    "Socket is not connected"
    "Can't send after socket shutdown"
    "Too many references: can't splice"
    "Connection timed out"
    "Connection refused"
    "Too many levels of symbolic links"
    "File name too long"
    "Host is down"
    "No route to host"
    "Directory not empty"
    "Too many processes"
    "Too many users"
    "Disc quota exceeded"))


;;;; Unix pathname host support.

;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" | search-list ] { file "/" }*
;;; search-list := [^:/]*:
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := "." ([0-9]+ | "*")
;;;
;;; Note: this grammer is ambiguous.  The string foo.bar.5 can be parsed
;;; as either just the file specified or as specifying the file, type, and
;;; version.  Therefore, we use the following rules when confronted with
;;; an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file.  It is not
;;; considered a dot in the following rules.
;;;
;;; - If there is only one dot, it seperates the file and the type.
;;;
;;; - If there are multiple dots and the stuff following the last dot
;;; is a valid version, then that is the version and the stuff between
;;; the second to last dot and the last dot is the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the following
;;; characters, it is considered part of a wildcard pattern and has the
;;; following meaning.
;;;
;;; ? - matches any character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;
;;; Any of these special characters can be preceeded by a backslash to
;;; cause it to be treated as a regular character.
;;;

(defun remove-backslashes (namestr start end)
  "Remove and occurences of \\ from the string because we've already
   checked for whatever they may have been backslashed."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start)))
	 (dst 0)
	 (quoted nil))
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted nil)
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error "Backslash in bad place in ~S." namestr))
    (shrink-simple-string result dst)))

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (let ((pattern (empty-queue)))
	(let ((quoted nil)
	      (any-quotes nil)
	      (last-regular-char nil)
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (add-q (if any-quotes
                                (remove-backslashes namestr
                                                    last-regular-char
                                                    index)
                                (subseq namestr last-regular-char index))
                            pattern)
		     (setf any-quotes nil)
		     (setf last-regular-char nil))))
	    (loop
             (when (>= index end)
               (return))
             (let ((char (schar namestr index)))
               (cond (quoted
                      (incf index)
                      (setf quoted nil))
                     ((char= char #\\)
                      (setf quoted t)
                      (setf any-quotes t)
                      (unless last-regular-char
                        (setf last-regular-char index))
                      (incf index))
                     ((char= char #\?)
                      (flush-pending-regulars)
                      (add-q :single-char-wild pattern)
                      (incf index))
                     ((char= char #\*)
                      (flush-pending-regulars)
                      (add-q :multi-char-wild pattern)
                      (incf index))
                     ((char= char #\[)
                      (flush-pending-regulars)
                      (let ((close-bracket
                             (position #\] namestr :start index :end end)))
                        (unless close-bracket
                          (error "``['' with no corresponding ``]'' in ~S"
                                 namestr))
                        (add-q (list :character-set
                                     (subseq namestr
                                             (1+ index)
                                             close-bracket))
                               pattern)
                        (setf index (1+ close-bracket))))
                     (t
                      (unless last-regular-char
                        (setf last-regular-char index))
                      (incf index)))))
	    (flush-pending-regulars)))
        (setq pattern (queue2list pattern))
	(cond ((null pattern)
	       "")
	      ((and (null (cdr pattern))
		    (simple-string-p (car pattern)))
	       (car pattern))
	      (t (make-pattern pattern))))))

(defun extract-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
			     :from-end t))
	 (second-to-last-dot (and last-dot
				  (position #\. namestr :start (1+ start)
					    :end last-dot :from-end t)))
	 (version :newest))
    ;; If there is a second-to-last dot, check to see if there is a valid
    ;; version after the last dot.
    (when second-to-last-dot
      (cond ((and (= (+ last-dot 2) end)
		  (char= (schar namestr (1+ last-dot)) #\*))
	     (setf version :wild))
	    ((and (< (1+ last-dot) end)
		  (do ((index (1+ last-dot) (1+ index)))
		      ((= index end) t)
		    (unless (char<= #\0 (schar namestr index) #\9)
		      (return nil))))
	     (setf version
		   (parse-integer namestr :start (1+ last-dot) :end end)))
	    (t
	     (setf second-to-last-dot nil))))
    (cond (second-to-last-dot
	   (values (maybe-make-pattern namestr start second-to-last-dot)
		   (maybe-make-pattern namestr
				       (1+ second-to-last-dot)
				       last-dot)
		   version))
	  (last-dot
	   (values (maybe-make-pattern namestr start last-dot)
		   (maybe-make-pattern namestr (1+ last-dot) end)
		   version))
	  (t
	   (values (maybe-make-pattern namestr start end)
		   nil
		   version)))))

(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash seperated chunks.
    (let ((pieces (empty-queue)))
      (loop
       (let ((slash (position #\/ namestr :start start :end end)))
         (add-q (cons start (or slash end)) pieces)
         (unless slash
           (return))
         (setf start (1+ slash))))
      (values absolute (queue2list pieces)))))

(defun maybe-extract-search-list (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((quoted nil))
    (do ((index start (1+ index)))
	((= index end)
	 (values nil start))
      (if quoted
	  (setf quoted nil)
	  (case (schar namestr index)
	    (#\\
	     (setf quoted t))
	    (#\:
	     (return (values (remove-backslashes namestr start index)
			     (1+ index)))))))))

(defun parse-unix-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (multiple-value-bind
      (absolute pieces)
      (split-at-slashes namestr start end)
    (let ((search-list
	   (if absolute
	       nil
	       (let ((first (car pieces)))
		 (multiple-value-bind
		     (search-list new-start)
		     (maybe-extract-search-list namestr
						(car first) (cdr first))
		   (when search-list
		     (setf absolute t)
		     (setf (car first) new-start))
		   search-list)))))
      (multiple-value-bind
	  (name type version)
	  (let* ((tail (car (last pieces)))
		 (tail-start (car tail))
		 (tail-end (cdr tail)))
	    (unless (= tail-start tail-end)
	      (setf pieces (butlast pieces))
	      (extract-name-type-and-version namestr tail-start tail-end)))
	;; Now we have everything we want.  So return it.
	(values nil ; no host for unix namestrings.
		nil ; no devices for unix namestrings.
		(let ((dirs (empty-queue)))
		  (when search-list
                    (error "search lists not implemented")
		    #|(add-q (intern-search-list search-list) dirs)|#)
		  (dolist (piece pieces)
		    (let ((piece-start (car piece))
			  (piece-end (cdr piece)))
		      (unless (= piece-start piece-end)
			(let ((dir (maybe-make-pattern namestr
						       piece-start
						       piece-end)))
			  (if (and (simple-string-p dir)
				   (string= dir ".."))
			      (add-q :up dirs)
			      (add-q dir dirs))))))
                  (setq dirs (queue2list dirs))
		  (cond (absolute
			 (cons :absolute dirs))
			(dirs
			 (cons :relative dirs))
			(t
			 nil)))
		name
		type
		version)))))

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
	   (ignore pathname))
  "Unix")

(defun unparse-unix-piece (thing)
  (etypecase thing
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   ((#\* #\? #\[)
	    (incf dstlen))))
       (let ((result (make-string dstlen))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       ((#\* #\? #\[)
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (let ((strings (empty-queue)))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (add-q piece strings))
	   (symbol
	    (case piece
	      (:multi-char-wild
	       (add-q "*" strings))
	      (:single-char-wild
	       (add-q "?" strings))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (add-q "[" strings)
	       (add-q (cdr piece) strings)
	       (add-q "]" strings))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (queue2list strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (let ((pieces (empty-queue)))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (cond #|((search-list-p (car directory))
		(add-q (search-list-name (pop directory)) pieces)
		(add-q ":" pieces))|#
	       (t
		(add-q "/" pieces))))
	(:relative
	 ;; Nothing special.
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (add-q "../" pieces))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((or simple-string pattern)
	   (add-q (unparse-unix-piece dir) pieces)
	   (add-q "/" pieces))
	  (t
	   (error "Invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (queue2list pieces))))

(defun unparse-unix-directory (pathname)
  (declare (type pathname pathname))
  (unparse-unix-directory-list (%pathname-directory pathname)))
  
(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (let ((strings (empty-queue)))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific))))
	   (version (%pathname-version pathname))
	   (version-supplied (not (or (null version) (eq version :newest)))))
      (when name
	(add-q (unparse-unix-piece name) strings))
      (when type-supplied
	(unless name
	  (error "Cannot specify the type without a file: ~S" pathname))
	(add-q "." strings)
	(add-q (unparse-unix-piece type) strings))
      (when version-supplied
	(unless type-supplied
	  (error "Cannot specify the version without a type: ~S" pathname))
	(add-q (if (eq version :wild)
                   ".*"
                   (format nil ".~D" version))
               strings)))
    (apply #'concatenate 'simple-string (queue2list strings))))

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory pathname)
	       (unparse-unix-file pathname)))

(defun unparse-unix-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
	   (error "~S cannot be represented relative to ~S"
		  pathname defaults)))
    (let ((strings (empty-queue)))
      (let* ((pathname-directory (%pathname-directory pathname))
	     (defaults-directory (%pathname-directory defaults))
	     (prefix-len (length defaults-directory))
	     (result-dir
	      (cond ((and (> prefix-len 1)
			  (>= (length pathname-directory) prefix-len)
			  (compare-component (subseq pathname-directory
						     0 prefix-len)
					     defaults-directory))
		     ;; Pathname starts with a prefix of default.  So just
		     ;; use a relative directory from then on out.
		     (cons :relative (nthcdr prefix-len pathname-directory)))
		    ((eq (car pathname-directory) :absolute)
		     ;; We are an absolute pathname, so we can just use it.
		     pathname-directory)
		    (t
		     ;; We are a relative directory.  So we lose.
		     (lose)))))
	(add-q (unparse-unix-directory-list result-dir) strings))
      (let* ((pathname-version (%pathname-version pathname))
	     (version-needed (and pathname-version
				  (not (eq pathname-version :newest))))
	     (pathname-type (%pathname-type pathname))
	     (type-needed (or version-needed
			      (and pathname-type
				   (not (eq pathname-type :unspecific)))))
	     (pathname-name (%pathname-name pathname))
	     (name-needed (or type-needed
			      (and pathname-name
				   (not (compare-component pathname-name
							   (%pathname-name
							    defaults)))))))
	(when name-needed
	  (unless pathname-name (lose))
	  (add-q (unparse-unix-piece pathname-name) strings))
	(when type-needed
	  (when (or (null pathname-type) (eq pathname-type :unspecific))
	    (lose))
	  (add-q "." strings)
	  (add-q (unparse-unix-piece pathname-type) strings))
	(when version-needed
	  (typecase pathname-version
	    ((member :wild)
	     (add-q ".*" strings))
	    (integer
	     (add-q (format nil ".~D" pathname-version) strings))
	    (t
	     (lose)))))
      (apply #'concatenate 'simple-string (queue2list strings)))))

;;;; Wildcard matching stuff.

(defmacro enumerate-matches ((var pathname &optional result
				  &key (verify-existance t))
			     &body body)
  (let ((body-name (gensym)))
    `(block nil
      (flet ((,body-name (,var)
               ,@body))
        (%enumerate-matches (pathname ,pathname)
                            ,verify-existance
                            #',body-name)
        ,result))))

(defun %enumerate-matches (pathname verify-existance function)
  (when (pathname-type pathname)
    (unless (pathname-name pathname)
      (error "Cannot supply a type without a name:~%  ~S" pathname)))
  (when (and (integerp (pathname-version pathname))
	     (member (pathname-type pathname) '(nil :unspecific)))
    (error "Cannot supply a version without a type:~%  ~S" pathname))
  (let ((directory (pathname-directory pathname)))
    (if directory
	(ecase (car directory)
	  (:absolute
	   (%enumerate-directories "/" (cdr directory) pathname
				   verify-existance function))
	  (:relative
	   (%enumerate-directories "" (cdr directory) pathname
				   verify-existance function)))
	(%enumerate-files "" pathname verify-existance function))))

(defun %enumerate-directories (head tail pathname verify-existance function)
  (if tail
      (let ((piece (car tail)))
	(etypecase piece
	  (simple-string
	   (%enumerate-directories (concatenate 'string head piece "/")
				   (cdr tail) pathname verify-existance
				   function))
#|	  (pattern
	   (let ((dir (unix-open-dir head)))
	     (when dir
	       (unwind-protect
		   (loop
		     (let ((name (unix-read-dir dir)))
		       (cond ((null name)
			      (return))
			     ((string= name "."))
			     ((string= name ".."))
			     ((pattern-matches piece name)
			      (let ((subdir (concatenate 'string
							 head name "/")))
				(when (eq (unix-file-kind subdir)
					  :directory)
				  (%enumerate-directories
				   subdir (cdr tail) pathname verify-existance
				   function)))))))
		 (unix-close-dir dir)))))
|#
	  ((member :up)
	   (%enumerate-directories (concatenate 'string head "../")
				   (cdr tail) pathname verify-existance
				   function))))
      (%enumerate-files head pathname verify-existance function)))

(defun %enumerate-files (directory pathname verify-existance function)
  (let ((name (pathname-name pathname))
	(type (pathname-type pathname))
	(version (pathname-version pathname)))
    (cond ((null name)
	   (when (or (not verify-existance)
		     (unix-file-kind directory))
	     (funcall function directory)))
#|	  ((or (pattern-p name)
	       (pattern-p type)
	       (eq version :wild))
	   (let ((dir (unix-open-dir directory)))
	     (when dir
	       (unwind-protect
		   (loop
		     (let ((file (unix-read-dir dir)))
		       (if file
			   (unless (or (string= file ".")
				       (string= file ".."))
			     (multiple-value-bind
				 (file-name file-type file-version)
				 (let ((*ignore-wildcards* t))
				   (extract-name-type-and-version
				    file 0 (length file)))
			       (when (and (components-match file-name name)
					  (components-match file-type type)
					  (components-match file-version
							    version))
				 (funcall function
					  (concatenate 'string
						       directory
						       file)))))
			   (return))))
		 (unix-close-dir dir)))))
|#
	  (t
	   (let ((file (concatenate 'string directory name)))
	     (unless (or (null type) (eq type :unspecific))
	       (setf file (concatenate 'string file "." type)))
	     (unless (or (null version) (eq version :newest))
	       (setf file (concatenate 'string file "."
				       (quick-integer-to-string version))))
	     (when (or (not verify-existance)
		       (unix-file-kind file))
	       (funcall function file)))))))

(defun quick-integer-to-string (n)
  (declare (type integer n))
  (cond ((zerop n) "0")
	((eql n 1) "1")
	((minusp n)
	 (concatenate 'simple-string "-"
		      (the simple-string (quick-integer-to-string (- n)))))
	(t
	 (do* ((len (1+ (truncate (integer-length n) 3)))
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q n)
	       (r 0))
	      ((zerop q)
	       (incf i)
	       (replace res res :start2 i :end2 len)
	       (shrink-simple-string res (- len i)))
	   (declare (simple-string res)
		    (fixnum len i r))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))


;;;; UNIX-NAMESTRING -- public
;;; 
(defun unix-namestring (pathname &optional (for-input t))
  "Convert PATHNAME into a string that can be used with UNIX system calls.
   Search-lists and wild-cards are expanded."
  (let((pathname pathname))
    (let ((names (empty-queue)))
      (enumerate-matches (name pathname nil :verify-existance for-input)
	(add-q name names))
      (let ((names (queue2list names)))
	(when names
	  (when (cdr names)
	    (error "~S is ambiguous:~{~%  ~A~}" pathname names))
	  (car names))))))


;;;; TRUENAME and PROBE-FILE.

;;; Truename  --  Public
;;;
;;; Another silly file function trivially different from another function.
;;;
(defun truename (pathname)
  "Return the pathname for the actual file described by the pathname
  An error is signalled if no such file exists."
  (let ((result (probe-file pathname)))
    (unless result
      (error "The file ~S does not exist." (namestring pathname)))
    result))

;;; Probe-File  --  Public
;;;
;;; If PATHNAME exists, return it's truename, otherwise NIL.
;;;
(defun probe-file (pathname)
  "Return a pathname which is the truename of the file if it exists, NIL
  otherwise."
  (let ((namestring (unix-namestring pathname t)))
    (when (and namestring (unix-file-kind namestring))
      (let ((truename (unix-resolve-links
		       (unix-maybe-prepend-current-directory
			namestring))))
	(when truename
	  (let ((*ignore-wildcards* t))
	    (pathname (unix-simplify-pathname truename))))))))


;;;; Other random operations.
#|
;;; Rename-File  --  Public
;;;
(defun rename-file (file new-name)
  "Rename File to have the specified New-Name.  If file is a stream open to a
  file, then the associated file is renamed.  If the file does not yet exist
  then the file is created with the New-Name when the stream is closed."
  (let* ((original (truename file))
	 (original-namestring (unix-namestring original t))
	 (new-name (merge-pathnames new-name original))
	 (new-namestring (unix-namestring new-name nil)))
    (unless original-namestring
      (error "~S doesn't exist." file))
    (unless new-namestring
      (error "~S can't be created." new-name))
    (multiple-value-bind (res error)
			 (unix-rename original-namestring
					   new-namestring)
      (unless res
	(error "Failed to rename ~A to ~A: ~A"
	       original new-name (unix-get-unix-error-msg error)))
      (when (streamp file)
	(rt::file-name file new-namestring))
      (values new-name original (truename new-name)))))

;;; Delete-File  --  Public
;;;
;;;    Delete the file, Man.
;;;
(defun delete-file (file)
  "Delete the specified file."
  (let ((namestring (unix-namestring file t)))
    (when (streamp file)
      (close file :abort t))
    (unless namestring
      (error "~S doesn't exist." file))

    (multiple-value-bind (res err) (unix-unlink namestring)
      (unless res
	(error "Could not delete ~A: ~A."
	       namestring
	       (unix-get-unix-error-msg err)))))
  t)
|#

#|
;;; User-Homedir-Pathname  --  Public
;;;
;;;    Return Home:, which is set up for us at initialization time.
;;;
(defun user-homedir-pathname (&optional host)
  "Returns the home directory of the logged in user as a pathname.
  This is obtained from the logical name \"home:\"."
  (declare (ignore host))
  #p"home:")
|#

#|
;;; File-Write-Date  --  Public
;;;
(defun file-write-date (file)
  "Return file's creation date, or NIL if it doesn't exist."
  (let ((name (unix-namestring file t)))
    (when name
      (multiple-value-bind
	  (res dev ino mode nlink uid gid rdev size atime mtime)
	  (unix-stat name)
	(declare (ignore dev ino mode nlink uid gid rdev size atime))
	(when res
	  (+ unix-to-universal-time mtime))))))
|#
#|
;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Returns the file author as a string, or nil if the author cannot be
   determined.  Signals an error if file doesn't exist."
  (let ((name (unix-namestring (pathname file) t)))
    (unless name
      (error "~S doesn't exist." file))
    (multiple-value-bind (winp dev ino mode nlink uid)
			 (unix-stat file)
      (declare (ignore dev ino mode nlink))
      (if winp (lookup-login-name uid)))))
|#


;;;; DIRECTORY.

;;; DIRECTORY  --  public.
;;; 
(defun directory (pathname &key (all t) (check-for-subdirs t)
			   (follow-links t))
  "Returns a list of pathnames, one for each file that matches the given
   pathname.  Supplying :ALL as nil causes this to ignore Unix dot files.  This
   never includes Unix dot and dot-dot in the result.  If :FOLLOW-LINKS is NIL,
   then symblolic links in the result are not expanded.  This is not the
   default because TRUENAME does follow links, and the result pathnames are
   defined to be the TRUENAME of the pathname (the truename of a link may well
   be in another directory.)"
  (let ((results nil))
    (let ((pathname (merge-pathnames pathname
                                     (make-pathname :name :wild
                                                    :type :wild
                                                    :version :wild))))
      (enumerate-matches (name pathname)
	(when (or all
		  (let ((slash (position #\/ name :from-end t)))
		    (or (null slash)
			(= (1+ slash) (length name))
			(char/= (schar name (1+ slash)) #\.))))
	  (push name results))))
    (let ((*ignore-wildcards* t))
      (mapcar #'(lambda (name)
		  (let ((name (if (and check-for-subdirs
				       (eq (unix-file-kind name)
					   :directory))
				  (concatenate 'string name "/")
				  name)))
		    (if follow-links (truename name) (pathname name))))
	      (sort (delete-duplicates results :test #'string=) #'string<)))))

;;;; File completion.

;;; COMPLETE-FILE -- Public
;;;
(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
			       ignore-types)
  (let ((files (directory (complete-file-directory-arg pathname defaults)
			  :check-for-subdirs nil
			  :follow-links nil)))
    (cond ((null files)
	   (values nil nil))
	  ((null (cdr files))
	   (values (merge-pathnames (file-namestring (car files))
				    pathname)
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (simple-string-p
				       (pathname-type pathname))
				      (member (pathname-type pathname)
					      ignore-types
					      :test #'string=)))
			     files)))
	     (cond ((null good-files))
		   ((null (cdr good-files))
		    (return-from complete-file
				 (values (merge-pathnames (file-namestring
							   (car good-files))
							  pathname)
					 t)))
		   (t
		    (setf files good-files)))
	     (let ((common (file-namestring (car files))))
	       (dolist (file (cdr files))
		 (let ((name (file-namestring file)))
		   (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
		     (unless (char= (schar common i) (schar name i))
		       (setf common (subseq common 0 i))
		       (return)))))
	       (values (merge-pathnames common pathname)
		       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory-arg (pathname defaults)
  (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
	 (type (pathname-type pathname)))
    (flet ((append-multi-char-wild (thing)
	     (etypecase thing
	       (null :wild)
	       (pattern
		(make-pattern (append (pattern-pieces thing)
				      (list :multi-char-wild))))
	       (simple-string
		(make-pattern (list thing :multi-char-wild))))))
      (if (or (null type) (eq type :unspecific))
	  ;; There is no type.
	  (make-pathname :defaults pathname
	    :name (append-multi-char-wild (pathname-name pathname))
	    :type :wild)
	  ;; There already is a type, so just extend it.
	  (make-pathname :defaults pathname
	    :name (pathname-name pathname)
	    :type (append-multi-char-wild (pathname-type pathname)))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
			&optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (directory (complete-file-directory-arg pathname defaults)
	     :follow-links nil
	     :check-for-subdirs nil))



;;; Default-Directory  --  Public
;;;
(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (multiple-value-bind (gr dir-or-error)
		       (rt::unix-current-directory)
    (if gr
	(let ((*ignore-wildcards* t))
	  (pathname (concatenate 'simple-string dir-or-error "/")))
	(error dir-or-error))))


;; from unix.lisp

(defun unix-file-kind (name &optional check-for-links)
  "Returns either :file, :directory, :link, :special, or NIL."
  (declare (simple-string name))
  (let ((mode (if check-for-links
                  (rt::unix-link-mode name)
                  (rt::unix-file-mode name))))
    (when mode
      (let ((kind (logand mode s-ifmt)))
        (cond ((eql kind s-ifdir) :directory)
              ((eql kind s-ifreg) :file)
              ((eql kind s-iflnk) :link)
              (t :special))))))

(defun unix-maybe-prepend-current-directory (name)
  (declare (simple-string name))
  (if (and (> (length name) 0) (char= (schar name 0) #\/))
      name
      (multiple-value-bind (win dir) (rt::unix-current-directory)
	(if win
	    (concatenate 'simple-string dir "/" name)
	    name))))

(defun unix-resolve-links (pathname)
  "Returns the pathname with all symbolic links resolved."
  (declare (simple-string pathname))
  (let ((len (length pathname))
	(pending pathname))
    (declare (fixnum len) (simple-string pending))
    (if (zerop len)
	pathname
	(let ((result (make-string 1024 :initial-element (code-char 0)))
	      (fill-ptr 0)
	      (name-start 0))
	  (loop
	    (let* ((name-end (or (position #\/ pending :start name-start) len))
		   (new-fill-ptr (+ fill-ptr (- name-end name-start))))
	      (replace result pending
		       :start1 fill-ptr
		       :end1 new-fill-ptr
		       :start2 name-start
		       :end2 name-end)
	      (let ((kind (unix-file-kind (if (zerop name-end) "/" result) t)))
		(unless kind (return nil))
		(cond ((eq kind :link)
		       (multiple-value-bind (link err)
                           (rt::unix-readlink result)
			 (unless link
			   (error "Error reading link ~S: ~S"
				  (subseq result 0 fill-ptr)
				  (get-unix-error-msg err)))
			 (cond ((or (zerop (length link))
				    (char/= (schar link 0) #\/))
				;; It's a relative link
				(fill result (code-char 0)
				      :start fill-ptr
				      :end new-fill-ptr))
			       ((string= result "/../" :end1 4)
				;; It's across the super-root.
				(let ((slash (or (position #\/ result :start 4)
						 0)))
				  (fill result (code-char 0)
					:start slash
					:end new-fill-ptr)
				  (setf fill-ptr slash)))
			       (t
				;; It's absolute.
				(and (> (length link) 0)
				     (char= (schar link 0) #\/))
				(fill result (code-char 0) :end new-fill-ptr)
				(setf fill-ptr 0)))
			 (setf pending
			       (if (= name-end len)
				   link
				   (concatenate 'simple-string
						link
						(subseq pending name-end))))
			 (setf len (length pending))
			 (setf name-start 0)))
		      ((= name-end len)
		       (return (subseq result 0 new-fill-ptr)))
		      ((eq kind :directory)
		       (setf (schar result new-fill-ptr) #\/)
		       (setf fill-ptr (1+ new-fill-ptr))
		       (setf name-start (1+ name-end)))
		      (t
		       (return nil))))))))))

(defun unix-simplify-pathname (src)
  (declare (simple-string src))
  (let* ((src-len (length src))
	 (dst (make-string src-len))
	 (dst-len 0)
	 (dots 0)
	 (last-slash nil))
    (macrolet ((deposit (char)
			`(progn
			   (setf (schar dst dst-len) ,char)
			   (incf dst-len))))
      (dotimes (src-index src-len)
	(let ((char (schar src src-index)))
	  (cond ((char= char #\.)
		 (when dots
		   (incf dots))
		 (deposit char))
		((char= char #\/)
		 (case dots
		   (0
		    ;; Either ``/...' or ``...//...'
		    (unless last-slash
		      (setf last-slash dst-len)
		      (deposit char)))
		   (1
		    ;; Either ``./...'' or ``..././...''
		    (decf dst-len))
		   (2
		    ;; We've found ..
		    (cond
		     ((and last-slash (not (zerop last-slash)))
		      ;; There is something before this ..
		      (let ((prev-prev-slash
			     (position #\/ dst :end last-slash :from-end t)))
			(cond ((and (= (+ (or prev-prev-slash 0) 2)
				       last-slash)
				    (char= (schar dst (- last-slash 2)) #\.)
				    (char= (schar dst (1- last-slash)) #\.))
			       ;; The something before this .. is another ..
			       (deposit char)
			       (setf last-slash dst-len))
			      (t
			       ;; The something is some random dir.
			       (setf dst-len
				     (if prev-prev-slash
					 (1+ prev-prev-slash)
					 0))
			       (setf last-slash prev-prev-slash)))))
		     (t
		      ;; There is nothing before this .., so we need to keep it
		      (setf last-slash dst-len)
		      (deposit char))))
		   (t
		    ;; Something other than a dot between slashes.
		    (setf last-slash dst-len)
		    (deposit char)))
		 (setf dots 0))
		(t
		 (setf dots nil)
		 (setf (schar dst dst-len) char)
		 (incf dst-len))))))
    (when (and last-slash (not (zerop last-slash)))
      (case dots
	(1
	 ;; We've got  ``foobar/.''
	 (decf dst-len))
	(2
	 ;; We've got ``foobar/..''
	 (unless (and (>= last-slash 2)
		      (char= (schar dst (1- last-slash)) #\.)
		      (char= (schar dst (- last-slash 2)) #\.)
		      (or (= last-slash 2)
			  (char= (schar dst (- last-slash 3)) #\/)))
	   (let ((prev-prev-slash
		  (position #\/ dst :end last-slash :from-end t)))
	     (if prev-prev-slash
		 (setf dst-len (1+ prev-prev-slash))
		 (return-from unix-simplify-pathname "./")))))))
    (cond ((zerop dst-len)
	   "./")
	  ((= dst-len src-len)
	   dst)
	  (t
	   (subseq dst 0 dst-len)))))

;;; GET-UNIX-ERROR-MSG -- public.
;;; 
(defun get-unix-error-msg (error-number)
  "Returns a string describing the error number which was returned by a
  UNIX system call."
  (declare (type integer error-number))
  (if (array-in-bounds-p *unix-errors* error-number)
      (svref *unix-errors* error-number)
      (format nil "Unknown error [~d]" error-number)))

(setq *default-pathname-defaults*
  (%make-pathname :host *unix-host*
                  :device nil
                  :directory nil
                  :name nil
                  :type nil
                  :version :newest))
