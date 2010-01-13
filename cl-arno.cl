(defpackage :cl-arno
    (:use     #:cl)
    (:export  #:enable-arc-lambdas #:enable-brackets #:in #:range #:aif #:aand #:awhen #:awhile #:awith #:aunless #:lc #:uc #:mkstr #:str #:str+= #:reread #:symb #:vector-to-list* #:~ #:~s #:!~ #:resplit #:split #:join #:x #:range #:glob #:unglob #:glob-lines #:select #:f= #:f/= #:flatten #:system #:foreach #:import-forced #:with-temporary-file #:it #:ls #:argv #:mkhash #:pick #:o #:keys #:-> #:defstruct-and-export #:keyw)
    #-abcl (:export #:getenv)
	)

(in-package :cl-arno)
(require 'asdf)
(asdf:operate 'asdf:load-op 'cl-ppcre)
#-abcl (asdf:operate 'asdf:load-op 'drakma)
#+sbcl (asdf:operate 'asdf:load-op 'sb-posix)
#-abcl (asdf:operate 'asdf:load-op 'closer-mop)

; **** Lambda expressions ala Arc by Brad Ediger ***
;CL-USER> ([+ 1 _] 10)
;11
;CL-USER> ([+ _ __] 1 2)
;3
;added possibility of functional position for _ or __ :
;CL-USER> ([_ 1 2] (list 1 2 3))
;(2 3)

(defun square-bracket-reader (stream char)
  (declare (ignore char))
  (let ((contents (read-delimited-list #\] stream t)))
     `(lambda (&optional ,(intern "_") ,(intern "__"))
              (declare (ignorable ,(intern "_") ,(intern "__")))
              ,(if (or (eq (intern "_") (nth 0 contents))
                       (eq (intern "__") (nth 0 contents)))
                   `(access ,@contents)
                   contents))))

(defun enable-arc-lambdas ()
  (set-macro-character #\[ #'square-bracket-reader)
  (set-macro-character #\] (get-macro-character #\) nil)))

(enable-arc-lambdas)

(defun access (object start &rest args)
  (cond ((null object) nil)
        ((and (or (listp object) (vectorp object)) (numberp start))
           (if (and args (car args))
               (if (or (> (car args) (- (length object) 1)) (and (minusp (car args)) (> (- (car args)) (+ (length object) 1))))
                   (error "Second index out of bounds")
                   (subseq object start 
                                  (if (>= (car args) 0) (+ (car args) 1) (+ (length object) 1 (car args)))))
               (elt object start)))
        ((functionp object)
           (apply object (cons start args)))
        ((hash-table-p object)
           (if args (mapcar [gethash _ object] (cons start args)) (gethash start object)))
	((typep object 'structure-object)
	   (slot-value object start))
        ((and (symbolp start) (symbolp (elt object 0)) (evenp (length object)))
           (getf object start))
        ((and (symbolp start) (consp (elt object 0)))
           (cdr (assoc start object)))))

(defun setaccess (object start &rest args)
  (cond ((and (or (listp object) (vectorp object))
              (numberp start))
           (if (> (length args) 1) (setf (subseq object start (car args)) (cadr args)) (setf (elt object start) (car args))))
        ((hash-table-p object)
           (setf (gethash start object) (car args)))
	((typep object 'structure-object)
	   (setf (slot-value object start) (car args)))
        ((and (symbolp start) (symbolp (elt object 0)) (evenp (length object)))
           (if #1=(getf object start)
             (setf #1# (car args))
             (nconc object (list start (car args)))))
        ((and (symbolp start) (consp (elt object 0)))
           (if #2=(assoc start object)
                (rplacd #2# (car args))
                (nconc object (list (cons start (car args))))))
        (t (error "Type not supported by setf {}"))))

(defsetf access setaccess)

(defun bracket-reader (stream char)
  (declare (ignore char))
  `(access ,@(read-delimited-list #\} stream t)))

(defun enable-brackets ()
  (set-macro-character #\{ #'bracket-reader)
  (set-macro-character #\} (get-macro-character #\) nil)))

(enable-brackets)

(defun compose-reader (stream char)
  (declare (ignore char))
  (prog1
    (read-delimited-list #\) stream t)
    (unread-char #\) stream)))

(defun enable-compose ()
  (set-macro-character #\! #'compose-reader))

(defun pick (object &rest places)
  (mapcar [access object _] places))

;function composition from on-lisp (compose)
(defun o (&rest fns)
  "compose functions"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns))) #'(lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t :initial-value (apply fn1 args)))) #'identity))

;*** in ala python ***
(defun in (seq elmt)
	"does seq contain elmt ?"
	(if (position elmt seq :test #'equal) t nil))

; mkstr by P.G. (On Lisp)
(defun mkstr (&rest args)
  (with-output-to-string (s)
     (dolist (a args) (princ a s))))

; reread by P.G. (On Lisp)
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

; symb by P.G. (On Lisp)
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun keyw (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)) "KEYWORD")))

; *** Paul Graham's anaphoric if (cf. On Lisp) ***
(defmacro aif (test-form then-form &optional else-form)
  "Executes <body> with <it> bound to <expr> if <expr> is not nil"
	`(let ((it ,test-form)) 
		(if it ,then-form ,else-form)))

; *** Paul Graham's anaphoric when (cf. On Lisp) ***
(defmacro awhen (test-form &rest then-forms)
  "Executes <body> with <it> bound to <expr> if <expr> is not nil"
	`(let ((it ,test-form)) 
	 	 (when it ,@then-forms)))

; *** Paul Graham's anaphoric and (cf. On Lisp) ***
(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))
 

(defmacro aunless (test-form &rest then-forms)
    "Executes <body> with <it> bound to <expr> if <expr> is nil"
    `(let ((it ,test-form))
         (unless it ,@then-forms)))

; *** Paul Graham's anaphoric while (cf. On Lisp) ***
(defmacro awhile (expr &body body) 
  "Executes <body> with <it> bound to <expr> as long as <expr> is not nil"
	`(do ((it ,expr ,expr)) ((not it)) ,@body))

(defmacro awith (expr &body body)
  "Executes <body> with <it> bound to <expr>"
	`(let ((it ,expr)) ,@body))

(defun lc (string)
  "Converts a string to lowercase (shortcut for string-downcase)"
  (string-downcase string))

(defun uc (string)
  "Converts a string to uppercase (shortcut for string-upcase)"
  (string-upcase string))  

; *** Paul Graham's anaphoric function from arc ***
; CL version from http://setagaya.googlecode.com/svn-history/r25/trunk/home/mc/arc-compat/anaphoric-op.lisp
(defmacro afn (parms &body body)
    "Creates an anaphoric function, which can be called recursively
  with the name self. This allows a recursive function to be
  created without assigning it a name."
    `(labels ((self ,parms ,@body))
                  #'self))

(defun vector-to-list* (object)
  (let ((result (list nil))
        (length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
         (splice result (cdr splice)))
        ((= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))

(defun parse-re (re)
  (let ((result (list "")))
       (loop for i from 1 below (length re)
             do (if (and (char= {re i} #\/) (char/= {re (- i 1)} {"\\" 0})) 
                    (push "" result)
                    (setf (car result) (mkstr (car result) {re i}))))
       (when (< (length result) 3) (push (car result) result) (setf (cadr result) ""))
       (if (char/= {re 0} #\/) (setf (car result) (mkstr {re 0} (car result))))
       (if (in (car result) #\i) (setf (caddr result) (mkstr "(?i)" (caddr result))))
       (setf (cadr result) (cl-ppcre::regex-replace-all "\\\\/" (cadr result) "/"))
       (reverse result)))

;*** Regexp match ala Perl ***
(defun ~ (re string-or-list &optional match-nb1 match-nb2)
  "If <string-or-list> is a string:
    returns nil if regular expression <re> does not match the string
    returns the part of the string that matches <re> and all grouped matches ()
    if <match-nb1> is not nil, only match number <match-nb1> will be returned
   if <string-or-list> is a list
    returns the elements of that list that match <re>
    
    example : (re \"\w+(\d)\" \"ab2cc\")"
    (declare (optimize debug))
  (destructuring-bind (regexp subre flags) (parse-re re)
    (declare (ignorable subre))
    (if (listp string-or-list)
        (let ((matching (remove-if-not [cl-ppcre:scan regexp _] string-or-list)))
             (if match-nb1
                 (if match-nb2
                     (~ re (nth match-nb1 matching) match-nb2)
                     (nth match-nb1 matching))
                 matching))
        (let ((match-indexes (cl-ppcre:all-matches regexp string-or-list)))
           (when match-indexes
             (unless (in flags #\g) (setf (cddr match-indexes) nil))
             (apply (if (in flags #\g) #'identity [first _]) (list
               (loop for i from 0 below (length match-indexes) by 2 collect
                  (apply (if match-nb1 [nth match-nb1 _] #'identity) (list
                      (let ((m (multiple-value-list (cl-ppcre:scan-to-strings regexp string-or-list :start {match-indexes i}))))
                        (if {m 1} (cons (car m) (vector-to-list* (cadr m))) nil))))))))))))

(defun !~ (re string-or-list)
  "If <string-or-list> is a string:
    returns nil if <re> matches the string
    returns the string if <re> does not match
   if <string-or-list> is a list
    returns the elements of that list that do not match <re>
    
    example: (!~ \"\w{3}\" (list \"aaa\" \"bb\" \"ccc\"))"
  (destructuring-bind (regexp subre flags) (parse-re re)
    (declare (ignorable subre flags))
    (if (listp string-or-list)
        (remove-if [cl-ppcre::scan regexp _] string-or-list)
        (if (not (cl-ppcre::scan regexp string-or-list))
            string-or-list))))

;*** Regexp substitution a la Perl (or nearly...) ***
(defun ~s (re string-or-list &optional match-nb)
  "Replaces all substrings that match <re> in <string> by <replacement>.
  <flags> can contain Perl regexp flags like g"
  (if (listp string-or-list)
    (mapcar [~s re _ match-nb] string-or-list)
    (destructuring-bind (regexp subre flags) (parse-re re)
      (let ((matches (~ re string-or-list match-nb)))
        (values
          (if (in flags #\g)
            (cl-ppcre::regex-replace-all regexp string-or-list subre)
            (cl-ppcre::regex-replace regexp string-or-list subre))
          matches)))))

(defun resplit (re string)
  (cl-ppcre:split (car (parse-re re)) string))

(defun split (sep seq)
  (if (= (length sep) 0)
      (coerce seq 'list)
      (loop for start = 0 then (+ end (length sep))
                   for end = (search sep seq :start2 start)
                          collecting {seq start (- (or end (length seq)) 1)}
                                 while end)))

(defun join (join-seq seq-list)
  (if (> (length seq-list) 1)
      (concatenate (class-of join-seq) (car seq-list) join-seq (join join-seq (cdr seq-list)))
      (car seq-list)))

;*** x a la perl/python/ruby ***
(defun x (seq nb)
  "returns a sequence composed of <nb> times <sequence>"
  (let ((s seq))
    (loop for i from 1 below nb do (setf s (concatenate 'sequence s seq)))
    (coerce s (class-of seq))))

(defmacro foreach (list &rest body)
  "Executes body for each element of <list>, with:
    <it>               bound to the current element
    <its-index>        bound to its index in the list,"
    (let ((var (if (eq (car body) 'as)
                   (symbol-name (cadr body))
                   "it")))
     `(loop for i from 0 below (length ,list) collect
        (let ((,(reread var)         {,list i})
              (,(reread "@" var)  i))
           (declare (ignorable ,(reread var) ,(reread "@" var)))
           (progn ,@(if (eq (car body) 'as) {body 1 -1} body))))))

; *** Range nearly ala Python (in Python, end is not included) ***
; >(range 1 10)
; (1 2 3 4 5 6 7 8 9 10)
; >(range -10 -100 -30)
; (-10 -40 -70 -100)
; >(range 10)
; (0 1 2 3 4 5 6 7 8 9 10)
(defun range (a &optional b (step 1))
	"Builds a range of numbers"
	(let ((start (if b a 0)) (end (if b b a)))
		(if (> step 0)
			(loop for x from start to end by step collect x)		
			(loop for x from start downto end by (- step) collect x))))

; Reads a url/file/stream entirely then closes it and returns the contents as a string
; based on Shawn Betts's slurp http://www.emmett.ca/~sabetts/slurp.html
(defun glob (path-or-stream &key binary)
  "Globs the whole provided file, url or stream into a string or into a byte array if <binary>"
  (cond
    #-abcl
    ((and (stringp path-or-stream) (string-equal {path-or-stream 0 6} "http://"))
       (multiple-value-bind (body status-code headers real-url stream must-close reason-phrase)
                            (drakma:http-request path-or-stream :want-stream t :force-binary binary)
            (declare (ignorable headers real-url stream must-close reason-phrase))
            (if (= 200 status-code) (glob body :binary binary) (if binary #() ""))))
    ((and binary (stringp path-or-stream))
       (with-open-file   (s path-or-stream :element-type '(unsigned-byte 8)) (glob s :binary t)))
    ((stringp path-or-stream)
       (with-open-file   (s path-or-stream)
                         (let ((seq (make-array (file-length s) :element-type 'character :fill-pointer t)))
                              (setf (fill-pointer seq) (read-sequence seq s))
                              seq)))
    ((and (streamp path-or-stream) binary)
       (let ((buf (make-array 4096 :element-type '(unsigned-byte 8)))
             (seq (make-array 0 :element-type '(unsigned-byte 8) :adjustable t)))
            (loop for index = 0 then (+ index pos)
                  for pos = (read-sequence buf path-or-stream)
                  while (plusp pos)
                  do (adjust-array seq (+ index pos))
                     (replace seq buf :start1 index :end2 pos))
             seq))
    ((streamp path-or-stream)
       (with-open-stream (s path-or-stream)
                        (with-output-to-string (out)
                                               (do ((x (read-char s nil s) (read-char s nil s)))
                                                   ((eq x s))
                                                   (write-char x out)))))))

(defun unglob (filename sequence &key if-exists)
  (progn
    (with-open-file (stream filename
                            :direction :output
                            :element-type (if (stringp sequence) 'character '(unsigned-byte 8))
                            :if-exists if-exists
                            :if-does-not-exist :create)
      (write-sequence sequence stream))
    t))

(defun glob-lines (path-or-stream)
  "Globs the whole provided file, url or stream into an array of its lines"
  (resplit "/\\r\\n|\\n/" (glob path-or-stream)))

(defmacro with-temporary-file (assignment &rest body)
  (destructuring-bind (filename extension) assignment
                      `(let ((,filename (str "/tmp/highres_" (get-internal-real-time) (random 100) "." ,extension)))
                          (prog1
                             (progn
                                ,@body)
                             (delete-file ,filename)))))

(defun f= (f &rest objects)
  (apply #'equal (mapcar f objects)))

(defun f/= (&rest args)
  (not (apply #'f= args)))

;flatten (Adapted from On Lisp)
(defun flatten (&rest x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
          (rec x nil)))

(defun str (&rest args)
  (apply #'mkstr (remove-if-not #'identity (flatten args))))

(defmacro str+= (place &rest args)
  `(setf ,place (apply #'str (list ,place ,@args))))

; System based on run-prog-collect-output from stumpwm (GPL)
(defun system (command)
  "run a command and read its output."
  (with-input-from-string (ar command)
    #+allegro (with-output-to-string (s) (excl:run-shell-command "/bin/sh" :output s :wait t :input ar))
    #+clisp   (with-output-to-string (s)
                (let ((out (ext:run-program "/bin/sh" :arguments () :input ar :wait t :output :stream)))
                  (loop for i = (read-char out nil out)
                        until (eq i out)
                        do (write-char i s))))
    #+cmu     (with-output-to-string (s) (ext:run-program "/bin/sh" () :input ar :output s :error s :wait t))
    #+sbcl    (with-output-to-string (s) (sb-ext:run-program "/bin/sh" () :input ar :output s :error s :wait t)))
    #+ccl     (with-output-to-string (s) (ccl:run-program "/bin/sh" () :wait t :output s :error t :input ar))
    #+abcl    (with-output-to-string (s) (ext:run-shell-command command :output s))
    #-(or allegro clisp cmu sbcl ccl abcl)
              (error 'not-implemented :proc (list 'pipe-input prog args)))

; get-env from stumpwm (also found in the CL cookbook) (GPL or better)
#-abcl ;abcl has it predefined !
(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro       (sys::getenv (string var))
  #+clisp         (ext:getenv (string var))
  #+(or cmu scl)  (cdr (assoc (string var) ext:*environment-list* :test #'equalp :key #'string))
  #+gcl           (si:getenv (string var))
  #+lispworks     (lw:environment-variable (string var))
  #+lucid         (lcl:environment-variable (string var))
  #+mcl           (ccl::getenv var)
  #+sbcl          (sb-posix:getenv (string var))
  #+openmcl       (ccl:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl openmcl)
                  (error 'not-implemented :proc (list 'getenv var)))

(defun (setf getenv) (val var)
  "Set the value of the environment variable, @var{var} to @var{val}."
  #+allegro      (setf (sys::getenv (string var)) (string val))
  #+clisp        (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl) (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp :key #'string)))
                    (if cell
                        (setf (cdr cell) (string val))
                        (push (cons (intern (string var) "KEYWORD") (string val)) ext:*environment-list*)))
  #+gcl          (si:setenv (string var) (string val))
  #+lispworks    (setf (lw:environment-variable (string var)) (string val))
  #+lucid        (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl         (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #+openmcl      (ccl:setenv (string var) (string val))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl openmcl)
                 (error 'not-implemented :proc (list '(setf getenv) var)))

; argv adapted from CL-cookbook
(defun argv (&optional index)
  (let ((args (or #+sbcl                    sb-ext:*posix-argv*  
                  #+lispworks               system:*line-arguments-list*
                  #+cmu                     extensions:*command-line-words*
                  #-(or sbcl lispworks cmu) (error 'not-implemented :proc (list 'getenv var)))))
    (awhen args
       (if index (nth index it) it))))

(defun filesize (filepath)
  (with-open-file (s filepath) (file-length s)))

(defun ls (dir &key recurse files-only dirs-only)
   #+(or :sbcl :cmu :scl :lispworks)
    (let* ((contents (directory (str dir "/*.*")))
           (dirs nil)
           (files nil))
       (mapcar #'str
         (if (not (or recurse files-only dirs-only))
             contents
             (progn
               (loop for path in contents
                     when (directory (str path "/")) do (push path dirs)
                     when (not (or dirs-only (directory (str path "/")))) do (push path files))
               (flatten (when (not dirs-only) files)
                        (when (not files-only) dirs)
                        (when recurse
                           (mapcar [ls _ :recurse recurse :files-only files-only :dirs-only dirs-only] dirs))))))))

(defun mkhash (&rest args)
  (let ((hash (make-hash-table :test 'equal)))
       (loop for i from 0 below (length args) by 2
             do (setf {hash {args i}} {args (+ i 1)}))
       hash))

#-abcl
(defun slot-names (class)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (find-class class))))

#-abcl
(defun keys (o)
  (cond ((hash-table-p o)
          (loop for k being each hash-key of o collect k))
        ((and (listp o) (symbolp {o 0}) (evenp (length o)))
          (loop for i from 0 below (length o) by 2 collect {o i}))
        ((and (listp o) (consp {o 0}))
          (mapcar #'car o))
        ((eq (type-of o) 'standard-object)
          (slot-names (class-of o)))
        (t nil)))

#-abcl
(defun slot-type (class slot)
  (dolist (s (closer-mop:class-slots (find-class class)))
     (when (eql (closer-mop:slot-definition-name s) slot)
        (return-from slot-type (closer-mop:slot-definition-type s)))))

#-abcl
(defun -> (o type &key exclude only unflatten)
  (declare (optimize debug))
  (cond
     ((or (eq type t) (eq (type-of o) type))
        o)
     ((and (numberp o) (eq 'string type))
        (format nil "~S" o))
     ((and (keys o) (in '(alist plist hash-table) type))
        (let ((o2 (case type
                    ((plist alist) nil)
                    (hash-table (make-hash-table)))))
          (loop for key in (sort (or only (keys o)) [string> (str _) (str __)])
                do (funcall (case type
                              (plist [setf (getf o2 key) _])
                              (alist [nconc o2 (list (cons key _))])
                              (hash-table [setf (gethash key o2) _]))
                          {o key}))
          o2))
     ((and (keys o) (or (subtypep type 'structure-object) (subtypep type 'standard-object)))
        (apply (symb "MAKE-" type)
          (loop for key in (or only (slot-names type))
                collect (reread (symb ":" key))
                collect (-> (if unflatten o {o key}) (slot-type type key) :unflatten unflatten :exclude exclude :only only))))
     (t (cl:coerce o type))))

(defmacro defstruct-and-export (structure &rest members)
	(append
	 `(progn
	 ,(append `(defstruct ,structure ,@members))
	 ,`(export ,`(quote ,(intern (concatenate 'string "MAKE-"
	 (symbol-name structure))))) ,`(export ,`(quote ,(intern
	 (concatenate 'string "COPY-" (symbol-name structure))))))
	 (mapcar
		 #'(lambda (member)
			 `(export ,`(quote ,(intern (concatenate 'string (symbol-name structure) "-" (symbol-name (if (listp member) (car member) member)))))))
		 members)))
