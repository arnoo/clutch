(defpackage :cl-arno
    (:use     #:cl)
    (:export  #:date #:d- #:d+ #:d-delta #:enable-arc-lambdas #:enable-brackets #:in #:range #:aif #:aand #:awhen #:awhile #:awith #:aunless #:lc #:uc #:mkstr #:str #:str+= #:reread #:symb #:vector-to-list* #:~ #:~s #:!~ #:resplit #:split #:join #:x #:range #:glob #:unglob #:glob-lines #:select #:f= #:f/= #:flatten #:sh #:system #:foreach #:import-forced #:with-temporary-file #:it #:ls #:argv #:mkhash #:pick #:o #:keys #:-> #:defstruct-and-export #:keyw #:rm #:fload #:fsave #:fselect #:fselect1 #:mkdir #:md5 #:sha1 #:sha256 #:memoize-to-disk #:with-each-line #:ut #:miltime #:y-m-d)
    #-abcl (:export #:getenv))

(in-package :cl-arno)
(require 'asdf)
(asdf:operate 'asdf:load-op 'cl-ppcre :verbose nil)
(asdf:operate 'asdf:load-op 'ironclad :verbose nil)
#-abcl (asdf:operate 'asdf:load-op 'drakma)
#+sbcl (require 'sb-posix)
#+sbcl (require 'closer-mop)

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
           (let ((end (and args (car args)))
                 (len (length object)))
             (when (or (> start len)
                       (<= start (- len)))
               (error "Index out of bounds"))
             (if end
                 (if (or (> end len)
                         (and (minusp end)
                              (< end (- -1 len))))
                   (error "Second index out of bounds")
                   (subseq object (mod start len)
                                  (if (minusp end) (+ len 1 end)
                                      end)))
                 (elt object (mod start len)))))
        ((functionp object)
           (apply object (cons start args)))
        ((hash-table-p object)
           (if args (mapcar [gethash _ object] (cons start args)) (gethash start object)))
	((typep object 'structure-object)
     #-abcl
	   (slot-value object (symb start))
     #+abcl
     (eval (list (symb (uc (str (type-of object) "-" start))) object)))
        ((and (or (symbolp start) (stringp start)) (symbolp (elt object 0)) (evenp (length object)))
           (getf object (symb start)))
        ((and (or (symbolp start) (stringp start)) (consp (elt object 0)))
           (cdr (assoc (symb start) object)))))

(defun setaccess (object start &rest args)
  (cond ((and (or (listp object) (vectorp object))
              (numberp start))
           (if (cdr args) 
              (setf (subseq object start (car args)) (cadr args))
              (setf (elt object start) (car args))))
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
	(not (null (position elmt seq :test #'equal))))

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

(defun lc (object)
  "Converts an object to a lowercase string"
  (string-downcase (str object)))

(defun uc (object)
  "Converts an object to an uppercase string"
  (string-upcase (str object)))

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
                 (mapcar [~ re _ match-nb1 match-nb2] matching)
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
  <flags> can contain Perl regexp flags like g
  replacement can be a string which may contain the special substrings \\& for the whole match, \\` for the part of target-string before the match, \\' for the part of target-string after the match, \\N or \\{N} for the Nth register where N is a positive integer."
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
            collecting {seq start (or end (length seq))}
            while end)))

(defun join (join-seq seq-list)
  (if (cdr seq-list)
      (concatenate (class-of join-seq) (car seq-list) join-seq (join join-seq (cdr seq-list)))
      (car seq-list)))

;*** x a la perl/python/ruby ***
(defun x (seq nb)
  "returns a sequence composed of <nb> times <sequence>"
  (let ((s ""))
    (loop for i below nb do (setf s (concatenate (class-of seq) s seq)))
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

(defmacro with-stream (args &rest body)
  (destructuring-bind (name path-or-stream &key binary) args
   `(cond ((streamp ,path-or-stream)
             (let ((,name ,path-or-stream))
                ,@body))
          #-abcl
          ((and (stringp ,path-or-stream) 
                (> (length ,path-or-stream) 5)
                (string-equal {,path-or-stream 0 6} "http://"))
             (multiple-value-bind (response status-code headers real-url stream must-close reason-phrase)
                                  (drakma:http-request ,path-or-stream :want-stream t :force-binary ,binary)
                  (declare (ignorable headers real-url stream must-close reason-phrase))
                (if (= 200 status-code)
                  (let ((,name response))
                    ,@body)
                  (with-input-from-string (,name "")
                    ,@body))))
          ((and ,binary (stringp ,path-or-stream))
             (with-open-file (,name ,path-or-stream :element-type '(unsigned-byte 8))
                ,@body))
          ((stringp ,path-or-stream)
             (with-open-file (,name ,path-or-stream)
                ,@body)))))

(defun looks-like-file (path-or-stream)
  (not (or (streamp path-or-stream)
           (and (stringp path-or-stream) 
               (> (length path-or-stream) 5)
               (string-equal {path-or-stream 0 6} "http://")))))
  

(defun glob (path-or-stream &key binary (offset 0) limit)
  "Globs the whole provided file, url or stream into a string or into a byte array if <binary>,
  starting at offset. If offset is negative, start from end-offset. Read at most limit characters."
  (if (looks-like-file path-or-stream)
    (with-open-file (s path-or-stream)
       (let ((fl (file-length path-or-stream)))
         (when (< 0 offset)
           (setf offset (max (- fl offset) 0)))
         (when (and limit (< 0 limit)
           (setf limit (max 0 (- fl limit)))))
       (let (seq (make-array (aif limit (min it fl) fl)
                        :element-type 'character
                        :fill-pointer t))
            (awhen offset
              (file-position s offset))
            (setf (fill-pointer seq) (read-sequence seq s))
            seq)))
    (with-stream (s path-or-stream :binary binary)
       (let ((buf (if binary
                        (make-array 4096 :element-type '(unsigned-byte 8))
                        (make-string 4096)))
             (seq (if binary
                        (make-array 0 :element-type '(unsigned-byte 8) :adjustable t)
                        (make-string 0))))
            (loop for index = 0 then (+ index pos)
                  for pos = (read-sequence buf s)
                  with posoffset = (max 0 offset)
                  with remoffset = posoffset
                  while (and (plusp pos)
                             (or (not limit)
                                 (and (plusp limit) (< pos (+ limit 4096)))))
                  do (when (and (> (+ index pos) posoffset))
                        (if binary
                          (adjust-array seq (or limit (- (+ index pos) posoffset)))
                          (setf seq (concatenate 'string seq (make-string pos))))
                        (replace seq buf
                            :start1 index
                            :start2 remoffset)
                        (decf remoffset pos)))
             (when (and binary (equal seq "")) (setf seq #()))
             (when (< 0 offset)
               (setf seq (if (< (length seq) offset) "" {seq offset -1})))
             (when (and limit (< 0 limit)
               (setf limit (max 0 (- (length seq) limit)))))
             (aif limit {seq 0 (min (plusp it (length seq)))} seq)))))

(defun unglob (filename object &key if-exists binary readable)
  (progn
    (with-open-file (stream filename
                            :direction :output
                            :element-type (if binary '(unsigned-byte 8) 'character)
                            :if-exists if-exists
                            :if-does-not-exist :create)
      (if (and (not readable)
               (or (stringp object)
                   (and (vectorp object) binary)))
        (write-sequence object stream)
        (prin1 object stream)))
    t))

(defun count-lines (file)
  (let ((total 0))
    (with-open-file (file-stream file)
      (do ((it (read-line file-stream) (read-line file-stream nil 'eof)))
          ((eq it 'eof) (values))
          (incf total)))
    total))

(defun back-n-lines (file-stream n)
  (let ((buf (make-string 4096))
        (lines 0))
      (loop until (or (>= lines n) (< (length buf) 4096))
            do (file-position file-stream (max 0 (- (file-position file-stream) 4096)))
               (awith (read-sequence buf file-stream)
                  (file-position file-stream (- (file-position file-stream) it)))
               (incf lines (count #\Newline buf)))
      (print (str "LINES : " lines))
      (if (> lines n)
        (file-position file-stream (+ (file-position file-stream)
                                        (length (~ (str "/^" (x "[^\\n]*\\n" (- lines n)) "/")
                                                   buf 0)))))))

(defmacro with-each-fline (args &rest body)
  (destructuring-bind (path-or-stream &key (offset 0) limit) args
    `(block read-loop
        (let ((offset ,offset)
              (limit ,limit))
          (when (< limit 0)
            (error "Limit must be a positive integer (number of lines)"))
          (when (and limit
                     (< limit 0)
                     (looks-like-file ,path-or-stream))
            ;we need to know the total number of lines
            (let ((total (count-lines ,path-or-stream)))
              (when (< offset 0)
                (setf offset (max (+ total offset) 0)))
              (when (< limit 0) 
                (setf limit (max (+ total limit) 0)))))
            (when (and (not (looks-like-file ,path-or-stream))
                       (or (< offset 0)
                           (and limit (< limit 0))))
              ; we have to read the whole thing
              (let ((done-lines 0))
                (loop for it in {(resplit "/\\n/" (glob ,path-or-stream)) offset limit}
                      do ,@body
                         (incf done-lines))
                (return-from read-loop done-lines)))
          (with-stream (s ,path-or-stream)
            (when (and (looks-like-file ,path-or-stream)
                       (< offset 0))
                ; we can read it backwards to set the file-position then set offset to 0
                (file-position s (file-length s))
                (back-n-lines s (- offset))
                (setf offset 0))
              (let ((skipped-lines 0)
                    (done-lines 0))
                (do ((it (read-line s) (read-line s nil 'eof)))
                    ((eq it 'eof) (values))
                    (if (>= skipped-lines offset)
                      (progn
                        (when (and limit (>= done-lines limit))
                          (return-from read-loop done-lines))
                        (incf done-lines)
                        ,@body)
                      (incf skipped-lines)))
                done-lines))))))

(defmacro with-each-line (args &rest body)
  (destructuring-bind (str &key (offset 0) limit) args
    `(with-input-from-string (s ,str)
       (with-each-fline (s :offset ,offset :limit ,limit)
         ,@body))))

(defun glob-lines (path-or-stream &key (offset 0) limit)
  "Globs the whole provided file, url or stream into an array of its lines."
  (let ((lines nil))
    (with-each-fline (path-or-stream :offset offset :limit limit)
      (nconc lines (list it)))
    lines))

(defun mapflines (fn path-or-stream &key  (offset 0) limit)
  "Apply fn to each line of path-or-stream."
  (let ((lines nil))
    (with-each-fline (path-or-stream :offset offset :limit limit)
      (nconc lines (list (funcall fn it))))
    lines))

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

(defun rpad (string chars &key (with " "))
  ;TODO : utiliser with
  (format nil (str "~" chars "<~A~;~>") string))

(defun lpad (string chars &key (with " "))
  ;TODO : utiliser with
  (format nil (str "~" chars "<~A~;~>") string))

; sh based on run-prog-collect-output from stumpwm (GPL)
(defun sh (command)
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

;(defun sh (command)
;  (with-input-from-sh (s command)
;    (glob s)))

;(defmacro with-input-from-sh (args &rest body)
;  (destructuring-bind (stdout command &key stderr stdin) args
;    `#+sbcl (sb-ext:run-program "/bin/sh" (list command) :input stdin :output stdout :error (aif stderr it stdout))
;    `#+abcl (ext:run-shell-command command :output stdout)
;    @body
;    ))

; for backwards compatibility
(defun system (command)
  (sh command))

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

(defun rm (file)
  (delete-file file))

(defun ls (path &key recurse files-only dirs-only)
 #+(or :sbcl :cmu :scl :lispworks :abcl)
  (if
      (and (probe-file path) (not (probe-file (str path "/."))))
      (list path)
    (let* ((contents (directory (str path "/*.*")))
           (dirs nil)
           (files nil))
       (mapcar #'str
         (if (not (or recurse files-only dirs-only))
             contents
             (progn
               (loop for subpath in contents
                     when (directory (str subpath "/")) do (push subpath dirs)
                     when (not (or dirs-only (directory (str subpath "/")))) do (push subpath files))
               (flatten (when (not dirs-only) files)
                        (when (not files-only) dirs)
                        (when recurse
                           (mapcar [ls _ :recurse recurse :files-only files-only :dirs-only dirs-only] dirs)))))))))

(defun mkdir (dir)
  (ensure-directories-exist (make-pathname :directory dir)))

(defun mkhash (&rest args)
  (let ((hash (make-hash-table :test 'equal)))
       (loop for i from 0 below (length args) by 2
             do (setf {hash {args i}} {args (+ i 1)}))
       hash))

(defun slot-names (class)
  #+sbcl(mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (find-class class)))
  #-sbcl(let ((elts (mapcar [str _]
                            (read-from-string (~s "/^.*?\\(/(/"
                                                  (with-output-to-string (s) (prin1 (funcall (symb "NAKE-" class)) s)))))))
          (loop for i from 1 below (length elts) by 2 collect {elts i})))

(defun keys (o)
  (cond ((hash-table-p o)
          (loop for k being each hash-key of o collect k))
        ((and (listp o) (symbolp {o 0}) (evenp (length o)))
          (loop for i from 0 below (length o) by 2 collect {o i}))
        ((and (listp o) (consp {o 0}))
          (mapcar #'car o))
        ((eq (type-of o) 'standard-object)
          #+sbcl(slot-names (class-of o))
          #-sbcl(let ((elts (mapcar [str _]
                                    (read-from-string (~s "/^.*?\\(/(/"
                                                          (with-output-to-string (s) (prin1 o s)))))))
                   (loop for i from 1 below (length elts) by 2 collect {elts i})))
        (t nil)))

(defun kvalues (o)
  (mapcar [access o _] (keys o)))

#-abcl
(defun slot-type (class slot)
  (dolist (s (closer-mop:class-slots (find-class class)))
     (when (eql (closer-mop:slot-definition-name s) slot)
        (return-from slot-type (closer-mop:slot-definition-type s)))))

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
          (loop for key in (sort (or only (keys o)) [string> (str _) (str __)]) do (funcall (case type
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
      ,`(export ,`(quote ,(intern (concatenate 'string "MAKE-" (symbol-name structure)))))
      ,`(export ,`(quote ,(intern (concatenate 'string "COPY-" (symbol-name structure))))))
     (mapcar
       #'(lambda (member)
           `(export ,`(quote ,(intern (concatenate 'string (symbol-name structure) "-" (symbol-name (if (listp member) (car member) member)))))))
       members)))

(defun fsave (object dir &key timestamped id id-slot)
  (let ((lock (str dir "/.cl-arno_lock")))
    (unless timestamped
      (loop while (ls lock) do (sleep 0.1))
      (unglob lock ""))
    (unless id (setf id (aif (and id-slot {object id-slot})
                             it
                             (let ((items (~ "/^.*\\/(.*)(###|$)/" (!~ "/\\/\\.[^\\/]*$/" (ls dir)) 1)))
                                (if items
                                    (if (= (length (~ "/^\\d+$/" items)) (length items))
                                        (+ 1 (apply #'max (mapcar #'read-from-string items)))
                                        (let ((i (str (gensym)))) ; FIXME: this is very stupid (don't use as is)
                                             (loop while (ls i) do (setf i (str (gensym))))
                                             i))
                                    0)))))
    (when id-slot (setf {object id-slot} id))
    (let ((file (str dir "/" id (if timestamped
                                  (str "###" (apply #'format
                                                    (append '(nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D")
                                                             (pick (multiple-value-list (get-decoded-time))
                                                                   5 4 3 2 1 0))))))))
      (unglob file object :readable t :if-exists :supersede))
    (unless timestamped
      (rm lock))
      id))

(defun fload (dir id &key version)
  (unless version
    (awhen (~ (str "/\\/" id "###(\\d+)$/") (ls dir) 1)
      (setf version (apply #'max (mapcar #'read-from-string it)))))
  (read-from-string (glob (str dir "/" id (aif version (str "###" it))))))

(defun fselect (from &key key value limit)
  (let ((lock (str from "/.cl-arno_lock"))
        (loaded 0))
      (loop while (ls lock) do (sleep 0.1))
  (remove-if-not [and _ (equal {_ key} value)]
     (mapcar [if (or (not limit) (< loaded limit))
                 (progn
                     (incf loaded)
                     (fload from _))
                 nil]
             (resplit "/\\n/" (sh (str "cd '" from "' && grep -l '"
                                                             (str (prin1-to-string key) " "
                                                                  (prin1-to-string value))
                                                                   "' *")))))))

(defun fselect1 (from &key key value)
  (aif (fselect from :key key :value value :limit 1)
    (first it)
    nil))

(defmacro ironclad-digest (obj algo)
  `(ironclad:byte-array-to-hex-string
       (ironclad:digest-sequence ,algo
           (cond ((stringp ,obj) (ironclad:ascii-string-to-byte-array ,obj))
                 ((arrayp ,obj) ,obj)))))

(defun sha1 (obj)
  (ironclad-digest obj :sha1))

(defun sha256 (obj)
  (ironclad-digest obj :sha256))

(defun md5 (obj)
  (ironclad-digest obj :md5))

(defun memoize-to-disk (fn &key (dir "/tmp") prefix force-reset remember-last)
  (unless prefix
    (setf prefix (~ "/cl-arno-mem-\{([A-E0-9]+)\}/" (str fn) 1))
    (setf force-reset t))
  (when force-reset (sh (str "rm -f " dir "/" prefix "#*" )))
  #'(lambda (&rest args) 
    (let ((file (str dir "/"
                     prefix
                     "#" (sha256 (str args)))))
       (loop while (ls (str "." file ".lock")) do (sleep 0.01))
       (if (ls file)
           (read-from-string (glob file))
           (awith (apply fn args)
              (sh (str "touch ." file ".lock"))
              (unglob file it :readable t)
              (sh (str "rm -f ." file ".lock"))
              it)))))

; Memoize adapted from OnLisp
(defun memoize (fn &key remember-last)
  (let ((cache (mkhash))
        (calls nil))
     #'(lambda (&rest args)
          (when remember-last
            (push calls args))
          (multiple-value-bind (val hit) (gethash args cache)
            (if hit
                val
                (progn
                  (when remember-last
                     (when (> (hash-table-size cache) remember-last)
                       (remhash cache {calls -1})
                       (setf calls {calls 0 remember-last})))
                  (setf (gethash args cache)
                        (apply fn args))))))))

(defstruct-and-export date
  s m h
  dow day month year
  dst zone)

(defun strtout (str)
  (let ((result (sh (str "date -d \"" str "\" +%s"))))
    (if (!~ "/^\\d+\\n$/" result)
        (error result)
        (+ (read-from-string result) 2208988800))))

(defun date (&optional timestamp-or-string zone)
  (multiple-value-bind (s m h day month year dow daylight-p zone)
                       (decode-universal-time 
                          (aif timestamp-or-string
                            (cond
                              ((stringp it) (strtout it))
                              ((and (numberp it) (< it 2400))
                                   (strtout (str {it 0 2} ":" {it 2 4})))
                              (t it))
                            (get-universal-time))
                          zone)
  (make-date
	  :s s
	  :m m
	  :h h
	  :day day
	  :month month
	  :year year
	  :dow dow
	  :dst daylight-p
	  :zone zone
  )))

(defun ut (&optional str-or-date)
  (declare (optimize debug))
  (if str-or-date
    (if (stringp str-or-date)
      (ut (date str-or-date))
      (encode-universal-time
         (date-s str-or-date)
         (date-m str-or-date)
         (date-h str-or-date)
         (date-day str-or-date)
         (date-month str-or-date)
         (date-year str-or-date)
         (+ (date-zone str-or-date) (if (date-dst str-or-date) 1 0))
         ))
    (get-universal-time)))

(defun d-delta (date1 date2)
  (- (ut date1) (ut date2)))

(defun d+ (date seconds)
  (date (+ (ut date) seconds) (date-zone date)))

(defun d- (date seconds)
  (d+ date (- seconds)))

(defun miltime (&optional (date (date (ut))))
  (+ (* 100 (date-h date))
     (date-m date)
     (/ 100 (date-s date))))
  
(defun to-zone (date zone)
  (date (ut date) zone))

(defun y-m-d (date)
  (str (date-year date) "-" (date-month date) "-" (date-day date)))

(defun date-rfc-2822 (date)
  (format t "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
       {(list "Mon" "Tue" "Wen" "Thu" "Fri" "Sat" "Sun") (date-dow date)}
       (date-day date)
       {(list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (date-month date)}
       (date-year date)
       (date-h date)
       (date-m date)
       (date-s date)
       (~s "/^(-|)(\\d{3})$/\\{1}0\\2/" (str (* 100 (date-zone date))))))

(defun date-gnu (date format)
  (sh (str "date -d '" (date-rfc-2822 date) "' +'" format "'")))
