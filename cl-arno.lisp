;
;   Copyright 2011 Arnaud Betremieux <arno@arnoo.net>, except where
;   mentioned otherwise.
;
;   The program in this file is free software: you can redistribute it
;   and/or modify it under the terms of the GNU General Public License
;   as published by the Free Software Foundation, either version 3 of
;   the License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(defpackage :cl-arno
    (:use     #:cl)
    (:export  #:date #:d- #:d+ #:d-delta #:ut #:miltime #:y-m-d #:date-wom #:date-week #:d= #:d/= #:d> #:d<
              #:enable-arc-lambdas #:enable-brackets #:defstruct-and-export 
              #:in #:range #:vector-to-list* #:flatten #:pick #:pushend #:popend
              #:aif #:aand #:awhen #:awhile #:awith #:aunless #:acond #:rlambda
              #:lc #:uc #:str #:symb #:keyw  #:~ #:~s #:!~ #:resplit #:split #:join #:x #:lpad #:rpad #:lines
              #:glob #:unglob #:glob-lines #:with-each-line #:mapflines 
              #:f= #:f/= #:with-temporary-file #:it
              #:sh #:ls #:argv #:mkhash #:keys #:rm #:mkdir 
              #:fload #:fsave #:fselect #:fselect1
              #:md5 #:sha1 #:sha256
              #:memoize #:memoize-to-disk #:before #:after #:o 
              #:xor)
    #-abcl (:export #:getenv))

(in-package :cl-arno)

(defvar +shell+ "/bin/bash")
(defvar +months-abbr+ (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar +months+ (list "" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
(defvar +days-abbr+ (list "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar +days+ (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defmacro pushend (object lst)
  `(if ,lst
       (nconc ,lst (list ,object))
       (setf ,lst (list ,object))))

(defmacro popend (lst)
  `(prog1
     (last ,lst)
     (nbutlast ,lst)))

(defmacro rlambda (args &rest body)
  `(labels ((recurse ,args ,@body))
      #'recurse))

(defun flatten (&rest lst)
  "Recursively flatten a list of lists"
  (let ((result nil))
    (funcall (rlambda (l)
                (if (atom l)
                    (pushend l result)
                    (mapcar #'recurse l)))
             lst)
    result))

(defun str (&rest args)
  (with-output-to-string (s)
    (apply (lambda (o) (princ o s))
           (remove-if-not #'identity (flatten args)))))

(defun lc (object)
  "Converts an object to a lowercase string"
  (string-downcase (str object)))

(defun uc (object)
  "Converts an object to an uppercase string"
  (string-upcase (str object)))

(defun keyw (&rest args)
  (values (intern (uc args)) "KEYWORD"))

(defun symb (&rest args)
  (values (intern (uc args))))

(eval-when (:compile-toplevel :load-toplevel :execute)

   ; **** Lambda expressions à la Arc by Brad Ediger ***
   ; CL-USER> ([+ 1 _] 10)
   ; 11
   ; CL-USER> ([+ _ __] 1 2)
   ; 3
   ; added possibility of functional position for _ or __,
   ; based on {} reader
   ; CL-USER> ([_ 1 2] (list 1 2 3))
   ; (2 3)

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

   ; Accessor reader macro :
   ; > (setf a (list 1 2 3))
   ; > {a 2} 
   ; 3
   ; > (setf f (lambda (x) (+ x 2)))
   ; > {f 3}
   ; 5
   ; > {"abcde" 2 4}
   ; "cd"
   ; > {"abcde" 2 -1}
   ; "cde"
   ; > (defstruct s a b)
   ; > {(make-s :a 1 :b 2) 'a}
   ; > 1
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

   ;
   ; Function composition reader macro à la Arc:
   ; (car!list 2) is equivalent to (car (list 2))
   ;
  
   (defun compose-reader (stream char)
     (declare (ignore char))
     (prog1
       (read-delimited-list #\) stream)
       (unread-char #\) stream)))
   
   (defun enable-compose ()
     (set-macro-character #\! #'compose-reader))
   
   (enable-compose))

(defun o (&rest fns)
  "Compose functions"
  (if (cdr fns)
      (lambda (&rest args) (funcall (car fns) (apply (apply #'o (cdr fns)) args)))
      (car fns)))

(defun pick (object &rest places)
  "Select several slots from a struct, object or sequence
   > (pick (list 1 2 3) 0 1)
   (list 1 2)
   > (pick (make-s :a 1 :b 2 :c 3) :a :c)
   (list 1 3)
   > (pick "abcd" 2 4)
   (list #\b #\d)
   > (pick (lambda (x) (+ x 1)) 2 4)
   (list 3 5)
  "
  (mapcar [object _] places))

(defun in (seq elmt)
	"does seq contain elmt ?"
	(not (null (position elmt seq :test #'equal))))

;
; Anaphoric macros
;

(defmacro awith (form &rest body)
  "Executes <body> with <it> bound to <form>"
	`(let ((it ,form)) ,@body))

(defmacro aif (test then &optional else)
  `(awith ,test
     (if it
         ,then
         ,else)))

(defmacro awhen (test &rest body)
  "Executes <body> with <it> bound to result of evaluating <test> if this result is not nil"
  `(awith ,test
     (when it
           ,@body)))
 
(defmacro aunless (test &rest body)
  "Executes <body> with <it> bound to result of evaluating <test> if this result is nil"
  `(awith ,test
     (unless it ,@body)))

(defmacro awhile (test &rest body)
  "Loops on <body> with <it> bound to result of evaluating <test> as long as this result is nil"
  `(awith ,test
      (loop while it
            do (progn ,@body))))

(defmacro aand (test &rest tests)
  "n "
  `(awith ,test
      (and it ,@tests)))

(defmacro acond (&rest forms)
  "Anaphoric cond : like a regular cond, except the result of evaluating the condition form can be accessed as <it>"
  (let ((blockname (gensym)))
    `(block ,blockname
      ,@(loop for form in forms
          collect `(awhen ,(car form) (return-from ,blockname ,(cadr form))))
      nil)))

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
                  (setf (car result) (str (car result) {re i}))))
     (when (< (length result) 3)
        (push (car result) result)
        (setf (cadr result) ""))
     (when (char/= {re 0} #\/)
        (setf (car result) (str {re 0} (car result))))
     (when (in (car result) #\i)
        (setf (caddr result) (str "(?i)" (caddr result))))
     (setf (cadr result) (cl-ppcre::regex-replace-all "\\\\/" (cadr result) "/"))
     (apply #'values (reverse result))))

(defgeneric ~ (re string-or-list &optional match-nb1 match-nb2))

(defmethod ~ ((re string) (string-or-list string) &optional match-nb1 match-nb2)
   "Returns nil if regular expression <re> does not match the string
    returns the part of the string that matches <re> and all grouped matches ()
    if <match-nb1> is not nil, only match number <match-nb1> will be returned
    
    example : (re \"\w+(\d)\" \"ab2cc\")"
  (multiple-value-bind (regexp subre flags) (parse-re re)
    (declare (ignorable subre))
    (let ((match-indexes (cl-ppcre:all-matches regexp string-or-list)))
       (when match-indexes
         (funcall (if match-nb2
                      [nth match-nb2 _]
                      (if (in flags #\g)
                              #'identity
                              #'first))
           (loop for i from 0
                       below (if (in flags #\g) (length match-indexes) 1)
                       by 2
              collect
              (funcall (if match-nb1 [nth match-nb1 _] #'identity)
                (let ((m (multiple-value-list (cl-ppcre:scan-to-strings regexp string-or-list :start {match-indexes i}))))
                  (if {m 1} (cons (car m) (vector-to-list* (cadr m))) nil)))))))))

(defmethod ~ ((re string) (string-or-list list) &optional match-nb1 match-nb2)
  "returns the elements of list that match <re>
    
   example : (re \"\w+(\d)\" \"ab2cc\")"
  (let ((matching (remove-if-not [cl-ppcre:scan (parse-re re) _] string-or-list)))
       (if match-nb1
           (mapcar [~ re _ match-nb1 match-nb2] matching)
           matching)))

(defgeneric /~ (re obj))

(defmethod /~ ((re string) (obj string))
  "Returns nil if <re> matches string obj
   returns the string if <re> does not match"
  (if (cl-ppcre::scan (parse-re re) obj)
      nil
      obj))

(defmethod /~ ((re string) (obj list))
  "Returns the elements of list <obj> that do not match <re>
    example: (/~ \"\w{3}\" (list \"aaa\" \"bb\" \"ccc\"))"
  (remove-if [cl-ppcre::scan (parse-re re) _]
             obj))

(defgeneric ~s (re obj &optional match-nb))

(defmethod ~s ((re string) (obj string) &optional match-nb)
  "Replaces all substrings that match <re> in <string> by <replacement>.
  <flags> can contain Perl regexp flags like g
  replacement can be a string which may contain the special substrings \\& for the whole match, \\` for the part of target-string before the match, \\' for the part of target-string after the match, \\N or \\{N} for the Nth register where N is a positive integer."
  (multiple-value-bind (regexp subre flags) (parse-re re)
    (let ((matches (~ re obj match-nb)))
      (values
        (if (in flags #\g)
          (cl-ppcre::regex-replace-all regexp obj subre)
          (cl-ppcre::regex-replace     regexp obj subre))
        matches))))

(defmethod ~s ((re string) (obj list) &optional match-nb)
  (mapcar [~s re _ match-nb] obj))

(defun resplit (re str)
  (cl-ppcre:split (parse-re re) str))

(defun lines (str)
  (resplit "/\\r\\n|\\n/" str))

(defun split (sep seq)
  (if (= (length sep) 0)
      (coerce seq 'list)
      (loop for start = 0 then (+ end (length sep))
            for end = (search sep seq :start2 start)
            collecting {seq start (or end (length seq))}
            while end)))

(defun join (join-seq &rest seq-lists)
  (awith (flatten seq-lists)
    (if (cdr it)
        (concatenate (class-of join-seq) (car it) join-seq (join join-seq (cdr it)))
        (car it))))

(defun x (seq nb)
  "returns a sequence composed of <nb> times <sequence>
   > (x \"a\" 3)
   \"aaa\""
  (let ((s ""))
    (loop for i below nb do (setf s (concatenate (class-of seq) s seq)))
    (coerce s (class-of seq))))

(defgeneric range (a &optional b step)
	(:documentation "Builds a range of numbers or chars
   >(range 1 10)
   (1 2 3 4 5 6 7 8 9 10)
   >(range -10 -100 -30)
   (-10 -40 -70 -100)
   >(range 10)
   (0 1 2 3 4 5 6 7 8 9 10)
   >(range #\a #\d)
   (#\a #\b #\c #\d)
  "))

(defmethod range ((a fixnum) &optional b (step 1))
	(let ((start (if b a 0))
        (end (if b b a)))
		(if (> step 0)
			(loop for x from start to end by step collect x)		
			(loop for x from start downto end by (- step) collect x))))

(defmethod range ((a character) &optional b (step 1))
	(mapcar #'code-char
          (range (char-code a)
                 (if b (char-code b) nil)
                 step)))

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
  starting at offset. If offset is negative, start from end-offset. Read at most limit characters.
  (glob \"http://www.google.com\")
  (glob \"/etc/hosts\" :offset 10)
  (glob stream :offset 2 :limit 20)
   "
  (when (and limit (< limit 0))
    (error "Limit must be a positive integer (number of lines)"))
  (if (looks-like-file path-or-stream)
    (with-open-file (s path-or-stream)
       (let ((fl (file-length s)))
         (when (< offset 0)
           (setf offset (max (+ fl offset) 0)))
         (let ((seq (make-array (aif limit (min it (- fl offset)) (- fl offset))
                          :element-type 'character
                          :fill-pointer t)))
              (file-position s offset)
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
                  for remoffset = posoffset then (- remoffset pos)
                  while (and (plusp pos)
                             (or (not limit)
                                 (< index (+ limit 4096))))
                  do (when (> (+ index pos) posoffset)
                        (if binary
                          (progn
                             (adjust-array seq (or limit (- (+ index pos) posoffset)))
                             (replace seq buf
                                  :start1 index
                                  :start2 remoffset))
                          (setf seq (concatenate 'string seq {{buf 0 pos} remoffset -1})))))
             (when (and binary (equal seq "")) (setf seq #()))
             (when (< offset 0)
               (setf seq (if (> (- offset) (length seq))
                             ""
                             {seq (- -1 offset) -1})))
             (aif limit {seq 0 limit} seq)))))

(defun unglob (filename object &key if-exists binary readable)
  "Print <object> into <filename>"
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
  "Go back n lines in a file stream"
  (let ((buf (make-string 4096))
        (lines 0))
      (loop until (or (>= lines n) (< (length buf) 4096))
            do (file-position file-stream (max 0 (- (file-position file-stream) 4096)))
               (awith (read-sequence buf file-stream)
                  (file-position file-stream (- (file-position file-stream) it)))
               (incf lines (count #\Newline buf)))
      (if (> lines n)
        (file-position file-stream (+ (file-position file-stream)
                                        (length (~ (str "/^" (x "[^\\n]*\\n" (- lines n)) "/")
                                                   buf 0)))))))

(defmacro with-each-fline (args &rest body)
  (destructuring-bind (path-or-stream &key (offset 0) limit) args
    `(block read-loop
        (let ((offset ,offset)
              (limit ,limit))
          (when (and limit (< limit 0))
            (error "Limit must be a positive integer (number of lines)"))
          (when (and (not (looks-like-file ,path-or-stream))
                     (< offset 0))
            ; we have to read the whole thing
            (let ((done-lines 0))
              (loop for it in {(resplit "/\\n/" (glob ,path-or-stream)) offset (+ offset limit)}
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
      (setf lines (nconc lines (list (funcall fn it)))))
    lines))

(defmacro with-temporary-file (assignment &rest body)
  (destructuring-bind (filename extension) assignment
    `(let ((,filename (str "/tmp/highres_" (get-internal-real-time) (random 100) "." ,extension)))
        (prog1
           (progn
              ,@body)
           (delete-file ,filename)))))

;
; Comparison of results of function application : 
; (f= #'sin a b) is equivalent to (= (sin a) (sin b))
;

(defmacro defunfcom (&rest fns)
  `(progn
      ,@(loop for fn in fns
              collect `(defun ,(symb "F" fn) (f &rest objects)
                          ,(format nil "Compares with ~A the results of applying f to each object ~% Ex : (f~A #'sin 1 2) is equivalent to (~A (sin 1) (sin 2))" fn fn fn)
                          (apply (function ,fn) (mapcar f objects))))))

(defunfcom = /= < > <= >=)

(defun f-equal (f x y)
  (equal {f x} {f y}))

(defun rpad (string chars &key (with " "))
  (str string (x (str with) (max 0 (- chars (length string))))))

(defun lpad (string chars &key (with " "))
  (str (x (str with) (max 0 (- chars (length string)))) string))
    
; sh based on run-prog-collect-output from stumpwm (GPL)
(defun sh (command)
  "run a command and read its output."
  (with-input-from-string (ar command)
    #+allegro (with-output-to-string (s) (excl:run-shell-command +shell+ :output s :wait t :input ar))
    #+clisp   (with-output-to-string (s)
                (let ((out (ext:run-program "/bin/sh" :arguments () :input ar :wait t :output :stream)))
                  (glob out)))
    #+cmu     (with-output-to-string (s) (ext:run-program +shell+ () :input ar :output s :error s :wait t))
    #+sbcl    (with-output-to-string (s) (sb-ext:run-program +shell+ () :input ar :output s :error s :wait t))
    #+ccl     (with-output-to-string (s) (ccl:run-program +shell+ () :wait t :output s :error t :input ar))
    #+abcl    (with-output-to-string (s) (ext:run-shell-command (str +shell+ " -c \"" (~s "/\"/\\\"/g" command) "\"") :output s))
    #-(or allegro clisp cmu sbcl ccl abcl)
              (error 'not-implemented :proc (list 'pipe-input prog args))))

; get-env from stumpwm (GPL) (also found in the CL cookbook) 
#-abcl ;abcl has it predefined
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
                        (push (cons (keyw var) (string val)) ext:*environment-list*)))
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
  (if (probe-file file)
    (delete-file file)
    nil))

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
  (mapcar [_ o] (keys o)))

#-abcl
(defun slot-type (class slot)
  (dolist (s (closer-mop:class-slots (find-class class)))
     (when (eql (closer-mop:slot-definition-name s) slot)
        (return-from slot-type (closer-mop:slot-definition-type s)))))

(defmacro defstruct-and-export (structure &rest members)
	(append
	  `(progn
       ,(append `(defstruct ,structure ,@members))
       ,`(export ,`(quote ,(symb "MAKE-" (symbol-name structure))))
       ,`(export ,`(quote ,(symb "COPY-" (symbol-name structure)))))
     (mapcar
       #'(lambda (member)
           `(export ,`(quote ,(symb (symbol-name structure) "-"
                                    (symbol-name (if (listp member) (car member) member))))))
       members)))

(defun fsave (object dir &key timestamped id id-slot)
  (let ((lock (str dir "/.cl-arno_lock")))
    (unless timestamped
      (loop while (ls lock) do (sleep 0.1))
      (unglob lock ""))
    (unwind-protect
      (progn
        (unless id (setf id (aif (and id-slot {object id-slot})
                                 it
                                 (let ((items (~ "/^.*\\/(.*)(###|$)/" (!~ "/\\/\\.[^\\/]*$/" (ls dir)) 1)))
                                    (if items
                                        (if (= (length (~ "/^\\d+$/" items)) (length items))
                                            (+ 1 (apply #'max (mapcar #'read-from-string items)))
                                            (let ((i (str (gensym)))) ; TODO: this is very stupid (don't use as is)
                                                 (loop while (ls i) do (setf i (str (gensym))))
                                                 i))
                                        0)))))
        (when id-slot (setf {object id-slot} id))
        (let ((file (str dir "/" id (if timestamped
                                      (str "###" (apply #'format
                                                        (append '(nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D")
                                                                 (pick (multiple-value-list (get-decoded-time))
                                                                       5 4 3 2 1 0))))))))
          (unglob file object :readable t :if-exists :supersede)))
      (unless timestamped
        (rm lock)))
      id))

(defun fload (dir id &key version)
  (unless version
    (awhen (~ (str "/\\/" id "###(\\d+)$/") (ls dir) 1)
      (setf version (apply #'max (mapcar #'read-from-string it)))))
  (read-from-string (glob (str dir "/" id (aif version (str "###" it))))))

(defun fselect (from &key key value limit)
  (let ((lock (str from "/.cl-arno_lock"))
        (loaded 0))
      (loop while (ls lock) do (sleep 0.01))
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

(defstruct-and-export date
  s m h
  dow day month year
  dst zone)

(defun date-rfc-2822 (date)
  "Returns a date in the RFC-2822 format : Mon, 07 Aug 2006 12:34:56 -0600"
  (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
       {+days-abbr+ (date-dow date)}
       (date-day date)
       {+months-abbr+ (date-month date)}
       (date-year date)
       (date-h date)
       (date-m date)
       (date-s date)
       (~s "/^(-|)(\\d{3})$/\\{1}0\\2/" (str (* 100 (date-zone date))))))

(defun date-gnu (date format)
  (sh (str "date -d '" (date-rfc-2822 date) "' +'" format "'")))

(defun strtout (str)
  "Converts a string to a CL universal time"
  (let ((result (sh (str "date -d \"" str "\" +%s"))))
    (if (/~ "/^(-|)\\d+\\n$/" result)
      (error "Invalid date string : ~A" str)
      (+ (read-from-string result) 2208988800))))

(defun date (&key ut miltime str zone)
  (multiple-value-bind (us um uh uday umonth uyear udow udaylight-p uzone)
                       (decode-universal-time 
                          (if (or ut str miltime)
                            (acond
                              (str (strtout it))
                              (miltime (let ((milstr (lpad (str it) 4 :with 0)))
                                          (strtout (str {milstr 0 2} ":" {milstr 2 4}))))
                              (ut ut))
                            (get-universal-time))
                          zone)
  (make-date
	  :s      us
	  :m      um
	  :h      uh
	  :day    uday
	  :month  umonth
	  :year   uyear
	  :dow    udow
	  :dst    udaylight-p
	  :zone   (or zone uzone)
  )))

(defun ut (&optional str-or-date)
  (declare (optimize debug))
  (if str-or-date
    (if (stringp str-or-date)
      (strtout str-or-date)
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

(defun decode-duration (duration)
  "Transforms a duration string : '1m 1d'... into a number of seconds
   Warning : approximative for months and years"
  (if (numberp duration)
      duration
      (apply #'+ (mapcar (lambda (dur)
                            (if (~ "/^\d+$/" dur) 
                                dur
                                (* (cond 
                                      ((~ "/s$/i" dur) 1)
                                      ((~ "/m$/ " dur) 60)
                                      ((~ "/h$/i" dur) 3600)
                                      ((~ "/d$/i" dur) (* 24 3600))
                                      ((~ "/w$/i" dur) (* 7 24 3600))
                                      ((~ "/M$/ " dur) (* 30 24 3600))
                                      ((~ "/y$/i" dur) (* 12 30 24 3600))
                                      (t (error "Can't decode duration")))
                                   (parse-integer {dur 0 -2}))))
                         (split " " duration)))))

(defun encode-duration (duration)
  "Transforms a number of seconds into a duration string : '1m 1d'...
   Warning : approximative for months and years"
  (~s "/\\s+$//"
     (let* ((du duration)
         (y  (floor du (* 12 30 24 3600)))
         (du (if (plusp du) (rem du (* 12 30 24 3600)) 0))
         (mo (floor du (* 30 24 3600)))
         (du (if (plusp du) (rem du (* 30 24 3600)) 0))
         (w  (floor du (* 7 24 3600)))
         (du (if (plusp du) (rem du (* 7 24 3600)) 0))
         (d  (floor du (* 24 3600)))
         (du (if (plusp du) (rem du (* 24 3600)) 0))
         (h  (floor du 3600))
         (du (if (plusp du) (rem du 3600) 0))
         (m  (floor du 60))
         (s  (if (plusp du) (rem du 60) 0)))
        (str (if (plusp y)  (str y  "Y "))
             (if (plusp mo) (str mo "M "))
             (if (plusp w)  (str w  "w "))
             (if (plusp d)  (str d  "d "))
             (if (plusp h)  (str h  "h "))
             (if (plusp m)  (str m  "m "))
             (if (plusp s)  (str s  "s"))))))

(defun d+ (date duration)
  "Adds a number of seconds or a a duration string : '1m 1d'... to a date
   Warning : approximative for adding months and years"
  (date :ut (+ (ut date) (if (numberp duration) duration (decode-duration duration)))
        :zone (date-zone date)))

(defun d- (date duration)
  "Removes a number of seconds or a a duration string : '1m 1d'... from a date
   Warning : approximative for adding months and years"
  (d+ date (- (if (numberp duration) duration (decode-duration duration)))))

(defun d> (&rest dates)
  (apply #'> (mapcar #'ut dates)))

(defun d< (&rest dates)
  (apply #'< (mapcar #'ut dates)))

(defun d= (&rest dates)
  (apply #'= (mapcar #'ut dates)))

(defun d/= (&rest dates)
  (apply #'/= (mapcar #'ut dates)))

(defun date-week (date)
  "ISO week number, with Monday as first day of week (1..53)"
  (read-from-string (sh (str "date -d '" (date-rfc-2822 date) "' +%V"))))

(defun date-wom (date)
  "Week of month (1..5)"
  (- (date-week date) (date-week (d- date (str (- (date-day date) 1) "d"))) -1))

(defun miltime (&optional (d (date)))
  "Returns a 'military' time format for date : 1243,22 for 12:43:22"
  (+ (* 100 (date-h d))
     (date-m d)
     (/ 100 (date-s d))))
  
(defun to-zone (date zone)
  "Convert date to timezone zone"
  (date :ut (ut date) :zone zone))

(defun y-m-d (date)
  "Date in format 2003-12-22"
  (str (date-year date) "-" (date-month date) "-" (date-day date)))

(defmacro xor (&rest args)
  "Exclusive OR : returns nil if nothing or more than one element in args is true, returns the only true element overwise. If more than one element is found to be true, the rest is not evaluated."
  `(block xor
    (let ((result nil))
      (loop for a in (list ,@args)
        do (when a
             (if result
               (return-from xor nil)
               (setf result a))))
    result)))

(defun memoize-to-disk (fn &key (dir "/tmp") prefix force-reset remember-last expire)
  (unless prefix
    (setf prefix (str "cl-arno-mem-" (~ "/\{([A-E0-9]+)\}/" (str fn) 1)))
    (setf force-reset t))
  (when force-reset (sh (str "rm -f " dir "/" prefix "#*" )))
  #'(lambda (&rest args) 
    (let* ((time (ut))
           (file (str dir "/"
                      prefix
                      "#" (sha256 (str args))))
           (lock (str file ".lock")))
       (loop while (ls lock) do (sleep 0.01))
       (when expire
         (loop for f in (~ (str "/\\/" prefix "#[^\\/]+/") (ls dir))
           do (when (> (- time (file-write-date f)) expire)
                (rm f))))
       (if (ls file)
           (read-from-string (glob file))
           (awith (apply fn args)
              (unglob lock "")
              (unwind-protect
                (progn
                  (when remember-last
                    (awith (~ (str "/\\/" prefix "#[^\\/]+/") (ls dir))
                      (when (>= (length it) remember-last)
                         (rm (first (sort it [> (file-write-date _) (file-write-date __)]))))))
                  (unglob file it :readable t))
                (rm lock))
              it)))))

(defun memoize (fn &key remember-last expire)
  (let ((cache (mkhash))
        (calls (mkhash)))
     #'(lambda (&rest args)
          (when expire
                (loop for time = (ut)
                      for call in (keys calls)
                      do (when (> (- time {calls call}) expire)
                            (remhash call cache)
                            (remhash call calls))))
          (when (or remember-last expire)
            (setf {calls args} (ut)))
          (multiple-value-bind (val hit) (gethash args cache)
            (if hit
                val
                (progn
                  (when (and remember-last
                             (>= (hash-table-count cache) remember-last))
                     (awith (first (sort (keys calls) [> {calls _} {calls __}]))
                       (remhash it calls)
                       (remhash it cache)))
                  (setf {cache args}
                        (apply fn args))))))))

(defmacro before (fn &rest body)
  "Redefines <fn> so that <body> gets executed first each time <fn> is called. <body> can access the arguments passed to fn through variable <args>. Will not work with inlined functions.
  
  Example :
    (defun a (x) (+ x 2))
    (before 'a (format t \"a to be called with args ~A\" args)) "
  (let ((sym (gensym)))
    `(progn
       (let ((,sym (fdefinition ,fn)))
         (setf (fdefinition ,fn)
               (lambda (&rest args)
                 ,@body
                 (apply ,sym args)))))))

(defmacro after (fn &rest body)
  "Redefines <fn> so that <body> gets executed next each time <fn> is called. <body> can access the arguments passed to fn through variable <args>. Will not work with inlined functions.
  
  Example :
    (defun a (x) (+ x 2))
    (after 'a (format t \"a was called with args ~A\" args)) "
  (let ((sym  (gensym)))
    `(progn
       (let ((,sym (fdefinition ,fn)))
         (setf (fdefinition ,fn)
               (lambda (&rest args)
                 (prog1 (apply ,sym args)
                        ,@body)))))))

(defun file-repl (file)
  "Watches file for additions, and evals the additions.
   I use it to send forms from Vim to the REPL.
   At the end, I have a full log of my session in the file."
  (let ((offset 0)
        (form nil))
    (loop do (sleep 1)
             (setf form nil)
             (with-open-file (f file)
                (file-position f offset)
                (loop until (eq form 'EOF)
                      do (setf form (read f nil 'EOF))
                         (unless (eq form 'EOF) (eval form)))
                (setf offset (file-position f))))))
