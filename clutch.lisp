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

(defpackage :clutch
    (:use     #:cl #:anaphora)
    (:export  #:date #:d- #:d+ #:d-delta #:ut #:miltime #:y-m-d #:date-wom #:date-week #:to-zone
              #:date-rfc-2822 #:date-rfc-3339 #:date-gnu #:decode-duration #:encode-duration
              #:d= #:d/= #:d> #:d< #:d<= #:d>=
              #:date-day #:date-year #:date-month #:date-h #:date-m #:date-s
              #:enable-arc-lambdas #:enable-brackets #:enable-compose #:defstruct-and-export 
              #:in #:range #:vector-to-list* #:flatten #:pick #:pushend #:pushendnew #:popend
              #:while #:awhile #:awith #:rlambda #:acond
              #:if-bind #:when-bind #:while-bind
              #:str #:lc #:uc #:ucfirst #:symb #:keyw #:~ #:~s #:/~ #:resplit #:split #:join #:x #:trim #:lpad #:rpad #:strip #:lines #:str-replace
              #:gulp #:ungulp #:gulplines #:with-each-fline #:mapflines #:file-lines #:filesize 
              #:f= #:f/= #:f> #:f< #:f<= #:f>= #:f-equal #:with-temporary-file #:it
              #:sh #:ls #:argv #:mkhash #:rm #:rmdir #:mkdir #:probe-dir #:getenv #:grep
              #:keys #:kvalues #:email-error
              #:md5 #:sha1 #:sha256 #:uuid
              #:memoize #:disk-store #:hash-store  #:before #:after #:o 
              #:+days+ #:+days-abbr+ #:+months+ #:+months-abbr+
              #:xor #:?)
    #-abcl (:export #:getenv))

(in-package :clutch)

(defvar +shell+ "/bin/bash")
(defvar +months-abbr+ (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar +months+ (list "" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
(defvar +days-abbr+ (list "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar +days+ (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun email-error (address)
  (let ((count@ 0))
    (loop for i across address
          when (char= i #\@)
          do (incf count@))
    (or (/= count@ 1)
        (> (length address) 254))))

(defmacro pushend (object lst)
  "Appends <object> to list <lst>"
  ` (setf ,lst (if ,lst
                   (nconc ,lst (list ,object))
                   (list ,object))))

(defmacro pushendnew (object lst &key test)
  "Appends <object> to list <lst> if <object> is not already in <lst> (using equality test <test>)"
  `(unless (in ,lst ,object :test (if ,test ,test #'equal))
      (pushend ,object ,lst)))

(defmacro popend (lst)
  "Removes the last element of list <lst> and returns it"
  `(prog1
     (car (last ,lst))
     (nbutlast ,lst)))

(defmacro rlambda (args &rest body)
  "Creates a lambda that can call itself
   by using function name 'recurse'"
  `(labels ((recurse ,args ,@body))
      #'recurse))

(defun flatten (&rest lst)
  "Recursively flatten a list of lists"
  (let ((result nil))
    (funcall (rlambda (l)
                (when l
                  (if (atom l)
                      (pushend l result)
                      (mapcar #'recurse l))))
             lst)
    result))

(defun str (&rest args)
  "Converts <args> to a string"
  (with-output-to-string (s)
    (mapcar (lambda (o) (princ o s))
            (remove nil (flatten args)))))

(defun keyw (&rest args)
  "Converts <args> to a keyword"
  (values (intern (uc args) "KEYWORD")))

(defun symb (&rest args)
  "Converts <args> to a symbol"
  (values (intern (uc args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
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

   (defgeneric access (object start &rest args))

   (defmethod access ((object null) start &rest args)
      nil)

   (defmethod access ((object sequence) (start number) &rest args)
      (let ((end (if args (car args) nil))
            (len (length object)))
        (when (or (if end (> start len)
                          (>= start len))
                  (and (minusp start)
                       (or (if end (< start (- (- len) 1)) (< start (- len))))))
          (error (str (when end "First ") "Index out of bounds")))
        (if end
            (if (or (> end len)
                    (and (minusp end)
                         (< end (- -1 len))))
              (error "Second index out of bounds")
              (subseq object (if (minusp start) (+ len 1 start)
                                 start)
                             (if (minusp end) (+ len 1 end)
                                 end)))
            (elt object (mod start len)))))

   (defmethod access ((object function) start &rest args)
      (apply object (cons start args)))

   (defmethod access ((object hash-table) start &rest args)
      (gethash start object))

   (defmethod access ((object standard-object) start &rest args)
      (slot-value object (symb start)))

   (defmethod access ((object structure-object) start &rest args)
      (slot-value object (symb start)))
   
   (defmacro setaccess (object start &rest args)
      `(cond ((null ,object) nil)
             ((and (or (listp ,object) (vectorp ,object)) (numberp ,start))
                (let ((end ,(and (cdr args) (car args)))
                      (len (length ,object)))
                   (when (or (> ,start len)
                             (<= ,start (- len)))
                        (error "Index out of bounds"))
                   (if end
                      (if (or (> end len)
                              (and (minusp end)
                                   (< end (- -1 len))))
                          (error "Second index out of bounds")
                          (setf (subseq ,object (mod ,start len)
                                        (if (minusp end) (+ len 1 end)
                                            end))
                                ,(cadr args)))
                      (if (vectorp ,object)
                          (setf ,object (concatenate (type-of ,object)
                                                     (access ,object 0 ,start)
                                                     (make-sequence (append (butlast (type-of ,object)) (list 1)) 1 :initial-element ,(car args))
                                                     (access ,object (+ ,start 1) -1)))
                          (setf (elt ,object (mod ,start len)) ,(car args))))))
            ((hash-table-p ,object)
               (setf (gethash ,start ,object) ,(car args)))
            ((or (typep ,object 'structure-object) (typep ,object 'standard-object))
               (setf (slot-value ,object (symb ,start)) ,(car args)))))

   (defsetf access setaccess)

   (defun bracket-reader (stream char)
     (declare (ignore char))
     `(access ,@(read-delimited-list #\} stream t)))
   
   (defun enable-brackets ()
     (set-macro-character #\{ #'bracket-reader)
     (set-macro-character #\} (get-macro-character #\) nil)))
   
   (enable-brackets))

(eval-when (:compile-toplevel :load-toplevel :execute)

   ; **** Lambda expressions a la Arc by Brad Ediger ***
   ; CL-USER> ([+ 1 _] 10)
   ; 11
   ; CL-USER> ([+ _ __] 1 2)
   ; 3
   ; with added possibility of functional position for _ or __,
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

   (enable-arc-lambdas))

(defun lc (&rest object)
  "Converts an object to a lowercase string"
  (string-downcase (str object)))

(defun uc (&rest object)
  "Converts an object to an uppercase string"
  (string-upcase (str object)))

(defun ucfirst (&rest object)
  (let ((strd (str object)))
    (str (uc {strd 0}) {strd 1 -1})))

(defun split (sep seq)
  (if (= (length sep) 0)
      (mapcar #'str (coerce seq 'list))
      (loop for start = 0 then (+ end (length sep))
            for end = (search sep seq :start2 start)
            collecting (subseq seq start (or end (length seq)))
            while end)))

(defun o (&rest fns)
  "Compose functions <fns>"
  (if (cdr fns)
      (lambda (&rest args) (funcall (car fns) (apply (apply #'o (cdr fns)) args)))
      (car fns)))

;
; Comparison of results of function application : 
; (f= #'sin a b) is equivalent to (= (sin a) (sin b))
; Something generic like (=^sin a b) could be nicer, although
; I don't think it can be transformed to (= (sin a) (sin b)) using
; reader macros.
;

(defmacro defunfcom (&rest fns)
  `(progn
      ,@(loop for fn in fns
              collect `(defun ,(intern (concatenate 'string "F" (symbol-name fn))) (f &rest objects)
                          ,(format nil "Compares with ~A the results of applying f to each object ~% Ex : (f~A #'sin 1 2) is equivalent to (~A (sin 1) (sin 2))" fn fn fn)
                          (apply (function ,fn) (mapcar f objects))))))

(defunfcom = /= < > <= >=)

(defun f-equal (f x y)
  (equal {f x} {f y}))

(defun pick (object &rest places)
  "Select several slots from a struct, object or sequence
   > (pick (list 1 2 3) 0 1)
   (list 1 2)
   > (pick (make-s :a 1 :b 2 :c 3) :a :c)
   (list 1 3)
   > (pick \"abcd\" 2 4)
   (list #\b #\d)
   > (pick (lambda (x) (+ x 1)) 2 4)
   (list 3 5)
  "
  (mapcar (lambda (x) (access object x))
          places))

(defun in (seq obj &key (test #'equal))
	"Returns t if <seq> contains <obj> (using equality test <test>)"
	(not (null (position obj seq :test test))))

;
; Anaphoric macros
;

(defmacro awith (form &body body)
  "Evaluates <body> with <it> bound to <form>"
	`(let ((it ,form)) ,@body))

(defmacro acond (&rest forms)
  "Anaphoric cond : like a regular cond, except the result of evaluating the condition form can be accessed as <it>"
    (let ((blockname (gensym)))
        `(block ,blockname
              ,@(loop for form in forms
                        collect `(awhen ,(car form) (return-from ,blockname ,(cadr form))))
                    nil)))

(defmacro if-bind ((var test) then &optional else)
  "Evaluates <then with <var> bound to result of evaluating <test> if this result is not nil, <else> otherwise"
  `(let ((,var ,test))
     (if ,var
         ,then
         ,else)))

(defmacro when-bind ((var test) &body body)
  "Evaluates <body> with <var> bound to result of evaluating <test> if this result is not nil"
  `(let ((,var ,test))
     (when ,var
           ,@body)))
 
(defmacro awhile (test &body body)
  "Loops on <body> with <it> bound to result of evaluating <test> as long as this result is not nil"
  `(loop for it = ,test
         while it
         do (progn ,@body)))

(defmacro while-bind ((var test) &body body)
  "Loops on <body> with <var> bound to result of evaluating <test> as long as this result is not nil"
  `(loop for ,var = ,test
         while ,var
         do (progn ,@body)))

(defmacro while (test &body body)
  "Loops on <body> as long as <test> does not evaluates to nil"
  `(loop while ,test
         do (progn ,@body)))

(defun ? (test)
  "Returns nil if <test> evaluates to nil, t otherwise"
  (if test t nil))

(defun vector-to-list* (object)
  (declare (optimize speed))
  (let ((result (list nil))
        (length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
         (splice result (cdr splice)))
        ((= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))

(defun parse-re (re)
  (declare (optimize speed))
  (let ((result (list "")))
    (loop for i from 1 below (length re)
          do (if (and (char= {re i} #\/) (char/= {re (- i 1)} {"\\" 0})) 
                 (push "" result)
                 (setf (car result) (str (car result) {re i}))))
     (when (= (length result) 2)
       (push "" (cdr result)))
     (when (in (car result) #\i)
        (setf (caddr result) (str "(?i)" (caddr result))))
     (when (in (car result) #\x)
        (setf (caddr result) (cl-ppcre::regex-replace-all "\\s" (caddr result) "")))
     (setf (cadr result) (cl-ppcre::regex-replace-all "\\\\/" (cadr result) "/"))
     (apply #'values (reverse result))))

(defgeneric ~ (re string-or-list &optional capture-nb))

(defmethod ~ ((re string) (string-or-list string) &optional capture-nb)
   "Returns nil if regular expression <re> does not match the string
    returns the part of the string that matches <re> and all grouped matches ()
    if <capture-nb> is not nil, only capture number <capture-nb> will be returned
    for each match.
    
    example : (~ \"/\w+(\d)/\" \"ab2cc\")"
  (declare (optimize speed))
  (multiple-value-bind (regexp subre flags) (parse-re re)
    (declare (ignorable subre))
    (let ((result nil))
      (cl-ppcre:do-matches (match-start match-end regexp (str string-or-list) result)
        (pushend (funcall (aif capture-nb [_ it] #'identity)
                    (awith (multiple-value-list (cl-ppcre:scan-to-strings regexp (str string-or-list) :start match-start :end match-end))
                      (cons (car it) (vector-to-list* (cadr it)))))
                 result)
        (when (and result (not (in flags #\g)))
          (return (car result)))))))

(defmethod ~ ((re string) (string-or-list pathname) &optional capture-nb)
  (declare (optimize speed))
  (~ re (str string-or-list) capture-nb))

(defmethod ~ ((re string) (string-or-list list) &optional capture-nb)
  "returns the elements of list that match <re>
    
   Specify a <capture-nb> value to return capture number <capture-nb> instead of the string that matches.

   example : (re \"\w+(\d)\" \"ab2cc\")"
  (declare (optimize speed))
  (let ((matching (remove-if-not [progn (unless (or (stringp _) (pathnamep _))
                                           (error "Element of list for ~~ cannot be assimilated to a string : ~A" _))
                                        (cl-ppcre:do-matches (a b (parse-re re) (str _)) (return t))]
                                 string-or-list)))
       (if capture-nb
           (mapcar [~ re _ capture-nb] matching)
           matching)))

(defgeneric /~ (re obj))

(defmethod /~ ((re string) (obj string))
  "Returns nil if <re> matches string obj
   returns the string if <re> does not match"
  (declare (optimize speed))
  (cl-ppcre:do-matches (a b (parse-re re) obj t) (return nil)))

(defmethod /~ ((re string) (obj list))
  "Returns the elements of list <obj> that do not match <re>
    example: (/~ \"\w{3}\" (list \"aaa\" \"bb\" \"ccc\"))"
  (declare (optimize speed))
  (remove-if-not [/~ re _]
             obj))

(defmethod /~ ((re string) (obj pathname))
  (declare (optimize speed))
  (/~ re (str obj)))

(defun str-replace (what with-what in &key count)
  (loop for pos = (search what (or result in)
                          :start2 (if pos (+ pos 1) 0))
        while (and pos (or (null count) (> count 0)))
        for result = (str {(or result in) 0 pos}
                          with-what
                          {(or result in) (+ pos (length what)) -1})
        do (when count (decf count))
        finally (return-from str-replace (or result in))))

(defgeneric ~s (re obj &optional capture-nb))

(defmethod ~s ((re string) (obj string) &optional capture-nb)
  "Replaces all substrings that match <re> in <string> by <replacement>.
  <flags> can contain Perl regexp flags like g
  replacement can be a string which may contain the special substrings \\& for the whole match, \\` for the part of target-string before the match, \\' for the part of target-string after the match, \\N or \\{N} for the Nth register where N is a positive integer."
  (declare (optimize speed))
  (multiple-value-bind (regexp subre flags) (parse-re re)
    (let ((matches (~ re obj capture-nb)))
      (when (in flags #\e)
        (let ((repl subre))
          (setf subre (lambda (&rest matches)
                        (let ($0 $1 $2 $3 $4 $5 $6 $7 $8 $9)
                          (loop for m in matches
                                for i from 0
                                do (setf (symbol-value (symb "$" i)) m))
                          (str (eval (read-from-string repl))))))))
      (values
        (if (in flags #\g)
          (cl-ppcre::regex-replace-all regexp obj subre :simple-calls t)
          (cl-ppcre::regex-replace     regexp obj subre :simple-calls t))
        matches))))

(defmethod ~s ((re string) (obj pathname) &optional capture-nb)
  (declare (optimize speed))
  (~s re (str obj) capture-nb))

(defmethod ~s ((re string) (obj list) &optional capture-nb)
  (declare (optimize speed))
  (mapcar [~s re _ capture-nb] obj))

(defun resplit (re str)
  (cl-ppcre:split (parse-re re) str))

(defun lines (str)
  (resplit "/\\r\\n|\\n/" str))

(defun join (join-seq &rest seq-lists)
  (awith (flatten seq-lists)
    (if (cdr it)
        (concatenate (class-of (car it)) (car it) 
                     (if (subtypep (type-of join-seq) 'sequence)
                         join-seq
                         (make-sequence (class-of (car it)) 1 :initial-element join-seq))
                     (join join-seq (cdr it)))
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

(defmacro with-stream ((name path-or-stream &key binary) &rest body)
   `(cond ((streamp ,path-or-stream)
             (let ((,name ,path-or-stream))
                ,@body))
          #-abcl
          ((and (stringp ,path-or-stream) 
                (> (length ,path-or-stream) 5)
                (string-equal {,path-or-stream 0 7} "http://"))
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
          ((or (stringp ,path-or-stream) (pathnamep ,path-or-stream))
             (with-open-file (,name ,path-or-stream)
                ,@body))))

(defun looks-like-file (path-or-stream)
  (not (or (streamp path-or-stream)
           (and (stringp path-or-stream) 
               (> (length path-or-stream) 5)
               (string-equal {path-or-stream 0 7} "http://")))))
  

(defun gulp (path-or-stream &key binary (offset 0) limit)
  "gulps the whole provided file, url or stream into a string or into a byte array if <binary>,
  starting at offset. If offset is negative, start from end-offset. Read at most limit characters.
  (gulp \"http://www.google.com\")
  (gulp \"/etc/hosts\" :offset 10)
  (gulp stream :offset 2 :limit 20)
   "
  (when (and limit (< limit 0))
    (error "Limit must be a positive integer (number of lines)"))
  (if (looks-like-file path-or-stream)
    (let ((el-type (if binary '(unsigned-byte 8) 'character)))
      (with-open-file (s path-or-stream :element-type el-type)
         (let ((fl (file-length s)))
           (when (< offset 0)
             (setf offset (max (+ fl offset) 0)))
           (let ((seq (make-array (aif limit (min it (- fl offset)) (- fl offset))
                            :element-type el-type
                            :fill-pointer t)))
                (file-position s offset)
                (setf (fill-pointer seq) (read-sequence seq s))
                seq))))
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
                  for remoffset = posoffset then (max (- remoffset pos) 0)
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
             (when (and binary (string= seq "")) (setf seq #()))
             (when (< offset 0)
               (setf seq (if (> (- offset) (length seq))
                             ""
                             {seq (- -1 offset) -1})))
             (aif limit {seq 0 limit} seq)))))

(defun ungulp (filename object &key if-exists binary readable)
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

(defun file-lines (file)
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

(defmacro with-each-fline ((path-or-stream &key (offset 0) limit as) &rest body)
  "Evaluates <body> for each line in <path-or-stream>, with the line
   available as <it>, and the line number as <@it>."
    (let ((linesymb (if as as 'it))
          (nbsymb (if as (symb '@ as) '@it)))
      `(block read-loop
          (let ((offset ,offset)
                (limit ,limit))
            (when (and limit (< limit 0))
              (error "Limit must be a positive integer (number of lines)"))
            (when (and (not (looks-like-file ,path-or-stream))
                       (< offset 0))
              ; we have to read the whole thing
              (let ((done-lines 0)
                    (,nbsymb offset))
                (loop for ,linesymb in {(lines (gulp ,path-or-stream)) offset (if limit (+ offset limit -1) -1)}
                      do (progn ,@body
                                (incf ,nbsymb)
                                (incf done-lines)))
                (return-from read-loop done-lines)))
            (with-stream (s ,path-or-stream)
                (when (and (looks-like-file ,path-or-stream)
                           (= (file-length s) 0))
                  (return-from read-loop 0))
                (when (and (looks-like-file ,path-or-stream)
                         (< offset 0))
                  ; we can read it backwards to set the file-position then set offset to 0
                  (file-position s (file-length s))
                  (back-n-lines s (- offset))
                  (setf offset 0))
                (let ((skipped-lines 0)
                      (done-lines 0)
                      (,nbsymb offset))
                  (do ((,linesymb (read-line s) (read-line s nil 'eof)))
                      ((eq ,linesymb 'eof) (values))
                      (if (>= skipped-lines offset)
                        (progn
                          (when (and limit (>= done-lines limit))
                            (return-from read-loop done-lines))
                          (incf done-lines)
                          (incf ,nbsymb)
                          ,@body)
                        (incf skipped-lines)))
                  done-lines))))))

(defun gulplines (path-or-stream &key (offset 0) limit)
  "Gulps the whole provided file, URL or stream into an array of its lines, optionally starting at line <offset> and limiting output to <limit> lines. <offset> can be negative, in which case reading will start -<offset> lines from the end."
  (let ((lines nil))
    (with-each-fline (path-or-stream :offset offset :limit limit)
      (pushend it lines))
    lines))

(defun mapflines (fn path-or-stream &key  (offset 0) limit)
  "Apply <fn> to each line of <path-or-stream>."
  (let ((lines nil))
    (with-each-fline (path-or-stream :offset offset :limit limit)
      (setf lines (nconc lines (list (funcall fn it)))))
    lines))

(defun grep (regexp path &key recursive matches-only names-only lines-only capture)
  "Check <path> for files lines matching <regexp>, recursively if <recursive>.
   By default, returns (filename line matches) for each line matching. If <capture> is 
   specified, only capture group <capture> will be returned in matches."
  (if (probe-dir path)
      (loop for file in (ls path :files-only t :recursive recursive)
            nconc (grep regexp file :matches-only matches-only :names-only names-only :lines-only lines-only :capture capture))
      (let ((result nil))
        (with-each-fline (path)
           (let ((matches (~ regexp it capture)))
              (when matches
                (cond 
                  (matches-only (pushend matches result))
                  (names-only   (pushendnew (probe-file path) result))
                  (lines-only   (pushend it result))
                  (t            (pushend (list (probe-file path) @it matches) result))))))
        result)))

(defmacro with-temporary-file ((filename extension) &rest body)
  "Evaluates <body> with a temporary file. <filename> is the variable you want to assign the filename to, and <extension> the extension you want the temporary file to have."
  `(let ((,filename (str "/tmp/highres_" (get-internal-real-time) (random 100) "." ,extension)))
      (prog1
         (progn
            ,@body)
         (delete-file ,filename))))

(defun trim (seq length)
  (if length
      {seq 0 (min (length seq) length)}
      seq))

;TODO: what if with is more than one char ??
(defun rpad (string chars &key (with " "))
  (awith (str string)
    (str it (x (str with) (max 0 (- chars (length it)))))))

(defun lpad (string chars &key (with " "))
  (awith (str string)
    (str (x (str with) (max 0 (- chars (length it)))) it)))
    
(defun strip (string &rest chars)
      (values (~s (if chars
                      (str "/(^[" (str chars) "]*|[" (str chars) "]*$)//g")
                      "/(^(\\s|\\n|\\r\\n)*|(\\s|\\n|\\r\\n)*$)//g")
                  string)))

; sh based on run-prog-collect-output from stumpwm (GPL)
; note : it might be nice to return (values stdin stdout) ?
(defun sh (command)
  "run a command and read its output."
  (with-input-from-string (ar command)
    #+allegro (with-output-to-string (s) (excl:run-shell-command +shell+ :output s :wait t :input ar))
    #+clisp   (with-output-to-string (s)
                (let ((out (ext:run-program "/bin/sh" :arguments () :input ar :wait t :output :stream)))
                  (gulp out)))
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

(defun probe-dir (path)
  "Returns <path> if <path> leads to a directory, nil otherwise"
  (probe-file (str path "/.")))

(defun ls (path &key recursive files-only dirs-only)
  "If <path> is a file, return <path>.
   If <path> is a directory, return its contents, recursively if <recursive>.
   Limit resulting list to files if <files-only> and to directories if <dirs-only>.
   Return nil if <path> is neither a file nor a directory."
  (if (probe-dir path)
      (let* ((contents (directory (str path "/*.*")))
             (dirs nil)
             (files nil))
         (remove nil
           (if (not (or recursive files-only dirs-only))
               contents
               (progn
                 (loop for subpath in contents
                       do (if (probe-dir subpath)
                              (pushend subpath dirs)
                              (pushend subpath files)))
                 (flatten (when (not dirs-only) files)
                          (when (not files-only) dirs)
                          (when recursive
                             (mapcar [ls _ :recursive recursive :files-only files-only :dirs-only dirs-only] dirs)))))))
      (if dirs-only
          nil
          (aif (probe-file path) (list it) nil))))

(defun rm (path &key recursive)
  "Deletes file <path>, or entire directory <path> if <recursive>."
  (if (probe-file path)
      (if (probe-dir path)
          (progn
             (unless recursive
                (error "'~A' is a directory and rm not called with recursive=t" path))
             (mapcar [rm _]
                     (ls path :recursive t :files-only t))
             (mapcar [rmdir _]
                     (sort (ls path :recursive t :dirs-only t)
                           #'> :key (o #'length #'str)))
             (rmdir path))
          (delete-file path))
      nil))

(defun mkdir (dir)
  "Creates directory <dir>"
  (awith (make-pathname :directory `(,(if (char= {dir 0} #\/) :absolute :relative) ,dir))
    (ensure-directories-exist it)
    it))

(defun rmdir (dir)
  "Deletes directory <dir> if it is empty, signals an error otherwise."
  (if (ls dir)
    (error "Directory '~A' is not empty" dir))
    #+:sbcl(progn (sb-posix:rmdir dir) t)
    #+:abcl(delete-file dir)
    #-(or :abcl :sbcl) (error "Not implemented"))

(defun mkhash (&rest args)
  (let ((hash (make-hash-table :test 'equal)))
       (loop for i from 0 below (length args) by 2
             do (setf {hash {args i}} {args (+ i 1)}))
       hash))

(defun slot-names (class)
  #+sbcl(mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class))
  #-sbcl(let ((elts (mapcar [str _]
                            (read-from-string (~s "/^.*?\\(/(/"
                                                  (with-output-to-string (s) (prin1 (funcall (symb "MAKE-" class)) s)))))))
          (loop for i from 1 below (length elts) by 2 collect {elts i})))

(defun keys (o)
  (cond ((hash-table-p o)
          (loop for k being each hash-key of o collect k))
        ((and (listp o) (symbolp {o 0}) (evenp (length o)))
          (loop for i from 0 below (length o) by 2 collect {o i}))
        ((and (listp o) (consp {o 0}))
          (mapcar #'car o))
        ((subtypep (type-of o) 'structure-object)
          (slot-names (class-of o)))
        (t nil)))

(defun kvalues (o)
  (apply #'pick (append (list o) (keys o))))

(defmacro defstruct-and-export (structure &rest members)
	(append
	  `(progn
       ,(append `(defstruct ,structure ,@members))
       ,`(export ,`(quote ,(intern (concatenate 'string "MAKE-" (symbol-name structure)))))
       ,`(export ,`(quote ,(intern (concatenate 'string "COPY-" (symbol-name structure))))))
     (mapcar
       #'(lambda (member)
           `(export ,`(quote ,(intern (concatenate 'string (symbol-name structure) "-"
                                                           (symbol-name (if (listp member) (car member) member)))))))
       members)))

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

(defun uuid-ns (ns)
  (case (keyw ns)
    (:DNS uuid:+namespace-dns+)
    (:URL uuid:+namespace-url+)
    (:OID uuid:+namespace-oid+)
    (:x500 uuid:+namespace-x500+)))

(defun uuid (&key ns name (v 4))
  "Generates a UUID with algorithm version <v> (4=random by default) of RFC 4122, with name <name> in namespace <ns>."
  (str (case v
         (1 (uuid:make-v1-uuid))                    ; time based
         (3 (uuid:make-v3-uuid (uuid-ns ns) name))  ; name based MD5
         (4 (uuid:make-v4-uuid))                    ; random
         (5 (uuid:make-v5-uuid (uuid-ns ns) name))  ; name based SHA1
         (otherwise (error "Incorrect UUID version : ~A" v)))))

(defstruct-and-export date
  s m h
  dow day month year
  dst zone)

(defun date-formatzone (date)
  "Returns a 4 figure zone string starting with + or -"
  (~s "/^(\\d)/+\\1/"
      (~s "/^(-|)(\\d{3})$/\\{1}0\\2/"
          (str (* -100 (+ (if (date-dst date) -1 0)
                              (date-zone date)))))))

(defun date-rfc-3339 (&optional date)
  "Returns a date in the RFC-3339 format : 1937-01-01T12:00:27.87+00:20"
  (unless date (setf date (date)))
  (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A"
         (date-year date)
         (date-month date)
         (date-day date)
         (date-h date)
         (date-m date)
         (date-s date)
         (if (zerop (date-zone date))
             "Z"
             (awith (date-formatzone date)
               (str {it 0 3} ":" {it 2 -2})))))


(defun date-rfc-2822 (&optional date)
  "Returns a date in the RFC-2822 format : Mon, 07 Aug 2006 12:34:56 -0600"
  (unless date (setf date (date)))
  (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
       {+days-abbr+ (date-dow date)}
       (date-day date)
       {+months-abbr+ (date-month date)}
       (date-year date)
       (date-h date)
       (date-m date)
       (date-s date)
       (date-formatzone date)))

(defun date-gnu (date format)
  (strip (sh (str "date -d '" (date-rfc-2822 date) "' +'" format "'"))))

(defun strtout (str)
  "Converts a string to a Common Lisp universal-time"
  (let ((result (sh (str "date -d \"" str "\" +%s"))))
    (if (/~ "/^(-|)\\d+\\n$/" result)
      (error "Invalid date string : ~A" str)
      (+ (read-from-string result) 2208988800))))

(defun date (&key ut miltime str zone)
  "Returns a date structure based on either Common Lisp universal-time <ut>, 'military' (i.e. 1234 for today 12:34) time <miltime>, or string <str>, in timezone <zone>."
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
	  :zone   uzone
  )))

(defun ut (&optional str-or-date &key zone)
  "Returns the current Common Lisp universal-time, or if <str-or-date> is specified, the universal-time described by that string or date structure"
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
  "Delta in seconds between <date1> and <date2>"
  (- (ut date1) (ut date2)))

(defun decode-duration (duration)
  "Transforms a duration string : '1m 1d'... into an approximated number of seconds"
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
                                      ((~ "/y$/i" dur) (* 365 24 3600))
                                      (t (error "Can't decode duration")))
                                   (parse-integer {dur 0 -2}))))
                         (split " " duration)))))

(defun encode-duration (duration)
  "Transforms a number of seconds into a duration string : '1m 1d'... into an approximated number of seconds"
  (~s "/\\s+$//"
     (let* ((du duration)
         (y  (floor du (* 365 24 3600)))
         (du (if (plusp du) (rem du (* 365 24 3600)) 0))
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
        (str (if (plusp y)  (str y  "y "))
             (if (plusp mo) (str mo "M "))
             (if (plusp w)  (str w  "w "))
             (if (plusp d)  (str d  "d "))
             (if (plusp h)  (str h  "h "))
             (if (plusp m)  (str m  "m "))
             (if (plusp s)  (str s  "s"))))))

(defun d+ (date duration)
  "Adds a number of seconds or a duration string : '1m 1d'... to a date
   Warning : approximative for duration strings"
  (date :ut (+ (ut date) (if (numberp duration) duration (decode-duration duration)))
        :zone (date-zone date)))

(defun d- (date duration)
  "Removes a number of seconds or a duration string : '1m 1d'... from a date
   Warning : approximative for duration strings"
  (d+ date (- (if (numberp duration) duration (decode-duration duration)))))

(defun d> (&rest dates)
  "Are <dates> in strict reverse order ?"
  (apply #'> (mapcar #'ut dates)))

(defun d< (&rest dates)
  "Are <dates> in strict order ?"
  (apply #'< (mapcar #'ut dates)))

(defun d>= (&rest dates)
  "Are <dates> in reverse order ?"
  (apply #'>= (mapcar #'ut dates)))

(defun d<= (&rest dates)
  "Are <dates> in order ?"
  (apply #'<= (mapcar #'ut dates)))

(defun d= (&rest dates)
  "Are <dates> identical ?"
  (apply #'= (mapcar #'ut dates)))

(defun d/= (&rest dates)
  "Are <dates> different ?"
  (apply #'/= (mapcar #'ut dates)))

(defun date-week (date)
  "ISO week number, with Monday as first day of week (1..53)"
  (read-from-string (sh (str "date -d '" (date-rfc-2822 date) "' +%V"))))

(defun date-wom (date)
  "Week of month (1..5)"
  (- (date-week date) (date-week (d- date (str (- (date-day date) 1) "d"))) -1))

(defun miltime (&optional (d (date)))
  "Returns a 'military' time format for date : 1243,22 for 12:43:22"
  (float (+ (* 100 (date-h d))
            (date-m d)
            (/ (date-s d) 100))))
  
(defun to-zone (date zone)
  "Convert date to timezone zone"
  (date :ut (ut date) :zone zone))

(defun y-m-d (date)
  "Date in format YYYY-MM-DD"
  (str (lpad (date-year  date) 4 :with 0) "-"
       (lpad (date-month date) 2 :with 0) "-"
       (lpad (date-day   date) 2 :with 0)))

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

(defun serialize (obj)
  #-abcl(flexi-streams:with-output-to-sequence (s)
          (cl-store:store obj s))
  #+abcl(prin1-to-string obj))

(defclass store () ())
(defclass hash-store (store) ((hash :accessor hash :initform (mkhash))))
(defclass disk-store (store)
  ((dir :accessor dir :initarg :dir :initform "/tmp")
   (force-reset :accessor force-reset :initarg :force-reset)
   (prefix :accessor prefix :initarg :prefix :initform "")))

(defmethod memo-disk-fname ((store disk-store) args)
  (str (dir store) "/" (prefix store) "#" (sha256 (serialize args))))

(defmethod memo-disk-fre ((store disk-store))
  (str "/\\/" (prefix store) "#[^\\/]+$/"))

(defmethod initialize-instance :after ((store disk-store) &key)
  (unless (prefix store)
    (setf (prefix store) (str "clutch-mem-" (uuid)))
    (setf (force-reset store) t))
  (when (force-reset store) (mapcar #'rm (~ (memo-disk-fre store) (ls (dir store))))))

(defmethod memo-store ((store hash-store) key value)
  (setf {(hash store) key} value)
  value)

(defmethod memo-store ((store disk-store) key value)
  (let ((file (memo-disk-fname store key)))
    #+abcl(ungulp file value :readable t) #-abcl(cl-store:store value file))
  value)

(defmethod memo-rm-entries-older-than ((store hash-store) time)
  (loop for call in (keys (hash store))
          do (when (< (cadr {(hash store) call}) time)
               (remhash call (hash store)))))

(defmethod memo-rm-entries-older-than ((store disk-store) time)
  (loop for f in (~ (memo-disk-fre store)
                    (ls (dir store)))
        do (when (< (file-write-date f) time)
              (rm f))))

(defmethod memo-rm-oldest-entries ((store hash-store) keep-nb)
  (when (>= (hash-table-count (hash store)) keep-nb)
      (awith (sort (keys (hash store)) [or (< {{(hash store) _} -2} {{(hash store) __} -2})
                                   (< {{(hash store) _} -1} {{(hash store) __} -1})])
        (while (>= (length it) keep-nb)
           (remhash (pop it) (hash store))))))

(defmethod memo-rm-oldest-entries ((store disk-store) keep-nb)
  (let ((files (~ (memo-disk-fre store)
                  (ls (dir store)))))
     (when (>= (length files) keep-nb)
       (awith (sort files #'< :key #'file-write-date)
         (while (>= (length it) keep-nb)
           (rm (pop it)))))))

(defmethod memo-get ((store hash-store) key)
  (gethash key (hash store)))

(defmethod memo-get ((store disk-store) key)
  (let* ((file (memo-disk-fname store key))
         (existsp (ls file)))
    (values (when existsp
               (progn #+abcl(read-from-string (gulp file))
                      #-abcl(cl-store:restore file)))
            existsp)))

(defmethod memo-rm ((store hash-store) key)
  (remhash key (hash store)))

(defmethod memo-rm ((store disk-store) key)
  (awith (memo-disk-fname store key)
    (when (ls it)
       (rm i))))

(defun memoize (fn &key remember-last expire store)
  "Returns a memoized version of function <fn>. If <remember-last> is specified, it will limit the number of remembered results. If <expire> is specified, it will limit the number of seconds a result is remembered for."
  (let ((cache (or store (make-instance 'hash-store)))
        (cycle 0))
     #'(lambda (&rest args)
          (if (eq (first (last args 2)) :_expire_entry)
            (memo-rm cache args)
            (let ((utc (ut)))
              (when expire
                 (memo-rm-entries-older-than cache (- utc expire)))
              (multiple-value-bind (val hit) (memo-get cache args)
                (when remember-last
                   (memo-rm-oldest-entries cache remember-last)
                   (incf cycle)
                   (when (= cycle most-positive-fixnum) (setf cycle 0)))
                (apply #'values
                  (if hit
                      (let ((newval (append (nbutlast val 2) (list utc cycle))))
                        (when (or remember-last expire)
                           (memo-store cache args newval))
                        newval)
                      (memo-store cache
                                  args
                                  (append (multiple-value-list (apply fn args)) (list utc cycle)))))))))))

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
