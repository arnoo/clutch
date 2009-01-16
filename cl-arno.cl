(require 'cl-ppcre)
(require 'drakma)
(require 'clsql)

; **** Lambda expressions ala Arc by Brad Ediger ***
;CL-USER> ([+ 1 _] 10)
;11
;CL-USER> ([+ _ __] 1 2)
;3

(defun square-bracket-reader (stream char)
  (declare (ignore char))
  `(lambda (&optional ,(intern "_") ,(intern "__"))
           (declare (ignorable ,(intern "_") ,(intern "__")))
           ,(read-delimited-list #\] stream t)))

(defun enable-arc-lambdas ()
  (set-macro-character #\[ #'square-bracket-reader)
  (set-macro-character #\] (get-macro-character #\) nil)))

(enable-arc-lambdas)

;*** in ala python ***
(defun in (seq elmt)
	"does seq contain elmt ?"
	(if (position elmt seq :test #'equal) t nil))

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

; mkstr by P.G. (On Lisp)
(defun mkstr (&rest args)
  (with-output-to-string (s)
     (dolist (a args) (princ a s))))

(defun str (&rest args)
  (apply #'mkstr (remove-if-not #'identity args)))

(defmacro str+= (place &rest args)
  `(setf ,place (apply #'str (list ,place ,@args)))
  )

; reread by P.G. (On Lisp)
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))


; symb by P.G. (On Lisp)
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))



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
             do (if (and (char= (elt re i) #\/) (char/= (elt re (- i 1)) (elt "\\" 0))) 
                    (push "" result)
                    (setf (car result) (mkstr (car result) (elt re i)))))
       (when (< (length result) 3) (push (car result) result) (setf (cadr result) ""))
       (if (char/= (elt re 0) #\/) (setf (car result) (mkstr (elt re 0) (car result))))
       (if (in (car result) #\i) (setf (caddr result) (mkstr "(?i)" (caddr result))))
       (reverse result)))

;*** Regexp match ala Perl ***
(defun ~ (re string-or-list &optional match-nb)
  "If <string-or-list> is a string:
    returns nil if regular expression <re> does not match the string
    returns the part of the string that matches <re> and all grouped matches ()
    if <match-nb> is not nil, only match number <match-nb> will be returned
   if <string-or-list> is a list
    returns the elements of that list that match <re>
    
    example : (re \"\w+(\d)\" \"ab2cc\")"
    (declare (optimize debug))
  (destructuring-bind (regexp subre flags) (parse-re re)
    (declare (ignorable subre))
    (if (listp string-or-list)
        (remove-if-not [cl-ppcre:scan regexp _] string-or-list)
        (let ((match-indexes (cl-ppcre:all-matches regexp string-or-list)))
           (when match-indexes
             (unless (in flags #\g) (setf (cddr match-indexes) nil))
             (apply (if (in flags #\g) #'identity [first _]) (list
               (loop for i from 0 below (length match-indexes) by 2 collect
                  (apply (if match-nb [nth match-nb _] #'identity) (list
                      (let ((m (multiple-value-list (cl-ppcre:scan-to-strings regexp string-or-list :start (elt match-indexes i)))))
                        (if (nth 1 m) (cons (car m) (vector-to-list* (cadr m))) nil))))))))))))


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

;*** Regexp substitution ala Perl (or nearly...) ***
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

; split adapted from lisp-magick's string-split
(defun split (sep str)
  (if (string-equal sep "") (resplit "//" str)
    (loop for start = 0 then (+ end (length sep))
                 for end = (search sep str :start2 start)
                        collecting (subseq str start end)
                               while end)))

(defun join (join-string string-list)
  (if (> (length string-list) 1) (mkstr (car string-list) join-string (join join-string (cdr string-list))) (car string-list)))

;*** x a la perl/python/ruby *** TODO : rendre universel pour tout type de sequence
(defun x (string number)
  "returns a string composed of <number> times <string>"
  (if (> number 1) (concatenate 'string string (x string (- number 1)))
      (if (<= number 0) "" string)))


(defmacro foreach (list &rest body)
  "Executes body for each element of <list>, with:
    <it>               bound to the current element
    <its-index>        bound to its index in the list,
    <its-rank>         bound to its rank in the list (its-index+1)
    <the-previous-one> bound to the previous element (nil if the current index is 0)
    <number-of-elements> bound to the length of <list>"
  `(let ((number-of-elements (length ,list))) 
     (declare (ignorable number-of-elements))
     (loop for i from 0 below (length ,list) collect
        (let ((it         (elt ,list i))
              (its-index  i)
              (the-previous-one (if (> i 0) (elt ,list (- i 1))))
              (its-rank   (+ 1 i)))
           (declare (ignorable it its-index the-previous-one its-rank))
           (progn ,@body)))))

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
  "Globs the whole provided file, url or stream into a string or into a byte array if <binary> or some bytes cannot be decoded to characters"
  (cond
    ((and (stringp path-or-stream) (string-equal (subseq path-or-stream 0 7) "http://"))
       (glob (drakma:http-request path-or-stream :want-stream t :force-binary binary) :binary binary))
    ((and binary (stringp path-or-stream))
       (with-open-file   (s path-or-stream :element-type '(unsigned-byte 8)) (glob s :binary t)))
    ((stringp path-or-stream)
       (handler-case 
            (with-open-file   (s path-or-stream)
                              (let ((seq (make-array (file-length s) :element-type 'character :fill-pointer t)))
                                   (setf (fill-pointer seq) (read-sequence seq s))
                                   seq))
            (SB-INT:STREAM-DECODING-ERROR () (glob path-or-stream :binary t))))
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

(defun unglob (filename sequence)
  (progn
    (with-open-file (stream filename
                            :direction :output
                            :element-type (if (stringp sequence) 'character '(unsigned-byte 8)))
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

(defmacro import-forced (&rest symbols-as-strings)
  (let ((ourpackage (package-name *package*)))
       `(progn
           ,@(foreach symbols-as-strings
             (let* ((elts (~ "/^(.+):(.+)$/" it))
                     (zepackage (cadr elts))
                     (zesymbol (caddr elts)))
                    `(progn (in-package ,(read-from-string (mkstr ":" zepackage)))
                      (export (read-from-string ,zesymbol))
                      (in-package ,(read-from-string (mkstr ":" ourpackage)))
                      (import (read-from-string ,(mkstr zepackage ":" zesymbol)))))))))

(defmacro def-view-class-with-accessors-and-initargs (name superclasses slots &rest class-options)
  `(clsql:def-view-class
      ,name
      ,superclasses
      ,(mapcar [append _ `(:accessor ,(symb (symbol-name name) "-" (symbol-name (car _))) :initarg ,(reread ":" (symbol-name  (car _))))] slots)
      ,class-options))

(defun select (object &key where order-by)
    (mapcar #'car (reduce #'append (mapcar [clsql:select _ :where (clsql-sys::generate-sql-reference where) :order-by (clsql-sys::generate-sql-reference order-by)] (if (listp object) object (list object))))))

;flatten (On Lisp)
(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
          (rec x nil)))

(defun access (object start &rest args)
  (cond ((or (listp object) (vectorp object))
           (if args (subseq object start (car args)) (elt object start)))
        ((functionp object)
           (apply object (cons start args)))
        ((hash-table-p object)
           (if args (mapcar [gethash _ object] (cons start args)) (gethash start object)))
        ))

(defun setaccess (object start &rest args)
  (cond ((or (listp object) (vectorp object))
           (if (> (length args) 1) (setf (subseq object start (car args)) (cadr args)) (setf (elt object start) (car args))))
        ((hash-table-p object)
           (setf (gethash start object) (car args)))
        ))

(defsetf access setaccess)

(defun bracket-reader (stream char)
  (declare (ignore char))
  `(access ,@(read-delimited-list #\} stream t)))

(defun enable-brackets ()
  (set-macro-character #\{ #'bracket-reader)
  (set-macro-character #\} (get-macro-character #\) nil)))

(enable-brackets)

(defun test (description output expected &key (test #'equal))
  (princ description)
  (if (funcall test output expected)
      (progn (format t " -> ok~%") t)
      (progn (format t " -> FAILED !~% ### OUTPUT ###~%") (describe output) (format t "### EXPECTED ###~%") (describe expected) nil)))

(defmacro test-suite (name &rest body)
  `(progn
      (format t "*** ~A ***~%" ,name)
      (block test-suite1
             (foreach (quote ,body) (if (eval it) t (progn (format t "aborting test suite~%") (return-from test-suite1)))))))

(defun update-record (object &rest args)
  (clsql:update-records-from-instance (apply #'reinitialize-instance (cons object args))))