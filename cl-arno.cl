
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
	`(let ((it ,eifxpr)) ,@body))



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


;*** Regexp match ala Perl ***
(defun ~ (re string-or-list)
  "If <string-or-list> is a string:
    returns nil if regular expression <re> does not match the string
    returns the part of the string that matches <re> and all grouped matches ()
   if <string-or-list> is a list
    returns the elements of that list that match <re>
    
    example : (re \"\w+(\d)\" \"ab2cc\")"
  (if (listp string-or-list)
      (remove-if-not [cl-ppcre::scan re _] string-or-list)
      (let ((m (multiple-value-list (cl-ppcre::scan-to-strings re string-or-list))))
           (if (nth 1 m) (cons (car m) (vector-to-list* (cadr m))) nil))))


(defun !~ (re string-or-list)
  "If <string-or-list> is a string:
    returns nil if <re> matches the string
    returns the string if <re> does not match
   if <string-or-list> is a list
    returns the elements of that list that do not match <re>
    
    example: (!~ \"\w{3}\" (list \"aaa\" \"bb\" \"ccc\"))"
  (if (listp string-or-list)
      (remove-if [cl-ppcre::scan re _] string-or-list)
      (if (not (cl-ppcre::scan re string-or-list))
          string-or-list)))


;*** Regexp substitution ala Perl (or nearly...) ***
(defun ~s (re replacement string &optional flags)
  "Replaces all substrings that match <re> in <string> by <replacement>.
  <flags> can contain Perl regexp flags like g"
	(values
		(if (in flags #\g)
			(cl-ppcre::regex-replace-all re string replacement)
			(cl-ppcre::regex-replace re string replacement))))


(defun resplit (re string)
  (cl-ppcre:split re string))


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
        (let ((it         (nth i ,list))
              (its-index  i)
              (the-previous-one (if (> i 0) (nth (- i 1) ,list)))
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
(defun glob (path-or-stream)
  "Globs the whole provided file, url or stream into a string or into a byte array if some bytes cannot be decoded to characters"
  (if (streamp path-or-stream)
      (with-open-stream (s path-or-stream)
                        (with-output-to-string (out)
                                               (do ((x (read-char s nil s) (read-char s nil s)))
                                                   ((eq x s))
                                                   (write-char x out))))
      (if (and (stringp path-or-stream)
               (string-equal (subseq path-or-stream 0 7) "http://"))
          (multiple-value-bind (body status headers real-url stream must-close reason-phrase)
                               (drakma:http-request path-or-stream :want-stream t)
                               (glob stream))
          (handler-case 
            (with-open-file   (s path-or-stream)
                              (let ((seq (make-array (file-length s) :element-type 'character :fill-pointer t)))
                                   (setf (fill-pointer seq) (read-sequence seq s))
                                   seq))
            (SB-INT:STREAM-DECODING-ERROR () (with-open-file   (s path-or-stream :element-type '(unsigned-byte 8))
                (let ((seq (make-array (file-length s) :element-type '(unsigned-byte 8) :fill-pointer t)))
                     (setf (fill-pointer seq) (read-sequence seq s))
                     seq)))))))


(defun unglob (filename sequence)
  (progn
    (with-open-file (stream filename
                            :direction :output
                            :element-type (if (stringp sequence) 'character '(unsigned-byte 8)))
      (write-sequence sequence stream))
    t))


(defun glob-lines (path-or-stream)
  "Globs the whole provided file, url or stream into an array of its lines"
  (resplit "\\r\\n|\\n" (glob path-or-stream)))


(defun lc (string)
  "Converts a string to lowercase (shortcut for string-downcase)"
  (string-downcase string))


(defun uc (string)
  "Converts a string to uppercase (shortcut for string-upcase)"
  (string-upcase string))  


(defun pos (&rest args)
  "Shortcut for position"
  (apply #'position args))  


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

(export 'symb)

(defmacro import-forced (&rest symbols-as-strings)
  (let ((ourpackage (package-name *package*)))
       `(progn
           ,@(foreach symbols-as-strings
             (let* ((elts (~ "^(.+):(.+)$" it))
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

(defmacro template (file values)
    `(let ((s (make-string-output-stream)))
                    (html-template:fill-and-print-template ,file ,values :stream s)
                              (get-output-stream-string s)))
