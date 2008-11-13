(require 'cl-ppcre)
(require 'drakma)

; **** Lambda expressions ala Arc by Brad Ediger ***
;CL-USER> ([+ 1 _] 10)
;11
;CL-USER> ([+ _ __] 1 2)
;3

(defun square-bracket-reader (stream char)
  (declare (ignore char))
  `(lambda (&optional _ __)
    (declare (ignorable _ __)) ; don't warn about unused variables
    ,(read-delimited-list #\] stream t)))

(set-macro-character #\[ #'square-bracket-reader)
  ; Make ] behave like ) to the reader so that [+ 1 _] works,
  ; not just [+ 1 _ ].
(set-macro-character #\] (get-macro-character #\) nil))

;*** in ala python ***
(defun in (seq elmt)
	"does seq contain elmt ?"
	(if (position elmt seq :test #'equal) t nil))


; *** Paul Graham's anaphoric if (cf. On Lisp) ***
(defmacro aif (test-form then-form &optional else-form)
	`(let ((it ,test-form)) 
		(if it ,then-form ,else-form)))


; *** Paul Graham's anaphoric while (cf. On Lisp) ***
(defmacro awhile (expr &body body) 
	`(do ((it ,expr ,expr)) ((not it)) ,@body))


(defmacro awith (expr &body body)
	`(let ((it ,expr)) ,@body))


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
  (if (listp string-or-list)
      (remove-if-not [cl-ppcre::scan re _] string-or-list)
      (let ((m (multiple-value-list (cl-ppcre::scan-to-strings re string-or-list))))
           (if (nth 1 m) (cons (car m) (vector-to-list* (cadr m))) nil))))

(defun !~ (re string-or-list)
  (if (listp string-or-list)
      (remove-if [cl-ppcre::scan re _] string-or-list)
      (if (not (cl-ppcre::scan re string-or-list))
          string-or-list)))

;*** Regexp substitution ala Perl (or nearly...) ***
(defun ~s (re replacement string &optional flags)
	(values
		(if (in flags #\g)
			(cl-ppcre::regex-replace-all re string replacement)
			(cl-ppcre::regex-replace re string replacement))))

(defun resplit (re string)
  (cl-ppcre:split re string))

;*** x a la perl/python/ruby *** TODO : rendre universel pour tout type de sequence
(defun x (string number)
  (if (> number 1) (concatenate 'string string (x string (- number 1)))
      (if (<= number 0) "" string)))

(defmacro foreach (list &rest body)
  `(let ((number-of-elements (length ,list))) 
     (loop for i from 0 below (length ,list) do
        (let ((it         (nth i ,list))
              (its-index  i)
              (the-previous-one (if (> i 0) (nth (- i 1) ,list)))
              (its-rank   (+ 1 i)))
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
  (resplit "\\r\\n|\\n" (glob path-or-stream)))

(defun lc (string)
  (string-downcase string))

(defun uc (string)
  (string-upcase string))  

(defun pos (&rest args)
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

; attempt at arc like function compose. Not so good / useful ? 
;(defun compose-reader (stream char)
;  (declare (ignore char))
;    (prog1 (read-delimited-list #\) stream t) (unread-char #\) stream)))
;     (list (read stream t nil t) (read stream t nil t)))

;(set-macro-character #\! #'compose-reader)

