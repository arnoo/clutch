(require 'cl-ppcre)

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
(defun ~ (re string)
	(let ((m (multiple-value-list (cl-ppcre::scan-to-strings re string))))
		(if (nth 1 m) (cons (car m) (vector-to-list* (cadr m))) nil)))


;*** Regexp substitution ala Perl (or nearly...) ***
(defun ~s (re string replacement &optional flags)
	(values
		(if (in flags #\g)
			(cl-ppcre::regex-replace-all re string replacement)
			(cl-ppcre::regex-replace re string replacement))))



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


; Shawn Betts's slurp http://www.emmett.ca/~sabetts/slurp.html
(defun slurp (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
      seq))

; Reads a file/stream entirely then closes it and returns the contents as a string
(defun glob (file-or-stream)
  (if (streamp file-or-stream)
      (with-open-stream (s file-or-stream) (slurp s))
      (with-open-file   (s file-or-stream) (slurp s)))
  )

; attempt at arc like function compose. Not so good / useful ? 
;(defun compose-reader (stream char)
;  (declare (ignore char))
;    (prog1 (read-delimited-list #\) stream t) (unread-char #\) stream)))
;     (list (read stream t nil t) (read stream t nil t)))

;(set-macro-character #\! #'compose-reader)

