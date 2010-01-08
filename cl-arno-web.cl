(load "../cl-arno/cl-arno.cl")

(mapcar #'require '(html-template hunchentoot))

(defpackage :cl-arno-web
    (:use     #:cl #:cl-arno)
    (:export  #:template #:hpost #:hget #:hsession #:hlang #:static-gzip-handler))

(in-package :cl-arno-web)
(enable-brackets)
(enable-arc-lambdas)

(defvar *form-validators* (make-hash-table :test 'equal))
(defvar *default-validators* (hash-table )
   )

;(defmacro template (file values)
;  `(let ((s (make-string-output-stream)))
;        (html-template:fill-and-print-template ,(make-pathname :directory `(:absolute ,*root-dir*) :name file) ,values :stream s)
;        (get-output-stream-string s)))

(defun htpost (&optional var)
  (aif var (hunchentoot:post-parameter it) (hunchentoot:post-parameters*)))

(defun htget (&optional var)
  (aif var (hunchentoot:get-parameter it) (hunchentoot:get-parameters*)))

(defun hsession (var)
  (hunchentoot:session-value var))

;(defun static-gzip-handler ()
;  (aif {(hunchentoot:headers-in*) :ACCEPT-ENCODING}
;       (when (~ "/gzip,deflate/" it)
;            (setf (hunchentoot:header-out :CONTENT-ENCODING) "gzip") 
;            (hunchentoot:handle-static-file (str *root-dir* (hunchentoot:request-uri*) ".gz"))))
;    (hunchentoot:handle-static-file (str *root-dir* (hunchentoot:request-uri*))))

(defun html (code &key (title "") (css ""))
  (setf code (~s "/^\s+//" code))
  (let ((start (str (lc {code 0 -1})  "XXXXXXXXX")))
    (when (string/= {start 0 8} "<!doctype")
      (if (string/= {start 0 4} "<html")
        (if (string/= {start 0 4} "<head")
          (if (string/= {start 0 4} "<body")
    	    (str "<!DOCTYPE XHTML...><html><head><title>" title "</title></head><body>" code "</body></html>")
    	    (str "<!DOCTYPE XHTML...><html><head><title>" title "</title></head>" code "</html>"))
	  (str "<!DOCTYPE XHTML...><html>" code "</html>"))
        (str "<!DOCTYPE XHTML...>" code)))))

(defun validate (html &key (access 0) fragment)
	(split "\\n" (system (str "tidy -eq -access " access))))

(defun form (fields &key attrs validators)
  (let ((token (gensym)))
    (when validators (setf {*form-validators* token} validators))
    (str (tag 'input :attrs `(:type 'hidden :name 'cl-arno-token :value ,token))
    )))

(defun form-errors (fields validators)
  (let ((errors nil))
    (when (not {fields :cl-arno-token}) (push 'no-token errors))
    (let ((validators (aif validators it {*form-validators* {fields :cl-arno-token}})))
      (when validators
        (loop for validator in validators
              do (aif (apply #'validator )))))
    errors))

(defun tag (name &key attrs)
  (str "<" name " " 
       (str (loop for i below (length attrs) by 2
                          collect (str {attrs i} "='" {attrs (+ i 1)} "' ")))
       "/>"))

(defun table (list &key vheaders hheaders sortable jsortable attrs transpose)
  (declare (optimize debug))
  (str (tag 'table :attrs attrs)
       (if vheaders
         (str "<tr><th>"
              (join "</th><th>" (if (eq vheaders t) (pop list) (mapcar #'str vheaders)))
              "</th></tr>"))
       (mapcar (lambda (line)
                 (str "<tr><td>" (join "</td><td>" (mapcar #'str line)) "</td></tr>")) list)
       "</table>"))

(defmacro start-server (&key port dispatchers)
  `(progn
     (setf hunchentoot:*dispatch-table* (list
       ,(loop for i below (length dispatchers)
              collect `(let* (($$ (~ (str "/" (~s "/\\//\\\\\//" ,{dispatchers i}) "/") ))
			      ($0 (if $$ {$$ 0}))
			      ($1 (if (> (length $$) 1) {$$ 1} nil))
			      ($2 (if (> (length $$) 2) {$$ 2} nil))) 
		         (hunchentoot:create-regex-dispatcher
		           ,{dispatchers i}
		  	   (lambda () ,{dispatchers (+ i 1)}))))))
	(hunchentoot:start (make-instance 'hunchentoot:acceptor :port ,port))))

