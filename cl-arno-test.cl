(load "../cl-arno/cl-arno.cl")

(defpackage :cl-arno-test
    (:use     #:cl #:cl-arno)
    (:export  #:test-suite #:test #:with-mocks))

(in-package :cl-arno-test)

(defun test (description output expected &key (test #'equal))
  (princ description)
  (if (funcall test output expected)
      (progn (format t " -> ok~%") t)
      (progn (format t " -> FAILED !~% ### OUTPUT ###~%") (describe output) (format t "### EXPECTED ###~%") (describe expected) nil)))

(defmacro test-suite (name &rest body)
  `(progn
      (format t "*** ~A ***~%" ,name)
      (block test-suite1
             (loop for test in (quote ,body) do (unless (eval test) (progn (format t "aborting test suite~%~%") (return-from test-suite1))))
             (format t "~%")
             t)))

(defmacro with-mocks (mocks &rest body)
  (let* ((functions (remove-duplicates (mapcar [if (listp (car _)) (caar _) (car _)] mocks)))
         (gensyms   (mapcar [gensym] (range 1 (length functions)))))
    `(let (,@(foreach functions
            `(,{gensyms @it} (fdefinition (quote ,it)))))
    (prog1
        (progn
          ,@(foreach functions as f
            `(setf (fdefinition (quote ,f))
                  (lambda (&rest args)
                          (declare (ignorable args))
                          (cond
                            ,@(foreach (remove-if-not [and (listp (car _)) (eq (caar _) f)] mocks)
                                 `((equal args (list ,(cadar it))) ,(cadr it)))
                            (t ,(aif (remove-if-not [and (atom (car _)) (eq (car _) f)] mocks)
                                      (cadar it)
                                      '(error "unexpected arguments"))))
                          )))
          ,@(macroexpand body))
        ,@(foreach functions
           `(setf (fdefinition (quote ,it)) ,{gensyms @it}))))))
