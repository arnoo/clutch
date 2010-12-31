(load "../cl-arno/cl-arno.cl")

(defpackage :cl-arno-test
    (:use     #:cl #:cl-arno)
    (:export  #:test-suite #:test #:with-mocks))

(in-package :cl-arno-test)

(defmacro test (description form &key expect expect-error (test #'equal) fatal (level 0))
  `(progn
     (format t (str (x "  " (+ ,level 1)) ,description))
     (multiple-value-bind (ret err) (ignore-errors ,form)
       (when (typep err 'error)
         (setf ret err))
       (if (and (funcall ,test (if (typep err 'error) err ret)
                               (aif ,expect-error it ,expect))
                (not (xor ,expect-error (typep err 'error))))
          (progn (format t " -> ok~%") t)
          (progn (format t " -> FAILED !~% ### OUTPUT ###~%") (describe ret) (format t "### EXPECTED ###~%") (describe ,expect) nil)))))

(defmacro test-suite (args &rest body)
  (destructuring-bind (name &key (level 0) setup teardown) args
    `(progn
      (let ((total 0)
            (passed 0)
            (skipped 0))
          (declare (ignore skipped))
          (format t (str (x "  " ,level) "### Test suite '" ,name "'~%"))
          ,setup
          (loop for form in (quote ,body)
                do (cond ((eq (car form) 'test)
                             (incf total)
                             ;(push ,level (caddr form))
                             ;(push :level (caddr form))
                             (when (eval form) ; TODO: gerer le fatal : skip le reste
                                (incf passed)))
                         ((eq (car form) 'test-suite)
                             ;(push (+ ,level 1) (caddr form))
                             ;(push :level (caddr form))
                             (eval form))
                         (t (eval form))))
          ,teardown
          (format t (str (x "  " ,level) "### " passed "/" total " passed in '" ,name "'~%"))
          ))))

(defmacro with-mocks (mocks &rest body)
  (let* ((functions (remove-duplicates (mapcar [if (listp (car _)) (caar _) (car _)] mocks)))
         (gensyms   (mapcar [gensym] (range 1 (length functions)))))
    `(let (,@(loop for f in functions collect
            `(,{gensyms f} (fdefinition (quote ,f)))))
    (prog1
        (progn
          ,@(loop for f in functions collect
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
        ,@(loop for f in functions collect
           `(setf (fdefinition (quote ,f)) ,{gensyms f}))))))
