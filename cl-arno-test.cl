(load "../cl-arno/cl-arno.cl")

(defpackage :cl-arno-test
    (:use     #:cl #:cl-arno)
    (:export  #:test-suite #:test #:with-mocks))

(in-package :cl-arno-test)

(defmacro test (description form &key expect expect-type expect-error (test #'equal) fatal (level 0))
  `(let ((errors nil))
     (format t (str (x "  " (+ ,level 1)) ,description))
     (multiple-value-bind (ret err) (ignore-errors ,form)
       (when (and ,expect-type (not (subtypep ret ,expect-type)))
         (push (format nil "Output type ~A does not match expected type ~A" (type-of ret) ,expect-type)
               errors))
       (when (and (typep err 'error) (not ,expect-error))
         (push (str "An error occured and none was expected :" (with-output-to-string (s) (describe err s)))
               errors))
       (when (and (not (typep err 'error)) ,expect-error)
         (push (str "An error was expected and none occured.")
               errors))
       (when (and (not errors)
                  ,expect
                  (not (funcall ,test ret ,expect)))
          (push (str "Output does not match expected. Output :"
                     (with-output-to-string (s) (describe ret s))
                     "Expected :"
                     (with-output-to-string (s) (describe ,expect s))
                     )
                errors))
       (if errors
          (progn (format t " -> FAILED ! ~% ~A ~%" (join "\n" errors))
                 nil)
          (progn (format t " -> ok~%")
                 t)))))

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
                            ,@(loop for it in (remove-if-not [and (listp (car _)) (eq (caar _) f)] mocks)
                                 collect `((equal args (list ,(cadar it))) ,(cadr it)))
                            (t ,(aif (remove-if-not [and (atom (car _)) (eq (car _) f)] mocks)
                                      (cadar it)
                                      '(error "unexpected arguments"))))
                          )))
          ,@(macroexpand body))
        ,@(loop for f in functions collect
           `(setf (fdefinition (quote ,f)) ,{gensyms f}))))))
