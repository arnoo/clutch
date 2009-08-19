(load "cl-arno.cl")

(defpackage :cl-arno-tests (:use #:cl #:sb-ext #:cl-arno))
(in-package :cl-arno-tests)

(if 
  (and
(test "Aif 1"                                   (aif t it)                                        t)
(test "Aif 2"                                   (aif nil 2 it)                                    nil)
(test "Square bracket reader [+ 1 _]"           ([+ 1 _] 2)                                       3)
(test "Square bracket reader [+ _ __]"          ([+ _ __] 2 3)                                    5)
(test "Square bracket reader [_ 1]"             ([_ 1] (list 1 2 3))                              2)
(test "Curly brackets reader list"              {(list 1 2 3) 1}                                  2)
(test "Curly brackets reader string"            {"abc" 1}                                         #\b)
(test "Curly brackets reader subseq"            {"abc" 1 1}                                       "b")
(test "Curly brackets reader modulo 1"          {"abc" 0 -1}                                      "abc")
(test "Curly brackets reader nil args"          {"abc" 0 nil}                                     #\a)
(test "Curly brackets reader function"          {(lambda (x) (+ x 1)) 1}                          2)
(test "Curly brackets setter hash"              (let ((hash (mkhash 'a 1 'b 2)))
                                                    (setf {hash 'c} 5)
                                                    (gethash 'c hash))                            5)
(test "Curly brackets setter list"              (let ((alist (list 1 2 3 4)))
                                                    (setf {alist 1} 5)
                                                    alist)                                        '(1 5 3 4))
(test "Curly brackets setter alist new"         (let ((alist '((a . 1) (b . 2))))
                                                    (setf {alist 'c} 5)
                                                    alist)                                        '((a . 1) (b . 2) (c . 5)))
(test "Curly brackets setter alist existing"    (let ((alist '((a . 1) (b . 2))))
                                                    (setf {alist 'a} 5)
                                                    alist)                                        '((a . 5) (b . 2)))
(test "Curly brackets setter plist new"         (let ((plist (list 'a 1 'b 2)))
                                                    (setf {plist 'c} 5)
                                                    plist)                                        '(a 1 b 2 c 5))
(test "Curly brackets setter plist existing"    (let ((plist (list 'a 1 'b 2)))
                                                    (setf {plist 'a} 5)
                                                    plist)                                        '(a 5 b 2))
;(test "Curly brackets setter string"            (let ((astring "abc"))
;                                                    (setf {astring 1} #\g)
;                                                    (elt astring 1))                              #\g)
(test "In <list> <element> -> t"                (in (list 1 2 3) 1)                               t)
(test "In <list> <element> -> nil"              (in (list 1 2 3) 4)                               nil)
(test "Range 1..3"                              (range 1 3)                                       '(1 2 3))
(test "Range 0..4 by 2"                         (range 0 4 2)                                     '(0 2 4))
(test "Range 1..-1 by -1"                       (range 1 -1 -1)                                   '(1 0 -1))
(test "Foreach"                                 (foreach (list 1 2) (list it @it)) '((1 0) (2 1)))
;(test "Foreach as"                              (foreach (list 1 2) as e (list e @e)) '((1 0) (2 1)))
(test "Regexp simple"                           (~ "/\\w/" "bob")                                 '("b"))
(test "Regexp global"                           (~ "/\\w/g" "bob")                                '(("b") ("o") ("b")))
(test "Regexp group"                            (~ "/(\\w)\\w/g" "bob")                           '(("bo" "b")))
(test "Regexp subst"                            (~s "/b/a/" "bob")                                "aob")
(test "Regexp subst global"                     (~s "/b/a/g" "bob")                               "aoa")
(test "Regexp subst nocase"                     (~s "/B/A/i" "bob")                               "Aob")
(test "Regexp subst nocase global"              (~s "/B/A/gi" "bob")                              "AoA")
(test "Regexp subst list"                       (~s "/b/a/" (list "bob" "cob"))                   '("aob" "coa"))
(test "Regexp subst global list"                (~s "/b/a/g" (list "bob" "cob"))                  '("aoa" "coa"))
(test "Regexp advanced syntax"                  (~ "/(\\s|^)(\\w+\\/\\w+)(;|$)/" "image/jpeg;aa") '("image/jpeg;" "" "image/jpeg" ";"))
                                                (if (probe-file "/tmp/tarno") (delete-file "/tmp/tarno") t)
(test "Glob/unglob"                             (progn (unglob "/tmp/tarno" "aaa") 
                                                       (glob "/tmp/tarno"))                       "aaa")
                                                (if (probe-file "/tmp/tarno") (delete-file "/tmp/tarno") t)
;(test "Glob http"                               (glob "") "")
                                                (defun f (x) x)
(test "with-mocks"                              (with-mocks ((f 4) ((f 3) 2)) (list (f 3) (f 2) (f 4))) '(2 4 4))
(test "mkhash"                                  {(mkhash "a" 2 "b" 3) "a"} 2)
)
  (format t "*** All tests passed successfully ***~%")
  (format t "*** There were errors ***~%"))
