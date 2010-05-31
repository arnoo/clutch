(load "cl-arno-test.cl")

(defpackage :cl-arno-tests (:use #:cl #:cl-arno #:cl-arno-test))
(in-package :cl-arno-tests)

(defstruct test-struct
  (slot1 nil :type string)
  (slot2 nil :type integer)
  (slot3 nil :type list))

(defstruct test-struct2
  (slot1 nil :type test-struct)
  (slot2 nil :type (real 0)))

(if 
  (and
(test "Symb"                                    (symb "A")					'a :test 'eq)
(test "Keyw"                                    (keyw "a")					:a :test 'eq)
(test "Aif 1"                                   (aif t it)					t)
(test "Aif 2"                                   (aif nil 2 it)					nil)
(test "Square bracket reader [+ 1 _]"           ([+ 1 _] 2)					3)
(test "Square bracket reader [+ _ __]"          ([+ _ __] 2 3)					5)
(test "Square bracket reader [_ 1]"             ([_ 1] (list 1 2 3))				2)
(test "Curly brackets reader list"              {(list 1 2 3) 1}				2)
(test "Curly brackets reader string"            {"abc" 1}       				#\b)
(test "Curly brackets reader subseq"            {"abc" 1 2}     				"b")
(test "Curly brackets reader modulo 1"          {"abc" 0 -1}    				"abc")
(test "Curly brackets reader nil args"          {"abc" 0 nil}   				#\a)
(test "Curly brackets reader function"          {(lambda (x) (+ x 1)) 1}                        2)
(test "Curly brackets setter hash"              (let ((hash (mkhash 'a 1 'b 2)))
                                                    (setf {hash 'c} 5)
                                                    (gethash 'c hash))                          5)
(test "Curly brackets setter list"              (let ((alist (list 1 2 3 4)))
                                                    (setf {alist 1} 5)
                                                    alist)                                      '(1 5 3 4))
(test "Curly brackets setter alist new"         (let ((alist '((a . 1) (b . 2))))
                                                    (setf {alist 'c} 5)
                                                    alist)                                      '((a . 1) (b . 2) (c . 5)))
(test "Curly brackets setter alist existing"    (let ((alist '((a . 1) (b . 2))))
                                                    (setf {alist 'a} 5)
                                                    alist)                                      '((a . 5) (b . 2)))
(test "Curly brackets setter plist new"         (let ((plist (list 'a 1 'b 2)))
                                                    (setf {plist 'c} 5)
                                                    plist)                                      '(a 1 b 2 c 5))
(test "Curly brackets setter plist existing"    (let ((plist (list 'a 1 'b 2)))
                                                    (setf {plist 'a} 5)
                                                    plist)                                      '(a 5 b 2))
;(test "Curly brackets setter string"            (let ((astring "abc"))
;                                                    (setf {astring 1} #\g)
;                                                    (elt astring 1))                           #\g)
(test "In <list> <element> -> t"                (in (list 1 2 3) 1)                             t)
(test "str 1 2 3"                               (str 1 2 3)                 			"123")
(test "str (list (list 1 2 3) 2)"               (str (list (list 1 2 3) 2)) 			"1232")
(test "In <list> <element> -> nil"              (in (list 1 2 3) 4)         			nil)
(test "Range 1..3"                              (range 1 3)                 			'(1 2 3))
(test "Range 0..4 by 2"                         (range 0 4 2)               			'(0 2 4))
(test "Range 1..-1 by -1"                       (range 1 -1 -1)             			'(1 0 -1))
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
(test "Regexp filter"                           (~ "/\\d/" (list "a" "a1" "b" "2")) '("a1" "2"))
(test "Regexp filter capturing single match"    (~ "/\\w(\\d)/" (list "a" "a1" "b" "2") 1) '("1"))
                                                (if (probe-file "/tmp/tarno") (delete-file "/tmp/tarno") t)
(test "Glob/unglob"                             (progn (unglob "/tmp/tarno" "aaa") 
                                                       (glob "/tmp/tarno"))                       "aaa")
                                                (if (probe-file "/tmp/tarno") (delete-file "/tmp/tarno") t)
;(test "Glob http"                               (glob "") "")
                                                (mkdir "/tmp/tarno")
(test "Save not timestamped 1"                  (save "test" "/tmp/tarno")                        0)
(test "Save not timestamped 2"                  (save "test" "/tmp/tarno")                        1)
(test "Save not timestamped 3"                  (save "test" "/tmp/tarno")                        2)
(test "Save not timestamped 4"                  (save "test" "/tmp/tarno" :id 2)                  2)
                                                (rm "/tmp/tarno/0")
                                                (rm "/tmp/tarno/1")
                                                (rm "/tmp/tarno/2")
(test "Save timestamped "                       (save "test" "/tmp/tarno" :id 2 :timestamped t)   2)
(test "Save timestamped 2"                      (save "test" "/tmp/tarno" :id 2 :timestamped t)   2)
                                                (defun f (x) x)
(test "with-mocks"                              (with-mocks ((f 4) ((f 3) 2)) (list (f 3) (f 2) (f 4))) '(2 4 4))
(test "mkhash"                                  {(mkhash "a" 2 "b" 3) "a"} 2)
(test "keys plist"                              (keys (list 'a 1 'b 2))                           '(a b))
(test "keys alist"                              (keys '((a . 1) (b . 2)))                         '(a b))
(test "keys hash"                               (keys (mkhash 'a 1 'b 2))                         '(a b))
;(test "keys struct"                             (keys ('a 1 'b 2))                                (list 'a 'b))
;(test "keys obj"                                (keys ('a 1 'b 2))                                '(a b))
(test "nb -> string"                            (-> 1 'string)                                    "1")
(test "hash-table -> plist"                     (-> (mkhash 'a 1 'b 2) 'alist)                    '(a 1 b 2))
(test "hash-table -> alist"                     (-> (mkhash 'a 1 'b 2) 'plist)                    '((a . 1) (b . 2)))
(test "hash-table -> struct"                    (-> (mkhash 'slot1 (mkhash 'slot1 "a" 'slot2 2 'slot3 (list 1 2)) 'slot2 2) 'test-struct2) (make-test-struct2 :slot1 (make-test-struct :slot1 "a" :slot2 2 :slot3 (list 1 2)) :slot2 2))
)
  (format t "*** All tests passed successfully ***~%")
  (format t "*** There were errors ***~%"))
