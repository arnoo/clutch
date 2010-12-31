(load "cl-arno-test.cl")

(defpackage :cl-arno-tests (:use #:cl #:cl-arno #:cl-arno-test))
(in-package :cl-arno-tests)

(test-suite ("cl-arno")
  (test-suite ("conversions"
               :setup (progn (defstruct test-struct
                               (slot1 nil :type string)
                               (slot2 nil :type integer)
                               (slot3 nil :type list))
                             (defstruct test-struct2
                               (slot1 nil :type test-struct)
                               (slot2 nil :type (real 0)))))

    (test "Symb"
      (symb "A")
      :expect 'a
      :test 'eq)

    (test "Keyw"
      (keyw "a")
      :expect	:a
      :test 'eq)

    (test "str 1 2 3"
      (str 1 2 3)
      :expect "123")

    (test "str (list (list 1 2 3) 2)"
      (str (list (list 1 2 3) 2))
      :expect "1232")
    
    (test "nb -> string"
      (-> 1 'string)
      :expect "1")

    ;(test "hash-table -> plist"
    ;  (-> (mkhash 'a 1 'b 2) 'alist)
    ;  :expect (list a 1 b 2))

    ;(test "hash-table -> alist"
    ;  (-> (mkhash 'a 1 'b 2) 'plist)
    ;  :expect '((a . 1) (b . 2)))

    (test "hash-table -> struct"
      (-> (mkhash 'slot1 (mkhash 'slot1 "a" 'slot2 2 'slot3 (list 1 2)) 'slot2 2) 'test-struct2)
      :expect (make-test-struct2 :slot1 (make-test-struct :slot1 "a" :slot2 2 :slot3 (list 1 2)) :slot2 2)))

  (test-suite ("anaphoric macros")
    (test "Aif 1"
      (aif t it)
      :expect	t)

    (test "Aif 2"
      (aif nil 2 it)
      :expect	nil))

  (test-suite ("Reader macros")
    (test "Square bracket reader [+ 1 _]"
      ([+ 1 _] 2)
      :expect	3)

    (test "Square bracket reader [+ _ __]"
      ([+ _ __] 2 3)
      :expect 5)

    (test "Square bracket reader [_ 1]"
      ([_ 1] (list 1 2 3))
      :expect 2)

    (test "Curly brackets reader list"
      {(list 1 2 3) 1}
      :expect	2)

    (test "Curly brackets reader list negative"
      {(list 1 2 3) -1}
      :expect 3)

    (test "Curly brackets reader string"
      {"abc" 1}
      :expect	#\b)

    (test "Curly brackets reader subseq"
      {"abc" 1 2}
      :expect "b")

    (test "Curly brackets reader modulo 1"
      {"abc" 0 -1}
      :expect "abc")

    (test "Curly brackets reader modulo 2"
      {"abc" -2 -1}
      :expect "bc")

    (test "Curly brackets reader modulo 3"
      {"abc" 0 -4}
      :expect "")

    (test "Curly brackets reader nil args"
      {"abc" 0 nil}
      :expect #\a)

    (test "Curly brackets reader function"
      {(lambda (x) (+ x 1)) 1}
      :expect 2)

    (test "Curly brackets setter hash"
      (let ((hash (mkhash 'a 1 'b 2)))
         (setf {hash 'c} 5)
         (gethash 'c hash))
      :expect 5)

    (test "Curly brackets setter list"
      (let ((alist (list 1 2 3 4)))
         (setf {alist 1} 5)
         alist)
      :expect '(1 5 3 4))

    (test "Curly brackets setter alist new"
      (let ((alist '((a . 1) (b . 2))))
         (setf {alist 'c} 5)
         alist)
      :expect '((a . 1) (b . 2) (c . 5)))

    (test "Curly brackets setter alist existing"
      (let ((alist '((a . 1) (b . 2))))
         (setf {alist 'a} 5)
         alist)
      :expect '((a . 5) (b . 2)))

    (test "Curly brackets setter plist new"
      (let ((plist (list 'a 1 'b 2)))
         (setf {plist 'c} 5)
         plist)
      :expect '(a 1 b 2 c 5))

    (test "Curly brackets setter plist existing"
      (let ((plist (list 'a 1 'b 2)))
         (setf {plist 'a} 5)
         plist)
      :expect '(a 5 b 2))

    ;(test "Curly brackets setter string"
    ;  (let ((astring "abc"))
    ;    (setf {astring 1} #\g)
    ;    (elt astring 1))                           
    ; :expect #\g)
    )

  (test-suite ("in, range, foreach")
      (test "In <list> <element> -> t"
        (in (list 1 2 3) 1)
        :expect t)

      (test "In <list> <element> -> nil"
        (in (list 1 2 3) 4)
        :expect nil)

      (test "Range 1..3"
        (range 1 3)
        :expect '(1 2 3))

      (test "Range 0..4 by 2"
        (range 0 4 2)
        :expect '(0 2 4))

      (test "Range 1..-1 by -1"
        (range 1 -1 -1)
        :expect '(1 0 -1))

      (test "Foreach"
        (foreach (list 1 2)
          (list it @it))
        :expect '((1 0) (2 1)))

      ;(test "Foreach as"
      ;   (foreach (list 1 2) as e (list e @e))
      ; :expect '((1 0) (2 1)))
      )

  (test-suite ("regexps")
    (test "Regexp simple"
      (~ "/\\w/" "bob")
      :expect '("b"))

    (test "Regexp global"
      (~ "/\\w/g" "bob")
      :expect '(("b") ("o") ("b")))

    (test "Regexp group"
      (~ "/(\\w)\\w/g" "bob")
      :expect '(("bo" "b")))

    (test "Regexp group extract"
      (~ "/(\\w)\\w/g" "bob" 1 0)
      :expect "b")

    (test "Regexp subst"
      (~s "/b/a/" "bob")
      :expect "aob")

    (test "Regexp subst global"
      (~s "/b/a/g" "bob")
      :expect "aoa")

    (test "Regexp subst nocase"
      (~s "/B/A/i" "bob")
      :expect "Aob")

    (test "Regexp subst nocase global"
       (~s "/B/A/gi" "bob")
       :expect "AoA")
  
    (test "Regexp subst list"
       (~s "/b/a/" (list "bob" "cob"))
       :expect (list "aob" "coa"))
  
    (test "Regexp subst global list"
       (~s "/b/a/g" (list "bob" "cob"))
       :expect (list "aoa" "coa"))
  
    (test "Regexp advanced syntax"
       (~ "/(\\s|^)(\\w+\\/\\w+)(;|$)/" "image/jpeg;aa")
       :expect '("image/jpeg;" "" "image/jpeg" ";"))
  
    (test "Regexp filter"
       (~ "/\\d/" (list "a" "a1" "b" "2"))
       :expect '("a1" "2"))
  
    (test "Regexp filter capturing single match"
       (~ "/\\w(\\d)/" (list "a" "a1" "b" "2") 1)
       :expect '("1")))

  (test-suite ("memoize")
    (test "memoize"
        (let ((a 0))
           (awith (memoize [+ _ a])
             {it 0}
             (setf a 1)
             {it 0}))
        :expect 0))

  (test-suite ("time and date")
         (test "ut"
           (ut)
           :expect (get-universal-time))
     
         (test "ut now"
           (ut "now")
           :expect (get-universal-time))
     
         (test "ut January 22 1964 23:12"
           (ut "January 22 1964 23:12")
           :expect (encode-universal-time 0 12 23 22  1 1964 -1))
     
         (test "ut (date)"
           (ut (date))
           :expect (get-universal-time))
     
         (test "ut (date \"now\")"
           (ut (date "now"))
           :expect  (get-universal-time))
     
         (test "ut (date January 22 1964 23:12)"
           (ut (date "January 22 1964 23:12"))
           :expect (encode-universal-time 0 12 23 22  1 1964 -1)))

  (test-suite ("Glob / unglob"
                :setup (when (probe-file "/tmp/tarno")
                          (delete-file "/tmp/tarno")))

     (test "Glob/unglob file"
       (progn (unglob "/tmp/tarno" "abcde") 
              (glob "/tmp/tarno"))
       :expect "abcde")

     (test "Glob file offset"
       (glob "/tmp/tarno" :offset 3)
       :expect "de")

     (test "Glob file negative offset"
       (glob "/tmp/tarno" :offset -3)
       :expect "cde")

     (test "Glob file limit"
       (glob "/tmp/tarno" :limit 3)
       :expect "abc")

     (test "Glob file offset+limit"
       (glob "/tmp/tarno" :offset 2 :limit 3)
       :expect "cde")

     (test "Glob file offset+limit 2"
       (glob "/tmp/tarno" :offset 2 :limit 2)
       :expect "cd")

     (test "Glob file neg offset+limit"
       (glob "/tmp/tarno" :offset -3 :limit 2)
       :expect "cd")

     (test "Glob stream"
       (with-input-from-string (s "abcde")
         (glob s))
       :expect "abcde")

     (test "Glob stream offset"
       (with-input-from-string (s "abcde")
         (glob s :offset 3))
       :expect "de")

     (test "Glob stream negative offset"
       (with-input-from-string (s "abcde")
         (glob s :offset -3))
       :expect                   "cde")

     (test "Glob stream limit"
       (with-input-from-string (s "abcde")
         (glob s :limit 3))
       :expect                     "abc")

     (test "Glob stream offset+limit"
       (with-input-from-string (s "abcde")
         (glob s :offset 2 :limit 3))
       :expect          "cde")

     (test "Glob stream offset+limit 2"
       (with-input-from-string (s "abcde")
         (glob s :offset 2 :limit 2))
       :expect           "cd")

     (test "Glob stream neg offset+limit"
           (with-input-from-string (s "abcde")
             (glob s :offset -3 :limit 2))
           :expect          "cd"))
     
   (test-suite ("glob-flines"
                :setup (unglob "/tmp/tarno" (format nil "a~%b~%c~%d~%e~%") :if-exists :overwrite)
                :teardown (if (probe-file "/tmp/tarno") (delete-file "/tmp/tarno")))

     (test "mapflines"
           (mapflines #'identity "/tmp/tarno")
           :expect (list "a" "b" "c" "d" "e"))

     (test "mapflines offset"
           (mapflines #'identity "/tmp/tarno"
                      :offset 1)
           :expect (list "b" "c" "d" "e"))

     (test "mapflines limit"
           (mapflines #'identity "/tmp/tarno"
                      :limit 2)
           :expect (list "a" "b"))

     (test "mapflines limit + offset"
           (mapflines #'identity "/tmp/tarno"
                      :offset 2
                      :limit 2)
           :expect (list "c" "d"))

     (test "mapflines -offset"
       (mapflines #'identity "/tmp/tarno"
             :offset -2)
       :expect (list "d" "e"))

     (test "mapflines -offset+limit"
       (mapflines #'identity "/tmp/tarno"
             :offset -3
             :limit 2)
       :expect (list "c" "d"))

     (test "mapflines stream"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"))
       :expect (list "a" "b" "c" "d" "e"))

     (test "mapflines stream offset"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"
             :offset 1))
       :expect (list "b" "c" "d" "e"))

     (test "mapflines stream limit"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"
             :limit 2))
       :expect (list "a" "b"))

     (test "mapflines stream limit+offset"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"
             :offset 2
             :limit 2))
       :expect (list "c" "d"))

     (test "mapflines stream -offset"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"
             :offset -2))
       :expect (list "d" "e"))

     (test "mapflines stream -offset+limit"
       (with-input-from-string (s "a\nb\nc\nd\ne")
          (mapflines #'identity "/tmp/tarno"
             :offset -3
             :limit 2))
       :expect (list "c" "d")))

  (test-suite ("save"
                :setup (mkdir "/tmp/tarno")
                :teardown (sh "rm -rf /tmp/tarno"))

      (test "Save not timestamped 1"
        (fsave "test" "/tmp/tarno")
        :expect 0)

      (test "Save not timestamped 2"
        (fsave "test" "/tmp/tarno")
        :expect 1)

      (test "Save not timestamped 3"
        (fsave "test" "/tmp/tarno")
        :expect 2)

      (test "Save not timestamped 4"
        (fsave "test" "/tmp/tarno" :id 2)
        :expect 2)

      (test "Save timestamped "
        (fsave "test" "/tmp/tarno" :id 2 :timestamped t)
        :expect   2)

      (test "Save timestamped 2"
        (fsave "test" "/tmp/tarno" :id 2 :timestamped t)
        :expect   2))

      (defun f (x) x)

      ;(test "with-mocks"
      ;  (with-mocks ((f 4) ((f 3) 2)) (list (f 3) (f 2) (f 4)))
      ;  :expect '(2 4 4))

      (test "mkhash"
        {(mkhash "a" 2 "b" 3) "a"}
        :expect 2)

      (test "keys plist"
        (keys (list 'a 1 'b 2))
        :expect '(a b))

      (test "keys alist"
        (keys '((a . 1) (b . 2)))
        :expect '(a b))

      (test "keys hash"
        (keys (mkhash 'a 1 'b 2))
        :expect '(a b))

      (test "keys struct"
        (keys (list 'a 1 'b 2))
        :expect (list 'a 'b))

      (test "keys obj"
        (keys (list 'a 1 'b 2))
        :expect '(a b))

  (test-suite ("hashing algorithms")
      (test "md5"
        (md5 "couscous")
        :expect "12e63937875297a7269d3a60924e8634")

      (test "sha1"
        (sha1 "couscous")
        :expect "e32e859930f14d92a15870788f8a21c8316ebdc7")

      (test "sha256"
        (sha256 "couscous")
        :expect "438292433b9c125743d88dc39afa226f0a5226cac25d6e08bfe29b419374a1a3"))

  (test-suite ("xor")
      (test "xor nil nil nil"
        (xor nil nil nil)
        :expect nil)

      (test "xor t nil nil"
        (xor t nil nil)
        :expect t)

      (test "xor nil t nil"
        (xor nil t nil)
        :expect t)

      (test "xor t t nil"
        (xor t t nil)
        :expect nil)

      (test "xor t t"
        (xor t t)
        :expect nil)

      (test "xor 1 nil"
        (xor 1 nil)
        :expect 1)

      (test "xor nil (+ 1 3)"
        (xor nil (+ 1 3))
        :expect 4)
  ))
