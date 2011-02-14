;
;   Copyright 2011 Arnaud Betremieux <arno@arnoo.net>
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(require 'clutch)
(require 'cleanser)

(defpackage :clutch-tests (:use #:cl #:clutch #:cleanser))
(in-package :clutch-tests)

(setf cleanser:*quiet* t)

(defstruct test-struct
     (a nil :type string)
     (b nil :type integer)
     (c nil :type list))

(defvar *a* 2)

(defun fn (x) (+ x *a*))


(test-suite ("join / split")

  (test "split"
    (split " " "1 2 3")
    :expect (list "1" "2" "3"))

  (test "join"
    (join " " (list "1" "2" (list "3" "4")))
    :expect "1 2 3 4"))



(test-suite ("conversions")

  (test "Symb"
    (symb "A")
    :expect 'a
    :test 'eq)

  (test "Keyw"
    (keyw "a")
    :expect	:A
    :test 'eq)

  (test "str 1 2 3"
    (str 1 2 3)
    :expect "123")

  (test "str (list (list 1 2 3) 2)"
    (str (list (list 1 2 3) 2))
    :expect "1232"))


(test-suite ("anaphoric macros")
  (test "Awith"
    (awith 22 (+ 1 it))
    :expect	23)

  (test "Aif"
    (aif t it)
    :expect	t)

  (test "Aif 2"
    (aif nil 2 it)
    :expect	nil)

  (test "if-bind"
    (if-bind (b nil) b 2)
    :expect	2)

  (test "if-bind 2"
    (if-bind (b 2) b 4)
    :expect	2)

  (test "awhen"
    (let ((a 1))
      (awhen nil (incf a) (incf a))
      a)
    :expect	1)

  (test "awhen 2"
    (let ((a 1))
      (awhen 2 (incf a it) (incf a it))
      a)
    :expect	5)

  (test "when-bind"
    (let ((a 1))
      (when-bind (b nil) (incf a) (incf a))
      a)
    :expect	1)

  (test "when-bind 2"
    (let ((a 1))
      (when-bind (b 2) (incf a b) (incf a b))
      a)
    :expect	5)

  (test "awhile"
    (let ((a 1)
          (c t))
      (awhile c
        (incf a)
        (when (> a 3) (setf c (not it))))
      a)
    :expect	4)

  (test "while-bind"
    (let ((a 1)
          (c t))
      (while-bind (b c)
        (incf a)
        (when (> a 3) (setf c (not b))))
      a)
    :expect	4)

  (test "aand"
    (aand 2 (+ it 3))
    :expect	5)

  (test "aand 2"
    (aand nil 2)
    :expect	nil)

  (test "acond"
    (acond 
      (nil 22)
      (23 it)
      (t 24))
    :expect	23)
  )


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

  (test "Curly brackets setter list 2"
      (let ((alist (list 1 2 3 4)))
         (setf {alist 1 3} (list 5 6))
         alist)
      :expect '(1 5 6 4))

  (test "Curly brackets setter string"
    (let ((astring "abc"))
      (setf {astring 1} #\g)
      astring)                           
   :expect "agc")

  (test "Curly brackets setter struct"
    (let ((s (make-test-struct :a "x" :b 2)))
       (setf {s 'a} "y")
       (test-struct-a s))
    :expect "y")
  )


(test-suite ("pick")
   (test "pick list"
      (pick (list 1 2 3) 0 1)
      :expect (list 1 2))

   (test "pick struct"
      (pick (make-test-struct :a "1" :b 2 :c (list 1 2)) 'a 'c)
      :expect (list "1" (list 1 2)))
   
   (test "pick string"
     (pick "abcd" 1 3)
     :expect (list #\b #\d))

   (test "pick lambda"
     (pick (lambda (x) (+ x 1)) 2 4)
     :expect (list 3 5)))


(test-suite ("compose")
   (test "compose reader"
     (car!list 2)
     :expect 2)

   (test "compose function"
     (funcall (o #'floor #'+) 1 2.5)
     :expect 3))


(test-suite ("in, range")
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

    (test "Range #\a..#\d"
      (range #\a #\d)
      :expect '(#\a #\b #\c #\d))
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
      (~ "/(\\w)\\w/g" "boda" 1)
      :expect (list "b" "d"))

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


(test-suite ("rm / probe-dir / mkdir / rmdir")
    (test "mkdir / probe-dir"
       (progn (mkdir "/tmp/clutchtest")
              (probe-dir "/tmp/clutchtest"))
       :expect-type 'pathname
       :fatal t)

    (test "rmdir"
       (progn (rmdir "/tmp/clutchtest")
              (probe-dir "/tmp/clutchtest"))
       :expect nil
       :fatal t)

    (test "rm"
           (progn (mkdir "/tmp/clutchtest")
                  (ungulp "/tmp/clutchtest/file" "contents")
                  (rm "/tmp/clutchtest/file")
                  (probe-file "/tmp/clutchtest"))
           :expect #p"/tmp/clutchtest/"
           :fatal t)

    (test "rm non recursive on dir"
       (progn (ungulp "/tmp/clutchtest/file" "contents")
              (rm "/tmp/clutchtest"))
       :expect-error t
       :fatal t)

    (test "rm non recursive on dir 2"
       (and (probe-dir "/tmp/clutchtest") (probe-file "/tmp/clutchtest"))
       :expect #p"/tmp/clutchtest/"
       :fatal t)

    (test "rm recursive"
       (progn (mkdir "/tmp/clutchtest/rep")
              (ungulp "/tmp/clutchtest/rep/file2" "contents")
              (rm "/tmp/clutchtest" :recursive t)
              (probe-dir "/tmp/clutchtest"))
       :expect nil
       :fatal t)
  )


(test-suite ("memoize")

  (test "memoize 1"
      (let ((a 0))
         (awith (memoize [+ _ a])
             {it 0}
             (setf a 1)
             {it 0}))
        :expect 0)

    (test "memoize 2"
            (let ((a 0))
               (awith (memoize [+ _ a] :remember-last 1)
                 {it 0}
                 (setf a 1)
                 {it 1}
                 {it 0}))
            :expect 1)

    (test "memoize 3"
            (let ((a 0))
               (awith (memoize [+ _ a] :expire 1)
                 {it 0}
                 (setf a 1)
                 (sleep 2)
                 {it 0}))
            :expect 1)

    (test "memoize 4"
            (let ((a 0))
               (awith (memoize [+ _ a] :remember-last 1 :expire 10)
                 {it 0}
                 (setf a 1)
                 {it 1}
                 {it 0}))
            :expect 1)

    (test "memoize 5"
            (let ((a 0))
               (awith (memoize [+ _ a] :remember-last 3)
                 {it 0}
                 (setf a 1)
                 {it 1}
                 {it 2}
                 {it 3}
                 {it 0}))
            :expect 1)
    )


;(test-suite ("memoize to disk")
;
;  (test "memoize-to-disk 1"
;      (let ((a 0))
;         (awith (memoize-to-disk [+ _ a])
;           {it 0}
;           (setf a 1)
;           {it 0}))
;      :expect 0)
;
;  (test "memoize-to-disk 2"
;          (let ((a 0))
;             (awith (memoize-to-disk [+ _ a] :remember-last 1)
;               {it 0}
;               (setf a 1)
;               {it 1}
;               {it 0}))
;          :expect 1)
;
;  (test "memoize-to-disk 3"
;          (let ((a 0))
;             (awith (memoize-to-disk [+ _ a] :expire 1)
;               {it 0}
;               (setf a 1)
;               (sleep 2)
;               {it 0}))
;          :expect 1)
;
;  (test "memoize-to-disk 4"
;          (let ((a 0))
;             (awith (memoize-to-disk [+ _ a] :remember-last 1 :expire 10)
;               {it 0}
;               (setf a 1)
;               {it 1}
;               {it 0}))
;          :expect 1)
;
;  (test "memoize-to-disk 5"
;          (let ((a 0))
;             (awith (memoize-to-disk [+ _ a] :remember-last 3)
;               {it 0}
;               (setf a 1)
;               {it 1}
;               {it 2}
;               {it 3}
;               {it 0}))
;          :expect 1)
;
;  (test "memoize-to-disk 6"
;          (let ((a 0))
;             (awith (memoize-to-disk [+ _ a] :remember-last 3)
;               {it 0}
;               {it 1}
;               {it 2}
;               {it 3}
;               (setf a 1)
;               {it 3}))
;          :expect 3)
;
;  )


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
   
       (test "ut (date :str \"now\")"
         (ut (date :str "now"))
         :expect  (get-universal-time))

       (test "ut (date :miltime 1245)"
         (ut (date :miltime 1245))
         :expect (ut (date :str "12:45")))
   
       (test "date-week (date January 22 1964 23:12)"
         (date-week (date :str "January 22 1964 23:12"))
         :expect 4)
   
       (test "date-wom (date January 22 1964 23:12)"
         (date-wom (date :str "January 22 1964 23:12"))
         :expect 4)
   
       (test "ut (date January 22 1964 23:12)"
         (ut (date :str "January 22 1964 23:12"))
         :expect (encode-universal-time 0 12 23 22  1 1964 -1))
   
       (test "d<"
         (d< (date :str "January 22 1964 23:12")
             (date :str "January 23 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect t)
   
       (test "d< 2"
         (d< (date :str "January 24 1964 23:12")
             (date :str "January 22 1964 23:12"))
         :expect nil)
   
       (test "d>"
         (d> (date :str "January 22 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect nil)
   
       (test "d> 2"
         (d> (date :str "January 24 1964 23:12")
             (date :str "January 23 1964 23:12")
             (date :str "January 22 1964 23:12"))
         :expect t)
   
       (test "d="
         (d= (date :str "January 22 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect nil)
   
       (test "d= 2"
         (d= (date :str "January 24 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect t)
   
       (test "d/="
         (d/= (date :str "January 22 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect t)
   
       (test "d/= 2"
         (d/= (date :str "January 24 1964 23:12")
             (date :str "January 24 1964 23:12"))
         :expect nil)
   
       (test "d-delta"
         (d-delta (date :str "January 24 1964 23:12")
                  (date :str "January 24 1964 22:12"))
         :expect 3600)
       )


(test-suite ("gulp / ungulp"
              :setup (rm "/tmp/clutchtest" :recursive t)
              :teardown (rm "/tmp/clutchtest" :recursive t))

   (test "gulp/ungulp file"
     (progn (ungulp "/tmp/clutchtest" "abcde") 
            (gulp "/tmp/clutchtest"))
     :expect "abcde")

   (test "gulp file offset"
     (gulp "/tmp/clutchtest" :offset 3)
     :expect "de")

   (test "gulp file negative offset"
     (gulp "/tmp/clutchtest" :offset -3)
     :expect "cde")

   (test "gulp file limit"
     (gulp "/tmp/clutchtest" :limit 3)
     :expect "abc")

   (test "gulp file offset+limit"
     (gulp "/tmp/clutchtest" :offset 2 :limit 3)
     :expect "cde")

   (test "gulp file offset+limit 2"
     (gulp "/tmp/clutchtest" :offset 2 :limit 2)
     :expect "cd")

   (test "gulp file neg offset+limit"
     (gulp "/tmp/clutchtest" :offset -3 :limit 2)
     :expect "cd")

   (test "gulp stream"
     (with-input-from-string (s "abcde")
       (gulp s))
     :expect "abcde")

   (test "gulp stream offset"
     (with-input-from-string (s "abcde")
       (gulp s :offset 3))
     :expect "de")

   (test "gulp stream negative offset"
     (with-input-from-string (s "abcde")
       (gulp s :offset -3))
     :expect                   "cde")

   (test "gulp stream limit"
     (with-input-from-string (s "abcde")
       (gulp s :limit 3))
     :expect                     "abc")

   (test "gulp stream offset+limit"
     (with-input-from-string (s "abcde")
       (gulp s :offset 2 :limit 3))
     :expect          "cde")

   (test "gulp stream offset+limit 2"
     (with-input-from-string (s "abcde")
       (gulp s :offset 2 :limit 2))
     :expect           "cd")

   (test "gulp stream neg offset+limit"
         (with-input-from-string (s "abcde")
           (gulp s :offset -3 :limit 2))
         :expect          "cd"))


(test-suite ("grep"
             :setup (progn (mkdir "/tmp/clutchtest")
                           (ungulp "/tmp/clutchtest/1" (format nil "a~%b~%c~%d~%e~%") :if-exists :overwrite)
                           (ungulp "/tmp/clutchtest/2" (format nil "b~%a~%c~%d~%e~%") :if-exists :overwrite)
                           (ungulp "/tmp/clutchtest/3" (format nil "o~%b~%c~%d~%e~%") :if-exists :overwrite)
                           (mkdir "/tmp/clutchtest/subdir")
                           (ungulp "/tmp/clutchtest/subdir/1" (format nil "o~%d~%c~%b~%e~%") :if-exists :overwrite)
                           (mkdir "/tmp/clutchtest/subdir2")
                           (ungulp "/tmp/clutchtest/subdir2/1" (format nil "o~%c~%b~%d~%e~%") :if-exists :overwrite))
             :teardown (rm "/tmp/clutchtest" :recursive t))

    (test "grep <regexp> <file>"
       (grep "/b/" "/tmp/clutchtest/1")
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "b"))))

    (test "grep <regexp> <dir>"
       (grep "/b/" "/tmp/clutchtest/")
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "b"))
                     (list #p"/tmp/clutchtest/2" 1 (list "b"))
                     (list #p"/tmp/clutchtest/3" 4 (list "b"))))

    (test "grep <regexp> <dir> recursive"
       (grep "/b/" "/tmp/clutchtest/" :recursive t)
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "b"))
                     (list #p"/tmp/clutchtest/2" 1 (list "b"))
                     (list #p"/tmp/clutchtest/3" 4 (list "b"))
                     (list #p"/tmp/clutchtest/subdir/1" 4 (list "b"))
                     (list #p"/tmp/clutchtest/subdir2/1" 3 (list "b"))))

    (test "grep <regexp> <dir> recursive names-only"
       (grep "/b/" "/tmp/clutchtest/" :recursive t :names-only t)
       :expect (list #p"/tmp/clutchtest/1"
                     #p"/tmp/clutchtest/2"
                     #p"/tmp/clutchtest/3"
                     #p"/tmp/clutchtest/subdir/1"
                     #p"/tmp/clutchtest/subdir2/1"))

    (test "grep <regexp> <dir> recursive capture"
       (grep "/(\\w)/" "/tmp/clutchtest/" :recursive t :capture 1)
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "b" "b"))
                     (list #p"/tmp/clutchtest/2" 1 (list "b" "b"))
                     (list #p"/tmp/clutchtest/3" 4 (list "b" "b"))
                     (list #p"/tmp/clutchtest/subdir/1" 4 (list "b" "b"))
                     (list #p"/tmp/clutchtest/subdir2/1" 3 (list "b" "b"))))

    (test "grep <regexp> <dir> recursive matches-only + capture"
       (grep "/(\\w)/" "/tmp/clutchtest/" :recursive t :matches-only t :capture 1)
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "2" "2"))))

    (test "grep <regexp> <dir> recursive lines-only"
       (grep "/b/" "/tmp/clutchtest/" :recursive t :lines-only t)
       :expect (list 2 1 4 4 3))
 )

     
 (test-suite ("gulplines, mapflines"
              :setup (ungulp "/tmp/clutchtest" (format nil "a~%b~%c~%d~%e~%") :if-exists :overwrite)
              :teardown (rm "/tmp/clutchtest"))

   (test "mapflines"
         (mapflines #'identity "/tmp/clutchtest")
         :expect (list "a" "b" "c" "d" "e"))

   (test "gulplines"
         (gulplines "/tmp/clutchtest")
         :expect (list "a" "b" "c" "d" "e"))

   (test "mapflines offset"
         (mapflines #'identity "/tmp/clutchtest"
                    :offset 1)
         :expect (list "b" "c" "d" "e"))

   (test "mapflines limit"
         (mapflines #'identity "/tmp/clutchtest"
                    :limit 2)
         :expect (list "a" "b"))

   (test "mapflines limit + offset"
         (mapflines #'identity "/tmp/clutchtest"
                    :offset 2
                    :limit 2)
         :expect (list "c" "d"))

   (test "mapflines -offset"
     (mapflines #'identity "/tmp/clutchtest"
           :offset -2)
     :expect (list "d" "e"))

   (test "mapflines -offset+limit"
     (mapflines #'identity "/tmp/clutchtest"
           :offset -3
           :limit 2)
     :expect (list "c" "d"))

   (test "mapflines stream"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"))
     :expect (list "a" "b" "c" "d" "e"))

   (test "mapflines stream offset"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"
           :offset 1))
     :expect (list "b" "c" "d" "e"))

   (test "mapflines stream limit"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"
           :limit 2))
     :expect (list "a" "b"))

   (test "mapflines stream limit+offset"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"
           :offset 2
           :limit 2))
     :expect (list "c" "d"))

   (test "mapflines stream -offset"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"
           :offset -2))
     :expect (list "d" "e"))

   (test "mapflines stream -offset+limit"
     (with-input-from-string (s "a\nb\nc\nd\ne")
        (mapflines #'identity "/tmp/clutchtest"
           :offset -3
           :limit 2))
     :expect (list "c" "d"))

   (test "with-each-line string"
     (let ((result nil))
       (with-each-line ("a\nb\nc\nd\ne" :offset -3 :limit 2)
         (push it result))
       result)
     :expect (list "c" "d"))
   )

(test-suite ("keys, mkhash")
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
      :expect '(a b)))

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
  )

  (test-suite ("before / after")
    (before 'fn (setf *a* 3))
    (after  'fn (setf *a* 4))
    (setf *a* 2)

    (test "before"
      (fn 0)
      :expect 3)

    (test "after"
      *a*
      :expect 4)
  )

  (test-suite ("uuid")

    (let* ((hexd "[0-9A-Z]")
           (ure (str "/" hexd "{8}-" hexd "{4}-" hexd "{4}-" hexd "{4}-" hexd "{12}" "/")))

      (test "uuid default"
         (? (~ ure (uuid)))
         :expect t)

      (test "uuid v1"
         (? (~ ure (uuid :v 1)))
         :expect t)

      (test "uuid v3"
         (? (~ ure (uuid :v 3 :ns 'URL :name "name")))
         :expect t)

      (test "uuid v4"
         (? (~ ure (uuid :v 4)))
         :expect t)

      (test "uuid v5"
         (? (~ ure (uuid :v 5 :ns 'OID :name "name")))
         :expect t)))
