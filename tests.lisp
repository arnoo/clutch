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


(test-suite ("pushend / pushendnew / popend")

  (test "pushend"
    (let ((l (list 1 2 3)))
      (pushend 4 l)
      l)
    :expect (list 1 2 3 4))

  (test "pushend 2"
    (let ((l nil))
      (pushend 4 l)
      l)
    :expect (list 4))

  (test "pushendnew"
    (let ((l (list 1 2 3)))
      (pushendnew 4 l)
      l)
    :expect (list 1 2 3 4))

  (test "pushendnew 2"
    (let ((l nil))
      (pushendnew 4 l)
      l)
    :expect (list  4))

   (test "pushendnew 3"
    (let ((l (list 1 2 3)))
      (pushendnew 3 l)
      l)
    :expect (list 1 2 3))

  (test "popend"
    (let ((l (list 1 2 3)))
      (list (popend l) l))
    :expect (list 3 (list 1 2)))
   
   )

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

(test-suite ("string manipulation")

    (test "uc"
      (uc "bAc123")
      :expect "BAC123")

    (test "lc"
      (lc "BaC123")
      :expect "bac123")

    (test "lpad"
      (lpad "abc" 6)
      :expect "   abc")

    (test "lpad with"
      (lpad "ab" 6 :with "c")
      :expect "ccccab")

    (test "rpad"
      (rpad "rs" 3)
      :expect "rs ")

    (test "rpad with"
      (rpad "rs" 5 :with "g")
      :expect "rsggg")

    (test "strip"
      (strip "asbd")
      :expect "asbd")

    (test "strip 2"
      (strip (format nil "~% asbd ~% "))
      :expect "asbd")
)


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


(test-suite ("reader macros")
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
  
    (test "Regexp filter other types"
       (~ "/\\d/" (list #p"a" #p"a1" "b" "2"))
       :expect '(#p"a1" "2"))
  
    (test "Regexp filter reverse other types"
       (/~ "/\\d/" (list #p"a" #p"a1" "b" "2"))
       :expect '(#p"a" "b"))

    (test "Regexp filter"
       (~ "/\\d/" (list "a" "a1" "b" "2"))
       :expect '("a1" "2"))
  
    (test "Regexp filter capturing single match"
       (~ "/\\w(\\d)/" (list "a" "a1" "b" "2") 1)
       :expect '("1"))
    
    (test "Regexp filter reverse"
       (/~ "/\\d/" (list "a" "a1" "b" "2"))
       :expect '("a" "b"))
   
    (test "Regexp match reverse"
       (/~ "/\\d/" "a1")
       :expect nil)
 
    (test "Regexp match reverse 2"
       (/~ "/\\d/" "a")
       :expect t)

    (test "resplit"
       (resplit "/\\d/" "a1b2c3d4")
       :expect (list "a" "b" "c" "d"))
    )


(test-suite ("filesystem interactions")

    (test "ls non existing file"
      (ls "adfakjhoqiuyerhkljlaskdj.sssaapoiu442223")
      :expect nil)
    
    (test "ls existing file"
      (car (ls "clutch.lisp"))
      :expect-type 'pathname)

    (test "mkdir / probe-dir"
       (progn (mkdir "/tmp/clutchtest")
              (probe-dir "/tmp/clutchtest"))
       :expect #p"/tmp/clutchtest/"
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

(test-suite ("ls"
             :setup (progn (mkdir "/tmp/clutchtest")
                           (ungulp "/tmp/clutchtest/1" (format nil "a~%b~%c~%d~%e~%") :if-exists :overwrite)
                           (ungulp "/tmp/clutchtest/2" (format nil "b~%a~%c~%d~%e~%") :if-exists :overwrite)
                           (ungulp "/tmp/clutchtest/3" (format nil "o~%b~%c~%d~%e~%") :if-exists :overwrite)
                           (mkdir "/tmp/clutchtest/subdir")
                           (ungulp "/tmp/clutchtest/subdir/1" (format nil "o~%d~%c~%b~%e~%") :if-exists :overwrite)
                           (mkdir "/tmp/clutchtest/subdir2")
                           (ungulp "/tmp/clutchtest/subdir2/1" (format nil "o~%c~%b~%d~%e~%") :if-exists :overwrite))
             :teardown (rm "/tmp/clutchtest" :recursive t))

    (test "ls"
       (ls "/tmp/clutchtest")
       :expect (list #p"/tmp/clutchtest/1" #p"/tmp/clutchtest/2" #p"/tmp/clutchtest/3" #p"/tmp/clutchtest/subdir/" #p"/tmp/clutchtest/subdir2/"))

    (test "ls files-only"
       (ls "/tmp/clutchtest" :files-only t)
       :expect (list #p"/tmp/clutchtest/1" #p"/tmp/clutchtest/2" #p"/tmp/clutchtest/3"))

    (test "ls dirs-only"
       (ls "/tmp/clutchtest" :dirs-only t)
       :expect (list #p"/tmp/clutchtest/subdir/" #p"/tmp/clutchtest/subdir2/"))

    (test "ls recursive"
       (ls "/tmp/clutchtest" :recursive t)
       :expect (list #p"/tmp/clutchtest/1" #p"/tmp/clutchtest/2" #p"/tmp/clutchtest/3" #p"/tmp/clutchtest/subdir/" #p"/tmp/clutchtest/subdir2/" #p"/tmp/clutchtest/subdir/1" #p"/tmp/clutchtest/subdir2/1"))

    (test "ls recursive files-only"
       (ls "/tmp/clutchtest" :recursive t :files-only t)
       :expect (list #p"/tmp/clutchtest/1" #p"/tmp/clutchtest/2" #p"/tmp/clutchtest/3" #p"/tmp/clutchtest/subdir/1" #p"/tmp/clutchtest/subdir2/1"))

    (test "ls recursive dirs-only"
       (ls "/tmp/clutchtest" :recursive t :dirs-only t)
       :expect (list #p"/tmp/clutchtest/subdir/" #p"/tmp/clutchtest/subdir2/"))

    (test "ls single file"
       (ls "/tmp/clutchtest/1")
       :expect (list #p"/tmp/clutchtest/1"))

    (test "ls non existing file"
       (ls "/tmp/clutchtest/1afadslkjwer")
       :expect nil)

    (test "ls single file recursive"
       (ls "/tmp/clutchtest/1" :recursive t)
       :expect (list #p"/tmp/clutchtest/1"))

    (test "ls single file dirs-only"
       (ls "/tmp/clutchtest/1" :dirs-only t)
       :expect nil)

    (test "ls single file files-only"
       (ls "/tmp/clutchtest/1" :files-only t)
       :expect (list #p"/tmp/clutchtest/1"))
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
                 {it 1}
                 (setf a 1)
                 {it 2}
                 {it 3}
                 (list {it 0} {it 1})))
            :expect (list 1 2))
    )


(test-suite ("memoize to disk")

  (test "memoize-to-disk 1"
      (let ((a 0))
         (awith (memoize-to-disk [+ _ a])
           {it 0}
           (setf a 1)
           {it 0}))
      :expect 0)

  (test "memoize-to-disk 2"
          (let ((a 0))
             (awith (memoize-to-disk [progn (sleep 1) (+ _ a)] :remember-last 1)
               {it 0}
               (setf a 1)
               (list {it 1} {it 0})))
          :expect (list 2 1))

  (test "memoize-to-disk 3"
          (let ((a 0))
             (awith (memoize-to-disk [+ _ a] :expire 1)
               {it 0}
               (setf a 1)
               (sleep 2)
               {it 0}))
          :expect 1)

  (test "memoize-to-disk 4"
          (let ((a 0))
             (awith (memoize-to-disk [progn (sleep 1) (+ _ a)] :remember-last 1 :expire 10)
               {it 0}
               (setf a 1)
               {it 1}
               {it 0}))
          :expect 1)

  (test "memoize-to-disk 5"
          (let ((a 0))
             (awith (memoize-to-disk [progn (sleep 1) (+ _ a)] :remember-last 3)
               {it 0}
               (setf a 1)
               {it 1}
               {it 2}
               {it 3}
               {it 0}))
          :expect 1)

  (test "memoize-to-disk 6"
          (let ((a 0))
             (awith (memoize-to-disk [progn (sleep 1) (+ _ a)] :remember-last 3)
               {it 0}
               {it 1}
               {it 2}
               {it 3}
               (setf a 1)
               {it 3}))
          :expect 3)

  (test "memoize-to-disk hash 2 args"
    (let ((h (mkhash 'a 1))
          (b 1))
       (awith (memoize-to-disk [+ {_ 'a} b __])
           {it h 0}
           (setf b 2)
           {it h 0}))
    :expect 2)

    (test "memoize-to-disk changing hash" ; Fails on ABCL (No CL-STORE)
      (let ((h (mkhash 'a 1)))
           (awith (memoize-to-disk [_ 'a])
                  {it h}
                  (setf {h 'a} 2)
                  {it h}))
      :expect 2)
  )


(test-suite ("time and date")
       (test "ut"
         (ut)
         :expect (get-universal-time))
   
       (test "ut now"
         (ut "now")
         :expect (get-universal-time))
   
       (test "ut January 22 1964 23:12 +0200"
         (ut "January 22 1964 23:12 +0200")
         :expect (encode-universal-time 0 12 23 22  1 1964 -2))
   
       (test "ut (date)"
         (ut (date))
         :expect (get-universal-time))
   
       (test "ut (date :str \"now\")"
         (ut (date :str "now"))
         :expect (get-universal-time))

       (test "ut (date :miltime 1245)"
         (ut (date :miltime 1245))
         :expect (ut (date :str "12:45")))

       (test "miltime"
         (miltime (date :str "12:45"))
         :expect 1245.0)
   
       (test "date-week (date January 22 1964 23:12)"
         (date-week (date :str "January 22 1964 23:12"))
         :expect 4)
   
       (test "date-wom (date January 22 1964 23:12)"
         (date-wom (date :str "January 22 1964 23:12"))
         :expect 4)
   
       (test "ut (date January 22 1964 23:12 +0200)"
         (ut (date :str "January 22 1964 23:12 +0200"))
         :expect (encode-universal-time 0 12 23 22  1 1964 -2))
   
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

       (test "y-m-d"
         (y-m-d (date :str "March 1 1964"))
         :expect "1964-03-01")

       (test "date-gnu"
         (date-gnu (date :str "Wed, 24 Jun 1992 22:12:00 +0200" :zone -2) "%s")
         :expect "709416720")

       (test "date-rfc-2822"
         (date-rfc-2822 (date :str "Wed, 24 Jun 1992 22:12:00 +0200" :zone -2))
         :expect "Wed, 24 Jun 1992 22:12:00 +0200")

       (test "d+"
         (d+ (date :str "January 24 1964 23:12") (* 60 22))
         :expect (date :str "January 24 1964 23:34")
         :test #'d=)

       (test "d<="
         (d<= (date :str "December 24 1954 23:12") (date :str "January 14 1924 23:12"))
         :expect nil)

       (test "d<= 2"
         (d<= (date :str "February 14 1924 23:12") (date :str "December 24 1954 23:12"))
         :expect t)

       (test "d<= 3"
         (d<= (date :str "December 24 1954 23:12") (date :str "December 24 1954 23:12"))
         :expect t)

       (test "d>="
         (d>= (date :str "January 24 1964 23:12") (date :str "July 11 1924 23:12"))
         :expect t)

       (test "d>= 2"
         (d>= (date :str "July 11 1924 23:12") (date :str "January 24 1964 23:12"))
         :expect nil)

       (test "d>= 3"
         (d>= (date :str "January 24 1964 23:12") (date :str "January 24 1964 23:12"))
         :expect t)

       (test "decode-duration"
         (decode-duration "22s")
         :expect 22)

       (test "decode-duration 2"
         (decode-duration "1y 2M 3w 5d 4h 5m 22s")
         :expect (+ 22 (* 5 60) (* 4 3600) (* 5 3600 24) (* 3 3600 24 7) (* 3600 24 365) (* 2 3600 24 30)))

       (test "encode-duration"
         (encode-duration 22)
         :expect "22s")

       (awith (+ 22 (* 5 60) (* 4 3600) (* 5 3600 24) (* 3 3600 24 7) (* 3600 24 365) (* 2 3600 24 30))
         (test "encode-duration 2"
             (decode-duration (encode-duration it))
             :expect it))

       (test "to-zone"
         (let* ((d  (date :str "Wed, 24 Jun 1992 22:12:00 +0200"))
                (d2 (to-zone d 3)))
            (list (d-delta d2 d) (date-zone d2)))
         :expect (list 0 3))
       )

(test-suite ("f= f> f< ...")
      (test "f="
        (f= #'sin 1 1)
        :expect t)
      (test "f= 2"
        (f= #'sin 1 2)
        :expect nil)
      (test "f-equal"
        (f-equal #'length "a" "b")
        :expect t)
      (test "f-equal 2"
        (f-equal #'length "aa" "b")
        :expect nil)
      (test "f>"
        (f> #'sin 2 1)
        :expect t)
      (test "f> 2"
        (f> #'sin 1 2)
        :expect nil)
      (test "f> 3"
        (f> #'sin 1 1)
        :expect nil)
      (test "f<"
        (f< #'sin 1 2)
        :expect t)
      (test "f< 2"
        (f< #'sin 2 1)
        :expect nil)
      (test "f< 3"
        (f< #'sin 1 1)
        :expect nil)
      (test "f>="
        (f>= #'sin 2 1)
        :expect t)
      (test "f>= 2"
        (f>= #'sin 1 2)
        :expect nil)
      (test "f>= 3"
        (f>= #'sin 1 1)
        :expect t)
      (test "f<="
        (f<= #'sin 1 2)
        :expect t)
      (test "f<= 2"
        (f<= #'sin 2 1)
        :expect nil)
      (test "f<= 3"
        (f<= #'sin 1 1)
        :expect t))

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
                     (list #p"/tmp/clutchtest/3" 2 (list "b"))))

    (test "grep <regexp> <dir> recursive"
       (grep "/b/" "/tmp/clutchtest/" :recursive t)
       :expect (list (list #p"/tmp/clutchtest/1" 2 (list "b"))
                     (list #p"/tmp/clutchtest/2" 1 (list "b"))
                     (list #p"/tmp/clutchtest/3" 2 (list "b"))
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
       (grep "/(b)/" "/tmp/clutchtest/" :recursive t :capture 1)
       :expect (list (list #p"/tmp/clutchtest/1" 2 "b")
                     (list #p"/tmp/clutchtest/2" 1 "b")
                     (list #p"/tmp/clutchtest/3" 2 "b")
                     (list #p"/tmp/clutchtest/subdir/1" 4 "b")
                     (list #p"/tmp/clutchtest/subdir2/1" 3 "b")))

    (test "grep <regexp> <dir> recursive matches-only + capture"
       (grep "/(\\w)/" "/tmp/clutchtest/" :recursive t :matches-only t :capture 1)
       :expect (list "a" "b" "c" "d" "e" "b" "a" "c" "d" "e" "o" "b" "c" "d" "e" "o" "d" "c" "b" "e" "o" "c" "b" "d" "e"))

    (test "grep <regexp> <dir> recursive lines-only"
       (grep "/b/" "/tmp/clutchtest/" :recursive t :lines-only t)
       :expect (list "b" "b" "b" "b" "b"))
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
   )

(test-suite ("keys, kvalues, mkhash")
    (test "mkhash"
      {(mkhash "a" 2 "b" 3) "a"}
      :expect 2)

    (test "keys hash"
      (keys (mkhash 'a 1 'b 2))
      :expect '(a b))

    (test "kvalues hash"
      (kvalues (mkhash 'a 1 'b 2))
      :expect '(1 2))

    (test "keys struct"
      (keys (make-test-struct :a "1" :b 2))
      :expect (list 'a 'b 'c))

    (test "kvalues struct"
      (kvalues (make-test-struct :a "1" :b 2))
      :expect (list "1" 2 nil)))

(test-suite ("getenv")

    (test "getenv PATH"
      (getenv "PATH")
      :expect-type 'string)
    
    (test "setf getenv"
      (progn
        (setf (getenv "CLUTCHTEST") "ABC")
        (getenv "CLUTCHTEST"))
      :expect "ABC")

)

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
