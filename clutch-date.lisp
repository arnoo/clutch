;
;   Copyright 2015 Arnaud Bétrémieux <arnaud@btmx.fr>, except where
;   mentioned otherwise.
;
;   The program in this file is free software: you can redistribute it
;   and/or modify it under the terms of the GNU General Public License
;   as published by the Free Software Foundation, either version 3 of
;   the License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(defpackage :clutch-date
    (:use    #:cl #:simple-date #:clutch #:cffi #:named-readtables)
    (:export #:ut
             #:now
             #:now+
             #:y-m-d
             #:date-rfc-2822
             #:date-rfc-3339
             #:date-format-gnu
             #:date-unix
             #:miltime
             #:time-min
             #:time-max
             #:time-incf
             #:time-decf
             ))

(in-package :clutch-date)
(in-readtable clutch)

(defvar +months-abbr+ (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar +months+ (list "" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
(defvar +days-abbr+ (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(defvar +days+ (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defmacro with-decoded-timestamp (timestamp &body body)
  `(multiple-value-bind (year month day hour minute second millisecond) 
                        (decode-timestamp timestamp)
     ,@body))

(defun date-rfc-3339 (&optional (timestamp (now)))
  "Returns a date in the RFC-3339 format : 1937-01-01T12:00:27.87+00:20"
  (with-decoded-timestamp timestamp
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A"
           year
           month
           day
           hour
           minute
           second
           "Z")))

(defun date-rfc-2822 (&optional (timestamp (now)))
  "Returns a date in the RFC-2822 format : Mon, 07 Aug 2006 12:34:56 -0600"
  (with-decoded-timestamp timestamp
    (declare (ignore millisecond))
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
         {+days-abbr+ (day-of-week timestamp)}
         day
         {+months-abbr+ month}
         year
         hour
         minute
         second
         "+0000")))

(defun date-unix (&optional (timestamp (now)))
  (ut-to-unix (timestamp-to-universal-time timestamp)))

(defun date-format-gnu (timestamp format)
  (strip (sh (str "date -d '" (date-rfc-2822 timestamp) "' +'" format "'"))))

(defun ut (&optional str-or-timestamp)
  "Returns the current Common Lisp universal-time, or if <str-or-date> is specified, the universal-time described by that string"
  (if str-or-timestamp
      (cond
        ((subtypep (type-of str-or-timestamp) 'date) (timestamp-to-universal-time str-or-timestamp))
        ((stringp str-or-timestamp) (strtout str-or-timestamp))
        (t (error "Argument to (ut) should be a string or a simple-date:timestamp")))
      (get-universal-time)))

(defun strtout (str)
  "Converts a string to a Common Lisp universal-time"
  (let ((result (sh (str "date -d \"" str "\" +%s"))))
    (if (/~ "/^(-|)\\d+\\n$/" result)
      (error "Invalid date string : ~A" str)
      (unix-to-ut (read-from-string result)))))

(defun miltime (&optional (timestamp (now)))
  "Returns a 'military' time for timestamp : 1243.22 for 12:43:22"
  (with-decoded-timestamp timestamp
    (float (+ (* 100 hour)
              minute
              (/ second 100)))))
  
(defun y-m-d (&optional (timestamp (now)))
  "Date in format YYYY-MM-DD"
  (with-decoded-timestamp timestamp
    (str (lpad year  4 :with 0) "-"
         (lpad month 2 :with 0) "-"
         (lpad day   2 :with 0))))

(defun now ()
  (universal-time-to-timestamp (ut)))

(defun now+ ()
  (multiple-value-bind (sec usec) (gettimeofday)
    (time-add (universal-time-to-timestamp (unix-to-ut sec))
              (encode-interval :millisecond (round (/ usec 1000))))))

(defun time-max (&rest times)
  (first (sort times 'time>)))

(defun time-min (&rest times)
  (first (sort times 'time<)))

(defmacro time-incf (time interval)
  `(setf ,time
         (time-add ,time ,interval)))

(defmacro time-decf (time interval)
  `(setf ,time
         (time-subtract ,time ,interval)))

;;; gettimeofday from CCFI examples
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(define-foreign-type null-pointer-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser null-pointer))

(defmethod translate-to-foreign (value (type null-pointer-type))
  (cond
    ((null value) (null-pointer))
    ((null-pointer-p value) value)
    (t (error "~A is not a null pointer." value))))

(define-foreign-type syscall-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser syscall-result))

(defmethod translate-from-foreign (value (type syscall-result-type))
  (if (minusp value)
      (error "System call failed with return value ~D." value)
      value))

(defcfun ("gettimeofday" %gettimeofday) syscall-result
  (tp :pointer)
  (tzp null-pointer))

(defun gettimeofday ()
  (with-foreign-object (tv 'timeval)
    (%gettimeofday tv nil)
    (with-foreign-slots ((tv-sec tv-usec) tv timeval)
      (values tv-sec tv-usec))))
