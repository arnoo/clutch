Clutch
======

This is the utility library I wrote as I was learning Common Lisp. I still use it all the time, and it evolves as I require new utilities.

Yet, it still contains code that is not great, but works well enough that I have not taken the time to rewrite it.

I have released it under the GPL, in the hope that it can be useful to somebody, and that I might get suggestions for additions or improvements.

Most functions should work pretty well on SBCL and ABCL, which are the two implementations I use regularly. There is code specific to other implementations in some places, usually because it comes from other people, but I have never tested any of it.

There is a test suite (tests.lisp), based on my test library (cleanser), that should cover most of the code.

Some date functions rely on the GNU 'date' program. While a bit ugly and non-portable, this works well enough for my purposes right now, and would be too complex to rewrite.


There are two main reader macros : {} and [].

The first is used to access data structures :
    > {"abc" 1} 
    #\b
    > {"abc" 1 2}
    "bc"
    > {"abc" 1 -1}
    "bc"
    > {(list 1 2 3) 1 -1}
    (2 3)
    > {some-struct-or-object 'a}
    <value of slot a>

This was inspired by Arc's possibility of using a functional position for data structures. In that spirit, {} can also be used to apply functions and lambdas :
    > {(lambda (x) (+ 1 x)) 2}
    3

Except lambda, these forms can also be used with setf :
    > (setf l (list 1 2))
    (1 2)
    > (setf {l 1} 3)
    3
    > l
    (1 3)


[] is used for anonymous functions and is supposed to work like in Arc :
    > (mapcar [+ 1 _] (list 1 2))
    (2 3)

It can also be used with structures in a functional position like this (and then uses the same accessor function as {})
    > (mapcar [_ 2] (list "abc" "def"))
    (#\c #\f)


Memoization
-----------

    > (memoize fn)
Will return a memoized version of <fn>. 

    > (memoize-to-disk fn)
Does the same, but stores the cache on disk, which can be useful if the results are large or if they need to be kept between sessions.

Both memoization functions accept the keyword arguments <remember-last>, which indicates the number of results to remember, and <expire>, which controls how many seconds results are remembered for. Memoize-to-disk has other parameters to customize the storage of results : ###


Reading files
-------------

gulp is a function to read a file or stream in one go and return a string :
    > (gulp "/var/log/messages")
will return a string with the contents of file /var/log/messages

Say I want to read only a part of the file, I could use the keyword parameters <offset> and <limit> :

    > (gulp "/var/log/messages" :offset 20)
Will read the whole file starting at character 20

    > (gulp "/var/log/messages" :offset 20 :limit 30)
Will read starting at character 20 and will read only 30 characters.

    > (gulp "/var/log/messages" :offset -20 :limit 10)
Will read character 20 from the end and will read only 10 characters.

gulplines does the same, but returns a list of strings, each string a line from the file. It also accepts offset and limit parameters that work the same way, but count in lines and not characters.
    > (gulplines "/var/log/messages" :offset -20 :limit 10)
Will return a list of 10 lines starting 20 lines from the end of the file.

If working on a large file, I probably don't want to load it all in memory, so I also have the option of working on it line by line :
    > (with-each-line ("/var/log/messages") (print it))

I can use the same <offset> and <limit> parameters there :
    > (with-each-line ("/var/log/messages" :offset -20 :limit 10) (print it))

All of these functions accept a stream instead of a filename, or a URL :
    > (with-each-line ("http://www.google.com") (print it))
    > (gulp "http://www.google.com")


Stack manipulation
------------------

pushend, pushendnew and popend act like push, pushnew and pop but with the end of a list and not the beginning :

    > (setf lst (list 1 2 3))
    (1 2 3)
    
    > (pushend 4 l)
    (1 2 3 4)
    
    > (pushendnew 4 l)
    (1 2 3 4)
    
    > (popend l)
    4

    > l
    (1 2 3)
    
    > (pushendnew 4 l)
    (1 2 3 4)


Anaphoric macros
----------------

These are inspired by Paul Graham's book "On Lisp".

awith (form &rest body)
Evaluates <body> with <it> bound to <form>.

aif (test then &optional else)
Evaluates <then with <it> bound to result of evaluating <test> if this result is not nil, <else> otherwise
    > (aif a (print it) (print "a is nil"))

awhen (test &body body)
Evaluates <body> with <it> bound to result of evaluating <test> if this result is not nil
    > (awhen a (print it) (print "a is not nil"))

awhile (test &body body)
Loops on <body> with <it> bound to result of evaluating <test> as long as this result is not nil
    > (awhile a (print it) (print "a is not nil"))

aand (test &rest tests)
    > (aand (list 1 2) (cdr it))
    2

acond (&rest forms)
Like a regular cond, except the result of evaluating the condition form can be accessed as <it>.


Regular expressions
-------------------

CL-PPCRE is a great library, but I was missing the simplicity of the Perl syntax. This is what I have tried to reproduce with functions ~, /~ and ~s.
Unfortunately, backspaces have to be escaped. Maybe a clever reader macro could be nicer than regexps as strings.

Filter a list : keep only elements that match the regexp :
    > (~ "/\\w{3}/" (list "abc" "def" "ac"))
    ("abc" "def")

Filter a list : keep only elements that don't match the regexp :
    > (/~ "/\\w{3}/" (list "abc" "def" "ac"))
    ("ac")

Check whether a string matches a regexp :
    > (~ "/\\w{3}/" "ac")
    NIL

    > (~ "/\\w{3}/" "abc")
    ("abc")

Extract part of the string with a capture group :
    > (~ "/(\\w{2})\\w/" "abc")
    ("abc" "ab")

If I am only interested in the first capture group :
    > (~ "/(\\w{2})\\w/" "abc" 1)
    "ab"

### flags : global, case insensitive

Check whether a string matches a regexp (reversed) :
    > (/~ "/\\w{3}/" "ac")
    t

Do a regexp based substitution :
    > (~s "/b/a/" "bob")
    "aob"

Globally :
    > (~s "/b/a/g" "bob")
    "aoa"

List all the lines that match regexp in a file :
    > (grep "/\\[EE\\]/" "/var/log/Xorg.0.log")
    ((#P"/var/log/Xorg.0.log" 6 ("EE")) (#P"/var/log/Xorg.0.log" 15 ("EE"))
     (#P"/var/log/Xorg.0.log" 52 ("EE")) (#P"/var/log/Xorg.0.log" 111 ("EE")))

Each element in the list returned has the form (<filename> <line number> <result of ~>).

List all the lines that match regexp in directory, only first level :
    > (grep "/\\[EE\\]/" "/var/log/")
    ((#P"/var/log/Xorg.0.log" 6 ("EE")) (#P"/var/log/Xorg.0.log" 15 ("EE"))
     (#P"/var/log/Xorg.0.log" 52 ("EE")) (#P"/var/log/Xorg.0.log" 111 ("EE")))

List all the lines that match regexp in directory, recursively :
    > (grep "/\\[EE\\]/" "/var/log/" :recursive t)
    ((#P"/var/log/Xorg.0.log" 6 ("EE")) (#P"/var/log/Xorg.0.log" 15 ("EE"))
     (#P"/var/log/Xorg.0.log" 52 ("EE")) (#P"/var/log/Xorg.0.log" 111 ("EE")))

Return only the matches :
    > (grep "/EE/" "/var/log/Xorg.0.log" :matches-only t)
    (("EE") ("EE") ("EE") ("EE"))

Return only the filenames :
    > (grep "/EE/" "/var/log/Xorg.0.log" :names-only t)
    (#P"/var/log/Xorg.0.log")

Return only the first capture :
    > (grep "/(EE)/" "/var/log/Xorg.0.log" :matches-only t :capture 1)
    ("EE" "EE" "EE" "EE")


Other Filesystem interactions
-----------------------------

Delete a file :
    > (rm "/tmp/testfile")
    t

Delete a directory recursively
    > (rm "/tmp/testdir" :recursive t)
    t

Delete an empty directory
    > (rmdir "/tmp/testdir)
    t

Create a directory
    > (mkdir "/tmp/testdir")
    #p"/tmp/testdir"

Check that a file exists (returns nil if file does not exist)
    > (ls "/tmp/testfile")
    (#p"/tmp/testfile")

List the contents of a directory
    > (ls "/tmp/testdir")
    (#p"/tmp/testdir/file1" #p"/tmp/testdir/file2" #p"/tmp/testdir/subdir1")

List only files
    > (ls "/tmp/testdir" :files-only t)
    (#p"/tmp/testdir/file1" #p"/tmp/testdir/file2")

List only directories
    > (ls "/tmp/testdir" :dirs-only t)
    (#p"/tmp/testdir/subdir1")

List the contents of a directory recursively
    > (ls "/tmp/testdir" :recursive t)
    (#p"/tmp/testdir/file1" #p"/tmp/testdir/file2" #p"/tmp/testdir/subdir1" #p"/tmp/testdir/subdir1/file3")

List only files recursively
    > (ls "/tmp/testdir" :files-only t :recursive t)
    (#p"/tmp/testdir/file1" #p"/tmp/testdir/file2" #p"/tmp/testdir/subdir1/file3")

Check if a directory exists (returns nil if the given path does not exist or points to a file))
    > (probe-dir "/tmp/testdir")
    #p"/tmp/testdir"

Function composition
--------------------

Function o composes other functions :
(o #'sqrt #'sin) is a function that applies sin then sqrt

Arc-like function composition : sqrt!sin transforming into (o #'sqrt #'sin)
would be nice, but as reader macros can't read backwards, I'm not sure this is possible.

Another thing I often find myself needing is "mapping" function composition : i.e.
(= (car x) (car y))

It would be nice to have a generic syntax for that like (=^car x y)
I'm not sure this can be done either. For lack of a better solutions, I use comparison functions that take another function to be mapped to the arguments before comparison :

(f= #'car x y) is equivalent to (= (car x) (car y))

f>, f<, f<=, f>= and f-equal work the same way


Before and After
----------------

Functions "before" and "after" attach code to be executed respectively before and after a function is called :

    > (defun a (x) (+ x 2))
    > (before 'a (format t \"a to be called with args ~A\" args))"
    > (after 'a (format t \"a was called with args ~A\" args))"


Date manipulations
------------------

Clutch introduces a date structure with the following slots :

    s      seconds
    m      minutes
    h      hours
    day    day of the month
    month  month
    year   year
    dow    day of week (0 for Sunday to 6)
    dst    t or nil for DST or not
    zone   timezone (hours to add to UTC, not counting DST ###)

Date structures can be created with the "date" function :

Creating a date from a Common Lisp universal time :
    > (date :ut 3506915075)
    #S(DATE :S 35 :M 4 :H 8 :DOW 3 :DAY 17 :MONTH 2 :YEAR 2011 :DST NIL :ZONE -1)

Creating a date from a "military time" (today) :
    > (date :miltime 2302)
    #S(DATE :S 0 :M 02 :H 23 :DOW 3 :DAY 17 :MONTH 2 :YEAR 2011 :DST NIL :ZONE -1)

Creating a date from a string :
    > (date :str "now")
    #S(DATE :S 35 :M 4 :H 8 :DOW 3 :DAY 17 :MONTH 2 :YEAR 2011 :DST NIL :ZONE -1)

Date structures can be compared with with d>, d<, d>=, d<=, d=, and d/=.

The time in seconds between two dates can be obtained with d-delta :
    > (d-delta date1 date2)
    10

The "military" time (hours\*100+minutes). I have added seconds/100 for precision :
    > (miltime (date :ut 3506915075))
    804.35

The week of the month :
    > (date-wom (date :ut 3506915075))
    3

The week of the year :
    > (date-week (date :ut 3506915075))

    > (y-m-d (date :ut 3506915075))

Get a Common-Lisp universal time (equivalent to get-universal-time)
    > (ut)
    3506915075

Get the Common-Lisp universal time corresponding to a string (uses GNU date)
    > (ut "now")
    3506915075
