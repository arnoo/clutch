Clutch
======

This is the utility library I wrote as I was learning Common Lisp. I still use it all the time, and it evolves as I require new utilities. It contains code that is not great, but works well enough that I have not taken the time to rewrite it.

I have released it under the GPL, in the hope that it can be useful to somebody, and that I might get suggestions for additions or improvements.

Most functions should work pretty well on SBCL and ABCL, which are the two implementations I use regularly. There is code specific to other implementations in some places, usually because it comes from other people, but I have never tested any of it.

There is a test suite (tests.lisp), based on my test library (cleanser), that should cover most of the code.

Some date functions rely on the GNU 'date' program. While a bit ugly and non-portable, this works well enough for my purposes right now, and would be too complex to rewrite.



There are two main reader macros , {} and [].

The first is used to access data structures :
> {"abc" 1} 
#\b
> {"abc" 1 2}
"bc"
> {"abc" 1 -1}
"bc"
> {(list 1 2 3) 1 -1}
(list 2 3)
> {some-struct-or-object 'a}
<value of slot a>

This was inspired by Arc's possibility of using a functional position for data structures. In that spirit, {} can also be used to apply functions and lambdas :
> {(lambda (x) (+ 1 x)) 2}
3

[] is used for anonymous functions and is supposed to work like in Arc :
> (mapcar [+ 1 _] (list 1 2))
(list 2 3)

It can also be used with structures in a functional position like this (and then uses the same accessor function as {})
> (mapcar [_ 2] (list "abc" "def"))
(list #\c #\f)


Memoization
-----------

> (memoize fn)
will return a memoized version of <fn>. 

> (memoize-to-disk fn)
does the same, but stores the cache on disk, which can be useful if the results are large or if they need to be kept between sessions.

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

If I'm going to work on a large file, I probably don't want to load it all in memory, so I also have the option of working on it line by line :
> (with-each-line ("/var/log/messages") (print it))

I can use the same <offset> and <limit> parameters there :
> (with-each-line ("/var/log/messages" :offset -20 :limit 10) (print it))

All these functions accept a stream instead of a filename, or a URL, which is then fetched using Drakma :
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

inspired by Onlisp

> awith (form &rest body)
Evaluates <body> with it bound to <form>.



> aif (test then &optional else)
> awhen (test &rest body)
> awhile (test &rest body)
> while (test &rest body)
> aand (test &rest tests)
> acond (&rest forms)


Regular expressions
-------------------

CL-PPCRE is a great library, but I was missing the simplicity of the Perl syntax.
Unfortunately, backspaces have to be escaped. Maybe a clever reader macro could be nicer than regexps as strings.

> (~ "/\\w{3}/" (list "abc" "def" "ac"))
(list "abc" "def")

> (~ "/\\w{3}/" "ac")
NIL

> (~ "/\\w{3}/" "abc")
("abc")

> (~ "/(\\w{2})\\w/" "abc")
("abc" "ab")

If i am only interested in the first capture group :
> (~ "/(\\w{2})\\w/" "abc" 1)
"ab"

> (grep "/



Function composition
--------------------

Function o composes other functions :
(o #'sqrt #'sin) is a function that applies sin then sqrt

Arc like function composition : sqrt!sin transforming into (o #'sqrt #'sin)
would be nice, but as reader macros can't read backwards, I'm not sure this is possible.

Another thing I often find myself needing is "mapping" function composition : i.e.
(= (car x) (car y))
It would be nice to have a generic syntax for that like (=^car x y)
I'm not sure this can be done either. For lack of a better solutions, I have included comparison functions that take another function to be mapped to the arguments before comparison :

(f= #'car x y) is equivalent to (= (car x) (car y))

f>, f<, f<=, f>= and f-equal work the same way


Before and After
----------------

Functions before and after attach code to be executed respectively before and after a function is called :

> (defun a (x) (+ x 2))
> (before 'a (format t \"a to be called with args ~A\" args))"
> (after 'a (format t \"a was called with args ~A\" args))"
