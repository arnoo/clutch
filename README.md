This is the utility library I wrote as I was learning Common Lisp. I still use it all the time, and it evolves as I require new utilities. It still contains code that is not great, but works well enough that I have not taken the time to rewrite it.

I have released it under the GPL, in the hope that it can be useful to somebody, and that I might get suggestions for additions or improvements.

Most functions should work pretty well on SBCL and ABCL, which are the two implementations I use regularly. There is code specific to other implementations in some places, usually because it comes from other people, but I have never tested any of it.

There is a test suite (tests.lisp), based on my test library (cleanser), that should cover most of the code.

Some date functions rely on the Linux 'date' program. While a bit ugly, this works well enough for my purposes right now. I don't know how that would translate outside of Linux though.




A little primer :

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
> {some-struct 'a}
<value of slot a>

This was inspired by Arc's possibility of using a functional position for data structures. In that spirit, {} can also be used to apply functions and lambdas :
> {(lambda (x) (+ 1 x)) 2}
3

[] is used for anonymous functions and is supposed to work like in Arc :
> (mapcar [+ 1 _] (list 1 2))
(list 2 3)

It can also be used with structures in a functional position like this :
> (mapcar [_ 2] (list "abc" "def"))
(list #\c #\f)


Memoization functions :

> (memoize fn)
will return a memoized version of <fn>. 

> (memoize-to-disk fn)
does the same, but stores the cache on disk, which can be useful if the results are large or if they need to be kept between sessions.

Both memoization functions accept the keyword arguments <remember-last>, which indicates the number of results to remember, and <expire>, which controls how many seconds results are remembered for. memoize-to-disk has other parameters to customize the storage of results.


"gulp" and associates :

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

gulplines does the same, but returns a list of strings, each string a line in the file. It also accepts offset and limit parameters that work the same way, but count in lines and not characters.
> (gulplines "/var/log/messages" :offset -20 :limit 10)
Will return a list of 10 lines starting 20 lines from the end of the file.

If I'm going to work on a large file, I probably don't want to load it all in memory, so I also have the option of working on it line by line :
> (with-each-line ("/var/log/messages") (print it))

I can use the same <offset> and <limit> parameters there :
> (with-each-line ("/var/log/messages" :offset -20 :limit 10) (print it))

All these functions accept a stream instead of a filename, or a URL, which is then fetched using Drakma :
> (with-each-line ("http://www.google.com") (print it))
> (gulp "http://www.google.com")


pushend and popend act like push and pop but with the end of a list and not the beginning :
> (pushend
> (popend


Anaphoric macros inspired by Onlisp :
> awith (form &rest body)
> aif (test then &optional else)
> awhen (test &rest body)
> aunless (test &rest body)
> awhile (test &rest body)
> while (test &rest body)
> aand (test &rest tests)
> acond (&rest forms)

