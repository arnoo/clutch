;(defun fiche (id)
;	(let ((fiche {fiches id}))
;		(if fiche
;			<form id="fiche">
;				<label for="nom">Nom :</label><input type="text" id="nom" value="{fiche nom}"/>
;			</form>)
;			))
;
;(defun liste ()
;	<div id="liste">
;	(loop for fiche in #/fiches collect
;		<a href="/{fiche id}">{fiche nom}</a>)
;	</div>)
;
;(defvar *css*
;	(css
;		form#fiche
;			{
;			border: 1px solid #000000;
;			}
;		))
;
;(html (fiche id) :css *css* :title {{#/fiches id} nom})


(defun html (code &key (title "") (css ""))
  (setf code (~s "/^\s+//" code))
  (let ((start (lc {code 0 9})))
    (when (string/= start "<!doctype")
      (if (string/= {start 0 5} "<html")
        (if (string/= {start 0 5} "<head")
          (if (string/= {start 0 5} "<body")
            (setf code (str "<body>" code "</body>"))
    	    (setf code (str "<head><title>" title "</title></head><body>" code "</body>")))
    	  (setf code (str "<head><title>" title "</title></head>" code)))
        (setf code (str "<html>" code "</html>")))
      (setf code (str "<!DOCTYPE XHTML...>" code))))
  code)

;(defun clean-css (css &key nosprite)
;	(loop for group in (split "}" (~s "/(;|}|\w)(1\n+\s+)+/$1/g" css))
;	      with parts = (split "{" group)
;	      with lines = (split ";" {parts 1})
;	      collect (if nosprite
;			  lines
;			  (cons {parts 0}
;				(loop for line in lines
;			              with lineparts = (split ":" line)
;				      (aif (~ "/url\('([^']*)'\)/" {lineparts 1})
;                                            
;))
;))
;
;(defun compress-js (js)
;)

(defun validate (html &key (acessibility 0))
	(system (str "tidy -eq -access " accessibility)))


(defun xml-reader (stream char)
	(if (= (peek-char nil stream) #\Space) (unread-char char stream))
	(let* ((tagstart (loop with c = (read-char stream)
			until (or (= c #\Space) (= c #\>)) 
			collect c))
		(tagname (~s "/(\s|\/>|>)$//" tagstart))
		(self-closing (in (list "img" "br" "input" "!--") tagname))
		(fulltag (if (string= {tagstart 0 -1} ">")
				tagstart
				(str tag (loop with c = (read-char stream)
					until (= c #\>)
					collect c))))
		(html (if (or (string= {fulltag -2 1} "/") self-closing)
			  fulltag
			  (loop with read=""
				with count=-1 then (+ count (if (~ (str "/<\/" tagname "$/") read) 1 -1))
				until (= count 0)
				collect (read-char stream) into read))))
		(print (validate (~s "/<!--\ TMPL.*?-->//g" html)))
		`(str ,html)))

(defmacro start-server (&key port dispatchers)
  `(progn
     (setf hunchentoot:*dispatch-table* (list
       ,(loop for i in below (length dispatchers)
              collect `(let* (($$ (~ (str "/" (~s "/\\//\\\\\//" {dispatchers i}) "/") ))
			      ($0 (if $$ {$$ 0}))
			      ($1 (if (> (length $$) 1) {$$ 1} nil))
			      ($2 (if (> (length $$) 2) {$$ 2} nil))) 
		         (hunchentoot:create-regexp-dispatcher
		           ,{dispatchers i}
		  	   (lambda () ,{dispatchers (+ i 1)}))))))
	`(start-server)))
;
;(start-server
;  :dispatchers ("/images/"		(folder "/images/" :gzip t)
;	        "/(\d+)"		(html (fiche $1)))
;  :port 80)
;

