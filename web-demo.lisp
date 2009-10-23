(defun fiche (id)
	(let ((fiche {fiches id}))
		(if fiche
			<form id="fiche">
				<label for="nom">Nom :</label><input type="text" id="nom" value="{fiche nom}"/>
			</form>)
			))

(defun liste ()
	<div id="liste">
	(loop for fiche in #/fiches collect
		<a href="/{fiche id}">{fiche nom}</a>)
	</div>)

(defvar *css*
	(css
		form#fiche
			{
			border: 1px solid #000000;
			}
		))

(html (fiche id) :css *css* :title {{#/fiches id} nom})




(defun html (code &key title)
	(let ((hasbody    nil)
	      (hashead    nil)
	      (hashtml    nil)
	      (hasdoctype nil))
	(html-parse:parse-html code :callback-only t
				    :callbacks (list (cons :BODY    [setf hasbody    t])
						     (cons :HEAD    [setf hashead    t])
						     (cons :HTML    [setf hashtml    t])
						     (cons :DOCTYPE [setf hasdoctype t])))
	(case (and 
	(error "HTML : No body found but head found")
))

(defun clean-css (css &jey nosprite)
	(loop for group in (split "}" (~s "/(;|}|\w)(1\n+\s+)+/$1/g" css))
	      with parts = (split "{" group)
	      with lines = (split ";" {parts 1})
	      collect (if nosprite
			  lines
			  (cons {parts 0}
				(loop for line in lines
			              with lineparts = (split ":" line)
				      (aif (~ "/url\('([^']*)'\)/" {lineparts 1})
                                            
))
))

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
		(print (validate html))
		`(str ,html)))
