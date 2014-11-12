(load "pc.scm")


(define unicode-char-overflow-right (integer->char 9758))
(define unicode-char-overflow-left (integer->char 9756))
(define unicode-char-double-overflow (integer->char 9757))


(define formatter
	(lambda (format-string  . optional-list)
		(if (null? optional-list)
			(formatter-with-args format-string '() #\nul)
			(formatter-with-args format-string (car optional-list) #\nul))))

(define get-assoc (lambda(var-name var-map )
	(let ((x (assoc var-name var-map)))
		(if (pair? x)
		    (cadr x)
		    (error `get-assoc (format "couldn't find association for variable name ~a" (symbol->string var-name))))
		)))

;;;;;;;;;;;;;;;;;;;;;;
;;;;;  examples  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <nat>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))

       (*disj 2)
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  end of examples  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define ^<wrap>
 	(lambda (<left> <right>)
 		(lambda (<p>)
 			(new 	(*parser <left>)
 					(*parser <p>)
 					(*parser <right>)
 					(*caten 3)
 					(*pack-with
 					(lambda ( < e >) e))
 					done  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  string defenition  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <symbol>
  (new (*parser (range-ci #\a #\z))
       (*parser (range-ci #\0 #\9))
       (*parser (one-of "!$^*-_=+<>?/"))
       (*disj 3) *plus
       (*pack
	(lambda (s)
	  (string->symbol
	   (list->string s))))
       (*parser <nat>)
       *diff
       done))


(define <white>
(const (lambda(ch)
	(char-whitespace? ch))))


(define <string-new>
(new (*parser <any-char>)
	(*parser <white>)
	(*parser (char #\}))
	(*disj 2)
	*diff	
	*star
	
	(*pack (lambda(ch)(list->string ch)))
	
	done))

;identifies {   variable   }
(define <variable>
((^<wrap> (word "~{")(char #\}))
((^<wrap> (star <white>)(star <white>))
<symbol>)))
;test (<sen> "{day-of-week}" `((day-of-week "Friday")(day-of month "never")))



(define <sen>
(lambda(string-l l)
(<sym> (string->list string-l)
 (lambda (e s)
	      (get-assoc e l))
	    (lambda (w) `(failed with report: ,@w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  Allignment related  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;recognize {var} (without the ~)
(define <allignment-variable>
((^<wrap> (char #\{)(char #\}))
((^<wrap> (star <white>)(star <white>))
<symbol>)))

; identfies ----
(define <lines>
	(new 	(*parser (char #\-))
			*plus
	done))

;identifies ----n--- and returns n
(define <lines-nat-lines>
	(new 	(*parser <lines>)
			(*parser <nat>)
			(*parser <lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 n _2 ) n))
	done))

; identifies ~<-----n---- and returns n
(define <left-arrow>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-nat-lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 _2 n ) n))
	done))

; identifies ~-----n----> and returns n
(define <right-arrow>
	(new 	(*parser (char #\~))
			(*parser <lines-nat-lines>)
			(*parser (char #\>))
			(*caten 3)
			(*pack-with
				(lambda ( _1 n _3 ) n))
	done))

; identifies ~<-----n----> and returns n
(define <middle-arrow>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-nat-lines>)
			(*parser (char #\>))
			(*caten 4)
			(*pack-with
				(lambda ( _1  _2 n _4 ) n))
	done))

;;;;;;;  allignment with variables  ;;;;;;;

;identifies ----{var}--- and returns var
(define <lines-var-lines>
	(new 	(*parser <lines>)
			(*parser <allignment-variable>)
			(*parser <lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 var _2 ) var))
	done))

; identifies ~<-----{var}--- and returns var
(define <left-arrow-var>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-var-lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 _2 var ) var))
	done))

; identifies ~-----{var}----> and returns var
(define <right-arrow-var>
	(new 	(*parser (char #\~))
			(*parser <lines-var-lines>)
			(*parser (char #\>))
			(*caten 3)
			(*pack-with
				(lambda ( _1 var _3 ) var))
	done))

; identifies ~<-----{var}----> and returns var
(define <middle-arrow-var>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-var-lines>)
			(*parser (char #\>))
			(*caten 4)
			(*pack-with
				(lambda ( _1  _2 var _4 ) var))
	done))

;recognize any arrow with var ot num and returns `(var ,var) or `(num ,n)
;example: "~<--{var1}-->" returns (var middle var1)
;"~<--1-->" returns (num middle 1)
(define <allignment>
	(new 	
			(*parser <right-arrow>)
			(*pack (lambda (n) `(right ,n)))
			(*parser <middle-arrow>)
			(*pack (lambda (n) `(middle ,n)))
			(*parser <left-arrow>)
			(*pack (lambda (n) `(left ,n)))
			(*disj 3)
			(*pack (lambda (list) `(num-allign ,@list)))

			(*parser <right-arrow-var>)
			(*pack (lambda (var) `(right ,var)))
			(*parser <middle-arrow-var>)
			(*pack (lambda (var) `(middle ,var)))
			(*parser <left-arrow-var>)
			(*pack (lambda (var) `(left ,var)))
			(*disj 3)
			(*pack (lambda (list) `(var-allign ,@list)))

			(*disj 2)
	done)
)

(define num-allign?
	(lambda (x)
		(eq? `num-allign (car x))
	))

(define get-allign-num
	(lambda (num-allign)
		(caddr num-allign)
	))

(define get-allign-var
	(lambda (num-allign)
		(caddr num-allign)
	))

(define get-allign-direction
	(lambda (allign)
		(cadr allign)
	))

(define get-dir-format-string
	(lambda (direction length)
		(let ((format-str 
				(cond 	((eq? `middle direction) 123)
				       	((eq? `left direction) "s")
				   		((eq? `right direction) "@s"))))
		(list->string `(#\~ ,@(string->list (number->string length)) ,@(string->list format-str)) )
		)))


; TODO: need to fix the calculation of start and end indexes of the substring
(define cut-allign-middle (lambda (str output-length)
	(let* ( 	(str-length (string-length str))
				(cut-total (+ (- str-length output-length) 2))
				(cut-left (floor (/ cut-total 2)))
				(cut-right (- cut-total cut-left))
				(left-index cut-left)
				(right-index (- str-length cut-right)))
	;(display `(,str-length ,#\space  ,cut-total ,#\space  ,cut-left ,#\space  ,cut-right ,#\space ,left-index ,#\space ,right-index ,#\space  ))
	;(display (substring str left-index right-index) )

	(if (= 1 output-length)
	    (list->string `(,unicode-char-double-overflow))
	    (list->string `(,unicode-char-overflow-left ,@(string->list (substring str left-index right-index))  ,unicode-char-overflow-right)))
)))

(define cut-allign-left (lambda (str output-length)
	(list->string `( ,@(split-list (string->list str) (- output-length 1) (lambda(left right) left))  ,unicode-char-overflow-right))
))

(define cut-allign-right (lambda (str output-length)
	(list->string `(,unicode-char-overflow-left ,@(split-list (string->list str) (- (string-length str)(- output-length 1)) (lambda(left right) right)) ))
))

;example: (cut-allign-string "012345678" 'right 3) returns "☜78"
; PRE: (< (string-length str) output-length)
(define cut-allign-string (lambda (str direction output-length)
	(cond 	((eq? `middle direction) (cut-allign-middle str output-length))
			((eq? `left direction) (cut-allign-left str output-length))
			((eq? `right direction) (cut-allign-right str output-length)))
))

(define add-spaces-right
	(lambda (str n) (if (= n 0)
    	str
    	(add-spaces-right (string-append str " ") (- n 1) ))   ))

(define add-spaces-left
	(lambda (str n) (if (= n 0)
    	str
    	(add-spaces-left (string-append " " str) (- n 1) ))   ))

(define pad-allign-middle (lambda (str output-length)
	(let*  ((total-pad (- output-length (string-length str)))
			(pad-left (floor (/ total-pad 2)))
			(pad-right (- total-pad pad-left)))
	(add-spaces-left (add-spaces-right str pad-right) pad-left))
))

(define pad-allign-left (lambda (str output-length)
	(letrec ((add-spaces (lambda (str n)
								(if (= n 0)
								    str
								    (add-spaces (list->string `( ,@(string->list str) #\space )) (- n 1) )))))
	(add-spaces str (- output-length (string-length str)))
	)))

(define pad-allign-right (lambda (str output-length)
	(letrec ((add-spaces (lambda (str n)
								(if (= n 0)
								    str
								    (add-spaces (list->string `( #\space ,@(string->list str))) (- n 1) )))))
	(add-spaces str (- output-length (string-length str)))
	)))

(define pad-string (lambda (str direction output-length)
	(cond 	((eq? `middle direction) (pad-allign-middle str output-length))
			((eq? `left direction) (pad-allign-left str output-length))
			((eq? `right direction) (pad-allign-right str output-length)))
))

;checks if cut or pad is needed
(define allign-string
	(lambda (str direction output-length)
		(let ((str-length (string-length str)))
			(if (> str-length output-length)
			    (cut-allign-string str direction output-length)
			    (pad-string str direction output-length))
		)))


;recognize ~<--1-->{var1} or ~<--{var0}-->{var1}
;example: "~<--1-->{var1}"" returns ((num middle 1) var1)
;returns procedure that waits for variables-list and returns the formatted string
(define <allignment-and-var>
	(new 	(*parser <allignment>)
			(*parser <allignment-variable>)
			(*caten 2)
			(*pack-with (lambda (allign var)
							(lambda (var-map)
								(let( 	(to-print (get-assoc  var var-map))
										(print-length
											(if (num-allign? allign)
										    (get-allign-num allign)
										    (get-assoc (get-allign-var allign) var-map)))
										(direction (get-allign-direction allign)))
								(allign-string to-print direction print-length)
								))))
	done))

;test for <allignment-and-var>
(<allignment-and-var> (string->list "~<--{var1}-->{var2}") (lambda (x y) (x  `((var1 2)(var2 "abc") )  )) (lambda(x) 'fail))

;test for <allignment-and-var> 
(<allignment-and-var> (string->list "~<--1-->{var1}") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;2.2.7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <hex-digit>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))

(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* 256 h))))
       done))

(define <hex-char>
  (new (*parser (word-ci "\\x"))
  		(*parser (word-ci "\\u"))
  		(*parser (word-ci "\\16#"))
  		(*parser (word-ci "\\h"))
  		(*disj 4)	
       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*caten 2)
       (*pack-with (lambda (_< ch ) ch))
       done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <string-meta-char>
  (new 
       (*parser <hex-char>)
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\{newline}" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\{return}" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{page}" #\page)) ; formfeed
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\{tab}" #\tab))
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))
       (*parser (^<meta-char> "\\{" #\{))
       (*parser (^<meta-char> "\\}" #\}))
       (*parser (^<meta-char> "~~" #\~))
       (*parser (^<meta-char> "\\\\" #\\))
       (*disj 18)
       done))

(define <string-char>
  (new (*parser <string-meta-char>)

       (*parser <any-char>)

       (*parser (char #\{))
       (*parser (char #\~))
       (*parser (char #\}))
       (*disj 3)

       *diff
       (*disj 2)
       done))

(define <string>
    (new(*parser <string-char>) *plus
       (*pack
	(lambda (chars)
	  (list->string chars)))

       done))


(define formatter-with-args
	(lambda  (format-string optional-list string-to-print)
		(<formatter> (string->list format-string)
	(lambda  (e s)     
         (let* (
         	(matching (e optional-list ))
         	  (matching-list 
         	  	(if (not matching) matching 
         	  		(if(number? matching)
         	  			(string->list (number->string matching))
         				(string->list matching)))))	
         (if (not matching-list) `(couldnt find variable) 
         (cond  
           
           ((and (null? s)(not(char? string-to-print))) (list->string `(,@string-to-print ,@ matching-list)))
	   ((and (null? s)(char? string-to-print))matching)  
           (else(if (char? string-to-print)	
				(formatter-with-args (list->string s) optional-list  matching-list)
				(formatter-with-args (list->string s) optional-list `(,@string-to-print ,@ matching-list))))))))
	    
          (lambda (w) `(failed with report: ,@w)))))



(define <comment-string>
(new 
	 (*parser <string-char>)
	 (*parser (word "}}"))
	 (*parser (word "~{{"))
	
	 (*disj 2)
	 *diff	 
	 (*parser <variable>)
	 (*disj 2)
	*star
	 (*pack (lambda(ch)ch))
	done))


(define <comment>
(new (*parser (word "~{{"))
	(*parser <comment-string>) 
	(*parser (word "}}"))
	(*caten 3)
	(*parser (word "~{{"))
	(*parser <comment-string>)
	(*delayed (lambda() <comment>))
	(*parser (word "}}"))
	(*caten 4)
	(*disj 2)
done))


(define <content>
	(new (*parser <string>)
		 (*parser <variable>)
	*diff
		(*parser <comment>)
		*diff
		(*parser <allignment>)
		*diff
		(*parser <allignment-variable>)
		*diff
	done))

(define <error>
(new   
       (*parser (char #\{))
       (*parser (char #\~))
       (*parser (char #\}))
       (*disj 3)
	done))

(define <formatter>
	(new
	(*parser <content>)
	(*pack (lambda(ch) (lambda(l) ch)))
	(*parser <variable>)
	(*pack (lambda(ch)(lambda(l)(let ((value (assoc ch l)))
		(if value
			(cadr value)value)))))
    (*parser <comment>)
    (*pack (lambda(ch)(lambda(l)"")))
    (*parser <allignment-and-var>)
    (*pack (lambda(token)(lambda(var-map)(token var-map))))
    (*parser <error>)
    (*pack (lambda(ch)(lambda(_)(error `illegal-char (format "illegal-char was found ~a" ch)))))
	(*disj 5)
done))
