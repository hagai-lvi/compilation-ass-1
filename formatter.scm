(load "pc.scm")


(define unicode-char-overflow-right (integer->char 9758))
(define unicode-char-overflow-left (integer->char 9756))
(define unicode-char-double-overflow (integer->char 9757))


(define formatter
	(lambda (format-string  . optional-list)
		(if (null? optional-list)
			(formatter-with-args format-string '() #\nul)
			(formatter-with-args format-string (car optional-list) #\nul))))


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

(define <my-parser>
	(new (*parser (char #\0))
		 (*pack (lambda (_) 0))
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


(define <left-bracket>
	(new (*parser (char #\{  ))
	done))

(define <right-bracket>
	(new (*parser (char #\}  ))
	done))


; consumes a single white-space
(define <white-space>
	(new	(*parser <any-char>)
			(*guard (lambda (c) (char-whitespace? c)))
			;(*pack (lambda (_) #\@ ))
	done))

; consumes many white-spaces
(define <white-spaces>
	(new	
			(*parser <white-space>)
			*star
	done))

; waits for a new parser, and consumes spaces from around that parser
; example:
; (<wrapped-in-spaces> <nat>) gets "   \t \n   123   \t   " and returns 123 
(define <wrapped-in-spaces>
	(^<wrap> <white-spaces> <white-spaces>))


; waits for a new parser, and curly brackets from around that parser
; example:
; (<wrapped-in-brackets> <nat>) gets "{123}" and returns 123 
(define <wrapped-in-brackets>
	(^<wrap> <left-bracket> <right-bracket>))

;That's a lousy name!
; isolates natural-numbers from { \t   123    \n}
; example:
; (test-string <nat-wrapped-in-spaces-and-brackets> "{   1   }") returns 1
(define <nat-wrapped-in-spaces-and-brackets>
	(<wrapped-in-brackets> (<wrapped-in-spaces>  <nat>)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  EXPERIEMNTS  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (^<wrap> <left-bracket> <right-bracket>))
(define y (x <nat>))

;Test for <white-space>
(<white-space> (string->list "   ")
	(lambda (x y)	;success
		`(match: ,x left: ,y))
	(lambda(x)		;fail
		'fail))

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

(define <comment-string>
(new (*parser <any-char>)
	 (*parser (word "}}"))
	 (*parser (word "{{"))
	 (*disj 2)
	 *diff
	 *star
	 (*pack (lambda(ch)(list->string ch)))
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


(define <sen>
(lambda(string-l l)
(<sym> (string->list string-l)
 (lambda (e s)
	      (cadr (assoc  e l)))
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
			*star
	done))

;;;test for <lines>
(<lines> (string->list "---") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))



;identifies ----n--- and returns n
(define <lines-nat-lines>
	(new 	(*parser <lines>)
			(*parser <nat>)
			(*parser <lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 n _2 ) n))
	done))

;;;test for <lines-nat-lines>
(<lines-nat-lines> (string->list "---5-") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))

; identifies ~<-----n---- and returns n
(define <left-arrow>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-nat-lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 _2 n ) n))
	done))
;;;test for <left-arrow>
(<left-arrow> (string->list "~<--10--") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))

; identifies ~-----n----> and returns n
(define <right-arrow>
	(new 	(*parser (char #\~))
			(*parser <lines-nat-lines>)
			(*parser (char #\>))
			(*caten 3)
			(*pack-with
				(lambda ( _1 n _3 ) n))
	done))

;test for <right-arrow>
(<right-arrow> (string->list "~--10-->") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))

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

;;;test for <middle-arrow>
(<middle-arrow> (string->list "~<--10-->") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))


;;;;;;;  allignment with variables  ;;;;;;;

;identifies ----var--- and returns var
(define <lines-var-lines>
	(new 	(*parser <lines>)
			(*parser <allignment-variable>)
			(*parser <lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 var _2 ) var))
	done))

;;;test for <lines-nat-lines>
(<lines-var-lines> (string->list "---{var}-") (lambda (x y) `(match: ,x left: ,y))
 (lambda(x) 'fail))

; identifies ~<-----{var}--- and returns var
(define <left-arrow-var>
	(new 	(*parser (char #\~))
			(*parser (char #\<))
			(*parser <lines-var-lines>)
			(*caten 3)
			(*pack-with
				(lambda ( _1 _2 var ) var))
	done))
;;;test for <left-arrow>
(<left-arrow-var> (string->list "~<--{var}--") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))

; identifies ~-----{var}----> and returns var
(define <right-arrow-var>
	(new 	(*parser (char #\~))
			(*parser <lines-var-lines>)
			(*parser (char #\>))
			(*caten 3)
			(*pack-with
				(lambda ( _1 var _3 ) var))
	done))

;test for <right-arrow>
(<right-arrow-var> (string->list "~--{var}-->") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))

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

;;;test for <middle-arrow>
(<middle-arrow-var> (string->list "~<--{var}-->") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 'fail))



(define <allignment>
	(new 	
			(*parser <right-arrow>)
			(*parser <middle-arrow>)
			(*parser <left-arrow>)
			(*disj 3)
			(*pack (lambda (n) `(num ,n)))

			(*parser <right-arrow-var>)
			(*parser <middle-arrow-var>)
			(*parser <left-arrow-var>)
			(*disj 3)
			(*pack (lambda (var) `(var ,var)))

			(*disj 2)
	done)
)

;test for <allignment>
(<allignment> (string->list "~<--10--") (lambda (x y) `(match: ,(cadr x) left: ,y)) (lambda(x) 'fail))











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
  		(*disj 3)	
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
       (*parser (^<meta-char> "\\\"" #\"))
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
       (*disj 18)
       done))

(define <string-char>
  (new (*parser <string-meta-char>)

       (*parser <any-char>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*parser (char #\~))
       (*disj 3)

       *diff
       (*disj 2)
       done))

(define <string>
    (new(*parser <string-char>) *star
       (*pack
	(lambda (chars)
	  (list->string chars)))

       done))


(define <content>
	(new (*parser <string>)
		 (*parser <variable>)
	*diff
		(*parser <comment>)
		*diff
	done))

(define formatter-no-args
	(lambda (format-string)
		`do_something))

(define formatter-with-args
	(lambda(format-string optional-list string-to-print)
		(<formatter> (string->list format-string)
	(lambda (e s)     
         (let* (
         	(matching (e optional-list ))
         	  (matching-list (if(number? matching)
         	  (string->list (number->string matching))
         	(string->list matching))))	
         (cond  
           ((and (null? s)(not(char? string-to-print))) (list->string `(,@string-to-print ,@ matching-list)))
	   ((and (null? s)(char? string-to-print))matching)  
           (else(if(char? string-to-print)	
                 (formatter-with-args (list->string s) optional-list  matching-list)
          (formatter-with-args (list->string s) optional-list `(,@string-to-print ,@ matching-list)))))))
	    
          (lambda (w) `(failed with report: ,@w)))))

(define <formatter>
	(new
	(*parser <content>)
	(*pack (lambda(ch) (lambda(l) ch)))
	(*parser <variable>)
	(*pack (lambda(ch)(lambda(l)(cadr (assoc ch l)))))
        (*parser <comment>)
        (*pack (lambda(ch)(lambda(l)"")))
	(*disj 3)
done))


(define (test-4)
  (let ((env
	'((betty "Betty")
	(bought "bought")
	(a "a")
	(bit "bit")
	(of "of")
	(butter "butter")
	(but "but")
	(the "the")
	(was "was")
	(too "too")
	(bitter "bitter")
	(so "so")
	(she "she")
	(brought "brought")
	(back "back"))))
    (display
     (formatter "
~{betty} ~{bought} ~{a} ~{bit} ~{of} ~{butter},
~{but} ~{the} ~{butter} ~{was} ~{too} ~{bitter},
~{so} ~{she} ~{brought} ~{the} ~{bitter} ~{butter} ~{back}.
" env))))

(define (test-5)
  (let ((env
	'((betty "Betty")
	(bought "bought")
	(a "a")
	(bit "bit")
	(of "of")
	(butter "butter")
	(but "but")
	(the "the")
	(was "was")
	(too "too")
	(bitter "bitter")
	(so "so")
	(she "she")
	(brought "brought")
	(back "back"))))
    (display
     (formatter "
~---10--->{betty} ~{bought} ~{a} ~{bit} ~{of} ~{butter},
~---10--->{but} ~{the} ~{butter} ~{was} ~{too} ~{bitter},
~---10--->{so} ~{she} ~{brought} ~{the} ~{bitter} ~{butter} ~{back}.
" env))))
	       
(define (test-6)
  (let ((env
	 '((left-arrow-head "<")
	   (right-arrow-head ">")
	   (empty-head "")
	   (great "Great!")
	   (width 10))))
    (display
     (formatter
      (formatter "
[~~~{empty-head}---~{width}---~{right-arrow-head}{great}]\\\\{newline}
[~~~{left-arrow-head}---~{width}---~{right-arrow-head}{great}]\\\\{newline}
[~~~{left-arrow-head}---~{width}---~{empty-head}{great}]\\\\{newline}
" env)
      env))))
