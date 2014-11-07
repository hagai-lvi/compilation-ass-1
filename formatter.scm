(load "pc.scm")
;testing 123333;
;commit test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;            examples             ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         end of examples         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;      EXPERIEMNTS       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (^<wrap> <left-bracket> <right-bracket>))
(define y (x <nat>))

;Test for <white-space>
(<white-space> (string->list "   ")
	(lambda (x y)	;success
		`(match: ,x left: ,y))
	(lambda(x)		;fail
		'fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  string defenition  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
((^<wrap> (char #\{)(char #\}))
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
(new (*parser (word "{{"))
	(*parser <comment-string>) 
	(*parser (word "}}"))
	(*caten 3)
	(*parser (word "{{"))
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
	      (cadr (assoc (string->symbol e) l)))
	    (lambda (w) `(failed with report: ,@w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  Allignment related  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; identfies ----
(define <lines>
	(new 	(*parser (char #\-))
			*star
	done))

;;;test for <lines>
(<lines> (string->list "---") (lambda (x y) `(match: ,x left: ,y)) (lambda(x) 
'fail))



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
(<lines-nat-lines> (string->list "---5-") (lambda (x y) `(match: ,x left: ,y))
 (lambda(x) 'fail))

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
(<left-arrow> (string->list "~<--10--") (lambda (x y) `(match: ,x left: ,y)) (
lambda(x) 'fail))

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
(<right-arrow> (string->list "~--10-->") (lambda (x y) `(match: ,x left: ,y)) 
(lambda(x) 'fail))

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
(<middle-arrow> (string->list "~<--10-->") (lambda (x y) `(match: ,x left: ,y)
) (lambda(x) 'fail))


;;;;;;;;;;;; allignment with variables

;identifies ----var--- and returns var
(define <lines-var-lines>
	(new 	(*parser <lines>)
			(*parser <variable>)
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
(<left-arrow-var> (string->list "~<--{var}--") (lambda (x y) `(match: ,x left: ,y)) (
lambda(x) 'fail))

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
(<right-arrow-var> (string->list "~--{var}-->") (lambda (x y) `(match: ,x left: ,y)) 
(lambda(x) 'fail))

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
(<middle-arrow-var> (string->list "~<--{var}-->") (lambda (x y) `(match: ,x left: ,y)
) (lambda(x) 'fail))

















