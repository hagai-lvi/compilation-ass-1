(load "pc.scm")

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
















