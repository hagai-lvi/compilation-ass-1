;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils
(load "formatter.scm")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite foo

	(define-test test-<lines>-1
		(assert-equal? (<lines> (string->list "---") (lambda (x y) (list->string x)) (lambda(x) 'fail)) "---")
		(assert-equal? (<lines> (string->list "---") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<lines-nat-lines>-1
		(assert-equal? (<lines-nat-lines> (string->list "---5-") (lambda (x y)  x) (lambda(x) 'fail)) 5)
		(assert-equal? (<lines-nat-lines> (string->list "---5-") (lambda (x y)  (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<left-arrow>-1
		(assert-equal? (<left-arrow> (string->list "~<--10--") (lambda (x y) x) (lambda(x) 'fail)) 10)
		(assert-equal? (<left-arrow> (string->list "~<--10--") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<right-arrow>-1
		(assert-equal? (<right-arrow> (string->list "~--10-->") (lambda (x y) x) (lambda(x) 'fail)) 10)
		(assert-equal? (<right-arrow> (string->list "~--10-->") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<middle-arrow>-1
		(assert-equal? (<middle-arrow> (string->list "~<--10-->") (lambda (x y) x) (lambda(x) 'fail)) 10)
		(assert-equal? (<middle-arrow> (string->list "~<--10-->") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<lines-var-lines>-1
		(assert-equal? (<lines-var-lines> (string->list "---{var}-") (lambda (x y) (symbol->string x)) (lambda(x) 'fail)) "var")
		(assert-equal? (<lines-var-lines> (string->list "---{var}-") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)
	(define-test test-<left-arrow-var>-1
		(assert-equal? (<left-arrow-var> (string->list "~<--{var}--") (lambda (x y) (symbol->string x)) (lambda(x) 'fail)) "var")
		(assert-equal? (<left-arrow-var> (string->list "~<--{var}--") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<right-arrow-var>-1
		(assert-equal? (<right-arrow-var> (string->list "~--{var}-->") (lambda (x y) (symbol->string x)) (lambda(x) 'fail)) "var")
		(assert-equal? (<right-arrow-var> (string->list "~--{var}-->") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<middle-arrow-var>-1
		(assert-equal? (<middle-arrow-var> (string->list "~<--{var}-->") (lambda (x y) (symbol->string x)) (lambda(x) 'fail)) "var")
		(assert-equal? (<middle-arrow-var> (string->list "~<--{var}-->") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<allignment>-num-1
		(assert-equal? (<allignment> (string->list "~<--10--") (lambda (x y) x) (lambda(x) 'fail)) `(num-allign left 10))
		(assert-equal? (<allignment> (string->list "~<--10--") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<allignment>-var-1
		(assert-equal? (<allignment> (string->list "~<--{var1}--") (lambda (x y) x) (lambda(x) 'fail)) `(var-allign left var1))
		(assert-equal? (<allignment> (string->list "~<--{var1}--") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test formatter-test-1
	(assert-equal? (formatter "~~abc") "~abc"))

	(define-test formatter-test-2 
	(let ((env `((var1 "abc"))))
		(assert-equal? (formatter "~~abc" env) "~abc"))
	)

	(define-test formatter-test-3 
	(let ((env `(
	(a "a"))))
		(assert-equal? (formatter "~{a}" env) "a")))

	(define-test formatter-test-4
	(let ((env `(
	(a "a")
	(ab "ab"))))
		(assert-equal? (formatter "~{a} ~{ab}" env) "a ab")))
	
	(define-test formatter-test-5
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? 	(formatter "~<--5--{ab}" env) 	"ab   ")))

	(define-test formatter-test-6
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--{five}--{ab}" env) "ab   ")))

	(define-test formatter-test-7
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~--5-->{ab}" env) "   ab")))

	(define-test formatter-test-8
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~--{five}-->{ab}" env) "   ab")))

	(define-test formatter-test-9
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--5-->{ab}" env) " ab  ")))

	(define-test formatter-test-10
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--{five}-->{ab}" env) " ab  ")))

	(define-test formatter-test-11
	(let ((env `((var1 "abc"))))
		(assert-equal? (formatter "~~~{var1}" env) "~abc")))

	(define-test formatter-test-12
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
		(assert-equal? (formatter "
~{betty} ~{bought} ~{a} ~{bit} ~{of} ~{butter},
~{but} ~{the} ~{butter} ~{was} ~{too} ~{bitter},
~{so} ~{she} ~{brought} ~{the} ~{bitter} ~{butter} ~{back}." env) 
"
Betty bought a bit of butter,
but the butter was too bitter,
so she brought the bitter butter back."
				 )))

	(define-test formatter-test-13
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
	(assert-equal? (formatter "
~---10--->{betty} ~{bought} ~{a} ~{bit} ~{of} ~{butter},
~---10--->{but} ~{the} ~{butter} ~{was} ~{too} ~{bitter},
~---10--->{so} ~{she} ~{brought} ~{the} ~{bitter} ~{butter} ~{back}." env) "
     Betty bought a bit of butter,
       but the butter was too bitter,
        so she brought the bitter butter back.")))

	(define-test formatter-test-14
	(let ((env `(
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc}" env) " ab   abc")))

	(define-test formatter-test-15
	(let ((env `(
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--3-->{abcd}" env) " ab   abc ☜b☞")))


	(define-test formatter-test-16
	(let ((env `(
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--3-->{abcd} <---5-->~{{no tilde here}}" env) " ab   abc ☜b☞ <---5-->")))

	(define-test formatter-test-17
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--2-->{abcd} <---5-->~{{no tilde here}}" env) " ab   abc ☜☞ <---5-->")))

	(define-test formatter-test-18
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--1-->{abcd} <---5-->~{{no tilde here}}" env) " ab   abc ☝ <---5-->")))

	(define-test formatter-test-19
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--1-->{abcd} <---5-->~{{no tilde here}} ~{
			ab}" env) " ab   abc ☝ <---5--> ab")))

	(define-test formatter-test-20
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd"))))
		(assert-equal? (formatter "~<--{five}-->{ab} ~<--{three}--{abc} ~<--1-->{abcd} <---5-->~{{no tilde here}} ~{
			ab 		
			 }" env) " ab   abc ☝ <---5--> ab")))

	(define-test formatter-test-21
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd")
	(tilde "~")
	(left-arrow "<")
	(right-arrow ">"))))
		(assert-equal? (formatter "~~~{left-arrow}--{five}--{ab}" env) "~<--{five}--{ab}")))

	(define-test formatter-test-22
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd")
	(tilde "~")
	(left-arrow "<")
	(right-arrow ">"))))
		(assert-equal? (formatter "~<--{five}--{ab}" env) "ab   ")))

	(define-test formatter-test-23
	(let ((env `(
	(two 2)
	(three 3)
	(four 4)
	(five 5)
	(ab "ab")
	(abc "abc")
	(abcd "abcd")
	(tilde "~")
	(left-arrow "<")
	(right-arrow ">"))))
		(assert-equal? (formatter (formatter "~~~{left-arrow}--{five}--{ab}" env) env) "ab   ")))
)

;(run-test-suites foo)
;(run-test foo first-test)
;(run-tests foo test-one)


(exit (run-test-suites foo))





















