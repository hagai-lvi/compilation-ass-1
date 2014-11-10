;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils
(load "formatter.scm")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite foo

	(define-test test-one
	(assert-equal? (formatter "~~abc") "~abc"))

	(define-test mytest-2 
	(let ((env `((var1 "abc"))))
		(assert-equal? (formatter "~~abc" env) "~abc"))
	)

	(define-test mytest-3 
	(let ((env `(
	(a "a"))))
		(assert-equal? (formatter "~{a}" env) "a")))

	(define-test mytest-4
	(let ((env `(
	(a "a")
	(ab "ab"))))
		(assert-equal? (formatter "~{a} ~{ab}" env) "a ab")))
	
	(define-test mytest-5
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? 	(formatter "~<--5--{ab}" env) 	"ab   ")))

	(define-test mytest-6
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--{five}--{ab}" env) "ab   ")))

	(define-test mytest-7
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~--5-->{ab}" env) "   ab")))

	(define-test mytest-8
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~--{five}-->{ab}" env) "   ab")))

	(define-test mytest-9
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--5-->{ab}" env) "  ab ")))

	(define-test mytest-10
	(let ((env `(
	(five 5)
	(ab "ab"))))
		(assert-equal? (formatter "~<--{five}-->{ab}" env) "  ab ")))

	(define-test mytest-11
	(let ((env `((var1 "abc"))))
		(assert-equal? (formatter "~~~{var1}" env) "~abc")))

)

(run-test-suite foo)

;(run-test foo first-test)

;(run-tests foo test-one)

