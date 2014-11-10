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

	(define-test mytest-12
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

	(define-test mytest-13
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
	
	(define-test fail
		(assert-equal? `a `(var-allign left var1))
		
	)
)

(run-test-suite foo)
;(run-test foo first-test)
;(run-tests foo test-one)


(exit)

(define (test-6)
  (let ((env
	 '((left-arrow-head "<")
	   (right-arrow-head ">")
	   (empty-head "")
	   (great "Great!")
	   (width 10))))
    (display
      (formatter "~---10--->{great}]\\" env))))






















