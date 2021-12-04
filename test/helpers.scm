;; Copyright (c) 2014-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(set-current-module (resolve-module '(test tap)))

(with-test-bundle (test tap helpers)
  (plan 11)
  (define-test "Is *plan* okay?"
    (pass-if-true (or (boolean? (plan))
                      (integer? (plan)))))
  (define-test "Default pp-width okay?"
    (pass-if-= (test-pp-width) 60))
  (let ((thing (make-settable 23)))
    (define-test "settable thing initialised to 23"
      (pass-if-= 23 (thing)))
    (define-test "settable thing return old state when modified"
      (pass-if-= 23 (thing 'foobar)))
    (define-test "settable thing set to symbol foobar"
      (pass-if-eq? 'foobar (thing))))
  (define-test "settable thing's initial state needs to adhere to its predicate"
    (pass-if-exception 'invalid-settable-value
                       (make-settable 42 symbol?)))
  (define-test "settable thing's initial state needs to adhere to its predicate"
    (pass-if-no-exception (make-settable 23 number?)))
  (let ((thing (make-settable 'foobar symbol?)))
    (define-test "settable thing initialised to 23"
      (pass-if-eq? 'foobar (thing)))
    (define-test "settable thing return old state when modified"
      (pass-if-eq? 'foobar (thing 'quux)))
    (define-test "settable thing set to symbol foobar"
      (pass-if-eq? 'quux (thing)))
    (define-test "settable thing requires predicate on update"
      (pass-if-exception 'invalid-settable-value
                         (thing 23)))))
