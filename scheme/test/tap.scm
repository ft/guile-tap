;; TAP emiting test suite for scheme (currently GNU Guile only).

;; Copyright 2012-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Redistribution  and  use  in  source  and  binary  forms,  with  or  without
;; modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;  2. Redistributions  in binary  form must  reproduce the  above  copyright
;;     notice, this  list of conditions  and the following disclaimer  in the
;;     documentation and/or other materials provided with the distribution.
;;
;;  THIS SOFTWARE IS PROVIDED  "AS IS"  AND ANY EXPRESS OR  IMPLIED WARRANTIES,
;;  INCLUDING, BUT  NOT LIMITED TO,  THE IMPLIED WARRANTIES  OF MERCHANTABILITY
;;  AND FITNESS FOR A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT SHALL THE
;;  AUTHOR OR CONTRIBUTORS  OF THE PROJECT BE LIABLE FOR  ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL,  EXEMPLARY, OR  CONSEQUENTIAL DAMAGES  (INCLUDING, BUT
;;  NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE,
;;  DATA,  OR PROFITS;  OR BUSINESS  INTERRUPTION)  HOWEVER CAUSED  AND ON  ANY
;;  THEORY  OF  LIABILITY,  WHETHER  IN CONTRACT,  STRICT  LIABILITY,  OR  TORT
;;  (INCLUDING NEGLIGENCE  OR OTHERWISE) ARISING IN  ANY WAY OUT OF  THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; A TAP test suite is a set of files, that emit output according to the TAP
;; protocol. One or more test cases form a test-bundle. A file may hold one or
;; more test-bundles.
;;
;; This implementation uses the following API:
;;
;;   - with-test-bundle
;;         Surrounds a set of test-cases.
;;
;;   - define-test
;;         Surrounds the code of one test case, which should be implemented
;;         in terms of a `pass-if-*' call (see below).
;;
;;   - plan and no-plan
;;         Setup the test plan for a test-bundle.
;;
;;   - Various pass-if-* macros
;;         These are convenience macros, to easily write test cases.
;;
;; A valid test script may look like this:
;;
;; (use-modules (test tap))
;;
;; (with-test-bundle (foo bar)
;;   (plan 1)
;;   (define-test "basic math"
;;     (pass-if-= (+ 1 2)
;;                3)))
;;
;; TAP Version Support
;;
;;   This code currently produces output according to TAP protocol version 12,
;;   but emitting version 13 output should not be a problem at all. However,
;;   since the current Perl Test::More module doesn't even do that itself, this
;;   code won't bother.

(define-module (test tap)
  #:use-module (system vm frame)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (pass-if-exception
            pass-if-any-exception
            pass-if-no-exception
            pass-if-=             pass-if-not-=
            pass-if-~=            pass-if-not-~=
            pass-if-<             pass-if-not-<
            pass-if->             pass-if-not->
            pass-if-<=            pass-if-not-<=
            pass-if->=            pass-if-not->=
            pass-if-zero?         pass-if-not-zero?
            pass-if-eq?           pass-if-not-eq?
            pass-if-eqv?          pass-if-not-eqv?
            pass-if-equal?        pass-if-not-equal?
            pass-if-null?         pass-if-not-null?
            pass-if-string=?      pass-if-not-string=?
            pass-if-string-ci=?   pass-if-not-string-ci=?
            pass-if-re-match      pass-if-not-re-match
            pass-if-true          pass-if-false
            plan                  no-plan
            require
            todo
            define-test
            define-tap-test
            with-fs-test-bundle
            with-test-bundle
            for-each-test
            force-import
            make-labeled-values
            tap/bail-out
            tap/comment
            tap/get-option
            tap/set-option))

;; Internal variables

(define* (make-settable init #:optional (valid? (const #t)))
  (unless (valid? init)
    (throw 'invalid-settable-value init))
  (let ((state init))
    (lambda args
      (match args
        (() state)
        ((update)
         (unless (valid? update)
           (throw 'invalid-settable-value update))
         (let ((old-state state))
           (set! state update)
           old-state))
        (_ (error (format #f "settable: Invalid call structure: ~a~%"
                          (cons '_ args))))))))

(define-syntax-rule (define! name expr ...)
  (define name (make-settable expr ...)))

(define! test-description #f)
(define! test-hierarchy   '())
(define! test-case-count  0  integer?)
(define! test-case-todo   #f boolean?)

;; TAP module options

(define (valid-plan? plan)
  (or (not plan)
      (and (integer? plan)
           (positive? plan))))

(define! plan                  #f                   valid-plan?)
(define! todo-prints-diag      #f                   boolean?)
(define! test-pp-width         60                   integer?)
(define! test-force-tap-header #f                   boolean?)
(define! test-fs-suffix        ".t$"                string?)
(define! test-fs-prefix        "^[0-9]+-"           string?)
(define! test-fs-root          (getcwd)             string?)
(define! test-fs-file          (car (command-line)) string?)

(define options
  `((fs-file                  . ,test-fs-file)
    (fs-prefix                . ,test-fs-prefix)
    (fs-root                  . ,test-fs-root)
    (fs-suffix                . ,test-fs-suffix)
    (tap-todo-prints-diagnose . ,todo-prints-diag)
    (tap-force-header         . ,test-force-tap-header)
    (tap-pretty-print-width   . ,test-pp-width)))

(define (opt:get-entry key extr)
  (let ((result (filter-map (lambda (x)
                              (if (eq? (car x) key)
                                  x #f))
                            options)))
    (if (null? result)
        '()
        (extr (car result)))))

(define (with-option key . rest)
  (let ((parameter (assq-ref options key)))
    (unless parameter
      (throw 'invalid-tap-option key))
    (apply parameter rest)))

(define (tap/get-option key)
  (with-option key))

(define (tap/set-option key value)
  (with-option key value))

;; Plan handling

(define (no-plan)
  (plan #f))

(define (deterministic-plan?)
  (integer? (plan)))

(define (non-deterministic-plan?)
  (not (plan)))

;; Test suite core

;; `update-test' updates all relevant book-keeping when a new test is run. It
;; also handles some output which is applicable only for the first test within
;; a bundle.
(define (update-test name)
  (if (= (test-case-count) 0)
      (begin
        (tap/header)
        (tap/comment (format #f "test bundle: ~a" (test-hierarchy)))
        (if (number? (plan))
            (tap/plan (plan)))))
  (test-case-count (+ (test-case-count) 1))
  (test-description name))

(define (print-location loc)
  (tap/comment (format #f "    file: ~s" (assq-ref loc 'filename)))
  (tap/comment (format #f "    line: ~d" (assq-ref loc 'line)))
  (tap/comment (format #f "  column: ~d" (assq-ref loc 'column))))

(define (pp-expression expression)
  (pretty-print expression
                #:width (test-pp-width)
                #:display? #f
                #:per-line-prefix "#     "))

(define-immutable-record-type <exception>
  (make-exception name arguments source-location expression stack)
  exception?
  (name exception-name)
  (arguments exception-arguments)
  (source-location exception-location)
  (expression exception-expression)
  (stack exception-stack with-stack))

(set-record-type-printer! <exception>
  (lambda (record port)
    (write-char #\# port)
    (write-char #\[ port)
    (write 'exception port)
    (write-char #\space port)
    (let ((name (exception-name record))
          (arguments (exception-arguments record)))
      (write name port)
      (write-char #\space port)
      (write (cond
              ((null? arguments) '*empty-argument*)
              ((<= (length arguments) 2) arguments)
              (else (list (car arguments) '...)))
             port))
    (write-char #\] port)))

;; `error-diag' provides detailed diagnostic output for failed tests.
(define (error-diag test full loc expression evaled data)
  (format #t "#~%# failed test: ~s~%" (test-description))
  (format #t "#~%# location:~%")
  (print-location loc)
  (format #t "#~%# full test-expression:~%")
  (pp-expression full)
  (format #t "#~%# form:~%")
  (pp-expression test)
  (format #t "#~%# expression:~%")
  (pp-expression expression)
  (format #t "#~%# evaluated form:~%")
  (let ((form (let loop ((rest data) (acc (list (car test))))
                (if (null? rest)
                    (reverse acc)
                    (let ((this (car rest))
                          (rest (cdr rest)))
                      (loop rest (cons (if (list? this)
                                           (syntax->datum #`(quote #,this))
                                           this)
                                       acc)))))))
    (pp-expression form))
  (format #t "#~%# evaluated expression:~%")
  (pp-expression evaled)
  (format #t "#~%"))

(define (handle-wrong-number-of-arguments name loc input-a input)
  (let ((name* (syntax->datum name))
        (expected (syntax->datum input-a))
        (actual (syntax->datum input)))
    (format #t "#~%# failed test: ~s~%" (test-description))
    (format #t "#~%# Wrong number of arguments with: ~a~%" name*)
    (format #t "#~%# location:~%")
    (print-location loc)
    (format #t "#~%# expected form:~%")
    (pp-expression (cons name* expected))
    (format #t "#~%# actual form:~%")
    (pp-expression (cons name* actual))
    (format #t "#~%")))

;; `deal-with-exception' diagnoses caught exceptions.
(define (deal-with-exception excp)

  (define format-error-msgs '(unbound-variable
                              wrong-number-of-args
                              wrong-type-arg))

  (define (else-handler args)
    (tap/comment "argument:")
    (tap/comment (format #f "    ~s" (exception-arguments excp)))
    (tap/comment ""))

  (tap/comment "exception:")
  (tap/comment (format #f "    ~s" (exception-name excp)))
  (tap/comment "")
  (cond ((member (exception-name excp) format-error-msgs)
         ;; This expansion of ‘match’ seems to cause the compiler to yield two
         ;; unused-variable warnings: else, failure. Just for the record.
         (match (exception-arguments excp)
           ((#f fmt (arg) #f)
            (tap/comment (format #f fmt arg))
            (tap/comment ""))
           ((proc fmt (args ...) ...)
            (tap/comment (format #f "In procedure ~a: " proc))
            (tap/comment (string-append "    " (apply format (append (list #f fmt)
                                                                     (car args)))))
            (tap/comment ""))
           (else (else-handler (exception-arguments excp)))))
        ((null? (exception-arguments excp))
         (tap/comment "Empty exception argument.")
         (tap/comment ""))
        (else (else-handler (exception-arguments excp))))
  (tap/comment "backtrace:")
  (format #t "#~%")
  (for-each (lambda (s) (format #t "#   ~a~%" s))
            (string-split (with-output-to-string
                            (lambda ()
                              (display-backtrace (exception-stack excp)
                                                 (current-output-port))))
                          #\newline)))

;; `require' is used to dynamically determine whether a dependency of a
;; test-case or even a test-bundle is satisfied and either skip the test or the
;; whole bundle. `require' must be called as soon as possible within
;; `with-test-bundle' or `define-test'.
(define (require test)
  (if (not test)
      (throw 'sts/test-requirement-failed)))

;; The `todo' form may be used to wrap any number of expressions in. Any test
;; executed within this form will be marked as "TODO". So, it may fail without
;; failing the whole test-suite.
(define-syntax todo
  (lambda (x)
    (syntax-case x ()
      ((_ exp0 exp1 ...)
       #'(let ()
           (test-case-todo #t)
           exp0
           exp1
           ...
           (test-case-todo #f))))))

;; The `define-test' is used to introduce each and every test case together
;; with a name for human beings to recognise it by. Test code (as in
;; `pass-if-*') may not appear outside of an `define-test' form.
(define-syntax define-test
  (lambda (x)
    (syntax-case x (skip)
      ((_ skip name code ...)
       #'(let ()
           (update-test name)
           (tap/skip (test-case-count) (test-description))))
      ((_ name code ...)
       #'(let ()
           (update-test name)
           (catch 'sts/test-requirement-failed
             (lambda ()
               code ...)
             (lambda (k . a)
               (tap/skip (test-case-count) (test-description)))))))))

;; `with-test-bundle' initialises all the accounting data for a test bundle and
;; provides an environment to run tests in. In particular, it handles plan
;; output and catches the exception thrown by `tap/bail-out'.
;;
;; `define-test' calls should *ONLY* appear within `with-test-bundle' (or its
;; cousin `with-fs-test-bundle' which is really implemented by the former,
;; too).
(define-syntax with-test-bundle
  (lambda (x)
    (syntax-case x (skip)
      ((_ skip hierarchy code ...)
       #'(tap/bundle-skip (format #f "~a" (quote hierarchy))))
      ((_ hierarchy code ...)
       #'(let ()
           (test-case-count 0)
           (test-hierarchy (quote hierarchy))
           (no-plan)
           (catch 'sts/tap/bail-out
             (lambda ()
               (catch 'sts/test-requirement-failed
                 (lambda ()
                   code ...
                   (if (non-deterministic-plan?)
                       (tap/plan (test-case-count))))
                 (lambda (k . a)
                   (tap/bundle-skip (format #f "~a" (quote hierarchy))))))
             (lambda (k . a)
               #t)))))))

;; `with-fs-test-bundle' is an addon to `with-test-bundle'. It takes the
;; test-code's files-name, strips off a root directory, a prefix and a suffix,
;; splits the rest at forward slashes and uses the resulting list as the
;; test-bundle's hierarchy:
;;
;;    root: "/usr/src/thing/tests"
;;  suffix: "-scm.t$"
;;  prefix: "^[0-9]+"-
;;    file: "/usr/src/thing/tests/foo/bar/0001-baz-scm.t"
;;
;; Resulting hierarchy: (foo bar baz)
;;
;; `deduce-hierarchy' maps a file-name to a hierarchy as described above. All
;; this hierarchy is an absolutely cosmetic feature, for human beings to tell
;; what a set of test-cases is about. Even if you don't set this feature up
;; correctly and still use `with-fs-test-bundle', the tests will run *just*
;; *fine*.
(define* (deduce-hierarchy filename
                           #:key
                           (root (test-fs-root))
                           (suffix (test-fs-suffix))
                           (prefix (test-fs-prefix)))
  (let* ((root-dir (if (string-match "/$" root)
                       root
                       (string-concatenate (list root "/"))))
         (relfile (if (string-match
                       (string-concatenate (list "^" root-dir))
                       filename)
                      (substring filename (string-length root-dir))
                      filename))
         (h (string-split relfile #\/))
         (h-start (list-head h (- (length h) 1)))
         (end (list-ref h (- (length h) 1)))
         (s-stripped (if (string-match suffix end)
                         (regexp-substitute #f
                                            (string-match suffix end)
                                            'pre)
                         end))
         (last-part (if (string-match prefix s-stripped)
                        (regexp-substitute #f
                                           (string-match prefix s-stripped)
                                           'post)
                        s-stripped)))
    (map (lambda (x)
           (string->symbol x))
         (append h-start (list last-part)))))

;; Finally, the `with-fs-test-bundle' macro, which is implemented in terms of
;; `with-test-bundle' through the help of `deduce-hierarchy'.
(define-syntax with-fs-test-bundle
  (lambda (x)
    (with-syntax (((hierarchy ...)
                   (map (lambda (x)
                          (datum->syntax #'x x))
                        (deduce-hierarchy (test-fs-file)))))
      (syntax-case x (skip)
        ((_ skip code ...)
         #'(with-test-bundle skip (hierarchy ...) code ...))
        ((_ code ...)
         #'(with-test-bundle (hierarchy ...) code ...))))))

(define (caught-title expected?)
  (let ((common "aught exception(s) while evaluating test expression!"))
    (if expected?
        (tap/comment (string-append "C" common))
        (tap/comment (string-append "Unexpectedly c" common)))
    (tap/comment "")))

(define-syntax with-exception-handling
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       #'(let* ((stack #f)
                (value (catch #t
                         (lambda () exp)
                         (lambda (key . arguments)
                           (make-exception key arguments
                                           (current-source-location)
                                           (quote exp)
                                           stack))
                         (lambda (key . arguments)
                           (set! stack (make-stack #t))))))
           (if (exception? value)
               (with-stack value stack)
               value))))))

(define (maybe-list x)
  (if (list? x)
      (syntax->datum #`(quote #,x))
      x))

(define (dtp:tree-map f p? t)
  (define (atom? x)
    (not (pair? x)))
  (syntax-case t ()
    (() t)
    (e (p? #'e) (f t))
    (e (atom? #'e) #'e)
    (else (cons (dtp:tree-map f p? (car t))
                (dtp:tree-map f p? (cdr t))))))

(define (dtp:xchange-expressions in val expression)
  (let ((ex (let loop ((i in) (v val) (acc '()))
              (if (null? i)
                  (reverse acc)
                  (loop (cdr i) (cdr v)
                        (cons (cons (car i) (car v)) acc))))))
    (dtp:tree-map (lambda (x) (assoc-ref ex x))
                  (lambda (x) (member x in))
                  expression)))

(define (dtp:quasiquote-temps temps expression)
  (dtp:tree-map (lambda (x) #`(unquote (maybe-list #,x)))
                (lambda (x) (member x temps))
                (list #'quasiquote expression)))

;; This is the main driver of the  whole library. It's a macro that defines
;; other macros. In particular, tests expressed  by this library use one of
;; the numerous pass-if-* macros. Like this:
;;
;;    (define-test "Basic arithmetic works"
;;      (pass-if-= 65 (+ 23 42)))
;;
;; The ‘define-test’  form is another  macro, that basically  establishes a
;; name for an  enclosed test and takes care of  collecting its result. The
;; ‘pass-if-=’ form is a test define by this macro, like this:
;;
;;    (define-tap-test (pass-if-= a b) (= a b))
;;
;; The  idea here  is to  only express  the innermost  expression that  the
;; actual test should  use, and hide all the additional  machinery, such as
;; exception handling and as well as  helpful output in case of test failu-
;; res.
(define-syntax define-tap-test
  (lambda (stx-a)
    (syntax-case stx-a ()
      ;; The simple form captures a name for the generated macro as well as
      ;; a list  of arguments that macro expects, along  with an expression
      ;; that will become the core  of the test the target macro expresses.
      ;; The  implementation is a  call to the  general form of  this macro
      ;; with its ‘allow-exception?’ argument set to ‘#f’.
      ((_ (name-a input-a ...) exp)
       #'(define-tap-test #f (name-a input-a ...) exp))
      ;; This is  the general expander: If ‘allow-exception?’  is true, the
      ;; generated  code expects exceptions  to be part  of the test  it is
      ;; expressing.  If it's  false, exceptions constitute  unexpected er-
      ;; rors.
      ((_ allow-exception? (name-a input-a ...) exp)
       #'(begin
           ;; This is where the bodies of the generated ‘pass-if-*’ macros
           ;; start.
           (define-syntax name-a
             (lambda (stx)
               (with-ellipsis :::
                 (syntax-case stx ()
                   ;; Now the  generated macros all have a name  and a list
                   ;; of  arguments. This  syntax-case has a  guard expres-
                   ;; sion  that makes sure  the macro was called  with the
                   ;; same number of arguments as the expression's spec, at
                   ;; the time ‘define-tap-test’ was called, indicated.
                   ((name input :::)
                    (= (length #'(input-a ...))
                       (length #'(input :::)))
                    ;; One of the jobs of  a testing framework, in my mind,
                    ;; is to  be helpful toward a user in  case a test case
                    ;; does not pass its specification. This implementation
                    ;; tries to do this by separately evaluating all of the
                    ;; expressions from  its specification separately. That
                    ;; way you don't get just  a final result of success or
                    ;; failure, but also a set of intermediate results.
                    ;;
                    ;; (result ...) is a set of variables that will be used
                    ;; to store the evaluated versions of each of the input
                    ;; parameters.
                    ;;
                    ;; exp* is a  variant of the specification's expression
                    ;; with  all of the  input parameters exchanged  by the
                    ;; variables, that will carry their evaluated versions.
                    ;;
                    ;; evaled is a version  of exp* and therefore exp, that
                    ;; quotes the entire  expression from the specification
                    ;; except for the  result variables that were generated
                    ;; earlier. This when evaluated  gives you a version of
                    ;; the  input expression  with the  values of  the eva-
                    ;; luated input parameters exchanged in place of them.
                    (with-syntax (((result :::) (generate-temporaries #'(input :::))))
                      (with-syntax ((exp* (dtp:xchange-expressions #'(input-a ...) #'(result :::) #'exp)))
                        (with-syntax ((evaled (dtp:quasiquote-temps #'(result :::) #'exp*)))
                          ;; Now first populate  all temporary result vari-
                          ;; ables.  Then produce a final  result by evalu-
                          ;; ating exp*. And that  is basically it. All the
                          ;; rest is just handling different outcomes, suc-
                          ;; cesses, failures, exceptions (are they allowed
                          ;; or not? — did they  happen in one of the input
                          ;; expressions or in  the evaluation of the final
                          ;; expression?).  Code wise the body  of this let
                          ;; is comparatively simple.
                          #'(let* ((result (with-exception-handling input)) :::
                                   (final (with-exception-handling exp*))
                                   (exception-in-arguments?
                                    (not (not (member #t (map exception?
                                                              (list result :::))))))
                                   (late-exception? (exception? final))
                                   (failed? (or (and (not allow-exception?)
                                                     exception-in-arguments?)
                                                late-exception?
                                                (not final)))
                                   (success? (not failed?))
                                   (exception-helper
                                    (lambda (x)
                                      (if (exception? x)
                                          (deal-with-exception x)))))
                              ;; Report the TAP result of the test.
                              (tap/result (test-case-count)
                                          (test-description)
                                          (test-case-todo)
                                          success?)
                              ;; Output diagnostics if the result requires it.
                              (when (and (or (todo-prints-diag)
                                             (not (test-case-todo)))
                                         failed?)
                                (error-diag '(name-a input-a ...)
                                            '(name-a input :::)
                                            (current-source-location)
                                            'exp
                                            evaled
                                            (list result :::))
                                (when exception-in-arguments?
                                  (caught-title allow-exception?)
                                  (for-each exception-helper (list result :::)))
                                (when (and (not exception-in-arguments?)
                                           late-exception?)
                                  (caught-title #f)
                                  (exception-helper final)))
                              ;; Make the entire ‘pass-if-*’ form return
                              ;; the boolean result of the test.
                              success?)))))
                   ;; Here's  the same  syntax-case as before,  but without
                   ;; the guard, so it picks up all the cases where the ge-
                   ;; nerated macro is called  with the wrong number of ar-
                   ;; guments. This case will fail the invalid test and no-
                   ;; tify the user via TAP output.
                   ((name e :::)
                    #'(begin
                        (tap/result (test-case-count)
                                    (test-description)
                                    (test-case-todo)
                                    #f)
                        (handle-wrong-number-of-arguments
                         'name
                         (current-source-location)
                         (list 'input-a ...) (list 'e :::))
                        ;; In this misused case, the ‘pass-if-*’ form
                        ;; returns boolean false.
                        #f)))))))))))

;; pass-if-*

(define yes (lambda (x) #t))

(define-syntax handle-exception
  (lambda (x)
    (syntax-case x ()
      ((_ exp sel mod)
       #'(if (exception? exp)
             (mod (sel (exception-name exp)))
             (mod #f))))))

(define-syntax any-exception-fails
  (lambda (x)
    (syntax-case x ()
      ((_ e) #'(handle-exception e yes not)))))

(define-syntax require-any-exception
  (lambda (x)
    (syntax-case x ()
      ((_ e) #'(handle-exception e yes identity)))))

(define-syntax require-specific-exception
  (lambda (x)
    (syntax-case x ()
      ((_ exception expression)
       #'(handle-exception expression
                           (lambda (x) (eq? exception x))
                           identity)))))

(define-tap-test (pass-if-= a b) (= a b))
(define-tap-test (pass-if-not-= a b) (not (= a b)))

(define-tap-test (pass-if-~= a b eps) (and (< a (+ b eps))
                                           (> a (- b eps))))
(define-tap-test (pass-if-not-~= a b eps) (not (and (< a (+ b eps))
                                                    (> a (- b eps)))))

(define-tap-test (pass-if-< a b) (< a b))
(define-tap-test (pass-if-not-< a b) (< a b))

(define-tap-test (pass-if-> a b) (> a b))
(define-tap-test (pass-if-not-> a b) (> a b))

(define-tap-test (pass-if-<= a b) (<= a b))
(define-tap-test (pass-if-not-<= a b) (<= a b))

(define-tap-test (pass-if->= a b) (>= a b))
(define-tap-test (pass-if-not->= a b) (>= a b))

(define-tap-test (pass-if-zero? a) (zero? a))
(define-tap-test (pass-if-not-zero? a) (zero? a))

(define-tap-test (pass-if-eq? a b) (eq? a b))
(define-tap-test (pass-if-not-eq? a b) (not (eq? a b)))

(define-tap-test (pass-if-eqv? a b) (eqv? a b))
(define-tap-test (pass-if-not-eqv? a b) (not (eqv? a b)))

(define-tap-test (pass-if-equal? a b) (equal? a b))
(define-tap-test (pass-if-not-equal? a b) (not (equal? a b)))

(define-tap-test (pass-if-null? a) (null? a))
(define-tap-test (pass-if-not-null? a) (null? a))

(define-tap-test (pass-if-string=? a b) (string=? a b))
(define-tap-test (pass-if-not-string=? a b) (not (string=? a b)))

(define-tap-test (pass-if-string-ci=? a b) (string-ci=? a b))
(define-tap-test (pass-if-not-string-ci=? a b) (not (string-ci=? a b)))

(define-tap-test (pass-if-re-match pattern string) (string-match pattern string))
(define-tap-test (pass-if-not-re-match pattern string) (not (string-match pattern string)))

(define-tap-test #t (pass-if-no-exception expression)
  (any-exception-fails expression))
(define-tap-test #t (pass-if-any-exception expression)
  (require-any-exception expression))
(define-tap-test #t (pass-if-exception exception expression)
  (require-specific-exception exception expression))

(define-tap-test (pass-if-true x) x)
(define-tap-test (pass-if-false x) (not x))

;; Utility functions and macros

(define-syntax make-labeled-values
  (lambda (x)
    (syntax-case x ()
      ((_ symbol ...)
       #'(list (cons (quote symbol) symbol) ...)))))

(define-syntax force-import
  (lambda (x)
    (syntax-case x ()
      ((_ module symbol)
       #'(define symbol (@@ module symbol)))
      ((_ module symbol0 symbol1 ...)
       #'(begin (force-import module symbol0)
                (force-import module symbol1 ...))))))

(define-syntax for-each-test
  (syntax-rules (=>)
    ((_ (lst => current) exp0 exp ...)
     (let loop ((rest lst))
       (when (not (null? rest))
         (let ((current (car rest)))
           exp0 exp ...)
         (loop (cdr rest)))))))

;; --- TAP writers ---
;;
;; See <http://testanything.org> for details. This code tries to implement the
;; protocol in the version stored in the `*sts/tap/version*' variable.

(define *sts/tap/version* 12) ; v13 is with YAML support

;; Every test bundle with TAP version >= 13, prints a header announcing the
;; version of the implemented TAP protocol.
(define (tap/header)
  (if (or (test-force-tap-header)
          (>= *sts/tap/version* 13))
      (format #t "TAP version ~d~%" *sts/tap/version*)))

;; Every test bundle should announce how many tests it is going to run (if it
;; can't be deduced how many test there are going to be you can also print the
;; plan at the end of the bundle's output).
(define (tap/plan plan)
  (format #t "1..~d~%" plan))

;; If a bundle is to be skipped entirely (maybe a prerequisite is not
;; fullfilled), that has to be announced with a plan announcing a plan from 1
;; to 0 (yes, zero) with a SKIP and description.
(define (tap/bundle-skip description)
  (format #t "1..0 # SKIP ~a~%" description))

;; The normal outcome of a test is either "ok" or "not ok".
(define (tap/result num description todo result)
  (format #t "~a ~d - ~a~a~%" (if result "ok" "not ok")
                              num
                              description
                              (if todo " # TODO" "")))

;; Tests can also be skipped.
(define (tap/skip num description)
  (format #t "ok ~d - ~a # SKIP~%" num description))

;; You can also spit out arbitrary other data, which the harness will ignore.
(define (tap/comment data)
  (cond ((list? data)
         (for-each (lambda (string)
                     (format #t "# ~a~%" string))
                   data))
        (else (format #t "# ~a~%" data))))

;; When something went really wrong, you can bail out of a test bundle
;; entirely.
(define (tap/bail-out description)
  (let ((desc (if (string= description "")
                  ""
                  (string-concatenate (list " " description)))))
    (format #t "Bail out!~a~%" desc))
  (throw 'sts/tap/bail-out))

;; And finally, diagnostic messages may be formatted in YAML instead of simple
;; comments, as required by TAP version 13. TODO: This should probably be split
;; into multiple procedures.
(define* (tap/yaml #:key (message #f)
                         (severity #f)
                         (got #f)
                         (expected #f))
  (format #t " ~a~% ~a: ~a~% ~a~%" "---" "status" "not implemented yet" "..."))
