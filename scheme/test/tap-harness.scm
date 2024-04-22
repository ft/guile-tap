;; Copyright 2021-2024 Frank Terbeck <ft@bewatermyfriend.org>
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

(define-module (test tap-harness)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (enable-harness-colours!
            make-harness-callback
            make-bundle-state
            input->record
            harness-run
            harness-stdin
            harness-analyse
            harness-combined-result
            pp-harness-state
            pp-bundle-state
            bundle-deterministic?
            bundle-finalise
            bundle-plan
            bundle-state
            bundle-results
            tap-process
            return
            clear-previous
            echo-input
            render-parsed
            progress-completion
            progress-plan
            progress-test
            tap-harness:main))

(define *tap-harness-version* 12)
(define *progress-column* 50)

(define (assq-change alist key value)
  (let loop ((rest alist))
    (match rest
      (((k . v) . rest*)
       (if (eq? k key)
           (cons (cons key value) rest*)
           (cons (cons k v) (loop rest*))))
      (_ (list (cons key value))))))

(define (match-string-fn obj . lst)
  (call/ec
   (lambda (return)
     (fold (lambda (el acc)
             (match el
               ((pattern . callback)
                (let ((result (string-match pattern obj)))
                  (when result
                    (return (callback result))))))
             acc)
           #f lst))))

(define-syntax-rule (match-string (obj capture) (pattern e0 e* ...) ...)
  (match-string-fn obj (cons pattern (lambda (capture) e0 e* ...)) ...))

(define (match->number m i)
  (string->number (match:substring m i)))

(define (maybe-string m i)
  (let ((obj (match:substring m i)))
    (and (string? obj)
         (not (string-null? obj))
         obj)))

(define (test-directive s)
  (match-string (s m)
    ("[ \t]+#[ \t]+TODO(|[ \t]+(.*))$"        `(todo (reason . ,m)))
    ("[ \t]+#[ \t]+SKIP[^ \t]*(|[ \t]+(.*))$" `(skip (reason . ,m)))))

(define (test-suffix s)
  (if (and (string? s) (not (string-null? s)))
      (match (test-directive s)
        ((directive ('reason . m))
         (cons (match:prefix m)
               `(,directive (reason . ,(maybe-string m 2)))))
        (_ (cons s #f)))
      (cons #f #f)))

(define (test-result m)
  (let ((result (match:substring m 1))
        (snumber (match:substring m 3))
        (desc+direct (test-suffix (match:substring m 5))))
    `(test (result . ,(string= "ok" result))
           (number . ,(and snumber (string->number snumber)))
           (description . ,(car desc+direct))
           (directive . ,(cdr desc+direct)))))

(define (test-plan m)
  (let* ((start (match->number m 1))
         (end (match->number m 2))
         (number (1+ (- end start)))
         (skip? (match:substring m 4))
         (reason (match:substring m 5))
         (directive (if (or (<= number 0) skip?)
                        `(skip (reason . ,(and reason
                                               (not (string-null? reason))
                                               reason)))
                        #f)))
    `(plan (number . ,number)
           (start . ,start)
           (end . ,end)
           (directive . ,directive))))

(define version    "^TAP version ([0-9]+)$")
(define bailout    "^Bail out!(|[ \t]+(.*))$")
(define testplan   "^([0-9]+)\\.\\.([0-9]+)(|[ \t]+#[ \t]+SKIP[^ \t]*(|[ \t]+(.*)))$")
(define diagnostic "^# ?(.*)$")
(define testline   "^(ok|not ok)(| +([0-9]+))( -[ \t]+|[ \t]+|)(.*)$")

(define (input->record input)
  (define (test-version m) `(version . ,(match->number m 1)))
  (define (test-bailout m) `(bailout (reason . ,(maybe-string m 2))))
  (define (test-diagnostic m) `(diagnostic . ,(match:substring m 1)))
  (or (match-string (input m)
        (version    (test-version m))
        (bailout    (test-bailout m))
        (testplan   (test-plan m))
        (diagnostic (test-diagnostic m))
        (testline   (test-result m)))
      `(unknown . ,input)))

(define-immutable-record-type <harness-callback>
  (make-harness-callback* version bailout plan diagnostic test completion
                          unknown)
  harness-callback?
  (version    cb:version)
  (bailout    cb:bailout)
  (plan       cb:plan)
  (diagnostic cb:diagnostic)
  (test       cb:test)
  (completion cb:completion)
  (unknown    cb:unknown))

(define (default-callback state input parsed) state)

(define* (make-harness-callback #:key
                                (version default-callback)
                                (bailout default-callback)
                                (plan default-callback)
                                (diagnostic default-callback)
                                (test default-callback)
                                (completion default-callback)
                                (unknown default-callback))
  (make-harness-callback* version bailout plan diagnostic test completion
                          unknown))

(define (directive? directive obj)
  (let ((value (assq-ref obj 'directive)))
    (and value (eq? (car value) directive))))

(define (skip? obj)
  (directive? 'skip obj))

(define (todo? obj)
  (directive? 'todo obj))

(define (test-pass? test)
  (assq-ref test 'result))

(define (make-results)
  '((pass) (fail) (todo) (skip) (todo-but-pass) (skip-but-fail)))

(define (finalise-results results)
  (map (lambda (r) (cons (car r) (reverse (cdr r))))
       results))

(define-immutable-record-type <bundle-state>
  (make-bundle-state* name title version
                      state auxiliary
                      number plan deterministic?
                      log results outcome)
  bundle-state?
  (name             bundle-name             change-bundle-name)
  (title            bundle-title            change-bundle-title)
  (version          bundle-version          change-bundle-version)
  (state            bundle-state            change-bundle-state)
  (auxiliary        bundle-auxiliary        change-bundle-auxiliary)
  (number           bundle-number           change-bundle-number)
  (plan             bundle-plan             change-bundle-plan)
  (deterministic?   bundle-deterministic?   change-bundle-deterministic?)
  (log              bundle-log              change-bundle-log)
  (results          bundle-results          change-bundle-results)
  (outcome          bundle-outcome          change-bundle-outcome))

(define* (make-bundle-state #:key
                            (name 'test) (title #f)
                            (version *tap-harness-version*)
                            (state 'init) (auxiliary '()) (number 1)
                            (plan #f) (deterministic? #f)
                            (log '()) (results (make-results))
                            (outcome '(pass)))
  (make-bundle-state* name title version
                      state auxiliary
                      number plan deterministic?
                      log results outcome))

(define (make-bundle-outcome s)
  (define (count k) (length (assq-ref (bundle-results s) k)))
  (let ((number-of-tests (if (bundle-deterministic? s)
                             (assq-ref (bundle-plan s) 'number)
                             (bundle-number s)))
        (n-pass (count 'pass))
        (n-fail (count 'fail))
        (n-skip (count 'skip))
        (n-todo (count 'todo))
        (n-skip-but-fail (count 'skip-but-fail))
        (n-todo-but-pass (count 'todo-but-pass)))
    (let ((n-combined (+ n-pass n-fail n-skip n-todo
                         n-skip-but-fail n-todo-but-pass)))
      (apply append (list (cond ((skip? (bundle-plan s)) '(skip))
                                ((not (= number-of-tests n-combined))
                                 `(fail (plan (expected ,number-of-tests)
                                              (actual ,n-combined))))
                                ((zero? n-fail) '(pass))
                                (else '(fail)))
                          (if (zero? n-todo-but-pass)
                              '()
                              '(todo-but-pass))
                          (if (zero? n-skip-but-fail)
                              '()
                              '(skip-but-fail)))))))

(define (bundle-finalise s)
  (let ((next (set-fields
               s
               ((bundle-state) 'finished)
               ((bundle-number) (1- (bundle-number s)))
               ((bundle-log) (reverse (bundle-log s)))
               ((bundle-results) (finalise-results (bundle-results s))))))
    (if (equal? '(fail) (bundle-outcome next))
        next
        (set-field next (bundle-outcome) (make-bundle-outcome next)))))

(define (push-result state kind obj)
  (change-bundle-results
   state
   (let loop ((rest (bundle-results state)))
     (match (car rest)
       ((k . v) (if (eq? k kind)
                    (cons (cons* k obj v) (cdr rest))
                    (cons (car rest) (loop (cdr rest)))))
       (_ (throw 'unknown-result-kind kind))))))

(define (push-bundle-log s obj)
  (cons obj (bundle-log s)))

(define (push-bundle-log* s obj)
  (change-bundle-log s (push-bundle-log s obj)))

(define colour-map '((black   . #\0)
                     (red     . #\1)
                     (green   . #\2)
                     (yellow  . #\3)
                     (blue    . #\4)
                     (magenta . #\5)
                     (cyan    . #\6)
                     (white   . #\7)
                     (default . #\9)))

(define attribute-map '((reset      . #\0)
                        (bold       . #\1)
                        (dim        . #\2)
                        (standout   . #\3)
                        (underscore . #\4)
                        (blink      . #\5)
                        (reverse    . #\7)
                        (invisible  . #\8)))

(define (make-colour group c)
  (string #\esc #\[ group (assq-ref colour-map c) #\m))

(define (make-fg-colour c)
  (make-colour #\3 c))

(define (make-bg-colour c)
  (make-colour #\4 c))

(define (make-attribute c)
  (string #\esc #\[ (assq-ref attribute-map c) #\m))

(define with-colour? #f)

(define (enable-harness-colours!)
  (set! with-colour? #t))

(define (cfmt:arg f a)
  (define (disp obj)
    (cond ((eq? f #t) (begin (display obj (current-output-port))
                             #t))
          ((not f) (if (symbol? obj)
                       (symbol->string obj)
                       obj))
          (else (begin (display obj f)
                       #t))))
  (define (maybe-colour maker thing)
    (if with-colour? (disp (maker thing)) ""))
  (match a
    (('fg c) (maybe-colour make-fg-colour c))
    (('bg c) (maybe-colour make-bg-colour c))
    (('at attr ...) (if with-colour?
                        (disp (string-concatenate (map make-attribute attr)))
                        ""))
    ((? string? s) (disp s))
    ((? symbol? s) (disp s))
    ((? char? c)   (disp c))
    ((args ...) (apply format (cons f args)))))

(define (cfmt f . args)
  (let ((worker (lambda (x) (cfmt:arg f x))))
    (if f
        (begin (for-each worker args)
               #t)
        (string-concatenate (map-in-order worker args)))))

(define (handle-version s version)
  (unless (= version *tap-harness-version*)
    (format #t "# Warning: Test indicates TAP version ~a, this harness implements ~a~%"
            version *tap-harness-version*))
  (change-bundle-version s version))

(define (handle-bailout s bailout)
  (case (bundle-state s)
    ((finished) (push-bundle-log s (cons 'bailout-although-finished bailout)))
    ((init active) (push-bundle-log* (change-bundle-state s 'finished)
                                     (cons 'bailout bailout)))))

(define (handle-plan s plan)
  (case (bundle-state s)
    ((init) (set-fields s
                        ((bundle-deterministic?) #t)
                        ((bundle-plan) plan)
                        ((bundle-state) (if (skip? plan)
                                             'finished
                                             'active))))
    ((active) (cond ((not (bundle-deterministic? s))
                     (set-fields s
                                 ((bundle-plan) plan)
                                 ((bundle-state) 'finished)))
                    (else (push-bundle-log*
                           s (cons 'plan-with-existing-plan plan)))))
    ((finished) (push-bundle-log* s (cons 'plan-although-finished plan)))))

(define (with-number? test)
  (assq-ref test 'number))

(define (change-test-number test n)
  (let loop ((rest test))
    (cond ((null? rest) rest)
          ((eq? (caar rest) 'number) (cons (cons 'number n)
                                           (cdr rest)))
          (else (cons (car rest) (loop (cdr rest)))))))

(define (add-result result s)
  (let* ((result (if (with-number? result)
                     result
                     (change-test-number result (bundle-number s))))
         (kind (if (test-pass? result)
                   (cond ((todo? result) 'todo-but-pass)
                         ((skip? result) 'skip)
                         (else 'pass))
                   (cond ((todo? result) 'todo)
                         ((skip? result) 'skip-but-fail)
                         (else 'fail)))))
    (push-result s kind result)))

(define (bundle-number++ s)
  (change-bundle-number s (1+ (bundle-number s))))

(define (handle-testresult s result)
  (bundle-number++
   (add-result
    result
    (case (bundle-state s)
      ((init) (set-fields s
                          ((bundle-deterministic?) #f)
                          ((bundle-state) 'active)))
      ((finished) (push-bundle-log s (cons 'result-although-finished result)))
      ((active) s)))))

(define (handle-diagnostic s diagnostic)
  s)

(define (handle-unknown s data)
  s)

(define (handle-broken s obj input)
  (format #t "# Fatal Error! Got broken data!~%")
  (format #t "#   state: ~a~%" s)
  (format #t "#     obj: ~a~%" obj)
  (format #t "#   input: ~s~%" input)
  (format #t "# Please report this issue! Giving up.~%")
  (quit 1))

(define* (tap-process s input #:optional (cb (make-harness-callback)))
  (let* ((parsed (input->record input))
         (p (lambda (k s) ((k cb) s input parsed))))
    (match parsed
      (('version    . version)    (p cb:version    (handle-version    s version)))
      (('bailout    . bailout)    (p cb:bailout    (handle-bailout    s bailout)))
      (('plan       . plan)       (p cb:plan       (handle-plan       s plan)))
      (('diagnostic . diagnostic) (p cb:diagnostic (handle-diagnostic s diagnostic)))
      (('test       . testresult) (p cb:test       (handle-testresult s testresult)))
      (('unknown    . data)       (p cb:unknown    (handle-unknown    s data)))
      (broken (handle-broken s broken input)))))

(define (run-test p r)
  (if r
      (open-pipe* OPEN_READ r p)
      (open-pipe p OPEN_READ)))

(define (process-return-code state rc)
  (let ((exit-value (status:exit-val rc))
        (stop-signal (status:stop-sig rc))
        (term-signal (status:term-sig rc))
        (push (lambda (obj)
                (change-bundle-outcome (push-bundle-log* state obj)
                                       '(fail)))))
    (cond ((and (zero? exit-value)
                (not stop-signal)
                (not term-signal))
           state)
          (stop-signal (push `(stop-signal . ,stop-signal)))
          (term-signal (push `(term-signal . ,term-signal)))
          (else (push `(non-zero-return-code . ,exit-value))))))

(define (program->state p r callback)
  (let ((port (run-test p r)))
    (let loop ((state (make-bundle-state #:name p)) (input (read-line port)))
      (if (eof-object? input)
          (let ((rc (close-pipe port)))
            ((cb:completion callback) (bundle-finalise
                                       (process-return-code state rc)) input #f))
          (loop (tap-process state input callback) (read-line port))))))

(define* (harness-run #:key
                      (run-programs '())
                      (runner #f)
                      (callback (make-harness-callback)))
  (map-in-order (lambda (p) (program->state p runner callback))
                run-programs))

(define* (harness-stdin #:optional (callback (make-harness-callback)))
  (let loop ((state (make-bundle-state))
             (input (read-line))
             (results '()))
    (cond ((eof-object? input) (reverse (cons ((cb:completion callback)
                                               (bundle-finalise state)
                                               input #f)
                                              results)))
          ((eq? (bundle-state state) 'finished)
           (loop (make-bundle-state) (read-line) (cons (bundle-finalise state)
                                                       results)))
          (else (loop (tap-process state input callback)
                      (read-line) results)))))

(define (pp obj)
  (pretty-print obj
                #:per-line-prefix "    "
                #:width 120
                #:max-expr-width 80))

(define (pp-bundle-state s)
  (let ((version (bundle-version s))
        (state (bundle-state s))
        (number (bundle-number s))
        (plan (bundle-plan s))
        (deterministic? (bundle-deterministic? s))
        (log (bundle-log s))
        (results (bundle-results s))
        (outcome (bundle-outcome s)))
    (format #t "TAP version ~a~%" version)
    (format #t "state: ~a~%" state)
    (format #t "number: ~a~%" number)
    (format #t "plan: ~a (deterministic? ~a)~%" plan deterministic?)
    (format #t "log:~%")     (pp log)
    (format #t "results:~%") (pp results)
    (format #t "outcome:~%") (pp outcome)
    s))

(define (pp-harness-state s)
  (map-in-order pp-bundle-state s))

(define (number-of-tests state)
  (apply + (map (compose length cdr)
                (bundle-results state))))

(define (bundle-analyse-version state)
  (let ((version (bundle-version state)))
    (format #t "Test Results (TAP version ~a):~%" version)
    (unless (= version *tap-harness-version*)
      (format #t "Warning: This processor implements version ~a!~%"
              *tap-harness-version*))))

(define (bundle-analyse-plan state)
  (let ((tests-that-ran (number-of-tests state))
        (tests-planned (assq-ref (bundle-plan state) 'number))
        (deterministic? (bundle-deterministic? state)))
    (if deterministic?
        (if (= tests-planned tests-that-ran)
            (format #t "Ran ~a test~p, as planned.~%"
                    tests-planned tests-planned)
            (format #t "~a test~p were planned, but ran ~a, which is ~a!~%"
                    tests-planned tests-planned tests-that-ran
                    (if (< tests-that-ran tests-planned)
                        'less 'more)))
        (format #t "Ran ~a test~p.~%" tests-that-ran tests-that-ran))))

(define (starts-with obj x)
  (and (list? obj)
       (not (null? obj))
       (eq? (car obj) x)))

(define (bundle:pass? obj) (starts-with obj 'pass))
(define (bundle:fail? obj) (starts-with obj 'fail))
(define (bundle:oddity? obj) (not (= 1 (length obj))))

(define* (harness-analyse states #:key (pre-summary (lambda (s) #t)))
  (define (count* slot f)
    (fold (lambda (s cnt)
            (if (f (slot s))
                (1+ cnt)
                cnt))
          0 states))
  (define (count f) (count* bundle-outcome f))
  (let loop ((rest states))
    (unless (null? rest)
      (let ((log (bundle-log (car rest)))
            (name (bundle-name (car rest)))
            (skip-but-fail (assq-ref (bundle-results (car rest)) 'skip-but-fail))
            (todo-but-pass (assq-ref (bundle-results (car rest)) 'todo-but-pass))
            (render-test (lambda (t)
                           (display "    ")
                           (render-parsed (car rest) #f t)))
            (treat-tests (lambda (lst) (map (lambda (x) (cons 'test x)) lst))))
        (unless (null? log)
          (format #t "~%Logbook for ~a:~%" name)
          (pp log))
        (unless (null? skip-but-fail)
          (format #t "~%Failing tests marked as SKIP in ~a:~%" name)
          (for-each render-test (treat-tests skip-but-fail)))
        (unless (null? todo-but-pass)
          (format #t "~%Passing tests marked as TODO in ~a:~%" name)
          (for-each render-test (treat-tests todo-but-pass)))
        (loop (cdr rest)))))
  (pre-summary states)
  (let* ((n (length states))
         (n-skip (count* bundle-plan skip?))
         (n-pass (count bundle:pass?))
         (n-fail (count bundle:fail?))
         (n-oddity (count bundle:oddity?))
         (tell (lambda (m colour text)
                 (cfmt #t "  • "
                       `(fg ,colour)
                       `("~a of ~a bundle~p ~a.~%" ,m ,n ,n ,text)
                       '(fg default)))))
    (format #t "Processed ~a test bundle~p:~%" n n)
    (unless (zero? n-pass)
      (tell n-pass 'green 'passed))
    (unless (zero? n-fail)
      (tell n-fail 'red 'failed))
    (when (positive? n-skip)
      (tell n-skip 'default "were skipped"))
    (unless (zero? n-oddity)
      (tell n-oddity 'yellow "encountered an oddity")
      (for-each (lambda (s)
                  (let ((name (bundle-name s))
                        (outcome (bundle-outcome s)))
                    (when (bundle:oddity? outcome)
                      (cfmt #t "  • "
                            '(fg yellow)
                            `("~a: ~a~%" ,name ,outcome)
                            '(fg default)))))
                states)))
  states)

(define (echo-input s i p)
  (display i)
  (newline)
  s)

(define (render-directive d)
  (match d
    ((kind ('reason . r))
     (format #f "# ~a~a~a"
             (string-upcase (symbol->string kind))
             (if r " " "")
             (if r r "")))))

(define (render-parsed s i p)

  (define (add-directive d)
    (when d
      (cfmt #t " " '(fg yellow) (render-directive d) '(fg default))))

  (define* (maybe obj #:key (prefix " "))
    (when obj
      (display prefix)
      (display obj)))

  (match p
    (('test ('result . result)
            ('number . num)
            ('description . desc)
            ('directive . dir))
     (cfmt #t `(fg ,(if dir
                        'yellow
                        (if result 'green 'red)))
           (if result "ok" "not ok")
           '(fg default))
     (maybe num)
     (maybe desc #:prefix " - ")
     (add-directive dir))

    (('plan ('number . num)
            ('start . start)
            ('end . end)
            ('directive . dir))
     (cfmt #t '(fg magenta) `("~a..~a" ,start ,end) '(fg default))
     (add-directive dir))

    (('diagnostic . text)
     (match-string (text m)
       ("^test bundle: " (cfmt #t '(fg magenta) "# " text '(fg default)))
       ("" (cfmt #t '(at dim) "# " text '(at reset)))))

    (('bailout ('reason . reason))
     (display "Bail out!")
     (maybe reason))

    (('version . version)
     (display "TAP version ")
     (display version))

    (('unknown . text) (display text)))

  (newline)
  s)

(define (return)
  (display #\return))

(define (clear-previous s)
  (let ((n (assq-ref (bundle-auxiliary s) 'last-progress-length)))
    (when n
      (display (make-string n #\space))
      (return))))

(define (progress-string state string)
  (display string)
  (return)
  (change-bundle-auxiliary state (assq-change (bundle-auxiliary state)
                                              'last-progress-length
                                              (string-length string))))

(define (progress-plan s i p)
  (clear-previous s)
  (let* ((n (assq-ref (bundle-plan s) 'number))
         (str (if (bundle-deterministic? s)
                  (format #f "Initialising deterministic plan: ~a tests." n)
                  (format #f "Non deterministic plan signals ~a tests." n))))
    (progress-string s str)))

(define (progress-test s i p)
  (clear-previous s)
  (let* ((n (assq-ref (bundle-plan s) 'number))
         (m (1- (bundle-number s)))
         (str (format #f "~a ~v,,'.t ~a/~a"
                      (bundle-name s) *progress-column* m n)))
    (progress-string s str)))

(define (progress-completion s i p)
  (define (is-pass?) (bundle:pass? (bundle-outcome s)))
  (define (is-skip?) (skip? (bundle-plan s)))
  (clear-previous s)
  (cfmt #t
        `("~a ~v,,'.t "
          ,(bundle-name s)
          ,*progress-column*)
        (cond ((is-pass?) '(fg green))
              ((is-skip?) '(fg yellow))
              (else '(fg red)))
        (cond ((is-pass?) "ok")
              ((is-skip?) "skip")
              (else "fail"))
        '(fg default)
        #\newline)
  s)

(define (harness-combined-result states)
  (define (get x) (lambda (b) (assq-ref (bundle-results b) x)))
  (define (count x) (fold + 0 (map (compose length (get x)) states)))
  (let ((all-outcomes-pass? (every (lambda (x) (member x '(skip pass)))
                                   (map (compose car bundle-outcome) states)))
        (pass (count 'pass))
        (fail (count 'fail))
        (skip (count 'skip))
        (todo (count 'todo))
        (skip-but-fail (count 'skip-but-fail))
        (todo-but-pass (count 'todo-but-pass)))

    (let* ((n (+ pass fail skip todo skip-but-fail todo-but-pass))
           (always (lambda _ #t))
           (tell (lambda* (m colour text #:optional (test positive?))
                   (when (test m)
                     (cfmt #t "  • "
                           `(fg ,colour)
                           `("~a of ~a test~p ~a." ,m ,n ,n ,text)
                           '(fg default))
                     (newline))))
           (pass? (and all-outcomes-pass? (zero? fail))))
      (newline)
      (format #t "Processed ~a tests:~%" n)
      (tell pass (if pass? 'green 'red) "passed" always)
      (tell fail (if pass? 'green 'red) "failed" always)
      (tell skip          'default "were skipped")
      (tell skip-but-fail 'red     "are marked to SKIP but signaled failure")
      (tell todo          'default "were marked as TODO")
      (tell todo-but-pass 'cyan    "are marked as TODO but signaled success")
      (newline)
      (cfmt #t "Test Result: "
            `(fg ,(if pass? 'green 'red))
            (if pass? "PASS" "FAIL")
            '(at reset) #\newline)
      (if pass? 0 1))))

;; Main application implementation

(define *name*    'tap-harness)
(define *version* '((major . 0)
                    (minor . 1)
                    (patch . 0)))

(define (pp-version v)
  (string-join (map (compose number->string cdr) v) "."))

(define (usage)
  (newline)
  (format #t " Usage:   tap-harness [OPTION(s)...] [PROGRAM(s)...]~%")
  (newline)
  (format #t "  --help, -h             Print this help text.~%")
  (format #t "  --version, -V          Print version information about the program.~%")
  (format #t "  --debug, -D            Enable debugging output in the program.~%")
  (format #t "  --verbose, -v          Enable verbose harness operation.~%")
  (format #t "  --colour, -c           Enable terminal colours in harness output.~%")
  (newline)
  (format #t "  --exec EXEC, -e EXEC   Run PROGRAM(s) via EXEC.~%")
  (newline))

(define (tap-harness:main params)
  (define parameters (let loop ((rest params))
                       (cond ((null? rest) params)
                             ((string= (car rest) "--") (cons "tap-harness"
                                                              (cdr rest)))
                             (else (loop (cdr rest))))))
  (define option-spec
    '((blib              (single-char #\b))
      (colour            (single-char #\c))
      (color)
      (comments          (single-char #\o))
      (count)
      (debug             (single-char #\D))
      (directives)
      (dry               (single-char #\d))
      (failures          (single-char #\f))
      (help              (single-char #\h))
      (ignore-exit)
      (man               (single-char #\H))
      (merge             (single-char #\m))
      (nocolor)
      (nocount)
      (norc)
      (normalize)
      (parse             (single-char #\p))
      (quiet             (single-char #\q))
      (QUIET             (single-char #\Q))
      (recurse           (single-char #\r))
      (reverse)
      (shuffle           (single-char #\s))
      (tainting-checks   (single-char #\T))
      (tainting-warnings (single-char #\t))
      (timer)
      (trap)
      (verbose           (single-char #\v))
      (version           (single-char #\V))
      (fatal-warnings    (single-char #\W))
      (enable-warnings   (single-char #\w))
      (include-path      (single-char #\I) (value #t))
      (load-plugin       (single-char #\P) (value #t))
      (load-module       (single-char #\M) (value #t))
      (archive           (single-char #\a) (value #t))
      (exec              (single-char #\e) (value #t))
      (ext                                 (value #t))
      (formatter                           (value #t))
      (harness                             (value #t))
      (jobs              (single-char #\j) (value #t))
      (rc                                  (value #t))
      (rules                               (value #t))
      (source                              (value #t))
      (state                               (value #t))
      (statefile                           (value #t))))

  ;; These are options that prove supports, that we just eat and ignore. Some
  ;; of them might be nice to have, though. We'll make a note of their use in
  ;; --debug mode in the future.
  (define prove-compat
    '(norc
      man
      lib blib shuffle nocolor count nocount dry failures comments ignore-exit
      merge recurse reverse quiet QUIET parse directives timer trap normalize
      ext harness formatter source archive jobs state statefile rc rules))

  (define opts (getopt-long parameters option-spec
                            #:stop-at-first-non-option #t))

  (define (opt o)
    (option-ref opts o #f))

  (define (read-files-from-stdin)
    (let loop ((input (read-line)) (files '()))
      (if (eof-object? input)
          (reverse files)
          (loop (read-line) (cons input files)))))

  (define harness-callback
    (if (opt 'verbose)
        (make-harness-callback #:test render-parsed
                               #:plan render-parsed
                               #:diagnostic render-parsed
                               #:version render-parsed
                               #:bailout render-parsed)
        (make-harness-callback #:plan progress-plan
                               #:test progress-test
                               #:bailout render-parsed
                               #:completion progress-completion)))

  (when (opt 'help)
    (usage)
    (quit 0))

  (when (opt 'version)
    (format #t "~a version ~a~%" *name* (pp-version *version*))
    (quit 0))

  (define file-list
    (fold-right (lambda (x a)
                  (if (string= x "-")
                      (append (read-files-from-stdin) a)
                      (cons x a)))
                '()
                (opt '())))

  (define (with-arguments?)
    ((compose not zero? length) file-list))

  (when (and (opt 'exec) (not (with-arguments?)))
    (format #t "~a: Option --exec (-e) requires non-option arguments to run.~%"
            *name*)
    (quit 1))

  (when (or (opt 'colour) (opt 'color))
    (enable-harness-colours!))

  (quit
   (harness-combined-result
    (if (with-arguments?)
        (harness-analyse ((if (opt 'debug) pp-harness-state identity)
                          (harness-run #:run-programs file-list
                                       #:runner (opt 'exec)
                                       #:callback harness-callback))
                         #:pre-summary (lambda (_) (newline)))
        (harness-analyse ((if (opt 'debug) pp-harness-state identity)
                          (harness-stdin harness-callback))
                         #:pre-summary (lambda (_) (newline)))))))
