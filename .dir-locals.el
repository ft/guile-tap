((nil
  (meta:project-author  . "guile-tap workers")
  (meta:project-licence . "LICENCE")
  (meta:project-code "scheme"))
 (scheme-mode
  (eval . (put 'match-string             'scheme-indent-function 1))
  (eval . (put 'for-each-test            'scheme-indent-function 1))
  (eval . (put 'with-test-bundle         'scheme-indent-function 1))
  (eval . (put 'with-fs-test-bundle      'scheme-indent-function 0))
  (eval . (put 'with-ellipsis            'scheme-indent-function 1))
  (eval . (put 'set-record-type-printer! 'scheme-indent-function 1))
  (eval . (put 'define-test              'scheme-indent-function 1))))
