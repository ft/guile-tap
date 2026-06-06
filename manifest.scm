;; guix shell
;; guix shell --pure
;; guix shell --container

(specifications->manifest
 '("guile"
   "guile-readline"
   "coreutils"))
