;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;; Must run doom sync or doom/reload after modifying this file

;; Installing from git repo (see more on straight docs)
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Use `:branch' to install from a branch
;; straight can't deal with non-master branches:  raxod502/straight.el#279
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release.
;; unpin one or multiple packages:
;(unpin! pinned-package another-pinned-package)
;; Or all (not recommended)
;(unpin! t)

(package! elpher)
(package! eww)
