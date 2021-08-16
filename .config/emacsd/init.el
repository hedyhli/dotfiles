;;; I don't use emacs built-in manager anymore but keeping it here just in case
;;(package-initialize)
;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;(setq package-archives '(
;;  ("gnu" . "http://elpa.gnu.org/packages/")
;;  ("melpa" . "http://melpa.milkbox.net/packages/")
;;  )
;;)

;;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'zenburn-theme)
(use-package evil
  :straight t)

;;(setq dracula-use-24-bit-colors-on-256-colors-terms t)
;;(load-theme 'dracula t)
;;(unless (display-graphic-p)
;;  (set-face-background 'default "black" nil))

(load-theme 'zenburn t)

(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

(require 'evil)
(evil-mode 1)
