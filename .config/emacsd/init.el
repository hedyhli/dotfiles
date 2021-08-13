;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;(setq package-archives '(
;;  ("gnu" . "http://elpa.gnu.org/packages/")
;;  ("melpa" . "http://melpa.milkbox.net/packages/")
;;  )
;;)
;;(setq dracula-use-24-bit-colors-on-256-colors-terms t)
;;(load-theme 'dracula t)
;;(unless (display-graphic-p)
;;  (set-face-background 'default "black" nil))

(load-theme 'zenburn t)

(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

;;(require 'evil)
;;(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elpher zenburn-theme spacemacs-theme moe-theme material-theme eww-lnum evil dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
