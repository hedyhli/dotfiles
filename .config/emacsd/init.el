(setq package-enable-at-startup nil)
(setq create-lockfiles nil)

;; Display
(set-face-attribute 'default nil :height 160)

;; I think this saves the last cursor pos or something
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Some better defaults?
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default major-mode 'text-mode)
(setq sentence-end-double-space nil)

;; Always tabs except for *.go?
;; TODO: set up language mode packages
(setq-default c-basic-offset  4
	      tab-width       4
	      indent-tabs-mode nil)
;; Some modes
(recentf-mode 1)
(show-paren-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; IDK
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-x") #'ielm)
	    (local-set-key (kbd "C-c C-c") #'eval-defun)
	    (local-set-key (kbd "C-c C-b") #'eval-buffer)))

(load (expand-file-name "packages.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
