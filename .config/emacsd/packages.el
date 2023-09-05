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
(straight-use-package 'elpher)
(straight-use-package 'eww)

;; vertico
;; Enable vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )
;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))
;; Pasted from vertico
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(straight-use-package 'diminish)
;; End of straight stuff hopefully

;; Not using dracula because its background not working
(load-theme 'zenburn t)

(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

;; Sometimes I disable this so I can experience how inefficient emacs's default
;; editting experience is
(require 'evil)
(evil-mode 1)

;; Modules
;; Force the load path thing at the head to reduce startup time
;; People have both lisp and site-lisp. what the heck. I'll name it modules
;; just because.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("modules"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; I have no idea if I need the add subdirs to load path thingy (see centaur
;; emacs)
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)
(require 'init-highlight)


(use-package imenu-list
  :straight t)
