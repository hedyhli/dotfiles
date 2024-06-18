;; NOTE:
;; This file is generated from packages.org

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst HAS-EMACS28+    (> emacs-major-version 27))
(defconst HAS-EMACS29+    (> emacs-major-version 28))
(defconst HAS-DYNMODULES  (featurep 'dynamic-modules))
(defconst HAS-NATIVECOMP  (featurep 'native-compile))

(set-face-attribute 'completions-first-difference nil :weight 'normal)

(defvar ./corfu-bar-color "gray20" ;; Default value is for dark themes
  "Color for the corfu scroll bar.")
(defvar ./cursor-color "white"
  "Color for cursor")
(defvar ./theme-type "dark"
  "Dark or light")

(use-package ef-themes
  :config
  (load-theme 'ef-elea-dark t)
  (set-face-attribute 'cursor nil :background "white")
  (set-face-attribute 'font-lock-builtin-face nil :weight 'normal)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "white")
  (set-face-attribute 'font-lock-property-name-face nil :foreground "white")
  (set-face-attribute 'font-lock-constant-face nil :foreground "white")

  (set-face-attribute 'window-divider nil :foreground "gray20")
  (set-face-attribute 'window-divider-first-pixel nil :foreground "black")
  (set-face-attribute 'window-divider-last-pixel nil :foreground "black")
)

(setq ./theme-type (symbol-name (frame-parameter nil 'background-mode)))
(setq ./cursor-color (if (string= ./theme-type "dark") "white" "black"))
(set-face-attribute 'cursor nil :background ./cursor-color)
;; TODO: Do this for window divider and corfu UI items, and magit diff backgrounds.
;; and org-link

(setq tab-bar-auto-width-max nil)

(setq tab-bar-new-tab-choice "*scratch*")

;; FIXME: Doesn't work
(set-face-attribute 'tab-bar-tab-inactive nil :background "textBackgroundColor")

(use-package elpher)

(use-package visual-fill-column
  :init
  (setq-default visual-fill-column-center-text t))

(use-package imenu-list
  :config
  (setq imenu-list-auto-resize t)
  ;; Auto-update Ilist buffer
  :hook (imenu-list-major-mode . (lambda ()
                                   (imenu-list-minor-mode 1)
                                   (visual-line-mode 1)  ;; REVIEW
                                   (display-line-numbers-mode -1)
                                   )))

(use-package math-symbol-lists
  :after cape
  :config
  ;; This is actually for C-\, then select input "math",
  ;; then the Ω will show in the status bar.
  (quail-define-package "math" "UTF-8" "Ω" t)
  ;; (quail-define-rules ; add whatever extra rules you want to define here...
  ;;  ("\\from"    #X2190)
  ;;  ("\\to"      #X2192)
  ;;  ("\\lhd"     #X22B2)
  ;;  ("\\rhd"     #X22B3)
  ;;  ("\\unlhd"   #X22B4)
  ;;  ("\\unrhd"   #X22B5))
  (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
        (append math-symbol-list-basic math-symbol-list-extended))
  )

(use-package dired
  :elpaca nil
  :config
  (setq delete-by-moving-to-trash t
        ;; Emacs 29
        dired-make-directory-clickable t
        dired-mouse-drag-files t
        )
)

(use-package wrap-region
  :config
  (wrap-region-add-wrappers
   '(("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode org-mode))
     ("=" "=" nil (org-mode))
     ("~" "~" nil (org-mode))
     ("*" "*" nil (markdown-mode org-mode))))
  :hook
  ((org-mode markdown-mode) . wrap-region-mode)
)

(use-package magit)

(use-package breadcrumb
  :diminish breadcrumb-mode
  :init
  (breadcrumb-mode 1))

(use-package vertico
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :hook
  ;; For find-file, remove old file path if I start typing a new one
  ('rfn-eshadow-update-overlay-hook . #'vertico-directory-tidy)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))
;; Pasted from vertico
(use-package emacs
  :elpaca nil
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
  (setq enable-recursive-minibuffers t)
  ;; From corfu
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
)

(use-package marginalia
  :diminish
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode 1)
)

(use-package consult
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-s") 'isearch-forward)
  (global-set-key (kbd "C-s") 'isearch-forward)
  (global-set-key (kbd "C-c g") 'consult-org-heading)
  (global-set-key (kbd "C-x C-b") 'consult-buffer)
  ;; Doesn't work?
  (global-set-key (kbd "C-t") 'consult-buffer)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region)
  )

(use-package corfu
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  ;; Default is M-SPC, if M-SPC is bound like I have on my Mac (Alfred) S-M-SPC also works
  ;;(corfu-separator ?\s) ;; Orderless separator
  ;; separator: Quit at boundary if no `corfu-separator' inserted
  (corfu-quit-at-boundary 'separator)
  ;; separator: only stay alive if no match and `corfu-separator' inserted
  (corfu-quit-no-match 'separator)
  ;; Don't change what I typed to what I selected when previewing completions
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  ;; Default = #'insert. Options: quit, nil
  ;;(corfu-on-exact-match nil)
  ;; Prevent last/first item being hidden behind windows
  ;; FIXME: Doesn't work
  (corfu-scroll-margin 2)
  (corfu-right-margin-width 2)

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; FIXME: doesn't work: evil insert/emacs keybinds takes higher precendence it seems
  (define-key corfu-map (kbd "<escape>") 'corfu-quit)

  :custom-face
  (corfu-border ((t (:background "gray20" :weight bold))))
  (corfu-default ((t (:inherit fixed-pitch))))

  :init
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)

  :config
  (setq corfu-bar-width 0.8)
  (set-face-attribute 'corfu-bar nil :background ./corfu-bar-color)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay '(0 . 0)) ;; Use popupinfo in minibuffer too, why not?
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (setq corfu-popupinfo-delay '(0 . 0))
)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-mapping ;; These are fetched (and cached) from pictogrammers.com/library/mdi
    '((array "ar" :icon "code-brackets" :face font-lock-type-face)
      (boolean "b" :face font-lock-builtin-face)
      (class "C" :face font-lock-type-face) ;; family-tree could be used. but too dense
      (color "#" :icon "palette" :face success)
      (command ">_" :face default)
      (constant "cn" :icon "lock-remove-outline" :face font-lock-constant-face)
      (constructor "C+" :icon "plus-circle-multiple" :face font-lock-function-name-face)
      (enummember "em" :icon "order-bool-ascending-variant" :face font-lock-builtin-face)
      (enum-member "em" :icon "order-bool-ascending-variant" :face font-lock-builtin-face)
      (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
      (event "ev" :icon "lightning-bolt-outline" :face font-lock-warning-face)
      (field "fd" :face font-lock-variable-name-face)
      (file "F" :icon "file-document-outline" :face font-lock-string-face)
      (folder "D" :icon "folder" :face font-lock-doc-face)
      (interface "if" :icon "application-brackets-outline" :face font-lock-type-face)
      (keyword "kw" :face font-lock-keyword-face)
      (macro "mc" :icon "lambda" :face font-lock-keyword-face)
      (magic "ma" :icon "shimmer" :face font-lock-builtin-face)
      (method "me" :face font-lock-function-name-face)
      (function "f" :icon "function" :face font-lock-function-name-face)
      (module "mo" :icon "package-variant-closed" :face font-lock-preprocessor-face)
      (numeric "0" :icon "numeric" :face font-lock-builtin-face)
      (operator "÷" :icon "division" :face font-lock-comment-delimiter-face)
      (param "pa" :icon "cog-outline" :face default)
      (property "pr" :icon "wrench" :face font-lock-variable-name-face)
      (reference "rf" :icon "library" :face font-lock-variable-name-face)
      (snippet "S" :face font-lock-string-face)
      (string "\"" :icon "text-box" :face font-lock-string-face)
      (struct "{}" :icon "code-braces" :face font-lock-variable-name-face)
      (text " " :face font-lock-doc-face) ; text-short could be used
      (typeparameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
      (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
      (unit "u" :icon "square-rounded-outline" :face font-lock-constant-face)
      (value "vl" :icon "plus-circle-outline" :face font-lock-builtin-face)
      (variable "v" :face font-lock-variable-name-face)
      (t "?" :face font-lock-warning-face)))
    (kind-icon-blend-background nil)
  :custom-face
  (kind-icon-default-face ((t (:background nil))))

  :config
  ;;(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;;:after math-symbol-lists
  :config
  ;; Add useful defaults completion sources from cape
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-emoji)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; (define-key evil-insert-state-map (kbd "C-x C-f") 'cape-file)
  ;; (define-key evil-insert-state-map (kbd "C-x C-d") 'cape-dict)
  ;; (define-key evil-insert-state-map (kbd "C-x C-w") 'cape-dabbrev)
  ;; (define-key evil-insert-state-map (kbd "C-x C-:") 'cape-emoji)

  (cape-char--define math "math" ?\\)
  (add-to-list 'completion-at-point-functions #'cape-math)
  ;; (define-key evil-insert-state-map (kbd "C-x C-$") 'cape-math)

  :hook (eshell-mode-hook . (lambda () (setq-local corfu-quit-at-boundary t
                                                   corfu-quit-no-match t
                                                   corfu-auto nil)
                              (corfu-mode)))

)

(use-package corfu-candidate-overlay
  :config
  (corfu-candidate-overlay-mode 1) ;; This is global
  (set-face-attribute 'corfu-candidate-overlay-face nil :foreground "dim grey")
  ;; Use TAB to accept a completion, how cool is that!
  (defun ./insert-state-tab ()
    "Handle TAB key in insert state.
  
  If corfu-candidate-overlay's overlay is active, calls
  `corfu-candidate-overlay--get-overlay-property', otherwise
  `evil-toggle-fold'. See my packages.org for this section for why I
  didn't use `org-cycle' here."
    (interactive)
    (if (overlayp corfu-candidate-overlay--overlay)
        (progn
          ;; This check is taken exactly from the implementation of
          ;; `corfu-candidate-overlay-complete-at-point's (as of
          ;; writing).
          (corfu-candidate-overlay--show)
          (if (and (overlayp corfu-candidate-overlay--overlay)
                   (not (string= (corfu-candidate-overlay--get-overlay-property 'after-string) "")))
              (corfu-candidate-overlay-complete-at-point)
            (if (string-match-p
                 "^\*+ "
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
            (evil-toggle-fold)
            (indent)
                )
            ))
            (if (string-match-p
                 "^\*+ "
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
            (evil-toggle-fold)
            (indent-for-tab-command)
      )))
  :bind ("TAB" . './insert-state-tab)
)

(use-package which-key
  :diminish
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package org
  :elpaca nil
  :config
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-list-indent-offset 2)

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "deep sky blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-block-begin-line ((t (:inherit (font-lock-comment-face fixed-pitch)))) t)
   '(org-block-end-line ((t (:inherit (font-lock-comment-face fixed-pitch)))) t)
   '(org-drawer ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (lua . t)
     (js . t)))

  (defun org-babel-execute:nim (body params)
    "Execute a block of Nim code with org-babel."
    (let ((in-file (org-babel-temp-file "n" ".nim"))
          (verbosity (or (cdr (assq :verbosity params)) 0)))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "nim compile --verbosity=%d --run %s" verbosity
               (org-babel-process-file-name in-file))
       "")))

  (defun org-babel-execute:moonscript (body params)
    "Execute a block of MoonScript code with org-babel."
    (let ((in-file (org-babel-temp-file "m" ".moon")))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "moon %s" (org-babel-process-file-name in-file)) "")))

  :hook
  (org-mode . (lambda () (visual-line-mode 1)
                (variable-pitch-mode)
                (display-line-numbers-mode -1)))
  )

(use-package org-superstar
  :config
  (setq org-superstar-configure-like-org-bullets t)
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-babel-safelist '("~/.config/emacs/packages.org"
                                         "~/.config/emacs/init.org")))

(use-package lua-mode)
(use-package moonscript)
(use-package nim-mode)

(use-package eglot
  :elpaca nil
  :defer t
  :hook
  ((python-ts-mode go-ts-mode lua-mode) . eglot-ensure)
)

;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'auto-mode-alist
             '("\\.go\\'" . (lambda ()
                               (go-ts-mode)
                               )))
(add-to-list 'auto-mode-alist
             '("go.mod\\'" . (lambda ()
                               (go-mod-ts-mode)
                               )))

;; FIXME
(use-package diminish
  :config
  (diminish 'buffer-face-mode)
  (diminish 'org-auto-tangle-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'visual-line-mode)
  (diminish 'org-indent-mode)
  (diminish 'subword-mode)
  )

(defun ./eshell-fn-on-files (fun1 fun2 args)
  "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
  (unless (null args)
    (let ((filenames (flatten-list args)))
      (funcall fun1 (car filenames))
      (when (cdr filenames)
        (mapcar fun2 (cdr filenames))))
    ;; Return an empty string, as the return value from `fun1'
    ;; probably isn't helpful to display in the `eshell' window.
    ""))

(defun eshell/ff (&rest files)
  "find-file on first arg, find-file-other-window on rest"
  (./eshell-fn-on-files 'find-file 'find-file-other-window files))

(defun eshell/f (&rest files)
  "Edit one or more files in another window."
  (./eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

(defalias 'eshell/emacs 'eshell/ff)
(defalias 'eshell/vi 'eshell/ff)
(defalias 'eshell/vim 'eshell/ff)
(defalias 'eshell/nv 'eshell/ff)
(defalias 'eshell/nvim 'eshell/ff)

(defun eshell/less (&rest files)
  "view-file-other-window"
  (view-file-other-window files))

(defalias 'eshell/more 'eshell/less)

(defun ./eshell-exit-with-window ()
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after './eshell-exit-with-window)

(defun eshell/do (&rest args)
  "Execute a command sequence over a collection of file elements.
Separate the sequence and the elements with a `::' string.
For instance:

    do chown _ angela :: *.org(u'oscar')

The function substitutes the `_' sequence to a single filename
element, and if not specified, it appends the file name to the
command. So the following works as expected:

    do chmod a+x :: *.org"
  (seq-let (forms elements) (-split-on "::" args)
    (dolist (element (-flatten (-concat elements)))
      (message "Working on %s ... %s" element forms)
      (let* ((form (if (-contains? forms "_")
                       (-replace "_" element forms)
                     (-snoc forms element)))
             (cmd  (car form))
             (args (cdr form)))
        (eshell-named-command cmd args)))))

(defun ./eshell--buffer-from-dir (dir)
  "Return buffer name of an Eshell based on DIR."
  (format "*eshell: %s*"
          (thread-first dir
                        (split-string "/" t)
                        (last)
                        (car))))

(defun ./eshell-there (parent)
  "Open an eshell session in a PARENT directory.
The window is smaller and named after this directory.
If an Eshell is already present that has been named
after PARENT, pop to that buffer instead."
  (if-let* ((term-name (./eshell--buffer-from-dir parent))
            (buf-name  (seq-contains (buffer-list) term-name
                                     (lambda (a b) (string-equal (buffer-name b) a)))))
      (pop-to-buffer buf-name)

    (let* ((default-directory parent)
           (height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)
      (setq eshell-buffer-name term-name)
      (eshell))))

(defun ./eshell-here ()
  "Opens a new shell in the directory of the current buffer.
Renames the eshell buffer to match that directory to allow more
than one eshell window."
  (interactive)
  (./eshell-there (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory)))

(bind-key "C-`" './eshell-here)

(defun ./eshell-send (command &optional dir)
  "Send COMMAND to the Eshell buffer named with DIR.
  The Eshell may have moved away from the directory originally
  opened with DIR, but it should have the name of the buffer.
  See `eshell--buffer-from-dir'."
  (interactive "sCommand to Send: ")
  (unless dir
    (setq dir (projectile-project-root)))
  (save-window-excursion
    (eshell-there dir)
    (goto-char (point-max))
    (insert command)
    (eshell-send-input)))

(defun ./execute-command-on-file-buffer (cmd)
  "Executes a shell command, CMD, on the current buffer's file.
Appends the filename to the command if not specified, so:

    chmod a+x

Works as expected. We replace the special variable `$$' with the
filename of the buffer. Note that `eshell-command' executes this
command, so eshell modifiers are available, for instance:

    mv $$ $$(:r).txt

Will rename the current file to now have a .txt extension.
See `eshell-display-modifier-help' for details on that."
  (interactive "sExecute command on File Buffer: ")
  (let* ((file-name (buffer-file-name))
         (full-cmd (cond ((string-match (rx "$$") cmd)
                          (replace-regexp-in-string (rx "$$") file-name cmd))
                         ((and file-name (string-match (rx (literal file-name)) cmd))
                          cmd)
                         (t
                          (concat cmd " " file-name)))))
    (message "Executing: %s" full-cmd)
    (eshell-command full-cmd)))

(use-package eshell
  :elpaca nil
  :init
  (setq eshell-error-if-no-glob t
        ;; This jumps back to the prompt:
        eshell-scroll-to-bottom-on-input 'all
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t

        ;; Since eshell starts fast, let's dismiss it on exit:
        eshell-kill-on-exit t
        eshell-destroy-buffer-when-process-dies t

        ;; Parameter differences could be hard to remember. Maybe next time
        eshell-prefer-lisp-functions nil))

(use-package eat
  :config
  (define-key eat-mode-map (kbd "C-c C-d") #'eat-self-input)
  ;; :hook
  ;; (eshell-mode . #'eat-eshell-mode)
  )

(provide 'packages)
