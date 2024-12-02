;; NOTE:
;; This file is generated from init.org

(defvar ./variable-pitch-display-font
  "Fira Sans"
  "Font used for titles")

(defvar ./face-fixed-pitch
  '(:foundry "apple" :family "IBM PLex Mono" :height 160)
  "Contents to be passed to (`set-face-attribute' 'fixed-pitch nil [...]) ")
(defvar ./face-variable-pitch
  '(:foundry "apple" :family "Inter" :height 170)
  "Contents to be passed to (`set-face-attribute' 'variable-pitch nil [...]) ")

(load (locate-user-emacs-file "dotslash-local-pre.el") :no-error :no-message)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defun display-startup-echo-area-message ()
  (message (format "Emacs %s loaded from %s in %s"
            emacs-version user-emacs-directory (emacs-init-time))))

(setq initial-scratch-message
      (concat ";; *SCRATCH*\n"
              ";; C-c C-b (eval-buffer)  C-c C-r (eval-region)\n"
              ";; C-x C-s will let you choose where to save\n\n; "))

;; Setting this in early-init doesn't seem to work
(set-frame-position (selected-frame) 100 10)
(set-face-attribute 'default nil :height 160)

(add-to-list 'bdf-directory-list "~/Library/Fonts")

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:inherit ./face-variable-pitch))))
;;  '(fixed-pitch ((t (:inherit ./face-fixed-pitch)))))

(apply #'set-face-attribute 'variable-pitch nil ./face-variable-pitch)
(apply #'set-face-attribute 'fixed-pitch nil ./face-fixed-pitch)
(apply #'set-face-attribute 'default nil ./face-fixed-pitch)

;;(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))

;; I think this saves the last cursor pos or something
;; FIXME: doesn't work
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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
(winner-mode 1)
(delete-selection-mode 1) ;; Select and type would replace the selected text

;; IDK
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

(add-hook 'elisp-mode-hook
          (lambda ()
            ;; These are never used...
            ;; (local-set-key (kbd "C-c C-x") #'ielm)
            ;; (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-r") 'eval-region)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

(setq window-divider-default-right-width 8)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(defun ./org-insert-src (beg end)
  "Insert (or wrap region with, or cancel) src block.

If cursor currently on an empty line, it is wrapped in src block, with
no other content in the src block other than the empty line, then the
src block together with the line after it (if empty) is deleted. This
undoes the effect of ./org-insert-src without active region, and
cancelling org-edit-src with C-c C-k.

Code type is prompted and `org-edit-src-code' called only for insert,
not for wrap.

Uses 'elisp' if currently in user-emacs-directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (let ((selection (buffer-substring-no-properties beg end))
        (code-type '(if (string=
                         (file-truename user-emacs-directory)
                         (file-name-directory (buffer-file-name)))
                        "elisp"
                      (read-from-minibuffer "#+BEGIN_SRC "))))
    (if (< (length selection) 2)
        (if (./org-empty-src-p)
            ;; Delete empty src block and exit
            (progn
              (previous-line)
              (delete-line) ;; Newline also deleted
              (delete-line)
              (delete-line)
              ;; Delete empty line inserted by ./org-insert-src itself
              (if (./match-line-p "")
                  (delete-line))) 
          ;; Otherwise:
          ;; Insert src block with given code type and edit
          (progn
            (setq code-type (eval code-type))
            (deactivate-mark)
            (beginning-of-line)
            (newline)
            (insert (format "#+BEGIN_SRC %s\n" code-type))
            (newline)
            (insert "#+END_SRC\n")
            (previous-line 2)
            (org-edit-src-code)))
      ;; Wrap selected region
      ;;(setq code-type (eval code-type))
      (goto-char beg)
      (previous-line) (end-of-line)
      (newline)
      (insert "#+BEGIN_SRC ")
      ;; save-excursion doesn't seem to work here
      (goto-char (+ end 11)) (end-of-line)
      (newline)
      (insert "#+END_SRC")
      ;; FIXME: putting cursor at the begin src part afterwards doesn't work
      (re-search-backward "^\\s-*#\\+BEGIN_SRC")
      (end-of-line))))

(defun ./match-line-p (regexp &optional move keep-pos start)
  "Wrapper around string-match-p to use contents of current line.

Returns whether current line, after moving down by MOVE lines, can be
matched with REGEXP.

If REGEXP is an empty string, return t for empty line, nil otherwise.
MOVE argument is passed to `next-line'.

If REGEXP and is non-nil, REGEXP and START is passed to
`string-match-p' with no changes, and its return-value is returned
as-is.

MOVE argument is passed as-is to `next-line' immediately.

If KEEP-POS is non-nil, pass MOVE argument to `previous-line' after obtaining
contents of the required line."
  (next-line move)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if keep-pos
        (previous-line move))
    (if (= (length regexp) 0)
        (if (= (length line) 0)
            t nil)
      (string-match-p regexp line start))))

(defun ./org-empty-src-p ()
  "Return whether point is within a org src block with a single empty line."
  (let ((line))
    (save-excursion
      (if (./match-line-p "^\\s-*#\\+begin_src" -1)
          (if (./match-line-p "" +1)
              (if (./match-line-p "^\\s-*#\\+end_src" +1)
                  t nil) nil) nil))))

(defun ./org-split-src ()
  "Split current src block into two blocks at point.

Retains src properties."
  (interactive)
  (insert "#+END_SRC\n\n")
  (save-excursion
    (re-search-backward "^\\s-*#\\+begin_src")
    (defvar beginsrc (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (insert beginsrc)
  (previous-line))

(defun ./load-directory (dir)
  "Load *.el files in a given directory"
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun ./load-file-if-exists (file)
  "Same as load-file but NOP if file does not exist"
  (if (file-exists-p file)
      (load-file file)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

(dolist (path '("dotslash-lisp" "dotslash-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))
(require 'packages)
(require 'init-highlight)

(load (locate-user-emacs-file "dotslash-local-post.el") :no-error :no-message)
