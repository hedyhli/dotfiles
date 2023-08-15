;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; No need to run doom sync after modifying this file
(setq user-full-name "hedy"
      user-mail-address "hedy@tilde.cafe")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 18 :weight 'light))

;; Elfeed: Use sans for articles
(add-hook 'elfeed-show-mode-hook
      (lambda () (buffer-face-set 'variable-pitch)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant) ;; Doom's dracula is a bit funny
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; Can set to relative or nil
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; FIXING EVIL DON"T USE CLIPBOARD::
;;(fset 'evil-visual-update-x-selection 'ignore)
;; FIXME DOES NOT WORK!
(setq evil-visual-update-x-selection-p nil)
;;(setq evil-kill-on-visual-paste nil)
;;
;; XXX: Workaround from: https://discourse.doomemacs.org/t/how-to-set-up-clipboard/3742
;; don't put deleted strings to X11 clipboard
(setq select-enable-clipboard nil)
;; copying and pasting selected blocks in visual mode to and from X11 clipboard
(map! "S-C-c" #'clipboard-kill-ring-save)
(map! "S-C-v" #'clipboard-yank)
;; How to use yank/paste and system clipboard:
;; - Anything copied outside of emacs, paste in emacs with S-C-v
;; - Yank within emacs, will not override clipboard outside emacs
;; - To paste yanks within emacs, use default paste bind or use p
;; - Copy sth to clipboard from emacs: Use S-C-c, paste outside with normal
;;   system bind

;; Seriously? I just want to quit. Damn. Why confirm lol
(setq confirm-kill-emacs nil)

(after! treemacs
  (setq treemacs-width 20
        treemacs-project-follow-cleanup t)
  (treemacs-load-theme "Default"))

(after! magit
  (custom-set-faces
 '(magit-diff-added-highlight ((((type tty)) (:background nil))))
 '(magit-diff-context-highlight ((((type tty)) (:background nil))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:background nil))))
 '(magit-section-highlight ((((type tty)) nil)))
 '(magit-diff-highlight-hunk-body ((((type tty)) (:background nil))))
 '(magit-diff-base-highlight ((((type tty)) (:background nil))))
 ))

;; Scroll margin like scrolloff in vim
(setq scroll-margin 6)
;;(setq doom-themes-treemacs-variable-pitch-face nil)

;; Horizontal scrolling is pain
(global-visual-line-mode t)

;; Give it the IDE vibes
; (run-with-timer 1 nil (lambda () (save-selected-window (treemacs))))

;;org-todo
(setq org-todo-keyword-faces
      '(("NOW" . "labelColor") ("CANCELED" . "systemRedColor") ("DONE" . "selectedControlColor") ("FINISH" . "selectedControlColor") ("PAST" . "controlTextColor")
        ("RECUR" . "systemYellowColor") ("MARK" . "systemOrangeColor") ("PLAN" . "systemBrownColor")
        ("OVERDUE". "systemRedColor") ("DUE" . "systemYellowColor") ("STARTED" . "labelColor")))
(setq diary-file "~/Documents/diary/diary")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

;; centaur tabs
(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.  All buffer name start
with * will group to \"Emacs\".  Other buffer group by
`centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
    ;; "Remote")
    ;; Put org mode first since some of these begin with "*"
    ;; elisp seems to short-circuit "or"
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")   ;; Org Agenda next to org files is useful
    ((derived-mode-p 'vterm-mode)
     "VTerm")
    ((or (derived-mode-p 'prog-mode) ;; Put magit along with program files
      (memq major-mode '(magit-process-mode
                         magit-status-mode
                         magit-diff-mode
                         magit-log-mode
                         magit-file-mode
                         magit-blob-mode
                         magit-blame-mode))
      (derived-mode-p 'dired-mode))
     "Dev")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")
    ((or (string-equal "elfeed" (substring (buffer-name) 1 7))
         (string-equal "eww" (substring (buffer-name) 1 4))
         (string-equal "elpher" (substring (buffer-name) 1 7)))
     "E-apps")
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs") ;; This should be at the very end to match ones where none of the
              ;; previous mode matches failed
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string ;;SHELL = fish
					  "$SHELL --login -c 'string join : $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(add-hook 'vterm-mode-hook
      (lambda () (visual-line-mode) (centaur-tabs-local-mode) (turn-off-evil-mode)))
