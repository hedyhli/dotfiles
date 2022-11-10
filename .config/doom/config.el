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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
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
(setq doom-themes-treemacs-variable-pitch-face nil)
