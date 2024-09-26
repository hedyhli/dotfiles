;;; init-highlight.el --- Highlighting stuff
;;; Commentary:
;; Highlighting symbols, indentation, brackets, TODO/REVIEW/FIXME/HACK/XXX

;;; Code:

;; TODO: do I need hl-line package??

;; Highlight indentions
(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  (highlight-indent-guides-mode
   .
   ;; FIXME: No clue why setting this in config doesn't work.
   (lambda () (set-face-attribute 'highlight-indent-guides-character-face nil
                                   :foreground "gray30")
              (set-face-attribute 'highlight-indent-guides-top-character-face nil
                                   :foreground "gray70" :weight 'bold)))

  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t)

  :config
  (with-no-warnings
    ;; Don't display first level of indentation
    (defun my-indent-guides-for-all-but-first-column (level responsive display)
      (unless (< level 1)
        (highlight-indent-guides--highlighter-default level responsive display)))
    (setq highlight-indent-guides-highlighter-function
          #'my-indent-guides-for-all-but-first-column)

    ;; Disable in `macrostep' expanding
    (with-eval-after-load 'macrostep
      (advice-add #'macrostep-expand
                  :after (lambda (&rest _)
                           (when highlight-indent-guides-mode
                             (highlight-indent-guides-mode -1))))
      (advice-add #'macrostep-collapse
                  :after (lambda (&rest _)
                           (when (derived-mode-p 'prog-mode 'yaml-mode)
                             (highlight-indent-guides-mode 1)))))

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defun my-ivy-cleanup-indentation (str)
        "Clean up indentation highlighting in ivy minibuffer."
        (let ((pos 0)
              (next 0)
              (limit (length str))
              (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str))))))
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))))


;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :bind (:map special-mode-map
         ("w" . rainbow-mode))
  :hook ((html-mode php-mode) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1)
  (dolist (keyword '("REVIEW" "HACK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))
  :bind (:map hl-todo-mode-map
         ([C-f3] . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  ;;:hook (after-init . global-hl-todo-mode)
)
 (provide 'init-highlight)
;;; init-highlight.el ends here
