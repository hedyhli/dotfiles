(setq user-full-name "hedy"
      user-mail-address "hedy@tilde.cafe")

(setq diplay-line-numbers-type t)

(setq confirm-kill-emacs nil)

(setq scroll-margin 6)

(global-visual-line-mode t)

(setq evil-shift-width 2)
(setq tab-width 4)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string "[ \t\n]*$" ""
          (shell-command-to-string ;;SHELL = fish
           "$SHELL --login -c 'string join : $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
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

(define-key ctl-x-4-map "t" 'toggle-window-split)

(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 18 :weight 'light))

(setq doom-theme 'doom-vibrant) ;; Doom's dracula is a bit funny

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; don't put deleted strings to X11 clipboard
(setq select-enable-clipboard nil)
;; copying and pasting selected blocks in visual mode to and from X11 clipboard
(map! "S-C-c" #'clipboard-kill-ring-save)
(map! "S-C-v" #'clipboard-yank)

(setq org-directory "~/org/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))  ;; Why jupyter when you have this JK

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(after! treemacs
  (setq treemacs-width 20
        treemacs-project-follow-cleanup t)
  (treemacs-load-theme "Default"))

;; Elfeed: Use sans for articles
(add-hook 'elfeed-show-mode-hook
      (lambda () (buffer-face-set 'variable-pitch)))

(add-hook! 'vterm-mode-hook 'evil-insert)

(after! company
  (setq company-idle-delay nil))

(require 'math-symbol-lists)
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

(add-to-list 'company-backends 'company-math-symbols-unicode)

;; Emoji completion
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)
(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)

(use-package! wrap-region
  :config
  (wrap-region-add-wrappers
   '(("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode org-mode))
     ("=" "=" nil (org-mode))
     ("~" "~" nil (org-mode))
     ("*" "*" nil (markdown-mode org-mode)))))

(add-hook! ('org-mode 'markdown-mode) 'wrap-region-mode)

(use-package! org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/orgroam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(customize-set-variable 'org-anki-default-deck "Doom")

(defun my/org-anki-sync-fix-refs ()
  "Fix 'Unable to resolve link: XXX'"
  (interactive)
  (org-id-update-id-locations
   (directory-files-recursively org-roam-directory "\\.org$")))

(customize-set-variable 'org-anki-field-templates
 '(("Basic"
    ("Front" . (lambda (it)        ;; strip text-properties
                (let ((breadcrumb (substring-no-properties
                                   ;; arguments:
                                   ;; - prepend file name?
                                   ;; - incl current heading?
                                   ;; - SEPARATOR
                                   ;; - return as string?
                                   (org-display-outline-path nil nil " > " t)))
                      (path (buffer-file-name)))
                  ;; "File Title: Heading > SubHeading > Sub-subheading"
                  (setq breadcrumb (concat (org-get-title) ": " breadcrumb))
                  (concat
                   (format
                    (concat "<div id=\"meta\">"
                              "<p id=\"breadcrumb\">%s</p>"
                              "<p id=\"open-file\">"
                                "<a href='emacs:///%s'>Open file</a>"
                              "</p>"
                            "</div>"
                            "<h3>%s</h3>")
                    breadcrumb path it (org-get-heading)))))))))

(setq org-latex-create-formula-image-program 'dvisvgm)
(plist-put org-format-latex-options :scale 4.0)
(plist-put org-format-latex-options :background nil)

(defun my/scale-current-overlay ()
  (interactive)
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (- (point) 10) (+ (point) 5) ))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          'margin 'right-margin)))
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale 7.0))))

(defun list-overlays-at (&optional pos)
      "Describe overlays at POS or point."
      (interactive)
      (setq pos (or pos (point)))
      (let ((overlays (overlays-at pos))
            (obuf (current-buffer))
            (buf (get-buffer-create "*Overlays*"))
            (props '(priority window category face mouse-face display
                     help-echo modification-hooks insert-in-front-hooks
                     insert-behind-hooks invisible intangible
                     isearch-open-invisible isearch-open-invisible-temporary
                     before-string after-string evaporate local-map keymap
                     field))
            start end text)
        (if (not overlays)
            (message "None.")
          (set-buffer buf)
          (erase-buffer)
          (dolist (o overlays)
            (setq start (overlay-start o)
                  end (overlay-end o)
                  text (with-current-buffer obuf
                         (buffer-substring start end)))
            (when (> (- end start) 13)
              (setq text (concat (substring text 1 10) "...")))
            (insert (format "From %d to %d: \"%s\":\n" start end text))
            (dolist (p props)
              (when (overlay-get o p)
                (insert (format " %15S: %S\n" p (overlay-get o p))))))
          (pop-to-buffer buf))))

(defun eshell-fn-on-files (fun1 fun2 args)
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
  (eshell-fn-on-files 'find-file 'find-file-other-window files))

(defun eshell/f (&rest files)
  "Edit one or more files in another window."
  (eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

(defalias 'eshell/emacs 'eshell/ff)
(defalias 'eshell/vi 'eshell/ff)
(defalias 'eshell/vim 'eshell/ff)
(defalias 'eshell/nv 'eshell/ff)
(defalias 'eshell/nvim 'eshell/ff)

(defun eshell/less (&rest files)
  "view-file-other-window"
  (view-file-other-window files))

(defalias 'eshell/more 'eshell/less)

(defun my/eshell-exit-with-window ()
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'my/eshell-exit-with-window)

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

(defun eshell--buffer-from-dir (dir)
  "Return buffer name of an Eshell based on DIR."
  (format "*eshell: %s*"
          (thread-first dir
                        (split-string "/" t)
                        (last)
                        (car))))

(defun eshell-there (parent)
  "Open an eshell session in a PARENT directory.
The window is smaller and named after this directory.
If an Eshell is already present that has been named
after PARENT, pop to that buffer instead."
  (if-let* ((term-name (eshell--buffer-from-dir parent))
            (buf-name  (seq-contains (buffer-list) term-name
                                     (lambda (a b) (string-equal (buffer-name b) a)))))
      (pop-to-buffer buf-name)

    (let* ((default-directory parent)
           (height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)
      (setq eshell-buffer-name term-name)
      (eshell))))

(defun eshell-here ()
  "Opens a new shell in the directory of the current buffer.
Renames the eshell buffer to match that directory to allow more
than one eshell window."
  (interactive)
  (eshell-there (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory)))

(bind-key "C-`" 'eshell-here)

(defun ha-eshell-send (command &optional dir)
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

(defun execute-command-on-file-buffer (cmd)
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

(use-package! eshell
  ;;:type built-in
  ;;:custom (eshell-banner-message '(ha-eshell-banner))

  :init
  (setq eshell-error-if-no-glob t
        ;; This jumps back to the prompt:
        eshell-scroll-to-bottom-on-input 'all
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t

        ;; Since eshell starts fast, let's dismiss it on exit:
        eshell-kill-on-exit t
        eshell-destroy-buffer-when-process-dies t

        ;; Can you remember the parameter differences between the
        ;; executables `chmod' and `find' and their Emacs counterpart?
        ;; Me neither, so this makes it act a bit more shell-like:
        eshell-prefer-lisp-functions nil))
