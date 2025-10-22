;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'dired-x)
(require 'notifications)

(add-to-list 'load-path "~/.emacs.d/src")
(add-to-list 'exec-path "~/.local/bin" t)
(add-to-list 'exec-path "~/.rbenv/shims" t)
(add-to-list 'exec-path "~/go/bin" t)
(setenv "PATH" (concat "~/.local/bin:" "~/.rbenv/shims:" "~/go/bin:" (getenv "PATH")))

(recentf-mode 1)
(hl-line-mode 1)
(which-function-mode 1)

(setq-default flycheck-disabled-checkers '(python-pylint))
(setq-default electric-indent-inhibit t)
(setq-default default-tab-width 4)

(setq auth-source-debug t)
(setq auth-sources '("~/.authinfo.gpg"))

(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq bidi-inhibit-bpa t)
(setq company-show-quick-access t)
(setq create-lockfiles nil)
(setq epg-pinentry-mode 'loopback)
(setq mail-user-agent 'mu4e-user-agent)
(setq markdown-fontify-code-blocks-natively t)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5)
(setq scroll-margin 10)
(setq use-short-answers t)
(setq user-full-name "Robert Gibboni")
(setq user-mail-address "galileo@gmail.com")
(setq which-func-unknown "n/a")

(set-face-attribute 'default nil :height 86)

(use-package straight
  :custom
  (straight-use-package-by-default t)
  :init
  (straight-use-package '( vertico :files (:defaults "extensions/*")
                           :includes (vertico-buffer
                                      vertico-directory
                                      vertico-flat
                                      vertico-indexed
                                      vertico-mouse
                                      vertico-quick
                                      vertico-repeat
                                      vertico-reverse))))

(use-package dired
  :straight nil
  :bind (:map dired-mode-map ("<SPC>" . dired-view-file-other-window)))

(use-package dired-preview)

(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer)))

(use-package plstore
  :config
  (add-to-list 'plstore-encrypt-to "A09542E9140692F4"))

(use-package emacs-async
  :hook
  (dired-mode . dired-async-mode)
  :config (setq dired-async-message-function
                (lambda (text face &rest args)
                  (call-process "notify-send" nil 0 nil
                                "Emacs: dired-async"
                                (apply #'format text args))
                  (apply #'dired-async-mode-line-message text face args))))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package pretty-hydra)

;; Flexible text folding
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title "HideShow" :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("A" hs-toggle-all "toggle all")
     ;; ("a" hs-show-all "show all")
     ;; ("i" hs-hide-all "hide all")
     ("y" hs-toggle-hiding "toggle hiding")
     ("u" hs-cycle "cycle block")
     ("i" hs-show-block "show block")
     ("o" hs-hide-block "hide block")
     ("p" hs-hide-level "hide level"))
    "Move"
    (("h" beginning-of-line "←←")
     ("l" end-of-line "→→")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("." pager-page-down "↘")
     ("n" pager-page-up "↖")
     ("M-<" beginning-of-buffer "beg")
     ("M->" end-of-buffer "end"))))
  :bind (:map hs-minor-mode-map
              ("C-~" . hideshow-hydra/body)
              ("C-S-<escape>" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

(use-package pager)

(use-package vertico
  :demand
  :config
  (setq vertico-cycle t)
  ;; currently requires melpa version of vertico
  (setq vertico-preselect 'directory)
  :init
  (vertico-mode)
  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  :bind (:map vertico-map
              ("/" . #'my/vertico-insert)))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  :demand
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("C-c C-j" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; virtual buffers
  ;; b Buffers
  ;; SPC Hidden buffers
  ;; * Modified buffers
  ;; f Files (Requires recentf-mode)
  ;; r File registers
  ;; m Bookmarks
  ;; p Project
  ;; Custom other sources configured in consult-buffer-sources.
)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-gh
  :after consult)
(use-package consult-gh-embark
  :after consult-gh
  :config
  (consult-gh-embark-mode +1))
(use-package consult-gh-forge
  :after consult-gh
  :config
  (consult-gh-forge-mode +1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package consult-mu
  :straight (consult-mu :type git :host github :repo "armindarvish/consult-mu" :files (:defaults "extras/*.el"))
  :after (mu4e consult)
  :custom
  (consult-mu-args '((expand-file-name "build/mu/mu" (straight--repos-dir "mu"))))
  ;;maximum number of results shown in minibuffer
  (consult-mu-maxnum 200)
  ;;show preview when pressing any keys
  (consult-mu-preview-key 'any)
  ;;do not mark email as read when previewed. If you turn this to t, be aware that the auto-loaded preview if the preview-key above is 'any would also get marked as read!
  (consult-mu-mark-previewed-as-read nil)
  ;;mark email as read when selected.
  (consult-mu-mark-viewed-as-read t)
  ;;use reply to all when composing reply emails
  (consult-mu-use-wide-reply t)
  :config
  ;;create a list of saved searches for quick access using `histroy-next-element' with `M-n' in minibuffer. Note the "#" character at the beginning of each query! Change these according to
  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))
  ;; require embark actions for marking, replying, forwarding, etc. directly from minibuffer
  (require 'consult-mu-embark)
  ;; require extra module for composing (e.g. for interactive attachment) as well as embark actions
  (require 'consult-mu-compose)
  (require 'consult-mu-compose-embark)
  ;; require extra module for searching contacts and runing embark actions on contacts
  (require 'consult-mu-contacts)
  (require 'consult-mu-contacts-embark)
  ;; change the prefiew key for compose so you don't open a preview of every file when selecting files to attach
  (setq consult-mu-compose-preview-key "M-o")
  ;; pick a key to bind to consult-mu-compose-attach in embark-file-map
  (setq consult-mu-embark-attach-file-key "C-a")
  (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*" "CashApp" "[🚀⭐🎉🎁🎲🌻📦💰💌💘💲🚚🔻]" "^[A-Z\.]*@gmail.com$" "^[Gg][Aa].*@gmail.com$" "^[Gg]alileo.*" "autozone@em\.autozone\.com\.mx" "discoursemail\.com$" "docs\.google\.com$" "mg1\.substack\.com$" "onf\.ru$" "reply" "unsubscribe"))
  (setq consult-mu-contacts-ignore-case-fold-search t)
  (consult-mu-compose-embark-bind-attach-file-key)
  ;; choose if you want to use dired for attaching files (choice of 'always, 'in-dired, or nil)
  (setq consult-mu-compose-use-dired-attachment 'always))

(use-package goto-last-change)

(use-package avy
  :bind ("M-j" . avy-goto-char-timer)
  :config
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(use-package hydra)

(use-package vundo
  :bind (("C-x u" . vundo)))

(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-secondary-browser-function 'eww-browse-url)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))

(use-package multiple-cursors
  :custom (mc/insert-numbers-default 1))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  (:map magit-mode-map
        ("C-<tab>" . nil))
  :config
  (require 'magit-extras)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-margin '(t "%a, %b %d, %Y" magit-log-margin-width t 20))
  )

(use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode)
)

(use-package forge
  :straight (:type git :host github :repo "magit/forge" :branch "main")
  :after magit)

(use-package github-review
  :custom
  (define-key github-review-mode-map (kbd "M-o") nil))

(use-package git-link)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package olivetti
  :custom
  (olivetti-minimum-body-width 150)
  :hook
  (eww-mode . olivetti-mode)
  (markdown-mode . olivetti-mode)
  (org-mode . olivetti-mode))

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package nlinum
  :hook (prog-mode . nlinum-mode))

(use-package editorconfig)

(use-package jsonrpc)

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t) (jupyter . t) (sql . t)))
  (org-babel-python-command "python")
  (org-confirm-babel-evaluate nil)
  (org-export-with-sub-superscripts nil)
  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-goto-auto-isearch nil)
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-support-shift-select t)
  :pretty-hydra
  ((:title "Org" :color pink :quit-key ("q" "C-g"))
   ("Navigate"
    (
     ("n" outline-next-visible-heading "Next")
     ("j" outline-next-visible-heading "Next")
     ("p" outline-previous-visible-heading "Previous")
     ("k" outline-previous-visible-heading "Previous")
     ("u" outline-up-heading "Up level")
     ("f" org-forward-heading-same-level "Forward same level")
     ("l" org-forward-heading-same-level "Forward same level")
     ("h" org-backward-heading-same-level "Backward same level"))
    "Focus"
    (("M-s" org-narrow-to-subtree "Narrow to subtree")
     ("M-w" widen "Widen")
     ("/" org-tree-slide-mode :toggle t)
     ("," org-tree-slide-move-previous-tree "Previous tree")
     ("." org-tree-slide-move-next-tree "Next tree"))
    "Modify"
    (("<prior>" org-metaup "Move section up" :column "Modify")
     ("<next>" org-metadown "Move section down")
     ("<" org-promote-subtree "Promote")
     (">" org-demote-subtree "Demote")
     ("t" org-todo "Toggle TODO")
     ("$" org-archive-subtree "Archive")
     ("d" org-cut-subtree "Kill")
     ("a" org-insert-heading "Insert above")
     ("b" (org-insert-heading '(4)) "Insert below"))
    "Act"
    (("s" consult-outline "Search" :color red :column "Actions")
     ("w" org-copy-subtree "Copy")
     ("y" org-paste-subtree "Paste")
     ("I" org-clock-in "Clock in")
     ("O" org-clock-out "Clock in"))
    "Other"
    (("J" (progn
            (jupyter-hydra/body)
            (hydra-push '(org-hydra/body)))
      "Jupyter" :color blue)
     ("q" hydra-pop "exit" :color blue))))
  :bind
  (
   ("C-c C-j" . nil)
   ("C-c a" . org-agenda)
   ("<f6>" . org-capture)
   :map org-mode-map
   ("C-c h" . org-hydra/body)
   ("C-c C-s" . org-schedule))
  :config
  ;; Only evaluate this after org-agenda is loaded
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "N") #'org-agenda-do-date-later)
    (define-key org-agenda-mode-map (kbd "P") #'org-agenda-do-date-earlier)))

(use-package jupyter
  :straight (:type git :host github :repo "r-b-g-b/jupyter" :branch "remap-hydra")
  :after org
  :custom
  (jupyter-repl-echo-eval-p t)
  (org-babel-default-header-args:jupyter-python '((:async . "yes")))
  :config
  (defun my/org-babel-jupyter-handle-result-ansi-escapes ()
    "Handle ANSI escapes in Jupyter src-block result."
    (org-babel-map-src-blocks nil
      (when (org-babel-jupyter-language-p lang)
        (goto-char (org-babel-where-is-src-block-result))
        (ansi-color-apply-on-region (point) (org-babel-result-end)))))
  (add-hook 'org-babel-after-execute-hook #'my/org-babel-jupyter-handle-result-ansi-escapes)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (org-babel-jupyter-aliases-from-kernelspecs)
  (defun my/jupyter-execute-and-insert ()
    (interactive)
    (org-ctrl-c-ctrl-c)
    (jupyter-org-insert-src-block t current-prefix-arg))
  (defun my/jupyter-org-kill-block-and-results ()
    (interactive)
    (jupyter-org-kill-block-and-results)
    (org-babel-next-src-block))
  (defun my/jupyter-org-restart-kernel ()
    "Restart the kernel of the source block where point is."
    (interactive)
    (jupyter-org-with-src-block-client
     (jupyter-repl-restart-kernel)))
  (define-key jupyter-org-interaction-mode-map (kbd "C-c h") #'jupyter-org-hydra/body t)
  (remove-hook 'org-mode-hook #'org-babel-jupyter-make-local-aliases)
  (add-hook 'org-mode-hook #'org-babel-jupyter-make-local-aliases 10)
  :pretty-hydra
  ((:title "Jupyter" :color pink)
   ("Execute"
    (("e" jupyter-org-execute-and-next-block "Execute and advance")
     ("C-e" org-ctrl-c-ctrl-c "Execute and stay")
     ("M-e" my/jupyter-execute-and-insert "Execute and insert")
     ("C-M-e" jupyter-org-execute-subtree "Subtree to point")
     ("I" jupyter-org-interrupt-kernel "Interrupt")
     ("0" my/jupyter-org-restart-kernel "Restart kernel"))
    "Navigate"
    (
     ("p" org-babel-previous-src-block "Previous")
     ("k" org-babel-previous-src-block "Previous")
     ("P" jupyter-org-previous-busy-src-block "Previous busy")
     ("n" org-babel-next-src-block "Next")
     ("j" org-babel-next-src-block "Previous")
     ("N" jupyter-org-next-busy-src-block "Next busy")
     ("g" jupyter-org-jump-to-visible-block "Visible")
     ("G" jupyter-org-jump-to-block "Any")
     ("<" org-tree-slide-move-previous-tree "Previous tree")
     (">" org-tree-slide-move-next-tree "Next tree")
     ("/" org-tree-slide-mode :toggle t)
     ("," org-tree-slide-move-previous-tree "Previous tree")
     ("." org-tree-slide-move-next-tree "Next tree")
     ("<tab>" org-cycle "Toggle fold"))
    "Edit"
    (("<prior>" jupyter-org-move-src-block "Move up")
     ("<next>" (jupyter-org-move-src-block t) "Move down")
     ("d" my/jupyter-org-kill-block-and-results "Kill")
     ("w" jupyter-org-copy-block-and-results "Copy")
     ("o" (jupyter-org-clone-block t) "Clone")
     ("m" jupyter-org-merge-blocks "Merge")
     ("s" jupyter-org-split-src-block "Split")
     ("u" undo "Undo")
     ("a" (jupyter-org-insert-src-block nil current-prefix-arg) "Insert above")
     ("b" (jupyter-org-insert-src-block t current-prefix-arg) "Insert below")
     ("l" org-babel-remove-result "Clear result")
     ("L" jupyter-org-clear-all-results "Clear all results")
     ("h" jupyter-org-edit-header "Header"))
    "Misc"
    (("?" jupyter-org-inspect-src-block "Inspect")
     ("r" org-babel-hide-result-toggle "Toggle result")
     ("C-s" org-babel-jupyter-scratch-buffer "Scratch")
     ("i" org-toggle-inline-images "Toggle images")
     ("t" org-babel-tangle "Tangle")
     ("T" article-treat-ansi-sequences "Treat ANSI")
     ("q" hydra-pop "exit" :color blue))))

  :bind
  (:map jupyter-org-interaction-mode-map ("C-c j" . jupyter-hydra/body)))

(use-package quarto-mode
  :straight (:host github :repo "quarto-dev/quarto-emacs")
  :mode (("\\.qmd" . poly-quarto-mode))
  )

(use-package ob-async
  :after jupyter
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "jupyter-julia")))

(use-package ob-http
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(http . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-mermaid
  :after org
  :custom
  (ob-mermaid-cli-path "~/.local/bin/mmdc"))

(use-package ob-bigquery
  :straight (:type git :host github :repo "lhernanz/ob-bigquery"))

(use-package orgit
  :after org
  :straight (:host github
                   :repo "magit/orgit"
                   :files ("orgit.el")))

(use-package org-pandoc-import
  :straight (:host github
                   :repo "tecosaur/org-pandoc-import"
                   :files ("*.el" "filters" "preprocessors"))
  :custom
  (org-pandoc-import-markdown-args '(:wrap "preserve")))

(use-package org-msg
  :straight ( :host github
              :repo "jsilve24/org-msg"
              :branch "master")

  :after org
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi%s,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t
        org-msg-signature "

Best,

#+begin_signature
--
Robert
#+end_signature")
  (org-msg-mode))

(use-package org-pomodoro
  :after org
  :custom
  (org-pomodoro-audio-player "/usr/bin/play")
  (org-pomodoro-finished-sound-args "-v 0.2")
  (org-pomodoro-short-break-sound-args "-v 0.2"))

(use-package org-ref
  :after org
  :custom
  (bibtex-completion-bibliography '("~/org-roam/bibliography/references.bib"))
  (bibtex-completion-library-path '("~/org-roam/bibliography/bibtex-pdfs/"))
  (bibtex-completion-notes-path "~/org-roam/bibliography/notes/")
  (bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")

  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-display-formats
   '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))

  :config
  (require 'bibtex)

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)

  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  )

(use-package org-roam
  :config
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-notes-by-tag "project"))
    (add-to-list 'org-agenda-files "~/org-roam/schedule.org"))
  (my/org-roam-refresh-agenda-list)

  :custom
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %<%H:%M> %?"
      :target (file+datetree "log.org" week))))
  (org-roam-v2-ack t)
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/org-roam")
  (org-roam-graph-executable "neato")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project\n")
      :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n s" . org-store-link)
         :map org-mode-map
         ("C-M-i" . completion-at-point))

  :config
  (org-roam-db-autosync-mode)

  :commands (org-roam-node-list))

(use-package org-roam-bibtex
  :after (org-roam org-ref))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-sidebar
  :after org)

(use-package org-tree-slide
  :after org)

(use-package org-variable-pitch
  :after org)

(use-package ox-gfm
  :after org
  :config
  (require 'ox-gfm nil t))

(use-package ox-quarto
  :straight (:host github :repo "jrgant/ox-quarto")
  :after org
  )

(use-package ox-ipynb
  :straight ( :host github
              :repo "jkitchin/ox-ipynb"
              :files ("ox-ipynb.el"))
  :after org)

(use-package ox-pandoc
  :after org
  :custom
  (org-pandoc-options-for-gfm '(:wrap . "none"))
  (org-pandoc-import-global-args '(:wrap "none")))

(use-package ox-reveal
  :after org)

(use-package gptel
  :defer t
  :custom
  (gptel-model "gpt-5")
  (gptel-api-key (auth-info-password (nth 0 (auth-source-search :max 1 :host "platform.openai.com"))))
  (gptel-default-mode 'org-mode))

;; (use-package aidermacs
;;   :straight (:host github :repo "MatthewZMD/aidermacs")
;;   :bind (("C-x p a" . aidermacs-transient-menu))
;;   :custom
;;   ; See the Configuration section below
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "4o")
;;   :hook ((
;;          aidermacs-before-run-backend .
;;           (lambda ()
;;             (setenv "OPENAI_API_KEY" (auth-info-password (nth 0 (auth-source-search :max 1 :host "platform.openai.com")))))
;;          )))

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  ;; (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or chatgpt model
  (setenv "OPENAI_API_KEY" (auth-info-password (nth 0 (auth-source-search :max 1 :host "platform.openai.com"))))
  ;; Or gemini model
  ;; (setq aider-args '("--model" "gemini-exp"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  :bind (("C-x p a" . aider-transient-menu)))

(use-package ellama
  :commands (make-llm-ollama)
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
                    (make-llm-ollama
                     :chat-model "llama3:8b-instruct-q4_0"
                     :embedding-model "llama3:8b-instruct-q4_K_M"))
  :custom
  (ellama-keymap-prefix "C-c l")
  (ellama-user-nick (getenv "USER"))
  (ellama-assistant-nick "ellama"))

(use-package eshell
  :after eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-git-prompt)

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind (:map vterm-mode-map ("C-<backspace>" . (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
         :map project-prefix-map ("v" . multi-vterm-project)))

(use-package multi-vterm
  :after vterm)

(use-package vterm-toggle
  :after vterm)

(use-package proced
  :ensure nil
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'string))

(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M--" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-directory) ; group by project.el project root, with fall back to default-directory
  :custom
  (popper-echo-dispatch-keys '("M-a" "M-s" "M-d" "M-f" "M-g"))
  (popper-mode +1)
  (popper-echo-mode +1)  ; For echo area hints
  (popper-reference-buffers
   '("^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$" shell-mode
     "^\\*term.*\\*$" term-mode
     "^\\*vterm.*\\*$" vterm-mode
     "^\\*Python.*\\*$" inferior-python-mode
     "\\*Messages\\*"
     "Output\\*$"
     ;; ("\\*Async Shell Command\\*" . hide)
     ("\\*Warnings\\*" . hide)
     help-mode
     compilation-mode))
)

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
          if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(use-package tp
  :straight (:host codeberg :repo "martianh/tp.el"))

(use-package mastodon
  :custom
  (mastodon-instance-url "https://mastodon.sdf.org")
  (mastodon-active-user "rbgb"))

(use-package ement)

(use-package elfeed
  :custom
  (elfeed-feeds
      '(("http://nullprogram.com/feed/" code)
        ("https://planet.emacslife.com/atom.xml" code emacs)
        ("https://sburris.xyz/atom.xml" code)
        ("https://www.seangoedecke.com/rss.xml" ai code)
        ("https://drew.silcock.dev/rss.xml" code)
        ("https://emacsrocks.com/atom.xml" code emacs)
        ("https://www.data-is-plural.com/feed.xml" code)
        ("https://lilianweng.github.io/index.xml" ml code)
        ("https://www.data-is-plural.com/feed.xml" data)
        ("https://waxy.org/feed/" culture)
        ("http://feeds.kottke.org/main" culture)
        ("https://www.polygon.com/rss/index.xml" games)
        ("https://p.bauherren.ovh/rss" emacs)))
  (elfeed-search-title-max-width 100)
  :bind (:map elfeed-search-mode-map ("g" . elfeed-update)))

(use-package erc
  :custom
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs")))
  (erc-dcc-get-default-directory "~/Downloads/erc")
  (erc-kill-buffer-on-part t)
  (erc-nick "rbgb")
  (erc-server "irc.libera.chat")
  (erc-track-shorten-start 8)
  (erc-user-full-name "rKAST"))

(use-package which-key)

(use-package eww
  :bind (:map eww-mode-map (("C-<return>" . eww-open-in-new-buffer) ("S-<space>" . scroll-up-command))))

(use-package elpher
  :bind (:map elpher-mode-map (("n" . next-line) ("p" . previous-line) ("v" . push-button) ("q" . elpher-back))))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 1)
  (company-minimum-prefix-length 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                 '(("pylsp")
                                   ("ruff" "server")))))
  :custom
  (eglot-workspace-configuration
   '((:pylsp . (:plugins (:flake8 (:enabled :json-false))))))
  :hook
  ((python-mode . eglot-ensure)))

(use-package yasnippet
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :config (yas-global-mode 1))

;; python
(use-package python
  :custom
  (dap-python-debugger 'debugpy)
  (python-shell-interpreter "python3")
  :config
  (defhydra python:hydra (python-mode-map "C-c h" :color pink)
    ("n" end-of-defun "Next" :column "Navigate")
    ("p" beginning-of-defun "Previous")
    ("s" consult-imenu "Search")
    ("q" nil "Quit" :color blue :column "Quit")))

(use-package python-black
  :after python)

(use-package pyvenv)

(use-package dap-mode
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-mode 1)
  (require 'dap-python))

(use-package flycheck
  :config (global-flycheck-mode))

;; (use-package uv-mode
;;   :straight (:type git :host github :repo "z80dev/uv-mode")
;;   :hook ((python-mode . uv-mode-auto-activate-hook)))

(use-package docker
  :bind ("C-c d" . docker))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package browse-at-remote
  :bind ("C-c g g" . 'browse-at-remote))

(use-package ag)

(use-package transpose-frame)

(use-package impatient-mode
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer))))

(use-package diminish)

(use-package ligature
  :config
  ;; ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                                       "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                                       "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                                       "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                                       "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                                       "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                                       "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                                       ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                                       "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                                       "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                                       "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(use-package indent-tools)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :no-query)
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward) ("C" . pdf-view-center-in-window)))

;; modes
;; (use-package json-mode
;;   :config
;;   (add-hook 'json-mode-hook (lambda () (define-key json-mode-map (kbd "C-c >") 'indent-tools-hydra/body))))

(use-package markdown-mode
  :custom
  (markdown-nested-imenu-heading-index nil)
  :config
  (defhydra markdown:hydra (markdown-mode-map "C-c h" :color pink)
    ("n" markdown-next-visible-heading "Next" :column "Navigate")
    ("j" markdown-next-visible-heading "Next")
    ("p" markdown-previous-visible-heading "Previous")
    ("k" markdown-previous-visible-heading "Previous")
    ("u" markdown-up-heading "Up level")
    ("f" markdown-forward-same-level "Forward same level")
    ("l" markdown-forward-same-level "Forward same level")
    ("b" markdown-backward-same-level "Backward same level")
    ("h" markdown-backward-same-level "Backward same level")
    ("q" nil "Quit" :color red)))

(use-package markdown-toc)

(use-package poly-markdown)

(use-package mermaid-mode
  :mode
  ("\\.mermaid\\'" "\\.mmjs\\'"))

(use-package bicep-mode
  :straight (:type git :host github :repo "christiaan-janssen/bicep-mode"))

(use-package hs-minor-mode
  :straight nil
  :hook (json-mode prog-mode yaml-mode))

(use-package column-number-mode
  :straight nil
  :hook prog-mode)

(use-package subword-mode
  :straight nil
  :hook python-mode)

(use-package display-fill-column-indicator-mode
  :straight nil
  :hook python-mode)

(use-package visual-line-mode
  :straight nil
  :hook org-mode)

(use-package yaml-mode
  :config
  :bind (:map yaml-mode-map ("C-c C-j" . consult-imenu)))

(use-package pandoc-mode)

(use-package scad-mode
  :custom
  (scad-preview-image-size '(800 . 800))
  (scad-preview-window-size 90)
  :defines scad-preview-image-size scad-preview-window-size)


(use-package sclang
  :straight ( :type git
              :host github
              :repo "supercollider/scel"
              :files ("el/*"))
  :config
  (require 'sclang)
  :bind ("C-z" . sclang-switch-to-post))

(use-package sclang-extensions
  :hook sclang-mode
  :bind (:map sclang-extensions-mode-map ("C-c C-c" . sclang-eval-line)))

(use-package djvu)

(use-package fasta-mode
  :straight (:type git :host github :repo "vaiteaopuu/emacs-fasta-mode" :files ("fasta-mode.el" "sequence-alignement.el")))

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package dockerfile-mode)

(use-package lua-mode)

(use-package kotlin-mode)

(use-package stan-mode)

(use-package ess)

(use-package powershell)

(use-package arduino-mode)

(use-package sqlite3)

(use-package emms
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-config" "--save-position-on-quit" "--no-audio-display" ))
  (emms-info-asynchronously t)
  (emms-show-format "♪ %s")
  (emms-source-file-default-directory "/mnt/uplandvault/music"))

(emms-all)

(use-package ready-player
  :straight ( :type git
              :host github
              :repo "xenodium/ready-player"
              :files ("ready-player.el"))
  :config
  (ready-player-mode +1))

(use-package sqlformat
  :custom
  (sqlformat-command 'sqlfmt))

(use-package csv-mode)

(use-package web-mode
  :custom
  (web-mode-engines-alist '(("django" . "\\.html?\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 4)
  :mode
  ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.vue\\'"))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package kubernetes
  :config
  (unbind-key "C-<tab>"  kubernetes-overview-mode-map)
  ;; wider column width (21 to 40) for pvc name
  (setq kubernetes-persistentvolumeclaims--column-heading
        ["%-40s %10s %10s %15s %6s" "Mame Phase Capacity Class Age"])
  (kubernetes-ast-define-component persistentvolumeclaim-line (state persistentvolumeclaim)
    (-let* ((current-time (kubernetes-state--get state 'current-time))
            (pending-deletion (kubernetes-state--get state 'persistentvolumeclaims-pending-deletion))
            (marked-persistentvolumeclaims (kubernetes-state--get state 'marked-persistentvolumeclaims))
            ((&alist 'spec (&alist 'storageClassName storage-class)
                     'status (&alist 'phase phase 'capacity (&alist 'storage capacity))
                     'metadata (&alist 'name name 'creationTimestamp created-time))
             persistentvolumeclaim)
            ([fmt] kubernetes-persistentvolumeclaims--column-heading)
            (list-fmt (split-string fmt))
            (line `(line ,(concat
                           ;; Name
                           (format (pop list-fmt) (s-truncate 40 name))
                           " "
                           ;; Phase
                           (propertize (format (pop list-fmt) phase) 'face 'kubernetes-dimmed)
                           " "
                           ;; Capacity
                           (propertize (format (pop list-fmt) capacity) 'face 'kubernetes-dimmed)
                           " "
                           ;; Storage Class
                           (propertize (format (pop list-fmt) (s-truncate 12 storage-class)) 'face 'kubernetes-dimmed)
                           " "
                           ;; Age
                           (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                             (propertize (format (pop list-fmt) (kubernetes--time-diff-string start current-time))
                                         'face 'kubernetes-dimmed))))))
      `(nav-prop (:persistentvolumeclaim-name ,name)
                 (copy-prop ,name
                            ,(cond
                              ((member name pending-deletion)
                               `(propertize (face kubernetes-pending-deletion) ,line))
                              ((member name marked-persistentvolumeclaims)
                               `(mark-for-delete ,line))
                              (t
                               line)))))))

(use-package mu4e
  :straight ( :host github
              :repo "djcb/mu"
              :files ("build/mu4e/*")
              :pre-build (("./autogen.sh") ("make")))
  :preface
  (defun my/org-msg-no-temp-buffer (orig-fun &rest args)
    "Advice to set `org-export-show-temporary-export-buffer' to `nil'."
    (let ((org-export-show-temporary-export-buffer nil))
      (apply orig-fun args)))
  (defun my/mu4e-mark-spam()
    "Add a tag to the message at point."
    (interactive)
    (mu4e-headers-mark-and-next 'spam))
  :hook
  (mu4e-update-pre . mu4e-update-index-nonlazy)
  :custom
  (message-kill-buffer-on-exit t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-complete-only-personal t)
  (mu4e-compose-format-flowed t)
  (mu4e-confirm-quit nil)
  (mu4e-debug nil)
  (mu4e-doc-dir "/home/robert/.emacs.d/straight/repos/mu")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-headers-fields '((:human-date . 10) (:from . 16) (:to . 16) (:subject . 70) (:maildir . 20)))
  (mu4e-headers-visible-columns 140)
  (mu4e-headers-visible-lines 50)
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check nil)
  (mu4e-maildir "~/.mail")
  (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  (mu4e-notification-support t)
  (mu4e-search-include-related nil)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-split-view 'vertical)
  (mu4e-update-interval (* 60 5))
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-images t)
  (mu4e-bookmarks
   '(
      (
        :name "Galileo"
        :query "(m:/galileo/Inbox or m:\"/galileo/Sent Mail\" or (from:galileo@gmail.com and not (m:/galileo/Trash or m:/galileo/Drafts))) and date:45d..now and not flag:trashed"
        :key ?g
      )
      (
        :name "Galileo all"
        :query "(m:/galileo/Inbox or m:\"/galileo/Sent Mail\" or m:/galileo/Archive) and date:90d..now and not flag:trashed"
        :key ?G
      )
      (
        :name "DrivenData"
        :query "(m:/drivendata/Inbox or m:\"/drivendata/Sent Mail\" or (from:robert@drivendata.org and not (m:/drivendata/Trash or m:/drivendata/Drafts))) and date:45d..now and not flag:trashed"
        :key ?d
      )
      (
        :name "DrivenData all"
        :query "(m:/drivendata/Inbox or m:\"/drivendata/Sent Mail\" or m:/drivendata/Archive) and date:90d..now and not flag:trashed"
        :key ?D
      )
      (
        :name "All"
        :query "not m:/galileo/Spam and not m:/galileo/Trash and not m:/drivendata/Spam and not m:/drivendata/Trash and date:21d..now"
        :key ?a
      )
      (
        :name "Unread messages"
        :query "flag:unread and not m:/galileo/Spam and not m:/galileo/Trash and not m:/drivendata/Spam and not m:/drivendata/Trash and date:21d..now"
        :key ?u
      )
    )
  )
  :config
  (advice-add 'org-msg-preview :around #'my/org-msg-no-temp-buffer)
  (advice-add 'org-msg-ctrl-c-ctrl-c :around #'my/org-msg-no-temp-buffer)
  (add-hook 'message-sent-hook
            (lambda ()
              (interactive)
              (switch-to-buffer "*mu4e-article*")
              (mu4e-view-quit)
              (kill-buffer "*Org ASCII Export*")))

  (setq mu4e-contexts
        (list
         ;; galileo@gmail.com
         (make-mu4e-context
          :name "galileo"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/galileo" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "galileo@gmail.com")
                  (user-full-name    . "Robert Gibboni")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-smtp-user . "galileo@gmail.com")
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/galileo/Drafts")
                  (mu4e-sent-folder  . "/galileo/Sent Mail")
                  (mu4e-refile-folder  . "/galileo/Archive")
                  (mu4e-trash-folder  . "/galileo/Trash")))
         ;; robert@drivendata.org
         (make-mu4e-context
          :name "drivendata"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/drivendata" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "robert@drivendata.org")
                  (user-full-name    . "Robert Gibboni")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-smtp-user . "robert@drivendata.org")
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/drivendata/Drafts")
                  (mu4e-sent-folder  . "/drivendata/Sent Mail")
                  (mu4e-refile-folder  . "/drivendata/Archive")
                  (mu4e-trash-folder  . "/drivendata/Trash")))))
  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                ;; Here's the main difference to the regular trash mark, no +T
                ;; before -N so the message is not marked as IMAP-deleted:
                :action (lambda (docid msg target)
                          (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))
  (add-to-list 'mu4e-marks
               '(spam
                 :char       "s"
                 :prompt     "spam"
                 :dyn-target (lambda (target msg) (s-replace "Trash" "Spam" (mu4e-get-trash-folder msg)))
                 :action      (lambda (docid msg target)
                                (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N")
)))
  (defun my/contact-processor (contact)
    (cond
      ((string-match-p "CashApp" contact) nil)
      ((string-match-p "[🚀⭐🎉🎁🎲🌻📦💰💌💘💲🚚🔻]" contact) nil)
      ((string-match-p "^[A-Z\.]*@gmail.com$" contact) nil)
      ((string-match-p "^[Gg][Aa].*@gmail.com$" contact) nil)
      ((string-match-p "^[Gg]alileo.*" contact) nil)
      ((string-match-p "autozone@em\.autozone\.com\.mx" contact) nil)
      ((string-match-p "discoursemail\.com$" contact) nil)
      ((string-match-p "docs\.google\.com$" contact) nil)
      ((string-match-p "mg1\.substack\.com$" contact) nil)
      ((string-match-p "onf\.ru$" contact) nil)
      ((string-match-p "reply" contact) nil)
      ((string-match-p "unsubscribe" contact) nil)
      (t contact)))
  (setq mu4e-contact-process-function 'my/contact-processor)
  (defun my/mu4e-view-message-with-message-id ()
    (interactive)
    (let ((input (read-string "Message ID: ")))
      (mu4e-view-message-with-message-id (replace-regexp-in-string "^mu4e:msgid:" "" input))))
  :bind (
         :map mu4e-headers-mode-map
              ("i" . my/mu4e-view-message-with-message-id)
              ("v" . mu4e-headers-view-message)
              ("@" . my/mu4e-mark-spam)))

(defun my/yank-buffer-name ()
  (interactive)
  (let ((buffer-name* (buffer-name)))
    (kill-new buffer-name*)
    (message buffer-name*)
    buffer-name*))

(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<tab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-recent-tab)
(global-set-key [mouse-10] 'next-buffer)
(global-set-key [mouse-11] 'previous-buffer)
(global-set-key [mouse-8] 'previous-buffer)
(global-set-key [mouse-9] 'next-buffer)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
(global-unset-key (kbd "C-z"))

;; M-backspace does not copy to clipboard
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun insert-random-string (length)
  "Inserts a random string of characters at the current mark in the buffer."
  (interactive "p")
  (let* ((allowed-chars "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()")
         (length (if length length 20))
         (random-string (cl-loop repeat length concat (string (elt allowed-chars (random (length allowed-chars)))))))
    (push-mark)
    (insert random-string)))

(defun insert-random-number (length)
  "Inserts a random string of characters at the current mark in the buffer."
  (interactive "p")
  (let* ((allowed-chars "0123456789")
         (length (if length length 20))
         (random-string (cl-loop repeat length concat (string (elt allowed-chars (random (length allowed-chars)))))))
    (push-mark)
    (insert random-string)))

(use-package move-border
  :straight ( :host github
              :repo "ramnes/move-border"
              :branch "master"
              :files ("move-border.el")))

(defun my/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(use-package org-gcal
  :defer
  :custom
  (org-gcal-client-id (plist-get (nth 0 (auth-source-search :max 1 :host "clean-sector-432602-p9.apps.googleusercontent.com")) :user))
  (org-gcal-client-secret (my/lookup-password :host "clean-sector-432602-p9.apps.googleusercontent.com"))
  (org-gcal-fetch-file-alist '(("robert@drivendata.org" .  "~/org-roam/schedule.org"))))

(use-package nano-agenda
  :bind (:map nano-agenda-mode-map ("h" . nano-agenda-backward-day) ("l" . nano-agenda-forward-day) ("j" . nano-agenda-backward-week) ("j" . nano-agenda-forward-week) ("k" . nano-agenda-backward-week)))


(use-package mu4e-dashboard
  :straight ( :host github
              :repo "rougier/mu4e-dashboard"
              :branch "main"
              :files ("mu4e-dashboard.el"))
  :custom
  (mu4e-dashboard-mu-program "~/.emacs.d/straight/repos/mu/build/mu/mu"))

(use-package mu4e-thread-folding
  :after mu4e
  :hook (mu4e-headers-mode . mu4e-thread-folding-mode)
  :straight ( :host github
              :repo "rougier/mu4e-thread-folding"
              :branch "master"
              :files ("mu4e-thread-folding.el"))
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                                 :shortname ""
                                 :function (lambda (msg) "  "))))
  :custom-face
  (mu4e-header-face ((t (:inherit default :background ,(color-lighten-name (face-background 'default) 0)))))
  (mu4e-thread-folding-child-face ((t (:inherit default :background ,(color-darken-name (face-background 'default) 30)))))
  (mu4e-thread-folding-root-unfolded-face ((t (:inherit default :background ,(color-lighten-name (face-background 'default) 0)))))
  (mu4e-thread-folding-root-folded-face ((t (:inherit default :background ,(color-lighten-name (face-background 'default) 0)))))
  :custom
  (mu4e-thread-folding-default-view 'folded)
  (mu4e-thread-folding-root-folded-prefix-string " ▶")
  (mu4e-thread-folding-root-unfolded-prefix-string " ▼")
  (mu4e-headers-fields '((:empty         .    2)
                         (:human-date    .   12)
                         (:from          .   22)
                         (:subject       .   80)
                         (:maildir       .   16)
                         )))

(use-package w3m)

(use-package time-zones
  :straight ( :host github
              :repo "xenodium/time-zones"))

(use-package svg-lib)
(use-package svg-tag-mode
  :requires svg-lib
  :straight ( :host github
              :repo "rougier/svg-tag-mode"
              :branch "main"
              :files ("svg-tag-mode.el"))
  :hook (org-mode)
  :custom
  (svg-tag-tags '(
                  ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-tag :radius 0 :inverse t :margin 0))))
                  ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'font-lock-comment-face :radius 0 :inverse t :margin 0)))))))

(use-package nano-mu4e
  :requires svg-tag-mode
  :straight ( :host github
              :repo "rougier/nano-emacs"
              :branch "master"
              :files ("nano-mu4e.el")))

(pretty-hydra-define my-window (:foreign-keys warn :title "Utilities" :quit-key "q")
  ("Actions"
   (
    ("TAB" tab-bar-switch-to-tab "Switch")
    ("o" other-window "Other")
    ("x" ace-delete-window "Delete")
    ("s" ace-swap-window "Swap")
    ("a" ace-select-window "Select"))

   "Font size"
   (("1" (set-face-attribute 'default nil :height 70) "small")
    ("2" (set-face-attribute 'default nil :height 90) "normal")
    ("3" (set-face-attribute 'default nil :height 110) "large")
    ("4" (set-face-attribute 'default nil :height 130) "xlarge")
    ("5" (set-face-attribute 'default nil :height 160) "xxlarge"))

   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "Balance")
    ("m" ace-delete-other-windows "Maximize")
    ("f" toggle-frame-fullscreen "Toggle fullscreen"))

   "Split"
   (("b" split-window-right "Horizontally")
    ("B" split-window-horizontally-instead "Horizontally instead")
    ("v" split-window-below "Vertically")
    ("V" split-window-vertically-instead "Vertically instead"))

   "Zoom"
   (("+" text-scale-increase "In")
    ("-" text-scale-decrease "Out")
    ("0" (text-scale-mode -1) "Default"))))

(bind-key (kbd "C-x +") 'my-window/body)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-so-long-mode t)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(show-paren-mode t)
(tab-bar-mode t)
(which-key-mode t)
(winner-mode t)
(desktop-read)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 50)
 '(ag-ignore-list nil)
 '(all-the-icons-dired-monochrome nil)
 '(column-number-mode t)
 '(csv-separators '("," "	" ";"))
 '(dired-listing-switches "-alh")
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice "~/projects")
 '(org-agenda-files nil)
 '(org-babel-python-command "python3")
 '(org-edit-src-content-indentation 0)
 '(send-mail-function 'smtpmail-send-it)
 '(shr-max-image-proportion 0.7)
 '(split-height-threshold 100)
 '(vc-follow-symlinks t)
 '(w3m-home-page "https://lite.duckduckgo.com/lite")
 '(warning-suppress-log-types '((native-compiler) (comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(web-mode-enable-control-block-indentation t)
 '(world-clock-list
   '(("America/New_York" "New York")
     ("America/Phoenix" "Phoenix")
     ("America/Denver" "Denver")
     ("America/Los_Angeles" "Los Angeles")
     ("Europe/London" "London")
     ("Asia/Manila" "Philippines"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:background "#262626"))))
 '(jupyter-repl-traceback ((t (:background "black")))))
