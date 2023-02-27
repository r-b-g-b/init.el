;; package --- Summary
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

(use-package spacemacs-theme
  :straight t
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package hydra
  :straight t)

(use-package undo-tree
  :after hydra
  :straight t
  :init
  (global-undo-tree-mode)
  (defhydra hydra-undo-tree (:hint nil)
      "
  _p_: undo  _n_: redo _s_: save _l_: load   "
      ("p"   undo-tree-undo)
      ("n"   undo-tree-redo)
      ("s"   undo-tree-save-history)
      ("l"   undo-tree-load-history)
      ("u"   undo-tree-visualize "visualize" :color blue)
      ("q"   nil "quit" :color blue))
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package ivy-hydra
  :straight t
  :after hydra)

(use-package ivy-rich
  :straight t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode))

(use-package tree-sitter
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package multiple-cursors
  :straight t)

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :straight t
  :after magit)

(use-package github-review
  :straight t)

(use-package gh-notify
  :straight t)

(use-package git-link
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package linum-mode
  :ensure nil
  :hook prog-mode)

(use-package vterm
  :straight t
  :custom
  (vterm-always-compile-module t))

(use-package multi-vterm
  :straight t
  :after vterm)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :straight t
  :after (counsel ivy projectile)
  :config (counsel-projectile-mode))

(use-package mastodon
  :straight t
  :custom
  (mastodon-instance-url "https://mastodon.sdf.org")
  (mastodon-active-user "rbgb"))

(use-package elpher
  :straight t)

(use-package which-key
  :straight t)

(use-package eww
  :bind (:map eww-mode-map ("C-<return>" . eww-open-in-new-buffer))
  :custom
  (browse-url-browser-function 'eww-browse-url))

(use-package company
  :straight t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :defines lsp-highlight-symbol-at-point
  :commands (lsp lsp-deferred)
  :hook (;; (csharp-mode . lsp)
         ;; (python-mode . lsp)
         (css-mode . lsp)
         (dockerfile-mode . lsp)
         ;; (forge-post-mode . lsp)
         (js-mode . lsp)
         ;; (markdown-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
         ;; (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil
              lsp-keymap-prefix "C-c l"
              lsp-lens-enable t
              lsp-signature-auto-activate nil)
  :config
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(
     ("pylsp.plugins.black.enabled" t t)
     ("pylsp.plugins.black.line_length" 120 t)
     ("pylsp.plugins.ruff.enabled" t t)
     ("pylsp.plugins.mccabe.enabled" nil)
     ("pylsp.plugins.pycodestyle.enabled" nil)
     ("pylsp.plugins.pydocstyle.enabled" nil)
     ("pylsp.plugins.pyflakes.enabled" nil)
     ("pylsp.plugins.isort.enabled" t t)
     )))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :straight t
  :after lsp)

;; lsp-doctor suggests
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)
(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil) ; if set to true can cause a performance hit

(use-package yasnippet
  :straight t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

;; python
(use-package python-mode
  :ensure nil
  :hook
  (python-mode . lsp-deferred))

(use-package pyvenv
  :straight t
  :init (setenv "WORKON_HOME" "~/anaconda3/envs/")
  :config
  (pyvenv-mode 1))

(use-package ein
  :straight t)

(use-package ein-notebook
  :after ein
  :bind (:map ein:notebook-mode-map
	 ("C-<return>" . ein:worksheet-execute-cell-and-insert-below-km)
         ("S-<return>" . ein:worksheet-execute-cell-and-goto-next-km))
  :custom
  (ein:output-area-inlined-images t)
  :config
  (defhydra ein:notebook-navigation (ein:notebook-mode-map "C-c h")
    "navigate"
    ("<up>" ein:worksheet-move-cell-up-km)
    ("<down>" ein:worksheet-move-cell-down-km)
    ("a" ein:worksheet-insert-cell-above-km)
    ("b" ein:worksheet-insert-cell-below-km)
    ("e" ein:worksheet-execute-cell-and-goto-next-km)
    ("k" ein:worksheet-kill-cell-km)
    ("m" ein:worksheet-merge-cell-km)
    ("n" ein:worksheet-goto-next-input-km)
    ("p" ein:worksheet-goto-prev-input-km)
    ("q" nil :color blue)
    ("s" ein:notebook-save-notebook-command)
    ("w" ein:worksheet-copy-cell-km)
    ("y" ein:worksheet-yank-cell-km)))


(use-package flycheck
  :straight t
  :config (global-flycheck-mode))

(use-package python-black
  :straight t
  :after python)

;; org
(use-package org
  :init
  (setq org-startup-indented t)
  :bind
  ("C-c C-j" . nil)
  ("C-c a" . org-agenda)
  ("<f6>" . org-capture)
  :custom
  (org-support-shift-select t)
  (org-confirm-babel-evaluate nil)
  (org-goto-auto-isearch nil)
  :config
  (progn
    (unbind-key "C-c C-j")
    (bind-key "C-c C-j" 'counsel-outline)))

(use-package org-roam
  :straight t
  :commands (org-roam-node-list)
  :init
  (setq org-roam-v2-ack t)
  :custom
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
	 ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

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
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "project")))

(my/org-roam-refresh-agenda-list)

(use-package org-bullets
  :straight t
  :hook org-mode
  :config
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords
   'org-mode '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-variable-pitch
  :straight t)

(use-package org-variable-pitch-minor-mode
  :hook org-mode)

(use-package org-roam-ui
  :straight t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package ob-mermaid
  :straight t
  :custom
  (ob-mermaid-cli-path "~/.local/bin/mmdc"))

(use-package ox-reveal
  :straight t)

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package keychain-environment
  :straight t)

(use-package browse-at-remote
  :straight t
  :bind ("C-c g g" . 'browse-at-remote))

(use-package ag
  :straight t)

(use-package transpose-frame
  :straight t)

(use-package impatient-mode
  :straight t
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer))))

(use-package diminish
  :straight t)

(use-package ligature
  :straight t
  :config
  ;; ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))

  ;; ;; Enable ligatures in programming modes
  ;; (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
  ;;   "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
  ;;   "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
  ;;   "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
  ;;   "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
  ;;   "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
  ;;   "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
  ;;   ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
  ;;   "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
  ;;   "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
  ;;   "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(use-package indent-tools
  :straight t)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(use-package livedown
  :straight t
  :custom
  (livedown-autostart nil)
  (livedown-browser nil)
  (livedown-open t)
  (livedown-port 1337))

(use-package pdf-tools
  :straight t)

;; modes
;; (use-package json-mode
;;   :config
;;   (add-hook 'json-mode-hook (lambda () (define-key json-mode-map (kbd "C-c >") 'indent-tools-hydra/body))))

(use-package markdown-mode
  :straight t)

(use-package poly-markdown
  :straight t)

(use-package mermaid-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :config
  (add-hook 'yaml-mode-hook (lambda () (define-key yaml-mode-map (kbd "C-c >") 'indent-tools-hydra/body))))

(use-package scad-mode
  :straight t
  :defines scad-preview-image-size scad-preview-window-size
  :init
  (setq scad-preview-image-size '(800 . 800))
  (setq scad-preview-window-size 90))

(use-package typescript-mode
  :straight t
  :custom
  (typescript-indent-level 2))

(use-package dockerfile-mode
  :straight t)

(use-package kotlin-mode
  :straight t)

(use-package csharp-mode
  :straight t)

(use-package stan-mode
  :straight t)

(use-package arduino-mode
  :straight t)

;; (use-package vue-mode
;;   :straight t
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp))

(use-package csv-mode
  :straight t)

(use-package web-mode
  :straight t
  :custom
  (web-mode-engines-alist '(("django"    . "\\.html?\\'")))
  :mode
  ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.vue\\'")
  :hook
  (lsp))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package hs-minor-mode
  :hook (json-mode prog-mode yaml-mode))

(use-package column-number-mode
  :hook prog-mode)

(use-package subword-mode
  :hook python-mode)

(use-package display-fill-column-indicator-mode
  :hook python-mode)

(use-package column-number-mode
  :hook prog-mode)

(use-package visual-line-mode
  :hook org-mode)

(set-face-attribute 'default nil :height 86)
(setq-default flycheck-disabled-checkers '(python-pylint))
(setq-default electric-indent-inhibit t)
(setq-default default-tab-width 4)
(setq-default linum-format "%4d\u2502 ")

(setq auth-source-debug t)
(setq epg-pinentry-mode 'loopback)
(setq markdown-fontify-code-blocks-natively t)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5)
(setq scroll-margin 10)
(setq use-short-answers t)
(setq user-mail-address "rbgb@sdf.org")
(setq user-full-name "Robert Gibboni")
(setq which-func-unknown "n/a")

(use-package mu4e
  :straight ( :host github
              :repo "djcb/mu"
	      :branch "release/1.8"
	      :files ("build/mu4e/*")
	      :pre-build (("./autogen.sh") ("make")))
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/.mail")
  (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  (mu4e-update-interval (* 10 60))
  (mu4e-use-fancy-chars t)
  :config
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
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/galileo/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/galileo/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/galileo/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/galileo/[Gmail]/Trash")))

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
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/drivendata/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/drivendata/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/drivendata/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/drivendata/[Gmail]/Trash")))

         ;; rbgb@sdf.org
         (make-mu4e-context
          :name "sdf"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/sdf" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "rbgb@sdf.org")
                  (user-full-name    . "Robert Gibboni")
                  (smtpmail-smtp-server  . "mx.sdf.org")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/sdf/INBOX.Drafts")
                  (mu4e-sent-folder  . "/sdf/INBOX.Sent")
                  (mu4e-refile-folder  . "/sdf/INBOX.Archive")
                  (mu4e-trash-folder  . "/sdf/INBOX.Trash"))))))

(add-to-list 'load-path "~/.emacs.d/src")
(require 'mu4e-thread-folding)

(add-to-list 'mu4e-header-info-custom
             '(:empty . (:name "Empty"
                         :shortname ""
                         :function (lambda (msg) "  "))))
(setq mu4e-headers-fields '((:empty         .    2)
                            (:human-date    .   12)
                            (:flags         .    6)
                            (:mailing-list  .   10)
                            (:from          .   22)
                            (:subject       .   nil)))

'(mu4e-thread-folding-child-face ((t (:extend t :background "gray15" :underline nil))))
'(mu4e-thread-folding-root-folded-face ((t (:extend t :background "grey10" :overline nil :underline nil))))
'(mu4e-thread-folding-root-unfolded-face ((t (:extend t :background "gray10" :overline nil :underline nil))))

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-quick-access t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
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

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq create-lockfiles nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-hl-line-mode t)
(global-tree-sitter-mode t)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tab-bar-mode t)
(tool-bar-mode -1)
(which-function-mode t)
(which-key-mode t)
(winner-mode t)
(keychain-refresh-environment)

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
 '(lsp-openscad-server "~/.cargo/bin/openscad-lsp")
 '(org-agenda-files nil)
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (shell . t)))
 '(org-babel-python-command "ipython --no-banner --classic --no-confirm-exit")
 '(org-edit-src-content-indentation 0)
 '(projectile-project-search-path '("~/projects"))
 '(send-mail-function 'smtpmail-send-it)
 '(split-height-threshold 100)
 '(w3m-home-page "https://lite.duckduckgo.com/lite")
 '(web-mode-enable-control-block-indentation t))
