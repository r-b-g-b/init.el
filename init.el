;; package --- Summary
;;; Commentary:
;;; Code:

;; https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
        (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package flycheck
  :ensure t
  :init
  :config (global-flycheck-mode))

(use-package pyvenv
  :ensure t
  :init (setenv "WORKON_HOME" "~/anaconda3/envs/")
	(pyvenv-mode 1))

(use-package pylint
  :ensure t)

(use-package blacken
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package org
  :init
  (setq org-startup-indented t)
  :bind (("C-c a" . org-agenda)
	 ("<f6>" . org-capture))
  :custom
  (org-goto-interface 'outline-path-completion)
  (org-support-shift-select t))

(use-package org-roam
  :ensure t
  :custom (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-graph-executable "neato"))

(use-package deft
  :ensure
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel ivy projectile)
  :config (counsel-projectile-mode))

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords
   'org-mode '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org-variable-pitch
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path "~/.local/bin/mmdc"))

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode))

(use-package better-shell
    :ensure t
    :bind (("C-'" . better-shell-shell)
           ("C-;" . better-shell-remote-open)))

(use-package w3m
  :ensure t
  :custom
  (w3m-home-page "https://lite.duckduckgo.com/lite"))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package keychain-environment
  :ensure t)

(use-package ox-reveal
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package browse-at-remote
  :ensure t
  :bind ("C-c g g" . 'browse-at-remote))

(use-package which-key
  :ensure t)

(use-package ag
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package multiple-cursors
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package poly-markdown
  :ensure t)


(use-package transpose-frame
  :ensure t)

(use-package impatient-mode
  :ensure t
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer))))

(use-package undo-tree
  :ensure t
  :defines undo-tree-auto-save-history undo-tree-history-directory-alist
  :init
  (global-undo-tree-mode))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(use-package direx
  :ensure t
  :init
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory))

(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package json-mode
  :ensure t)

(use-package mermaid-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package scad-mode
  :ensure t
  :defines scad-preview-image-size scad-preview-window-size
  :init
  (setq scad-preview-image-size '(800 . 800))
  (setq scad-preview-window-size 90))

(use-package typescript-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package stan-mode
  :ensure t)

(use-package arduino-mode
  :ensure t)

(use-package anaconda-mode
  :ensure t)

(use-package python-black
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist
	'(("django"    . "\\.html?\\'")))
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package ein
  :ensure t
  :defines ein:output-area-inlined-images
  :functions ein:worksheet-execute-cell-and-goto-next-km ein:worksheet-execute-cell-and-insert-below-km
  :bind
  ("C-<return>" . 'ein:worksheet-execute-cell-and-goto-next-km)
  ("C-S-<return>" . 'ein:worksheet-execute-cell-and-insert-below-km)
  :config
  (setq ein:output-area-inlined-images t))

(use-package diminish
  :ensure t)

(use-package company-anaconda
  :ensure t)

(use-package ligature
  :load-path "/home/robert/.emacs.d/elisp/ligature.el/"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

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

(use-package indent-tools
  :ensure t)

(use-package realgud
  :ensure t)

(use-package realgud-ipdb
  :ensure t)

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :after vterm)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(use-package livedown
  :load-path "emacs-livedown"
  :custom
  (livedown-autostart nil)
  (livedown-browser nil)
  (livedown-open t)
  (livedown-port 1337))

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
 '(fill-column 100)
 '(ibuffer-formats
   '((mark modified read-only locked " "
	   (name 50 50 :left :elide)
	   " "
	   (size 9 -1 :right)
	   " "
	   (mode 16 16 :left :elide)
	   " " filename-and-process)
     (mark " "
	   (name 16 -1)
	   " " filename)))
 '(initial-buffer-choice "~/projects")
 '(org-agenda-files
   '("/home/robert/projects/candid-orgmatch/org/cds_graph.org" "/home/robert/projects/candid-orgmatch/org/candid.org" "/home/robert/projects/drivendata-platform/org/render-migration.org" "/home/robert/org/todo.org" "/home/robert/org/hrwg/hrwg.org" "/home/robert/projects/drivendata-platform/org/platform.org"))
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (shell . t)))
 '(org-babel-python-command "ipython --no-banner --classic --no-confirm-exit")
 '(org-edit-src-content-indentation 0)
 '(package-selected-packages
   '(mermaid-mode ob-mermaid w3m deft org-roam-export zenburn-theme yaml-mode which-key web-mode use-package undo-tree typescript-mode treemacs-tab-bar treemacs-magit treemacs-icons-dired transpose-frame stan-mode sqlite3 spacemacs-theme scad-mode realgud-ipdb pyvenv python-black pylint poly-markdown ox-reveal org-variable-pitch org-roam org-bullets multiple-cursors multi-vterm kotlin-mode keychain-environment jsonl json-mode ivy-rich indent-tools impatient-mode forge flycheck evil-collection emojify ein doom-themes doom-modeline dockerfile-mode docker direx dired-icon diminish csv-mode csharp-mode counsel-projectile company-anaconda browse-at-remote blacken better-shell arduino-mode all-the-icons-dired ag a))
 '(projectile-project-search-path '("~/projects"))
 '(safe-local-variable-values
   '((pyvenv-workon . candid-entity-graph)
     (pyvenv-workon . candid-orgmatch)))
 '(split-height-threshold 100)
 '(w3m-home-page "https://lite.duckduckgo.com/lite"))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tab-bar-mode t)
(tool-bar-mode -1)
(which-function-mode t)
(which-key-mode t)
(winner-mode t)

(add-hook 'json-mode-hook 'hs-minor-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'indent-tools-minor-mode)
(add-hook 'python-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook 'hs-minor-mode)

(set-face-attribute 'default nil :height 86)
(setq-default flycheck-disabled-checkers '(python-pylint))
(setq-default electric-indent-inhibit t)
(setq-default default-tab-width 4)
(setq-default linum-format "%4d\u2502 ")
(setq markdown-fontify-code-blocks-natively t)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5)
(setq scroll-margin 10)
(setq which-func-unknown "n/a")

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

(global-hl-line-mode t)
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
