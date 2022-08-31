;;; package --- Summary
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
  :bind ("M-o" . ace-window))

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

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("<f6>" . org-capture)))

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

(use-package ox-reveal
  :ensure t)

(use-package forge
  :after magit)

(use-package browse-at-remote
  :ensure t
  :bind ("C-c g g" . 'browse-at-remote))

(use-package which-key
  :ensure t)

(use-package ag
  :ensure t)

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
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :init
  (global-undo-tree-mode))

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

(use-package yaml-mode
  :ensure t)

(use-package scad-mode
  :ensure t)

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
  :ensure t)

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 18)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-evil
  :after (treemacs)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil))  ; browser to use

(show-paren-mode t)
(which-key-mode t)
(winner-mode t)
(menu-bar-mode -1)
(tab-bar-mode t)

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
(setq default-tab-width 4)
(setq linum-format "%4d\u2502 ")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 50)
 '(ag-ignore-list nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" default))
 '(ein:output-area-inlined-images t)
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
   '("~/org/hrwg.org" "/home/robert/org/todo.org" "/home/robert/projects/drivendata-platform/org/platform.org" "/home/robert/projects/candid-orgmatch/org/candid.org"))
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (shell . t)))
 '(org-babel-python-command "ipython --no-banner --classic --no-confirm-exit")
 '(org-startup-indented t)
 '(safe-local-variable-values
   '((pyvenv-workon . candid-entity-graph)
     (pyvenv-workon . candid-orgmatch)))
 '(scad-preview-image-size '(800 . 800))
 '(scad-preview-window-size 90)
 '(show-paren-mode t)
 '(split-height-threshold 100)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
