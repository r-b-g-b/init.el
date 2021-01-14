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

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

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
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel ivy)
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

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
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

(use-package which-key
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-smart-open t))

(use-package transpose-frame
  :ensure t)

(use-package impatient-mode
  :ensure t
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer))))

(use-package origami
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package direx
  :ensure t
  :init
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory))

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

(use-package doom-themes
  :ensure t)

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

(show-paren-mode t)
(which-key-mode t)
(winner-mode t)

(add-hook 'elpy-mode-hook (lambda () (elpy-shell-toggle-dedicated-shell 1)))
(add-hook 'json-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook 'hs-minor-mode)

(setq default-tab-width 4)
(setq elpy-rpc-virtualenv-path 'current)
(setq linum-format "%4d\u2502 ")
(setq markdown-fontify-code-blocks-natively t)
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5)
(setq scroll-margin 10)
(setq-default flycheck-disabled-checkers '(python-pylint))

(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key [mouse-10] 'next-buffer)
(global-set-key [mouse-11] 'previous-buffer)
(global-set-key [mouse-8] 'previous-buffer)
(global-set-key [mouse-9] 'next-buffer)
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
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs managed configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" default)))
 '(ein:output-area-inlined-images t)
 '(elpy-rpc-backend "jedi" t)
 '(elpy-rpc-timeout 10)
 '(initial-buffer-choice "~/projects")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
    (scad-mode typescript-mode ranger ein csv-mode csharp-mode stan-mode kotlin-mode multiple-cursors direx ztree blacken snakemake-mode company-jedi toml-mode docker-compose-mode swiper impatient-mode arduino-mode counsel ivy-rich neotree hideshow-org ess ag counsel-projectile magit json-mode jsonnet-mode dockerfile-mode ivy yaml-mode projectile elpy markdown-mode+ dracula-theme company flycheck ace-window transpose-frame gnu-elpa-keyring-update mmm-mode markdown-mode)))
 '(safe-local-variable-values
   (quote
    ((pyvenv-workon . deriveone-wwl-skill-package)
     (pyvenv-workon . deriveone-wwl-nerherder-service)
     (pyvenv-workon . candid-orgmatch)
     (pyvenv-workon . deriveone-wwl-transcript-parsing)
     (pyvenv-workon . stallcatchers)
     (pyvenv-workon . buildseg_1)
     (pyvenv-workon . stallcatchers/)
     (pyvenv-workon . rapidsos-covid-911/)
     (pyvenv-workon . loggingisfun/)
     (pyvenv-workon . typingisfun/)
     (pyvenv-workon . hbr-retention-analysis/))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Mono" :foundry "monotype" :slant normal :weight normal :height 79 :width normal)))))
