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
  :init
  (global-undo-tree-mode))

(use-package direx
  :ensure t
  :init
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory))

(use-package zenburn-theme
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package json-mode
  :ensure t)

(use-package js2-mode
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

(use-package stan-mode
  :ensure t)

(use-package anaconda-mode
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
  :load-path "/home/robert/.emacs.d/elisp/ligature/"
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

(show-paren-mode t)
(which-key-mode t)
(winner-mode t)

(add-hook 'json-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'python-mode-hook 'indent-tools-minor-mode)
(add-hook 'python-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook 'hs-minor-mode)

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

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-quick-access t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
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
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq create-lockfiles nil)

(set-frame-font "Fira Code 8" nil t)

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
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(org-agenda-files '("~/.org"))
 '(package-selected-packages '(forge ivy-rich ace-window use-package ein))
 '(safe-local-variable-values
   '((pyvenv-workon . candid-entity-graph)
     (pyvenv-workon . candid-orgmatch)))
 '(scad-preview-image-size '(800 . 800))
 '(scad-preview-window-size 90)
 '(show-paren-mode t)
 '(split-height-threshold 100)
 '(tool-bar-mode nil))

(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
