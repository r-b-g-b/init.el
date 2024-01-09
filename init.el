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

(require 'dired-x)
(require 'notifications)

(add-to-list 'load-path "~/.emacs.d/src")
(add-to-list 'load-path "~/.emacs.d/src/mastodon-alt")

(set-face-attribute 'default nil :height 86)
(setq-default flycheck-disabled-checkers '(python-pylint))
(setq-default electric-indent-inhibit t)
(setq-default default-tab-width 4)

(setq auth-source-debug t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq company-show-quick-access t)
(setq create-lockfiles nil)
(setq epg-pinentry-mode 'loopback)
(setq mail-user-agent 'mu4e-user-agent)
(setq markdown-fontify-code-blocks-natively t)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 5)
(setq scroll-margin 10)
(setq use-short-answers t)
(setq user-full-name "Robert Gibboni")
(setq user-mail-address "galileo@gmail.com")
(setq which-func-unknown "n/a")
; (set-variable 'read-mail-command 'mu4e)

(setenv "PATH" (concat "/home/robert/anaconda3/bin:" (getenv "PATH")))

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package dired
  :straight nil
  :bind (:map dired-mode-map ("<SPC>" . dired-view-file-other-window)))

(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer)))

(use-package emacs-async
  :hook
  (dired-mode . dired-async-mode))

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

(use-package counsel
  :after (ivy org)
  :config (counsel-mode)
  :bind (:map org-mode-map ("C-c C-j" . counsel-outline)))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-avy)

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

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (define-key undo-tree-mode-map (kbd "C-x u") nil)
  :config
  (defun my/undo-tree-visualize ()
    (interactive)
    (undo-tree-visualize)
    (undo-tree-hydra/body))
  :pretty-hydra
  ((:title "Undo" :color pink)
   ("Navigate"
    (
     ("k" undo-tree-visualize-undo "Undo")
     ("j" undo-tree-visualize-redo "Next")
     ("p" undo-tree-visualize-undo "Undo")
     ("n" undo-tree-visualize-redo "Next")
     ("h" undo-tree-visualize-switch-branch-left "Left")
     ("l" undo-tree-visualize-switch-branch-right "Right")
     ("q" undo-tree-visualizer-quit "Quit" :color blue))))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :bind (("C-x u" . my/undo-tree-visualize)))

(use-package ivy-hydra
  :after hydra)

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode))

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

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))

(use-package multiple-cursors)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  (:map magit-mode-map
        ("C-<tab>" . nil))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package github-review
  :custom
  (define-key github-review-mode-map (kbd "M-o") nil))

(use-package gh-notify)

(use-package git-link)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package nlinum
  :hook (prog-mode . nlinum-mode))

;; org
(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t) (jupyter . t)))
  (org-babel-python-command "python")
  (org-confirm-babel-evaluate nil)
  (org-export-with-sub-superscripts nil)
  (org-goto-auto-isearch nil)
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
    "Modify"
    (("<prior>" org-metaup "Move section up" :column "Modify")
     ("<next>" org-metadown "Move section down")
     ("<" org-promote-subtree "Promote")
     (">" org-demote-subtree "Demote")
     ("t" org-todo "Toggle TODO")
     ("$" org-archive-subtree "Archive")
     ("d" org-cut-subtree "Kill")
     ("i" org-insert-heading "Insert" :exit t))
    "Act"
    (("s" counsel-outline "Search" :color red :column "Actions")
     ("w" org-copy-subtree "Copy")
     ("y" org-paste-subtree "Paste")
     ("I" org-clock-in "Clock in")
     ("O" org-clock-out "Clock in"))))
  :bind
  ("C-c C-j" . nil)
  ("C-c a" . org-agenda)
  ("<f6>" . org-capture)
  (:map org-mode-map ("C-c h" . org-hydra/body)))

(use-package jupyter
  :after org
  :config
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
  (unbind-key "C-c h" jupyter-org-interaction-mode-map)

  :pretty-hydra
  ((:title "Jupyter" :color amaranth :quit-key ("q" "C-g"))
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
     ("<tab>" org-cycle "Toggle fold"))
    "Edit"
    (("<prior>" jupyter-org-move-src-block "Move up")
     ("<next>" (jupyter-org-move-src-block t) "Move down")
     ("d" my/jupyter-org-kill-block-and-results "Kill")
     ("w" jupyter-org-copy-block-and-results "Copy")
     ("o" (jupyter-org-clone-block t) "Clone")
     ("m" jupyter-org-merge-blocks "Merge")
     ("s" jupyter-org-split-src-block "Split")
     ("a" (jupyter-org-insert-src-block nil current-prefix-arg) "Insert above")
     ("b" (jupyter-org-insert-src-block t current-prefix-arg) "Insert below")
     ("l" org-babel-remove-result "Clear result")
     ("L" jupyter-org-clear-all-results "Clear all results")
     ("h" jupyter-org-edit-header "Header"))
    "Misc"
    (("/" jupyter-org-inspect-src-block "Inspect")
     ("r" org-babel-hide-result-toggle "Toggle result")
     ("C-s" org-babel-jupyter-scratch-buffer "Scratch")
     ("i" org-toggle-inline-images "Toggle images")
     ("t" org-babel-tangle "Tangle"))))
  :bind
  (:map jupyter-org-interaction-mode-map ("C-c j" . jupyter-hydra/body)))

(use-package orgit
  :straight (:host github
                   :repo "magit/orgit"
                   :files ("orgit.el")))

(use-package org-pandoc-import
  :straight (:host github
                   :repo "tecosaur/org-pandoc-import"
                   :files ("*.el" "filters" "preprocessors")))

(use-package verb
  :after org
  :custom
  (verb-babel-timeout 600)
  :config
  (add-to-list 'org-babel-load-languages '(verb . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(unbind-key "C-c h" global-map)

(use-package org-roam
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
         ("C-c n s" . org-store-link)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode)
  :commands (org-roam-node-list))

(use-package ivy-bibtex)

(use-package org-msg
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
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-ref
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

(use-package org-roam-bibtex
  :after (org-roam org-ref)
  :config
  (require 'org-ref))

(use-package org-variable-pitch)

(use-package ob-async
  :after jupyter
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia" "bash")))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-pomodoro
  :custom
  (org-pomodoro-audio-player "/usr/bin/play")
  (org-pomodoro-finished-sound-args "-v 0.2")
  (org-pomodoro-short-break-sound-args "-v 0.2"))

(use-package ox-gfm
  :config
  (require 'ox-gfm nil t))

(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "~/.local/bin/mmdc"))

(use-package ox-reveal)

(use-package ox-slack)

(use-package ox-pandoc
  :custom
  (org-pandoc-options-for-gfm '((wrap . "none"))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package gptel
  :custom
  (gptel-api-key (plist-get (nth 0 (auth-source-search :max 1 :host "openai.com")) :secret))
  (gptel-default-mode 'org-mode))

(use-package eshell
  :after eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-git-prompt)

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind
  (:map vterm-mode-map ("C-<backspace>" . (lambda () (interactive) (vterm-send-key (kbd "C-w"))))))

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

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after (counsel ivy projectile)
  :config (counsel-projectile-mode))

(use-package mastodon
  :custom
  (mastodon-instance-url "https://mastodon.sdf.org")
  (mastodon-active-user "rbgb"))

(use-package ement)

(use-package erc
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "rbgb")
  (erc-user-full-name "Robert Gibboni")
  (erc-track-shorten-start 8)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs")))
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury))

(use-package elpher)

(use-package which-key)

(use-package eww
  :bind (:map eww-mode-map ("C-<return>" . eww-open-in-new-buffer))
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-secondary-browser-function 'browse-url-firefox))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2))

(use-package company-box
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
         (bicep-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
  :custom
  ;; (lsp-enable-which-key-integration t)
  (lsp-idle-delay 0.25)
  (lsp-log-io nil)
  :init (setq lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil
              lsp-keymap-prefix "C-c l"
              lsp-lens-enable t
              lsp-signature-auto-activate nil)
  :config
  (lsp-register-custom-settings
   '(
     ("pylsp.plugins.black.enabled" t t)
     ("pylsp.plugins.isort.enabled" t t)
     ("pylsp.plugins.pycodestyle.enabled" t t)
     ("pylsp.plugins.pyflakes.enabled" t t)
     ("pylsp.plugins.ruff.enabled" t t)
     ("pylsp.plugins.flake8.enabled" nil)
     ("pylsp.plugins.mccabe.enabled" nil)
     ("pylsp.plugins.pydocstyle.enabled" nil))))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

;; lsp-doctor suggests
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package yasnippet
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

;; python
(use-package python
  :hook
  (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i")
  (python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
  (python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
  (python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  :config
  (defhydra python:hydra (python-mode-map "C-c h" :color pink)
    ("n" end-of-defun "Next" :column "Navigate")
    ("p" beginning-of-defun "Previous")
    ("s" counsel-imenu "Search")
    ("q" nil "Quit" :color blue :column "Quit")))

(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/anaconda3/envs/")
  :config
  (pyvenv-mode 1))

(use-package dap-mode
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-mode 1)
  (require 'dap-python))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package python-black
  :after python)

(use-package docker
  :bind ("C-c d" . docker))

(use-package keychain-environment)

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

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(use-package livedown
  :custom
  (livedown-autostart nil)
  (livedown-browser nil)
  (livedown-open t)
  (livedown-port 1337))

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

(use-package mermaid-mode)

(use-package bicep-mode
  :straight (:type git :host github :repo "christiaan-janssen/bicep-mode"))

(use-package yaml-mode
  :config
  :bind (:map yaml-mode-map ("C-c C-j" . counsel-imenu)))

(use-package scad-mode
  :custom
  (scad-preview-image-size '(800 . 800))
  (scad-preview-window-size 90)
  :defines scad-preview-image-size scad-preview-window-size)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package dockerfile-mode)

(use-package lua-mode)

(use-package kotlin-mode)

(use-package stan-mode)

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

(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))

;; (use-package vue-mode
;;   :straight t
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp))

(use-package csv-mode)

(use-package web-mode
  :custom
  (web-mode-engines-alist '(("django"    . "\\.html?\\'")))
  :mode
  ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.vue\\'")
  :hook
  (lsp))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

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

(use-package kubernetes
  :config
  (unbind-key "C-<tab>"  kubernetes-overview-mode-map))

(use-package mu4e
  :straight ( :host github
              :repo "djcb/mu"
              :branch "release/1.10"
              :files ("build/mu4e/*")
              :pre-build (("./autogen.sh") ("make")))
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-format-flowed t)
  (mu4e-debug nil)
  (mu4e-doc-dir "/home/robert/.emacs.d/straight/repos/mu")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-headers-fields '((:empty . 2) (:human-date . 12) (:from . 22) (:subject)))
  (mu4e-headers-visible-columns 140)
  (mu4e-headers-visible-lines 50)
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check nil)
  (mu4e-maildir "~/.mail")
  (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  (mu4e-notification-support t)
  (mu4e-search-include-related nil)
  (mu4e-split-view 'horizontal)
  (mu4e-update-interval (* 10 60))
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-images t)
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
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                                 :shortname ""
                                 :function (lambda (msg) "  "))))
  (defun my/mu4e-view-message-with-message-id ()
    (interactive)
    (let ((input (read-string "Message ID: ")))
      (mu4e-view-message-with-message-id (replace-regexp-in-string "^mu4e:msgid:" "" input))))
  :bind (:map mu4e-headers-mode-map ("i" . my/mu4e-view-message-with-message-id)))

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

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

(global-hl-line-mode t)
(global-so-long-mode t)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(show-paren-mode t)
(tab-bar-mode t)
(which-function-mode t)
(which-key-mode t)
(winner-mode t)
(keychain-refresh-environment)
(desktop-read)
(pyvenv-workon "py3")

(use-package move-border
  :straight ( :host github
              :repo "ramnes/move-border"
              :branch "master"
              :files ("move-border.el")))

(use-package nano-agenda)

(pretty-hydra-define my-window (:foreign-keys warn :title "Utilities" :quit-key "q")
  ("Actions"
   (
    ("TAB" tab-bar-switch-to-tab "Switch")
    ("o" other-window "Other")
    ("x" ace-delete-window "Delete")
    ("s" ace-swap-window "Swap")
    ("a" ace-select-window "Select"))

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
(setenv "PATH" (concat "/home/robert/anaconda3/bin:" "/home/robert/go/bin:" (getenv "PATH")))

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
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(web-mode-enable-control-block-indentation t))
