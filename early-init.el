;;; early-init.el --- Pre-initialization file for Emacs

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup screen
(setq inhibit-startup-message t)

;; Full screen
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

(setq gc-cons-threshold 100000000)

(add-to-list 'default-frame-alist
             '(background-color . "#292b2e"))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
