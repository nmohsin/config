;; Helm customizations.

(use-package helm)

;; TODO(nadeemm): Figure out how to do all these bindings in the standard
;; use-package format. Currently they break if you bind them before
;; requiring helm-config.
(require 'helm-config)

;; Helm command prefix
(bind-key "M-h" 'helm-command-prefix)
;; Helm-M-x
(bind-key "M-x" 'helm-M-x)
;; Helm-mini
(bind-key "C-x b" 'helm-mini)
(bind-key "M-m" 'helm-mini)
;; Helm find files
(bind-key "C-x C-f" 'helm-find-files)
;; Helm kill ring
(bind-key "M-y" 'helm-show-kill-ring)
;; Bookmarks
(bind-key "M-h b" 'helm-bookmarks)
;; Mark rings
(bind-key "M-h m" 'helm-all-mark-rings)
;; Registers
(bind-key "M-h r" 'helm-register)
;; Helm Resume
(bind-key "M-h g" 'helm-resume)
;; Helm-projectile
(bind-key "C-c p h" 'helm-projectile)

(bind-key "<tab>" 'helm-execute-persistent-action helm-map)
(bind-key "C-i" 'helm-execute-persistent-action helm-map)
(bind-key "C-z" 'helm-select-action helm-map)

;; Turn on helm-mode globally.
(helm-mode 1)
;; Automatic resizing and associated config.
(helm-autoresize-mode t)
;; Projectile
(helm-projectile-on)
;; Resize
(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 15)


;; Helm-swoop
(use-package helm-swoop
  :custom
  (helm-swoop-split-direction 'split-window-horizontally "Split horizontally rather than vertically")
  (helm-swoop-use-fuzzy-match t "Fuzzy matching")
  (helm-swoop-pre-input-function (lambda () ""))
  :bind (("M-i" . helm-swoop)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

(use-package projectile
  :config
  (progn
  ;; Projectile
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)))
