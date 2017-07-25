;; Helm  customizations.

(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(bind-key "M-x" 'helm-M-x)

;; Helm-swoop
(require 'helm-swoop)

(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-use-fuzzy-match t)
(setq helm-swoop-pre-input-function
      (lambda () ""))
(bind-key "M-i" 'helm-swoop)
(bind-key "C-x M-i" 'helm-multi-swoop-all)

;; Helm kill ring
(bind-key "M-y" 'helm-show-kill-ring)

;; Helm-mini
(bind-key "M-m" 'helm-mini)
(bind-key "C-x b" 'helm-mini)

;; Helm-find-files
(bind-key "C-x C-f" 'helm-find-files)

;; Helm projectile
(require 'projectile)
(require 'helm-projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(bind-key "C-c C-f" 'helm-projectile-find-file)
(bind-key "M-h" 'helm-projectile)

;; Helm bookmarks
(require 'helm-config)
(bind-key "C-x r b" 'helm-bookmarks)
