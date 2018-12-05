;; Helm  customizations.

(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Helm-swoop
(require 'helm-swoop)

(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-use-fuzzy-match t)
(setq helm-swoop-pre-input-function
      (lambda () ""))

(bind-key "M-i" 'helm-swoop)
(bind-key "C-c M-i" 'helm-multi-swoop)
(bind-key "C-x M-i" 'helm-multi-swoop-all)

;; Helm kill ring
(bind-key "M-y" 'helm-show-kill-ring)

;; Helm-mini
(bind-key "M-m" 'helm-mini)

;; Helm bookmarks
(require 'helm-config)
(bind-key "C-x r b" 'helm-bookmarks)

;; Helm mark rings
(bind-key "C-x n r" 'helm-all-mark-rings)
