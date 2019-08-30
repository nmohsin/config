;; Package manager customizations.
(require 'package)

;; Add repositories.
(eval-after-load 'package
  '(add-to-list 'package-archives
                '("MELPA" . "http://melpa.org/packages/")))

;; Load package manager.
(package-initialize)

;; Cask and Pallet.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; bind-key
(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))
(require 'bind-key)
