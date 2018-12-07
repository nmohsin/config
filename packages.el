;; Package manager customizations.
(require 'package)

;; Add repositories.
(eval-after-load 'package
  '(add-to-list 'package-archives
                '("MELPA" . "http://melpa.milkbox.net/packages/")))

;; Load package manager.
(package-initialize)

;; Cask and Pallet.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
