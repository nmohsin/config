;; Ivy  customizations.

(require 'swiper)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (
   ;; Very commonly used commands.
   ("M-x" . 'counsel-M-x)
   ("C-s" . 'swiper)
   ("C-x C-f" . 'counsel-find-file)
   ;; Grep the current directory.
   ("C-c g" . 'counsel-ag)
   ;; Resume the last Ivy-based completion.
   ("C-c C-r" . 'ivy-resume)
   ;; Switch buffers.
   ("C-x b" . 'ivy-switch-buffer)))
