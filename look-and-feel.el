;; Look and feel customizations.

;; Get rid of pointless UI.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-screen t)

;; Add column numbers.
(column-number-mode 1)

;; Smooth scrolling.
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Enter should indent when necessary.
(bind-key "RET" 'newline-and-indent)

;; Display current buffer name in title bar
(setq frame-title-format "%b (%I) - %F")

;; Prevent sudden closing by accident.
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

;; Prompt when exit sequence is hit.
(when window-system
  (bind-key "C-x C-c" 'ask-before-closing))

;; Single letter yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display time in status bar.
(display-time)

;; Revert buffers automatically.
(global-auto-revert-mode 1)

;; Fix annoying backup files.
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
(setq delete-old-versions t)

;; Smart Mode Line settings.
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(setq sml/shorten-directory t
      sml/shorten-modes t
      sml/name-width 25
      sml/mode-width 'full)

(setq rm-blacklist '(" AC" " SP" " mate" " Plugged" " Gtags" " Abbrev" " Fill" " Guide"))

;; Set up themes
(load-theme 'zenburn t)
