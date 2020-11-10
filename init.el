;;; Main initialization file.

;; Config directory. Files in config/ are for configuration items that should be
;; the same on every machine. Accordingly, they are under version control.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq config-directory
      (concat user-emacs-directory (convert-standard-filename "config/")))

;; Local config directory. Files in config/local are for machine-specific
;; tweaks and are ignored by the version control system.
(setq local-config-directory
      (concat config-directory (convert-standard-filename "local/")))

(add-to-list 'load-path config-directory)
(add-to-list 'load-path local-config-directory)

;; Stuff configured via the customization UI, not version-controlled.
;; Only used for machine-specific things.
;; Load this first, so that themes are treated as safe.
(setq custom-file
      (concat local-config-directory (convert-standard-filename "custom.el")))
(load custom-file 'noerror)

(setq customizations
      (list
       "packages"
       "init-evil"
       "init-org"
       "look-and-feel"
       "init-helm"
       ;; "init-ivy"
       "navigation"
       "programming"
       ))

(defun load-customizations (directory customizations noerror)
  (dolist (customization customizations)
    (load (concat directory customization) noerror)))

(load-customizations config-directory customizations nil)
(load-customizations local-config-directory customizations t)


(server-start)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
