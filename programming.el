;; Customizations pertaining to writing code.

(setq ansi-color-faces-vector
      [default bold shadow italic underline bold bold-italic bold])

(global-hl-line-mode 1)

;; Make navigation easier for camel casing.
(global-subword-mode 1)

;; Auto-indent.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Expand/contract region.
(require 'expand-region)
(bind-key "C-=" 'er/expand-region)
(bind-key "C--" 'er/contract-region)

;; Trailing whitespace
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Delete extra blank lines at the end of a file
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

;; Shells for programming.
(defun nadeemm/create-shell-with-name (name)
  "Create shell named NAME."
  (unless (get-buffer name)
    (progn
      (shell)
      (switch-to-buffer "*shell*")
      (rename-buffer name))))

(defun nadeemm/init-shells ()
  (interactive)
  (nadeemm/create-shell-with-name "g4-shell")
  (nadeemm/create-shell-with-name "test-shell"))

;; Company mode for completion.
(require 'company)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(global-company-mode)
(bind-key "M-/" 'company-complete)


(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))


(bind-key "M-+" 'increment-number-at-point)


;; Copy current buffer's working directory to clipboard.
(defun copy-pwd-to-clipboard ()
  "Copy the current buffer's working directory to clipboard."
  (interactive)
  (kill-new default-directory))

(bind-key "C-c C-d" 'copy-pwd-to-clipboard)
