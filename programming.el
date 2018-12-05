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
  (nadeemm/create-shell-with-name "hg-shell")
  (nadeemm/create-shell-with-name "test-shell"))

;; Company mode for completion.
;; (require 'company)
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (global-company-mode)
;; (bind-key "M-/" 'company-complete)

;; Modify the colors used by the company-mode dropdown, since my theme has a
;; dark background.
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

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

;; Cycle between snake case, camel case, etc.
(require 'string-inflection)
(define-key global-map (kbd "C-c i") 'string-inflection-cycle)
(define-key global-map (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(define-key global-map (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(define-key global-map (kbd "C-c U") 'string-inflection-underscore)        ;; Force to underscore
(define-key global-map (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles

(require 'auto-yasnippet)
(bind-key "s-w" 'aya-create)
(bind-key "s-y" 'aya-expand)
