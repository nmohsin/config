;; Navigation customizations.

;; Fast window switching.
(bind-key "M-o" 'other-window)
(bind-key "M-O" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)

(bind-key "<f5>" 'revert-buffer)

;; IBuffer instead of the default.
(bind-key "C-x C-b" 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Pop to mark
(define-key global-map (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Smarter beginning of line.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Disable region activation for exchange-point-and-mark.
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Dired DWIM
(setq dired-dwim-target t)

(setq dired-auto-revert-buffer t)
(setq dired-isearch-filenames 1)

;; Avy
(avy-setup-default)
(bind-key "C-;" 'avy-goto-char)
(bind-key "C-:" 'avy-goto-char-2)
(bind-key "M-g l" 'avy-goto-line)
(bind-key "M-s" 'avy-goto-char-timer)

;; Winner mode
(winner-mode 1)
(bind-key "<f11>" 'winner-undo)
(bind-key "<f12>" 'winner-redo)

;; Show line numbers when using goto-line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Smartparens
(require 'smartparens-config)
(show-smartparens-global-mode)
(smartparens-global-mode)

(bind-key "C-M-f" 'sp-forward-sexp)
(bind-key "C-M-b" 'sp-backward-sexp)
(bind-key "C-M-n" 'sp-next-sexp)
(bind-key "C-M-p" 'sp-previous-sexp)

(bind-key "C-S-b" 'sp-backward-symbol)
(bind-key "C-S-f" 'sp-forward-symbol)

(bind-key "M-[" 'sp-backward-unwrap-sexp)

(bind-key "C-\\" 'sp-forward-slurp-sexp)
(bind-key "M-\\" 'sp-forward-barf-sexp)
(bind-key "C-|" 'sp-backward-slurp-sexp)
(bind-key "M-|" 'sp-backward-barf-sexp)

;; Minor remark on the behavior of transpose: It works deterministically only
;; when at the start of a word.
(bind-key "C-S-t" 'sp-transpose-sexp)

(bind-key "C-M-k" 'sp-kill-sexp)
(bind-key "C-k" 'sp-kill-hybrid-sexp)
(bind-key "M-k" 'sp-backward-kill-sexp)

;; These smartparens generalizations of C-a and C-e are my main uses, and since
;; I barely ever use backward-down-sexp, it feels more intuitive to reuse the
;; shortcut for backward-up-sexp instead.
(bind-key "C-M-d" 'sp-down-sexp)
(bind-key "C-M-u" 'sp-backward-up-sexp)
(bind-key "C-M-a" 'sp-backward-up-sexp)
(bind-key "C-M-e" 'sp-up-sexp)

;; Visual regexp
(require 'visual-regexp-steroids)
(bind-key "C-c r" 'vr/replace)
(bind-key "C-c q" 'vr/query-replace)
(bind-key "C-c m" 'vr/mc-mark)
(bind-key "C-M-r" 'vr/isearch-backward)
(bind-key "C-M-s" 'vr/isearch-forward)

(require 'multiple-cursors)
(bind-key "C-c C-]" 'mc/edit-lines)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C->" 'mc/mark-all-like-this)

;; Which-key
(require 'which-key)
(which-key-mode)

;; Undo-tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
