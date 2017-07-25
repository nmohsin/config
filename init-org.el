;; Org-mode setup
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c j" 'org-clock-goto)
(setq org-return-follows-link t)

;; Nice bullets.
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; All org files can contribute to the agenda.
(setq org-agenda-files (list "~/org"))

(add-hook 'org-mode-hook
          (visual-line-mode 1))

(add-hook 'org-mode-hook
          '(lambda () (setq fill-column 100) (turn-on-auto-fill)))

(setq org-default-notes-file (concat org-directory "/capture.org"))
(bind-key "C-c n" 'org-capture)

;; Templates for org-capture. So far contains the following:
;; - Capture new general task
;; - Capture random note
;; - Start work on a new work task
;; - Continue work on an ongoing task.
(setq org-capture-templates
      '(
        ("n" "Note"
         item (file+headline "~/org/notes.org" "Capture")
         " %U%? \n")))


(setq org-capture-templates
      '(
        ("t" "Task"
         entry (file+headline "~/org/tasks.org" "Capture")
         "* TODO %? %U\n"
         :empty-lines 1)
        ("n" "Note"
         item (file+headline "~/org/notes.org" "Capture")
         " %U%? \n")
        ("s" "Start work"
         entry (file "~/org/work.org")
         "* TODO %?\n  SCHEDULED: %t\n\n** Action Items [0/3]\n   - [ ] Write\n   - [ ] Review\n   - [ ] Submit\n\n** Notes/Questions\n"
         :empty-lines 1)
        ("c" "Continue work"
         entry (clock)
         "** Commentary %T\n   %?"
         :empty-lines 1)
        ("r" "Review"
         entry (file+headline "~/org/work.org" "Reviews")
         "* TODO %?\n  SCHEDULED: %t\n\n"
         :empty-lines 1)
        ("d" "Day planning"
         entry (file+datetree "~/org/journal.org")
         "* Day Planning %T\n  %?"
         :clock-in t
         :empty-lines 1)
        ("j" "Journal"
         entry (file+datetree "~/org/journal.org")
         "* %^{Heading} %T\n  %?"
         :empty-lines 1)
        ))

;; Starting timer on a TODO task should trigger the next state.
(defun nadeemm/get-todo-state ()
  (nth 2 (org-heading-components)))

(defun nadeemm/clock-in-from-todo ()
  "Cycle a task from TODO when clocking in."
  (if (equal (nadeemm/get-todo-state) "TODO")
      (org-todo)))

(defun nadeemm/schedule-task-now ()
  "Schedule task at current time."
  (interactive)
  (let ((now (with-temp-buffer (org-time-stamp '(16)) (buffer-string))))
    (org-schedule nil now)
    (message "Scheduled started task for now")))


;; Remove zero time clocks
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when done
(setq org-clock-out-when-done t)

(add-hook 'org-clock-in-hook 'nadeemm/clock-in-from-todo)
(add-hook 'org-clock-in-hook 'nadeemm/schedule-task-now)
(add-hook 'org-clock-in-hook 'org-timer-start)
(add-hook 'org-clock-out-hook 'org-timer-stop)


;; Hide done tasks in agenda view.
(setq org-agenda-skip-scheduled-if-done t)

;; Hide leading stars everywhere
(setq org-hide-leading-stars t)

;; Have return automatically indent.
(define-key org-mode-map [remap org-return] 'org-return-indent)

;; Ordered tasks
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks 'invisible)
