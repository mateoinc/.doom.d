;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mateo Barria-Urenda"
      user-mail-address "mateobarria@gmail.com")

;; Fonts
(setq doom-font (font-spec :family "CommitMono" :size 14) ; Primary font
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 15) ; non-monospace where applicable
      doom-serif-font (font-spec :family "CommitMono") ; for 'fixed-pitch-serif' face
      doom-big-font (font-spec :family "CommitMono" :size 24)) ; for big font mode ('SPC t b')

(setq catppuccin-flavor 'frappe) ;; 'frappe or 'latte, 'macchiato, or 'mocha
(setq catppuccin-highlight-matches t)
(setq catppuccin-italic-comments t)
(setq catppuccin-italic-blockquotes t)
(setq doom-theme 'catppuccin)
(add-hook 'server-after-make-frame-hook #'catppuccin-reload)

(setq doom-themes-treemacs-theme "doom-colors")

(global-hl-todo-mode)

(add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)

(display-battery-mode)
(display-time-mode)
(timeclock-mode-line-display)

(setq display-line-numbers-type t)

(setq fancy-splash-image (concat doom-user-dir "TransparentGNU_catpuccin.png"))
(setq +doom-dashboard-banner-dir doom-user-dir )
(setq +doom-dashboard-banner-file "TransparentGNU_catpuccin.png")

(setq evil-snipe-scope 'visible)

(after! dirvish
  (setq dirvish-quick-access-entries '(
                                       ("h" "~/" "Home")
                                       ("d" "~/Documents" "Documents")
                                       ("o" "/ssh:don-elias:/home/mbarria" "don-elias")
                                       ("i" "/ssh:diego-armando:/home/mbarria" "diego-armando")
                                       ("a" "/ssh:chamaco:/home/mbarria" "chamaco")
                                       ("u" "/ssh:chupete:/home/mbarria" "chupete")
                                       ("n" "~/org" "Org-mode")
                                       )))

(use-package! org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 1.0)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
          scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(setq org-directory "~/org/")
(setq +org-capture-inbox-file (doom-path org-directory "agenda/!nbox.org"))
(setq +org-capture-projects-file (doom-path org-directory "agenda/projects.org"))
(setq +org-capture-todo-file (doom-path org-directory "agenda/todo.org"))
(setq +org-capture-toread-file (doom-path org-directory "agenda/toread.org"))
;; (setq +org-capture-notes-file (doom-path org-directory "!nbox.org"))
;; timestamp DONEs
(setq org-log-done 'time)
(after! org (setq org-agenda-files (list "~/org/agenda/!nbox.org"
                                         "~/org/agenda/todo.org"
                                         "~/org/agenda/done.org"
                                         "~/org/agenda/projects.org"
                                         "~/org/agenda/someday.org"
                                         "~/org/agenda/toread.org"
                                         "~/org/agenda/meetings.org")))

(after! org
  (setq org-capture-templates
        '(("i" "Inbox"
           entry (file +org-capture-inbox-file)
           "* TODO %?\n:Created: %T\n%i\n%a"
           :prepend t
           :empty-lines 0)
          ("t" "General To-Do"
           entry (file+headline +org-capture-todo-file "Actions")
           "* TODO [#B] %?\n:Created: %T\n%i\n%a"
           :prepend t
           :empty-lines 0)
          ("e" "Email"
           entry (file+headline +org-capture-todo-file "Emails")
           "* TODO [#B] %? :email: \n:Created: %T\n** Correspondent(s)\n***\n** Notes\n** Sub-actions \n%a"
           :prepend t
           :empty-lines 0)
          ("p" "Project"
           entry (file +org-capture-projects-file)
           "* PROJ [#B] %?\n:Created: %T\n** Collaborators(s)\n***\n** Notes\n** [0%]Actions \n*** TODO\n%a"
           :prepend t
           :empty-lines 0)
          ;; Reading List
          ("r" "Paper"
           entry (file+headline +org-capture-toread-file "Scientific Articles")
           "* TODO [#B] %?\n:Created: %T\n** Author(s)\n***\n** Notes\n** Metadata \n*** URL: \n*** DOI: \n*** Magazine: \n***Discussion: \n%a"
           :prepend t
           :empty-lines 0)
          ("m" "Meeting"
           entry (file+datetree "~/org/agenda/meetings.org")
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
           :tree-type week
           :empty-lines 0))))

;; Define a function for capturing to Inbox
(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))
;; Specific Capture for Inbox + Shortcut to inbox file
(map! :map doom-leader-notes-map
      :desc "Capture to Inbox" "i" #'org-capture-inbox
      ;; :desc "Open Inbox" "I"  (cmd! (find-file +org-capture-inbox-file))
      )

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '((?A . "🔴")
                                    (?B . "🟠")
                                    (?C . "🟢"))))

(setq org-tag-alist '(
                      ;; Ticket types
                      (:startgroup . nil)
                      ("@bug" . ?b)
                      ("@feature" . ?f)
                      (:endgroup . nil)

                      ;; Ticket flags
                      ("@emergency" . ?e)
                      ("@research" . ?r)

                      ;; Special tags
                      ("CRITICAL" . ?x)
                      ("obstacle" . ?o)

                      ;; Meeting tags
                      ("meeting" . ?m)
                      ("misc" . ?z)
                      ("planning" . ?p)
                      ("progress" . ?s)

                      ;; Context Tags
                      (:startgroup . nil)
                      ("personal" . ?g)
                      ("work" . ?t)
                      ("phone" . ?c)
                      (:endgroup . nil)

                      ;; Work Log Tags
                      ("accomplishment" . ?a)
                      ))
;; Tag colors
(setq org-tag-faces
      '(
        ("work"  . (:foreground "mediumPurple1" :weight bold))
        ("planning"   . (:foreground "royalblue1"    :weight bold))
        ("progress"  . (:foreground "forest green"  :weight bold))
        ("personal"        . (:foreground "sienna"        :weight bold))
        ("meeting"   . (:foreground "yellow1"       :weight bold))
        ("CRITICAL"  . (:foreground "red1"          :weight bold))
        )
      )

;; Agenda View "d"
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-skip-deadline-if-done t)

(after! org-agenda
  (setq org-agenda-custom-commands
        '(
          ;; Daily Agenda & TODOs
          ("d" "Daily agenda and Next Actions"

           ;; Display items with priority A
           ((tags-todo "PRIORITY=\"A\"+Actions-Someday"
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "High-priority unfinished tasks:")))

            ;; View 7 days in the calendar view
            (agenda "" ((org-agenda-span 7)))

            ;; Display items with priority B (really it is view all items minus A & C)
            (tags-todo "+Actions-Someday"
                       ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                       (air-org-skip-subtree-if-priority ?C)
                                                       (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header "ALL normal priority tasks:")))

            ;; Display items with pirority C
            (tags-todo "PRIORITY=\"C\"+Actions-Someday"
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
            )

           ;; Don't compress things (change to suite your tastes)
           ((org-agenda-compact-blocks nil)))

          ;; GTD Inbox Tray
          ("i" "Inbox"

           ;; Display TODO items tagged as inbox_tray
           ((tags-todo "+inbox_tray"
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "Inbox")))

            ))

          ("j" "Someday"

           ;; Display items with priority A
           ((tags-todo "PRIORITY=\"A\"+Someday"
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "High-priority unfinished tasks:")))

            ;; Display items with priority B (really it is view all items minus A & C)
            (tags-todo "+Someday"
                       ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                       (air-org-skip-subtree-if-priority ?C)
                                                       (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header "ALL normal priority tasks:")))

            ;; Display items with pirority C
            (tags-todo "PRIORITY=\"C\"+Someday"
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
            )

           ;; Don't compress things (change to suite your tastes)
           ((org-agenda-compact-blocks nil)))


          )))

(defun org-refile--insert-link ( &rest _ )
  (unless (string-suffix-p "!nbox.org" buffer-file-name)
    (org-back-to-heading)
    (let* ((refile-region-marker (point-marker))
           (source-link (org-store-link nil)))
      (org-insert-heading)
      (insert source-link)
      (goto-char refile-region-marker))))

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to STRT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from STRT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "STRT")
     ((and (member (org-get-todo-state) (list "STRT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "80310231-6f03-4608-bcfe-8c4d04d24b83")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(after! org-roam-capture
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+filetags:\n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain
           "%?"
           :if-new (file+head "reference/${title}.org"
                              "#+title: ${title}\n#+filetags: :Reference:\n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("b" "bibliography" plain
           "%?"
           :if-new (file+head "bibliography/${citar-citekey}.org"
                              "#+title: ${title}\n#+filetags: :Bibliography:\n#+date: %u\n#+lastmod: %u\n\n- authors :: ${citar-author}\n- date :: ${citar-date}\n- DOI :: [[https://dx.doi.org/${citar-doi}][${citar-doi}]]\n- tags ::\n\n%i\n\n* PDF Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: ../../Bib/pdfs/${citar-citekey}.pdf\n:END:")
           :immediate-finish t
           :unnarrowed t)
          ("v" "video" plain
           "%?"
           :if-new (file+head "videos/${title}.org"
                              "#+title: ${title}\n#+filetags: :Video: \n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t))))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(require 'org-roam-protocol)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(require 'oc-bibtex)
(after! citar
  (setq! citar-bibliography '(
                              "/home/mbarria/org/Bib/biochem.bib"
                              "/home/mbarria/org/Bib/biology.bib"
                              "/home/mbarria/org/Bib/forcefields.bib"
                              "/home/mbarria/org/Bib/free_energy.bib"
                              "/home/mbarria/org/Bib/graphene.bib"
                              "/home/mbarria/org/Bib/md_software.bib"
                              "/home/mbarria/org/Bib/md_theory.bib"
                              "/home/mbarria/org/Bib/membranes.bib"
                              "/home/mbarria/org/Bib/nano_other.bib"
                              "/home/mbarria/org/Bib/nanotubes.bib"
                              "/home/mbarria/org/Bib/orgchem.bib"
                              "/home/mbarria/org/Bib/physics.bib"
                              ))
  (setq org-cite-global-bibliography citar-bibliography)
  (setq! citar-library-paths '("/home/mbarria/org/Bib/pdfs/"))
  (setq! citar-notes-paths '("/home/mbarria/org/roam/reference/"))
  (setq! citar-library-file-extensions  (list "pdf"))
  (setq! citar-file-additional-files-separator "-")
  )

(map! :map doom-leader-notes-map
      :desc "Insert Citation" "k" 'citar-insert-citation
      :desc "Open Reference" "p" 'citar-open)

;; org-roam + citar config
(after! citar-org-roam
  (setq citar-org-roam-subdir "bibliography")
  (setq citar-org-roam-note-title-template "${title}")
  (setq citar-org-roam-capture-template-key "b")
  (setq citar-org-roam-template-fields
        '((:citar-title . ("title"))
          (:citar-author . ("author" "editor"))
          (:citar-date . ("date" "year" "issued"))
          (:citar-doi . ("doi"))
          (:citar-pages . ("pages"))
          (:citar-type . ("=type="))))
  )

(after! reftex
  (setq! reftex-default-bibliography '("/home/mbarria/org/Bib/Bibliography.bib"))
  )

(defun get-bibtex-from-doi (doi)
  "Get a BibTeX entry from the DOI"
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "http://dx.doi.org/%s"
       	         (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (setq bibtex-entry
     	    (buffer-substring
             (string-match "@" (buffer-string))
             (point)))
      (kill-buffer (current-buffer))))
  (insert (decode-coding-string bibtex-entry 'utf-8))
  (bibtex-fill-entry))

(use-package! ob-async)

;;--------------------------
;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;;--------------------------

(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "lastmod")))

;; Create a function to start the review
(defun el-secretario-daily-review ()
  (interactive)
  (el-secretario-start-session
   (lambda ()
     (list
      ;; Clean up folders
      (el-secretario-files)
      ;; Take care of inbox using the variable
      (el-secretario-org-make-source nil (list +org-capture-inbox-file))

      ;; Go through TODOs
      (el-secretario-org-make-source '(todo "TODO") '(list +org-capture-todo-file))))))

(defun el-secretario-inbox-review ()
  (interactive)
  (el-secretario-start-session
   (lambda ()
     (list
      ;; Take care of inbox using the variable
      (el-secretario-org-make-source nil (list +org-capture-inbox-file))
      ))))

;; (run-with-idle-timer 300 t (lambda () (unless (org-clocking-p) (el-secretario-daily-review))))

(use-package! el-secretario-org
  :after el-secretario
  :config
  (define-key el-secretario-org-keymap
              "a" '("Archive" . org-archive-subtree))
  (define-key el-secretario-org-keymap
              "t" '("State" . org-todo))
  )

(map! :map doom-leader-notes-map
      :desc "Daily Review" "d" 'el-secretario-daily-review
      :desc "Inbox Review" "I" 'el-secretario-inbox-review
      )

;; Julia
(after! julia-mode
  (set-ligatures! 'julia-mode
    ;; Functional
    :lambda        "->"
    :def           "function"
    :composition   "struct"
    :map           "Dict"
    ;; Types
    :null          "Nothing"
    :true          "true"
    :false         "false"
    :int           "Int"
    :float         "Float64"
    :str           "String"
    :bool          "Bool"
    ;; Flow
    :not           "!"
    :in            "in"
    :not-in        "!in"
    :and           "&&"
    :or            "||"
    :for           "for"
    ;; :some          "some keyword"
    :return        "return"
    ;; Other
    :union         "union"
    :intersect     "intersect"
    :pipe          "|>"
    )
  )

(map! :map doom-leader-notes-map
      :desc "Ledger" "e" (cmd! (find-file (doom-path org-directory "ledger.org"))))

(after! tex-fold
  (add-to-list 'TeX-fold-macro-spec-list '("[c]" ("cite" "bibitem" "citep" "citet" "autocite" "fullcite")))
  )

;; Each path is relative to the path of the maildir you passed to mu
(set-email-account! "gmail"
                    '((mu4e-sent-folder       . "/gmail/[Gmail]/Enviados")
                      (mu4e-drafts-folder     . "/gmail/[Gmail]/Borradores")
                      (mu4e-trash-folder      . "/gmail/[Gmail]/Papelera")
                      (mu4e-refile-folder     . "/gmail/[Gmail]/Todos")
                      (smtpmail-smtp-user     . "mateobarria@gmail.com")
                      (user-mail-address      . "mateobarria@gmail.com")    ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nSaludos,\n\nMateo Barría\n\n(Sent with Mu4e)"))
                    t)

(setq org-msg-signature "\nMateo Barria-Urenda\n\n(Sent with Mu4e+Org-msg)")

(setq +mu4e-gmail-accounts '(("mateobarria@gmail.com" . "/mateobarria")))
;; don't need to run cleanup after indexing for gmail
(setq mu4e-index-cleanup nil
      ;; because gmail uses labels as folders we can use lazy check since
      ;; messages don't really "move"
      mu4e-index-lazy-check t)
(setq mu4e-update-interval 60)
;; Set default search to my inbox; as that is what I prioritize keeping clean
(after! mu4e
  (add-to-list 'mu4e-bookmarks '(:name "Inbox" :query "maildir:/gmail/INBOX" :key 105 :favorite t))
  (mu4e-modeline-mode))
;; Show it in modeline

(after! lsp-julia
  (setq lsp-julia-package-dir nil)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.10"))

(after! projectile
  (setq projectile-project-search-path '("~/Projects/" "~/Code/" ("~/Lab" . 1))) )

(setq nu--path  "/etc/profiles/per-user/mbarria/bin/nu")
(if (file-exists-p nu--path)
    (setq vterm-shell nu--path))

(use-package! multi-vterm
  :config
  (setq multi-vterm-dedicated-window-height-percent 33))

(after! multi-vterm
  (map! :leader
        (:prefix-map ("v" . "vterm")
         :desc "Dedicated Terminal"  "o" 'multi-vterm-dedicated-toggle
         :desc "New terminal" "v" 'multi-vterm
         :desc "Next" "n" 'multi-vterm-next
         :desc "Previous" "p" 'multi-vterm-prev
         :desc "Project" "P" 'multi-vterm-project
         :desc "Rename" "r" 'multi-vterm-rename-buffer
         )))

(setq vterm-tramp-shells '(("ssh" "/bin/bash") ("docker" "/bin/sh")))

(use-package! org-pandoc-import :after org)
(use-package! ox-pandoc :after org)

(after! elfeed
  (setq elfeed-db-directory "~/.elfeed-data")
  (add-hook 'elfeed-search-mode-hook #'elfeed-update))


(use-package! elfeed-protocol
  :after elfeed
  :config
  (setq elfeed-curl-extra-arguments '("--insecure"))
  ;; setup feeds
  (setq elfeed-protocol-feeds '(("owncloud+https://admin@nc.mbarria.cl"
                                 :use-authinfo t )))

  ;; enable elfeed-protocol
  (setq elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss))
  (setq elfeed-protocol-owncloud-maxsize 1000)
  (setq elfeed-protocol-owncloud-update-with-modified-time t)
  (setq elfeed-protocol-owncloud-fetch-category-as-tag t)
  (elfeed-protocol-enable)
  )

(map! :leader
      (:prefix-map ("S" . "Spotify")
       :desc "START"    "s" 'spotify-start
       :desc "Play"     "j" 'spotify-play
       :desc "Pause"    "k" 'spotify-pause
       :desc "Next"     "l" 'spotify-next
       :desc "Previous" "h" 'spotify-prev
       ))

;; Pueue
(map! :map doom-leader-open-map
      :desc "Pueue"     "q" 'pueue
      )
(map! :map doom-leader-code-map
      :desc "Add to Pueue"     "q" 'pueue-add
      )

(use-package! beacon
  :config (beacon-mode 1))

(use-package! nyan-mode
  :after doom-modeline
  :config
  (setq nyan-bar-length 15
        nyan-wavy-trail t)
  (nyan-mode)
  (nyan-start-animation))

(use-package! slurm-mode)

(use-package! gptel
  :config
  ;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
  ;;   :host "localhost:11434"               ;Where it's running
  ;;   :stream t                             ;Stream responses
  ;;   :models '("llama3.1:latest"))          ;List of models
  ;; Set default
  ;; OPTIONAL configuration
  (setq
   gptel-model 'deepseek-r1
   gptel-default-mode #'org-mode
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(
                             "deepseek-r1"
                             "llama3.1:latest"
                             "deepseek-coder-v2:latest"
                             "mistral:latest"
                             "mistral-nemo:latest"
                             "gemma2:latest"
                             "gemma2:27b"))))

(map! :leader
      (:prefix-map ("l" . "gptel")
       :desc "Dedicated Buffer" "l" 'gptel
       :desc "Menu"  "m"  'gptel-menu
       :desc "Send text"  "s"  'gptel-send
       :desc "Send text (Menu)"  "S"  (cmd! (gptel-send 1))
       :desc "Add/remove region"  "a"  'gptel-add
       :desc "Add file"  "A"  'gptel-add-file
       (:mode (org-mode)
        :desc "Limit to Heading" "h" 'gptel-org-set-topic
        :desc "Set properties" "p" 'gptel-org-set-properties)))

(use-package! whisper
  :config
  (map! :desc "Voice to Text" :g "C-c i" #'whisper-run)

  (defun whisper--break-sentences (n)
    "Put a line break every N sentences."
    (catch 'return
      (while t
        (dotimes (_ n)
          (forward-sentence 1)
          (when (eobp) (throw 'return nil)))
        (insert "\n")
        (when (= (char-after) ?\ )
          (delete-horizontal-space)))))

  (add-hook 'whisper-post-process-hook
            (lambda ()
              (whisper--break-sentences 1)))

  )

(use-package! typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

(use-package! outline-indent-mode
  :config
  (add-hook 'typst-ts-mode-hook #'outline-indent-mode))

(use-package! denote
  :custom
  (denote-directory (expand-file-name "~/Documents/denotes/"))
  ;; Set medium as known keywords. Let topics be discovered.
  (denote-known-keywords '("Book" "Article" "Meeting" "Metanote"))
  (denote-date-prompt-use-org-read-date t)
  :config
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package! consult-denote
  :after denote
  :config
  (consult-denote-mode))

(map! :leader
      (:prefix-map ("d" . "Denotes")
       :desc "Backlinks"             "b"  'denote-backlinks
       :desc "Child   Note"          "c"  'denote-sequence-new-child
       :desc "Sort Dired"            "d"  'denote-sort-dired
       :desc "Sequence Dired"        "D"  'denote-sequence-dired
       :desc "Open Note"             "f"  'denote-open-or-create
       :desc "Search Note"           "F"  'consult-denote-grep
       ;; :desc "Open Note"             "f"  'consult-denote-find
       :desc "Link"                  "l"  'denote-link
       :desc "Multiple Links"        "L"  'denote-add-links
       :desc "Denote"                "n"  'denote
       :desc "Parent  Note"          "p"  'denote-sequence-new-parent
       :desc "Sibling Note"          "s"  'denote-sequence-new-sibling
       :desc "Rename"                "r"  'denote-rename-file
       :desc "Front Matter Rename"   "R"  'denote-rename-file-using-front-matter
       (:prefix-map ("e" . "Explore")
        :desc "Chart Keywords"        "b"  'denote-explore-barchart-keywords
        :desc "Chart Degrees"         "B"  'denote-explore-barchart-degree
        :desc "Count Notes"           "c"  'denote-explore-count-notes
        :desc "Count Keywords"        "C"  'denote-explore-count-keywords
        :desc "Identify Duplicates"   "d"  'denote-explore-duplicate-notes
        :desc "Isolated notes"        "i"  'denote-explore-isolated-files
        :desc "Single-use keywords"   "k"  'denote-explore-single-keywords
        :desc "Zero keywords"         "K"  'denote-explore-zero-keywords
        :desc "Sync Metadata"         "m"  'denote-explore-sync-metadata
        :desc "Network"               "n"  'denote-explore-network
        :desc "Regenerate Network"    "N"  'denote-explore-network-regenerate
        :desc "Random note"           "r"  'denote-explore-random-note
        :desc "Rename keyword"        "R"  'denote-explore-rename-keyword
        :desc "Chart Timeline"        "t"  'denote-explore-barchart-timeline
        )))

(defun my/handle-denote-dired ()
  "Handle diredfl in denote directories."
  (when (member default-directory denote-dired-directories)
    (diredfl-mode -1)
    (dired-hide-details-mode 1)
    (denote-dired-mode 1)))

(add-hook 'dired-mode-hook #'my/handle-denote-dired 90)


