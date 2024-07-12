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

(display-battery-mode)
(display-time-mode)
(timeclock-mode-line-display)

(setq display-line-numbers-type t)

(setq fancy-splash-image (concat doom-user-dir "TransparentGNU_catpuccin.png"))
(setq +doom-dashboard-banner-dir doom-user-dir )
(setq +doom-dashboard-banner-file "TransparentGNU_catpuccin.png")

(setq evil-snipe-scope 'visible)

(setq org-directory "~/org/")
(setq +org-capture-inbox-file (doom-path org-directory "agenda/!nbox.org"))
;; (setq +org-capture-log-file (doom-path org-directory "agenda/work-log.org"))
(setq +org-capture-projects-file (doom-path org-directory "agenda/projects.org"))
(setq +org-capture-todo-file (doom-path org-directory "agenda/todo.org"))
;; (setq +org-capture-notes-file (doom-path org-directory "!nbox.org"))
;; timestamp DONEs
(setq org-log-done 'time)
(after! org (setq org-agenda-files (list "~/org/agenda/!nbox.org"
                                     "~/org/agenda/todo.org"
                                     "~/org/agenda/done.org"
                                     "~/org/agenda/projects.org"
                                     "~/org/agenda/meetings.org")))

(after! org
  (setq org-capture-templates
        '(("i" "Inbox"
           entry (file+headline +org-capture-inbox-file "Inbox")
           "* TODO %?\n:Created: %T\n%i\n%a"
           :prepend t
           :empty-lines 0)
          ("t" "General To-Do"
           entry (file+headline +org-capture-todo-file "Actions")
           "* TODO [#B] %?\n:Created: %T\n%i\n%a"
           :prepend t
           :empty-lines 0)
          ("e" "Email"
           entry (file+headline +org-capture-todo-file "Actions")
           "* TODO [#B] %? :email: \n:Created: %T\n** Correspondent(s)\n***\n** Notes\n** Sub-actions \n%a"
           :prepend t
           :empty-lines 0)
          ("p" "Project"
           entry (file +org-capture-projects-file)
           "* PROJ [#B] %?\n:Created: %T\n** Collaborators(s)\n***\n** Notes\n** [0%]Actions \n*** TODO\n%a"
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
      :desc "Open Inbox" "I"  (cmd! (find-file +org-capture-inbox-file)))

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
        ("d" "Daily agenda and all TODOs"

         ;; Display items with priority A
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))

          ;; View 7 days in the calendar view
          (agenda "" ((org-agenda-span 7)))

          ;; Display items with priority B (really it is view all items minus A & C)
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                   (air-org-skip-subtree-if-priority ?C)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))

          ;; Display items with pirority C
          (tags "PRIORITY=\"C\""
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
  )

  (map! :map doom-leader-notes-map
        :desc "Insert Citation" "p" 'citar-insert-citation
        :desc "Open Reference" "P" 'citar-open)

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

(after! lsp-julia
  (setq lsp-julia-package-dir nil)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.10"))

(after! projectile
 (setq projectile-project-search-path '("~/Projects/" "~/Code/" ("~/Lab" . 1))) )

(setq nu--path  "/etc/profiles/per-user/mbarria/bin/nu")
(if (file-exists-p nu--path)
    (setq vterm-shell nu--path))

(use-package! org-pandoc-import :after org)
(use-package! ox-pandoc :after org)

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

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(require 'exwm)
(require 'exwm-config)
(setq exwm-workspace-number 4)
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-t] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(add-hook!  'exwm-manage-finish-hook 'evil-emacs-state)
(push ?\M-\  exwm-input-prefix-keys)

(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-1 --right-of eDP-1 --auto")))
(exwm-randr-enable)

(use-package! slurm-mode)
