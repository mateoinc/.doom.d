;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mateo Barria-Urenda"
      user-mail-address "mateobarria@gmail.com")

;; Fonts
(setq doom-font (font-spec :family "CommitMono" :size 14) ; Primary font
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 15) ; non-monospace where applicable
      doom-serif-font (font-spec :family "CommitMono") ; for 'fixed-pitch-serif' face
      doom-big-font (font-spec :family "CommitMono" :size 24)) ; for big font mode ('SPC t b')

(setq catppuccin-flavor 'frappe) ;; 'frappe or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'catppuccin)

(setq doom-themes-treemacs-theme "doom-colors")

(global-hl-todo-mode)

(display-battery-mode)
(display-time-mode)
(timeclock-mode-line-display)

(setq display-line-numbers-type t)

(setq fancy-splash-image (concat doom-user-dir "CyberpunkGirl.png"))
(setq +doom-dashboard-banner-dir doom-user-dir )
(setq +doom-dashboard-banner-file "CyberpunkGirl.png")

(setq evil-snipe-scope 'visible)

(setq org-directory "~/Dropbox/org/")
(setq +org-capture-todo-file (doom-path org-directory "!nbox.org"))
(setq +org-capture-notes-file (doom-path org-directory "!nbox.org"))

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title:${title}\n#+filetags:\n#+date: %u\n#+lastmod: %u\n\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain
         "%?"
         :if-new (file+head "reference/${title}.org"
                            "#+title:${title}\n#+filetags: :Reference:\n#+date: %u\n#+lastmod: %u\n\n")
         :immediate-finish t
         :unnarrowed t)
        ("v" "video" plain
         "%?"
         :if-new (file+head "videos/${title}.org"
                            "#+title:${title}\n#+filetags: :Video: \n#+date: %u\n#+lastmod: %u\n\n")
         :immediate-finish t
         :unnarrowed t)))

;; Add property "type" to notes
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
;; Add this property to displays
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; auto tag new notes as drafts
(defun jethro/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))
(add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)

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
                              "/home/mbarria/Dropbox/org/Bib/biochem.bib"
                              "/home/mbarria/Dropbox/org/Bib/biology.bib"
                              "/home/mbarria/Dropbox/org/Bib/forcefields.bib"
                              "/home/mbarria/Dropbox/org/Bib/free_energy.bib"
                              "/home/mbarria/Dropbox/org/Bib/graphene.bib"
                              "/home/mbarria/Dropbox/org/Bib/md_software.bib"
                              "/home/mbarria/Dropbox/org/Bib/md_theory.bib"
                              "/home/mbarria/Dropbox/org/Bib/nano_other.bib"
                              "/home/mbarria/Dropbox/org/Bib/nanotubes.bib"
                              "/home/mbarria/Dropbox/org/Bib/orgchem.bib"
                              "/home/mbarria/Dropbox/org/Bib/physics.bib"
                            ))
  (setq org-cite-global-bibliography citar-bibliography)
  (setq! citar-library-paths '("/home/mbarria/Dropbox/org/roam/pdfs/"))
  (setq! citar-notes-paths '("/home/mbarria/Dropbox/org/roam/reference/"))
  (setq! citar-library-file-extensions  (list "pdf"))
  (map! :map doom-leader-notes-map
        :desc "Insert Citation" "p" 'citar-insert-citation
        :desc "Open Reference" "P" 'citar-open)
  )

(after! reftex
  (setq! reftex-default-bibliography '("/home/mbarria/Dropbox/org/Bib/Bibliography.bib"))
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
    :true          "True"
    :false         "False"
    :int           "Int"
    :float         "Float64"
    :str           "String"
    :bool          "Bool"
    ;; Flow
    :not           "!"
    :in            "in"
    :not-in        "!in"
    :and           "and"
    :or            "or"
    :for           "for"
    :some          "some keyword"
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

(use-package! elcord
  :config
  (setq elcord-editor-icon "emacs_material_icon")
  (elcord-mode)
  )
