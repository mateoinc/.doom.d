;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mateo Barria-Urenda"
      user-mail-address "mateobarria@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'frappe) ;; 'frappe or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'catppuccin)
;; (catppuccin-reload)
;; modeline
(display-battery-mode)
(display-time-mode)
(timeclock-mode-line-display)

;; Change the logo
(setq fancy-splash-image (concat doom-user-dir "ZTMY.png"))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq +org-capture-todo-file (doom-path org-directory "!nbox.org"))

;; org-cite configuration
(require 'oc-bibtex)
(after! citar
        ;; (setq org-cite-global-bibliography '("/home/mbarria/Dropbox/org/Bibliography.bib"))
        (setq! citar-bibliography '(
                                        "/home/mbarria/Dropbox/org/Bib/nanotubes.bib"
                                        "/home/mbarria/Dropbox/org/Bib/graphene.bib"
                                        "/home/mbarria/Dropbox/org/Bib/nano_other.bib"
                                        "/home/mbarria/Dropbox/org/Bib/md_software.bib"
                                        "/home/mbarria/Dropbox/org/Bib/md_theory.bib"
                                        ))
        (setq org-cite-global-bibliography citar-bibliography)
        (setq! citar-library-paths '("/home/mbarria/Dropbox/org/roam/pdfs/"))
        (setq! citar-notes-paths '("/home/mbarria/Dropbox/org/roam/reference/"))
        (setq! citar-library-file-extensions  (list "pdf"))
        (map! :map doom-leader-notes-map
                :desc "Insert Citation" "p" 'citar-insert-citation)
)
(after! reftex
  (setq! reftex-default-bibliography '("/home/mbarria/Dropbox/org/Bibliography.bib"))
  )

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Elfeed configuration.
(map! :map doom-leader-open-map
      :desc "Elfeed (RSS)"      "e" 'elfeed
      )

(add-to-list '+doom-dashboard-menu-sections
             '("Open RSS Feed"
               :icon (all-the-icons-octicon "rss" :face 'doom-dashboard-menu-title)
               :face (:inherit (doom-dashboard-menu-title bold))
               :action elfeed))
;; Emacs spotify
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
;; Ranger
;; (map! :map doom-leader-map
;;       :desc "Ranger"     ">" 'ranger
;;       )
;; org-roam
(setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                        "${title}\n#+filetags:\n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t)
        ("r" "reference" plain
           "%?"
           :if-new (file+head "reference/${title}.org"
                        "${title}\n#+filetags: :Reference:\n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t)
        ("v" "video" plain
           "%?"
           :if-new (file+head "videos/${title}.org"
                        "${title}\n#+filetags: :Video: \n#+date: %u\n#+lastmod: %u\n\n")
           :immediate-finish t
           :unnarrowed t)))
(require 'org-roam-protocol)
;; org-roam-ui
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

;; Fonts
(setq doom-font (font-spec :family "CommitMono" :size 14)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 15)
      doom-unicode-font (font-spec :family "CommitMono")
      doom-serif-font (font-spec :family "CommitMono")
      doom-big-font (font-spec :family "CommitMono" :size 24))
;; Treemacs colors
(setq doom-themes-treemacs-theme "doom-colors")


;;;; Ligatures
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
                                        ;    :list          "list keyword"
    ;; Flow
    :not           "!"
    :in            "in"
    :not-in        "!in"
    :and           "and"
    :or            "or"
    :for           "for"
    :some          "some keyword"
    :return        "return"
    ;; :yield         "yeild"
    ;; Other
    :union         "union"
    :intersect     "intersect"
    ;; :diff          "diff keyword"
    ;; :tuple         "Tuple Keyword "
    :pipe          "|>" ;; FIXME: find a non-private char
    ;; :dot           "Dot operator"
    )
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

(map! :map doom-leader-notes-map
      :desc "Ledger" "L" (cmd! (find-file (doom-path org-directory "ledger.dat"))))

(map! (:after evil-org
        :map evil-org-mode-map
        :n "gl" #'org-down-element))

(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

;; Load EXWM.
(require 'exwm)

;; Fix problems with Ido (if you use it).
(require 'exwm-config)
;; (exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 4)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
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

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
;; (exwm-enable)


(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
;; (defun exwm-change-screen-hook ()
;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
;;         default-output)
;;     (with-temp-buffer
;;       (call-process "xrandr" nil t nil)
;;       (goto-char (point-min))
;;       (re-search-forward xrandr-output-regexp nil 'noerror)
;;       (setq default-output (match-string 1))
;;       (forward-line)
;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
;;         (call-process
;;          "xrandr" nil nil nil
;;          "--output" (match-string 1) "--primary" "--auto"
;;          "--output" default-output "--off")
;;         (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))
(add-hook 'exwm-randr-screen-change-hook
        (lambda ()
        (start-process-shell-command
        "xrandr" nil "xrandr --output HDMI-1 --right-of eDP-1 --auto")))
(exwm-randr-enable)

(add-hook!  'exwm-manage-finish-hook 'evil-emacs-state)
(push ?\M-\  exwm-input-prefix-keys)

(after! latex
  (add-to-list 'TeX-view-program-list '("Sioyek" ("sioyek %o" (mode-io-correlate " --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
)
;; Global hightlight todos
(global-hl-todo-mode)

;; mu4e
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
