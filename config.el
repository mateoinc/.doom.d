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
(setq doom-theme 'doom-snazzy)

;; Change the logo
(setq fancy-splash-image (concat doom-user-dir "M-x_butterfly.png"))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")


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
;; org-tree-slide configuration
;;(define-key org-mode-map (kbd "<f8>") 'tree-org-mode-slide)
;;(define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
;; (map! :map org-mode-map
;;       :after org
;;       :n "<f8>" #'org-tree-slide-mode
;;       :n "<f9>" #'org-tree-slide-move-next-tree
;;       :n "<f7>" #'org-tree-slide-move-previous-tree
;;       :n "s-<f8>" #'org-tree-slide-skip-done-toggle)

;; Fonts
;;
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 15)
      doom-unicode-font (font-spec :family "Fira Code")
      doom-big-font (font-spec :family "Fira Code" :size 24))
;; Treemacs colors
(setq doom-themes-treemacs-theme "doom-colors")
