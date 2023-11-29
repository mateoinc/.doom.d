;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)

(unpin! org-roam)
(package! org-roam-ui)

(package! ob-async
  :recipe (:host github
           :repo "astahlman/ob-async"))

(package! spotify
  :recipe (:host github
           :repo "SnootierMoon/emacs-spotify"))

(package! pueue
  :recipe (:host github
           :repo "xFA25E/pueue"))

(package! beacon)

(package! nyan-mode)

(package! exwm)

(package! desktop-environment
  :recipe (:host github
           :repo "DamienCassou/desktop-environment"))

(package! elcord)
