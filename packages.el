;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)

(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
         (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
         :files
         (:defaults "etc")
         :build t :pre-build
         (with-temp-file "org-version.el"
           (require 'lisp-mnt)
           (let
               ((version
                 (with-temp-buffer
                   (insert-file-contents "lisp/org.el")
                   (lm-header "version")))
                (git-version
                 (string-trim
                  (with-temp-buffer
                    (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                    (buffer-string)))))
             (insert
              (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
              (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
              "(provide 'org-version)\n"))))
  :pin nil)

(unpin! org)

(unpin! org-roam)
(package! org-roam-ui)

(package! ob-async
  :recipe (:host github
           :repo "astahlman/ob-async"))

(package! git-auto-commit-mode)

(package! el-secretario)
(package! el-secretario-org)
(package! el-secretario-mu4e)
(package! el-secretario-elfeed)

(package! multi-vterm)

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! ox-pandoc)

(package! elfeed-dashboard)

(package! spotify
  :recipe (:host github
           :repo "SnootierMoon/emacs-spotify"))

(package! pueue
  :recipe (:host github
           :repo "xFA25E/pueue"))

(package! beacon)

(package! nyan-mode)

(package! slurm-mode)

(package! gtpel
  :recipe (:host github :repo "karthink/gptel"))

(package! typst-ts-mode
  :recipe (:host codeberg :repo "meow_king/typst-ts-mode"))

(package! outline-indent-mode
  :recipe (:host sourcehut :repo "meow_king/outline-indent-mode"))

(package! denote)
(package! consult-denote) ; consult integration for denote
(package! denote-explore) ; more options for exploring denote files
