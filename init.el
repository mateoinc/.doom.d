;;; init.el -*- lexical-binding: t; -*-

(doom!

       :completion
       ;; (company +childframe)           ; the ultimate code completion backend
       (corfu +icons +orderless +dabbrev)           ; the ultimate code completion backend
       (vertico +icons +childframe)    ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       modeline          ; snazzy, Atom-inspired modeline, plus API
       workspaces        ; tab emulation, persistence & separate workspaces
       (smooth-scroll +interpolate)  ; So smooth you won't believe it's not butter

       (window-select +numbers)     ; visually switch windows

       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       ophints           ; highlight the region an operation acts on
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (popup +defaults +all)   ; tame sudden yet inevitable temporary windows
       nav-flash         ; blink cursor line after big motions

       (ligatures +extra)         ; ligatures and symbols to make your code pretty again

       (treemacs +lsp)          ; a project drawer, like neotree but cooler

       (emoji +ascii  +github +unicode)

       :editor
       (evil +everywhere); come to the dark side, we have cookies

       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of

       :emacs
       (dired +icons +dirvish)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell +hunspell) ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       biblio                ; Writes a PhD for you (citation needed)
       editorconfig          ; let someone else argue about tabs vs spaces
       ein                   ; tame Jupyter notebooks with emacs
       (eval +overlay)       ; run code, run (also, repls)
       (lookup +dictionary)  ; navigate your code and its documentation
       lsp                   ; M-x vscode
       magit                 ; a git porcelain for Emacs
       make                  ; run make tasks from Emacs
       (pass +pass)          ; password manager for nerds
       pdf                   ; pdf enhancements
       tmux                  ; an API for interacting with tmux
       tree-sitter
       upload                ; map local to remote projects via ssh/ftp

       :os
       tty               ; improve the terminal Emacs experience

       :lang
       (cc +lsp +tree-stitter)         ; C > C++ == 1
       emacs-lisp        ; drown in parentheses
       (julia +lsp +tree-sitter +snail)             ; a better, faster MATLAB
       (latex +latexmk +cdlatex +lsp +fold)             ; writing papers in Emacs has never been so fun
       (nix +tree-sitter +lsp)               ; I hereby declare "nix geht mehr!"
       (python +lsp +tree-sitter +pyenv)           ; beautiful is better than ugly
       (sh +lsp)               ; she sells {ba,z,fi}sh shells on the C xor
       lua                 ; one-based indices? one-based indices

       data              ; config/data formats
       ledger            ; be audit you can be
       markdown          ; writing docs for people to ignore

       (org                ; organize your plain life in plain text
            +pretty        ; prettier defaults
            +dragndrop     ; drag images to org files
            +gnnuplot      ; Render images from gnuplot / plot org-tables
            +org-noter     ; Take notes of documents
            +hugo          ; Use Hugo to export websites
            +present       ; Use org-mode for presentations
            +roam2         ; A Zettelkasten for Emacs
            +journal       ; for keeping a work diary
            +pomodoro)     ; Use org-mode timers for productivity

       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;kotlin            ; a better, slicker Java(Script)
       ;;lean              ; for folks with too much to prove
       ;;nim               ; python + lisp at the speed of c
       ;;ocaml             ; an objective camel
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       (mu4e +org +gmail)

       :app
       calendar
       everywhere        ; *leave* Emacs!? You must be joking
       (rss +org +youtube)        ; emacs as an RSS reader

       :config
       ;; literate
       (default +bindings +smartparens)

)
