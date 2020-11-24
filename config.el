;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tianyu Gu"
      user-mail-address "macdavid313@gmail.com"
      user-login-name "macdavid313")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

;;; Common Lisp - Slime
(when (getenv "CL_DOCUMENTATION")
  (let* ((root (file-name-as-directory (getenv "CL_DOCUMENTATION")))
         (acl-doc-root (file-name-as-directory (concat root "allegro")))
         (cltl2-root (file-name-as-directory (concat root "cltl")))
         (hyperspec-root (file-name-as-directory (concat root "HyperSpec"))))

    (defun acl-doc ()
      "Quickly access AllegroCL Manual"
      (interactive)
      (eww-open-file (concat acl-doc-root "contents.htm")))

    (defun cltl2 ()
      "Quickly access Common Lisp the Language 2nd Edition"
      (interactive)
      (eww-open-file (concat (file-name-as-directory (concat cltl2-root "clm")) "node1.html")))

    (setq common-lisp-hyperspec-root (concat "file://" hyperspec-root))))

(defun sbcl-doc ()
  "Quickly access SBCL Manual"
  (interactive)
  (eww-browse-url "http://www.sbcl.org/manual/index.html"))

(use-package! slime
  :init
  (after! lisp-mode
    (set-repl-handler! 'lisp-mode #'slime-repl)
    (set-eval-handler! 'lisp-mode #'slime-eval-region)
    (set-lookup-handlers! 'lisp-mode
      :definition #'slime-edit-definition
      :documentation #'slime-describe-symbol))
  :config
  (setq slime-contribs
        '(slime-fancy slime-fuzzy slime-asdf slime-banner slime-company
                      slime-xref-browser slime-highlight-edits slime-scratch
                      slime-trace-dialog slime-mdot-fu))

  (setf slime-repl-history-file (concat doom-cache-dir "slime-repl-history")
        slime-kill-without-query-p t
        slime-default-lisp 'sbcl
        slime-protocol-version 'ignore
        slime-net-coding-system 'utf-8-unix)
  ;; slime-completion-at-point-functions 'slime-simple-completions)

  (set-popup-rules!
    '(("^\\*slime-repl"       :vslot 2 :size 0.3 :quit nil :ttl nil)
      ("^\\*slime-compilation" :vslot 3 :ttl nil)
      ("^\\*slime-traces"      :vslot 4 :ttl nil)
      ("^\\*slime-description" :vslot 5 :size 0.3 :ttl 0)
      ;; Do not display debugger or inspector buffers in a popup window. These
      ;; buffers are meant to be displayed with sufficient vertical space.
      ("^\\*slime-\\(?:db\\|inspector\\)" :ignore t)))

  (let ((quicklisp-path (concat (file-name-as-directory (concat (file-name-as-directory (getenv "HOME"))  "quicklisp"))
                                "setup.lisp"))
        (acl-path (file-name-as-directory (getenv "ACL_HOME")))
        (acl-smp-path (file-name-as-directory (getenv "ACL_SMP_HOME")))
        (agraph-client-path (getenv "AGRAPH_CLIENT"))
        (agraph-smp-client-path (getenv "AGRAPH_SMP_CLIENT")))

    (when (getenv "ACL_HOME")
      (add-to-list 'slime-lisp-implementations
                   (if agraph-client-path
                       `(alisp (,(concat acl-path "alisp") "-L" ,quicklisp-path "-L" ,agraph-client-path))
                     `(alisp (,(concat acl-path "alisp") "-L" ,quicklisp-path)))))

    (when (getenv "ACL_HOME")
      (add-to-list 'slime-lisp-implementations
                   (if agraph-client-path
                       `(mlisp (,(concat acl-path "mlisp") "-L" ,quicklisp-path "-L" ,agraph-client-path))
                     `(mlisp (,(concat acl-path "mlisp") "-L" ,quicklisp-path)))))

    (when (getenv "ACL_SMP_HOME")
      (add-to-list 'slime-lisp-implementations
                   (if agraph-client-path
                       `(alisp-smp (,(concat acl-smp-path "alisp") "-L" ,quicklisp-path "-L" ,agraph-smp-client-path))
                     `(alisp (,(concat acl-smp-path "alisp") "-L" ,quicklisp-path)))))

    (when (getenv "ACL_SMP_HOME")
      (add-to-list 'slime-lisp-implementations
                   (if agraph-client-path
                       `(mlisp-smp (,(concat acl-smp-path "mlisp") "-L" ,quicklisp-path "-L" ,agraph-smp-client-path))
                     `(mlisp (,(concat acl-smp-path "mlisp") "-L" ,quicklisp-path)))))

    (when (executable-find "sbcl")
      (add-to-list 'slime-lisp-implementations
                   `(sbcl ("sbcl" "--load" ,quicklisp-path))))

    (when (executable-find "lx86cl64")
      (add-to-list 'slime-lisp-implementations
                   `(ccl ("lx86cl64" "--load" ,quicklisp-path))))

    (when (executable-find "lisp")
      (add-to-list 'slime-lisp-implementations
                   `(cmucl ("lisp" "-load" ,quicklisp-path))))

    (when (executable-find "ecl")
      (add-to-list 'slime-lisp-implementations
                   `(ecl ("ecl" "--load" ,quicklisp-path)))))

  (let ((extras (when (require 'slime-company nil t)
                  '(slime-company))))
    (slime-setup (append '(slime-repl slime-fuzzy) extras)))

  (add-hook 'slime-mode-hook
            (lambda ()
              (interactive)
              ;; Start slime mode (slime-mode)
              ;; Some useful key-bindings
              (local-set-key [tab] 'slime-complete-symbol)
              (local-set-key (kbd "M-q") 'slime-reindent-defun)
              ;; We tell slime to not load failed compiled code
              (setq slime-load-failed-fasl 'never)))

  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-hook 'lisp-mode-hook (lambda ()
                              (unless (featurep 'slime)
                                (require 'slime)
                                (normal-mode))))

  (map! (:map slime-db-mode-map
         :n "gr" #'slime-db-restart-frame)
        (:map slime-inspector-mode-map
         :n "gb" #'slime-inspector-pop
         :n "gr" #'slime-inspector-reinspect
         :n "gR" #'slime-inspector-fetch-all
         :n "K"  #'slime-inspector-describe-inspectee)
        (:map slime-xref-mode-map
         :n "gr" #'slime-recompile-xref
         :n "gR" #'slime-recompile-all-xrefs)
        (:map lisp-mode-map
         :n "gb" #'slime-pop-find-definition-stack)

        (:localleader
         :map lisp-mode-map
         :desc "slime"          "'" #'slime
         :desc "slime (ask)"    ";" (cmd!! #'slime '-)
         :desc "Expand macro" "m" #'macrostep-expand
         (:prefix ("c" . "compile")
          :desc "Compile file"          "c" #'slime-compile-file
          :desc "Compile/load file"     "C" #'slime-compile-and-load-file
          :desc "Compile toplevel form" "f" #'slime-compile-defun
          :desc "Load file"             "l" #'slime-load-file
          :desc "Remove notes"          "n" #'slime-remove-notes
          :desc "Compile region"        "r" #'slime-compile-region)
         (:prefix ("e" . "evaluate")
          :desc "Evaluate buffer"     "b" #'slime-eval-buffer
          :desc "Evaluate last"       "e" #'slime-eval-last-expression
          :desc "Evaluate/print last" "E" #'slime-eval-print-last-expression
          :desc "Evaluate defun"      "f" #'slime-eval-defun
          :desc "Undefine function"   "F" #'slime-undefine-function
          :desc "Evaluate region"     "r" #'slime-eval-region)
         (:prefix ("g" . "goto")
          :desc "Go back"              "b" #'slime-pop-find-definition-stack
          :desc "Go to"                "d" #'slime-edit-definition
          :desc "Go to (other window)" "D" #'slime-edit-definition-other-window
          :desc "Next note"            "n" #'slime-next-note
          :desc "Previous note"        "N" #'slime-previous-note
          :desc "Next sticker"         "s" #'slime-stickers-next-sticker
          :desc "Previous sticker"     "S" #'slime-stickers-prev-sticker)
         (:prefix ("h" . "help")
          :desc "Who calls"               "<" #'slime-who-calls
          :desc "Calls who"               ">" #'slime-calls-who
          :desc "Lookup format directive" "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro"     "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos"                 "a" #'slime-apropos
          :desc "Who binds"               "b" #'slime-who-binds
          :desc "Disassemble symbol"      "d" #'slime-disassemble-symbol
          :desc "Describe symbol"         "h" #'slime-describe-symbol
          :desc "HyperSpec lookup"        "H" #'slime-hyperspec-lookup
          :desc "Who macro-expands"       "m" #'slime-who-macroexpands
          :desc "Apropos package"         "p" #'slime-apropos-package
          :desc "Who references"          "r" #'slime-who-references
          :desc "Who specializes"         "s" #'slime-who-specializes
          :desc "Who sets"                "S" #'slime-who-sets)
         (:prefix ("r" . "repl")
          :desc "Clear REPL"         "c" #'slime-repl-clear-repl
          :desc "Quit connection"    "q" #'slime-quit-lisp
          :desc "Restart connection" "r" #'slime-restart-inferior-lisp
          :desc "Sync REPL"          "s" #'slime-repl-sync)
         (:prefix ("s" . "stickers")
          :desc "Toggle breaking stickers" "b" #'slime-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers"     "c" #'slime-stickers-clear-defun-stickers
          :desc "Clear buffer stickers"    "C" #'slime-stickers-clear-buffer-stickers
          :desc "Fetch stickers"           "f" #'slime-stickers-fetch
          :desc "Replay stickers"          "r" #'slime-stickers-replay
          :desc "Add/remove sticker"       "s" #'slime-stickers-dwim)
         (:prefix ("t" . "trace")
          :desc "Toggle"         "t" #'slime-toggle-trace-fdefinition
          :desc "Toggle (fancy)" "T" #'slime-toggle-fancy-trace
          :desc "Untrace all"    "u" #'slime-untrace-all)))

  (when (featurep! :editor evil +everywhere)
    (add-hook 'slime-mode-hook #'evil-normalize-keymaps)))


(use-package! slime-company
  :init (slime-setup '(slime-company))
  :config
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))

;;;; Scheme and Geiser
(add-to-list 'auto-mode-alist '("\\.ss\\'"  . geiser-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . geiser-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . geiser-mode))

(let ((scheme (or (executable-find "scheme") (executable-find "chez"))))
  (when scheme
    (setq geiser-chez-binary scheme)))

(setq geiser-repl-autodoc-p t
      geiser-active-implementations '(chez))

(add-to-list 'auto-mode-alist '("\\.ss\\'"  . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;;; IRC Chat
(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "macdavid313"
                     :sasl-username "macdavid313"
                     :sasl-password ,(+pass-get-secret "irc/freenode.net")
                     :channels ("#lisp"))))
