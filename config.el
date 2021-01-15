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

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; Enable time in the mode-line
(display-time-mode 1)
(setq display-time-format "%R%p (%Z)")

(setq display-time-world-list
      '(("America/Los_Angeles" "San Francisco")
        ("Asia/Bangkok" "Bangkok")
        ("Europe/Kiev" "Kiev")
        ("Asia/Tokyo" "Tokyo")))

(defun display-time-world-minibuffer ()
  "Like `display-time-world', but just display it in the minibuffer."
  (interactive)
  (message
   (with-temp-buffer
     (display-time-world-display display-time-world-list)
     (buffer-string))))

;; Iterate through CamelCase words
(global-subword-mode 1)

;; ** Start maximised/fullscreen (cross-platf)
(add-hook 'window-setup-hook
          (if (eq initial-window-system 'x)
              'toggle-frame-maximized
            'toggle-frame-fullscreen)
          t)

;; make emacs always use its own browser for opening URL links
(setq browse-url-browser-function 'eww-browse-url)

;; open URL in a new buffer
(when (fboundp 'eww)
  (defun doom/rename-eww-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode)
        (if $title
            (rename-buffer (concat "eww " $title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'doom/rename-eww-buffer))

;; slightly nicer default buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

;; ** Don't ask to quit
(setq confirm-kill-emacs nil)

;;; line number
(setq display-line-numbers-type 'relative)

;;; Projectile
(setq projectile-indexing-method 'alien)
(setq projectile-sort-order 'recentf)

;;; Set the minimum characters to 3 before a search is fired
(after! ivy
  (setq ivy-more-chars-alist '((counsel-grep . 3)
                               (counsel-rg . 3)
                               (counsel-search . 3)
                               (t . 3))))

;;; company
(use-package! company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  ;; make aborting less annoying.
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000))

;;; LSP
(add-hook 'lsp-mode-hook
          (lambda ()
            (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
            (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")))

;;; Common Lisp and Slime
(eval-after-load 'autoinsert
  '(define-auto-insert
     '(lisp-mode . "Common Lisp Header")
     '("Short description: "
       (concat ";; -*- mode: Lisp; tab-width: 2 -*-\n;; "
               (file-name-nondirectory (buffer-file-name))
               "\n"
               ";; \n"
               ";; \n"
               ";; See the file LICENSE for the full license governing this code.\n\n"))))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

(when (getenv "CL_DOCUMENTATION")
  (let* ((root (file-name-as-directory (getenv "CL_DOCUMENTATION")))
         (acl-doc-root (file-name-as-directory (concat root "allegro")))
         (cltl2-root (file-name-as-directory (concat root "cltl"))))

    (defun acl-doc ()
      "Quickly access AllegroCL Manual"
      (interactive)
      (eww-open-file (concat acl-doc-root "contents.htm")))

    (defun cltl2 ()
      "Quickly access Common Lisp the Language 2nd Edition"
      (interactive)
      (eww-open-file (concat (file-name-as-directory (concat cltl2-root "clm")) "node1.html")))))

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
  (when (getenv "HYPERSPEC_ROOT")
    (let* ((hyperspec-root (file-name-as-directory (getenv "HYPERSPEC_ROOT")))
           (hyperspec-symbol-table (concat hyperspec-root "Data/Map_Sym.txt"))
           (hyperspec-issuex-table (concat hyperspec-root "Data/Map_IssX.txt")))
      (setq common-lisp-hyperspec-root hyperspec-root
            common-lisp-hyperspec-symbol-table hyperspec-symbol-table
            common-lisp-hyperspec-issuex-table hyperspec-issuex-table)))

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
    '(("^\\*slime-repl"        :vslot 2 :size 0.3 :quit nil :ttl nil)
      ("^\\*slime-compilation" :vslot 3 :ttl nil)
      ("^\\*slime-traces"      :vslot 4 :ttl nil)
      ("^\\*slime-description" :vslot 5 :size 0.3 :ttl 0)
      ;; Do not display debugger or inspector buffers in a popup window. These
      ;; buffers are meant to be displayed with sufficient vertical space.
      ("^\\*slime-\\(?:db\\|inspector\\)" :ignore t)))

  (let* ((quicklisp-home (concat (file-name-as-directory (getenv "HOME")) "quicklisp"))
         (quicklisp-path (when (file-exists-p quicklisp-home)
                           (concat (file-name-as-directory quicklisp-home)
                                   "setup.lisp")))
         (agraph-client-path (getenv "AGRAPH_CLIENT"))
         (agraph-smp-client-path (getenv "AGRAPH_SMP_CLIENT")))

    (when (getenv "ALISP")
      (add-to-list 'slime-lisp-implementations
                   `(alisp ,(remove nil
                                    `(,(getenv "ALISP")
                                      ,@(when quicklisp-path `("-L" ,quicklisp-path)))))))

    (when (getenv "MLISP")
      (add-to-list 'slime-lisp-implementations
                   `(mlisp ,(remove nil
                                    `(,(getenv "MLISP")
                                      ,@(when quicklisp-path `("-L" ,quicklisp-path))
                                      ,@(when agraph-client-path `("-L" ,agraph-client-path)))))))

    (when (getenv "ALISP_SMP")
      (add-to-list 'slime-lisp-implementations
                   `(alisp-smp ,(remove nil
                                        `(,(getenv "ALISP_SMP")
                                          ,@(when quicklisp-path `("-L" ,quicklisp-path)))))))

    (when (getenv "MLISP_SMP")
      (add-to-list 'slime-lisp-implementations
                   `(mlisp-smp ,(remove nil
                                        `(,(getenv "MLISP_SMP")
                                          ,@(when quicklisp-path `("-L" ,quicklisp-path))
                                          ,@(when agraph-smp-client-path `("-L" ,agraph-smp-client-path)))))))

    (when (executable-find "sbcl")
      (add-to-list 'slime-lisp-implementations
                   (if quicklisp-path
                       `(sbcl ("sbcl" "--load" ,quicklisp-path))
                     `(sbcl ("sbcl" "--noinform")))))

    (when (executable-find "lx86cl64")
      (add-to-list 'slime-lisp-implementations
                   (if quicklisp-path
                       `(ccl ("lx86cl64" "--load" ,quicklisp-path))
                     `(ccl ("lx86cl64")))))

    (when (executable-find "lisp")
      (add-to-list 'slime-lisp-implementations
                   (if quicklisp-path
                       `(cmucl ("lisp" "-load" ,quicklisp-path))
                     `(cmucl ("lisp")))))

    (when (executable-find "ecl")
      (add-to-list 'slime-lisp-implementations
                   (if quicklisp-path
                       `(ecl ("ecl" "-load" ,quicklisp-path))
                     `(ecl ("ecl"))))))

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

  ;; for Allegro CL source code
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
          :desc "Untrace all"    "u" #'slime-untrace-all))))

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

;;;; Paredit
(use-package! paredit
  :config
  ;; hook lisp modes
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  ;; Electric RETURN
  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
      (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))
  ;; Using local-set-key in a mode-hook is a better idea.
  (global-set-key (kbd "RET") 'electrify-return-if-match))

;;;; IRC Chat
(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "macdavid313"
                     :sasl-username "macdavid313"
                     :sasl-password ,(+pass-get-secret "irc/freenode.net")
                     :channels ("#lisp"))))
