;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tianyu Gu"
      user-mail-address "macdavid313@gmail.com")

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
(setq doom-theme 'doom-one)

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
    (set-repl-handler! 'lisp-mode #'slime-mrepl)
    (set-eval-handler! 'lisp-mode #'slime-eval-region)
    (set-lookup-handlers! 'lisp-mode
      :definition #'slime-edit-definition
      :documentation #'slime-describe-symbol))
  :config
  ;; package.el compiles the contrib subdir, but the compilation order
  ;; causes problems, so we remove the .elc files there. See
  ;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
  (mapc #'delete-file
        (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

  (setq slime-contribs
        '(slime-fancy slime-fuzzy slime-asdf slime-banner slime-company
                      slime-xref-browser slime-highlight-edits slime-scratch
                      slime-trace-dialog slime-mdot-fu))

  (setf slime-default-lisp 'sbcl)

  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)

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
  )

(use-package! slime-company
  :init (slime-setup '(slime-company))
  :config
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))
