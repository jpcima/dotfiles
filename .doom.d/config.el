;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jean Pierre Cimalando"
      user-mail-address "jp-dev@inbox.ru")

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
(setq doom-font (font-spec :family "Cascadia Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tango-dark)
;(setq doom-theme 'doom-one)
;(setq doom-theme 'doom-spacegrey)

(cl-case doom-theme
  (doom-one
   (setq doom-one-brighter-comments t)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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

;;------------------------------------------------------------------------------

;; Enable CUA
(cua-mode 1)

;; Tabs
(setq tab-always-indent t)
(remove-hook 'text-mode-hook 'visual-line-mode)

;; Window management
(defun jpc/select-previous-window ()
  "Select the previous window."
  (interactive)
  (select-window (previous-window)))
(defun jpc/select-next-window ()
  "Select the next window."
  (interactive)
  (select-window (next-window)))
(global-set-key (kbd "M-<left>") 'jpc/select-previous-window)
(global-set-key (kbd "M-<right>") 'jpc/select-next-window)

;; Window management keys
(require 'windmove)
(setq windmove-wrap-around t)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
;;(global-set-key (kbd "M-<left>") 'windmove-left)
;;(global-set-key (kbd "M-<right>") 'windmove-right)

;; Company
(add-hook 'company-mode-hook
          (lambda () (local-set-key (kbd "C-SPC") 'company-complete)))

;; Line numbers
(global-set-key (kbd "C-l") 'doom/toggle-line-numbers)

;; Whitespace
(require 'whitespace)
(define-global-minor-mode jpc/global-whitespace-mode whitespace-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode)
      (setq whitespace-style '(face trailing tab-mark))
      (whitespace-mode))))
(jpc/global-whitespace-mode 1)

;; Identifier colors
(defun jpc/activate-rainbow-identifiers-mode ()
  (rainbow-identifiers-mode t))
(add-hook 'prog-mode-hook #'jpc/activate-rainbow-identifiers-mode)
(add-hook 'dired-mode-hook #'jpc/activate-rainbow-identifiers-mode)
(add-hook 'wdired-mode-hook #'jpc/activate-rainbow-identifiers-mode)
(defadvice wdired-change-to-dired-mode
    (after jpc/activate-rainbow-identifiers-mode-after-wdired-change-to-dired-mode activate)
  (jpc/activate-rainbow-identifiers-mode))

;; Frames
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))

;; Smart tab
(require 'smart-tab)
(global-smart-tab-mode t)

;; Disable electric things
(setq electric-indent-mode nil)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Theme
(defun jpc/after-load-theme ()
  (cl-case doom-theme
    (tango-dark
     (set-face-attribute 'default nil :background "#2e3842")
     (set-face-attribute 'mode-line nil :background "#555753" :foreground "#eeeeec" :box '(:line-width -1 :style released-button))
     (set-face-attribute 'mode-line-inactive nil :background "#555753" :foreground "#999998" :box nil))))
(add-hook 'doom-load-theme-hook #'jpc/after-load-theme)

;; Text decorations
(defun jpc/after-load-theme/text-decorations ()
  (set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :underline t)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil :slant 'italic)
  ;(set-face-attribute 'font-lock-keyword-face nil :weight 'medium)
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold))
(add-hook 'doom-load-theme-hook #'jpc/after-load-theme/text-decorations)

;; Theme for all-the-icons-dired
(defun jpc/after-load-theme/all-the-icons-dired ()
  (cl-case doom-theme
    (tango-dark
     (set-face-attribute 'all-the-icons-dired-dir-face nil :foreground "goldenrod"))))
(eval-after-load 'all-the-icons-dired
  '(progn
     (jpc/after-load-theme/all-the-icons-dired)
     (add-hook 'doom-load-theme-hook #'jpc/after-load-theme/all-the-icons-dired)))

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Spaceline
(require 'spaceline-config)
(spaceline-emacs-theme)

;; CC mode style
(defun jpc/setup-cc-style ()
  "Configure the coding style for C-style languages."
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (when nil
    (c-set-offset 'label '*)
    (c-set-offset 'case-label '*)
    (c-set-offset 'access-label '/)))
(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook d-mode-hook))
  (add-hook hook 'jpc/setup-cc-style))

;; EditorConfig
(require 'editorconfig)
(editorconfig-mode 1)
(defadvice editorconfig-set-trailing-ws
    (around jpc/editorconfig-set-trailing-ws activate)
  "Disallow EditorConfig to mess with trailing whitespace.")

;; Quit confirmation
(setq confirm-kill-emacs nil)

;; Ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Subword
(global-subword-mode t)

;; Dired-omit
(eval-after-load 'dired-x
  '(setq dired-omit-files "^\\.$"))

;; Compilation
(setq compilation-read-command nil)

;; Projectile default commands
(defun jpc/projectile-append-to-default-command (type command &rest strings)
  (when-let* ((plist (alist-get type projectile-project-types))
              (cmd (plist-get plist command)))
    (plist-put plist command (apply #'concat cmd strings))))
(eval-after-load 'projectile
  '(let ((compile-jobs 3))
     (jpc/projectile-append-to-default-command 'make 'compile-command " -j " (number-to-string compile-jobs))
     (jpc/projectile-append-to-default-command 'cmake 'compile-command " -j " (number-to-string compile-jobs))
     (jpc/projectile-append-to-default-command 'cmake 'test-command " -j " (number-to-string compile-jobs))))

;; Projectile/Eshell
(defun jpc/create-shell-chain (&rest commands)
  (string-join (remove nil commands) " && "))
(defun jpc/run-project-in-eshell ()
  (interactive)
  (let ((default-directory (projectile-compilation-dir)))
    (+eshell/toggle nil (jpc/create-shell-chain
                         (projectile-compilation-command default-directory)
                         (projectile-run-command default-directory)))))
(defun jpc/test-project-in-eshell ()
  (interactive)
  (let ((default-directory (projectile-compilation-dir)))
    (+eshell/toggle nil (jpc/create-shell-chain
                         (projectile-compilation-command default-directory)
                         (projectile-test-command default-directory)))))
(defun jpc/compile-project-in-eshell ()
  (interactive)
  (let ((default-directory (projectile-compilation-dir)))
    (+eshell/toggle nil (projectile-compilation-command default-directory))))
(global-set-key (kbd "<f5>") #'jpc/run-project-in-eshell)
(global-set-key (kbd "<f6>") #'jpc/test-project-in-eshell)
(global-set-key (kbd "<f7>") #'jpc/compile-project-in-eshell)

;; Eshell
(require 'eshell)
(setq eshell-history-file-name nil)
(global-set-key (kbd "<f8>") #'+eshell/toggle)

;; D language
(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)
(add-hook 'd-mode-hook 'company-dcd-mode)
(set-company-backend! 'd-mode 'company-dcd)

;; Flycheck posframe
(require 'flycheck-posframe)
(flycheck-posframe-configure-pretty-defaults)
(setq flycheck-posframe-border-width 1)
(defun jpc/after-load-theme/flycheck-posframe ()
  (set-face-attribute 'flycheck-posframe-border-face nil :foreground (face-attribute 'shadow :foreground nil t)))
(add-hook 'doom-load-theme-hook #'jpc/after-load-theme/flycheck-posframe)
