;;; init.el --- GNU Emacs initialization file
;;; Commentary:
;;; Code:

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Window management
(defun jpc/select-previous-window ()
  "Select the previous window."
  (interactive)
  (select-window (previous-window)))
(defun jpc/select-next-window ()
  "Select the next window."
  (interactive)
  (select-window (next-window)))

;; Window management keys
(global-set-key (kbd "M-<left>") 'jpc/select-previous-window)
(global-set-key (kbd "M-<right>") 'jpc/select-next-window)

;; Mouse wheel
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase)

;; Smart tab
(require 'smart-tab)
(global-smart-tab-mode t)

;; Ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Dired
(require 'dired-x)
(add-hook 'dired-mode-hook
          (lambda () (dired-omit-mode t)))
(setq dired-omit-files "^\\.$")
(defun jpc/dired-omit-add (pattern)
  "Add a PATTERN to be omitted by dired listings."
  (setq dired-omit-files (concat dired-omit-files "\\|" pattern)))
(jpc/dired-omit-add "\\.o$\\|\\.obj$")
(jpc/dired-omit-add "\\.a$\\|\\.lib$")
(jpc/dired-omit-add "\\.so$\\|\\.so\\.\\|\\.dll$\\|\\.dylib$")
(jpc/dired-omit-add "^\\..*\\.swp$")

;; Truncate lines
(setq-default truncate-lines t)

;; Fill column
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook
          (lambda () (interactive)
            (fci-mode (if (derived-mode-p 'prog-mode) 1 0))))

;; Open .h files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Consider .tcc as C++ files
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))

;; Git
(jpc/dired-omit-add "^\\.git$")

;; Company
(require 'company)
(defun jpc/company-local-key-enable (backends)
  "Enable company completion using BACKENDS, and enable the shortcut key in the local buffer."
  (company-mode t)
  (set (make-local-variable 'company-backends) backends)
  (local-set-key (kbd "C-SPC") 'company-complete))

;; Company workaround for fci-mode (#180)
(defvar-local jpc/company-fci-mode-on-p nil)
(defun jpc/company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq jpc/company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun jpc/company-maybe-turn-on-fci (&rest ignore)
  (when jpc/company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'jpc/company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'jpc/company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'jpc/company-maybe-turn-on-fci)

;; Company for Emacs Lisp
(require 'company-elisp)
(defun jpc/company-elisp-mode-hookfn ()
  "Set up company hook for Emacs Lisp."
  (interactive)
  (jpc/company-local-key-enable '(company-elisp)))
(add-hook 'emacs-lisp-mode-hook 'jpc/company-elisp-mode-hookfn)

;; Company for C and C++
(require 'irony)
(require 'irony-cdb)
(require 'company-irony)
(require 'company-irony-c-headers)
(defun jpc/company-c-mode-hookfn ()
  "Set up company hook for C and C++."
  (interactive)
  (irony-mode t)
  (irony-cdb-autosetup-compile-options)
  (jpc/company-local-key-enable '(company-irony-c-headers company-irony)))
(add-hook 'c-mode-hook 'jpc/company-c-mode-hookfn)
(add-hook 'c++-mode-hook 'jpc/company-c-mode-hookfn)

;; Company for CMake
(require 'company-cmake)
(defun jpc/company-cmake-mode-hookfn ()
  "Set up company hook for CMake."
  (interactive)
  (jpc/company-local-key-enable '(company-cmake)))
(add-hook 'cmake-mode-hook 'jpc/company-cmake-mode-hookfn)

;; Flycheck for C and C++
(require 'flycheck)
(require 'flycheck-irony)
(defun jpc/flycheck-c-mode-hookfn ()
  "Set up flycheck hook for C and C++."
  (interactive)
  (irony-mode t)
  (flycheck-mode t)
  (irony-cdb-autosetup-compile-options)
  (set (make-local-variable 'flycheck-checkers) '(irony)))
(add-hook 'c-mode-hook 'jpc/flycheck-c-mode-hookfn)
(add-hook 'c++-mode-hook 'jpc/flycheck-c-mode-hookfn)

;; Irony default flags for C and C++
(defun jpc/irony-cdb-my-default-flags (command &rest args)
  "Run compilation database COMMAND with arguments ARGS.
This database provides default flags for C and C++."
  (let ((wdir (file-name-directory (buffer-file-name))))
    (cl-case command
      (get-compile-options
       (cl-case major-mode
         (c-mode `((("-std=gnu11") . ,wdir)))
         (c++-mode `((("-std=gnu++17") . ,wdir))))))))
(add-to-list 'irony-cdb-compilation-databases 'jpc/irony-cdb-my-default-flags t)

;; D language
(require 'flycheck-dmd-dub)
(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)
(require 'company-dcd)
(defun jpc/company-d-mode-hookfn ()
  "Set up company hook for D."
  (interactive)
  (company-dcd-mode t)
  (jpc/company-local-key-enable '(company-dcd)))
(add-hook 'd-mode-hook 'jpc/company-d-mode-hookfn)

;; Text
(require 'emojify)
(require 'company-emoji)
(defun jpc/company-text-mode-hookfn ()
  (interactive)
  (emojify-mode 1)
  (jpc/company-local-key-enable '(company-emoji)))
(add-hook 'text-mode-hook 'jpc/company-text-mode-hookfn)

;; Puredata
(jpc/dired-omit-add "\\.pd_linux$\\|\\.pd_darwin$")

;; Common Lisp
(require 'slime)
(require 'slime-company)
(defun jpc/company-lisp-mode-hookfn ()
  "Set up company hook for Lisp."
  (interactive)
  (jpc/company-local-key-enable '(company-slime)))
(when (load (expand-file-name "~/.common-lisp/quicklisp/slime-helper.el") t)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (sbcl-core ("sbcl-core")))
        slime-default-lisp 'sbcl-core)
  (add-hook 'lisp-mode-hook 'jpc/company-lisp-mode-hookfn))

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Shortcut for yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Date and time
(require 'calendar)
(defun jpc/insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
With a prefix argument, the date is inserted without the day of the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil omit-day-of-week-p)))
(global-set-key "\C-x\M-d" 'jpc/insert-current-date)

;; Load theme
(load-theme 'tango)

;; Choose the default font
(set-face-attribute 'default nil :family "Source Code Pro" :height 105)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   '((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(dired-listing-switches "-al --group-directories-first")
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(global-flycheck-mode t)
 '(global-subword-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(package-selected-packages
   '(slime-company company-emoji emojify slime spaceline powerline svg-mode-line-themes yaml-mode pkgbuild-mode julia-mode auctex company-dcd flycheck-dmd-dub d-mode fill-column-indicator company-irony-c-headers flycheck-irony flycheck cmake-mode company-irony irony company smart-tab))
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-auto-start 'always)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(whitespace-style '(face trailing tab-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
