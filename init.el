;;; init.el -- baol's dotemacs file
;;;
;;; Running on GNU/Emacs 25
;;;
;;; A collection yanked after killing someone else's buffer.
;;;
;;; Commentary:
;;;             A dotemacs for C++/HTML/python/robot-framework with
;;;             (almost) consistent usage of company and key-bindings.
;;;
;;;             Most notably:
;;;
;;;             M-/  completes
;;;             M-.  goes to definition/declaration
;;;             M-,  next
;;;             M-[  goes back
;;;
;;; Code:

(setq debug-on-quit 't)
(defconst package-list '(ag
                         clang-format
                         cmake-mode
                         company
                         company-jedi
                         diff-hl
                         ethan-wspace
                         flycheck
                         git-timemachine
                         highlight-symbol
                         json-mode
                         json-reformat
                         magit ;; (only on 24.4)
                         markdown-mode+
                         minimap
                         nose
                         atom-dark-theme
                         projectile
                         py-autopep8
                         rainbow-delimiters
                         rainbow-mode
                         realgud
                         request
                         robe
                         smart-mode-line
                         use-package
                         web-mode
                         yaml-mode
                         yascroll))

;; Required packages (rtags needs to be installed separately)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Autoinstall packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Add some local include paths
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; A side file to store informations that should not go on github
(load-file "~/.emacs.d/confidential.el")

;; load the color theme
(load-theme 'atom-dark t)

(require 'use-package)

;; Font settings
(defun my-font-candidate-filter (f)
  "Return the name of the font F if it exists or nil."
  (unless (find-font (font-spec :name f)) f))

(defun remove-all (predic seq &optional res)
  "Elements matching PREDIC are removed from SEQ (RES is for recursion)."
  (if (null seq)
      (reverse res)
      (cond ((and (not (null (car seq))) (listp (car seq)))
             (remove-all predic (cdr seq)
                         (cons (remove-all predic (car seq)) res)))
            ((funcall predic (car seq))
             (remove-all predic (cdr seq) res))
            (t (remove-all predic (cdr seq) (cons (car seq) res))))))

(defun my-font-candidate (&rest fonts)
  "Select the first available font from a list of FONTS."
  (car (remove-all #'my-font-candidate-filter fonts)))

(when (display-graphic-p)
  (set-face-attribute 'default nil :font
                      (my-font-candidate ' "Source Code Pro-14"
                                           "Inconsolata-12"
                                           "Consolas-12"
                                           "Monaco-14"
                                           "DejaVu Sans Mono-12"
                                           "Courier New-12")))
;; some useless alpha effects
;; (set-frame-parameter (selected-frame) 'alpha '(99 85))
;; (add-to-list 'default-frame-alist '(alpha 99 85))

;; Automatic window resizing and fullscreen mode
(defun set-frame-size-according-to-resolution ()
  "Re-size Emacs according to the current screen capabilities."
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1024)
            (add-to-list 'default-frame-alist (cons 'width 136))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 250)
                                      (frame-char-height)))))))

(when (eq window-system 'x) ; seems to be buggy on mac os
  (set-frame-size-according-to-resolution))

;;; work around
(unless (keymap-parent lisp-mode-shared-map)
  (set-keymap-parent lisp-mode-shared-map prog-mode-map))

;; (defun helm-spotting (query-string)
;;   "Load and start helm-spotting with QUERY-STRING."
;;   (interactive "sQuery: ")
;;   (load-file "~/.emacs.d/experiment/helm-spotting.el")
;;   (helm-spotting query-string))

;;
;; Common settings
;;

(setq-default indent-tabs-mode nil)

(cua-mode)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (suspend-frame)))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'fringe)
  (run-with-timer 0.15 nil 'invert-face 'fringe))

(setq-default visible-bell nil
              ring-bell-function 'my-terminal-visible-bell)

;; setting up scrollbar and visual bell
(scroll-bar-mode -1)
(tool-bar-mode -1)
(yascroll-bar-mode)

;;; Minimap (w/flycheck)
(minimap-mode)
(setq-default minimap-update-delay 0)
(run-with-idle-timer 0.5 t 'minimap-sync-overlays)

;; no HELM
(require 'ido)
(ido-mode 1)

;;; Global-map
;;
;;
(global-set-key (kbd "C-c i")     'imenu)
(global-set-key (kbd "C-x C-p")   'projectile-switch-project)
(global-set-key (kbd "C-x C-o")   'projectile-find-file)
(global-set-key (kbd "C-x C-k")   'projectile-ag)
(global-set-key (kbd "C-<tab>")   'projectile-find-other-file)
(global-set-key (kbd "C-c C-s")   'magit-status)
(global-set-key (kbd "C-c C-d")   'magit-diff)
(global-set-key (kbd "C-c C-b")   'magit-blame)
(global-set-key (kbd "C-c C-c")   'magit-commit)
(global-set-key (kbd "C-c C-a")   'magit-commit-amend)
(global-set-key (kbd "C-c C-p")   'magit-pull)
(global-set-key (kbd "C-c C-m")   'magit-merge)
(global-set-key (kbd "C-c C-r")   'magit-rebase)
(global-set-key (kbd "C-c p")     'magit-push)

;; Some generic settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(setq visible-bell t)

;; Increas/decrease font size with C-+, C--
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

;; These are damn useful (on older emacs versions at least)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Turn on auto-fill in `text-mode' and derived modes.
;; Try M-q on a long paragraph in a text file or C++ comment!
;; Enables simple multicursor editing using C-d
(defun my-text-mode-hook()
  "My hook for all text modes."
  (turn-on-auto-fill)
  (company-mode)
  (setq company-backends '(company-ispell company-files (company-abbrev company-dabbrev)))
  (global-set-key (kbd "C-d") 'mc/mark-next-symbol-like-this))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; Enable interactive autocompletion of files and commands
(projectile-global-mode)

(setq-default projectile-switch-project-action 'projectile-vc)

; (windmove-default-keybindings)

;; Check whitespaces wisely in all buffers
(require 'ethan-wspace)
(setq mode-require-final-newline nil)

(setq indent-tabs-mode nil)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)

;; Smart line with cow powers!
(setq-default sml/no-confirm-load-theme t)
(sml/setup)
(setq-default sml/theme 'respectful)

;;
;; Language specific settings
;;

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Robot Framework editing mode
;(load-file "~/.emacs.d/robot-mode/robot-mode.el")
;(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

;; Setup C++ like coding style
(defconst my-cc-style
  '("bsd"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)))

(c-add-style "my-cc-style" my-cc-style)
(setq-default c-default-style "my-cc-style")

;; C++ hook
(defun my-c++-mode-hook ()
  "My C++ setting."
  ;;  (flyspell-prog-mode)          ;; spell check the comments
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map))
  ;; (eldoc-mode)
  (ethan-wspace-mode)
  (setq indent-tabs-mode nil)
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-i" 'c-indent-line-or-region))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; RTAGS is Great for C++ navigation, refactoring and autocompletion
(require 'rtags)
(require 'company-rtags)

(defun my-rtags-c++-mode-hook ()
  "C++ setting for rtags."
  ;; (rtags-start-process-maybe) Please start the rdm process in a
  ;; separate terminal, it is more responsive that way.
  (setq company-backends '((company-keywords company-rtags company-files company-dabbrev-code)))
  (setq rtags-completions-enabled t
        rtags-display-current-error-as-tooltip t
        rtags-autostart-diagnostics t
        rtags-show-containing-function t
        rtags-track-container t
        company-rtags-max-wait 500)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "M-;") 'rtags-find-file)
  (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)
  (define-key c-mode-base-map (kbd "C-<") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "C-c i") 'rtags-imenu)
  (define-key c-mode-base-map (kbd "C-c r i") 'rtags-print-symbol-info)
  (define-key c-mode-base-map (kbd "M-[") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "M-]") 'rtags-location-stack-forward)
  (define-key c-mode-base-map (kbd "M-n") 'rtags-next-match)
  (define-key c-mode-base-map (kbd "M-p") 'rtags-previous-match)
  (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary))

(add-hook 'c++-mode-hook 'my-rtags-c++-mode-hook)

;; Add header line with current method
(setq rtags-track-container t)
(add-hook 'find-file-hook
          (lambda ()
            (setq header-line-format
                  (and
                   (rtags-is-indexed)
                   '(:eval
                     rtags-cached-current-container)))))

;; clang-format intergation
(require 'clang-format)
(define-key c-mode-base-map (kbd "M-q") (function clang-format-region))

;; Extend C++ extensions
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

;; Better www mode with javascript, css, php and html support, all on
;; the same file!
(require 'web-mode)

;; Python
(defun my-python-hook ()
  "Python mode customization."
  (unless (keymap-parent python-mode-map)
    (set-keymap-parent python-mode-map prog-mode-map))
  (jedi:setup)
  (eldoc-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (ethan-wspace-mode)
  (setq mode-require-final-newline nil)
  (setq company-backends '((company-jedi company-files company-keywords company-abbrev company-dabbrev)))
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-next)
  (define-key python-mode-map (kbd "M-[") 'jedi:goto-definition-pop-marker)
  )

(add-hook 'python-mode-hook 'my-python-hook)
(setq jedi:complete-on-dot t)

(defun my-ruby-hook ()
  "Ruby mode customization."
  (flycheck-mode)
  (robe-mode)
  (eldoc-mode)
  (eval-after-load 'company
    '(setq company-backends '(company-robe company-files))))

(add-hook 'ruby-mode-hook 'my-ruby-hook)

(defun my-elisp-mode-hook ()
  "My hook for Emacs Lisp mode."
  (flycheck-mode)
  (ethan-wspace-mode)
  (setq company-backends '((company-elisp company-files company-keywords company-abbrev company-dabbrev)))
  )

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-cmake-hook ()
  "CMake mode customization."
  (ethan-wspace-mode)
  (setq indent-tabs-mode nil)
  (eval-after-load 'company
    '(setq company-backends '(company-cmake company-files))))

(add-hook 'cmake-mode-hook 'my-cmake-hook)

(add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . gmpl-mode))

;; common settings for all programming modes
(defun my-prog-mode-hook()
  (goto-address-prog-mode)      ;; click on links and emails
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t)))
  (highlight-symbol-mode)
  (rainbow-mode)
  (rainbow-delimiters-mode)
  (hl-line-mode t)
  (company-mode)
  (bug-reference-prog-mode)
  (diff-hl-mode)
  (diff-hl-flydiff-mode)
  (define-key prog-mode-map (kbd "C-d") 'mc/mark-next-symbol-like-this)
  (define-key prog-mode-map (kbd "M-/") 'company-complete))


(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(set-face-foreground 'font-lock-warning-face "salmon2")

(use-package go-mode
  :mode "\\.go"
  :ensure t
  :config
  (progn

    ;; Tell ethan to disregard tabs this time
    (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))

    (add-hook 'go-mode-hook (lambda ()
                              (flycheck-mode)
                              (setq compile-command "go build -v && go test -v && go vet && golint")
                              (define-key (current-local-map) "\C-c\C-c" 'compile)
                              (setq tab-width 4)
                              (setq indent-tabs-mode 't)))

    (setq gofmt-command (cond
                         ((executable-find "goimports")
                          "goimports")
                         (t "gofmt")))

    ;; Call Gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; (use-package go-stacktracer
    ;;   :commands (go-stacktracer-region))

    (use-package go-eldoc
      :ensure t
      :init
      (progn
        (add-hook 'go-mode-hook 'go-eldoc-setup)))

    (use-package company-go
      :init
      (bind-key "M-." 'godef-jump go-mode-map)
      (bind-key "M-[" 'pop-tag-mark go-mode-map)
      (setq company-backends '(company-go))
      )
    )
  )

(load "server")
(unless (server-running-p) (server-start))

;;; init.el ends here
