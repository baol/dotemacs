;;; init.el -- baol's dotemacs file
;;;
;;; Running on Ubuntu 14.04 LTS GNU/Emacs 24.3
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

;;; work around
(unless (keymap-parent lisp-mode-shared-map)
  (set-keymap-parent lisp-mode-shared-map prog-mode-map))

;; Required packages (rtags needs to be installed separately)
(require 'package)
(defvar package-list '(clang-format
                       cmake-mode
                       company
                       company-jedi
                       ethan-wspace
                       flycheck
                       highlight-symbol
                       helm
                       ido-ubiquitous
                       ido-vertical-mode
                       indent-guide
                       json-mode
                       json-reformat
                       ;; magit (only on 24.4)
                       markdown-mode+
                       multiple-cursors
                       nyan-mode
                       popup
                       powerline
                       projectile
                       py-autopep8
                       rainbow-mode
                       sx
                       visual-regexp
                       web-mode
                       zenburn-theme))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;; Autoinstall packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Add some local include paths
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")


;; A side file to store informations that should not go on github
(load-file "~/.emacs.d/confidential.el")


;;
;; Common settings
;;

(require 'zenburn-theme)

;; Some generic settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(setq visible-bell t)


;; Increas/decrease font size with C-+, C--
(global-set-key (kbd "<C-+>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-=>") 'text-scale-increase)
(global-set-key (kbd "<C-\->") 'text-scale-decrease)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)


;; These are damn useful (on older emacs versions at least)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Turn on auto-fill in `text-mode' and derived modes.
;; Try M-q on a long paragraph in a text file or C++ comment!
;; Enables simple multicursor editing using C-d
(defun my-text-mode-hook()
  (turn-on-auto-fill)
  ;; Multiple cursors mode
  (multiple-cursors-mode)
  (global-set-key (kbd "C-d") 'mc/mark-next-symbol-like-this)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)


;; Font settings
(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match in FONTS."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts)
)

(when (display-graphic-p)
  (set-face-attribute 'default nil :font
                      (font-candidate ' "Inconsolata-12"
                                        "Consolas-12"
                                        "DejaVu Sans Mono-12"
                                        "Courier New-12")))


;; Recent file list (M-x recentf-open-file)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


;; Enable interactive autocompletion of files and commands
(ido-mode 1)
(ido-ubiquitous-mode 1)
(projectile-global-mode)
(global-set-key (kbd "C-M-p") 'projectile-find-file)
(global-set-key (kbd "C-<tab>") 'projectile-find-other-file)

(setq projectile-switch-project-action 'projectile-vc)

(windmove-default-keybindings)

;; Check whitespaces wisely in all buffers
(require 'ethan-wspace)
(setq mode-require-final-newline nil)
(global-ethan-wspace-mode 1)


;; Automatic window resizing and fullscreen mode
(defun set-frame-size-according-to-resolution ()
  "Re-size Emacs according to the current screen capabilities."
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 136))
           (add-to-list 'default-frame-alist (cons 'width 136)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (+ (x-display-pixel-height) 60)
                             (frame-char-height))))))
)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
)

(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

(setq-default indent-tabs-mode nil)
(set-frame-size-according-to-resolution)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)


;; Powerline!
(powerline-default-theme)
(nyan-mode)


;;
;; Language specific settings
;;


;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Robot Framework editing mode
(load-file "~/.emacs.d/robot-mode/robot-mode.el")
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))


;; Setup C++ like coding style
(defconst my-cc-style
  '("bsd"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)
  ))
(c-add-style "my-cc-style" my-cc-style)
(setq c-default-style "my-cc-style")


;; C++ hook
(defun my-c++-mode-hook ()
  "My C++ setting."
  ;;  (flyspell-prog-mode)          ;; spell check the comments
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map))
  (flycheck-mode)
  ;; (eldoc-mode)
  (setq indent-tabs-mode nil)
  (highlight-symbol-mode)
  (setq mode-require-final-newline nil)
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-i" 'c-indent-line-or-region)
)


(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;; RTAGS is Great for C++ navigation, refactoring and autocompletion
(require 'cl)
(require 'rtags)
(require 'company-rtags)

(defun my-rtags-c++-mode-hook ()
  "C++ setting for rtags."
  (rtags-start-process-maybe)
  (setq company-backends '(company-rtags))
  (setq rtags-completions-enabled t
        rtags-display-current-error-as-tooltip t
        rtags-autostart-diagnostics t
        rtags-show-containing-function t
        rtags-track-container t)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "M-;") 'rtags-find-file)
  (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)
  (define-key c-mode-base-map (kbd "C-<") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "M-s") 'rtags-imenu)
  (define-key c-mode-base-map (kbd "C-c r i") 'rtags-print-symbol-info)
  (define-key c-mode-base-map (kbd "M-[") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "M-]") 'rtags-location-stack-forward)
  (define-key c-mode-base-map (kbd "M-n") 'rtags-next-match)
  (define-key c-mode-base-map (kbd "M-p") 'rtags-previous-match)
)

(add-hook 'c++-mode-hook 'my-rtags-c++-mode-hook)


;; Add header line with current method
(setq rtags-track-container t)
(add-hook 'find-file-hook (lambda ()
                            (setq header-line-format (and (rtags-is-indexed)
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
(defun my-python-hook()
  (unless (keymap-parent python-mode-map)
    (set-keymap-parent python-mode-map prog-mode-map))
  (jedi:setup)
  (eldoc-mode)
  (flycheck-mode)
  (setq mode-require-final-newline nil)
  (setq company-backends '(company-jedi))
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-next)
  (define-key python-mode-map (kbd "M-[") 'jedi:goto-definition-pop-marker)
)

(add-hook 'python-mode-hook 'my-python-hook)
(setq jedi:complete-on-dot t)


;; common settings for all programming modes
(defun my-prog-mode-hook()
  (goto-address-prog-mode)      ;; click on links and emails
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t)))
  (highlight-symbol-mode)
  (rainbow-mode)
  (indent-guide-mode)
  (set-face-foreground 'indent-guide-face "#e09030")
  (hl-line-mode t)
  (company-mode)
  ;; Multiple cursors mode
  (multiple-cursors-mode)
  (define-key prog-mode-map (kbd "C-d") 'mc/mark-next-symbol-like-this)
  (define-key prog-mode-map (kbd "M-/") 'company-complete)
)
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(set-face-foreground 'font-lock-warning-face "#F33")


;; Setup indent guide to not slow down scrolling
(setq indent-guide-delay 0.3)
(setq indent-guide-recursive t)
(setq indent-guide-char ":") ;; "┊")


;;; init.el ends here
;;
;;  LocalWords:  init LocalWords baol's dotemacs rtags el
