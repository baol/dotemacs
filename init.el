;;; init.el -- baol's dotemacs file
;;;
;;; Commentary:
;;;             A dotemacs for C++/HTML/python/robot-framework with
;;;             (almost) consistent usage of company and key-bindings.
;;;
;;;             Most notably:
;;;
;;;             M-/  completes
;;;             M-.  goes to definition/declaration
;;;             M-,  finds next usage
;;;             M-]  goes back
;;;
;;; Code:

;; Required packages (rtags needs to be installed separately)
(defvar package-list '(
                       zenburn-theme
                       minimap
                       ido-ubiquitous
                       ethan-wspace
                       yasnippet
                       web-mode
                       popup
                       company
                       company-jedi
                       markdown-mode+
                       powerline
                       flycheck
                       rainbow-mode
                       visual-regexp
                       ))

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


;; Turn on auto-fill in `text-mode' and derived modes 
;; Try M-q on a long paragraph in a text file or C++ comment!
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Font settings
(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match in FONTS."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font
                    (font-candidate ' "Inconsolata-12"
                                      "Consolas-12"
                                      "DejaVu Sans Mono-12"
                                      "Courier New-12"))


;; Recent file list (M-x recentf-open-file)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


;; Enable interactive autocompletion of files and commands
(ido-mode 1)
(ido-ubiquitous-mode 1)


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
                             (frame-char-height)))))))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(global-set-key [f11] 'toggle-fullscreen)

(setq-default indent-tabs-mode nil)
(set-frame-size-according-to-resolution)

;; Some sublimity
(require 'minimap)
(minimap-mode)

;; Powerline!
(powerline-default-theme)

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
  (minimap-mode)                ;; use the minimap
  (goto-address-prog-mode)      ;; click on links and emails
  (flyspell-prog-mode)          ;; spell check the comments
  (flycheck-mode)
  (eldoc-mode)
  (company-mode)                ;; complete anything
  (setq indent-tabs-mode nil)
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-i" 'c-indent-line-or-region)
  )


;; Extend C++ extensions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; YASnippet
(require 'yasnippet)

;; RTAGS is Great for C++ navigation, refactoring and autocompletion
(require 'rtags)
(require 'company-rtags)

(defun my-rtags-c++-mode-hook ()
  "C++ setting for rtags."

  (rtags-start-process-maybe)

  (setq company-backends '(company-rtags company-yasnippet))

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
  (define-key c-mode-base-map (kbd "M-/") 'company-complete)
)

(add-hook 'c++-mode-hook 'my-rtags-c++-mode-hook)


;; Better www mode with javascript, css, php and html support, all on
;; the same file!
(require 'web-mode)


;; Python
(defun my-python-hook()
  (jedi:setup)
  (company-mode)
  (eldoc-mode)
  (flycheck-mode)
  (define-key python-mode-map (kbd "M-/") 'company-complete)
  (setq company-backends '(company-jedi company-yasnippet)))

(add-hook 'python-mode-hook 'my-python-hook)
(setq jedi:complete-on-dot t)

;;; init.el ends here
;;
;;  LocalWords:  init LocalWords baol's dotemacs rtags el
