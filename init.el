;; Required packages (rtags needs to be installed separately)
(setq package-list 
      '(zenburn-theme minimap ido-ubiquitous ethan-wspace yasnippet web-mode popup company markdown-mode+))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Autoinstall packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;; Add some local include paths
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")


;;; A side file to store informations that should not go on github
(load-file "~/.emacs.d/confidential.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'zenburn-theme)

;;; Some generic settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(setq visible-bell t)

;;; Increas/decrease font size with C-+, C--
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


;;;; Font settings
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font
                    (font-candidate ' "Inconsolata-12"
                                      "Consolas-12"
                                      "DejaVu Sans Mono-12"
                                      "Courier New-12"))


;;;; Recent file list (M-x recentf-open-file)
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


;;;; Automatic window resizing and fullscreen mode
(defun set-frame-size-according-to-resolution ()
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
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

(setq-default indent-tabs-mode nil)
(set-frame-size-according-to-resolution)


;;; Some sublimity
(require 'minimap)
(minimap-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language specific settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Robot Framework editing mode
(load-file "~/.emacs.d/robot-mode/robot-mode.el")
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))


;;;; Setup C++ like coding style
(defconst my-cc-style
  '("bsd"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)
  ))
(c-add-style "my-cc-style" my-cc-style)
(setq c-default-style "my-cc-style")


;;;; C++ hook
(defun my-c++-mode-hook ()
  "My C++ setting."
  (goto-address-prog-mode)
  (flyspell-prog-mode)
  (company-mode)
  (setq indent-tabs-mode nil)
  (rtags-start-process-unless-running)
  )


;;;; Extend C++ extensions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(add-hook 'c-initialization-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)


;; RTAGS is Great for C++ navigation, refactoring and autocompletion
(require 'rtags)
(require 'company-rtags)
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


;;;; More C++ keybindings (use C-c C-c to compile, C-i to indent)
(define-key c-mode-base-map "\C-c\C-c" 'compile)
(define-key c-mode-base-map "\C-i" 'c-indent-line-or-region)


;;; Better www mode with javascript, css, php and html support, all on
;;; the same file!
(require 'web-mode)
