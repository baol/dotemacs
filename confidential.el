(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


(defun open-curly-brace () (interactive) (insert-string "{"))
(defun close-curly-brace () (interactive) (insert-string "}"))
(defun open-square-brace () (interactive) (insert-string "["))
(defun close-square-brace () (interactive) (insert-string "]"))

(global-set-key (kbd "M-7") 'open-curly-brace)
(global-set-key (kbd "M-8") 'open-square-brace)
(global-set-key (kbd "M-9") 'close-square-brace)
(global-set-key (kbd "M-0") 'close-curly-brace)
