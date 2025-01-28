;;; packages.el --- External packages -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package wgrep
  :defer t)

(use-package browse-at-remote
  :defer t
  :bind
  (("C-x v o" . browse-at-remote))
  :config
  (setq browse-at-remote-use-http '("cd.senncom.com"))
  ;; (add-to-list 'browse-at-remote-remote-type-regexps
  ;;              `(:host ,(rx bol "cd.senncom.com" eol)
  ;;                      :type "stash"
  ;;                 :actual-host "cd.senncom.com:7990"))
  (add-to-list 'browse-at-remote-remote-type-domains '("cd.senncom.com" . "stash"))

  (defun et-fix-http-protocol-for-browse-at-remote (orig-fun &rest args)
    (let* ((parsed (url-generic-parse-url (car args)))
           (host (url-host parsed)))
      (if (member host browse-at-remote-use-http)
          (progn
            (let* ((generated-url (apply orig-fun args))
                   (new-url (string-replace "https://" "http://" (plist-get generated-url :url))))
              (plist-put generated-url :url new-url)))
        (apply orig-fun args))))

  (advice-add 'browse-at-remote--get-url-from-remote :around #'et-fix-http-protocol-for-browse-at-remote))

(use-package fd-dired
  :defer t
  :config
  (defun fd-dired-simple ()
    (interactive (list (read-string "Run fd (with args and search): " fd-dired-input-fd-args
                                    '(fd-dired-args-history . 1))))
    ;; if current buffer is a dired buffer, use its directory
    (let ((dir (if (eq major-mode 'dired-mode)
                   ;; if the element under the cursor is a directory use it
                   (if (file-directory-p (dired-get-file-for-visit))
                       (dired-get-file-for-visit))
                 default-directory)))
      (fd-dired dir fd-dired-input-fd-args)))
  :bind
  (:map dired-mode-map
        ("C-x C-d" . fd-dired-simple)))

;; TODO: use the `:command` keyword to autoload my functions, and define a custom macro for this functions
(use-package string-inflection
  :defer t
  :config
  (defun et-inflection-word-at-point (inflection-function)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (progn
            (buffer-substring-no-properties (car bounds) (cdr bounds))
            (kill-region (car bounds) (cdr bounds))
            (insert (funcall inflection-function (car kill-ring-yank-pointer))))
        (message "No symbol at point")
        nil)))

  (defun et-to-camel-case ()
    (interactive)
    (et-inflection-word-at-point 'string-inflection-camelcase))

  (defun et-to-kebab-case ()
    (interactive)
    (et-inflection-word-at-point 'string-inflection-kebab-case))

  (defun et-to-snake-case ()
    (interactive)
    (et-inflection-word-at-point 'string-inflection-underscore))

  (defun to-pascal-case ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point)))
               (bounds (bounds-of-thing-at-point 'symbol)))
           (kill-region (car bounds) (cdr bounds))
           (insert (string-inflection-pascal-case-function word))))

  (defun to-lower-case ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point)))
               (bounds (bounds-of-thing-at-point 'symbol)))
           (kill-region (car bounds) (cdr bounds))
           (insert (downcase word))))

  (defun search-and-replace-to-lowecase ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point))))
           (save-excursion
             (beginning-of-buffer)
             (query-replace-regexp word (downcase word))))))


(use-package gptel
  :defer t
  :bind
  ("C-c RET" . gptel-send)
  :config
  (setq gptel-model "gpt-4-1106-preview"))


(use-package avy
  :defer t
  :bind
  ("C-:" . avy-goto-char-timer))


(use-package paredit
  :defer t)


(use-package ox-jira
  :defer nil
  :ensure )

(provide 'packages)
