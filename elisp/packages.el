(use-package wgrep
  :straight t)

;; Magit
(use-package magit
  :commands (magit-status)
  :bind
  (("C-x g" . magit-status))
  :straight t
  :bind (:map magit-mode-map
              ("D" . #'magit-discard))
  :config
  (defun magit-commit-fast (commit-message)
    (interactive "commit message:")
    (let ((magit-commit-ask-to-stage t)
          (magit-commit-show-diff nil))
      (magit-git "commit" "--all" "-m" commit-message)))

  (defun et-file-commit-message ()
    (if-let ((files (magit-staged-files))
             (file-name (abbreviate-file-name (file-name-sans-extension (car files))))
             (final-file-name (mapconcat #'identity
                                         (cl-remove-duplicates (split-string file-name)
                                                               :test #'string-equal) ":")))
        (insert (concat final-file-name ": "))
      (error "No files staged")))

  (add-hook 'git-commit-setup-hook #'et-file-commit-message))


(use-package browse-at-remote
  :straight t
  :bind
  (("C-x v o" . browse-at-remote))
  :config
  (setq browse-at-remote-use-http '("cd.senncom.com"))
  (add-to-list 'browse-at-remote-remote-type-regexps
               `(:host ,(rx bol "cd.senncom.com" eol)
                       :type "stash"
                       :actual-host "cd.senncom.com:7990"))

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


(use-package elfeed-protocol
  :straight t
  :after elfeed)


(use-package elfeed
  :straight t
  :init
  (setq elfeed-use-curl t)

  (setq elfeed-protocol-ttrss-maxsize 200) ; bigger than 200 is invalid
  (setq elfeed-protocol-ttrss-fetch-category-as-tag t)
  (setq elfeed-protocol-enabled-protocols '(ttrss))
  (setq elfeed-protocol-feeds `((,(concat "ttrss+https://" user@ttrss-url "/tt-rss")
                                 :password ,ttrss-password)))
  :config
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable)

  (defun et-elfeed-eww-visit (&optional use-generic-p)
    "Visit the current entry in eww."
    (interactive "P")
    (let ((browse-url-browser-function 'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

  :bind
        (:map elfeed-search-mode-map
                ("b" . et-elfeed-eww-visit)
                ("B" . elfeed-show-visit))
        (:map elfeed-show-mode-map
                ("b" . et-elfeed-eww-visit)
                ("B" . elfeed-show-visit)))


(use-package fd-dired
  :straight t
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


(use-package string-inflection
  :straight t
  :config

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
           ;; save excusion
           (save-excursion
             (beginning-of-buffer)
             (query-replace-regexp word (downcase word))))))


(use-package vertico
  :defer t
  :straight t
  :config
  (setq vertico-cycle t)
  :init
  (vertico-mode))


(use-package consult
  :defer t
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))


(use-package embark
  :defer t
  :straight t
  :bind
  (("<f9>" . embark-export)
   ("C-." . embark-act)
   ("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-\\" . embark-act)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun dired-find-file-directory ()
    "In Dired, visit the file or directory named on this line."
    (interactive)
    (let ((filename (read-from-minibuffer "Input: ")))
      (dired--find-possibly-alternative-file (file-name-directory filename))))

  (define-key embark-file-map (kbd "d") #'dired-find-file-directory)

  ;; fix problem in marginaglia/embark
  (setf (alist-get 'xref-location embark-default-action-overrides)
      #'embark-consult-goto-grep)
  (setf (alist-get 'xref-location embark-exporters-alist)
      #'embark-consult-export-grep)
  (setf (alist-get 'consult-xref embark-default-action-overrides)
      #'embark-consult-goto-grep)
  (setf (alist-get 'consult-xref embark-exporters-alist)
      #'embark-consult-export-grep))


(use-package embark-consult
  :straight t
  :after (embark consult))


(use-package which-key
  :defer t
  :straight t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  :init
  (which-key-mode))


(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately while 'completing-read' prompter.
FN: the function to apply.
ARGS: the arguments to the function."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)


(use-package orderless
  :defer t
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles . (orderless flex))))))


(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :defer t
  :straight t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))


(use-package eglot
  :straight t
  :config
  (global-set-key (kbd "C-x C-.") 'eglot-code-actions)) ;; maybe is better if I create a proper keymap


(use-package spacemacs-theme
  :straight t)


(provide 'packages)
