;; Hooks for vc-next-action
(defun et-commit-filename ()
"File name to add to the header of a git commit."
  (require 'project)
  (let* ((root (project-root (project-current)))
         (file-name (abbreviate-file-name (file-name-sans-extension buffer-file-name)))
         (extension (file-name-extension buffer-file-name))
         (final-file-name (mapconcat #'identity
                                     (cl-remove-duplicates (split-string (file-relative-name file-name root))
                                                           :test #'string-equal) ":")))
         (when (or (string-equal extension "c")
                 (string-equal extension "h")
                 (string-equal extension "cpp")
                 (string-equal extension "hpp"))
                (setq final-file-name (format "%s:%s" final-file-name extension)))
         (concat final-file-name ": ")))

(defun et-insert-preamble (preamble)
"Insert the PREAMBLE (aka filepath:filename) in the git commit."
  (when (equal (buffer-name) "*vc-log*")
                   (insert preamble)))

(defun et-vc-log-advice (orig-fun &rest args)
  "Advice the 'vc-next-action' function with inser-preamble.
The arguments are ORIG-FUN (vc-next-action) and ARGS the argument
of 'vc-next-action'."
  (let ((preamble (et-commit-filename)))
    (apply orig-fun args)
    (et-insert-preamble preamble)))

;; Advicing vc-next-action
(advice-add 'vc-next-action :around #'et-vc-log-advice)

(defun et-open-config ()
  "Open this configuration."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun et-dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (format-replace-strings '(("" . "")))
    (goto-char (point-min))
    (format-replace-strings '((" " . ""))))
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun et-unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun et-trim-whitespace-based-on-encoding ()
  "Trim trailing whitespace based on the buffer's encoding."
  (interactive)
  (let ((coding-system (symbol-name buffer-file-coding-system)))
    (if (string-match "dos" coding-system)
        (progn
          ;; For Windows (CRLF) encoding
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t)
            (replace-match "")))
      (if (string-match "unix" coding-system)
        (delete-trailing-whitespace)))))

(add-hook 'before-save-hook 'et-trim-whitespace-based-on-encoding)

(defun set-file-coding-if-crlf ()
  "Check for ^M characters (Windows line endings) in the current buffer.
   If found, set the file's encoding to utf-8-dos."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\r\n" nil t)
      (set-buffer-file-coding-system 'utf-8-dos)
      (message "File encoding set to utf-8-dos due to Windows line endings (^M)."))))

(defun et-make-unix-dir (dir)
  "Find all non hidden files in DIR and convert their line ending into unix."
  (interactive)
  (let ((files (directory-files dir t)))
    (while files
      (let ((current-file (pop files)))
        (if (not (file-directory-p current-file))
            (with-temp-file current-file
              (insert-file-contents current-file)
              (et-dos2unix))
          (unless (string-match "^." (file-name-base current-file)) ;; remove all the hidden files
            (et-make-unix-dir current-file)))))))

(defvar-local project-test-cmd nil
  "Function for testing the current project, ovveride it in the dir locals var.")

(defvar-local project-run-cmd nil
  "Function for running the current project, ovveride it in the dir locals var.")

(put 'project-run-cmd 'safe-local-variable 'string-or-null-p)
(put 'project-test-cmd 'safe-local-variable 'string-or-null-p)

(defun et-project-run-tests ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compile-command (or project-test-cmd
                            compile-command)))
    (call-interactively #'compile)))

(defun et-project-run ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (run-command project-run-value))
    (cl-flet ((prompt-text (lambda () (insert run-command))))
      (minibuffer-with-setup-hook #'prompt-text
          (call-interactively #'project-async-shell-command)))))

(defun et-reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun et-search-for-word-in-directory (dir-to-search)
  "Search for current word inside the DIR-TO-SEARCH.
If there is no selected word, simply start an empty search."
  (interactive "DChoose the directory...")
  (let* ((string-to-search (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end)) "")))
    (consult-grep dir-to-search string-to-search)))

(defun et-find-file ()
  (interactive)
  ;; Project current check if we are inside a project otherwise uses the normal find
  (if (project-current)
    (project-find-file)
    (call-interactively 'find-file)))

(defun file-metadata ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (data (file-attributes fname))
         (access (current-time-string (nth 4 data)))
         (mod (current-time-string (nth 5 data)))
         (change (current-time-string (nth 6 data)))
         (size (nth 7 data))
         (mode (nth 8 data)))
    (message
     "%s:
  Accessed: %s
  Modified: %s
  Changed: %s
  Size: %s bytes
  Mode: %s"
     fname access mod change size mode)))

(defun et-other-window ()
  "Switch to the next window in a cyclic manner, including side windows."
  (interactive)
  (let ((windows (window-list)))
    (cond
     ((null windows)
      (message "No windows to switch to."))
     ((= 1 (length windows))
      (message "Only one window is available."))
     (t
      (select-window (if (eq (selected-window) (car (last windows)))
                         (car windows)
                       (next-window)))))))

(defun et-set-msdos-file-type ()
  "Set the file type as MSDOS (CRLF line endings)."
  (interactive)
  (setq buffer-file-coding-system 'dos)
  (message "File type set to MSDOS (CRLF line endings)."))

(provide 'utils)
