;;; utils.el --- Collections of utilities -*- lexical-binding: t; -*-

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

;;;###autoload
(defun et-open-config ()
  "Open this configuration."
  (interactive)
  (find-file (concat minimal-emacs-user-directory "init.el")))

;;;###autoload
(defun et-dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (format-replace-strings '(("\r" . "")))
    (goto-char (point-min))
    (format-replace-strings '(("\0" . ""))))
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun et-unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "\r\n"))))

;;;###autoload
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

;;;###autoload
(defun set-file-coding-if-crlf ()
  "Check for ^M characters (Windows line endings) in the current buffer.
   If found, set the file's encoding to utf-8-dos."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\r\n" nil t)
      (set-buffer-file-coding-system 'utf-8-dos)
      (message "File encoding set to utf-8-dos due to Windows line endings (^M)."))))

;;;###autoload
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

;;;###autoload
(defun et-project-run-tests ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compile-command (or project-test-cmd
                            compile-command)))
    (call-interactively #'compile)))

;;;###autoload
(defun et-project-run ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (run-command project-run-value))
    (cl-flet ((prompt-text (lambda () (insert run-command))))
      (minibuffer-with-setup-hook #'prompt-text
          (call-interactively #'project-async-shell-command)))))

;;;###autoload
(defun et-reload-init-file ()
  (interactive)
  (load-file user-init-file))

;;;###autoload
(defun et-search-for-word-in-directory (dir-to-search)
  "Search for current word inside the DIR-TO-SEARCH.
If there is no selected word, simply start an empty search."
  (interactive "DChoose the directory...")
  (let* ((string-to-search (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end)) "")))
    (consult-grep dir-to-search string-to-search)))

;;;###autoload
(defun et-find-file ()
  (interactive)
  ;; Project current check if we are inside a project otherwise uses the normal find
  (if (project-current)
    (project-find-file)
    (call-interactively 'find-file)))

;;;###autoload
(defun file-metadata ()
  (interactive)
    (let* ((fname (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                  (buffer-file-name)))
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

;;;###autoload
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

;;;###autoload
(defun et-set-msdos-file-type ()
  "Set the file type as MSDOS (CRLF line endings)."
  (interactive)
  (setq buffer-file-coding-system 'dos)
  (message "File type set to MSDOS (CRLF line endings)."))


;; (load-file (concat et-elisp-dir "gitea-utils.el"))

;;;###autoload
(defun et-reset-custom-variable (variable)
  "Reset the VARIABLE to its default value."
  (interactive
   (list (intern (completing-read "Custom variable: " obarray
                                  (lambda (v) (and (boundp v) (custom-variable-p v)))))))
  (print (type-of variable))
  (set variable (eval (car (get variable 'standard-value)))))

;; TODO: move this function
(defun et-download-and-extract-tar-gz (url target-directory)
  "Download a tar.gz file from URL and extract it into TARGET-DIRECTORY."
  (require 'url)
  (require 'tar-mode)
  (let ((download-file (concat temporary-file-directory "downloaded.tar.gz")))
    (url-copy-file url download-file t)
    (when (file-exists-p target-directory)
      (delete-directory target-directory t))
    (make-directory target-directory t)
    (let ((default-directory target-directory))
      (shell-command (format "tar -xf %s" download-file)))))

(defmacro define-search-function (name url)
  "Define a search function NAME that searches URL for the selected term or the term under the cursor."
  `(defun ,(intern (concat "search-in-" (symbol-name name))) (&optional term)
     ,(concat "Search " (symbol-name name) " for the selected term or the term under the cursor.")
     (interactive (list (read-string (concat "Search " ,(symbol-name name) " for: "))))
     (let* ((term (or term
                      (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (thing-at-point 'symbol t))))
            (query (if term
                       (url-encode-url term)
                     (user-error "No term found at point or selected"))))
       (browse-url (concat ,url query)))))

;;;###autoload (autoload 'search-in-msdn "utils")
(define-search-function msdn "https://learn.microsoft.com/en-us/search/?category=Documentation&terms=")
;;;###autoload (autoload 'search-in-source-graph "utils")
(define-search-function source-graph "https://sourcegraph.com/search?q=")

;;;###autoload
(defun et-decimal-to-hex-signed-16bit (decimal-number)
  "Convert a negative decimal number to its 16-bit signed hexadecimal representation and display it."
  (interactive "nEnter a decimal number: ")
  (let ((max-value 65536)) ;; 2^16
    (if (< decimal-number 0)
        (setq decimal-number (+ max-value decimal-number)))
    (let ((hex-representation (format "0x%04X" (logand decimal-number #xFFFF))))
      (message "Hexadecimal representation: %s" hex-representation)
      hex-representation)))

;;;###autoload
(defun et-print-bits (number)
  "Print the binary representation of NUMBER."
  (interactive "nEnter a number: ")
  (message "Binary representation: %s" (math-format-binary number)))

;;;###autoload
(defun et-count-lines-region (start end)
  "Count the number of lines in the selected region."
  (interactive "r")
  (message "Number of lines in region: %d" (count-lines start end)))

;;;###autoload
(defun search-for-word-in-buffer (word)
  "Search for word in the current buffer in reverse order and save the line containing the word."
(interactive "sSearch for word: ")
(let ((case-fold-search nil)
      (word (regexp-quote word))
      (line nil))
  (save-excursion
    (goto-char (point-max))
    (while (and (not line) (search-backward-regexp word nil t))
      (setq line (thing-at-point 'line t))))
  (if line
      (message "Found: %s" line)
    (message "Not found"))))

;;;###autoload
(defun et-split-compile (command)
  "Split the current buffer and run an Eshell command in the new buffer."
  (interactive
   (let ((default (or compile-command "make -k")))
     (list (read-string (format "Compile command (default: %s): " default) nil nil default))))
  (setq compile-command (or command "make -k"))
  (window-configuration-to-register ?z)
  (delete-other-windows)
  (save-some-buffers t)
  (let ((default-eshell "*eshell*")
        (default-directory (project-root (project-current t))))
    (unless (get-buffer default-eshell)
      (eshell)
      (other-window -1))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer default-eshell)
    (eshell/cd default-directory)
    ;; caputre line
    (eshell-interrupt-process) ;; kill current process
    (eshell-interrupt-process)
    (setq-local run-command command)
    (run-with-timer 0.5 nil
                    (lambda ()
                      (eshell-interrupt-process)
                      (eshell-return-to-prompt)
                      (insert run-command)
                      (eshell-send-input)))))

;;;###autoload
(defun et-add-directory-to-env (directory &optional error-message)
  (interactive)
  (if (not (file-directory-p directory))
      (and error-message ;; if we have an error, report it to the user
           (error error-message))
    (add-to-list 'exec-path (expand-file-name directory))
    (let ((string-path (mapconcat #'identity exec-path path-separator)))
      (eshell-set-path string-path)
      (setenv "PATH" string-path))))

(provide 'utils)
;; utils.el ends here
