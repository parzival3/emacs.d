(defun et-split-compile (command)
  "Split the current buffer and run an Eshell command in the new buffer."
  (interactive
   (let ((default (or compile-command "make -k")))
     (list (read-string (format "Compile command (default: %s): " default) nil nil default))))
  (setq compile-command (or command "make -k"))
  (window-configuration-to-register ?z)
  (delete-other-windows)
  (let ((default-eshell "*eshell*")
        (default-directory (project-root (project-current t))))
    (unless (get-buffer default-eshell)
      (eshell))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer default-eshell)
    (eshell-interrupt-process) ;; kill current process
    (setq-local run-command command)
    (run-with-timer 0.5 nil
                    (lambda ()
                      (eshell-interrupt-process)
                      (eshell-return-to-prompt)
                      (insert run-command)
                      (eshell-send-input)))))

(global-set-key (kbd "<f13>") 'et-split-compile)

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


(defun et-split-compile (command)
  "Split the current buffer and run an Eshell command in the new buffer."
  (interactive
   (let ((default (or compile-command "make -k")))
     (list (read-string (format "Compile command (default: %s): " default) nil nil default))))
  (setq compile-command (or command "make -k"))
  (window-configuration-to-register ?z)
  (delete-other-windows)
  (let ((default-eshell "*eshell*")
        (default-directory (project-root (project-current t))))
    (unless (get-buffer default-eshell)
      (eshell))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer default-eshell)
    ;; caputre line
    (eshell-interrupt-process) ;; kill current process
    (setq-local run-command command)
    (run-with-timer 0.5 nil
                    (lambda ()
                      (eshell-interrupt-process)
                      (eshell-return-to-prompt)
                      (insert run-command)
                      (eshell-send-input)))))
