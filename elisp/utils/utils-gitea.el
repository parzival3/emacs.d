;;; utils-gitea.el --- Gitea Utils -*- lexical-binding: t; -*-
(defcustom gitea-url "https://git.haento.info"
  "The url of the gitea server")

(defcustom gitea-token gitea-secret
  "The token to use to connect to gitea, variable is set in secrets.el")

(defcustom gitea-user "parzival3"
  "The user to use to connect to gitea")

(defun gitea-get-ssh-url (repo-name)
    (concat "git@git.haento.info:" gitea-user "/" repo-name ".git"))

;;;###autoload
(defun gitea-api-create-new-repo (repo-name default-branch auto-init &optional callback)
    (interactive "sEnter the repository name: \nsDefault branch: \nSAuto Init: ")
    (unless (project-current t)
        (user-error "Not in a git project"))
    (let* ((default-directory (project-root (project-current t)))
              (url (concat gitea-url "/api/v1/user/repos"))
              (headers `(("Authorization" . ,(concat "token " gitea-token))
                            ("Content-Type" . "application/json")
                            ("Accept" . "application/json")))
              (data (json-encode `(("name" . ,repo-name)
                                      ("private" . t)
                                      ("auto_init" . ,auto-init)
                                      ("default_branch" . ,default-branch)
                                      ("description" . ""))))
              (response-buffer (generate-new-buffer " *gitea-create-new-repo*")))
        (with-current-buffer response-buffer
            (erase-buffer))
        (let ((url-request-method "POST")
                 (url-request-extra-headers headers)
                 (url-request-data data)
                 (my-callback (lambda (status &rest test)
                                  (if-let ((http-error (plist-get status :error)))
                                      (error "Failed to create repository %s" http-error)
                                      (if (cl-search "HTTP/1.1 20" (buffer-string))
                                          (progn
                                              (message "Repository created successfully.")
                                              (when callback
                                                  (funcall callback)))
                                          (message "Failed to create repository."))
                                      ))))
            (url-retrieve url my-callback)
            (kill-buffer response-buffer))))


(defun gitea-create-new-repo (repo-name)
    (let* ((get-branch-name "git rev-parse --abbrev-ref HEAD")
              (branch-name (substring (shell-command-to-string get-branch-name) 0 -1))
              (callback (lambda ()
                            (magit-remote-add "gitea" (gitea-get-ssh-url repo-name))
                            (if (eq (and
                                        (shell-command "git fetch gitea")
                                        (shell-command "git push --set-upstream gitea -f '*:*'"))
                                    0)
                                (message "Repo cloned successfuly")
                                (error "Failed to create repository")))
                  ))
        (gitea-api-create-new-repo repo-name branch-name nil callback)))

;;;###autoload
(defun gitea-mirror-repo (repo-name)
    (interactive (list (read-string "Enter repository name: " (project-name (project-current t)))))
    (unless (project-current t)
        (user-error "Not in a git project"))
    (let ((list-of-remotes (shell-command-to-string "git remote -v")))
        (cond
            ((string-match "gitea" list-of-remotes) (error "Repository already exists on gitea"))
            ((string-match "origin" list-of-remotes) (gitea-create-new-repo repo-name))
            (t (gitea-create-new-repo repo-name)))))

(provide 'gitea)
;;; utils-gitea.el ends here
