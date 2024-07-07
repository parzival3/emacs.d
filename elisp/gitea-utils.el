(require 'json)
(require 'project)

(defcustom gitea-url "https://git.haento.info"
  "The url of the gitea server")

(defcustom gitea-token gitea-secret
  "The token to use to connect to gitea, variable is set in secrets.el")

(defcustom gitea-user "parzival3"
  "The user to use to connect to gitea")

(defun gitea-get-ssh-url (repo-name)
  (concat "git@git.haento.info:" gitea-user "/" repo-name ".git"))

(defun gitea-api-create-new-repo (repo-name)
  (interactive "sEnter the repository name: ")
  (unless (project-current t)
    (user-error "Not in a git project"))
  (let* ((default-directory (project-root (project-current t)))
         (url (concat gitea-url "/api/v1/user/repos"))
         (headers `(("Authorization" . ,(concat "token " gitea-token))
                    ("Content-Type" . "application/json")
                    ("Accept" . "application/json")))
         (data (json-encode `(("name" . ,repo-name)
                              ("private" . t)
                              ("auto_init" . t)
                              ("description" . ""))))
         (response-buffer (generate-new-buffer " *gitea-create-new-repo*")))
    (with-current-buffer response-buffer
      (erase-buffer))
    (let ((url-request-method "POST")
          (url-request-extra-headers headers)
          (url-request-data data))
      (url-retrieve url
                    (lambda (status)
                      (unless status
                        (if (cl-search "HTTP/1.1 20" (buffer-string))
                            (message "Repository created successfully.")
                          (message "Failed to create repository."))))
                      (kill-buffer response-buffer)))))

(defun gitea-create-new-repo (repo-name)
  (gitea-api-create-new-repo repo-name)
  (magit-remote-add "gitea" (gitea-get-ssh-url repo-name))
  (unless (eq 0 (shell-command "git push gitea -f '*:*'"))
    (error "Failed to create repository")))

(defun gitea-mirror-repo (repo-name)
  (interactive (read-string "Enter repository name: " (project-name (project-current t))))
  (unless (project-current t)
    (user-error "Not in a git project"))
  (let ((list-of-remotes (shell-command-to-string "git remote -v")))
    (cond
     ((string-match "gitea" list-of-remotes) (error "Repository already exists on gitea"))
     ((string-match "origin" list-of-remotes) (gitea-create-new-repo repo-name))
     (t (gitea-create-new-repo repo-name)))))

(provide 'gitea)
