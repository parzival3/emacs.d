(require 'json)
(require 'project)

(defcustom gitea-url "https://git.haento.info"
  "The url of the gitea server")

(defcustom gitea-token
    (with-temp-buffer
        (insert-file-contents "~/.gitea-token")
        (buffer-string))
    "The token to use to connect to gitea")

(defun gitea-create-new-repo (repo-name)
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
                      (kill-buffer response-buffer))
                    )))

(provide 'gitea)
