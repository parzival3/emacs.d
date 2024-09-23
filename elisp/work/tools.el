;;; tools.el --- Work tools -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package deferred
  :defer t
  :ensure nil)

(use-package jiralib
  :defer t
  :ensure nil
  :custom
  (jiralib-url "https://jira.kitenet.com/"))

(use-package org-jira
  :defer t
  :ensure nil
  :custom
  (org-jira-working-dir (concat user-emacs-directory "org-jira"))
  (org-jira-project-filename-alist (list "SECDCI" "DCI"))
  (org-jira-custom-jqls (list
                          '(:jql "project = SECDCI and ('Epic Link'  = SECDCI-3523 or 'Epic Link'  = SECDCI-3210) and status != Closed  order by created DESC"
                            :limit 100
                            :filename "dci-web-tickets")))
  :config
  (unless (file-directory-p org-jira-working-dir)
    (make-directory org-jira-working-dir)))
