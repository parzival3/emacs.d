(use-package org-capture
  :config
  ;; TODO I need to improove this
  (defun org-jurnal-capture ()
    "Insert a new entry in the org jurnal."
    (interactive)
    (org-capture nil "j"))

  (defun org-todo-capture ()
    "Insert a new entry in the org gtd file."
    (interactive)
    (org-capture nil "t"))

  (setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat git-directory "notes/gtd.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree ,(concat git-directory "notes/journal.org"))
         "* %?\nEntered on %U\n  %i"))))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory (concat git-directory "notes/org-roam"))
  (setq org-roam-dailies-directory "daily/")
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)
  (require 'org-roam-protocol)

  (setq org-roam-bookmarklet
        "javascript:location.href = (function() {
            let hostname = window.location.hostname;
            let page_title_components = window.location.pathname.split('/');
            // add bitbucket to the hosts
            if (hostname === 'github.com') {
                if (page_title_components.length >= 3) {
                    let title = 'github_' + page_title_components[1] + page_title_components[2];
                    return 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href)  + '&title=' +  encodeURIComponent(title) + '&body=' + encodeURIComponent(window.getSelection().toString());
                } else {
                    alert('The page doesn\'t have 3 components so I cannot create the proper note');
                    throw new Error('Wrong parameters for hostname');
                }

            }

            if (hostname === 'jira.kitenet.com') {
                if (page_title_components.length >= 3 && page_title_components[1] === 'browse' && page_title_components[2].match(/SEC\w+-\d+/g)) {
                    return 'org-protocol://roam-ref?template=ji&ref=' + encodeURIComponent(location.href)  + '&title=' +  encodeURIComponent(document.title) + '&body=' + encodeURIComponent(window.getSelection().toString());
                } else {
                    alert('The page doesn\'t match the 3 components' + page_title_components[2] + ' ' + page_title_components[3]);
                    throw new Error('Wroong parameters for hostname');
                }
            }
            return 'org-protocol://roam-ref?template=r&ref='  + encodeURIComponent(location.href)  + '&title=' + encodeURIComponent(document.title) + '&body=' + encodeURIComponent(window.getSelection().toString());
         })()")

  (setq p-daily-note-filename "%<%Y-%m-%d>.org"
        p-daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%? \n #+begin_quote \n ${body} \n #+end_quote\n"
           :target (file+head "web/${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("ji" "JIRA" plain "* %?\n"
           :target (file+head "JIRA/${slug}.org" "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
           :unnarrowed t
           :empty-lines-before 1)))

  (setq org-roam-dailies-capture-templates
      `(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))
        ("j" "journal" entry
        "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
        :if-new (file+head+olp ,p-daily-note-filename
                               ,p-daily-note-header
                               ("Log")))
        ("f" "feeling" entry
        "* %<%I:%M %p> - Feeling  :feeling:\n\n%?\n\n"
        :if-new (file+head+olp ,p-daily-note-filename
                               ,p-daily-note-header
                               ("Log")))))

  (defun org-roam-insert-jurnal ()
    (interactive)
    (org-roam-dailies-capture-today nil "j"))

  (defun org-roam-insert-feeling ()
    (interactive)
    (org-roam-dailies-capture-today nil "f"))

  (defun p-org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun p-org-roam-find-project ()
    (interactive)
    (let ((new-project '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")))
      (org-roam-node-add-capture
       (p-org-roam-filter-by-tag "Project") ;; filter function
       :templates
       `(("t" "TODO" entry "* TODO %?\n  %i\n  %a" :if-new ,new-project :unnarrowed t)
         ("c" "comment" entry "* %? :comment:\n\n%a\n\n" :if-new ,new-project :unnarrowed t)))))

  (cl-defun org-roam-node-add-capture (&optional filter-fn &key templates)
    "Add a capture using TEMPLATES to a node filterd using FILTER-FN"
    (interactive current-prefix-arg)
    (let ((node-name (org-roam-node-read nil filter-fn nil)))
      (org-roam-capture-
       :node node-name
       :templates templates
       :props '(:finalize find-file)))))

(use-package org
  :config
  (setq org-notes-folder (concat git-directory "/notes/"))
  (defun org-list-of-notes ()
    (interactive)
      (find-file (completing-read "Select org note to open: " (directory-files-recursively org-notes-folder ".org"))))

  (setq org-export-backends (add-to-list 'org-export-backends 'md)))
