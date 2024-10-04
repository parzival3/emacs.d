(with-current-buffer "device_list.h"
  (save-mark-and-excursion
    (save-match-data
      (defun refine-search (capture)
        (let* ((query-string "(init_declarator declarator: (identifier) @name value: (number_literal) @value)")
               (query (treesit-query-compile 'cpp query-string)))
          (treesit-query-capture (cdr capture) query)))
      (let* ((query-string "(init_declarator declarator: (identifier)  value: (number_literal) ) @capture")
             (query (treesit-query-compile 'cpp query-string))
             (captures (treesit-query-capture (treesit-buffer-root-node) query)))
        (setq mcaptures (mapcar #'refine-search captures))))))


(with-current-buffer csv-buffer
  (save-mark-and-excursion
    (save-match-data
      (let ((out-buffer (get-buffer-create "missing_decimal.csv"))
             (epos-csv  "EPOS PID Master List .csv"))
        (defun print-missing (capture)
          (let* ((name (treesit-node-text (cdar capture)))
                  (value (treesit-node-text (cdadr capture)))
                  (value (cl-parse-integer value :start 2 :radix 16 :junk-allowed t))
                  friendly-name
                  product-name)
            (goto-char (point-min))
            (unless (or (s-contains? "lenovo" name t)
                      (s-contains? "amz" name t)
                      (s-contains? "gsp" name t)
                      (s-contains? "gsa" name t)
                      (s-contains? "gtw" name t)
                      (s-contains? "gts" name t)
                      (s-contains? "gaming" name t)
                      (s-contains? "mask" name t)
                      (s-starts-with? "h3" name t)
                      (s-starts-with? "yku" name t)
                      (s-starts-with? "pid" name t)
                      (s-starts-with? "ac" name t)
                      (s-contains? "recovery" name t)
                      (s-contains? "default" name t)
                      (s-starts-with? "vid" name t)
                      (search-forward-regexp (format "%x" value) nil t))
              (with-current-buffer epos-csv
                (save-match-data
                (goto-char (point-min))
                (search-forward-regexp (format "0x%04x" value) nil nil)
                (beginning-of-line)
                (if (search-forward-regexp "^\\([[:digit:]],\\)\\{2\\}\\(.*?\\),\\(.*?\\)," nil t)
                  (progn (setq friendly-name (match-string 3))
                    (setq product-name (match-string 2)))
                  (message "%s" name)
                  (setq product-name nil
                    friendly-name nil))
                ))
              (with-current-buffer out-buffer
                (let ((vid #x1395)
                       (pid value)
                       (recovery-pid (logior #x8000 value))
                       (friendly-name (or friendly-name ""))
                       (product-name (or product-name  ""))
                       (recovery-name (or (and product-name
                                            (not (string-empty-p product-name))
                                            (format "%s (recovery)" product-name) "")
                                        "")))
                  (insert (format "%s,0x%04x,0x%04x,%d,%d,%s,%s\n" name vid pid vid pid product-name friendly-name))
                  (insert (format "%s,0x%04x,0x%04x,%d,%d,%s,%s\n" name vid recovery-pid vid recovery-pid recovery-name friendly-name)))))))
        (with-current-buffer out-buffer
          (erase-buffer)
          (insert "DCI source code name,vid,pid,vid(decimal),pid(decimal),Product Name,Friendly Name\n"))
        (mapcar #'print-missing mcaptures)))))
