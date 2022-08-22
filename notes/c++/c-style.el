(defconst dci-cc-style
 '("stroustrup" ; this is inheritance from the linux style
   (c-offsets-alist  . ((top-most-intro . --)
                        (defun-block-intro . +)
                        (func-decl-cont . 0)))))


(c-add-style "dci" dci-cc-style)
(setq c-default-style (append c-default-style '((cc-mode . "dci") (c++-mode . "dci") (c-mode . "dci"))))
