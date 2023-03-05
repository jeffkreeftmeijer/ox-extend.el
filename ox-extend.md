```emacs-lisp
(setq ox-extensions-alist '())

(defun ox-extend--apply (extension add_or_remove)
  "Call the ADD_OR_REMOVE function for an EXTENSION."
  (apply (plist-get (cdr (assoc extension ox-extensions-alist))
                    add_or_remove)
         ()))

(defun ox-extend--advise-org-publish-file (orig-fun &rest args)
  "Advise org-publish-file (ORIG-FUN) to add and remove each extension contained in ARGS."
  (let ((extensions (org-publish-property :extensions (nth 1 args))))
    (dolist (extension extensions)
      (ox-extend--apply extension :add))
    (apply orig-fun args)
    (dolist (extension extensions)
      (ox-extend--apply extension :remove))))

(advice-add 'org-publish-file :around #'ox-extend--advise-org-publish-file)
```