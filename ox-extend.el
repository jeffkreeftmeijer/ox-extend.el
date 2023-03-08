;;; ox-extend.el --- Structured extensions for ox.el

;;; Commentary:

;; ox-extend.el adds :extensions to org-publish-project-alist.
;;
;; Each extension registers itself in ox-extensions-alist with a
;; function to add and remove it. In org-publish-project-alist, the
;; :extensions key holds a list of enabled extensions for that
;; specific publishing project.
;;
;; The enabled extensions are added before org-publish-file, and
;; removed immediately after.

;;; Code:

(setq ox-extensions-alist '())

(defun ox-extend--apply (extension add_or_remove)
  "Call the ADD_OR_REMOVE function for an EXTENSION."
  (apply (plist-get (cdr (assoc extension ox-extend-extensions-alist))
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

;;;###autoload
(defun ox-extend-add ()
    (advice-add 'org-publish-file :around #'ox-extend--advise-org-publish-file))

(defun ox-extend-remove ()
    (advice-remove 'org-publish-file #'ox-extend--advise-org-publish-file))

(provide 'ox-extend)

;;; ox-extend.el ends here
