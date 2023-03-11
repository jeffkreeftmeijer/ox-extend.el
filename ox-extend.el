;;; ox-extend.el --- Structured extensions for ox.el

;;; Commentary:

;; ox-extend.el adds :extensions to org-publish-project-alist.
;;
;; Each extension registers itself in ox-extend-extensions-alist
;; with a function to add and remove it. In
;; org-publish-project-alist, the :extensions key holds a list of
;; enabled extensions for that specific publishing project.
;;
;; The enabled extensions are added before org-publish-file, and
;; removed immediately after.

;;; Code:

(require 'ox-publish)

(setq ox-extend-extensions-alist '())

(defun ox-extend--advise (orig-fun &rest args)
  "Advise org-publish-file (ORIG-FUN) to add and remove each extension contained in ARGS."
  (let ((extensions (org-publish-property :extensions (nth 1 args))))
    (dolist (extension extensions)
      (ox-extend--apply extension :add))
    (apply orig-fun args)
    (dolist (extension extensions)
      (ox-extend--apply extension :remove))))

(defun ox-extend--apply (extension add_or_remove)
  "Call the ADD_OR_REMOVE function for an EXTENSION."
  (apply (plist-get (cdr (assoc extension ox-extend-extensions-alist))
		    add_or_remove)
	 ()))

(defun ox-extend-add ()
    (advice-add 'org-publish-file :around #'ox-extend--advise))

(defun ox-extend-remove ()
    (advice-remove 'org-publish-file #'ox-extend--advise))

(ox-extend-add)

(provide 'ox-extend)

;;; ox-extend.el ends here
