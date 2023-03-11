(require 'ert)
(add-to-list 'load-path ".")
(require 'ox-extend)

(add-to-list
 'ox-extend-extensions-alist '('hello-world :add (lambda () (message "hello, world!"))
                                            :remove (lambda () (message "goodbye, world!"))))

(ert-deftest hello-world-test ()
  (org-publish-file "ox-extend.org"
                    '("ox-extend-example"
                      :base-directory "."
                      :publishing-directory "dist"
                      :extensions ('hello-world)))

  (let ((output (with-current-buffer "*Messages*" (buffer-string))))
    (let ((lines (split-string output "\n")))

      (should (string-match-p "hello, world!" output))
      (should (string-match-p "ox-extend.org" output))
      (should (string-match-p "goodbye, world!" output))))

  (delete-directory "dist" t))
