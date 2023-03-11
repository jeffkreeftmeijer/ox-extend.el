
# ox-extend: Structured extensions for ox.el

The `ox-extend` package requires `ox-publish.el` to advise `org-publish-file` and use the `org-publish-property`:

```emacs-lisp
(require 'ox-publish)
```

The `ox-extend-extensions-alist` is an association list for registered extensions: Each extension must register itseld in this list when it's loaded to be available for use in publishing projects.

```emacs-lisp
(setq ox-extend-extensions-alist '())
```

Each element consists of a symbol that identifies the extension and a list with an `:add` function to add the extension's advise and a `:remove` function to remove it.

```emacs-lisp
'('ox-html-git-mtime
  :add (lambda () (advice-add 'org-html-title :around #'ox-html-upcase-title))
  :remove (lambda () (advice-remove 'org-html-title #'ox-html-upcase-title))
```

To extend the functionality of the `org-publish-file` function, `ox-extend--advise` provides advise to add and remove the registered extensions. It finds the `:add` and `:remove` functions for all extensions added to the project's `:extensions` list in the `ox-extend-extensions-alist`.

For each extenson, the `:add` function is executed before `org-publish-file`, and `:remove` is called immediately after. This adds the extensions when they're needed, and removes them to prevent them from interfering with future publishing:

```emacs-lisp
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
```

The `ox-extend-add` and `ox-extend-remove` add and remove the main advise, enabling and disabling all extensions completely. When loading `ox-extend.el`, `ox-extend-add` is automatically called to install the main advise. The `ox-extend-remove` function exists for convenience and shouldn't have to be used regularly, if ever.

```emacs-lisp
(defun ox-extend-add ()
    (advice-add 'org-publish-file :around #'ox-extend--advise))

(defun ox-extend-remove ()
    (advice-remove 'org-publish-file #'ox-extend--advise))

(ox-extend-add)
```

Finally, use the `provide` fucntion to announce `ox-extend.el`:

```emacs-lisp
(provide 'ox-extend)
```


## Example

The *hello world* example adds advise to print "hello, world!" before `org-publish-file` is called, and "goodbye, world!" after.

Start by loading `ox-extend.el` by adding its directory to the `load-path` and calling `require`:

```emacs-lisp
(add-to-list 'load-path ".")
(require 'ox-extend)
```

With `ox-extend.el` loaded, add the extension to `ox-extend-extensions-alist` by passing an association list with an `:add` and a `:remove` key. Instead of actually altering the publishing behavior, both functions print a message to the `*Messages*` buffer:

```emacs-lisp
(add-to-list
 'ox-extend-extensions-alist '('hello-world :add (lambda () (message "hello, world!"))
                                            :remove (lambda () (message "goodbye, world!"))))
```

To try the extension, call `org-publish-file` with a project that lists `'hello-world` as one of its `:extensions`:

```emacs-lisp
(org-publish-file "ox-extend.org"
                  '("ox-extend-example"
                    :base-directory "."
                    :publishing-directory "dist"
                    :extensions ('hello-world)))
```

The `*Messages*` buffer prints both messages:

```
hello, world!
Publishing file ox-extend.org using ‘org-html-publish-to-html’
goodbye, world!
```


## Writing extensions

As an example, we're writing an extension named `ox-md-title`, which adds document titles to markdown files generated with `ox-md`.

Writing an extension involves advising one or more functions and registering the extension to be available to publishing projects. This extension will advise [`org-md-template`](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/ox-md.el#n721), which currently only returns the generated document contents:

```emacs-lisp
(defun org-md-template (contents _info)
  contents)
```

Our aim is to prepend the document's title with the correct markup.

First, require `ox-extend`. Unlike the example above, actual extensions don't alter the load path:

```emacs-lisp
(require 'ox-extend)
```

Then, write an `:around` [advice](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html) for `org-md-template`, which gets the original funcion and arguments. In the function body, we call out to `org-md--headline-title` to generate the title with arguments we find from the second argument in `args`:

```emacs-lisp
(defun ox-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args)))
    (let ((style (plist-get info :md-headline-style))
          (title (org-export-data (plist-get info :title) info)))
      (concat
       (org-md--headline-title (plist-get info :md-headline-style) 1 (org-export-data (plist-get info :title) info) nil)
       (apply orig-fun args)))))
```

To enable the extension, add `ox-md-title-add` and `ox-md-title-remove` and add them to the `ox-extend-extensions-alist`. We're also setting the `org-md-toplevel-hlevel` to `2`, as the extension adds an extra headline to the root of the page:

```emacs-lisp
(defun ox-md-title-add ()
  (setq org-md-toplevel-hlevel 2)
  (advice-add 'org-md-template :around #'ox-md-title--advise-template))

(defun ox-md-title-remove ()
  (setq org-md-toplevel-hlevel 1)
  (advice-remove 'org-md-template #'ox-md-title--advise-template))

(add-to-list
 'ox-extend-extensions-alist '('ox-md-title :add ox-md-title-add
                                            :remove ox-md-title-remove))
```

Finally, `provide` the `ox-md-title.el` package:

```emacs-lisp
(provide 'ox-md-title)
```

And use the extension when publishing<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>:

```emacs-lisp
(add-to-list 'load-path ".")
(require 'ox-md-title)

(org-publish-file "ox-extend.org"
                  '("ox-extend-markdown"
                    :base-directory "."
                    :publishing-directory "."
                    :publishing-function org-gfm-publish-to-gfm
                    :extensions ('ox-md-title)))
```

The exporter now prepends the document title in the Markdown export:

```shell
head ox-extend.md
```

```

# ox-extend: Structured extensions for ox.el

The `ox-extend` package requires `ox-publish.el` to advise `org-publish-file` and use the `org-publish-property`:

```emacs-lisp
(require 'ox-publish)
```

The `ox-extend-extensions-alist` is an association list for registered extensions: Each extension must register itseld in this list when it's loaded to be available for use in publishing projects.
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> : This publishing project uses [`ox-gf`](https://github.com/larstvei/ox-gfm) instead of Org's `ox-md`. That works because ox-gfm is a derived backend which also uses `org-md-template` as its template function.