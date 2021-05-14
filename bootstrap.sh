:;exec emacs -batch -l "$0" -f main "$@"

;; For now I write this code here. But eventually, I want it all to go in
;; bootstrap.org. This file should only load the bootstrap.org file.

;; As I mentioned (here), we want to minimize the amount of elisp code written
;; in non org files. Therefore, the only purpose of this file is to run the
;; bootstrap.org file which will actually do the bootstrapping.

(require 'org)
(require 'org-babel)

(defun printf (format-string &rest args)
  "Same as print, but allows format."
  (print (apply #'format format-string args)))

;; We want to load the file but
(defun org-babel%load-file ()
  "")

(org-babel-map-src-blocks)

;; Local Variables:
;; mode: emacs-lisp
;; End:
