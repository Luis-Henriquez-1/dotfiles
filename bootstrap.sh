:;exec emacs -batch -l "$0" -f main "$@"

;; For now I write this code here. But eventually, I want it all to go in
;; bootstrap.org. This file should only load the bootstrap.org file.

;; As I mentioned (here), we want to minimize the amount of elisp code written
;; in non org files. Therefore, the only purpose of this file is to run the
;; bootstrap.org file which will actually do the bootstrapping.

(require 'org)

(defun printf (format-string &rest args)
  "Same as print, but allows format."
  (print (apply #'format format-string args)))

(setq org-babel-default-header-args
      '((:session . "none")
	(:results . "silent")
	(:mkdirp  . "yes")
	(:exports . "code")
	(:cache   .  "no")
	(:noweb   .  "no")
	(:hlines  .  "no")
	(:tangle  .  "no")))

(setq org-confirm-babel-evaluate nil)

(defun main ()
  (org-babel-tangle-file "~/dotfiles/recipes.org")
  (org-babel-tangle-file "~/dotfiles/emacs.org"))

;; Local Variables:
;; mode: emacs-lisp
;; End:
