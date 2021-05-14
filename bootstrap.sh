:;exec emacs -batch -l "$0" -f main "$@"

;; For now I write this code here. But eventually, I want it all to go in
;; boostrap.org. This file should only load the bootstrap.org file.

(require 'org)
(require 'ob-lob)
(require 'cl-lib)
(require 'rx)

(defun printf (format-string &rest args)
  "Same as print, but allows format."
  (print (apply #'format format-string args)))

(defun main ()
  (let* ((bootstrap-file (cl-find-if (apply-partially #'string-match (rx "bootstrap.sh"))
				     command-line-args))
	 (dotfile-dir (file-name-directory (directory-file-name
					    bootstrap-file)))
	 (emacs-dir (expand-file-name "emacs/" dotfile-dir)))
    (when (y-or-n-p "Tangle emacs dir? ")
      (dolist (file (directory-files emacs-dir t (rx (1+ anything) ".org")))
        (printf "Tangling %s..." file)
        (org-babel-tangle-file file))))
  (printf "Done!"))

;; Local Variables:
;; mode: emacs-lisp
;; End:
