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
  ;; Tangle my emacs files.
  (dolist (file (directory-files "emacs/" t (rx (1+ anything) ".org")))
    (printf "Tangling %s..." file)
    (org-babel-tangle-file file))

  (printf "Done!")
  ;; Testing stuff
  ;; (let* ((bootstrap-file (cl-find-if (apply-partially #'string-match "bootstrap\\.sh") command-line-args))
  ;; 	 (dotfiles-dir (file-name-directory bootstrap-file)))
  ;;   (printf "Loading the library of babel from %s..." (expand-file-name "library-of-babel.org" dotfiles-dir))
  ;;   (org-babel-lob-ingest (expand-file-name "library-of-babel.org" dotfiles-dir))
  ;;   (printf "Tangling emacs config from %s..." (expand-file-name "emacs.org" dotfiles-dir))
  ;;   (org-babel-tangle-file (expand-file-name "emacs.org" dotfiles-dir))
  ;;   (printf "Done."))
  )

;; Local Variables:
;; mode: emacs-lisp
;; End:
