:;exec emacs -batch -l "$0" -f main "$@"

;; (require 'org)
;; (require 'org-babel)

(defun main ()
  (print (version))
  (print (format "I did it. you passed in %s" command-line-args-left))
  (sleep-for 2)
  (if (y-or-n-p "Do you want to do this?") (print "hello") (print "bye")))

;; Local Variables:
;; mode: emacs-lisp
;; End:
