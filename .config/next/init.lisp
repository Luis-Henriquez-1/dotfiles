;; (in-package next)
;; (define-command org-capture (&optional (buffer (current-buffer)))
;;   "Org-capture current page."
;;   (eval-in-emacs
;;    `(org-link-set-parameters
;;      "next"
;;      :store (lambda ()
;;               (org-store-link-props
;;                :type "next"
;;                :link ,(url buffer)
;;                :description ,(title buffer))))
;;    `(org-capture)))

;; (define-key *my-keymap* "C-M-o" 'org-capture)

;;

;; (define-configuration buffer ((default-modes (append '(vi-normal-mode) %slot-default))))
