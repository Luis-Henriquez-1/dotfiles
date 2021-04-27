;; -*- lexical-binding: t -*-

;; This snippet boostraps the config.el file if it has to.

(let ((config-el-file (concat user-emacs-directory "config.el"))
      (config-org-file (concat user-emacs-directory "config.org")))
  (unless (or (file-exists-p config-el-file)
	      (file-newer-than-file-p config-org-file config-el-file))
    (let ((src-block-regexp org-babel-src-block-regexp)
	  (body)
	  (content))
      (with-temp-buffer
	(insert-file-contents config-org-file)
	(goto-char (point-min))
	(while (re-search-forward src-block-regexp nil t nil)
	  (setq body (match-string-no-properties 5))
	  (push body content))))
    (with-temp-file config-file
      (insert (mapconcat #'identity (nreverse content) "\n"))))
  (load-file config-el-file))
