;; open-in-github.el --- Easy github linking in Emacs
;;
;; Copyright (C) 2015 Dan Smith
;;

(defun get-git-root (path)
  (while (and (not (string= path "")) (not (file-exists-p (concat path "/.git"))))
    (setq path (mapconcat 'identity (butlast (split-string path "/")) "/")))
  path)

(defun get-git-suffix (path)
  (substring path (length (get-git-root path))))

(defun get-project-name (gitpath)
  (with-temp-buffer
    (insert-file-contents (concat gitpath "review"))
    (search-forward-regexp "^project=")
    (substring (buffer-substring-no-properties (point) (line-end-position)) 0 -4)))

(defun get-git-hash (gitpath)
  (substring (shell-command-to-string "git rev-parse HEAD") 0 40))

(defun open-in-github ()
  (interactive)
  (let ((repopath (get-git-suffix (buffer-file-name)))
	(project (get-project-name (concat (get-git-root (buffer-file-name)) "/.git"))))
    (browse-url (concat "http://github.com/"
			project
			"/tree/"
                        (get-git-hash project)
			repopath
			(get-range)))))

(defun get-range ()
  (let ((one (count-lines 1 (point)))
	(two (count-lines 1 (+ 1 (mark)))))
    (concat "#L" (number-to-string (min one two))
	    "-L" (number-to-string (max one two)))))


(provide 'open-in-github)
