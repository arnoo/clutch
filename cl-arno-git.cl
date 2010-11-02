(defpackage :cl-arno-git
    (:use     #:cl #:cl-arno)
    (:export  #:git-revision-or-sha1 #:git-modified-p #:git-last-commit))

(in-package :cl-arno-git)

(defun git-revision-or-sha1 (file)
  (if (and #1=(git-last-commit file) (not (git-modified-p file)))
    (str "git-" #1#)
    (str "sh1-" (~ "/^((\\d|[a-f])+)\\s/" (sh (str "sha1sum " file)) 1))))

(defun git-modified-p (file)
  (unless (git-last-commit file) (error (str file " is not tracked in Git")))
  (> (length (sh (str "git status -s -- " file))) 0))

(defun git-last-commit (file)
  (aif (~ "/^((\\d|[a-f])+)\\s/" (sh (str "git log --pretty=oneline -- " file)) 1)
    it))
