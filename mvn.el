;;; maven.el --- helpers for compiling with maven

;; Copyright (C) 2013 Andrew Gwozdziewycz <git@apgwoz.com>

;; Version: 0.1
;; Keywords: compilation, maven, java

;; This file is NOT part of GNU Emacs

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar mvn-last-task "compile")
(defvar mvn-build-file-name "pom.xml")
(defvar mvn-command "mvn")

(defvar mvn-tasks-default '("compile" "test" "clean"))

(defun mvn-find-tasks (directory)
  (let ((output (shell-command-to-string (concat *mvn-tasks-command* " "
                                                 directory "/"
                                                 mvn-build-file-name))))
    (message output)
    (if (> (length output) 0)
        (mapcar '(lambda (x) (replace-regexp-in-string ".*<target.*name=\"\\([^\-][^\"]*\\).*" "\\1" x)) 
                (split-string output "[\n]"))
      nil)))

;; should cache tasks from the build file at some point
(defun mvn-tasks (directory)
  (let ((tasks (assoc-string directory *mvn-tasks-cache*)))
    (cdr 
     (or tasks
         (progn 
           (let ((newtasks (or (mvn-find-tasks directory) mvn-tasks-default)))
             (setq *mvn-tasks-cache*
                   (cons (cons directory newtasks) *mvn-tasks-cache*))
             newtasks))))))

(defun mvn-get-task (directory)
  (let ((task (completing-read-multiple (concat "Goal (default): ") 
                                        mvn-tasks-default)))
    (if (> (length task) 0)
        (mapconcat 'identity task " ")
      "")))

(defun mvn-find-root (indicator)
  (let ((cwd default-directory))
    (locate-dominating-file cwd mvn-build-file-name)))

;;;###autoload
(defun mvn-kill-cache ()
  (interactive)
  (setq *mvn-tasks-cache* '()))

;;;###autoload
(defun mvn (&optional task args)
  "Run mvn `task` in project root directory."
  (interactive)
  (let ((default-directory (mvn-find-root mvn-build-file-name)))
    (if default-directory
        (let ((task (or task (mvn-get-task default-directory))))
          (setq mvn-last-task task)
          (compile (concat mvn-command " " task " " args)))
      (message "Couldn't find a maven project."))))

;;;###autoload
(defun mvn-last ()
  "Run the last maven task in project"
  (interactive)
  (mvn (or mvn-last-task "")))

;;;###autoload
(defun mvn-compile ()
  (interactive)
  (mvn "compile"))

;;;###autoload
(defun mvn-clean ()
  (interactive)
  (mvn "clean"))

;;;###autoload
(defun mvn-test (prefix)
  (interactive "MTest: ")
  (if prefix
      (mvn "test" (concat "-Dtest=" prefix))
    (mvn "test")))

(provide 'mvn)
