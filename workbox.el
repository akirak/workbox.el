;;; workbox.el --- Compilation helpers -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/workbox.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a library that provides a helper function fir defining per-project
;; transient maps. You can define keymaps using `workbox-make-map'
;; function and dispatch a keymap for the current project using
;; `workbox-dispatch'.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'workbox-utils)

(declare-function pp-display-expression "pp")

(defgroup workbox nil
  ""
  :prefix "workbox-"
  :group 'project)

(defconst workbox-git-dir ".git")

(defcustom workbox-package-config-alist
  '(("package.json" :complete-and-run workbox-npm)
    ("mix.exs" :complete-and-run workbox-mix))
  ""
  :type '(alist :key-type file
                :value-type plist))

(defcustom workbox-find-file-function #'find-file
  "Function used to open a readme file."
  :type 'function)

(defcustom workbox-doc-dir-regexp
  (rx bol (or "doc" "docs" "documentation") eol)
  "Regexp matching the name of a documentation directory."
  :type 'regexp)

(defcustom workbox-doc-file-regexp
  (rx (or ".md") eol)
  "Regexp matching file names of documentation files."
  :type 'regexp)

(defvar workbox-default-directory nil
  "Directory in which package commands are run.

When commands are completed in `workbox-npm',
`workbox-mix', etc., this variable is set to the root
directory of the package. This can be useful for running the
command in an alternative action through embark, for example.")

(defvar workbox-package-config nil)

(defvar workbox-path-separator nil)

;;;; Utilities

(cl-defsubst workbox--path-separator ()
  (or workbox-path-separator
      (setq workbox-path-separator
            (string-remove-prefix "a" (file-name-as-directory "a")))))

(defun workbox--parent-dir (dir)
  (thread-last
    dir
    (string-remove-suffix (workbox--path-separator))
    (file-name-directory)))

(defun workbox--locate-file-regexp (regexp)
  "Return a file matching REGEXP inside the current project."
  (let ((dir default-directory)
        (pr (project-current)))
    (cl-flet
        ((match (file)
           (string-match-p regexp file)))
      (catch 'found
        (while dir
          (when-let (file (seq-find #'match (directory-files dir)))
            (throw 'found (expand-file-name file dir)))
          (when (and pr (file-equal-p dir (project-root pr)))
            (throw 'found nil))
          (setq dir (workbox--parent-dir dir)))))))

(defun workbox--locate-files-regexp (regexp)
  "Return files matching REGEXP inside the current project."
  (let ((dir default-directory)
        (case-fold-search t)
        (pr (project-current))
        results)
    (cl-flet
        ((match (file) (string-match-p regexp file)))
      (catch 'abort
        (while dir
          (let ((files (directory-files dir)))
            (when-let (file (seq-find #'match files))
              (push (expand-file-name file dir) results)))
          (when (and pr (file-equal-p dir (project-root pr)))
            (throw 'abort nil))
          (setq dir (workbox--parent-dir dir))))
      (nreverse results))))

(defun workbox--select-project-file (prompt files)
  (let* ((prefix-len (when-let (pr (project-current))
                       (length (expand-file-name (project-root pr)))))
         (candidates (if prefix-len
                         (mapcar `(lambda (s)
                                    (put-text-property 0 ,prefix-len
                                                       'invisible t
                                                       s)
                                    s)
                                 (mapcar #'expand-file-name files))
                       files)))
    (cl-labels
        ((completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'file)))
             (complete-with-action action candidates string pred))))
      (completing-read prompt #'completions))))

;;;; API

(defun workbox-locate-package ()
  (when-let (file (workbox--locate-file-regexp
                   (rx-to-string `(and bol
                                       (or ,@(mapcar #'car workbox-package-config-alist))
                                       eol))))
    (let ((sans-dir (file-name-nondirectory file)))
      (setq workbox-package-config (assoc sans-dir workbox-package-config-alist))
      (setq workbox-default-directory (file-name-directory file))
      file)))

(defmacro workbox-with-package-root (filename &rest progn)
  (declare (indent 1))
  `(let* ((root (or (locate-dominating-file (or workbox-default-directory
                                                default-directory)
                                            ,filename)
                    (error "File %s is not found" ,filename)))
          (default-directory root))
     (setq workbox-default-directory root)
     ,@progn))

;;;; Interactive commands

;;;###autoload
(defun workbox-run-some-package-manager ()
  (interactive)
  (if-let* ((package (workbox-locate-package))
            (cmd (plist-get (cdr workbox-package-config) :complete-and-run)))
      (call-interactively cmd)
    (user-error "No package is found")))

;;;###autoload
(defun workbox-readme ()
  "Browse a readme file in the project."
  (interactive)
  (if-let (files (workbox--locate-files-regexp (rx bol "README" (or "." eol))))
      (let ((default-directory (project-root (project-current))))
        (funcall workbox-find-file-function
                 (if (eq 1 (length files))
                     (car files)
                   (workbox--select-project-file "Readme: " files))))
    (user-error "No readme is found")))

;;;###autoload
(defun workbox-doc ()
  (interactive)
  (let* ((pr (or (project-current)
                 (user-error "You must run this command inside a project")))
         (dir (workbox--doc-dir pr))
         (files (if dir
                    (project-files pr (list dir))
                  (seq-filter (lambda (file)
                                (string-match-p workbox-doc-file-regexp file))
                              (project-files pr))))
         (default-directory (project-root pr)))
    (funcall workbox-find-file-function
             (workbox--select-project-file "Documentation: " files))))

(defun workbox--doc-dir (pr)
  (let ((root (project-root pr)))
    (when-let (dir (seq-find (lambda (file)
                               (string-match-p workbox-doc-dir-regexp file))
                             (directory-files root)))
      (file-name-as-directory (expand-file-name dir root)))))

(provide 'workbox)
;;; workbox.el ends here
