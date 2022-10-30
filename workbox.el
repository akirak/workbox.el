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

;;;; API

(defun workbox-locate-package ()
  (let ((dir default-directory)
        (regexp (rx-to-string `(and bol
                                    (or ,@(mapcar #'car workbox-package-config-alist))
                                    eol))))
    (cl-flet
        ((match (file)
           (string-match-p regexp file)))
      (catch 'found
        (while dir
          (let ((files (directory-files dir)))
            (when-let (file (seq-find #'match files))
              (setq workbox-package-config (assoc file workbox-package-config-alist))
              (setq workbox-default-directory dir)
              (throw 'found file))
            (when (member workbox-git-dir files)
              (throw 'found nil))
            (setq dir (workbox--parent-dir dir))))
        nil))))

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

(provide 'workbox)
;;; workbox.el ends here
