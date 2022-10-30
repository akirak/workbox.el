;;; workbox.el --- Per-project transient keymaps -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (hercules "0.3"))
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

(require 'hercules)
(require 'project)
(require 'cl-lib)
(require 'subr-x)

(declare-function pp-display-expression "pp")

(defgroup workbox nil
  "A helper macro for per-project keymaps."
  :prefix "workbox-"
  :group 'project
  :group 'hercules)

(require 'workbox-utils)

(defcustom workbox-composed-maps nil
  "A set of rules for composing keymaps."
  :type '(repeat (list (choice filename
                               function)
                       symbol)))

(defcustom workbox-hide-funs
  '(workbox-exit)
  "List of functions added to :hide-funs by default."
  :type '(repeat symbol))

(defcustom workbox-dispatch-fallback t
  "Whether to define a project keymap when none is found."
  :type 'boolean)

(defvar workbox-parent-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'workbox-exit)
    map)
  "The default parent parent map of composed keymaps.")

(defvar workbox-default-directory nil
  "Directory in which package commands are run.

When commands are completed in `workbox-npm',
`workbox-mix', etc., this variable is set to the root
directory of the package. This can be useful for running the
command in an alternative action through embark, for example.")

(defvar workbox-commands nil
  "Hash table to store per-project transient commands.

The user should not set this variable.")

;;;; Primary API

(cl-defun workbox-make-map (root &rest hercules-args
                                          &key init &allow-other-keys)
  "Define a hercules command for a project root.

ROOT should be the root directory of a project.

You can pass HERCULES-ARGS to `hercules-def' except for
:show-funs and :keymaps. Also, `workbox-hide-funs' is
added to :hide-funs.

If INIT is a quoted expression, its evaluation result will be the
initial value of the keymap. Otherwise, a composed keymap is
created from `workbox-parent-map' according to rules
defined in `workbox-composed-maps'. See
`make-composed-keymap' for how composition works."
  (let ((hide-funs (append (plist-get hercules-args :hide-funs)
                           ;; The last argument is not copied, so mutations to
                           ;; the original variable would affect existing
                           ;; definitions.
                           workbox-hide-funs))
        (hercules-args (thread-first hercules-args
                         (workbox--remove-plist :init)
                         (workbox--remove-plist :hide-funs))))
    (workbox--ensure)
    (let* ((root (workbox--normalize-root root))
           (command (gethash root workbox-commands))
           (map-symbol (when command
                         (or (get command 'workbox-map)
                             (error "Keymap for %s is not found" command)))))
      (if command
          (workbox--make-default-map root (symbol-value map-symbol))
        (setq command (make-symbol "workbox-command"))
        (setq map-symbol (make-symbol "workbox-map"))
        (set map-symbol (or (eval init)
                            (workbox--make-default-map root)))
        (put command 'workbox-map map-symbol)
        (puthash root command workbox-commands))
      (apply #'hercules-def
             :show-funs command
             :hide-funs hide-funs
             :keymap map-symbol
             hercules-args)
      (symbol-value map-symbol))))

;;;###autoload
(defun workbox-dispatch ()
  "Dispatch the keymap for the current project root.

This command dispatches a transient keymap defined using
`workbox-make-map'.

If there is no keymap defined for the project but
`workbox-dispatch-fallback' is non-nil, a fallback is
created and used. The fallback is the same as the initial map
created in `workbox-make-map'."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (if-let (command (workbox--find-by-root root))
          (funcall-interactively command)
        (if workbox-dispatch-fallback
            (progn
              (workbox-make-map root)
              (workbox-dispatch))
          (user-error "Not found for project %s" root)))
    (user-error "No project found")))

;;;; The default keymaps

(defun workbox--make-default-map (root &optional current)
  "Create the default keymap for the current project root.

ROOT is the root directory of a project.

If CURRENT is given, it should be the current keymap of the
project, and it will be updated."
  (let* ((matches (seq-filter (lambda (rule)
                                (workbox--test-rule (car rule)
                                                             root))
                              workbox-composed-maps))
         (maps (seq-map (pcase-lambda (`(,_ ,map-symbol . ,_))
                          (symbol-value map-symbol))
                        matches)))
    (if (and current (keymapp current))
        (dolist (map maps)
          (unless (member map (cdr current))
            (setcdr current (cons map (copy-sequence (cdr current)))))))
    (make-composed-keymap maps workbox-parent-map)))

(defun workbox--test-rule (condition root)
  "Test the CONDITION of a rule against ROOT."
  (cl-etypecase condition
    (string (file-exists-p (expand-file-name condition root)))))

;;;; Administration commands

(defun workbox-remove-project (root)
  "Remove the definition for the project ROOT.

Because `workbox-make-map' does not override an existing
value for the project, you may sometimes need to run this
function before you re-evaluate a `workbox-make-map'
form."
  (interactive (list (or (project-root (project-current))
                         (user-error "No project found"))))
  (workbox--ensure)
  (remhash (workbox--normalize-root root)
           workbox-commands))

(defun workbox-display-keymap (root)
  "Display the keymap for the current project ROOT."
  (interactive (list (or (project-root (project-current))
                         (user-error "Not in a project"))))
  (require 'pp)
  (pp-display-expression (or (workbox--get-map root)
                             (user-error "No definition for root %s"
                                         root))
                         "*Hercules Keymap*"))

(defun workbox--get-map (root)
  "Return a list of keymaps for ROOT, if any."
  (when-let* ((command (workbox--find-by-root root))
              (symbol (get command 'workbox-map)))
    (accessible-keymaps (symbol-value symbol))))

;;;; Utilities

;;;;; Hashtable operations

(defun workbox--ensure ()
  (unless workbox-commands
    (setq workbox-commands
          (make-hash-table :test #'equal))))

(defun workbox--find-by-root (root)
  (workbox--ensure)
  (or (gethash (workbox--normalize-root root)
               workbox-commands)
      (when-let (worktrees (workbox--git-worktrees root))
        (catch 'found
          (dolist (worktree worktrees)
            (when-let (command (gethash (workbox--normalize-root worktree)
                                        workbox-commands))
              (throw 'found command)))))))

;;;;; Miscellaneous

(defun workbox--normalize-root (root)
  (file-truename (file-name-as-directory root)))

(defun workbox--git-worktrees (root)
  "Return git worktrees for ROOT."
  (let ((default-directory root))
    (cl-remove (expand-file-name (string-remove-suffix "/" root))
               (thread-last (process-lines "git" "--no-pager"
                                           "worktree" "list" "--porcelain")
                 (mapcar (lambda (line)
                           (save-match-data
                             (when (string-match (rx bol "worktree " (group (+ anything)))
                                                 line)
                               (match-string 1 line)))))
                 (delq nil))
               :test #'equal)))

(defalias 'workbox-exit #'ignore)

(defmacro workbox-with-package-root (filename &rest progn)
  (declare (indent 1))
  `(let* ((root (or (locate-dominating-file default-directory ,filename)
                    (error "File %s is not found" ,filename)))
          (default-directory root))
     (setq workbox-default-directory root)
     ,@progn))

(provide 'workbox)
;;; workbox.el ends here
