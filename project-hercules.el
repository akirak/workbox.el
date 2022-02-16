;;; project-hercules.el --- Per-project transient keymaps -*- lexical-binding: t -*-

(require 'hercules)
(require 'project)

(defcustom project-hercules-composed-maps nil
  "A set of rules for composing keymaps."
  :type '(repeat (list (choice filename
                               function)
                       symbol)))

(defcustom project-hercules-hide-funs
  '(project-hercules-exit)
  "List of functions added to :hide-funs by default."
  :type '(repeat symbol))

(defcustom project-hercules-dispatch-fallback t
  "Whether to define a project keymap when none is found."
  :type 'boolean)

(defvar project-hercules-parent-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'project-hercules-exit)
    map)
  "The default parent parent map of composed keymaps.")

(defvar project-hercules-commands nil
  "Hash table to store per-project transient commands.

The user should not set this variable.")

;;;; Primary API

(cl-defmacro project-hercules-make-map (root &rest hercules-args
                                             &key init &allow-other-keys)
  "Define a hercules command for a project root.

ROOT should be the root directory of a project.

You can pass HERCULES-ARGS to `hercules-def' except for
:show-funs and :keymaps. Also, `project-hercules-hide-funs' is
added to :hide-funs.

If INIT is an expression, its evaluation result will be the
initial value of the keymap. Otherwise, a composed keymap is
created from `project-hercules-parent-map' according to rules
defined in `project-hercules-composed-maps'. See
`make-composed-keymap' for how composition works."
  (let ((hide-funs (append (plist-get hercules-args :hide-funs)
                           ;; The last argument is not copied, so mutations to
                           ;; the original variable would affect existing
                           ;; definitions.
                           project-hercules-hide-funs))
        (hercules-args (thread-first hercules-args
                         (project-hercules--remove-plist :init)
                         (project-hercules--remove-plist :hide-funs))))
    `(progn
       (project-hercules--ensure)
       (let* ((root (project-hercules--normalize-root ,root))
              (command (gethash root project-hercules-commands))
              (map-symbol (when command
                            (get command 'project-hercules-map))))
         (unless command
           (setq command (make-symbol "project-hercules-command"))
           (setq map-symbol (make-symbol "project-hercules-map"))
           (set map-symbol ,(or init '(project-hercules--make-default-map root)))
           (put command 'project-hercules-map map-symbol)
           (puthash root command project-hercules-commands))
         (hercules-def
          :show-funs command
          :hide-funs ',hide-funs
          :keymap map-symbol
          ,@hercules-args)
         (symbol-value map-symbol)))))

;;;###autoload
(defun project-hercules-dispatch ()
  "Dispatch the keymap for the current project root.

This command dispatches a transient keymap defined using
`project-hercules-make-map'.

If there is no keymap defined for the project but
`project-hercules-dispatch-fallback' is non-nil, a fallback is
created and used. The fallback is the same as the initial map
created in `project-hercules-make-map'."
  (interactive)
  (if-let (root (project-root (project-current)))
      (if-let (command (project-hercules--find-by-root root))
          (funcall-interactively command)
        (if project-hercules-dispatch-fallback
            (progn
              (project-hercules-make-map root)
              (project-hercules-dispatch))
          (user-error "Not found for project %s" root)))
    (user-error "No project found")))

;;;; The default keymaps

(defun project-hercules--make-default-map (root)
  "Create the default keymap for ROOT."
  (let ((matches (seq-filter (lambda (rule)
                               (project-hercules--test-rule (car rule)
                                                            root))
                             project-hercules-composed-maps)))
    (make-composed-keymap
     (seq-map (pcase-lambda (`(,_ ,map-symbol . ,_))
                (symbol-value map-symbol))
              matches)
     project-hercules-parent-map)))

(defun project-hercules--test-rule (condition root)
  "Test the CONDITION of a rule against ROOT."
  (cl-etypecase condition
    (string (file-exists-p (expand-file-name condition root)))))

;;;; Administration commands

(defun project-hercules-remove-project (root)
  "Remove the definition for the project ROOT.

Because `project-hercules-make-map' does not override an existing
value for the project, you may sometimes need to run this
function before you re-evaluate a `project-hercules-make-map'
form."
  (interactive (list (or (project-root (project-current))
                         (user-error "No project found"))))
  (project-hercules--ensure)
  (remhash (project-hercules--normalize-root root)
           project-hercules-commands))

(defun project-hercules-display-keymap (root)
  "Display the keymap for the current project ROOT."
  (interactive (list (or (project-root (project-current))
                         (user-error "Not in a project"))))
  (pp-display-expression (or (project-hercules--get-map root)
                             (user-error "No definition for root %s"
                                         root))
                         "*Hercules Keymap*"))

(defun project-hercules--get-map (root)
  "Return a list of keymaps for ROOT, if any."
  (when-let* ((command (project-hercules--find-by-root root))
              (symbol (get command 'project-hercules-map)))
    (accessible-keymaps (symbol-value symbol))))

;;;; Utilities

;;;;; Hashtable operations

(defun project-hercules--ensure ()
  (unless project-hercules-commands
    (setq project-hercules-commands
          (make-hash-table :test #'equal))))

(defun project-hercules--find-by-root (root)
  (project-hercules--ensure)
  (or (gethash (project-hercules--normalize-root root)
               project-hercules-commands)
      (when-let (worktrees (let ((default-directory root))
                             (thread-last (magit-list-worktrees)
                               (mapcar #'car)
                               (delq root))))
        (catch 'found
          (dolist (worktree worktrees)
            (when-let (command (gethash (project-hercules--normalize-root worktree)
                                        project-hercules-commands))
              (throw 'found command)))))))

;;;;; Miscellaneous

(defun project-hercules--normalize-root (root)
  (file-truename (file-name-as-directory root)))

(defun project-hercules--remove-plist (plist prop)
  (cond
   ((null plist) plist)
   ((eq prop (car plist))
    (project-hercules--remove-plist (cddr plist) prop))
   (t
    `(,(nth 0 plist)
      ,(nth 1 plist)
      ,@(project-hercules--remove-plist (cddr plist) prop)))))

(provide 'project-hercules)
;;; project-hercules.el ends here
