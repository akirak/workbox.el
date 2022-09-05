;;; project-hercules-npm.el --- Support for npm scripts -*- lexical-binding: t -*-

(require 'map)

(defgroup project-hercules-npm nil
  "Run a npm command."
  :prefix "project-hercules-npm-"
  :group 'project-hercules)

(define-widget 'project-hercules-npm-subcommands-type 'lazy
  "Built-in subcommand of a package manager."
  :tag "Package manager"
  :type '(repeat (cons (string :tag "Subcommand")
                       (plist :options
                              (((const :description)
                                string))))))

(defcustom project-hercules-npm-pm-alist
  '(("pnpm"
     :lock "pnpm-lock.yaml"
     :install-command "install"
     :builtin-commands project-hercules-npm-pnpm-commands)
    ("yarn"
     :lock "yarn.lock"
     :install-command ""
     :builtin-commands project-hercules-npm-yarn-commands)
    ("npm"
     :lock "package-lock.json"
     :install-command "install"
     :script-command "run"
     :builtin-commands project-hercules-npm-yarn-commands))
  "Alist of package managers for package.json."
  :type '(alist :key-type string
                :value-type plist))

(defcustom project-hercules-npm-pnpm-commands
  ;; TODO Add a complete set of builtin subcommands
  ;; See pnpm help
  '(("install"
     :description "Install all dependencies for a project")
    ("add"
     :description "Installs a package and any packages that it depends on")
    ("import"
     :description "Generates a pnpm-lock.yaml from an npm package-lock.json")
    ("remove"
     :description "Removes packages from node_modules and from the project's package.json")
    ("update"
     :description "Updates packages to their latest version based on the specified range")
    ("audit"
     :description "Checks for known security issues with the installed packages")
    ("outdated"
     :description "Check for outdated packages")
    ("exec"
     :description "Executes a shell command in scope of a project"))
  "List of pnpm subcommands that are not specific to a project."
  :type 'project-hercules-npm-subcommands-type)

(defcustom project-hercules-npm-yarn-commands
  ;; TODO Add a complete set of builtin subcommands
  '(("install"))
  "List of yarn subcommands that are not specific to a project."
  :type 'project-hercules-npm-subcommands-type)

(defcustom project-hercules-npm-npm-commands
  ;; TODO Add a complete set of builtin subcommands
  '(("install")
    ("lock"))
  "List of npm subcommands that are not specific to a project."
  :type 'project-hercules-npm-subcommands-type)

(defvar project-hercules-npm-history nil)

(defun project-hercules-npm--completion (program-ent)
  "Return a completion table for npm scripts."
  (let* ((program (car program-ent))
         (plist (cdr program-ent))
         (script-prefix (if-let (subcommand (plist-get plist :script-command))
                            (concat program " " subcommand)
                          program))
         (builtins-var (plist-get plist :builtin-commands))
         (data (with-temp-buffer
                 (insert-file-contents "package.json")
                 (goto-char (point-min))
                 (json-parse-buffer :array-type 'list)))
         (scripts (append (map-apply
                           `(lambda (subcommand body)
                              (propertize (concat ,script-prefix " " subcommand)
                                          'command body))
                           (map-elt data "scripts"))
                          (when builtins-var
                            (mapcar `(lambda (entry)
                                       (propertize (concat ,program " " (car entry))
                                                   'description
                                                   (plist-get (cdr entry) :description)))
                                    (symbol-value builtins-var))))))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . project-hercules-shell-command)
                         (annotation-function . project-hercules-npm-annotate)))
         (complete-with-action action ',scripts string pred)))))

(defun project-hercules-npm-annotate (candidate)
  (if-let (command (get-char-property 0 'command candidate))
      (concat " " command)
    (if-let (description (get-char-property 0 'description candidate))
        (concat " " description)
      "")))

(defun project-hercules-npm--install ()
  (let ((pm (completing-read "No lock file is found. Choose a package manager: "
                             (thread-last
                               project-hercules-npm-pm-alist
                               (mapcar #'car)
                               (seq-filter #'executable-find)))))
    (if-let (ent (assoc pm project-hercules-npm-pm-alist))
        (compile (concat pm " " (or (plist-get (cdr ent) :install-command)
                                    "")))
      (user-error "Please add an entry for %s to project-hercules-npm-pm-alist" pm))))

;;;###autoload
(defun project-hercules-npm ()
  "Run a npm command selected using `completing-read'."
  (interactive)
  (project-hercules-with-directory (locate-dominating-file default-directory
                                                           "package.json")
    (let ((program-ent (seq-some (pcase-lambda (entry)
                                   (if-let (lock (plist-get (cdr entry) :lock))
                                       (when (file-exists-p lock)
                                         entry)
                                     (error "Missing :lock attribute in %s" entry)))
                                 project-hercules-npm-pm-alist)))
      (if program-ent
          (compile (completing-read (format "Command (%s): " default-directory)
                                    (project-hercules-npm--completion program-ent)
                                    nil nil nil
                                    project-hercules-npm-history))
        (project-hercules-npm--install)))))

(provide 'project-hercules-npm)
;;; project-hercules-npm.el ends here
