;;; workbox-npm.el --- Support for npm scripts -*- lexical-binding: t -*-

(require 'map)
(require 'workbox)

(defgroup workbox-npm nil
  "Run a npm command."
  :prefix "workbox-npm-"
  :group 'workbox)

(define-widget 'workbox-npm-subcommands-type 'lazy
  "Built-in subcommand of a package manager."
  :tag "Package manager"
  :type '(repeat (cons (string :tag "Subcommand")
                       (plist :options
                              (((const :description)
                                string))))))

(defcustom workbox-npm-pm-alist
  '(("pnpm"
     :lock "pnpm-lock.yaml"
     :install-command "install"
     :builtin-commands workbox-npm-pnpm-commands)
    ("yarn"
     :lock "yarn.lock"
     :install-command ""
     :builtin-commands workbox-npm-yarn-commands)
    ("npm"
     :lock "package-lock.json"
     :install-command "install"
     :script-command "run"
     :builtin-commands workbox-npm-yarn-commands)
    ("bun"
     :lock "bun.lockb"
     :install-command "bun"
     :script-command "run"
     :builtin-commands workbox-npm-bun-commands))
  "Alist of package managers for package.json."
  :type '(alist :key-type string
                :value-type plist))

(defcustom workbox-npm-pnpm-commands
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
  :type 'workbox-npm-subcommands-type)

(defcustom workbox-npm-bun-commands
  '(("run"
     :description "Run JavaScript with bun, a package.json script, or a bin")
    ("build"
     :description "Build TypeScript and JavaScript into a single file")
    ("install"
     :description "Install dependencies")
    ("add"
     :description "Add a dependency")
    ("remove"
     :description "Remove a dpeendency")
    ("pm"
     :description "More commands for managing packages"))
  "List of pnpm subcommands that are not specific to a project."
  :type 'workbox-npm-subcommands-type)

(defcustom workbox-npm-yarn-commands
  ;; TODO Add a complete set of builtin subcommands
  '(("install"))
  "List of yarn subcommands that are not specific to a project."
  :type 'workbox-npm-subcommands-type)

(defcustom workbox-npm-npm-commands
  ;; TODO Add a complete set of builtin subcommands
  '(("install")
    ("lock"))
  "List of npm subcommands that are not specific to a project."
  :type 'workbox-npm-subcommands-type)

(defvar workbox-npm-history nil)

(defun workbox-npm--completion (program-ent)
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
           '(metadata . ((category . workbox-shell-command)
                         (annotation-function . workbox-npm-annotate)))
         (complete-with-action action ',scripts string pred)))))

(defun workbox-npm-annotate (candidate)
  (if-let (command (get-char-property 0 'command candidate))
      (concat " " command)
    (if-let (description (get-char-property 0 'description candidate))
        (concat " " description)
      "")))

(defun workbox-npm--install ()
  (let ((pm (completing-read (format "No lock file is found in %s. Choose a package manager: "
                                     (abbreviate-file-name default-directory))
                             (thread-last
                               workbox-npm-pm-alist
                               (mapcar #'car)
                               (seq-filter #'executable-find)))))
    (if-let (ent (assoc pm workbox-npm-pm-alist))
        (compile (concat pm " " (or (plist-get (cdr ent) :install-command)
                                    "")))
      (user-error "Please add an entry for %s to workbox-npm-pm-alist" pm))))

;;;###autoload
(defun workbox-npm ()
  "Run a npm command selected using `completing-read'."
  (interactive)
  (workbox-with-package-root "package.json"
    (let ((program-ent (seq-some (pcase-lambda (entry)
                                   (if-let (lock (plist-get (cdr entry) :lock))
                                       (when (file-exists-p lock)
                                         entry)
                                     (error "Missing :lock attribute in %s" entry)))
                                 workbox-npm-pm-alist)))
      (if program-ent
          (progn
            (unless (executable-find (car program-ent))
              (user-error "Executable %s is not found in PATH"
                          (car program-ent)))
            (compile (completing-read (format "Command (%s): " default-directory)
                                      (workbox-npm--completion program-ent)
                                      nil nil nil
                                      workbox-npm-history)))
        (workbox-npm--install)))))

(provide 'workbox-npm)
;;; workbox-npm.el ends here
