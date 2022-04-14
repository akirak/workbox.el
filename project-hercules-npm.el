;;; project-hercules-npm.el --- Support for npm scripts -*- lexical-binding: t -*-

(require 'map)

(defgroup project-hercules-npm nil
  "Run a npm command."
  :prefix "project-hercules-npm-"
  :group 'project-hercules)

(defcustom project-hercules-npm-pm-alist
  '(("pnpm"
     :lock "pnpm-lock.yaml"
     :builtin-commands project-hercules-npm-pnpm-commands)
    ("yarn"
     :lock "yarn.lock"
     :builtin-commands project-hercules-npm-yarn-commands)
    ("npm"
     :lock "package-lock.json"
     :script-command "run"
     :builtin-commands project-hercules-npm-yarn-commands))
  "Alist of package managers for package.json."
  :type '(alist :key-type string
                :value-type plist))

(defcustom project-hercules-npm-pnpm-commands
  ;; TODO Add a complete set of builtin subcommands
  '("install")
  "List of pnpm subcommands that are not specific to a project."
  :type '(repeat string))

(defcustom project-hercules-npm-yarn-commands
  ;; TODO Add a complete set of builtin subcommands
  '("install")
  "List of yarn subcommands that are not specific to a project."
  :type '(repeat string))

(defcustom project-hercules-npm-npm-commands
  ;; TODO Add a complete set of builtin subcommands
  '("install"
    "lock")
  "List of npm subcommands that are not specific to a project."
  :type '(repeat string))

(defvar project-hercules-npm-history nil)

(defun project-hercules-npm-completion (&optional dir)
  "Completion table for npm scripts."
  (let* ((default-directory (or dir
                                (locate-dominating-file default-directory
                                                        "package.json")))
         (program-ent (or (seq-some (pcase-lambda (entry)
                                      (if-let (lock (plist-get (cdr entry) :lock))
                                          (when (file-exists-p lock)
                                            entry)
                                        (error "Missing :lock attribute in %s" entry)))
                                    project-hercules-npm-pm-alist)
                          (error "No matching entry from project-hercules-npm-pm-alist")))
         (program (car program-ent))
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
                            (mapcar `(lambda (subcommand)
                                       (concat ,program " " subcommand))
                                    (symbol-value builtins-var))))))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . project-hercules-shell-command)
                         (annotation-function . project-hercules-npm-annotate)))
         (complete-with-action action ',scripts string pred)))))

(defun project-hercules-npm-annotate (candidate)
  (if-let (command (get-char-property 0 'command candidate))
      (concat " " command)
    ""))

;;;###autoload
(defun project-hercules-npm ()
  "Run a npm command selected using `completing-read'."
  (interactive)
  (let* ((default-directory (locate-dominating-file default-directory
                                                    "package.json"))
         (command (completing-read (format "Command (%s): " default-directory)
                                   (project-hercules-npm-completion
                                    default-directory)
                                   nil nil nil
                                   project-hercules-npm-history)))
    (compile command)))

(provide 'project-hercules-npm)
;;; project-hercules-npm.el ends here
