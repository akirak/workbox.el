;;; project-hercules-mix.el --- Mix integration -*- lexical-binding: t -*-

(require 'project-hercules-process)

(defvar project-hercules-mix-command-cache nil
  "Hash table that stores per-project alists of commands.")

(defvar project-hercules-mix-command-alist nil)

;;;###autoload
(defun project-hercules-mix ()
  "Run a Mix command."
  (interactive)
  (let* ((default-directory (locate-dominating-file default-directory
                                                    "mix.exs"))
         (command (completing-read (format "Mix command (%s): " default-directory)
                                   (project-hercules-mix-completion))))
    (project-hercules-mix--add-command (string-trim command))
    (compile command)))

(defun project-hercules-mix-clear ()
  (interactive)
  (clrhash project-hercules-mix-command-cache))

(defun project-hercules-mix-completion ()
  (let* ((command-alist (project-hercules-mix--commands)))
    (setq project-hercules-mix-command-alist command-alist)
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . project-hercules-shell-command)
                         (annotation-function . project-hercules-mix-annotate)))
         (complete-with-action action ',command-alist string pred)))))

(defun project-hercules-mix-annotate (command)
  (when-let (description (cdr (assoc command project-hercules-mix-command-alist)))
    ;; TODO Set a custom face
    (concat " " (propertize description 'face 'font-lock-comment-face))))

(defun project-hercules-mix--ensure-cache ()
  (unless project-hercules-mix-command-cache
    (setq project-hercules-mix-command-cache
          (make-hash-table :test #'equal))))

(defun project-hercules-mix--commands ()
  "Return an alist of Mix commands for the project."
  (project-hercules-mix--ensure-cache)
  (or (gethash default-directory project-hercules-mix-command-cache)
      (let (result)
        (with-temp-buffer
          (project-hercules-process-insert-stdout "mix" "help")
          (goto-char (point-min))
          (save-match-data
            (while (re-search-forward (rx bol (* space) (group "mix" (* (not (any "#"))))
                                          " # " (group (+ nonl)) eol)
                                      nil t)
              (push (cons (string-trim-right (match-string 1))
                          (string-trim (match-string 2)))
                    result))))
        (setq result (nreverse (copy-sequence result)))
        (puthash default-directory result project-hercules-mix-command-cache)
        result)))

(defun project-hercules-mix--add-command (command)
  "Add a Mix command to the per-project cache."
  (let ((current (project-hercules-mix--commands)))
    (unless (assoc command current)
      (push (list command nil) current)
      (puthash default-directory current project-hercules-mix-command-cache))))

(provide 'project-hercules-mix)
;;; project-hercules-mix.el ends here
