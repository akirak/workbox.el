;;; workbox-mix.el --- Mix integration -*- lexical-binding: t -*-

(require 'workbox)
(require 'workbox-process)

(defgroup workbox-mix nil
  "Elixir Mix support."
  :type 'workbox)

(defvar workbox-mix-command-cache nil
  "Hash table that stores per-project alists of commands.")

(defvar workbox-mix-command-alist nil)

(defcustom workbox-mix-command-runner #'compile
  "Function that runs mix commands."
  :type 'function)

;;;###autoload
(defun workbox-mix ()
  "Run a Mix command."
  (interactive)
  (workbox-with-package-root "mix.exs"
    (let* ((default-directory workbox-package-root-directory)
           (command (completing-read (format "Mix command (%s): " default-directory)
                                     (workbox-mix-completion))))
      (workbox-mix--add-command (string-trim command))
      (funcall workbox-mix-command-runner command))))

(defun workbox-mix-clear ()
  (interactive)
  (clrhash workbox-mix-command-cache))

(defun workbox-mix-completion ()
  (let* ((command-alist (workbox-mix--commands)))
    (setq workbox-mix-command-alist command-alist)
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . workbox-shell-command)
                         (annotation-function . workbox-mix-annotate)))
         (complete-with-action action ',command-alist string pred)))))

(defun workbox-mix-annotate (command)
  (when-let (description (cdr (assoc command workbox-mix-command-alist)))
    ;; TODO Set a custom face
    (concat " " (propertize description 'face 'font-lock-comment-face))))

(defun workbox-mix--ensure-cache ()
  (unless workbox-mix-command-cache
    (setq workbox-mix-command-cache
          (make-hash-table :test #'equal))))

(defun workbox-mix--commands ()
  "Return an alist of Mix commands for the project."
  (workbox-mix--ensure-cache)
  (or (gethash default-directory workbox-mix-command-cache)
      (let (result)
        (with-temp-buffer
          (workbox-process-insert-stdout "mix" "help")
          (goto-char (point-min))
          (save-match-data
            (while (re-search-forward (rx bol (* space) (group "mix" (* (not (any "#"))))
                                          " # " (group (+ nonl)) eol)
                                      nil t)
              (push (cons (string-trim-right (match-string 1))
                          (string-trim (match-string 2)))
                    result))))
        (setq result (cons '("iex -S mix" . "Run iex within the context of the application")
                           (nreverse (copy-sequence result))))
        (puthash default-directory result workbox-mix-command-cache)
        result)))

(defun workbox-mix--add-command (command)
  "Add a Mix command to the per-project cache."
  (let ((current (workbox-mix--commands)))
    (unless (assoc command current)
      (push (cons command nil) current)
      (puthash default-directory current workbox-mix-command-cache))))

(provide 'workbox-mix)
;;; workbox-mix.el ends here
