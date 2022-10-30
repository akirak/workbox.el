;;; workbox-process.el ---  -*- lexical-binding: t -*-

(defcustom workbox-process-use-direnv t
  "Whether to use the direnv integration."
  :group 'workbox
  :type 'boolean)

(defun workbox-process--direnv-allowed ()
  (with-temp-buffer
    (call-process "direnv" nil (list t nil) nil
                  "status")
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward "^Found RC allowed true" nil t)
        t))))

(defun workbox-process-insert-stdout (command &rest args)
  "Insert the standard output from a command into the buffer."
  (let ((err-file (make-temp-file command)))
    (unwind-protect
        (unless (zerop (if (and workbox-process-use-direnv
                                (executable-find "direnv")
                                (or (file-exists-p ".envrc")
                                    (file-exists-p ".env")))
                           (if (workbox-process--direnv-allowed)
                               (apply #'call-process "direnv"
                                      nil (list t err-file) nil
                                      "exec" "."
                                      command args)
                             (user-error "direnv is not allowed here"))
                         (apply #'call-process command
                                nil (list t err-file) nil
                                args)))
          (error "Error from command %s: %s"
                 (mapconcat #'shell-quote-argument (cons command args)
                            " ")
                 (with-temp-buffer
                   (insert-file-contents err-file)
                   (buffer-string))))
      (delete-file err-file))))

(provide 'workbox-process)
;;; workbox-process.el ends here
