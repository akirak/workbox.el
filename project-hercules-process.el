;;; project-hercules-process.el ---  -*- lexical-binding: t -*-

(defcustom project-hercules-process-use-direnv t
  "Whether to use the direnv integration."
  :group 'project-hercules
  :type 'boolean)

(defcustom project-hercules-history-file
  (locate-user-emacs-file "project-hercules/history.el")
  "File for storing the command history."
  :group 'project-hercules
  :type 'file)

(defvar project-hercules-history-variable nil)

(defun project-hercules--load-history ()
  (unless project-hercules-history-variable
    (let ((tbl (make-hash-table :test #'equal)))
      (when (file-exists-p project-hercules-history-file)
        (with-temp-buffer
          (insert-file-contents project-hercules-history-file)
          (dolist (`(,key . ,value) (read (point-min)))
            (puthash key value tbl))))
      (setq project-hercules-history-variable tbl))))

(defun project-hercules--save-history ()
  (when project-hercules-history-variable
    (with-temp-buffer
      (prin1 (map-into project-hercules-history-variable 'alist))
      (let ((dir (file-name-directory project-hercules-history-file)))
        (unless (file-directory-p dir)
          (make-directory dir))))))

(defun project-hercules--get-history (symbol &optional dir)
  (when var
    (cdr (assq symbol var)))
  (if var
      (let ((cell (cons symbol nil)))
        (push cell var)
        (cdr cell))
    (let ((cell (cons symbol nil)))
      (puthash dir (list cell) project-hercules-history-variable)
      (cdr cell))))

(defun project-hercules--complete (symbol completion)
  (project-hercules--load-history)
  (let* ((dir (abbreviate-file-name default-directory))
         (var (gethash dir project-hercules-history-variable))
         (history (when var
                    (cdr (assq symbol var))))
         (result (completing-read (format "%s (%s)"
                                          symbol
                                          (abbreviate-file-name default-directory))
                                  completion
                                  nil nil nil
                                  history)))
    (if var
        (progn
          (setf var (cons (cons symbol (cons result
                                             (cl-remove result history :test #'equal)))
                          (cl-remove symbol var :key #'car)))
          (puthash dir var project-hercules-history-variable))
      (puthash dir (list (cons symbol (list result)))
               project-hercules-history-variable))
    result))

(defun project-hercules-process--direnv-allowed ()
  (with-temp-buffer
    (call-process "direnv" nil (list t nil) nil
                  "status")
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward "^Found RC allowed true" nil t)
        t))))

(defun project-hercules-process-insert-stdout (command &rest args)
  "Insert the standard output from a command into the buffer."
  (let ((err-file (make-temp-file command)))
    (unwind-protect
        (unless (zerop (if (and project-hercules-process-use-direnv
                                (executable-find "direnv")
                                (or (file-exists-p ".envrc")
                                    (file-exists-p ".env")))
                           (if (project-hercules-process--direnv-allowed)
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

(provide 'project-hercules-process)
;;; project-hercules-process.el ends here
