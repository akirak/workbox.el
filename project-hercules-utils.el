;;; project-hercules-utils.el ---  -*- lexical-binding: t -*-

(defun project-hercules--remove-plist (plist prop)
  (let (result
        key
        (src (copy-sequence plist)))
    (while (setq key (pop src))
      (if (eq prop key)
          (pop src)
        (push key result)
        (push (pop src) result)))
    (nreverse result)))

(provide 'project-hercules-utils)
;;; project-hercules-utils.el ends here
