;;; workbox-utils.el ---  -*- lexical-binding: t -*-

(defun workbox--remove-plist (plist prop)
  (let (result
        key
        (src (copy-sequence plist)))
    (while (setq key (pop src))
      (if (eq prop key)
          (pop src)
        (push key result)
        (push (pop src) result)))
    (nreverse result)))

(provide 'workbox-utils)
;;; workbox-utils.el ends here
