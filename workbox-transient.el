;;; workbox-transient.el --- Transient interface -*- lexical-binding: t -*-

;;;###autoload (autoload 'workbox-transient "workbox-transient" nil 'interactive)
(transient-define-prefix workbox-transient ()
  "A convenient transient command for your projects."
  [:description
   workbox-transient-describe-package
   :if (lambda () workbox-package-config)
   ("p" "Run a package command" workbox-run-some-package-manager)]
  ["Compile and other project commands"
   ("r" workbox-transient-recompile)
   ("c" "Compile at the project root" project-compile)
   ("m" "Readme" workbox-readme)]
  (interactive)
  (workbox-locate-package)
  (transient-setup 'workbox-transient))

(defun workbox-transient-describe-package ()
  (when workbox-package-config
    (format "Package: %s in %s"
            (car workbox-package-config)
            (abbreviate-file-name workbox-default-directory))))

(transient-define-suffix workbox-transient-recompile ()
  :description (lambda () (format "Recompile (%s in %s)"
                                  compile-command
                                  (abbreviate-file-name compilation-directory)))
  :if (lambda () compile-command)
  (interactive)
  (recompile))

(provide 'workbox-transient)
;;; workbox-transient.el ends here
