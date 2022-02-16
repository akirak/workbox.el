;;; project-hercules-config.el --- An example configuration -*- lexical-binding: t -*-

(require 'project-hercules)

;; TODO: Add declare-function forms

;;;; Add entries to the parent map

(let ((parent project-hercules-parent-map))
  (define-key parent "c" #'project-compile)
  (define-key parent "l" #'magit-log)
  (define-key parent "s" #'magit-status))

(dolist (fn '(project-compile
              magit-log
              magit-status))
  (add-to-list 'project-hercules-hide-funs fn))

;;;; Composition examples

(defvar project-hercules-nix-flake-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'nix-flake-dispatch)
    map))

(setq project-hercules-composed-maps
      '(("flake.nix"
         project-hercules-nix-flake-map)))

(provide 'project-hercules-config)
;;; project-hercules-config.el ends here
