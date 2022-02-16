;;; project-hercules-config.el --- An example configuration -*- lexical-binding: t -*-

(require 'project-hercules)

(declare-function project-compile "project")
(declare-function magit-commit "ext:magit")
(declare-function magit-log "ext:magit")
(declare-function magit-stage-modified "ext:magit")
(declare-function magit-status "ext:magit")
(declare-function nix-flake-dispatch "ext:nix-flake")

;;;; Add entries to the parent map

(let ((parent project-hercules-parent-map))
  (define-key parent "b" #'project-compile)
  (define-key parent "c" #'magit-commit)
  (define-key parent "l" #'magit-log)
  (define-key parent "m" #'magit-stage-modified)
  (define-key parent "s" #'magit-status))

(dolist (symbol '(project-compile
                  magit-log))
  (add-to-list 'project-hercules-hide-funs symbol))

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
