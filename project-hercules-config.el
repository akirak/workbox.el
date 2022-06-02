;;; project-hercules-config.el --- An example configuration -*- lexical-binding: t -*-

(require 'project-hercules)

(declare-function project-compile "project")
(declare-function magit-commit "ext:magit")
(declare-function magit-log "ext:magit")
(declare-function magit-stage-modified "ext:magit")
(declare-function magit-status "ext:magit")
(declare-function magit-push "ext:magit")
(declare-function nix-flake-dispatch "ext:nix-flake")

(declare-function project-hercules-npm "project-hercules-npm")
(declare-function project-hercules-mix "project-hercules-mix")

;;;; Add entries to the parent map

(let ((parent project-hercules-parent-map))
  (define-key parent "x" #'project-compile))

(dolist (symbol '(project-compile
                  project-hercules-npm
                  project-hercules-mix
                  magit-log))
  (add-to-list 'project-hercules-hide-funs symbol))

;;;; Composition examples

(defvar project-hercules-nix-flake-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'nix-flake-dispatch)
    map))

(defvar project-hercules-npm-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'project-hercules-npm)
    (define-key map "rn" #'project-hercules-npm)
    map))

(defvar project-hercules-mix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'project-hercules-mix)
    (define-key map "rm" #'project-hercules-mix)
    map))

(defvar project-hercules-magit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'magit-commit)
    (define-key map "l" #'magit-log)
    (define-key map "m" #'magit-stage-modified)
    (define-key map "s" #'magit-status)
    (define-key map "P" #'magit-push)
    map))

(setq project-hercules-composed-maps
      '(("flake.nix"
         project-hercules-nix-flake-map)
        ("package.json"
         project-hercules-npm-map)
        (".git"
         project-hercules-magit-map)
        ("mix.exs"
         project-hercules-mix-map)))

(provide 'project-hercules-config)
;;; project-hercules-config.el ends here
