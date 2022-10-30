;;; workbox-config.el --- An example configuration -*- lexical-binding: t -*-

(require 'workbox)

(declare-function project-compile "project")
(declare-function magit-commit "ext:magit")
(declare-function magit-log "ext:magit")
(declare-function magit-stage-modified "ext:magit")
(declare-function magit-status "ext:magit")
(declare-function magit-push "ext:magit")
(declare-function nix-flake-dispatch "ext:nix-flake")

(declare-function workbox-npm "workbox-npm")
(declare-function workbox-mix "workbox-mix")

;;;; Add entries to the parent map

(let ((parent workbox-parent-map))
  (define-key parent "x" #'project-compile))

(dolist (symbol '(project-compile
                  workbox-npm
                  workbox-mix
                  magit-log))
  (add-to-list 'workbox-hide-funs symbol))

;;;; Composition examples

(defvar workbox-nix-flake-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'nix-flake-dispatch)
    map))

(defvar workbox-npm-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'workbox-npm)
    (define-key map "rn" #'workbox-npm)
    map))

(defvar workbox-mix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'workbox-mix)
    (define-key map "rm" #'workbox-mix)
    map))

(defvar workbox-magit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'magit-commit)
    (define-key map "l" #'magit-log)
    (define-key map "m" #'magit-stage-modified)
    (define-key map "s" #'magit-status)
    (define-key map "P" #'magit-push)
    map))

(setq workbox-composed-maps
      '(("flake.nix"
         workbox-nix-flake-map)
        ("package.json"
         workbox-npm-map)
        (".git"
         workbox-magit-map)
        ("mix.exs"
         workbox-mix-map)))

(provide 'workbox-config)
;;; workbox-config.el ends here
