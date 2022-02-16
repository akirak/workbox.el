;;; project-hercules-config.el --- An example configuration -*- lexical-binding: t -*-

(require 'project-hercules)

(defvar project-hercules-nix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'nix-flake-dispatch)
    map))

(setq project-hercules-composed-maps
      '(("flake.nix"
         project-hercules-nix-map)))

(provide 'project-hercules-config)
;;; project-hercules-config.el ends here
