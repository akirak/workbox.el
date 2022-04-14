{
  description = "FIXME: Your package description";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      # If your package is not on MELPA, fork github:melpa/melpa, create a new
      # branch for your package, add a recipe, and push it to GitHub.
      # url = "github:melpa/melpa";
      url = "github:akirak/melpa/project-hercules";
      flake = false;
    };

    nomake = {
      url = "github:emacs-twist/nomake";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
    };
  };

  outputs =
    { self
    , nomake
    , ...
    } @ inputs:
    nomake.lib.mkFlake {
      src = ./.;
      localPackages = [
        "project-hercules"
      ];
      extraPackages = [
        # project 0.8 is required for Emacs 27
        "project"
      ];
    };
}
