{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    hercules = {
      flake = false;
      owner = "wurosh";
      repo = "hercules";
      type = "github";
    };
    project-hercules = {
      flake = false;
      owner = "akirak";
      repo = "project-hercules.el";
      type = "github";
    };
    which-key = {
      flake = false;
      owner = "justbur";
      repo = "emacs-which-key";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
