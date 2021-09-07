{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.programs.emacs;
  inherit (config.magic_rb.pkgs) nixpkgs;
in
{
  options.magic_rb.programs.emacs = {
    enable = mkEnableOption "Enable emacs with my config";
    package = mkOption {
      description = "Which emacs package to use.";
      type = types.package;
      default =
        (nixpkgs.magic_rb.emacs.override
          { march = config.magic_rb.optimisation.march; })
          .bundle;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cfg.package
      fira-code
      (iosevka-bin.override { variant = "aile"; })
      (iosevka-bin.override { variant = "etoile"; })
      (iosevka-bin.override { variant = ""; })
      emacs-all-the-icons-fonts
    ];

    home.activation.emacsStraightVerions = config.lib.dag.entryAfter
      ["writeBoundary"] ''
        mkdir -p ~/.emacs.d/straight/versions
        ln -sfn ~/dotfiles/nix/home-manager/modules/emacs/straight-versions.el ~/.emacs.d/straight/versions/default.el
    '';
    
    home.file = {
      ".emacs".source = ./.emacs;
      ".emacs.d/org" = {
        source = ./.emacs.d/org;
        recursive = true;
      };
      ".emacs.d/lisp" = {
        source = ./.emacs.d/lisp;
        recursive = true;
      };
    };
  };
}
