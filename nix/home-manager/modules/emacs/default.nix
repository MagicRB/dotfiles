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
      default = nixpkgs.magic_rb.emacs;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cfg.package
      fira-code
      emacs-all-the-icons-fonts
    ];

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
