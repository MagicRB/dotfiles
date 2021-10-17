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
        let
          # gensymb is not here, dont add
          tex = with pkgs;
            texlive.combine
              { inherit (texlive)
                dvisvgm
                scheme-small
                preview
                siunitx
                amsmath
                ulem
                dvipng
                wrapfig
                cancel
                capt-of
                bytefield
                chemfig
                simplekv;
              };
          r = with pkgs;
            rWrapper.override
              { packages = with rPackages; [ ggplot2 ]; };
        in
          (nixpkgs.magic_rb.emacs.override
            { march = config.magic_rb.optimisation.march;
              hunspell.enable = true;
              hunspell.dictionaries = with pkgs.hunspellDicts;
                [ en_US ];
              additionalPackages =
                [ tex
                  r
                ] ++
                (with pkgs;
                  [ krita
                    ripgrep
                  ]);
            }).bundle;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cfg.package
      (makeDesktopItem {
        name = "Org-Protocol";
        exec = "emacsclient %u";
        comment = "Org protocol";
        desktopName = "org-protocol";
        type = "Application";
        mimeType = "x-scheme-handler/org-protocol";
      })

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
