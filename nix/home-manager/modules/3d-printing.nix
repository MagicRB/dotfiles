{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections."3dPrinting";
  inherit (config.magic_rb.pkgs) nixpkgs-unstable;
in
{
  options.magic_rb.packageCollections."3dPrinting" = {
    enable = mkEnableOption
      ''
        Enable 3D printing package collection, contains Prusa Slicer,
        Cura, OpenSCAD, and inkscape."
      '';
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (final: prev:
        { python3Packages = prev.python3Packages.override (attrs:
            { overrides = final: prev:
              { pivy = prev.pivy.overrideAttrs (attrs:
                { postPatch = ''
                    substituteInPlace distutils_cmake/CMakeLists.txt --replace \$'{SoQt_INCLUDE_DIRS}' \
                    \$'{Coin_INCLUDE_DIR}'\;\$'{SoQt_INCLUDE_DIRS}'
                  '';
                });
              };
            });
        }
      )
    ];

    home.packages = with pkgs; [
      openscad
      cura
      inkscape
      nixpkgs-unstable.prusa-slicer
      freecad
    ];
  };
}
