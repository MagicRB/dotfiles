{
  config,
  pkgs,
  lib,
  options,
  ...
}:
with lib; let
  cfg = config.magic_rb.gaming;
in {
  options.magic_rb.gaming = {
    enable = mkEnableOption "Enable Gaming support.";

    steam.remotePlay = mkOption {
      description = ''
        Open ports required for remote play;
      '';
      type = types.bool;
      default = false;
    };
  };

  config = mkMerge [
    (optionalAttrs (options ? "home-manager")
      {
        home-manager.users."main" = mkIf cfg.enable ({...}: {
          magic_rb = {
            packageCollections.wine.enable = true;
            programs.multimc.enable = true;
          };
        });
      })
    (mkIf cfg.enable {
      programs.steam = {
        enable = cfg.enable;
        remotePlay.openFirewall = cfg.steam.remotePlay;
      };
    })
  ];
}
