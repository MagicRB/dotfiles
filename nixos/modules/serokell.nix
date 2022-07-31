{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.magic_rb.serokell;
in {
  options.magic_rb.serokell = mkEnableOption "Enable Serokell related configuration";

  config = mkMerge [
    (mkIf cfg {
      nix.settings.substituters = [
        "https://cache.nixos.org"
        "s3://serokell-private-nix-cache?endpoint=s3.us-west-000.backblazeb2.com&profile=backblaze-cache-read"
      ];

      nix.settings.trusted-public-keys = [
        "serokell-1:aIojg2Vxgv7MkzPJoftOO/I8HKX622sT+c0fjnZBLj0="
      ];
    })
    (optionalAttrs (options ? "home-manager") {
      home-manager.users."main" = {...}: {
        programs.ssh.matchBlocks = {
          "*.serokell.team" = {
            port = 17788;
            user = "magicrb";
          };
        };
      };
    })
  ];
}
