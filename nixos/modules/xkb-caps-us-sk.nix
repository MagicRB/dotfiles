{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
let
  mkIf = nixpkgs.lib.mkIf;
  xserver-enable = config.services.xserver.enable;
in {
  services.xserver = {
    layout = mkIf xserver-enable "us,sk";
    xkbVariant = mkIf xserver-enable ",qwerty";
    xkbOptions = mkIf xserver-enable "grp:lalt_lshift_toggle ctrl:nocaps";
  };
}
