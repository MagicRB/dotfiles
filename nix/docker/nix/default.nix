inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
let
  bashLib = ../main.bash;
  init = nixpkgs.writeShellScriptBin "init" (builtins.readFile ./init);
  conf = nixpkgs.writeText "conf" ''
    _prog_busybox="${nixpkgs.busybox}"
    _prog_bashlib="${bashLib}"
    _prog_bash="${nixpkgs.bash}"
    _prog_nix="${nixpkgs.nixFlakes}"

    _conf_nixpkgs="${inputs.nixpkgs}"
    _conf_cacert="${nixpkgs.cacert}"
    _conf_user_uid="1000"
    _conf_user_gid="1000"
  '';
  entrypoint = nixpkgs.writeShellScriptBin "entrypoint.sh" ''
    ${init}/bin/init ${conf} "$@" 
  '';
in
nixpkgs.dockerTools.buildLayeredImage {
  name = "nix";
  tag = "latest";

  contents = [ entrypoint ];

  config = {
    Entrypoint = [ "/bin/entrypoint.sh" ];
  };
}
