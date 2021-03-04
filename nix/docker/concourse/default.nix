{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
let
  bashLib = ../main.bash;
  init = nixpkgs.writeShellScriptBin "init" (builtins.readFile ./init);
  conf = nixpkgs.writeText "conf" ''
    _prog_busybox="${nixpkgs.busybox}"
    _prog_bashlib="${bashLib}"
    _prog_bash="${nixpkgs.bash}"
    _prog_concourse="${custom.concourse}"

    _conf_user_uid="5000"
    _conf_user_gid="5000"
    _conf_data="/data/concourse"
    _conf_cacert="${nixpkgs.cacert}"
  '';
in
nixpkgs.dockerTools.buildLayeredImage {
  name = "concourse";
  tag = "latest";

  config = {
    Entrypoint = [ "${init}/bin/init" "${conf}" ];
  };
}
