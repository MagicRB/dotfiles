{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
let
  # pkgs = (import nixpkgs { inherit system; }).pkgsMusl;
  gitea =
    let 
      openssh = nixpkgs.openssh.override {
        withKerberos = false; withFIDO = false;
      };
      git = nixpkgs.git.override {
        inherit openssh;
        perlSupport = false;
        nlsSupport = false;
        withManual = false;
        guiSupport = false;
        pythonSupport = false;
        withpcre2 = false;
        sendEmailSupport = false;
      };
    in
      nixpkgs.gitea.override {
        inherit openssh git;
        pamSupport = false;
      };
  bashLib = ../main.bash;
  init = nixpkgs.writeShellScriptBin "init" (builtins.readFile ./init);
  conf = nixpkgs.writeText "conf" ''
    _prog_busybox="${nixpkgs.busybox}"
    _prog_bashlib="${bashLib}"
    _prog_bash="${nixpkgs.bash}"
    _prog_gitea="${nixpkgs.gitea}"

    _conf_user_uid="5000"
    _conf_user_gid="5000"
    _conf_data="/data/gitea"
    _conf_cacert="${nixpkgs.cacert}"
  '';
in
nixpkgs.dockerTools.buildLayeredImage {
  name = "gitea";
  tag = "latest";

  config = {
    Entrypoint = [ "${init}/bin/init" "${conf}" ];
  };
}
