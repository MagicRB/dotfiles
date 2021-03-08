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
  shadow =
    let 
      group = nixpkgs.writeTextFile {
        name = "group";
        destination = "/etc/group";
        text = ''
          root:x:0:
          nixbld:x:30000:nixbld1,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld2,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld3,nixbld30,nixbld31,nixbld32,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9
          nogroup:x:65534:
        '';
      };
      passwd = nixpkgs.writeTextFile {
        name = "passwd";
        destination = "/etc/passwd";
        text = with nixpkgs; ''
          root:x:0:0:System administrator:/root:${bash}/bin/bash
          ${lib.concatStringsSep "\n" (map (num: "nixbld1:x:${toString (30000 + num)}:30000:Nix build user ${(toString num)}:/var/empty:${busybox}/bin/nologin") [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32])}
          nobody:x:65534:65534:Unprivileged account (don't use!):/var/empty:${busybox}/bin/nologin
        '';
      };
      varEmpty = nixpkgs.runCommandNoCCLocal "var-empty" {} ''
        mkdir -p $out/var/empty
        # chown 0:0 $out/var/empty
        chmod 555 $out/var/empty
      '';
    in
      nixpkgs.symlinkJoin { name = "shadow"; paths = [ passwd group varEmpty ]; };
  tmp = nixpkgs.runCommandNoCCLocal "tmp" {} ''
    mkdir -p $out/tmp
  '';
  ca-certificates = nixpkgs.runCommandNoCCLocal "ca-certificates" {} ''
    mkdir -p $out/etc/ssl/certs
    ln -s ${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt $out/etc/ssl/certs/ca-bundle.crt
    ln -s $out/etc/ssl/certs/ca-bundle.crt $out/etc/ssl/certs/ca-certificates.crt
  '';
  entrypoint = nixpkgs.writeShellScriptBin "entrypoint.sh" ''
    ${init}/bin/init ${conf} "$@" 
  '';
  gitMin = nixpkgs.git.override {
    perlSupport = false;
    nlsSupport = false;
    withManual = false;
    guiSupport = false;
    pythonSupport = false;
    withpcre2 = false;
    sendEmailSupport = false;
  };
  config = {
    name = "nix";
    tag = "latest";

    contents = [ entrypoint shadow ca-certificates tmp ];

    config = {
      Entrypoint = [ "/bin/entrypoint.sh" ];
      Env = with nixpkgs; [
        "PATH=${lib.makeBinPath [ busybox nixFlakes bash gitMin ]}"
        "NIX_PATH=nixpkgs=${inputs.nixpkgs}"
        "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      ];
    };
  };
in
{
  build = nixpkgs.dockerTools.buildLayeredImage config;
  stream = nixpkgs.dockerTools.streamLayeredImage config;
}
