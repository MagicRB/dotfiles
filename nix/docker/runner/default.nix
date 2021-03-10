inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
let
  makeRunner = { pkgs, name, additionalPkgs }:
    let
      shadow = rlib.dockerTools.makeShadow {
        pkgs = nixpkgs;
      };
      tmp = nixpkgs.runCommandNoCCLocal "tmp" {} ''
    mkdir -p $out/tmp
  '';
      ca-certificates = rlib.dockerTools.makeCerts {
        pkgs = nixpkgs;
        certs = [ ../../redalder.org.crt ];
      };
      entrypoint = nixpkgs.writeShellScriptBin "entrypoint.sh" (builtins.readFile ./entrypoint.sh);
      gitMin = nixpkgs.git.override {
        perlSupport = false;
        nlsSupport = false;
        withManual = false;
        guiSupport = false;
        pythonSupport = false;
        withpcre2 = false;
        sendEmailSupport = false;
      };
    in
      pkgs.dockerTools.buildLayeredImage {
        inherit name;
        tag = "latest";

        contents = [ entrypoint shadow ca-certificates tmp ];

        config = {
          Entrypoint = [ "${nixpkgs.dumb-init}/bin/dumb-init" "--" "/bin/entrypoint.sh" ];
          Env = with nixpkgs; [
            "PATH=${lib.makeBinPath ([ busybox bash ] ++ additionalPkgs)}"
            "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
          ];
        };
      };
in
{
  vaultNomad = makeRunner
    { pkgs = nixpkgs;
      name = "vault-nomad-runner";
      additionalPkgs = with nixpkgs; [ vault nomad ];
    };
  concourseVault = makeRunner
    { pkgs = nixpkgs;
      name = "concourse-vault-runner";
      additionalPkgs = with nixpkgs; [ vault custom.concourse ];
    };
}
