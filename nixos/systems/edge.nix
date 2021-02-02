prelude:
let
  system = "aarch64-linux";
  activated = (prelude system "edge");
  inherit (activated) rlib rpkgs;
in
with activated; homeManagerConfiguration {
  configuration = { pkgs, ... }: {
    home.packages = [ # rpkgs.nixpkgs.file rpkgs.nixpkgs.emacs
                    ];
    home.stateVersion = "20.09";

    imports = rlib.callModules rpkgs [
      ../hm-modules/emacs
    ];
  };
  username = "u0_a269";
  inherit system;
  homeDirectory = "/data/data/com.termux/files/home";

  pkgs = rpkgs.nixpkgs;
}
