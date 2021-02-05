{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, atom, nixpkgs-mozilla }: 
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      cargoLock = ./Cargo.lock;
      rustChannelsOverlay = import "${nixpkgs-mozilla}/rust-overlay.nix";
    in
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                rustChannelsOverlay
                (self: super: let
                  nightlyChannel = (super.rustChannelOf { sha256 = "y5iX4AJfCWccwgbeYVZbEYs2B8w9UplvivKlNEv+wRk="; channel = "nightly"; });
                in {
                  rustc = nightlyChannel.rust;
                  inherit (nightlyChannel) cargo rust rust-std;
                })
              ];
            };
            atomNix = derivation {
              name = "atom-shell-locked";
              builder = pkgs.writeShellScript "builder.sh" ''
                ${pkgs.coreutils}/bin/mkdir $out
                ${pkgs.coreutils}/bin/cp -r ${atom}/* $out 
                ${pkgs.coreutils}/bin/cp ${./Cargo.nix} $out/Cargo.nix
              '';
              inherit system;
            };
          in
            with final; {
              atom-shell = (import "${atomNix}/Cargo.nix" { inherit nixpkgs pkgs; }).rootCrate.build;
            };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).atom-shell);
      };
}
