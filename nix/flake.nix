{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-20.09";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";
    
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-20.09";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixng = {
      url = "git+https://gitea.redalder.org/Magic_RB/NixNG";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    
    #  PACKAGES
    ## sss-cli
    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };

    ## Emacs
    emacs-overlay = {
      url = "git+https://github.com/nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    emacs = {
      url = "github:flatwhatson/emacs?ref=pgtk-nativecomp";
      flake = false;
    };
    vtermModule = {
      url = "git+https://github.com/akermu/emacs-libvterm";
      flake = false;
    };
    easy-hls-nix = {
      url = "github:jkachmar/easy-hls-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      inherit (nixpkgs-unstable.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;

      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      nixosConfigurations.omen = nixosSystem (import ./systems/omen.nix inputs);
      omen = self.nixosConfigurations.omen.config.system.build.toplevel;

      nixosConfigurations.heater = nixosSystem (import ./systems/heater.nix inputs);
      heater = self.nixosConfigurations.heater.config.system.build.toplevel;

      nixosConfigurations.tweedledee = nixosSystem (import ./systems/tweedledee.nix inputs);
      tweedledee = self.nixosConfigurations.tweedledee.config.system.build.toplevel;

      nixosConfigurations.tweedledum = nixosSystem (import ./systems/tweedledum.nix inputs);
      tweedledum = self.nixosConfigurations.tweedledum.config.system.build.toplevel;

      nixosConfigurations.mark = nixosSystem (import ./systems/mark.nix inputs);
      mark = self.nixosConfigurations.mark.config.system.build.toplevel;

      nixosConfigurations.recoveryUsb = nixosSystem (import ./systems/recovery-usb.nix inputs);
      recoveryUsb = self.nixosConfigurations.recoveryUsb.config.system.build.toplevel;

      homeConfigurations.edge = homeManagerConfiguration (import ./systems/edge.nix inputs);
      edge = self.homeConfigurations.edge.activationPackage;

      homeConfigurations.blowhole = homeManagerConfiguration (import ./systems/blowhole.nix inputs);
      blowhole = self.homeConfigurations.blowhole.activationPackage;


      overlays = {
        emacs = import ./overlays/emacs/default.nix inputs;
        emacsclient-remote = import ./overlays/emacsclient-remote;
        gpg-key = import ./overlays/gpg-key inputs.nixng.lib;
        screenshot = import ./overlays/screenshot inputs.nixng.lib;
        sss-cli = import ./overlays/sss-cli inputs.sss-cli;
        shh = import ./overlays/shh;
      };

      nixosModules = {
        vault-agent = import ./nixos-modules/vault-agent.nix;
      };

      packages =
        forAllSystems (system:
          let
            mkPkg' =
              nixpkgs: name: package: (import nixpkgs { inherit system; overlays = [ self.overlays."${name}" ]; }).magic_rb."${package}";
            mkPkg = name: mkPkg' nixpkgs name name;
          in
            {
              emacs = mkPkg "emacs";
              emacsclient-remote = mkPkg "emacsclient-remote";
              gpg-key = mkPkg "gpg-key";
              gpg-key-hs = mkPkg' nixpkgs-unstable "gpg-key" "gpg-key-hs";
              screenshot = mkPkg "screenshot";
              sss-cli = mkPkg "sss-cli";
              shh = mkPkg "shh";
            });
    };
}
