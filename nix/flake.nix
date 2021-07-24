{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";
    
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-21.05";
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
    
    yusdacra-dotfiles = {
      url = "github:yusdacra/nixos-config";
      flake = false;
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
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

  outputs = { self
            , nixpkgs
            , nixpkgs-unstable
            , home-manager
            , deploy-rs
            , ...
            }@inputs:
    let
      inherit (nixpkgs-unstable.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;

      supportedSystems = [ "x86_64-linux" ]; # add "i686-linux" "aarch64-linux" back after hls is fixed
      forAllSystems' = systems: f: nixpkgs.lib.genAttrs systems (system: f system);
      forAllSystems = forAllSystems' supportedSystems;
    in {
      nixosConfigurations.omen = nixosSystem (import ./systems/omen.nix inputs);
      omen = self.nixosConfigurations.omen.config.system.build.toplevel;

      nixosConfigurations.heater = nixosSystem (import ./systems/heater.nix inputs);
      heater = self.nixosConfigurations.heater.config.system.build.toplevel;

      nixosConfigurations.tweedledee = nixosSystem (import ./systems/tweedledee.nix inputs);
      tweedledee = self.nixosConfigurations.tweedledee.config.system.build.toplevel;

      nixosConfigurations.tweedledum = nixosSystem (import ./systems/tweedledum.nix inputs);
      tweedledum = self.nixosConfigurations.tweedledum.config.system.build.toplevel;

      nixosConfigurations.toothpick = nixosSystem (import ./systems/toothpick.nix inputs);
      toothpick = self.nixosConfigurations.toothpick.config.system.build.toplevel;

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
        discord-canary = import "${inputs.yusdacra-dotfiles}/overlays/discord-canary-system.nix";
        easy-hls-nix = import ./overlays/easy-hls-nix inputs.easy-hls-nix; 
      };

      nixosModules = {
        vault-agent = import ./nixos-modules/vault-agent.nix;
      };

      packages =
        forAllSystems (system:
          let
            mkPkg'' =
              pkgs: name: package:
              (import pkgs { inherit system;
                             overlays =
                               nixpkgs.lib.mapAttrsToList
                                 (_: v: v) self.overlays;
                           } ).magic_rb."${package}";
            mkPkg' = mkPkg'' nixpkgs-unstable;
            mkPkg = name: mkPkg'' nixpkgs-unstable name name;
          in
            {
              emacs = mkPkg "emacs";
              emacsclient-remote = mkPkg "emacsclient-remote";
              gpg-key = mkPkg "gpg-key";
              gpg-key-hs = mkPkg' "gpg-key" "gpg-key-hs";
              screenshot = mkPkg "screenshot";
              sss-cli = mkPkg "sss-cli";
              shh = mkPkg "shh";
              easy-hls-nix = if system == "x86_64-linux" then mkPkg "easy-hls-nix" else (import nixpkgs-unstable { inherit system; }).hello;
            });
    };
}
