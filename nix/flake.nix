{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";

    home-manager = {
      url = "github:nix-community/home-manager?ref=master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixng = {
      url = "github:MagicRB/NixNG";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    yusdacra-dotfiles = {
      url = "github:yusdacra/nixos-config";
      flake = false;
    };

    # ====================== QMK ======================
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    qmk = {
      url = "https://github.com/qmk/qmk_firmware?ref=0.16.5";
      type = "git";
      flake = false;
    };

    bootloadHID = {
      url = "github:whiteneon/bootloadHID";
      flake = false;
    };

    hidapitester = {
      url = "github:todbot/hidapitester";
      flake = false;
    };
    # ====================== --- ======================

    dwarffs = {
      url = "github:edolstra/dwarffs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nix.follows = "nix";
    };

    nix = {
      url = "github:NixOS/nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };


    secret = {
      url = "path:/home/main/dotfiles/secret";
      flake = false;
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nomad-driver-containerd-nix = {
      url = "git+https://gitea.redalder.org/Magic_RB/nomad-driver-containerd-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    #  PACKAGES

    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    ## Emacs
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    emacs = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    vtermModule = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
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
      pkgsForSystem =
        system:
        import nixpkgs
          { system = "x86_64-linux";
            overlays =
              [ inputs.poetry2nix.overlay
              ];
          };
    in with nixpkgs.lib; {
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

      homeConfigurations.maturita = homeManagerConfiguration (import ./systems/maturita.nix inputs);
      maturita = self.homeConfigurations.maturita.activationPackage;

      nixosConfigurations.blowhole = nixosSystem (import ./systems/blowhole.nix inputs);
      blowhole = self.nixosConfigurations.blowhole.config.system.build.toplevel;

      allSystems =
        let
          linkFarm = system: attrs:
            let
              pkgs = pkgsForSystem system;
            in
              pkgs.linkFarm "allSystems-${system}"
                (mapAttrsToList (n: v: { name = n; path = v; }) attrs);
          nixos = name: self.nixosConfigurations.${name}.config.system.build.toplevel;
          hm = name: self.homeConfigurations.${name}.activationPackage;
        in
          {
            x86_64-linux = linkFarm "x86_64-linux"
              {
                omen = nixos "omen";
                heater = nixos "heater";
                tweedledee = nixos "tweedledee";
                tweedledum = nixos "tweedledum";
                toothpick = nixos "toothpick";
                mark = nixos "mark";
                recoveryUsb = nixos "recoveryUsb";
                blowhole = nixos "blowhole";
              };

          aarch64-linux = linkFarm "aarch64-linux"
            {
              edge = hm "edge";
            };
        };

      overlays = {
        bootloadHID = import ./overlays/bootloadHID inputs;
        bitlbee = import ./overlays/bitlbee;
        hidapitester = import ./overlays/hidapitester inputs;
        emacs = import ./overlays/emacs-ng/default.nix inputs;
        emacsclient-remote = import ./overlays/emacsclient-remote;
        gpg-key = import ./overlays/gpg-key;
        screenshot = import ./overlays/screenshot;
        easy-hls-nix = import ./overlays/easy-hls-nix inputs.easy-hls-nix;
        uboot-clara-hd = import ./overlays/uboot-clara-hd;
        mainsail = import ./overlays/mainsail;
        discord-canary = import ./overlays/discord-canary;
        winetricks = import ./overlays/winetricks;
        dwarffs = inputs.dwarffs.overlay;
        deploy-rs = deploy-rs.overlay;
        # nyxt = import ./overlays/nyxt inputs.nyxt.lib;
        nix-gaming = final: prev: foldl (acc: x: acc // (x final prev)) {} (mapAttrsToList (_: v: v) inputs.nix-gaming.overlays);
        nixng = inputs.nixng.overlay;
        nomad-driver-containerd-nix = inputs.nomad-driver-containerd-nix.overlay;
      };

      nixosModules = {
        vault-agent = import ./nixos-modules/vault-agent.nix;
      };

      packages =
        forAllSystems (system:
          let
            pkgs = pkgsForSystem system;

            mkPkg'' =
              pkgs: name: package:
              (import pkgs { inherit system;
                             overlays =
                               mapAttrsToList
                                 (_: v: v) self.overlays;
                           } ).magic_rb."${package}";
            mkPkg' = mkPkg'' nixpkgs-unstable;
            mkPkg = name: mkPkg'' nixpkgs-unstable name name;
          in
            {
              bootloadHID = mkPkg "bootloadHID";
              bitlbee = mkPkg "bitlbee";
              hidapitester = mkPkg "hidapitester";
              emacs = mkPkg "emacs";
              emacsclient-remote = mkPkg "emacsclient-remote";
              uboot-clara-hd = mkPkg "uboot-clara-hd";
              gpg-key = mkPkg "gpg-key";
              gpg-key-hs = mkPkg' "gpg-key" "gpg-key-hs";
              screenshot = mkPkg "screenshot";
              sss-cli = mkPkg "sss-cli";
              shh = mkPkg "shh";
              mainsail = mkPkg "mainsail";
              winetricks = mkPkg "winetricks";

              qmk-firmware = (pkgs.callPackage
                (import ./extra/qmk/sp84.nix { inherit (inputs) qmk; }) {}).qmk-firmware;
            });

      devShells = forAllSystems (system:
        let pkgs = pkgsForSystem system;
        in
          { qmk =
              (pkgs.callPackage
                (import ./extra/qmk/sp84.nix { inherit (inputs) qmk; }) {}).shell;
          }
      );

      deploy.nodes = {
        blowhole = {
          hostname = "blowhole.in.redalder.org";
          sshOpts = [ "-t" ];
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.blowhole;
          };
        };

        toothpick = {
          hostname = "redalder.org";
          sshOpts = [ "-t" ];
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.toothpick;
          };
        };

        tweedledum = {
          hostname = "tweedledum.redalder.org";
          sshOpts = [ "-t" ];
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.tweedledum;
          };
        };
      };
    };
}
