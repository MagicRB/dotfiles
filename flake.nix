# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  inputs = {
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    nixpkgs-stable.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";

    nixinate.url = "github:MagicRB/nixinate";
    nixinate.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager?ref=master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "flake-utils";
    home-manager.inputs.flake-compat.follows = "flake-compat";

    nixng.url = "github:nix-community/NixNG";
    nixng.inputs.nixpkgs.follows = "nixpkgs";

    poetry2nix.url = "github:nix-community/poetry2nix";
    poetry2nix.inputs.nixpkgs.follows = "nixpkgs";
    poetry2nix.inputs.flake-utils.follows = "flake-utils";

    nix.url = "github:NixOS/nix";
    nix.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.inputs.utils.follows = "flake-utils";
    deploy-rs.inputs.flake-compat.follows = "flake-compat";

    nomad-driver-containerd-nix.url = "github:MagicRB/nomad-driver-containerd-nix"; # "git+https://gitea.redalder.org/Magic_RB/nomad-driver-containerd-nix";
    nomad-driver-containerd-nix.inputs.nixpkgs.follows = "nixpkgs";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";

    flake-parts.url = "github:MagicRB/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    webcord-flake.url = "github:fufexan/webcord-flake";
    webcord-flake.inputs.nixpkgs.follows = "nixpkgs";
    webcord-flake.inputs.dream2nix.follows = "dream2nix";
    webcord-flake.inputs.webcord.follows = "webcord";

    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpks.follows = "nixpkgs";
    dream2nix.inputs.alejandra.follows = "alejandra";
    dream2nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    dream2nix.inputs.flake-utils-pre-commit.follows = "flake-utils";

    alejandra.url = "github:kamadorueda/alejandra";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
    alejandra.inputs.fenix.follows = "fenix";
    alejandra.inputs.flakeCompat.follows = "flake-compat";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";

    nil.url = "github:oxalica/nil";
    nil.inputs.nixpkgs.follows = "nixpkgs";
    nil.inputs.flake-utils.follows = "flake-utils";

    webcord.url = "github:SpacingBat3/WebCord";
    webcord.flake = false;

    fenix.url = "github:nix-community/fenix";
    fenix.flake = false;

    devshell.url = "github:numtide/devshell";
    devshell.flake = false;

    gomod2nix.url = "github:tweag/gomod2nix";
    gomod2nix.flake = false;

    mach-nix.url = "github:DavHau/mach-nix";
    mach-nix.flake = false;

    crane.url = "github:ipetkov/crane";
    crane.flake = false;

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    emacs.url = "github:emacs-mirror/emacs";
    emacs.flake = false;

    vtermModule.url = "github:akermu/emacs-libvterm";
    vtermModule.flake = false;

    secret.url = "git+ssh://git@github.com/MagicRB/dotfiles-secret";
    secret.flake = false;

    qmk.url = "https://github.com/qmk/qmk_firmware";
    qmk.flake = false;
    qmk.type = "git";
    qmk.ref = "master";
    qmk.submodules = true;

    bootloadHID.url = "github:whiteneon/bootloadHID";
    bootloadHID.flake = false;

    hidapitester.url = "github:todbot/hidapitester";
    hidapitester.flake = false;

    yusdacra-dotfiles.url = "github:yusdacra/nixos-config";
    yusdacra-dotfiles.flake = false;

    fufexan-dotfiles.url = "github:fufexan/dotfiles";
    fufexan-dotfiles.flake = false;
  };

  outputs = {
    self,
    flake-parts,
    nixpkgs,
    nixinate,
    ...
  }:
    let
      systems = ["x86_64-linux" "aarch64-linux"];
      flake =
        flake-parts.lib.mkFlake
        {
          inherit self;
          specialArgs = {
            roots.nixos = ./. + "/nixos";
            roots.flake = ./.;
            roots.home-manager = ./. + "/home-manager";
          };
        }
        {
          inherit systems;

          imports = [
            ./modules
          ];
        };
    in
      flake // {
        apps = nixpkgs.lib.genAttrs systems (system:
          (nixinate.nixinate.${system} self)
          // flake.apps.${system}
        );
      };
  #   nixosConfigurations.omen = nixosSystem (import ./systems/omen.nix inputs);
  #   omen = self.nixosConfigurations.omen.config.system.build.toplevel;

  #   nixosConfigurations.heater = nixosSystem (import ./systems/heater.nix inputs);
  #   heater = self.nixosConfigurations.heater.config.system.build.toplevel;

  #   nixosConfigurations.tweedledee = nixosSystem (import ./systems/tweedledee.nix inputs);
  #   tweedledee = self.nixosConfigurations.tweedledee.config.system.build.toplevel;

  #   nixosConfigurations.tweedledum = nixosSystem (import ./systems/tweedledum.nix inputs);
  #   tweedledum = self.nixosConfigurations.tweedledum.config.system.build.toplevel;

  #   nixosConfigurations.toothpick = nixosSystem (import ./systems/toothpick.nix inputs);
  #   toothpick = self.nixosConfigurations.toothpick.config.system.build.toplevel;

  #   nixosConfigurations.mark = nixosSystem (import ./systems/mark.nix inputs);
  #   mark = self.nixosConfigurations.mark.config.system.build.toplevel;

  #   nixosConfigurations.recoveryUsb = nixosSystem (import ./systems/recovery-usb.nix inputs);
  #   recoveryUsb = self.nixosConfigurations.recoveryUsb.config.system.build.toplevel;

  #   homeConfigurations.edge = homeManagerConfiguration (import ./systems/edge.nix inputs);
  #   edge = self.homeConfigurations.edge.activationPackage;

  #   homeConfigurations.maturita = homeManagerConfiguration (import ./systems/maturita.nix inputs);
  #   maturita = self.homeConfigurations.maturita.activationPackage;

  #   nixosConfigurations.blowhole = nixosSystem (import ./systems/blowhole.nix inputs);
  #   blowhole = self.nixosConfigurations.blowhole.config.system.build.toplevel;

  #   apps = nixinate.nixinate.x86_64-linux self;

  #   allSystems =
  #     let
  #       linkFarm = system: attrs:
  #         let
  #           pkgs = pkgsForSystem system;
  #         in
  #           pkgs.linkFarm "allSystems-${system}"
  #             (mapAttrsToList (n: v: { name = n; path = v; }) attrs);
  #       nixos = name: self.nixosConfigurations.${name}.config.system.build.toplevel;
  #       hm = name: self.homeConfigurations.${name}.activationPackage;
  #     in
  #       {
  #         x86_64-linux = linkFarm "x86_64-linux"
  #           {
  #             omen = nixos "omen";
  #             heater = nixos "heater";
  #             tweedledee = nixos "tweedledee";
  #             tweedledum = nixos "tweedledum";
  #             toothpick = nixos "toothpick";
  #             mark = nixos "mark";
  #             recoveryUsb = nixos "recoveryUsb";
  #             blowhole = nixos "blowhole";
  #           };

  #       aarch64-linux = linkFarm "aarch64-linux"
  #         {
  #           edge = hm "edge";
  #         };
  #     };

  #   overlays = {
  #     bootloadHID = import ./overlays/bootloadHID inputs;
  #     bitlbee = import ./overlays/bitlbee;
  #     hidapitester = import ./overlays/hidapitester inputs;
  #     emacs = import ./overlays/emacs-ng/default.nix inputs;
  #     emacsclient-remote = import ./overlays/emacsclient-remote;
  #     gpg-key = import ./overlays/gpg-key;
  #     screenshot = import ./overlays/screenshot;
  #     easy-hls-nix = import ./overlays/easy-hls-nix inputs.easy-hls-nix;
  #     uboot-clara-hd = import ./overlays/uboot-clara-hd;
  #     mainsail = import ./overlays/mainsail;
  #     discord-canary = import ./overlays/discord-canary;
  #     winetricks = import ./overlays/winetricks;
  #     dwarffs = inputs.dwarffs.overlay;
  #     deploy-rs = deploy-rs.overlay;
  #     # nyxt = import ./overlays/nyxt inputs.nyxt.lib;
  #     nix-gaming = final: prev: foldl (acc: x: acc // (x final prev)) {} (mapAttrsToList (_: v: v) inputs.nix-gaming.overlays);
  #     nixng = inputs.nixng.overlay;
  #     nomad-driver-containerd-nix = inputs.nomad-driver-containerd-nix.overlay;
  #   };

  #   nixosModules = {
  #     vault-agent = import ./nixos-modules/vault-agent.nix;
  #   };

  #   packages =
  #     forAllSystems (system:
  #       let
  #         pkgs = pkgsForSystem system;

  #         mkPkg'' =
  #           pkgs: name: package:
  #           (import pkgs { inherit system;
  #                          overlays =
  #                            mapAttrsToList
  #                              (_: v: v) self.overlays;
  #                        } ).magic_rb."${package}";
  #         mkPkg' = mkPkg'' nixpkgs-unstable;
  #         mkPkg = name: mkPkg'' nixpkgs-unstable name name;
  #       in
  #         {
  #           bootloadHID = mkPkg "bootloadHID";
  #           bitlbee = mkPkg "bitlbee";
  #           hidapitester = mkPkg "hidapitester";
  #           emacs = mkPkg "emacs";
  #           emacsclient-remote = mkPkg "emacsclient-remote";
  #           uboot-clara-hd = mkPkg "uboot-clara-hd";
  #           gpg-key = mkPkg "gpg-key";
  #           gpg-key-hs = mkPkg' "gpg-key" "gpg-key-hs";
  #           screenshot = mkPkg "screenshot";
  #           sss-cli = mkPkg "sss-cli";
  #           shh = mkPkg "shh";
  #           mainsail = mkPkg "mainsail";
  #           winetricks = mkPkg "winetricks";

  #           qmk-firmware = (pkgs.callPackage
  #             (import ./extra/qmk/sp84.nix { inherit (inputs) qmk; }) {}).qmk-firmware;
  #         });

  #   devShells = forAllSystems (system:
  #     let pkgs = pkgsForSystem system;
  #     in
  #       { qmk =
  #           (pkgs.callPackage
  #             (import ./extra/qmk/sp84.nix { inherit (inputs) qmk; }) {}).shell;
  #       }
  #   );

  #   deploy.nodes = {
  #     blowhole = {
  #       hostname = "blowhole.in.redalder.org";
  #       sshOpts = [ "-t" ];
  #       profiles.system = {
  #         user = "root";
  #         path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.blowhole;
  #       };
  #     };

  #     toothpick = {
  #       hostname = "redalder.org";
  #       sshOpts = [ "-t" ];
  #       profiles.system = {
  #         user = "root";
  #         path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.toothpick;
  #       };
  #     };

  #     tweedledum = {
  #       hostname = "tweedledum.redalder.org";
  #       sshOpts = [ "-t" ];
  #       profiles.system = {
  #         user = "root";
  #         path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.tweedledum;
  #       };
  #     };
  #   };
  # };
}
