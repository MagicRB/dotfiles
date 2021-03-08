{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-20.09";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";
    
    ra-systems = {
      url = "git+https://gitea.redalder.org/RedAlder/systems";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager?ref=release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    #  PACKAGES
    ## Rust Things
    rust-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:oxalica/rust-overlay";
    };

    ## sss-cli
    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };

    ## yarn2nix
    yarn2nix = {
      flake = false;
      url = "github:Profpatsch/yarn2nix";
    };

    concourse = {
      flake = false;
      url = "github:concourse/concourse";
    };

    ## Emacs
    emacs-overlay = {
      url = "git+https://github.com/nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      # type = "git";
      #url = "https://git.savannah.gnu.org/git/emacs.git";
      url = "github:flatwhatson/emacs?ref=pgtk-nativecomp";
      #ref = "feature/native-comp";
      flake = false;
    };
    vtermModule = {
      url = "git+https://github.com/akermu/emacs-libvterm";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    with inputs; let
      rlib = import ./rlib.nix {
        inherit nixpkgs home-manager inputs;
        pkgs = {
          inherit nixpkgs nixpkgs-unstable nixpkgs-master;
        };
        custom = with rlib; {
          sss-cli = ./packages/sss-cli;
          atom-shell = ./packages/atom-shell;
          emacs = ./packages/emacs;
          emacsclient-remote = ./packages/emacsclient-remote;
          enter-env = ./packages/enter-env;
          screenshot = ./packages/screenshot;
          multimc-devel = ./packages/multimc-devel;
          concourse = ./packages/concourse-ci;
          gpg-key = ./packages/gpg-key;
          yarn2nix = ./packages/yarn2nix;
          rust =
            # system:
            let
              rustyPkgs = import inputs.nixpkgs {
                overlays = [ inputs.rust-overlay.overlay ];
                system = "x86_64-linux";
                # inherit system;
              };
            in
              rustyPkgs.rust-bin;
        };
        self = rlib;
        supportedSystems = [ "x86_64-linux" "i386-linux" "aarch64-linux" ];
      };

      rmodules = {
        klippy = ra-systems.flakes.klippy.nixosModules.klippy;
        moonraker = ra-systems.flakes.moonraker.nixosModules.moonraker;
        mainsail = ra-systems.flakes.mainsail.nixosModules.mainsail;
        home-manager = inputs.home-manager.nixosModules.home-manager;
      };

      omen = rlib.nixosSystem (import ./systems/omen.nix inputs);
      heater = rlib.nixosSystem (import ./systems/heater.nix inputs);
      mark = rlib.nixosSystem (import ./systems/mark.nix inputs);

      edge = rlib.homeManagerConfiguration (import ./systems/edge.nix);
      blowhole = rlib.homeManagerConfiguration (import ./systems/blowhole.nix);

      recoveryUsb = rlib.nixosSystem (import ./systems/recovery-usb.nix inputs);

      dockerImages = rlib.dockerImages {
        concourse = ./docker/concourse;
        gitea = ./docker/gitea;
        postgresql = ./docker/postgresql;
        concourse-vault-sidecar = ./docker/concourse-vault-sidecar;
        nix = (import ./docker/nix inputs);
      };
    in {
      nixosConfigurations.omen = omen;
      omen = omen.config.system.build.toplevel;

      nixosConfigurations.heater = heater;
      heater = heater.config.system.build.toplevel;

      nixosConfigurations.mark = mark;
      mark = mark.config.system.build.toplevel;

      homeConfigurations.edge = edge;
      edge = edge.activationPackage;

      homeConfigurations.blowhole = blowhole;
      blowhole = blowhole.activationPackage;

      recoveryUsb = recoveryUsb.config.system.build.isoImage;

      inherit dockerImages;

      halfFlakes = rlib.custom;
      CI = 
        {
          dockerImages = (rlib.linkFarm "dockerImages" (system: with dockerImages."${system}"; [
            {
              name = "gitea";
              path = gitea;
            }
            {
              name = "concourse";
              path = concourse;
            }
          ]));
          systems = (rlib.linkFarm "systems" (_: [
            {
              name = "omen";
              path = omen.config.system.build.toplevel;
            }
            {
              name = "heater";
              path = heater.config.system.build.toplevel;
            }
            {
              name = "blowhole";
              path = blowhole.activationPackage;
            }
            {
              name = "mark";
              path = mark.config.system.build.toplevel;
            }
          ]))."${system}";
          halfFlakes = (rlib.linkFarm "dockerImages" (system: with dockerImages."${system}"; [
            {
              name = "gitea";
              path = gitea;
            }
            {
              name = "concourse";
              path = concourse;
            }
          ]));

          packages = (rlib.linkFarm "dockerImages" (system: with dockerImages."${system}"; [
            {
            name = "sss-cli";
            path = sss-cli.defaultPackage."${system}";
            }
          ]));
        };
    };
}
