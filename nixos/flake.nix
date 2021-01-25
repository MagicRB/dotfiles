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
    ## sss-cli
    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };

    ## Emacs
    emacs-overlay = {
      url = "git+https://github.com/nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      type = "git";
      url = "https://git.savannah.gnu.org/git/emacs.git";
      ref = "feature/native-comp";
      flake = false;
    };
    vtermModule = {
      url = "git+https://github.com/akermu/emacs-libvterm";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    with inputs; let
      customModules = [
        ra-systems.flakes.klippy.nixosModules.klippy
        ra-systems.flakes.moonraker.nixosModules.moonraker
        ra-systems.flakes.mainsail.nixosModules.mainsail
        inputs.home-manager.nixosModules.home-manager
      ];
      prelude = system: rec {
        rlib = (import ./lib.nix inputs { lib = nixpkgs.lib; inherit system; });
        rpkgs = (rlib.getLegacyPkgs
          {
            allowUnfree = true;
          }
          {
            inherit (inputs) nixpkgs nixpkgs-unstable nixpkgs-master;
          }) // {
            custom = {
              sss-cli = rlib.halfCallFlakePackage ./packages/sss-cli;
              atom-shell = rlib.halfCallFlakePackage ./packages/atom-shell;
              emacs = rlib.halfCallFlakePackage ./packages/emacs;
              emacsclient-remote = rlib.halfCallFlakePackage ./packages/emacsclient-remote;
              enter-env = rlib.halfCallFlakePackage ./packages/enter-env;
              screenshot = rlib.halfCallFlakePackage ./packages/screenshot;
            };
            inherit rlib;
          };
      };
    in {
      nixosConfigurations.omen = let
        system = "x86_64-linux";
        inherit (prelude system) rlib rpkgs;
      in nixpkgs.lib.nixosSystem {
        inherit system;

        modules = rlib.callModules rpkgs [
          ./hardware/omen.nix
          ./modules/pin-nixpkgs.nix
          ./users/main.nix
        ] ++ [
          ((rlib.callModule rpkgs ./modules/home-manager.nix) ./hm-profiles/common.nix)
          ({ pkgs, ... }: {
            networking = {
              hostName = "omen";
              useDHCP = false;
              interfaces.eno1.useDHCP = true;
            };

            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";
          })

          (import ./profiles/laptop.nix {
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          } inputs rpkgs)
        ] ++ customModules;
      };

      nixosConfigurations.heater = let
        system = "x86_64-linux";
        inherit (prelude system) rlib rpkgs;
      in nixpkgs.lib.nixosSystem {
        inherit system;

        modules = rlib.callModules rpkgs [
          ./hardware/heater.nix
          ./profiles/workstation.nix
          ./modules/pin-nixpkgs.nix
          ./users/main.nix
        ] ++ [
          ((rlib.callModule rpkgs ./modules/home-manager.nix) ./hm-profiles/common.nix)
          ({ pkgs, ... }: {
            networking = {
              hostName = "heater";
              useDHCP = false;
              interfaces.enp3s0.useDHCP = true;
            };

            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";

            virtualisation.docker.enable = true;
          })
        ] ++ customModules;
      };

      nixosConfigurations.mark = let
        system = "x86_64-linux";
        inherit (prelude system) rlib rpkgs;
      in nixpkgs.lib.nixosSystem {
        inherit system;

        modules = rlib.callModules rpkgs [
          ./hardware/mark.nix
          ./users/main.nix
        ] ++ [
          ((rlib.callModule rpkgs ./modules/home-manager.nix) ./hm-profiles/mark.nix)
          ({ pkgs, ... }: {
            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";

            environment.systemPackages = [
              pkgs.gnupg
              rpkgs.sss-cli
            ];
          })
        ];
      };


      homeConfigurations.edge = let
        system = "aarch64-linux";
        inherit (prelude system) rlib rpkgs;
      in inputs.home-manager.lib.homeManagerConfiguration {
        configuration = { pkgs, ... }: {
          home.packages = [ rpkgs.nixpkgs.file rpkgs.nixpkgs.emacs ];
          home.stateVersion = "20.09";
        };
        username = "u0_a269";
        inherit system;
        homeDirectory = "/data/data/com.termux/files/home";
      };
      edge = self.homeConfigurations.edge.activationPackage;
    };
}
