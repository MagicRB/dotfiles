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
      halfFlakes = rlib.callHalfFlakes {
          sss-cli = ./packages/sss-cli;
          atom-shell = ./packages/atom-shell;
          emacs = ./packages/emacs;
          emacsclient-remote = ./packages/emacsclient-remote;
          enter-env = ./packages/enter-env;
          screenshot = ./packages/screenshot;
          multimc-devel = ./packages/multimc-devel;
          concourse = ./packages/concourse-ci;
      };
      rlib = import ./rlib.nix {
        inherit nixpkgs home-manager inputs;
        pkgs = {
          inherit nixpkgs nixpkgs-unstable nixpkgs-master;
        };
        custom = with rlib; {
          rust =
            system:
            let
              rustyPkgs = import inputs.nixpkgs {
                overlays = [ inputs.rust-overlay.overlay ];
                inherit system;
              };
            in
              rustyPkgs.rust-bin;
        } // halfFlakes;
        self = rlib;
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
    } // halfFlakes;
}
