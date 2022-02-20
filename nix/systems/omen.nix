inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    inputs.home-manager.nixosModules.home-manager
    ({ pkgs, config, secret, ... }: {
       home-manager.users."main" =
        { ... }: {
          imports = [ ../home-manager/modules/default.nix ];

          magic_rb = {
            optimisation.march = "skylake";
            pins = inputs;
            config = {
              allowUnfree = true;
            };
            overlays = inputs.self.overlays;

            programs = {
              alacritty.enable = true;
              bash = {
                enable = true;
                enableDirenv = true;
              };
              ssh.enable = true;
              emacs.enable = true;
              xmonad.enable = true;
              gpg.enable = true;
              multimc.enable = false;
            };
            packageCollections = {
              "3dPrinting".enable = false;
              cmdline.enable = true;
              graphical.enable = true;
              webdev.enable = false;
              wine.enable = false;
            };
          };

          services.syncthing.enable = true;
              
          home.stateVersion = "20.09";
        };

      magic_rb = {
        optimisation.march = "skylake";

        grub = {
          enable = true;
          efi.enable = true;
        };

        xserver = {
          enable = true;
          gpu = "nvidia";
          xmonad = true;

          nvidia = {
            prime = true;

            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };

          qwertyNeo2 = true;
          mimickInTty = true;
        };

        pins = inputs;
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;

        hardware.omen = true;
        flakes.enable = true;
        pulseaudio.enable = true;
        sshdEmacs.enable = true;
        networking = {
          bluetooth = true;
          networkManager = true;
        };
      };

      programs.steam.enable = true;

      services.openssh = {
        enable = true;
      };

      networking = {
        hostName = "omen";
        useDHCP = false;
        interfaces.eno1.useDHCP = true;
        hostId = "10c7ffc5";

        wireguard.interfaces."wg0" = {

        } // config.magic_rb.secret.wireguard."omen";
      };

      security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];

      virtualisation.docker.enable = true;

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";
    })
  ];
}
