inputs: {
  system = "x86_64-linux";
  hostname = "omen";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../home-manager/profiles/common.nix {
    multimc5 = false;
    wine = false;
    _3dPrinting = false;
    js-ts = false;
  };

  modules = [
    ../nixos/hardware/omen.nix # auto
    ../nixos/modules/efi-grub.nix # manual
    ../nixos/modules/pin-nixpkgs.nix # manual
    ../nixos/users/main.nix # auto
    ../nixos/profiles/laptop.nix # auto
    ../nixos/modules/xserver.nix # manual
  ] ++ [
    ({ nixpkgs, ... }: _: {
      magic_rb = {
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

          setSkLayout = true;
          emacsCtrl = true;
        };

        pins = {
          "nixpkgs" = inputs.nixpkgs;
          "nixpkgs-unstable" = inputs.nixpkgs-unstable;
          "nixpkgs-master" = inputs.nixpkgs-master;
        };
      };

      nixpkgs.pkgs = nixpkgs;

      hardware.steam-hardware.enable = true;

      networking = {
        hostName = "omen";
        useDHCP = false;
        interfaces.eno1.useDHCP = true;
        hostId = "10c7ffc5";
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";
    })
  ];

  compatModules = [];
}
