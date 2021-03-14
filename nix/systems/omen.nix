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
    ../nixos/hardware/omen.nix
    (import ../nixos/modules/pin-nixpkgs.nix inputs)
    ../nixos/users/main.nix
    (import ../nixos/profiles/laptop.nix {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    })
    ../nixos/modules/nvidia-5.11-patch.nix
  ] ++ [
    (_: _: {
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
