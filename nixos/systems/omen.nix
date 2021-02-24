inputs: {
  system = "x86_64-linux";
  hostname = "omen";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../hm-profiles/common.nix;

  modules = [
    ../hardware/omen.nix
    (import ../modules/pin-nixpkgs.nix inputs)
    ../users/main.nix
    (import ../profiles/laptop.nix {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    })
    ../modules/nvidia-5.11-patch.nix
  ] ++ [
    (_: _: {
      networking = {
        hostName = "omen";
        useDHCP = false;
        interfaces.eno1.useDHCP = true;
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";
    })
  ];

  compatModules = [];
}
