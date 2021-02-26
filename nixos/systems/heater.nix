inputs: {
  system = "x86_64-linux";
  hostname = "heater";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../hm-profiles/common.nix {
    multimc5 = true;
    wine = true;
    _3dPrinting = true;
  };

  modules = [
    ../hardware/heater.nix
    ../profiles/workstation.nix
    (import ../modules/pin-nixpkgs.nix inputs)
    ../users/main.nix
    ../modules/nvidia-5.11-patch.nix
  ] ++ [
    (_: _: {
      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;

        firewall.enable = false;
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      virtualisation.docker.enable = true;
    })
  ];

  compatModules = [];
}
