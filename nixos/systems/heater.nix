inputs: {
  system = "x86_64-linux";
  hostname = "heater";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../hm-profiles/common.nix;

  modules = [
    ../hardware/heater.nix
    ../profiles/workstation.nix
    (import ../modules/pin-nixpkgs.nix inputs)
    ../users/main.nix
  ] ++ [
    (_: _: {
      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      virtualisation.docker.enable = true;
    })
  ];

  compatModules = [];
}
