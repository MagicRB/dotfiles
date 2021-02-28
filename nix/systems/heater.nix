inputs: {
  system = "x86_64-linux";
  hostname = "heater";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../home-manager/profiles/common.nix {
    multimc5 = true;
    wine = true;
    _3dPrinting = true;
  };

  modules = [
    ../nixos/hardware/heater.nix
    ../nixos/profiles/workstation.nix
    (import ../nixos/modules/pin-nixpkgs.nix inputs)
    ../nixos/users/main.nix
    ../nixos/modules/nvidia-5.11-patch.nix
  ] ++ [
    (_: _: {
      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;

        firewall.enable = false;
        hostId = "3457b383";
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      virtualisation.docker.enable = true;
      boot = {
        supportedFilesystems = [ "zfs" ];
        zfs.enableUnstable = true;
      };
    })
  ];

  compatModules = [];
}
