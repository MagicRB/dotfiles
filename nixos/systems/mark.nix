inputs: {
  system = "x86_64-linux";
  hostname = "mark";
  check = false;

  hm."main" = import ../hm-profiles/mark.nix;

  modules = [
    ../hardware/mark.nix
    ../users/main.nix
  ] ++ [
    ({ custom, nixpkgs, ... }: _: {
      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      environment.systemPackages = [
        nixpkgs.gnupg
        custom.sss-cli
      ];
    })
  ];

  compatModules = [];
}
