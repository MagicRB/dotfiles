inputs: {
  system = "x86_64-linux";
  hostname = "mark";
  check = false;

  hm."main" = import ../home-manager/profiles/mark.nix;

  modules = [
    ../nixos/hardware/mark.nix
    ../nixos/users/main.nix
  ] ++ [
    ({ custom, nixpkgs, ... }: _: {
      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      environment.systemPackages = with nixpkgs; [
        gnupg
        pinentry
        openssl
        paperkey
        monkeysphere
      ] ++ [
        custom.sss-cli
      ];
    })
  ];

  compatModules = [];
}
