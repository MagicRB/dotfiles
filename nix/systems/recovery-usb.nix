inputs: {
  system = "x86_64-linux";
  hostname = "mark";
  check = false;

  hm."main" = import ../home-manager/profiles/mark.nix;

  modules = [
    (_: _: {
      supportedFilesystems = [ "zfs" ];
    })
  ];

  compatModules = [
    (import "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
  ];
}
