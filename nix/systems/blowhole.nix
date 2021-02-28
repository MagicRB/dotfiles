{
  system = "x86_64-linux";
  username = "main";
  homeDirectory = "/home/main";
  hostname = "blowhole";

  modules = [
    ../home-manager/profiles/headless.nix
    ({ nixpkgs, ... }: _: {
      home.stateVersion = "20.09";
    })
  ];
}
