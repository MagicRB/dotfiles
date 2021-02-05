{
  system = "x86_64-linux";
  username = "main";
  homeDirectory = "/home/main";
  hostname = "blowhole";

  modules = [
    ../hm-profiles/headless.nix
    ({ nixpkgs, ... }: _: {
      home.stateVersion = "20.09";
    })
  ];
}
