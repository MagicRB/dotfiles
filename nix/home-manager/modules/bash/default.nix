{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with custom; [
    emacsclient-remote
  ];
    
  home.file = {
    ".bashrc".source = rlib.substitute {
      runCommand = nixpkgs.runCommandNoCC;
      inFile = ./.bashrc;
      name = ".bashrc";
      vars = {
        "exa" = "${nixpkgs.exa}";
        "bat" = "${nixpkgs.bat}";
      };
    };
  };
}
