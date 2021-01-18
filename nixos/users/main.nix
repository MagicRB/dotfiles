inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, pkgs, ... }:
let
  nm-enable = config.networking.networkmanager.enable;
  docker-enable = config.virtualisation.docker.enable;
  mkIf = pkgs.lib.mkIf;
in {
  users = {
    mutableUsers = false;
    
    users.main = {
      isNormalUser = true;
      home = "/home/main";
      hashedPassword = "$6$F6w9J7PuM$O/0rEIRIjc9fWvZ2DPxttFU6vj0hKsyry0xO6CnVv6/2sbryCerZZDP2qlXHthSRPtgTfpnN0eaZa.RQDid5T.";
      description = "main";

      uid = 1000;

      extraGroups = [ "wheel" "audio" ]
                    ++ (if nm-enable then [ "networkmanager" ] else [])
                    ++ (if docker-enable then [ "docker" ] else []);
    };
    
    groups.main = {
      gid = 1000;
    };
  };
}
