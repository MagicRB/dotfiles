{ config, lib, ... }:
with lib;
let
  nm-enable = config.networking.networkmanager.enable;
  docker-enable = config.virtualisation.docker.enable;
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
                     ++ (optional nm-enable "network-manager")
                    ++ (optional docker-enable "docker");

      openssh.authorizedKeys.keys =
        [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFVkFvalffJ/SMjJGG3WPiqCqFygnWzhGUaeALBIoCsJ (none)" ];
    };
    
    groups.main = {
      gid = 1000;
    };
  };
}
