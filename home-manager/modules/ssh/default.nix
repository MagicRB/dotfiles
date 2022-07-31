{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.programs.ssh;
in {
  options.magic_rb.programs.ssh = {
    enable = mkEnableOption "Enable ssh_config";
  };

  config = mkIf cfg.enable {
    programs.ssh = {
      enable = true;

      controlMaster = "auto";
      controlPath = "~/.ssh/controlmasters/%r@%h:%p";
      controlPersist = "300s";
      serverAliveInterval = 30;
      matchBlocks = {
        "Host *redalder.org 10.64.1.* 10.64.0.*".extraOptions = {
          ExitOnForwardFailure = "yes";
          SendEnv = "INSIDE_EMACS";
          RemoteForward = "/home/main/.ssh/emacs-server /run/user/1000/emacs/server";
        };
      };
    };

    home.activation."ssh-controlmasters" = config.lib.dag.entryAfter ["writeBoundary"] ''
      mkdir -p ~/.ssh/controlmasters
    '';
  };
}
