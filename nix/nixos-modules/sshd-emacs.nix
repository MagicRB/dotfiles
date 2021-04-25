{ config, lib, options, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.sshdEmacs;
in
{
  options.magic_rb.sshdEmacs = {
    enable = mkEnableOption "Enable sshd options necessary for emacs socket forwarding.";
  };

  config = {
    services.openssh.extraConfig = ''
      AcceptEnv INSIDE_EMACS
      StreamLocalBindUnlink yes
    '';
  };
}
