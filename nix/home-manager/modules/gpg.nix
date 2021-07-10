{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.programs.gpg;
  inherit (config.magic_rb.pkgs) nixpkgs; 
in
{
  options.magic_rb.programs.gpg = {
    enable = mkEnableOption
      ''
        Enable gpg and gpg-key.
      '';
    pinentryFlavor = mkOption {
      description = "Which pinentry flavor should be used.";
      type = types.enum [
        "curses"
        "emacs"
        "mac"
        "gtk2"
        "qt"
        "gnome"
      ];
      default = "gtk2";
    };
  };
  
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nixpkgs.magic_rb.gpg-key
      gnupg
      pass
    ];

    home.file.".gpg-agent.conf".text = ''
      enable-ssh-support
      pinentry-program ${nixpkgs.pinentry."${cfg.pinentryFlavor}"}/bin/pinentry
    '';

    home.file.".profile".text = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';

    home.activation.gnupghome = config.lib.dag.entryAfter ["writeBoundary"] ''
      if [[ ! -e ~/.gnupg ]]
      then
        ln -sf /mnt/key/gnupg ~/.gnupg  
      fi

      if [[ ! -e ~/.gnupg/gpg-agent.conf ]] && [[ -d /mnt/key/gnupg ]]
      then
        ln -sf ~/.gpg-agent.conf /mnt/key/gnupg/gpg-agent.conf
      fi
    '';
  };
}
