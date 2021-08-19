{ ... }:
{
  imports = [
    ../secret-lib
    ./efi-grub.nix
    ./erase-my-darlings.nix
    ./main.nix
    ./networking.nix
    ./nix-flakes.nix
    ./pin-nixpkgs.nix
    ./pulseaudio.nix
    ./vault-agent.nix
    ./vps-remote-access.nix
    ./sshd-emacs.nix
    ./xserver
    ../hardware/default.nix
  ];
}
