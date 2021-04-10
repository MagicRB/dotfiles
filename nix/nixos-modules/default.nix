{ ... }:
{
  imports = [
    ./efi-grub.nix
    ./erase-my-darlings.nix
    ./main.nix
    ./networking.nix
    ./nix-flakes.nix
    ./pin-nixpkgs.nix
    ./pulseaudio.nix
    ./vault-agent.nix
    ./xserver.nix
    ../hardware/default.nix
  ];
}
