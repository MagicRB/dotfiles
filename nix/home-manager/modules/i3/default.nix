{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = (with nixpkgs-unstable; [
    i3status-rust
  ]) ++ (with nixpkgs; [
    font-awesome
    dejavu_fonts
    feh
  ]) ++ (with custom; [
    screenshot
  ]);

  home.file = {
    ".config/i3/config".text = (import ./config.nix { inherit hostname nixpkgs; screenshot = custom.screenshot; }).config;
    ".config/i3/status.toml".text = (import ./status.toml.nix { inherit hostname nixpkgs; }).config;
  };
}
