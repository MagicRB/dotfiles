nixpkgs-unstable: { config, pkgs, ... }:
{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.kernelPackages = nixpkgs-unstable.linuxPackages_latest;

  powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "performance";
}
