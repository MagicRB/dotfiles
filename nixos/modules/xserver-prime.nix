{ intelBusId, nvidiaBusId }:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
let
  mkForce = nixpkgs.lib.mkForce;
  nvidia-offload = nixpkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  environment.systemPackages = [ nvidia-offload ];

  services.xserver.videoDrivers = [ "nvidiaBeta" ];

  hardware.nvidia.prime = {
    offload.enable = true;
    
    inherit intelBusId nvidiaBusId;
  };
}
