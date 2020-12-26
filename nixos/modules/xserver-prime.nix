{ intelBusId, nvidiaBusId }: { config, pkgs, ... }:
let
  mkForce = pkgs.lib.mkForce;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  environment.systemPackages = [ nvidia-offload ];

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia.prime = {
    offload.enable = true;
    
    inherit intelBusId nvidiaBusId;
  };
}
