{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
with lib;
let
  nvidia-offload = nixpkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
  cfg = config.magic_rb.xserver;
in
{
  options.magic_rb.xserver = {
    enable = mkEnableOption "XServer for my setup style";
    gpu = mkOption {
      description = "Which GPU type do you have?";
      type = types.oneOf ["nvidia"];
    };
    prime = mkEnableOption "NVidia PRIME support";
    xmonad = mkEnableOption "Enable xmonad";

    intelBusId = mkOption {
      type = types.string;
    };
    nvidiaBusId = mkOption {
      type = types.string;
    };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;

      windowManager = mkIf cfg.xmonad {
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
      };

      displayManager = mkIf cfg.xmonad {
        defaultSession = "none+xmonad";
      };
      
      videoDrivers = [ "nvidia" ];
        
      libinput.enable = true;
    };

    hardware = {
      opengl.driSupport32Bit = true;
      # opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ]; # What does this do??
    };

    environment.systemPackages = mkIf (cfg.prime && cfg.gpu == "nvidia") [ nvidia-offload ];

    hardware.nvidia.prime = mkIf cfg.prime {
      offload.enable = true;
      
      intelBusId = cfg.intelBusId;
      nvidiaBusId = cfg.nvidiaBusId;
    };
  };
}
