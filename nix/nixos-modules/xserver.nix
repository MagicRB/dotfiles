{ config, lib, pkgs, ... }:
with lib;
let
  xserver-enable = config.services.xserver.enable;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
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
      type = types.enum ["nvidia"];
    };

    nvidia = mkOption {
      description = "NVidia section";
      type = types.submodule {
        options = {
          prime = mkEnableOption "NVidia PRIME support";
          
          intelBusId = mkOption {
            type = types.str;
            default = "";
          };
          nvidiaBusId = mkOption {
            type = types.str;
            default = "";
          };

          linux-5-11-patch = mkEnableOption "Linux 5.11 compat patch";
        };
      };
      default = {};
    };
    xmonad = mkEnableOption "Enable xmonad";
    lightdm = mkEnableOption "Enable lightdm";
    setSkLayout = mkEnableOption "Set SK layout";
    emacsCtrl = mkEnableOption "Rebind CapsLock to Ctrl";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.xserver = {
        enable = true;

        windowManager = mkIf cfg.xmonad {
          xmonad.enable = true;
          xmonad.enableContribAndExtras = true;
        };

        displayManager = mkIf cfg.xmonad {
          lightdm.enable = mkIf cfg.lightdm true;
          defaultSession = "none+xmonad";
        };
        
        
        libinput.enable = true;
      };

      hardware = {
        opengl.driSupport32Bit = true;
      };
    }
    (mkIf (cfg.gpu == "nvidia") {
      services.xserver.videoDrivers = [ "nvidia" ];

      environment.systemPackages = mkIf (cfg.nvidia.prime)
        [ nvidia-offload pkgs.libglvnd ];

      hardware.nvidia.prime = mkIf cfg.nvidia.prime {
        offload.enable = true;
        
        intelBusId = cfg.nvidia.intelBusId;
        nvidiaBusId = cfg.nvidia.nvidiaBusId;
      };
    })
    (mkIf cfg.setSkLayout {
      services.xserver = {
        layout = mkIf xserver-enable "us,sk";
        xkbVariant = mkIf xserver-enable ",qwerty";
      };
    })
    (mkIf cfg.emacsCtrl {
      services.xserver = {
        xkbOptions = mkIf xserver-enable "grp:lalt_lshift_toggle ctrl:nocaps";
      };
    })
  ]);
}
