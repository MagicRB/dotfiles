# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  xserver-enable = config.services.xserver.enable;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
  cfg = config.magic_rb.xserver;
in {
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
          primeOffload = mkEnableOption "NVidia PRIME sync support";
          primeSync = mkEnableOption "NVidia PRIME offload support";

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
    qwertyNeo2 = mkEnableOption "Add custom qwerty neo layout.";
    mimickInTty = mkEnableOption "Mimick xkb set keyboard layouts in TTYs.";
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
        opengl.enable = true;
        opengl.driSupport32Bit = true;
      };
    }
    (mkIf (cfg.gpu == "nvidia") {
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia.modesetting.enable = mkIf cfg.nvidia.primeSync true;

      services.xserver.deviceSection = mkIf cfg.nvidia.primeSync ''
        Option "Coolbits" "28"
        Option "UseEvents" "on"
      '';

      environment.systemPackages =
        mkIf cfg.nvidia.primeOffload
        [nvidia-offload pkgs.libglvnd];

      hardware.nvidia.prime = mkIf (cfg.nvidia.primeSync || cfg.nvidia.primeOffload) {
        sync.enable = cfg.nvidia.primeSync;
        offload.enable = cfg.nvidia.primeOffload;

        intelBusId = cfg.nvidia.intelBusId;
        nvidiaBusId = cfg.nvidia.nvidiaBusId;
      };
    })
    (mkIf cfg.qwertyNeo2 {
      services.xserver = {
        layout = "de,de";
        xkbVariant = "koy,neo_qwerty";
        xkbOptions = "ctrl:swap_lalt_lctl_lwin, altwin:menu_win, grp:sclk_toggle";

        extraLayouts."neo_qwerty" = {
          description = "QWERTY neo2 layout.";
          languages = ["de"];
          symbolsFile = ./qwerty_neo.xkb;
        };
      };
    })
    (mkIf cfg.mimickInTty {
      console.useXkbConfig = true;
    })
  ]);
}
