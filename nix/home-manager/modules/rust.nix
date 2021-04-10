{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.rust;
  fenix = config.magic_rb.pins.fenix.packages."${pkgs.stdenv.system}";
in
{
  options.magic_rb.packageCollections.rust = {
    enable = mkEnableOption "Enable the Rust package collection.";

    components = mkOption {
      description = "Which componenets to add, such as rustc, clippy or caargo.";
      type = with types; listOf str;
      default = [
        "cargo"
        "rustc"
        "rust-src"
        "rust-std"
        "clippy-preview"
        "rustfmt-preview"
      ];
    };

    rust-analyzer = mkOption {
      description = "Whether to add rust-analyzer too";
      type = types.bool;
      default = true;
    };
  };
  
  config = mkIf cfg.enable {
    home.packages = [
      (fenix.latest.withComponents cfg.components)
      
    ] ++ (optional cfg.rust-analyzer fenix.rust-analyzer);
  };
}
