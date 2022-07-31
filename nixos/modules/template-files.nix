{config, lib, pkgs, ...}:
with lib;
let
  cfg = config.services.template-files;
  format = pkgs.formats.json {};
in
{
  options.services.template-files = mkOption {
    description =
      ''
        Attrset of template-files
      '';
    type = with types;
      attrsOf (submodule
        ({...}:
          {
            options = {
              text = mkOption {
                description =
                  ''
                    Text to template into result, mutually exclusive with <option>json</option>
                  '';
                type = nullOr str;
                default = null;
              };
              json = mkOption {
                description =
                  ''
                    Nix attrset to template into result, mutually exclusive with <option>text</option>
                  '';
                type = nullOr format.type;
                default = null;
              };
              script = mkOption {
                description =
                  ''
                    Script to run prior to templating.
                  '';
                type = lines;
                default = "";
              };
              result = mkOption {
                description =
                  ''
                    Path to result of templating, filled in at runtime.
                  '';
                type = string;
                readOnly = true;
              };
            };
          }));
    default = {};
    apply =
      mapAttrs
        (n: v:
          v //
          {
            result =
              if v.text != null then
                "/run/cfg/templates/${n}"
              else if v.json != null then
                "/run/cfg/templates/${n}.json"
              else
                throw "unreachable";
          }
        );
  };

  config.systemd.tmpfiles.rules = mkIf (cfg != {})
    ["d /run/cfg/templates 1755 root root -"];

  config.systemd.services = flip mapAttrs' cfg
    (n: v:
      nameValuePair
        (n + "-dynamic")
        {
          path = with pkgs; [ envsubst ];
          serviceConfig = {
            Type = "oneshot";
            ExecStart =
              let
                template =
                  if v.text != null then
                    pkgs.writeText n v.text
                  else if v.json != null then
                    format.generate (n + ".json") v.json
                  else
                    throw "unreachable";
              in
                pkgs.writeShellScript (n + "-script")
                  ''
                    ${v.script}
                    envsubst -i "${template}" -o "${v.result}"
                  '';
          };
        }
    );
}
