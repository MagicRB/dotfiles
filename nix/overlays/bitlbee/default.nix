final: prev:
with final.lib;
{
  magic_rb = prev.magic_rb or {} // {
    bitlbee = (prev.bitlbee.override
      { enableLibPurple = true;
        pidgin = prev.pidgin.override
          { plugins = with prev; [ purple-discord ];
          };
      }).overrideAttrs
      (old:
        { configureFlags = old.configureFlags ++ singleton "--config=/tmp/bitlbee";
        }
      );
  };
}
