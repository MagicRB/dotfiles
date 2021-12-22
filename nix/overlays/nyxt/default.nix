nglib:
final: prev:
{
  lispPackages = prev.lispPackages // {
    cl-webkit2 = prev.lispPackages.cl-webkit2.overrideAttrs (old:
      {
          src = prev.fetchFromGitHub {
            owner = "joachifm";
            repo = "cl-webkit";
            rev = "5a1e07f7e58951b5d6c46b14bbe7df3ce79a2639";
            sha256 = "sha256-URyas57Q5hiGcYrwn1lLgIto22MriavyqziCYEvWnQo=";
          };
      });

    nyxt = prev.lispPackages.nyxt.overrideAttrs (old: {
      version = "2.2.3";
      src = prev.fetchFromGitHub {
        owner = "atlas-engineer";
        repo = "nyxt";
        rev = "2.2.3";
        sha256 = "sha256-FzMDGvbtXi3EUU5Ri1sSS00nOzpvO3DmgJ1fQuT6Ouw=";
      };
    });
  };
}
