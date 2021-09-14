nglib:
final: prev:
{
  lispPackages = prev.lispPackages // {
    cl-webkit2 = prev.lispPackages.cl-webkit2.overrideAttrs (old:
      {
          src = prev.fetchFromGitHub {
            owner = "joachifm";
            repo = "cl-webkit";
            rev = "90b1469713265096768fd865e64a0a70292c733d";
            sha256 = "sha256:0lxws342nh553xlk4h5lb78q4ibiwbm2hljd7f55w3csk6z7bi06";
          };
      });

    nyxt = prev.lispPackages.nyxt.overrideAttrs (old: {
      version = "2.1.1";
      src = prev.fetchFromGitHub {
        owner = "atlas-engineer";
        repo = "nyxt";
        rev = "2.1.1";
        sha256 = "sha256-GdTOFu5yIIL9776kfbo+KS1gHH1xNCfZSWF5yHUB9U8=";
      };
    });
  };
}
