final: prev:
{
  winetricks = prev.winetricks.overrideAttrs (_:
    {
      src = final.fetchFromGitHub {
        rev = "fa11b11a91a984666bf83b42e09be33ec0d6b294";
        sha256 = "sha256-exMhj3dS8uXCEgOaWbftaq94mBOmtZIXsXb9xNX5ha8=";
        owner = "Winetricks";
        repo = "winetricks";
      };
    }
  );
}
