final: prev:
let
  ghc = prev.haskellPackages.ghcWithPackages (p: with p; [shh shh-extras word8]);
in
{
  magic_rb = prev.magic_rb or {} // {
    shh = prev.writeShellScriptBin "shh" ''
      export PATH=${ghc}/bin:${prev.haskellPackages.shh}/bin:$PATH
      
      exec shh $@
    '';
  };
}
