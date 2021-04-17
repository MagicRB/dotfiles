nglib:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    gpg-key = (nglib prev.stdenv.system).writeSubstitutedShellScriptBin {
      name = "gpg-key";
      file = ./gpg-key;
      substitutes = with prev; {
        inherit cryptsetup busybox findutils;
      };
    };

    gpg-key-hs = with prev; writers.writeHaskellBin
      "gpg-key"
      { libraries = [ haskellPackages.shh ]; }
      ''
        {-# LANGUAGE TemplateHaskell #-}
        import Shh
        import System.Posix.User (getRealUserID)
        import System.Environment (getArgs)

        loadFromBins ["${findutils}", "${busybox}", "${cryptsetup}"]
        
        main = do
          userId <- getRealUserID 
          if userId /= 0 then do
            putStrLn "You must run this script as root." 
          else do
            cmdArgs <- getArgs
            let
              action = cmdArgs !! 0
            case action of
              "open" -> do
                cryptsetup "open" "/dev/disk/by-label/secret" "secret"
                mkdir "-p" "/mnt/key"

                mount "/dev/mapper/secret" "/mnt/key"
              "close" -> do
                umount "/mnt/key"
                cryptsetup "close" "secret"
                
                rm "-r" "/mnt/key"
                mntContains <- captureTrim <| find "/mnt" "-maxdepth" "0" "-empty"
                if show mntContains == "" then do
                  putStrLn "delete /mnt" -- rm "-r" "/mnt"
                else do
                  return ()
              otherwise -> do
                putStrLn "open - open key\nclose - close key"
      '';
  };
}
