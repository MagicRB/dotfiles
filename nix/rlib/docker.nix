{
  makeShadow = {
    pkgs
  , users ? []
  , groups ? []
  , withNixbld ? false
  }:
    let 
      group = pkgs.writeTextFile {
        name = "group";
        destination = "/etc/group";
        text = with pkgs.lib; ''
          root:x:0:
          ${optionalString withNixbld
            "nixbld:x:30000:nixbld1,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld2,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld3,nixbld30,nixbld31,nixbld32,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9" }
          nogroup:x:65534:

          ${concatMapStringsSep "\n"
            (user: "${group.name}:x:${toString group.id}")
            groups} 
        '';
      };
      passwd = pkgs.writeTextFile {
        name = "passwd";
        destination = "/etc/passwd";
        text = with pkgs; ''
          root:x:0:0:System administrator:/root:${bash}/bin/bash
          ${lib.optionalString withNixbld
            (lib.concatMapStringsSep "\n"
              (num: "nixbld1:x:${toString (30000 + num)}:30000:Nix build user ${(toString num)}:/var/empty:${busybox}/bin/nologin")
              [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32])}
          nobody:x:65534:65534:Unprivileged account (don't use!):/var/empty:${busybox}/bin/nologin

          ${lib.concatMapStringsSep "\n"
            (user: "${user.name}:x:${toString user.uid}:${toString user.gid}:${toString user.description}:${toString user.home}:${toString user.shell}")
            users} 
        '';
      };
      varEmpty = pkgs.runCommandNoCCLocal "var-empty" {} ''
        mkdir -p $out/var/empty
        # chown 0:0 $out/var/empty
        chmod 555 $out/var/empty
      '';
    in
      pkgs.symlinkJoin { name = "shadow"; paths = [ passwd group varEmpty ]; };

  makeCerts = { pkgs, certs }:
    let
      caBundle = pkgs.cacert;

      merged = pkgs.runCommandNoCC "ca-certificates.crt"
        { files = certs
            ++ [ "${caBundle}/etc/ssl/certs/ca-bundle.crt" ];
          preferLocalBuild = true;
        }
        ''
        cat $files > $out
      '';
    in
      pkgs.runCommandNoCCLocal "ca-certificates" {} ''
        mkdir -p $out/etc/ssl/certs
        ln -s ${merged} $out/etc/ssl/certs/ca-bundle.crt
        ln -s $out/etc/ssl/certs/ca-bundle.crt $out/etc/ssl/certs/ca-certificates.crt
    '';
}
