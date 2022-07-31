# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "emacs-ng";
  overlay = {
    emacs,
    vtermModule,
  }: final: prev:
    with prev.lib; let
      inherit (prev) callPackage stdenv;

      hunspellWithDicts = cfg:
        stdenv.mkDerivation {
          name = (appendToName "with-dicts" cfg.hunspell.package).name;
          buildInputs = [prev.makeWrapper];
          buildCommand = ''
            makeWrapper \
              ${cfg.hunspell.package.bin}/bin/hunspell $out/bin/hunspell \
                --prefix DICPATH : ${makeSearchPath "share/hunspell" cfg.hunspell.dictionaries}
          '';
          meta = removeAttrs cfg.hunspell.package.meta ["outputsToInstall"];
        };
    in {
      magic_rb =
        prev.magic_rb
        or {}
        // {
          libvterm-emacs = stdenv.mkDerivation {
            name = "vterm-emacs";
            src = vtermModule;
            buildInputs = with prev; [cmake libtool glib.dev libvterm-neovim];
            cmakeFlags = [
              "-DEMACS_SOURCE=${emacs}"
              "-DUSE_SYSTEM_LIBVTERM=ON"
            ];
            installPhase = ''
              mkdir -p $out/lib
              install ../vterm-module.so $out/lib
            '';
          };

          emacs =
            callPackage
            (module: let
              mkPkgOption = name:
                mkOption {
                  description = "`${name}` package.";
                  default = prev.${name};
                  type = types.package;
                };

              evaled =
                evalModules
                {
                  modules =
                    [module]
                    ++ singleton
                    ({config, ...}: {
                      options = {
                        pkgs = {
                          giflib = mkPkgOption "giflib";
                          librsvg = mkPkgOption "librsvg";
                          glib-networking = mkPkgOption "glib-networking";
                          webkitgtk = mkPkgOption "webkitgtk";
                          xorg = mkOption {
                            description = "`xorg` package set.";
                            default = prev.xorg;
                            type = with types; attrsOf package;
                          };
                          makeWrapper = mkPkgOption "makeWrapper";
                          libvterm-emacs = mkOption {
                            description = "`libvterm-emacs` package.";
                            default = final.magic_rb.libvterm-emacs;
                            type = types.package;
                          };
                        };

                        march = mkOption {
                          description = ''
                            Optimize for a specific architecture.
                          '';
                          type = with types; nullOr str;
                          default = null;
                        };

                        additionalPackages = mkOption {
                          description = ''
                            Additional packages to add statically to the Emacs closure, requires a
                            restart of Emacs for changes to take effect.
                          '';
                          type = with types; listOf package;
                          default = [];
                        };

                        environment = mkOption {
                          description = ''
                            Set additional environment variables.
                          '';
                          type = with types; attrsOf (oneOf [str path]);
                          default = {};
                        };

                        hunspell = {
                          enable = mkEnableOption "Enable hunspell and dictionaries";
                          package = mkPkgOption "hunspell";
                          dictionaries = mkOption {
                            description = ''
                              Dictionaries included with Hunspell.
                            '';
                            type = with types; listOf package;
                            default = [];
                          };
                        };

                        output = {
                          base = mkOption {
                            readOnly = true;
                            type = types.package;
                            description = ''
                              Emacs base output without additional packages available.
                            '';
                          };

                          bundle = mkOption {
                            readOnly = true;
                            type = types.package;
                            description = ''
                              Emacs bundle with additional packages.
                            '';
                          };
                        };
                      };

                      config = {
                        additionalPackages =
                          optional config.hunspell.enable (hunspellWithDicts config);

                        output.base =
                          (prev.emacs.override
                            {
                              withX = false;
                              withGTK3 = false;
                              nativeComp = true;
                              srcRepo = true;

                              libXaw = null;
                              Xaw3d = null;
                              gconf = null;
                              alsa-lib = null;
                              acl = null;
                              gpm = null;

                              AppKit = null;
                              GSS = null;
                              ImageIO = null;
                              sigtool = null;
                            })
                          .overrideAttrs
                          (old: {
                            src = emacs;

                            configureFlags = [
                              "--disable-build-details"
                              "--with-native-compilation"

                              "--with-xpm=yes"
                              "--with-jpeg=yes"
                              "--with-png=yes"
                              "--with-gif=yes"
                              "--with-tiff=yes"
                              "--with-json=yes"
                              "--with-cairo"
                              "--without-x"
                              "--with-pgtk"
                              "--with-modules"
                              "--prefix=\${out}"
                            ];

                            NIX_CFLAGS_COMPILE = "-O2 -pipe ${optionalString (config.march != null) ("-march=" + config.march)}";

                            buildInputs =
                              old.buildInputs
                              ++ (with config.pkgs; [giflib librsvg glib-networking webkitgtk xorg.libXpm]);

                            makeFlags =
                              old.makeFlags
                              or []
                              ++ [
                                "NATIVE_FULL_AOT=1"
                              ];
                          });

                        output.bundle = with config.pkgs;
                        with config.output;
                          stdenv.mkDerivation {
                            inherit (base) pname version;

                            phases = ["buildPhase"];

                            nativeBuildInputs = [makeWrapper];

                            buildPhase = ''
                              mkdir -p $out/bin
                              ${xorg.lndir}/bin/lndir -silent ${base} $out
                              wrapProgram $out/bin/emacs \
                                --set EMACSLOADPATH ${base}/share/emacs/site-lisp:${libvterm-emacs}/lib: \
                                --prefix PATH : ${makeBinPath config.additionalPackages} \
                                ${concatStringsSep " " (mapAttrsToList (k: v: "--set " + k + " " + v) config.environment)}
                            '';
                          };
                      };
                    });
                };
            in
              evaled.config.output)
            {};
        };
    };
}
