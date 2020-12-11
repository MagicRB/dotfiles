self: super:
let
  svt-av1 = let
    version = "0.8.6";
  in super.stdenv.mkDerivation rec {
    pname = "svt-av1";
    inherit version;

    nativeBuildInputs = with super.pkgs; [ cmake nasm ];

    src = fetchGit {
      url = "https://github.com/AOMediaCodec/SVT-AV1";
      ref = "v" + version;
    };
  };
in {
  ffmpeg-svt-av1 = (super.ffmpeg.overrideAttrs ( old: {
    buildInputs = old.buildInputs ++ [ svt-av1 ];
    configureFlags = old.configureFlags ++ [ "--enable-libsvtav1" ];
    src = fetchGit {
      url = "https://github.com/FFmpeg/FFmpeg";
      rev = "5b1ccd748a7c5317172981151a36711772455368";
    };
  })).override {
    version = "4.4-dev"; 
  };
}
