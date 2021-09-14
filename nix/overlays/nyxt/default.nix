nglib:
final: prev:
{
  lispPackages = prev.lispPackages // {
    cl-webkit2 = prev.lispPackages.callPackage (args @ { fetchFromGitHub, ... }:
      rec {
        baseName = "cl-webkit2";
        version = "cl-webkit-20210411-git";

        description = "An FFI binding to WebKit2GTK+";

        deps = [ args."alexandria" args."babel" args."bordeaux-threads" args."cffi" args."cl-cffi-gtk" args."cl-cffi-gtk-cairo" args."cl-cffi-gtk-gdk" args."cl-cffi-gtk-gdk-pixbuf" args."cl-cffi-gtk-gio" args."cl-cffi-gtk-glib" args."cl-cffi-gtk-gobject" args."cl-cffi-gtk-pango" args."closer-mop" args."iterate" args."trivial-features" args."trivial-garbage" ];

        src = fetchFromGitHub {
          owner = "joachifm";
          repo = "cl-webkit";
          rev = "90b1469713265096768fd865e64a0a70292c733d";
          sha256 = "sha256:0lxws342nh553xlk4h5lb78q4ibiwbm2hljd7f55w3csk6z7bi06";
        };

        packageName = "cl-webkit2";

        asdFilesToKeep = ["cl-webkit2.asd"];
        overrides = x: x;
      });
}
