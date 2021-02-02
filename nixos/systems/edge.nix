{
  system = "aarch64-linux";
  username = "u0_a269";
  homeDirectory = "/data/data/com.termux/files/home";
  hostname = "edge";

  modules = [
    # ../hm-modules/emacs
    (_: _: {
      home.packages = [ ];
      home.stateVersion = "20.09";
    })
  ];
}
