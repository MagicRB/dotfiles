{
  system = "x86_64-linux";
  hostname = "heater";
  check = false;

  config = {};

  modules = [
    (_: _: {
      boot.isContainer = true;
      time.timeZone = "Europe/Bratislava";

      services.openssh = {
        enable = true;
        ports = [
          226
        ];
      };
      users = {
        mutableUsers = false;

        users = {
          test = {
            isNormalUser = true;
            createHome = true;
            home = "/home/alice";
            hashedPassword = "$5$u0bQFm8u$VkR52hsgy08EPsLF0n1JrySKVMXgqnOdXbh8l9TOdaC";

            uid = 1002;
            extraGroups = [ "wheel" ];
          };
        };
      };

    })
  ];
}
