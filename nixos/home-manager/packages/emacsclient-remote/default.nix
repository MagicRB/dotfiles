(import (
  fetchTarball {
    url = "https://github.com/hercules-ci/flake-compat/archive/a2fd0358b1caf15a9179721425f71af978ef8a13.tar.gz";
    sha256 = "1vcrx860daz41qgy1wir0w6jl0sckaaw1gbvlhichbqwf8p0i251"; }
) {
  src =  ./.;
}).defaultNix
