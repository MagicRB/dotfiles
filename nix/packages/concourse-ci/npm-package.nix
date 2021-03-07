{  }:
  allDeps:
    {
      key = { name = "web"; scope = ""; };
      version = "0.0.1";
      nodeBuildInputs = let
        a = allDeps;
      in [
        (a."elm-test@^0.19.1-revision4")
        (a."@babel/plugin-syntax-dynamic-import@^7.8.3")
        (a."webpack-cli@^3.3.11")
        (a."@babel/preset-env@^7.8.4")
        (a."clean-css-cli@^4.3.0")
        (a."child-process-promise@^2.2.1")
        (a."less@^3.10.3")
        (a."elm-analyse@git+https://github.com/stil4m/elm-analyse.git")
        (a."uglify-js@^3.6.3")
        (a."@babel/core@^7.8.4")
        (a."babel-loader@^8.0.6")
        (a."elm@^0.19.1")
        (a."less-plugin-autoprefix@^2.0.0")
        (a."elm-format@0.8.2")
        (a."@mdi/svg@^5.0.45")
        (a."puppeteer@^1.20.0")
        (a."webpack@^4.41.6")
        (a."request@^2.86.0")
        ];
      meta = {
        license = {
          fullName = "Apache License 2.0";
          shortName = "asl20";
          spdxId = "Apache-2.0";
          url = "https://spdx.org/licenses/Apache-2.0.html";
          };
        };
      }
