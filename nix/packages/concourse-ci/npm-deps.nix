{ fetchurl, fetchgit }:
  self:
    super:
      let
        registries = {
          yarn = n:
            v:
              "https://registry.yarnpkg.com/${n}/-/${n}-${v}.tgz";
          npm = n:
            v:
              "https://registry.npmjs.org/${n}/-/${n}-${v}.tgz";
          };
        nodeFilePackage = key:
          version:
            registry:
              sha1:
                deps:
                  super._buildNodePackage {
                    inherit key version;
                    src = fetchurl {
                      url = registry key version;
                      inherit sha1;
                      };
                    nodeBuildInputs = deps;
                    };
        nodeFileLocalPackage = key:
          version:
            path:
              sha1:
                deps:
                  super._buildNodePackage {
                    inherit key version;
                    src = builtins.path { inherit path; };
                    nodeBuildInputs = deps;
                    };
        nodeGitPackage = key:
          version:
            url:
              rev:
                sha256:
                  deps:
                    super._buildNodePackage {
                      inherit key version;
                      src = fetchgit { inherit url rev sha256; };
                      nodeBuildInputs = deps;
                      };
        identityRegistry = url:
          _:
            _:
              url;
        scopedName = scope:
          name:
            { inherit scope name; };
        ir = identityRegistry;
        l = nodeFileLocalPackage;
        f = nodeFilePackage;
        g = nodeGitPackage;
        n = registries.npm;
        y = registries.yarn;
        sc = scopedName;
        s = self;
      in {
        "@babel/code-frame@7.10.4" = f (sc "babel" "code-frame") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.10.4.tgz") "168da1a36e90da68ae8d49c0f1b48c7c6249213a" [
          (s."@babel/highlight@^7.10.4")
          ];
        "@babel/code-frame@^7.10.4" = s."@babel/code-frame@7.10.4";
        "@babel/compat-data@7.11.0" = f (sc "babel" "compat-data") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/compat-data/-/compat-data-7.11.0.tgz") "e9f73efe09af1355b723a7f39b11bad637d7c99c" [
          (s."browserslist@^4.12.0")
          (s."invariant@^2.2.4")
          (s."semver@^5.5.0")
          ];
        "@babel/compat-data@^7.10.4" = s."@babel/compat-data@7.11.0";
        "@babel/compat-data@^7.11.0" = s."@babel/compat-data@7.11.0";
        "@babel/core@7.11.6" = f (sc "babel" "core") "7.11.6" (ir "https://registry.yarnpkg.com/@babel/core/-/core-7.11.6.tgz") "3a9455dc7387ff1bac45770650bc13ba04a15651" [
          (s."@babel/code-frame@^7.10.4")
          (s."@babel/generator@^7.11.6")
          (s."@babel/helper-module-transforms@^7.11.0")
          (s."@babel/helpers@^7.10.4")
          (s."@babel/parser@^7.11.5")
          (s."@babel/template@^7.10.4")
          (s."@babel/traverse@^7.11.5")
          (s."@babel/types@^7.11.5")
          (s."convert-source-map@^1.7.0")
          (s."debug@^4.1.0")
          (s."gensync@^1.0.0-beta.1")
          (s."json5@^2.1.2")
          (s."lodash@^4.17.19")
          (s."resolve@^1.3.2")
          (s."semver@^5.4.1")
          (s."source-map@^0.5.0")
          ];
        "@babel/core@^7.8.4" = s."@babel/core@7.11.6";
        "@babel/generator@7.11.6" = f (sc "babel" "generator") "7.11.6" (ir "https://registry.yarnpkg.com/@babel/generator/-/generator-7.11.6.tgz") "b868900f81b163b4d464ea24545c61cbac4dc620" [
          (s."@babel/types@^7.11.5")
          (s."jsesc@^2.5.1")
          (s."source-map@^0.5.0")
          ];
        "@babel/generator@^7.11.5" = s."@babel/generator@7.11.6";
        "@babel/generator@^7.11.6" = s."@babel/generator@7.11.6";
        "@babel/helper-annotate-as-pure@7.10.4" = f (sc "babel" "helper-annotate-as-pure") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-annotate-as-pure/-/helper-annotate-as-pure-7.10.4.tgz") "5bf0d495a3f757ac3bda48b5bf3b3ba309c72ba3" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-annotate-as-pure@^7.10.4" = s."@babel/helper-annotate-as-pure@7.10.4";
        "@babel/helper-builder-binary-assignment-operator-visitor@7.10.4" = f (sc "babel" "helper-builder-binary-assignment-operator-visitor") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-builder-binary-assignment-operator-visitor/-/helper-builder-binary-assignment-operator-visitor-7.10.4.tgz") "bb0b75f31bf98cbf9ff143c1ae578b87274ae1a3" [
          (s."@babel/helper-explode-assignable-expression@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-builder-binary-assignment-operator-visitor@^7.10.4" = s."@babel/helper-builder-binary-assignment-operator-visitor@7.10.4";
        "@babel/helper-compilation-targets@7.10.4" = f (sc "babel" "helper-compilation-targets") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-compilation-targets/-/helper-compilation-targets-7.10.4.tgz") "804ae8e3f04376607cc791b9d47d540276332bd2" [
          (s."@babel/compat-data@^7.10.4")
          (s."browserslist@^4.12.0")
          (s."invariant@^2.2.4")
          (s."levenary@^1.1.1")
          (s."semver@^5.5.0")
          ];
        "@babel/helper-compilation-targets@^7.10.4" = s."@babel/helper-compilation-targets@7.10.4";
        "@babel/helper-create-class-features-plugin@7.10.5" = f (sc "babel" "helper-create-class-features-plugin") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/helper-create-class-features-plugin/-/helper-create-class-features-plugin-7.10.5.tgz") "9f61446ba80e8240b0a5c85c6fdac8459d6f259d" [
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/helper-member-expression-to-functions@^7.10.5")
          (s."@babel/helper-optimise-call-expression@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-replace-supers@^7.10.4")
          (s."@babel/helper-split-export-declaration@^7.10.4")
          ];
        "@babel/helper-create-class-features-plugin@^7.10.4" = s."@babel/helper-create-class-features-plugin@7.10.5";
        "@babel/helper-create-regexp-features-plugin@7.10.4" = f (sc "babel" "helper-create-regexp-features-plugin") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-create-regexp-features-plugin/-/helper-create-regexp-features-plugin-7.10.4.tgz") "fdd60d88524659a0b6959c0579925e425714f3b8" [
          (s."@babel/helper-annotate-as-pure@^7.10.4")
          (s."@babel/helper-regex@^7.10.4")
          (s."regexpu-core@^4.7.0")
          ];
        "@babel/helper-create-regexp-features-plugin@^7.10.4" = s."@babel/helper-create-regexp-features-plugin@7.10.4";
        "@babel/helper-define-map@7.10.5" = f (sc "babel" "helper-define-map") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/helper-define-map/-/helper-define-map-7.10.5.tgz") "b53c10db78a640800152692b13393147acb9bb30" [
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/types@^7.10.5")
          (s."lodash@^4.17.19")
          ];
        "@babel/helper-define-map@^7.10.4" = s."@babel/helper-define-map@7.10.5";
        "@babel/helper-explode-assignable-expression@7.11.4" = f (sc "babel" "helper-explode-assignable-expression") "7.11.4" (ir "https://registry.yarnpkg.com/@babel/helper-explode-assignable-expression/-/helper-explode-assignable-expression-7.11.4.tgz") "2d8e3470252cc17aba917ede7803d4a7a276a41b" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-explode-assignable-expression@^7.10.4" = s."@babel/helper-explode-assignable-expression@7.11.4";
        "@babel/helper-function-name@7.10.4" = f (sc "babel" "helper-function-name") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-function-name/-/helper-function-name-7.10.4.tgz") "d2d3b20c59ad8c47112fa7d2a94bc09d5ef82f1a" [
          (s."@babel/helper-get-function-arity@^7.10.4")
          (s."@babel/template@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-function-name@^7.10.4" = s."@babel/helper-function-name@7.10.4";
        "@babel/helper-get-function-arity@7.10.4" = f (sc "babel" "helper-get-function-arity") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-get-function-arity/-/helper-get-function-arity-7.10.4.tgz") "98c1cbea0e2332f33f9a4661b8ce1505b2c19ba2" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-get-function-arity@^7.10.4" = s."@babel/helper-get-function-arity@7.10.4";
        "@babel/helper-hoist-variables@7.10.4" = f (sc "babel" "helper-hoist-variables") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-hoist-variables/-/helper-hoist-variables-7.10.4.tgz") "d49b001d1d5a68ca5e6604dda01a6297f7c9381e" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-hoist-variables@^7.10.4" = s."@babel/helper-hoist-variables@7.10.4";
        "@babel/helper-member-expression-to-functions@7.11.0" = f (sc "babel" "helper-member-expression-to-functions") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/helper-member-expression-to-functions/-/helper-member-expression-to-functions-7.11.0.tgz") "ae69c83d84ee82f4b42f96e2a09410935a8f26df" [
          (s."@babel/types@^7.11.0")
          ];
        "@babel/helper-member-expression-to-functions@^7.10.4" = s."@babel/helper-member-expression-to-functions@7.11.0";
        "@babel/helper-member-expression-to-functions@^7.10.5" = s."@babel/helper-member-expression-to-functions@7.11.0";
        "@babel/helper-module-imports@7.10.4" = f (sc "babel" "helper-module-imports") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-module-imports/-/helper-module-imports-7.10.4.tgz") "4c5c54be04bd31670a7382797d75b9fa2e5b5620" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-module-imports@^7.10.4" = s."@babel/helper-module-imports@7.10.4";
        "@babel/helper-module-transforms@7.11.0" = f (sc "babel" "helper-module-transforms") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/helper-module-transforms/-/helper-module-transforms-7.11.0.tgz") "b16f250229e47211abdd84b34b64737c2ab2d359" [
          (s."@babel/helper-module-imports@^7.10.4")
          (s."@babel/helper-replace-supers@^7.10.4")
          (s."@babel/helper-simple-access@^7.10.4")
          (s."@babel/helper-split-export-declaration@^7.11.0")
          (s."@babel/template@^7.10.4")
          (s."@babel/types@^7.11.0")
          (s."lodash@^4.17.19")
          ];
        "@babel/helper-module-transforms@^7.10.4" = s."@babel/helper-module-transforms@7.11.0";
        "@babel/helper-module-transforms@^7.10.5" = s."@babel/helper-module-transforms@7.11.0";
        "@babel/helper-module-transforms@^7.11.0" = s."@babel/helper-module-transforms@7.11.0";
        "@babel/helper-optimise-call-expression@7.10.4" = f (sc "babel" "helper-optimise-call-expression") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-optimise-call-expression/-/helper-optimise-call-expression-7.10.4.tgz") "50dc96413d594f995a77905905b05893cd779673" [
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-optimise-call-expression@^7.10.4" = s."@babel/helper-optimise-call-expression@7.10.4";
        "@babel/helper-plugin-utils@7.10.4" = f (sc "babel" "helper-plugin-utils") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-plugin-utils/-/helper-plugin-utils-7.10.4.tgz") "2f75a831269d4f677de49986dff59927533cf375" [];
        "@babel/helper-plugin-utils@^7.0.0" = s."@babel/helper-plugin-utils@7.10.4";
        "@babel/helper-plugin-utils@^7.10.4" = s."@babel/helper-plugin-utils@7.10.4";
        "@babel/helper-plugin-utils@^7.8.0" = s."@babel/helper-plugin-utils@7.10.4";
        "@babel/helper-plugin-utils@^7.8.3" = s."@babel/helper-plugin-utils@7.10.4";
        "@babel/helper-regex@7.10.5" = f (sc "babel" "helper-regex") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/helper-regex/-/helper-regex-7.10.5.tgz") "32dfbb79899073c415557053a19bd055aae50ae0" [
          (s."lodash@^4.17.19")
          ];
        "@babel/helper-regex@^7.10.4" = s."@babel/helper-regex@7.10.5";
        "@babel/helper-remap-async-to-generator@7.11.4" = f (sc "babel" "helper-remap-async-to-generator") "7.11.4" (ir "https://registry.yarnpkg.com/@babel/helper-remap-async-to-generator/-/helper-remap-async-to-generator-7.11.4.tgz") "4474ea9f7438f18575e30b0cac784045b402a12d" [
          (s."@babel/helper-annotate-as-pure@^7.10.4")
          (s."@babel/helper-wrap-function@^7.10.4")
          (s."@babel/template@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-remap-async-to-generator@^7.10.4" = s."@babel/helper-remap-async-to-generator@7.11.4";
        "@babel/helper-replace-supers@7.10.4" = f (sc "babel" "helper-replace-supers") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-replace-supers/-/helper-replace-supers-7.10.4.tgz") "d585cd9388ea06e6031e4cd44b6713cbead9e6cf" [
          (s."@babel/helper-member-expression-to-functions@^7.10.4")
          (s."@babel/helper-optimise-call-expression@^7.10.4")
          (s."@babel/traverse@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-replace-supers@^7.10.4" = s."@babel/helper-replace-supers@7.10.4";
        "@babel/helper-simple-access@7.10.4" = f (sc "babel" "helper-simple-access") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-simple-access/-/helper-simple-access-7.10.4.tgz") "0f5ccda2945277a2a7a2d3a821e15395edcf3461" [
          (s."@babel/template@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-simple-access@^7.10.4" = s."@babel/helper-simple-access@7.10.4";
        "@babel/helper-skip-transparent-expression-wrappers@7.11.0" = f (sc "babel" "helper-skip-transparent-expression-wrappers") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/helper-skip-transparent-expression-wrappers/-/helper-skip-transparent-expression-wrappers-7.11.0.tgz") "eec162f112c2f58d3af0af125e3bb57665146729" [
          (s."@babel/types@^7.11.0")
          ];
        "@babel/helper-skip-transparent-expression-wrappers@^7.11.0" = s."@babel/helper-skip-transparent-expression-wrappers@7.11.0";
        "@babel/helper-split-export-declaration@7.11.0" = f (sc "babel" "helper-split-export-declaration") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/helper-split-export-declaration/-/helper-split-export-declaration-7.11.0.tgz") "f8a491244acf6a676158ac42072911ba83ad099f" [
          (s."@babel/types@^7.11.0")
          ];
        "@babel/helper-split-export-declaration@^7.10.4" = s."@babel/helper-split-export-declaration@7.11.0";
        "@babel/helper-split-export-declaration@^7.11.0" = s."@babel/helper-split-export-declaration@7.11.0";
        "@babel/helper-validator-identifier@7.10.4" = f (sc "babel" "helper-validator-identifier") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-validator-identifier/-/helper-validator-identifier-7.10.4.tgz") "a78c7a7251e01f616512d31b10adcf52ada5e0d2" [];
        "@babel/helper-validator-identifier@^7.10.4" = s."@babel/helper-validator-identifier@7.10.4";
        "@babel/helper-wrap-function@7.10.4" = f (sc "babel" "helper-wrap-function") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-wrap-function/-/helper-wrap-function-7.10.4.tgz") "8a6f701eab0ff39f765b5a1cfef409990e624b87" [
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/template@^7.10.4")
          (s."@babel/traverse@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helper-wrap-function@^7.10.4" = s."@babel/helper-wrap-function@7.10.4";
        "@babel/helpers@7.10.4" = f (sc "babel" "helpers") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helpers/-/helpers-7.10.4.tgz") "2abeb0d721aff7c0a97376b9e1f6f65d7a475044" [
          (s."@babel/template@^7.10.4")
          (s."@babel/traverse@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/helpers@^7.10.4" = s."@babel/helpers@7.10.4";
        "@babel/highlight@7.10.4" = f (sc "babel" "highlight") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/highlight/-/highlight-7.10.4.tgz") "7d1bdfd65753538fabe6c38596cdb76d9ac60143" [
          (s."@babel/helper-validator-identifier@^7.10.4")
          (s."chalk@^2.0.0")
          (s."js-tokens@^4.0.0")
          ];
        "@babel/highlight@^7.10.4" = s."@babel/highlight@7.10.4";
        "@babel/parser@7.11.5" = f (sc "babel" "parser") "7.11.5" (ir "https://registry.yarnpkg.com/@babel/parser/-/parser-7.11.5.tgz") "c7ff6303df71080ec7a4f5b8c003c58f1cf51037" [];
        "@babel/parser@^7.10.4" = s."@babel/parser@7.11.5";
        "@babel/parser@^7.11.5" = s."@babel/parser@7.11.5";
        "@babel/plugin-proposal-async-generator-functions@7.10.5" = f (sc "babel" "plugin-proposal-async-generator-functions") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-async-generator-functions/-/plugin-proposal-async-generator-functions-7.10.5.tgz") "3491cabf2f7c179ab820606cec27fed15e0e8558" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-remap-async-to-generator@^7.10.4")
          (s."@babel/plugin-syntax-async-generators@^7.8.0")
          ];
        "@babel/plugin-proposal-async-generator-functions@^7.10.4" = s."@babel/plugin-proposal-async-generator-functions@7.10.5";
        "@babel/plugin-proposal-class-properties@7.10.4" = f (sc "babel" "plugin-proposal-class-properties") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-class-properties/-/plugin-proposal-class-properties-7.10.4.tgz") "a33bf632da390a59c7a8c570045d1115cd778807" [
          (s."@babel/helper-create-class-features-plugin@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-proposal-class-properties@^7.10.4" = s."@babel/plugin-proposal-class-properties@7.10.4";
        "@babel/plugin-proposal-dynamic-import@7.10.4" = f (sc "babel" "plugin-proposal-dynamic-import") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-dynamic-import/-/plugin-proposal-dynamic-import-7.10.4.tgz") "ba57a26cb98b37741e9d5bca1b8b0ddf8291f17e" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.0")
          ];
        "@babel/plugin-proposal-dynamic-import@^7.10.4" = s."@babel/plugin-proposal-dynamic-import@7.10.4";
        "@babel/plugin-proposal-export-namespace-from@7.10.4" = f (sc "babel" "plugin-proposal-export-namespace-from") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-export-namespace-from/-/plugin-proposal-export-namespace-from-7.10.4.tgz") "570d883b91031637b3e2958eea3c438e62c05f54" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-export-namespace-from@^7.8.3")
          ];
        "@babel/plugin-proposal-export-namespace-from@^7.10.4" = s."@babel/plugin-proposal-export-namespace-from@7.10.4";
        "@babel/plugin-proposal-json-strings@7.10.4" = f (sc "babel" "plugin-proposal-json-strings") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-json-strings/-/plugin-proposal-json-strings-7.10.4.tgz") "593e59c63528160233bd321b1aebe0820c2341db" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-json-strings@^7.8.0")
          ];
        "@babel/plugin-proposal-json-strings@^7.10.4" = s."@babel/plugin-proposal-json-strings@7.10.4";
        "@babel/plugin-proposal-logical-assignment-operators@7.11.0" = f (sc "babel" "plugin-proposal-logical-assignment-operators") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-logical-assignment-operators/-/plugin-proposal-logical-assignment-operators-7.11.0.tgz") "9f80e482c03083c87125dee10026b58527ea20c8" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-logical-assignment-operators@^7.10.4")
          ];
        "@babel/plugin-proposal-logical-assignment-operators@^7.11.0" = s."@babel/plugin-proposal-logical-assignment-operators@7.11.0";
        "@babel/plugin-proposal-nullish-coalescing-operator@7.10.4" = f (sc "babel" "plugin-proposal-nullish-coalescing-operator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-nullish-coalescing-operator/-/plugin-proposal-nullish-coalescing-operator-7.10.4.tgz") "02a7e961fc32e6d5b2db0649e01bf80ddee7e04a" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-nullish-coalescing-operator@^7.8.0")
          ];
        "@babel/plugin-proposal-nullish-coalescing-operator@^7.10.4" = s."@babel/plugin-proposal-nullish-coalescing-operator@7.10.4";
        "@babel/plugin-proposal-numeric-separator@7.10.4" = f (sc "babel" "plugin-proposal-numeric-separator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-numeric-separator/-/plugin-proposal-numeric-separator-7.10.4.tgz") "ce1590ff0a65ad12970a609d78855e9a4c1aef06" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-numeric-separator@^7.10.4")
          ];
        "@babel/plugin-proposal-numeric-separator@^7.10.4" = s."@babel/plugin-proposal-numeric-separator@7.10.4";
        "@babel/plugin-proposal-object-rest-spread@7.11.0" = f (sc "babel" "plugin-proposal-object-rest-spread") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-object-rest-spread/-/plugin-proposal-object-rest-spread-7.11.0.tgz") "bd81f95a1f746760ea43b6c2d3d62b11790ad0af" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-object-rest-spread@^7.8.0")
          (s."@babel/plugin-transform-parameters@^7.10.4")
          ];
        "@babel/plugin-proposal-object-rest-spread@^7.11.0" = s."@babel/plugin-proposal-object-rest-spread@7.11.0";
        "@babel/plugin-proposal-optional-catch-binding@7.10.4" = f (sc "babel" "plugin-proposal-optional-catch-binding") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-optional-catch-binding/-/plugin-proposal-optional-catch-binding-7.10.4.tgz") "31c938309d24a78a49d68fdabffaa863758554dd" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-optional-catch-binding@^7.8.0")
          ];
        "@babel/plugin-proposal-optional-catch-binding@^7.10.4" = s."@babel/plugin-proposal-optional-catch-binding@7.10.4";
        "@babel/plugin-proposal-optional-chaining@7.11.0" = f (sc "babel" "plugin-proposal-optional-chaining") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-optional-chaining/-/plugin-proposal-optional-chaining-7.11.0.tgz") "de5866d0646f6afdaab8a566382fe3a221755076" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-skip-transparent-expression-wrappers@^7.11.0")
          (s."@babel/plugin-syntax-optional-chaining@^7.8.0")
          ];
        "@babel/plugin-proposal-optional-chaining@^7.11.0" = s."@babel/plugin-proposal-optional-chaining@7.11.0";
        "@babel/plugin-proposal-private-methods@7.10.4" = f (sc "babel" "plugin-proposal-private-methods") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-private-methods/-/plugin-proposal-private-methods-7.10.4.tgz") "b160d972b8fdba5c7d111a145fc8c421fc2a6909" [
          (s."@babel/helper-create-class-features-plugin@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-proposal-private-methods@^7.10.4" = s."@babel/plugin-proposal-private-methods@7.10.4";
        "@babel/plugin-proposal-unicode-property-regex@7.10.4" = f (sc "babel" "plugin-proposal-unicode-property-regex") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-unicode-property-regex/-/plugin-proposal-unicode-property-regex-7.10.4.tgz") "4483cda53041ce3413b7fe2f00022665ddfaa75d" [
          (s."@babel/helper-create-regexp-features-plugin@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-proposal-unicode-property-regex@^7.10.4" = s."@babel/plugin-proposal-unicode-property-regex@7.10.4";
        "@babel/plugin-proposal-unicode-property-regex@^7.4.4" = s."@babel/plugin-proposal-unicode-property-regex@7.10.4";
        "@babel/plugin-syntax-async-generators@7.8.4" = f (sc "babel" "plugin-syntax-async-generators") "7.8.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-async-generators/-/plugin-syntax-async-generators-7.8.4.tgz") "a983fb1aeb2ec3f6ed042a210f640e90e786fe0d" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-async-generators@^7.8.0" = s."@babel/plugin-syntax-async-generators@7.8.4";
        "@babel/plugin-syntax-class-properties@7.10.4" = f (sc "babel" "plugin-syntax-class-properties") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-class-properties/-/plugin-syntax-class-properties-7.10.4.tgz") "6644e6a0baa55a61f9e3231f6c9eeb6ee46c124c" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-class-properties@^7.10.4" = s."@babel/plugin-syntax-class-properties@7.10.4";
        "@babel/plugin-syntax-dynamic-import@7.8.3" = f (sc "babel" "plugin-syntax-dynamic-import") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-dynamic-import/-/plugin-syntax-dynamic-import-7.8.3.tgz") "62bf98b2da3cd21d626154fc96ee5b3cb68eacb3" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-dynamic-import@^7.8.0" = s."@babel/plugin-syntax-dynamic-import@7.8.3";
        "@babel/plugin-syntax-dynamic-import@^7.8.3" = s."@babel/plugin-syntax-dynamic-import@7.8.3";
        "@babel/plugin-syntax-export-namespace-from@7.8.3" = f (sc "babel" "plugin-syntax-export-namespace-from") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-export-namespace-from/-/plugin-syntax-export-namespace-from-7.8.3.tgz") "028964a9ba80dbc094c915c487ad7c4e7a66465a" [
          (s."@babel/helper-plugin-utils@^7.8.3")
          ];
        "@babel/plugin-syntax-export-namespace-from@^7.8.3" = s."@babel/plugin-syntax-export-namespace-from@7.8.3";
        "@babel/plugin-syntax-json-strings@7.8.3" = f (sc "babel" "plugin-syntax-json-strings") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-json-strings/-/plugin-syntax-json-strings-7.8.3.tgz") "01ca21b668cd8218c9e640cb6dd88c5412b2c96a" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-json-strings@^7.8.0" = s."@babel/plugin-syntax-json-strings@7.8.3";
        "@babel/plugin-syntax-logical-assignment-operators@7.10.4" = f (sc "babel" "plugin-syntax-logical-assignment-operators") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-logical-assignment-operators/-/plugin-syntax-logical-assignment-operators-7.10.4.tgz") "ca91ef46303530448b906652bac2e9fe9941f699" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-logical-assignment-operators@^7.10.4" = s."@babel/plugin-syntax-logical-assignment-operators@7.10.4";
        "@babel/plugin-syntax-nullish-coalescing-operator@7.8.3" = f (sc "babel" "plugin-syntax-nullish-coalescing-operator") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-nullish-coalescing-operator/-/plugin-syntax-nullish-coalescing-operator-7.8.3.tgz") "167ed70368886081f74b5c36c65a88c03b66d1a9" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-nullish-coalescing-operator@^7.8.0" = s."@babel/plugin-syntax-nullish-coalescing-operator@7.8.3";
        "@babel/plugin-syntax-numeric-separator@7.10.4" = f (sc "babel" "plugin-syntax-numeric-separator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-numeric-separator/-/plugin-syntax-numeric-separator-7.10.4.tgz") "b9b070b3e33570cd9fd07ba7fa91c0dd37b9af97" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-numeric-separator@^7.10.4" = s."@babel/plugin-syntax-numeric-separator@7.10.4";
        "@babel/plugin-syntax-object-rest-spread@7.8.3" = f (sc "babel" "plugin-syntax-object-rest-spread") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-object-rest-spread/-/plugin-syntax-object-rest-spread-7.8.3.tgz") "60e225edcbd98a640332a2e72dd3e66f1af55871" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-object-rest-spread@^7.8.0" = s."@babel/plugin-syntax-object-rest-spread@7.8.3";
        "@babel/plugin-syntax-optional-catch-binding@7.8.3" = f (sc "babel" "plugin-syntax-optional-catch-binding") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-optional-catch-binding/-/plugin-syntax-optional-catch-binding-7.8.3.tgz") "6111a265bcfb020eb9efd0fdfd7d26402b9ed6c1" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-optional-catch-binding@^7.8.0" = s."@babel/plugin-syntax-optional-catch-binding@7.8.3";
        "@babel/plugin-syntax-optional-chaining@7.8.3" = f (sc "babel" "plugin-syntax-optional-chaining") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-optional-chaining/-/plugin-syntax-optional-chaining-7.8.3.tgz") "4f69c2ab95167e0180cd5336613f8c5788f7d48a" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-optional-chaining@^7.8.0" = s."@babel/plugin-syntax-optional-chaining@7.8.3";
        "@babel/plugin-syntax-top-level-await@7.10.4" = f (sc "babel" "plugin-syntax-top-level-await") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-top-level-await/-/plugin-syntax-top-level-await-7.10.4.tgz") "4bbeb8917b54fcf768364e0a81f560e33a3ef57d" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-top-level-await@^7.10.4" = s."@babel/plugin-syntax-top-level-await@7.10.4";
        "@babel/plugin-transform-arrow-functions@7.10.4" = f (sc "babel" "plugin-transform-arrow-functions") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-arrow-functions/-/plugin-transform-arrow-functions-7.10.4.tgz") "e22960d77e697c74f41c501d44d73dbf8a6a64cd" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-arrow-functions@^7.10.4" = s."@babel/plugin-transform-arrow-functions@7.10.4";
        "@babel/plugin-transform-async-to-generator@7.10.4" = f (sc "babel" "plugin-transform-async-to-generator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-async-to-generator/-/plugin-transform-async-to-generator-7.10.4.tgz") "41a5017e49eb6f3cda9392a51eef29405b245a37" [
          (s."@babel/helper-module-imports@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-remap-async-to-generator@^7.10.4")
          ];
        "@babel/plugin-transform-async-to-generator@^7.10.4" = s."@babel/plugin-transform-async-to-generator@7.10.4";
        "@babel/plugin-transform-block-scoped-functions@7.10.4" = f (sc "babel" "plugin-transform-block-scoped-functions") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-block-scoped-functions/-/plugin-transform-block-scoped-functions-7.10.4.tgz") "1afa595744f75e43a91af73b0d998ecfe4ebc2e8" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-block-scoped-functions@^7.10.4" = s."@babel/plugin-transform-block-scoped-functions@7.10.4";
        "@babel/plugin-transform-block-scoping@7.11.1" = f (sc "babel" "plugin-transform-block-scoping") "7.11.1" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-block-scoping/-/plugin-transform-block-scoping-7.11.1.tgz") "5b7efe98852bef8d652c0b28144cd93a9e4b5215" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-block-scoping@^7.10.4" = s."@babel/plugin-transform-block-scoping@7.11.1";
        "@babel/plugin-transform-classes@7.10.4" = f (sc "babel" "plugin-transform-classes") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-classes/-/plugin-transform-classes-7.10.4.tgz") "405136af2b3e218bc4a1926228bc917ab1a0adc7" [
          (s."@babel/helper-annotate-as-pure@^7.10.4")
          (s."@babel/helper-define-map@^7.10.4")
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/helper-optimise-call-expression@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-replace-supers@^7.10.4")
          (s."@babel/helper-split-export-declaration@^7.10.4")
          (s."globals@^11.1.0")
          ];
        "@babel/plugin-transform-classes@^7.10.4" = s."@babel/plugin-transform-classes@7.10.4";
        "@babel/plugin-transform-computed-properties@7.10.4" = f (sc "babel" "plugin-transform-computed-properties") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-computed-properties/-/plugin-transform-computed-properties-7.10.4.tgz") "9ded83a816e82ded28d52d4b4ecbdd810cdfc0eb" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-computed-properties@^7.10.4" = s."@babel/plugin-transform-computed-properties@7.10.4";
        "@babel/plugin-transform-destructuring@7.10.4" = f (sc "babel" "plugin-transform-destructuring") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-destructuring/-/plugin-transform-destructuring-7.10.4.tgz") "70ddd2b3d1bea83d01509e9bb25ddb3a74fc85e5" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-destructuring@^7.10.4" = s."@babel/plugin-transform-destructuring@7.10.4";
        "@babel/plugin-transform-dotall-regex@7.10.4" = f (sc "babel" "plugin-transform-dotall-regex") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-dotall-regex/-/plugin-transform-dotall-regex-7.10.4.tgz") "469c2062105c1eb6a040eaf4fac4b488078395ee" [
          (s."@babel/helper-create-regexp-features-plugin@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-dotall-regex@^7.10.4" = s."@babel/plugin-transform-dotall-regex@7.10.4";
        "@babel/plugin-transform-dotall-regex@^7.4.4" = s."@babel/plugin-transform-dotall-regex@7.10.4";
        "@babel/plugin-transform-duplicate-keys@7.10.4" = f (sc "babel" "plugin-transform-duplicate-keys") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-duplicate-keys/-/plugin-transform-duplicate-keys-7.10.4.tgz") "697e50c9fee14380fe843d1f306b295617431e47" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-duplicate-keys@^7.10.4" = s."@babel/plugin-transform-duplicate-keys@7.10.4";
        "@babel/plugin-transform-exponentiation-operator@7.10.4" = f (sc "babel" "plugin-transform-exponentiation-operator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-exponentiation-operator/-/plugin-transform-exponentiation-operator-7.10.4.tgz") "5ae338c57f8cf4001bdb35607ae66b92d665af2e" [
          (s."@babel/helper-builder-binary-assignment-operator-visitor@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-exponentiation-operator@^7.10.4" = s."@babel/plugin-transform-exponentiation-operator@7.10.4";
        "@babel/plugin-transform-for-of@7.10.4" = f (sc "babel" "plugin-transform-for-of") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-for-of/-/plugin-transform-for-of-7.10.4.tgz") "c08892e8819d3a5db29031b115af511dbbfebae9" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-for-of@^7.10.4" = s."@babel/plugin-transform-for-of@7.10.4";
        "@babel/plugin-transform-function-name@7.10.4" = f (sc "babel" "plugin-transform-function-name") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-function-name/-/plugin-transform-function-name-7.10.4.tgz") "6a467880e0fc9638514ba369111811ddbe2644b7" [
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-function-name@^7.10.4" = s."@babel/plugin-transform-function-name@7.10.4";
        "@babel/plugin-transform-literals@7.10.4" = f (sc "babel" "plugin-transform-literals") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-literals/-/plugin-transform-literals-7.10.4.tgz") "9f42ba0841100a135f22712d0e391c462f571f3c" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-literals@^7.10.4" = s."@babel/plugin-transform-literals@7.10.4";
        "@babel/plugin-transform-member-expression-literals@7.10.4" = f (sc "babel" "plugin-transform-member-expression-literals") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-member-expression-literals/-/plugin-transform-member-expression-literals-7.10.4.tgz") "b1ec44fcf195afcb8db2c62cd8e551c881baf8b7" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-member-expression-literals@^7.10.4" = s."@babel/plugin-transform-member-expression-literals@7.10.4";
        "@babel/plugin-transform-modules-amd@7.10.5" = f (sc "babel" "plugin-transform-modules-amd") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-amd/-/plugin-transform-modules-amd-7.10.5.tgz") "1b9cddaf05d9e88b3aad339cb3e445c4f020a9b1" [
          (s."@babel/helper-module-transforms@^7.10.5")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-amd@^7.10.4" = s."@babel/plugin-transform-modules-amd@7.10.5";
        "@babel/plugin-transform-modules-commonjs@7.10.4" = f (sc "babel" "plugin-transform-modules-commonjs") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-commonjs/-/plugin-transform-modules-commonjs-7.10.4.tgz") "66667c3eeda1ebf7896d41f1f16b17105a2fbca0" [
          (s."@babel/helper-module-transforms@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-simple-access@^7.10.4")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-commonjs@^7.10.4" = s."@babel/plugin-transform-modules-commonjs@7.10.4";
        "@babel/plugin-transform-modules-systemjs@7.10.5" = f (sc "babel" "plugin-transform-modules-systemjs") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-systemjs/-/plugin-transform-modules-systemjs-7.10.5.tgz") "6270099c854066681bae9e05f87e1b9cadbe8c85" [
          (s."@babel/helper-hoist-variables@^7.10.4")
          (s."@babel/helper-module-transforms@^7.10.5")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-systemjs@^7.10.4" = s."@babel/plugin-transform-modules-systemjs@7.10.5";
        "@babel/plugin-transform-modules-umd@7.10.4" = f (sc "babel" "plugin-transform-modules-umd") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-umd/-/plugin-transform-modules-umd-7.10.4.tgz") "9a8481fe81b824654b3a0b65da3df89f3d21839e" [
          (s."@babel/helper-module-transforms@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-modules-umd@^7.10.4" = s."@babel/plugin-transform-modules-umd@7.10.4";
        "@babel/plugin-transform-named-capturing-groups-regex@7.10.4" = f (sc "babel" "plugin-transform-named-capturing-groups-regex") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-named-capturing-groups-regex/-/plugin-transform-named-capturing-groups-regex-7.10.4.tgz") "78b4d978810b6f3bcf03f9e318f2fc0ed41aecb6" [
          (s."@babel/helper-create-regexp-features-plugin@^7.10.4")
          ];
        "@babel/plugin-transform-named-capturing-groups-regex@^7.10.4" = s."@babel/plugin-transform-named-capturing-groups-regex@7.10.4";
        "@babel/plugin-transform-new-target@7.10.4" = f (sc "babel" "plugin-transform-new-target") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-new-target/-/plugin-transform-new-target-7.10.4.tgz") "9097d753cb7b024cb7381a3b2e52e9513a9c6888" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-new-target@^7.10.4" = s."@babel/plugin-transform-new-target@7.10.4";
        "@babel/plugin-transform-object-super@7.10.4" = f (sc "babel" "plugin-transform-object-super") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-object-super/-/plugin-transform-object-super-7.10.4.tgz") "d7146c4d139433e7a6526f888c667e314a093894" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-replace-supers@^7.10.4")
          ];
        "@babel/plugin-transform-object-super@^7.10.4" = s."@babel/plugin-transform-object-super@7.10.4";
        "@babel/plugin-transform-parameters@7.10.5" = f (sc "babel" "plugin-transform-parameters") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-parameters/-/plugin-transform-parameters-7.10.5.tgz") "59d339d58d0b1950435f4043e74e2510005e2c4a" [
          (s."@babel/helper-get-function-arity@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-parameters@^7.10.4" = s."@babel/plugin-transform-parameters@7.10.5";
        "@babel/plugin-transform-property-literals@7.10.4" = f (sc "babel" "plugin-transform-property-literals") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-property-literals/-/plugin-transform-property-literals-7.10.4.tgz") "f6fe54b6590352298785b83edd815d214c42e3c0" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-property-literals@^7.10.4" = s."@babel/plugin-transform-property-literals@7.10.4";
        "@babel/plugin-transform-regenerator@7.10.4" = f (sc "babel" "plugin-transform-regenerator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-regenerator/-/plugin-transform-regenerator-7.10.4.tgz") "2015e59d839074e76838de2159db421966fd8b63" [
          (s."regenerator-transform@^0.14.2")
          ];
        "@babel/plugin-transform-regenerator@^7.10.4" = s."@babel/plugin-transform-regenerator@7.10.4";
        "@babel/plugin-transform-reserved-words@7.10.4" = f (sc "babel" "plugin-transform-reserved-words") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-reserved-words/-/plugin-transform-reserved-words-7.10.4.tgz") "8f2682bcdcef9ed327e1b0861585d7013f8a54dd" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-reserved-words@^7.10.4" = s."@babel/plugin-transform-reserved-words@7.10.4";
        "@babel/plugin-transform-shorthand-properties@7.10.4" = f (sc "babel" "plugin-transform-shorthand-properties") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-shorthand-properties/-/plugin-transform-shorthand-properties-7.10.4.tgz") "9fd25ec5cdd555bb7f473e5e6ee1c971eede4dd6" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-shorthand-properties@^7.10.4" = s."@babel/plugin-transform-shorthand-properties@7.10.4";
        "@babel/plugin-transform-spread@7.11.0" = f (sc "babel" "plugin-transform-spread") "7.11.0" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-spread/-/plugin-transform-spread-7.11.0.tgz") "fa84d300f5e4f57752fe41a6d1b3c554f13f17cc" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-skip-transparent-expression-wrappers@^7.11.0")
          ];
        "@babel/plugin-transform-spread@^7.11.0" = s."@babel/plugin-transform-spread@7.11.0";
        "@babel/plugin-transform-sticky-regex@7.10.4" = f (sc "babel" "plugin-transform-sticky-regex") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-sticky-regex/-/plugin-transform-sticky-regex-7.10.4.tgz") "8f3889ee8657581130a29d9cc91d7c73b7c4a28d" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/helper-regex@^7.10.4")
          ];
        "@babel/plugin-transform-sticky-regex@^7.10.4" = s."@babel/plugin-transform-sticky-regex@7.10.4";
        "@babel/plugin-transform-template-literals@7.10.5" = f (sc "babel" "plugin-transform-template-literals") "7.10.5" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-template-literals/-/plugin-transform-template-literals-7.10.5.tgz") "78bc5d626a6642db3312d9d0f001f5e7639fde8c" [
          (s."@babel/helper-annotate-as-pure@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-template-literals@^7.10.4" = s."@babel/plugin-transform-template-literals@7.10.5";
        "@babel/plugin-transform-typeof-symbol@7.10.4" = f (sc "babel" "plugin-transform-typeof-symbol") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-typeof-symbol/-/plugin-transform-typeof-symbol-7.10.4.tgz") "9509f1a7eec31c4edbffe137c16cc33ff0bc5bfc" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-typeof-symbol@^7.10.4" = s."@babel/plugin-transform-typeof-symbol@7.10.4";
        "@babel/plugin-transform-unicode-escapes@7.10.4" = f (sc "babel" "plugin-transform-unicode-escapes") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-unicode-escapes/-/plugin-transform-unicode-escapes-7.10.4.tgz") "feae523391c7651ddac115dae0a9d06857892007" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-unicode-escapes@^7.10.4" = s."@babel/plugin-transform-unicode-escapes@7.10.4";
        "@babel/plugin-transform-unicode-regex@7.10.4" = f (sc "babel" "plugin-transform-unicode-regex") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-unicode-regex/-/plugin-transform-unicode-regex-7.10.4.tgz") "e56d71f9282fac6db09c82742055576d5e6d80a8" [
          (s."@babel/helper-create-regexp-features-plugin@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-transform-unicode-regex@^7.10.4" = s."@babel/plugin-transform-unicode-regex@7.10.4";
        "@babel/preset-env@7.11.5" = f (sc "babel" "preset-env") "7.11.5" (ir "https://registry.yarnpkg.com/@babel/preset-env/-/preset-env-7.11.5.tgz") "18cb4b9379e3e92ffea92c07471a99a2914e4272" [
          (s."@babel/compat-data@^7.11.0")
          (s."@babel/helper-compilation-targets@^7.10.4")
          (s."@babel/helper-module-imports@^7.10.4")
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-proposal-async-generator-functions@^7.10.4")
          (s."@babel/plugin-proposal-class-properties@^7.10.4")
          (s."@babel/plugin-proposal-dynamic-import@^7.10.4")
          (s."@babel/plugin-proposal-export-namespace-from@^7.10.4")
          (s."@babel/plugin-proposal-json-strings@^7.10.4")
          (s."@babel/plugin-proposal-logical-assignment-operators@^7.11.0")
          (s."@babel/plugin-proposal-nullish-coalescing-operator@^7.10.4")
          (s."@babel/plugin-proposal-numeric-separator@^7.10.4")
          (s."@babel/plugin-proposal-object-rest-spread@^7.11.0")
          (s."@babel/plugin-proposal-optional-catch-binding@^7.10.4")
          (s."@babel/plugin-proposal-optional-chaining@^7.11.0")
          (s."@babel/plugin-proposal-private-methods@^7.10.4")
          (s."@babel/plugin-proposal-unicode-property-regex@^7.10.4")
          (s."@babel/plugin-syntax-async-generators@^7.8.0")
          (s."@babel/plugin-syntax-class-properties@^7.10.4")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.0")
          (s."@babel/plugin-syntax-export-namespace-from@^7.8.3")
          (s."@babel/plugin-syntax-json-strings@^7.8.0")
          (s."@babel/plugin-syntax-logical-assignment-operators@^7.10.4")
          (s."@babel/plugin-syntax-nullish-coalescing-operator@^7.8.0")
          (s."@babel/plugin-syntax-numeric-separator@^7.10.4")
          (s."@babel/plugin-syntax-object-rest-spread@^7.8.0")
          (s."@babel/plugin-syntax-optional-catch-binding@^7.8.0")
          (s."@babel/plugin-syntax-optional-chaining@^7.8.0")
          (s."@babel/plugin-syntax-top-level-await@^7.10.4")
          (s."@babel/plugin-transform-arrow-functions@^7.10.4")
          (s."@babel/plugin-transform-async-to-generator@^7.10.4")
          (s."@babel/plugin-transform-block-scoped-functions@^7.10.4")
          (s."@babel/plugin-transform-block-scoping@^7.10.4")
          (s."@babel/plugin-transform-classes@^7.10.4")
          (s."@babel/plugin-transform-computed-properties@^7.10.4")
          (s."@babel/plugin-transform-destructuring@^7.10.4")
          (s."@babel/plugin-transform-dotall-regex@^7.10.4")
          (s."@babel/plugin-transform-duplicate-keys@^7.10.4")
          (s."@babel/plugin-transform-exponentiation-operator@^7.10.4")
          (s."@babel/plugin-transform-for-of@^7.10.4")
          (s."@babel/plugin-transform-function-name@^7.10.4")
          (s."@babel/plugin-transform-literals@^7.10.4")
          (s."@babel/plugin-transform-member-expression-literals@^7.10.4")
          (s."@babel/plugin-transform-modules-amd@^7.10.4")
          (s."@babel/plugin-transform-modules-commonjs@^7.10.4")
          (s."@babel/plugin-transform-modules-systemjs@^7.10.4")
          (s."@babel/plugin-transform-modules-umd@^7.10.4")
          (s."@babel/plugin-transform-named-capturing-groups-regex@^7.10.4")
          (s."@babel/plugin-transform-new-target@^7.10.4")
          (s."@babel/plugin-transform-object-super@^7.10.4")
          (s."@babel/plugin-transform-parameters@^7.10.4")
          (s."@babel/plugin-transform-property-literals@^7.10.4")
          (s."@babel/plugin-transform-regenerator@^7.10.4")
          (s."@babel/plugin-transform-reserved-words@^7.10.4")
          (s."@babel/plugin-transform-shorthand-properties@^7.10.4")
          (s."@babel/plugin-transform-spread@^7.11.0")
          (s."@babel/plugin-transform-sticky-regex@^7.10.4")
          (s."@babel/plugin-transform-template-literals@^7.10.4")
          (s."@babel/plugin-transform-typeof-symbol@^7.10.4")
          (s."@babel/plugin-transform-unicode-escapes@^7.10.4")
          (s."@babel/plugin-transform-unicode-regex@^7.10.4")
          (s."@babel/preset-modules@^0.1.3")
          (s."@babel/types@^7.11.5")
          (s."browserslist@^4.12.0")
          (s."core-js-compat@^3.6.2")
          (s."invariant@^2.2.2")
          (s."levenary@^1.1.1")
          (s."semver@^5.5.0")
          ];
        "@babel/preset-env@^7.8.4" = s."@babel/preset-env@7.11.5";
        "@babel/preset-modules@0.1.4" = f (sc "babel" "preset-modules") "0.1.4" (ir "https://registry.yarnpkg.com/@babel/preset-modules/-/preset-modules-0.1.4.tgz") "362f2b68c662842970fdb5e254ffc8fc1c2e415e" [
          (s."@babel/helper-plugin-utils@^7.0.0")
          (s."@babel/plugin-proposal-unicode-property-regex@^7.4.4")
          (s."@babel/plugin-transform-dotall-regex@^7.4.4")
          (s."@babel/types@^7.4.4")
          (s."esutils@^2.0.2")
          ];
        "@babel/preset-modules@^0.1.3" = s."@babel/preset-modules@0.1.4";
        "@babel/runtime@7.11.2" = f (sc "babel" "runtime") "7.11.2" (ir "https://registry.yarnpkg.com/@babel/runtime/-/runtime-7.11.2.tgz") "f549c13c754cc40b87644b9fa9f09a6a95fe0736" [
          (s."regenerator-runtime@^0.13.4")
          ];
        "@babel/runtime@^7.8.4" = s."@babel/runtime@7.11.2";
        "@babel/template@7.10.4" = f (sc "babel" "template") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/template/-/template-7.10.4.tgz") "3251996c4200ebc71d1a8fc405fba940f36ba278" [
          (s."@babel/code-frame@^7.10.4")
          (s."@babel/parser@^7.10.4")
          (s."@babel/types@^7.10.4")
          ];
        "@babel/template@^7.10.4" = s."@babel/template@7.10.4";
        "@babel/traverse@7.11.5" = f (sc "babel" "traverse") "7.11.5" (ir "https://registry.yarnpkg.com/@babel/traverse/-/traverse-7.11.5.tgz") "be777b93b518eb6d76ee2e1ea1d143daa11e61c3" [
          (s."@babel/code-frame@^7.10.4")
          (s."@babel/generator@^7.11.5")
          (s."@babel/helper-function-name@^7.10.4")
          (s."@babel/helper-split-export-declaration@^7.11.0")
          (s."@babel/parser@^7.11.5")
          (s."@babel/types@^7.11.5")
          (s."debug@^4.1.0")
          (s."globals@^11.1.0")
          (s."lodash@^4.17.19")
          ];
        "@babel/traverse@^7.10.4" = s."@babel/traverse@7.11.5";
        "@babel/traverse@^7.11.5" = s."@babel/traverse@7.11.5";
        "@babel/types@7.11.5" = f (sc "babel" "types") "7.11.5" (ir "https://registry.yarnpkg.com/@babel/types/-/types-7.11.5.tgz") "d9de577d01252d77c6800cee039ee64faf75662d" [
          (s."@babel/helper-validator-identifier@^7.10.4")
          (s."lodash@^4.17.19")
          (s."to-fast-properties@^2.0.0")
          ];
        "@babel/types@^7.10.4" = s."@babel/types@7.11.5";
        "@babel/types@^7.10.5" = s."@babel/types@7.11.5";
        "@babel/types@^7.11.0" = s."@babel/types@7.11.5";
        "@babel/types@^7.11.5" = s."@babel/types@7.11.5";
        "@babel/types@^7.4.4" = s."@babel/types@7.11.5";
        "@mdi/svg@5.5.55" = f (sc "mdi" "svg") "5.5.55" (ir "https://registry.yarnpkg.com/@mdi/svg/-/svg-5.5.55.tgz") "758f443ed1288c13833e1d6e8008d36cb1a6e703" [];
        "@mdi/svg@^5.0.45" = s."@mdi/svg@5.5.55";
        "@types/json-schema@7.0.6" = f (sc "types" "json-schema") "7.0.6" (ir "https://registry.yarnpkg.com/@types/json-schema/-/json-schema-7.0.6.tgz") "f4c7ec43e81b319a9815115031709f26987891f0" [];
        "@types/json-schema@^7.0.5" = s."@types/json-schema@7.0.6";
        "@webassemblyjs/ast@1.9.0" = f (sc "webassemblyjs" "ast") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/ast/-/ast-1.9.0.tgz") "bd850604b4042459a5a41cd7d338cbed695ed964" [
          (s."@webassemblyjs/helper-module-context@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/wast-parser@1.9.0")
          ];
        "@webassemblyjs/floating-point-hex-parser@1.9.0" = f (sc "webassemblyjs" "floating-point-hex-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/floating-point-hex-parser/-/floating-point-hex-parser-1.9.0.tgz") "3c3d3b271bddfc84deb00f71344438311d52ffb4" [];
        "@webassemblyjs/helper-api-error@1.9.0" = f (sc "webassemblyjs" "helper-api-error") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-api-error/-/helper-api-error-1.9.0.tgz") "203f676e333b96c9da2eeab3ccef33c45928b6a2" [];
        "@webassemblyjs/helper-buffer@1.9.0" = f (sc "webassemblyjs" "helper-buffer") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-buffer/-/helper-buffer-1.9.0.tgz") "a1442d269c5feb23fcbc9ef759dac3547f29de00" [];
        "@webassemblyjs/helper-code-frame@1.9.0" = f (sc "webassemblyjs" "helper-code-frame") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-code-frame/-/helper-code-frame-1.9.0.tgz") "647f8892cd2043a82ac0c8c5e75c36f1d9159f27" [
          (s."@webassemblyjs/wast-printer@1.9.0")
          ];
        "@webassemblyjs/helper-fsm@1.9.0" = f (sc "webassemblyjs" "helper-fsm") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-fsm/-/helper-fsm-1.9.0.tgz") "c05256b71244214671f4b08ec108ad63b70eddb8" [];
        "@webassemblyjs/helper-module-context@1.9.0" = f (sc "webassemblyjs" "helper-module-context") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-module-context/-/helper-module-context-1.9.0.tgz") "25d8884b76839871a08a6c6f806c3979ef712f07" [];
        "@webassemblyjs/helper-wasm-bytecode@1.9.0" = f (sc "webassemblyjs" "helper-wasm-bytecode") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-bytecode/-/helper-wasm-bytecode-1.9.0.tgz") "4fed8beac9b8c14f8c58b70d124d549dd1fe5790" [];
        "@webassemblyjs/helper-wasm-section@1.9.0" = f (sc "webassemblyjs" "helper-wasm-section") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-section/-/helper-wasm-section-1.9.0.tgz") "5a4138d5a6292ba18b04c5ae49717e4167965346" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          ];
        "@webassemblyjs/ieee754@1.9.0" = f (sc "webassemblyjs" "ieee754") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/ieee754/-/ieee754-1.9.0.tgz") "15c7a0fbaae83fb26143bbacf6d6df1702ad39e4" [
          (s."@xtuc/ieee754@^1.2.0")
          ];
        "@webassemblyjs/leb128@1.9.0" = f (sc "webassemblyjs" "leb128") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/leb128/-/leb128-1.9.0.tgz") "f19ca0b76a6dc55623a09cffa769e838fa1e1c95" [
          (s."@xtuc/long@4.2.2")
          ];
        "@webassemblyjs/utf8@1.9.0" = f (sc "webassemblyjs" "utf8") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/utf8/-/utf8-1.9.0.tgz") "04d33b636f78e6a6813227e82402f7637b6229ab" [];
        "@webassemblyjs/wasm-edit@1.9.0" = f (sc "webassemblyjs" "wasm-edit") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-edit/-/wasm-edit-1.9.0.tgz") "3fe6d79d3f0f922183aa86002c42dd256cfee9cf" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/helper-wasm-section@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          (s."@webassemblyjs/wasm-opt@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          (s."@webassemblyjs/wast-printer@1.9.0")
          ];
        "@webassemblyjs/wasm-gen@1.9.0" = f (sc "webassemblyjs" "wasm-gen") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-gen/-/wasm-gen-1.9.0.tgz") "50bc70ec68ded8e2763b01a1418bf43491a7a49c" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/ieee754@1.9.0")
          (s."@webassemblyjs/leb128@1.9.0")
          (s."@webassemblyjs/utf8@1.9.0")
          ];
        "@webassemblyjs/wasm-opt@1.9.0" = f (sc "webassemblyjs" "wasm-opt") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-opt/-/wasm-opt-1.9.0.tgz") "2211181e5b31326443cc8112eb9f0b9028721a61" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          ];
        "@webassemblyjs/wasm-parser@1.9.0" = f (sc "webassemblyjs" "wasm-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-parser/-/wasm-parser-1.9.0.tgz") "9d48e44826df4a6598294aa6c87469d642fff65e" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-api-error@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/ieee754@1.9.0")
          (s."@webassemblyjs/leb128@1.9.0")
          (s."@webassemblyjs/utf8@1.9.0")
          ];
        "@webassemblyjs/wast-parser@1.9.0" = f (sc "webassemblyjs" "wast-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wast-parser/-/wast-parser-1.9.0.tgz") "3031115d79ac5bd261556cecc3fa90a3ef451914" [
          (s."@webassemblyjs/floating-point-hex-parser@1.9.0")
          (s."@webassemblyjs/helper-api-error@1.9.0")
          (s."@webassemblyjs/helper-code-frame@1.9.0")
          (s."@webassemblyjs/helper-fsm@1.9.0")
          (s."@xtuc/long@4.2.2")
          ];
        "@webassemblyjs/wast-printer@1.9.0" = f (sc "webassemblyjs" "wast-printer") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wast-printer/-/wast-printer-1.9.0.tgz") "4935d54c85fef637b00ce9f52377451d00d47899" [
          (s."@xtuc/long@4.2.2")
          ];
        "@xtuc/ieee754@1.2.0" = f (sc "xtuc" "ieee754") "1.2.0" (ir "https://registry.yarnpkg.com/@xtuc/ieee754/-/ieee754-1.2.0.tgz") "eef014a3145ae477a1cbc00cd1e552336dceb790" [];
        "@xtuc/ieee754@^1.2.0" = s."@xtuc/ieee754@1.2.0";
        "@xtuc/long@4.2.2" = f (sc "xtuc" "long") "4.2.2" (ir "https://registry.yarnpkg.com/@xtuc/long/-/long-4.2.2.tgz") "d291c6a4e97989b5c61d9acf396ae4fe133a718d" [];
        "accepts@1.3.7" = f "accepts" "1.3.7" y "531bc726517a3b2b41f850021c6cc15eaab507cd" [
          (s."mime-types@~2.1.24")
          (s."negotiator@0.6.2")
          ];
        "accepts@~1.3.7" = s."accepts@1.3.7";
        "acorn@6.4.1" = f "acorn" "6.4.1" y "531e58ba3f51b9dacb9a6646ca4debf5b14ca474" [];
        "acorn@^6.4.1" = s."acorn@6.4.1";
        "agent-base@4.3.0" = f "agent-base" "4.3.0" y "8165f01c436009bccad0b1d122f05ed770efc6ee" [
          (s."es6-promisify@^5.0.0")
          ];
        "agent-base@^4.3.0" = s."agent-base@4.3.0";
        "ajv-errors@1.0.1" = f "ajv-errors" "1.0.1" y "f35986aceb91afadec4102fbd85014950cefa64d" [];
        "ajv-errors@^1.0.0" = s."ajv-errors@1.0.1";
        "ajv-keywords@3.5.2" = f "ajv-keywords" "3.5.2" y "31f29da5ab6e00d1c2d329acf7b5929614d5014d" [];
        "ajv-keywords@^3.1.0" = s."ajv-keywords@3.5.2";
        "ajv-keywords@^3.4.1" = s."ajv-keywords@3.5.2";
        "ajv-keywords@^3.5.2" = s."ajv-keywords@3.5.2";
        "ajv@6.12.4" = f "ajv" "6.12.4" y "0614facc4522127fa713445c6bfd3ebd376e2234" [
          (s."fast-deep-equal@^3.1.1")
          (s."fast-json-stable-stringify@^2.0.0")
          (s."json-schema-traverse@^0.4.1")
          (s."uri-js@^4.2.2")
          ];
        "ajv@^6.1.0" = s."ajv@6.12.4";
        "ajv@^6.10.2" = s."ajv@6.12.4";
        "ajv@^6.12.3" = s."ajv@6.12.4";
        "ajv@^6.12.4" = s."ajv@6.12.4";
        "ansi-regex@4.1.0" = f "ansi-regex" "4.1.0" y "8b9f8f08cf1acb843756a839ca8c7e3168c51997" [];
        "ansi-regex@^4.1.0" = s."ansi-regex@4.1.0";
        "ansi-styles@3.2.1" = f "ansi-styles" "3.2.1" y "41fbb20243e50b12be0f04b8dedbf07520ce841d" [
          (s."color-convert@^1.9.0")
          ];
        "ansi-styles@4.3.0" = f "ansi-styles" "4.3.0" y "edd803628ae71c04c85ae7a0906edad34b648937" [
          (s."color-convert@^2.0.1")
          ];
        "ansi-styles@^3.2.0" = s."ansi-styles@3.2.1";
        "ansi-styles@^3.2.1" = s."ansi-styles@3.2.1";
        "ansi-styles@^4.1.0" = s."ansi-styles@4.3.0";
        "anymatch@2.0.0" = f "anymatch" "2.0.0" y "bcb24b4f37934d9aa7ac17b4adaf89e7c76ef2eb" [
          (s."micromatch@^3.1.4")
          (s."normalize-path@^2.1.1")
          ];
        "anymatch@3.1.1" = f "anymatch" "3.1.1" y "c55ecf02185e2469259399310c173ce31233b142" [
          (s."normalize-path@^3.0.0")
          (s."picomatch@^2.0.4")
          ];
        "anymatch@^2.0.0" = s."anymatch@2.0.0";
        "anymatch@~3.1.1" = s."anymatch@3.1.1";
        "aproba@1.2.0" = f "aproba" "1.2.0" y "6802e6264efd18c790a1b0d517f0f2627bf2c94a" [];
        "aproba@^1.1.1" = s."aproba@1.2.0";
        "arr-diff@4.0.0" = f "arr-diff" "4.0.0" y "d6461074febfec71e7e15235761a329a5dc7c520" [];
        "arr-diff@^4.0.0" = s."arr-diff@4.0.0";
        "arr-flatten@1.1.0" = f "arr-flatten" "1.1.0" y "36048bbff4e7b47e136644316c99669ea5ae91f1" [];
        "arr-flatten@^1.1.0" = s."arr-flatten@1.1.0";
        "arr-union@3.1.0" = f "arr-union" "3.1.0" y "e39b09aea9def866a8f206e288af63919bae39c4" [];
        "arr-union@^3.1.0" = s."arr-union@3.1.0";
        "array-flatten@1.1.1" = f "array-flatten" "1.1.1" y "9a5f699051b1e7073328f2a008968b64ea2955d2" [];
        "array-unique@0.3.2" = f "array-unique" "0.3.2" y "a894b75d4bc4f6cd679ef3244a9fd8f46ae2d428" [];
        "array-unique@^0.3.2" = s."array-unique@0.3.2";
        "asn1.js@5.4.1" = f "asn1.js" "5.4.1" y "11a980b84ebb91781ce35b0fdc2ee294e3783f07" [
          (s."bn.js@^4.0.0")
          (s."inherits@^2.0.1")
          (s."minimalistic-assert@^1.0.0")
          (s."safer-buffer@^2.1.0")
          ];
        "asn1.js@^5.2.0" = s."asn1.js@5.4.1";
        "asn1@0.2.4" = f "asn1" "0.2.4" y "8d2475dfab553bb33e77b54e59e880bb8ce23136" [
          (s."safer-buffer@~2.1.0")
          ];
        "asn1@~0.2.3" = s."asn1@0.2.4";
        "assert-plus@1.0.0" = f "assert-plus" "1.0.0" y "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525" [];
        "assert-plus@^1.0.0" = s."assert-plus@1.0.0";
        "assert@1.5.0" = f "assert" "1.5.0" y "55c109aaf6e0aefdb3dc4b71240c70bf574b18eb" [
          (s."object-assign@^4.1.1")
          (s."util@0.10.3")
          ];
        "assert@^1.1.1" = s."assert@1.5.0";
        "assign-symbols@1.0.0" = f "assign-symbols" "1.0.0" y "59667f41fadd4f20ccbc2bb96b8d4f7f78ec0367" [];
        "assign-symbols@^1.0.0" = s."assign-symbols@1.0.0";
        "async-each@1.0.3" = f "async-each" "1.0.3" y "b727dbf87d7651602f06f4d4ac387f47d91b0cbf" [];
        "async-each@^1.0.1" = s."async-each@1.0.3";
        "async-limiter@1.0.1" = f "async-limiter" "1.0.1" y "dd379e94f0db8310b08291f9d64c3209766617fd" [];
        "async-limiter@~1.0.0" = s."async-limiter@1.0.1";
        "asynckit@0.4.0" = f "asynckit" "0.4.0" y "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79" [];
        "asynckit@^0.4.0" = s."asynckit@0.4.0";
        "atob@2.1.2" = f "atob" "2.1.2" y "6d9517eb9e030d2436666651e86bd9f6f13533c9" [];
        "atob@^2.1.2" = s."atob@2.1.2";
        "autoprefixer@8.6.5" = f "autoprefixer" "8.6.5" y "343f3d193ed568b3208e00117a1b96eb691d4ee9" [
          (s."browserslist@^3.2.8")
          (s."caniuse-lite@^1.0.30000864")
          (s."normalize-range@^0.1.2")
          (s."num2fraction@^1.2.2")
          (s."postcss@^6.0.23")
          (s."postcss-value-parser@^3.2.3")
          ];
        "autoprefixer@^8.6.3" = s."autoprefixer@8.6.5";
        "aws-sign2@0.7.0" = f "aws-sign2" "0.7.0" y "b46e890934a9591f2d2f6f86d7e6a9f1b3fe76a8" [];
        "aws-sign2@~0.7.0" = s."aws-sign2@0.7.0";
        "aws4@1.10.1" = f "aws4" "1.10.1" y "e1e82e4f3e999e2cfd61b161280d16a111f86428" [];
        "aws4@^1.8.0" = s."aws4@1.10.1";
        "babel-loader@8.1.0" = f "babel-loader" "8.1.0" y "c611d5112bd5209abe8b9fa84c3e4da25275f1c3" [
          (s."find-cache-dir@^2.1.0")
          (s."loader-utils@^1.4.0")
          (s."mkdirp@^0.5.3")
          (s."pify@^4.0.1")
          (s."schema-utils@^2.6.5")
          ];
        "babel-loader@^8.0.6" = s."babel-loader@8.1.0";
        "babel-plugin-dynamic-import-node@2.3.3" = f "babel-plugin-dynamic-import-node" "2.3.3" y "84fda19c976ec5c6defef57f9427b3def66e17a3" [
          (s."object.assign@^4.1.0")
          ];
        "babel-plugin-dynamic-import-node@^2.3.3" = s."babel-plugin-dynamic-import-node@2.3.3";
        "babel-runtime@6.18.0" = f "babel-runtime" "6.18.0" y "0f4177ffd98492ef13b9f823e9994a02584c9078" [
          (s."core-js@^2.4.0")
          (s."regenerator-runtime@^0.9.5")
          ];
        "balanced-match@1.0.0" = f "balanced-match" "1.0.0" y "89b4d199ab2bee49de164ea02b89ce462d71b767" [];
        "balanced-match@^1.0.0" = s."balanced-match@1.0.0";
        "base64-js@1.3.1" = f "base64-js" "1.3.1" y "58ece8cb75dd07e71ed08c736abc5fac4dbf8df1" [];
        "base64-js@^1.0.2" = s."base64-js@1.3.1";
        "base@0.11.2" = f "base" "0.11.2" y "7bde5ced145b6d551a90db87f83c558b4eb48a8f" [
          (s."cache-base@^1.0.1")
          (s."class-utils@^0.3.5")
          (s."component-emitter@^1.2.1")
          (s."define-property@^1.0.0")
          (s."isobject@^3.0.1")
          (s."mixin-deep@^1.2.0")
          (s."pascalcase@^0.1.1")
          ];
        "base@^0.11.1" = s."base@0.11.2";
        "bcrypt-pbkdf@1.0.2" = f "bcrypt-pbkdf" "1.0.2" y "a4301d389b6a43f9b67ff3ca11a3f6637e360e9e" [
          (s."tweetnacl@^0.14.3")
          ];
        "bcrypt-pbkdf@^1.0.0" = s."bcrypt-pbkdf@1.0.2";
        "big.js@5.2.2" = f "big.js" "5.2.2" y "65f0af382f578bcdc742bd9c281e9cb2d7768328" [];
        "big.js@^5.2.2" = s."big.js@5.2.2";
        "binary-extensions@1.13.1" = f "binary-extensions" "1.13.1" y "598afe54755b2868a5330d2aff9d4ebb53209b65" [];
        "binary-extensions@2.1.0" = f "binary-extensions" "2.1.0" y "30fa40c9e7fe07dbc895678cd287024dea241dd9" [];
        "binary-extensions@^1.0.0" = s."binary-extensions@1.13.1";
        "binary-extensions@^2.0.0" = s."binary-extensions@2.1.0";
        "binary@0.3.0" = f "binary" "0.3.0" y "9f60553bc5ce8c3386f3b553cff47462adecaa79" [
          (s."buffers@~0.1.1")
          (s."chainsaw@~0.1.0")
          ];
        "binary@^0.3.0" = s."binary@0.3.0";
        "bindings@1.5.0" = f "bindings" "1.5.0" y "10353c9e945334bc0511a6d90b38fbc7c9c504df" [
          (s."file-uri-to-path@1.0.0")
          ];
        "bindings@^1.5.0" = s."bindings@1.5.0";
        "binwrap@0.2.2" = f "binwrap" "0.2.2" y "7d1ea74b28332f18dfdc75548aef993041ffafc9" [
          (s."mustache@^3.0.1")
          (s."request@^2.88.0")
          (s."request-promise@^4.2.4")
          (s."tar@^4.4.10")
          (s."unzip-stream@^0.3.0")
          ];
        "binwrap@^0.2.2" = s."binwrap@0.2.2";
        "bluebird@3.7.2" = f "bluebird" "3.7.2" y "9f229c15be272454ffa973ace0dbee79a1b0c36f" [];
        "bluebird@^3.5.0" = s."bluebird@3.7.2";
        "bluebird@^3.5.5" = s."bluebird@3.7.2";
        "bn.js@4.11.9" = f "bn.js" "4.11.9" y "26d556829458f9d1e81fc48952493d0ba3507828" [];
        "bn.js@5.1.3" = f "bn.js" "5.1.3" y "beca005408f642ebebea80b042b4d18d2ac0ee6b" [];
        "bn.js@^4.0.0" = s."bn.js@4.11.9";
        "bn.js@^4.1.0" = s."bn.js@4.11.9";
        "bn.js@^4.4.0" = s."bn.js@4.11.9";
        "bn.js@^5.1.1" = s."bn.js@5.1.3";
        "body-parser@1.19.0" = f "body-parser" "1.19.0" y "96b2709e57c9c4e09a6fd66a8fd979844f69f08a" [
          (s."bytes@3.1.0")
          (s."content-type@~1.0.4")
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."http-errors@1.7.2")
          (s."iconv-lite@0.4.24")
          (s."on-finished@~2.3.0")
          (s."qs@6.7.0")
          (s."raw-body@2.4.0")
          (s."type-is@~1.6.17")
          ];
        "brace-expansion@1.1.11" = f "brace-expansion" "1.1.11" y "3c7fcbf529d87226f3d2f52b966ff5271eb441dd" [
          (s."balanced-match@^1.0.0")
          (s."concat-map@0.0.1")
          ];
        "brace-expansion@^1.1.7" = s."brace-expansion@1.1.11";
        "braces@2.3.2" = f "braces" "2.3.2" y "5979fd3f14cd531565e5fa2df1abfff1dfaee729" [
          (s."arr-flatten@^1.1.0")
          (s."array-unique@^0.3.2")
          (s."extend-shallow@^2.0.1")
          (s."fill-range@^4.0.0")
          (s."isobject@^3.0.1")
          (s."repeat-element@^1.1.2")
          (s."snapdragon@^0.8.1")
          (s."snapdragon-node@^2.0.1")
          (s."split-string@^3.0.2")
          (s."to-regex@^3.0.1")
          ];
        "braces@3.0.2" = f "braces" "3.0.2" y "3454e1a462ee8d599e236df336cd9ea4f8afe107" [
          (s."fill-range@^7.0.1")
          ];
        "braces@^2.3.1" = s."braces@2.3.2";
        "braces@^2.3.2" = s."braces@2.3.2";
        "braces@~3.0.2" = s."braces@3.0.2";
        "brorand@1.1.0" = f "brorand" "1.1.0" y "12c25efe40a45e3c323eb8675a0a0ce57b22371f" [];
        "brorand@^1.0.1" = s."brorand@1.1.0";
        "browserify-aes@1.2.0" = f "browserify-aes" "1.2.0" y "326734642f403dabc3003209853bb70ad428ef48" [
          (s."buffer-xor@^1.0.3")
          (s."cipher-base@^1.0.0")
          (s."create-hash@^1.1.0")
          (s."evp_bytestokey@^1.0.3")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "browserify-aes@^1.0.0" = s."browserify-aes@1.2.0";
        "browserify-aes@^1.0.4" = s."browserify-aes@1.2.0";
        "browserify-cipher@1.0.1" = f "browserify-cipher" "1.0.1" y "8d6474c1b870bfdabcd3bcfcc1934a10e94f15f0" [
          (s."browserify-aes@^1.0.4")
          (s."browserify-des@^1.0.0")
          (s."evp_bytestokey@^1.0.0")
          ];
        "browserify-cipher@^1.0.0" = s."browserify-cipher@1.0.1";
        "browserify-des@1.0.2" = f "browserify-des" "1.0.2" y "3af4f1f59839403572f1c66204375f7a7f703e9c" [
          (s."cipher-base@^1.0.1")
          (s."des.js@^1.0.0")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "browserify-des@^1.0.0" = s."browserify-des@1.0.2";
        "browserify-rsa@4.0.1" = f "browserify-rsa" "4.0.1" y "21e0abfaf6f2029cf2fafb133567a701d4135524" [
          (s."bn.js@^4.1.0")
          (s."randombytes@^2.0.1")
          ];
        "browserify-rsa@^4.0.0" = s."browserify-rsa@4.0.1";
        "browserify-rsa@^4.0.1" = s."browserify-rsa@4.0.1";
        "browserify-sign@4.2.1" = f "browserify-sign" "4.2.1" y "eaf4add46dd54be3bb3b36c0cf15abbeba7956c3" [
          (s."bn.js@^5.1.1")
          (s."browserify-rsa@^4.0.1")
          (s."create-hash@^1.2.0")
          (s."create-hmac@^1.1.7")
          (s."elliptic@^6.5.3")
          (s."inherits@^2.0.4")
          (s."parse-asn1@^5.1.5")
          (s."readable-stream@^3.6.0")
          (s."safe-buffer@^5.2.0")
          ];
        "browserify-sign@^4.0.0" = s."browserify-sign@4.2.1";
        "browserify-zlib@0.2.0" = f "browserify-zlib" "0.2.0" y "2869459d9aa3be245fe8fe2ca1f46e2e7f54d73f" [
          (s."pako@~1.0.5")
          ];
        "browserify-zlib@^0.2.0" = s."browserify-zlib@0.2.0";
        "browserslist@3.2.8" = f "browserslist" "3.2.8" y "b0005361d6471f0f5952797a76fc985f1f978fc6" [
          (s."caniuse-lite@^1.0.30000844")
          (s."electron-to-chromium@^1.3.47")
          ];
        "browserslist@4.14.0" = f "browserslist" "4.14.0" y "2908951abfe4ec98737b72f34c3bcedc8d43b000" [
          (s."caniuse-lite@^1.0.30001111")
          (s."electron-to-chromium@^1.3.523")
          (s."escalade@^3.0.2")
          (s."node-releases@^1.1.60")
          ];
        "browserslist@^3.2.8" = s."browserslist@3.2.8";
        "browserslist@^4.12.0" = s."browserslist@4.14.0";
        "browserslist@^4.8.5" = s."browserslist@4.14.0";
        "buffer-crc32@0.2.13" = f "buffer-crc32" "0.2.13" y "0d333e3f00eac50aa1454abd30ef8c2a5d9a7242" [];
        "buffer-crc32@~0.2.3" = s."buffer-crc32@0.2.13";
        "buffer-from@1.1.1" = f "buffer-from" "1.1.1" y "32713bc028f75c02fdb710d7c7bcec1f2c6070ef" [];
        "buffer-from@^1.0.0" = s."buffer-from@1.1.1";
        "buffer-xor@1.0.3" = f "buffer-xor" "1.0.3" y "26e61ed1422fb70dd42e6e36729ed51d855fe8d9" [];
        "buffer-xor@^1.0.3" = s."buffer-xor@1.0.3";
        "buffer@4.9.2" = f "buffer" "4.9.2" y "230ead344002988644841ab0244af8c44bbe3ef8" [
          (s."base64-js@^1.0.2")
          (s."ieee754@^1.1.4")
          (s."isarray@^1.0.0")
          ];
        "buffer@^4.3.0" = s."buffer@4.9.2";
        "buffers@0.1.1" = f "buffers" "0.1.1" y "b24579c3bed4d6d396aeee6d9a8ae7f5482ab7bb" [];
        "buffers@~0.1.1" = s."buffers@0.1.1";
        "builtin-status-codes@3.0.0" = f "builtin-status-codes" "3.0.0" y "85982878e21b98e1c66425e03d0174788f569ee8" [];
        "builtin-status-codes@^3.0.0" = s."builtin-status-codes@3.0.0";
        "bytes@3.1.0" = f "bytes" "3.1.0" y "f6cf7933a360e0588fa9fde85651cdc7f805d1f6" [];
        "cacache@12.0.4" = f "cacache" "12.0.4" y "668bcbd105aeb5f1d92fe25570ec9525c8faa40c" [
          (s."bluebird@^3.5.5")
          (s."chownr@^1.1.1")
          (s."figgy-pudding@^3.5.1")
          (s."glob@^7.1.4")
          (s."graceful-fs@^4.1.15")
          (s."infer-owner@^1.0.3")
          (s."lru-cache@^5.1.1")
          (s."mississippi@^3.0.0")
          (s."mkdirp@^0.5.1")
          (s."move-concurrently@^1.0.1")
          (s."promise-inflight@^1.0.1")
          (s."rimraf@^2.6.3")
          (s."ssri@^6.0.1")
          (s."unique-filename@^1.1.1")
          (s."y18n@^4.0.0")
          ];
        "cacache@^12.0.2" = s."cacache@12.0.4";
        "cache-base@1.0.1" = f "cache-base" "1.0.1" y "0a7f46416831c8b662ee36fe4e7c59d76f666ab2" [
          (s."collection-visit@^1.0.0")
          (s."component-emitter@^1.2.1")
          (s."get-value@^2.0.6")
          (s."has-value@^1.0.0")
          (s."isobject@^3.0.1")
          (s."set-value@^2.0.0")
          (s."to-object-path@^0.3.0")
          (s."union-value@^1.0.0")
          (s."unset-value@^1.0.0")
          ];
        "cache-base@^1.0.1" = s."cache-base@1.0.1";
        "camelcase@5.3.1" = f "camelcase" "5.3.1" y "e3c9b31569e106811df242f715725a1f4c494320" [];
        "camelcase@^5.0.0" = s."camelcase@5.3.1";
        "caniuse-lite@1.0.30001123" = f "caniuse-lite" "1.0.30001123" y "7b981d81382ab2c8fd062f3e6439215e8c503c22" [];
        "caniuse-lite@^1.0.30000844" = s."caniuse-lite@1.0.30001123";
        "caniuse-lite@^1.0.30000864" = s."caniuse-lite@1.0.30001123";
        "caniuse-lite@^1.0.30001111" = s."caniuse-lite@1.0.30001123";
        "caseless@0.12.0" = f "caseless" "0.12.0" y "1b681c21ff84033c826543090689420d187151dc" [];
        "caseless@~0.12.0" = s."caseless@0.12.0";
        "chainsaw@0.1.0" = f "chainsaw" "0.1.0" y "5eab50b28afe58074d0d58291388828b5e5fbc98" [
          (s."traverse@>=0.3.0 <0.4")
          ];
        "chainsaw@~0.1.0" = s."chainsaw@0.1.0";
        "chalk@2.4.2" = f "chalk" "2.4.2" y "cd42541677a54333cf541a49108c1432b44c9424" [
          (s."ansi-styles@^3.2.1")
          (s."escape-string-regexp@^1.0.5")
          (s."supports-color@^5.3.0")
          ];
        "chalk@3.0.0" = f "chalk" "3.0.0" y "3f73c2bf526591f574cc492c51e2456349f844e4" [
          (s."ansi-styles@^4.1.0")
          (s."supports-color@^7.1.0")
          ];
        "chalk@^2.0.0" = s."chalk@2.4.2";
        "chalk@^2.4.1" = s."chalk@2.4.2";
        "chalk@^2.4.2" = s."chalk@2.4.2";
        "chalk@^3.0.0" = s."chalk@3.0.0";
        "child-process-promise@2.2.1" = f "child-process-promise" "2.2.1" y "4730a11ef610fad450b8f223c79d31d7bdad8074" [
          (s."cross-spawn@^4.0.2")
          (s."node-version@^1.0.0")
          (s."promise-polyfill@^6.0.1")
          ];
        "child-process-promise@^2.2.1" = s."child-process-promise@2.2.1";
        "chokidar@2.1.8" = f "chokidar" "2.1.8" y "804b3a7b6a99358c3c5c61e71d8728f041cff917" [
          (s."anymatch@^2.0.0")
          (s."async-each@^1.0.1")
          (s."braces@^2.3.2")
          (s."glob-parent@^3.1.0")
          (s."inherits@^2.0.3")
          (s."is-binary-path@^1.0.0")
          (s."is-glob@^4.0.0")
          (s."normalize-path@^3.0.0")
          (s."path-is-absolute@^1.0.0")
          (s."readdirp@^2.2.1")
          (s."upath@^1.1.1")
          (s."fsevents@^1.2.7")
          ];
        "chokidar@3.4.2" = f "chokidar" "3.4.2" y "38dc8e658dec3809741eb3ef7bb0a47fe424232d" [
          (s."anymatch@~3.1.1")
          (s."braces@~3.0.2")
          (s."glob-parent@~5.1.0")
          (s."is-binary-path@~2.1.0")
          (s."is-glob@~4.0.1")
          (s."normalize-path@~3.0.0")
          (s."readdirp@~3.4.0")
          (s."fsevents@~2.1.2")
          ];
        "chokidar@3.4.3" = f "chokidar" "3.4.3" y "c1df38231448e45ca4ac588e6c79573ba6a57d5b" [
          (s."anymatch@~3.1.1")
          (s."braces@~3.0.2")
          (s."glob-parent@~5.1.0")
          (s."is-binary-path@~2.1.0")
          (s."is-glob@~4.0.1")
          (s."normalize-path@~3.0.0")
          (s."readdirp@~3.5.0")
          (s."fsevents@~2.1.2")
          ];
        "chokidar@^2.1.8" = s."chokidar@2.1.8";
        "chokidar@^3.4.1" = s."chokidar@3.4.2";
        "chokidar@^3.4.2" = s."chokidar@3.4.3";
        "chownr@1.1.4" = f "chownr" "1.1.4" y "6fc9d7b42d32a583596337666e7d08084da2cc6b" [];
        "chownr@^1.1.1" = s."chownr@1.1.4";
        "chrome-trace-event@1.0.2" = f "chrome-trace-event" "1.0.2" y "234090ee97c7d4ad1a2c4beae27505deffc608a4" [
          (s."tslib@^1.9.0")
          ];
        "chrome-trace-event@^1.0.2" = s."chrome-trace-event@1.0.2";
        "cipher-base@1.0.4" = f "cipher-base" "1.0.4" y "8760e4ecc272f4c363532f926d874aae2c1397de" [
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "cipher-base@^1.0.0" = s."cipher-base@1.0.4";
        "cipher-base@^1.0.1" = s."cipher-base@1.0.4";
        "cipher-base@^1.0.3" = s."cipher-base@1.0.4";
        "class-utils@0.3.6" = f "class-utils" "0.3.6" y "f93369ae8b9a7ce02fd41faad0ca83033190c463" [
          (s."arr-union@^3.1.0")
          (s."define-property@^0.2.5")
          (s."isobject@^3.0.0")
          (s."static-extend@^0.1.1")
          ];
        "class-utils@^0.3.5" = s."class-utils@0.3.6";
        "clean-css-cli@4.3.0" = f "clean-css-cli" "4.3.0" y "8502aa86d1879e5b111af51b3c2abb799e0684ce" [
          (s."clean-css@^4.2.1")
          (s."commander@2.x")
          (s."glob@7.x")
          ];
        "clean-css-cli@^4.3.0" = s."clean-css-cli@4.3.0";
        "clean-css@4.2.3" = f "clean-css" "4.2.3" y "507b5de7d97b48ee53d84adb0160ff6216380f78" [
          (s."source-map@~0.6.0")
          ];
        "clean-css@^4.2.1" = s."clean-css@4.2.3";
        "cliui@5.0.0" = f "cliui" "5.0.0" y "deefcfdb2e800784aa34f46fa08e06851c7bbbc5" [
          (s."string-width@^3.1.0")
          (s."strip-ansi@^5.2.0")
          (s."wrap-ansi@^5.1.0")
          ];
        "cliui@^5.0.0" = s."cliui@5.0.0";
        "collection-visit@1.0.0" = f "collection-visit" "1.0.0" y "4bc0373c164bc3291b4d368c829cf1a80a59dca0" [
          (s."map-visit@^1.0.0")
          (s."object-visit@^1.0.0")
          ];
        "collection-visit@^1.0.0" = s."collection-visit@1.0.0";
        "color-convert@1.9.3" = f "color-convert" "1.9.3" y "bb71850690e1f136567de629d2d5471deda4c1e8" [
          (s."color-name@1.1.3")
          ];
        "color-convert@2.0.1" = f "color-convert" "2.0.1" y "72d3a68d598c9bdb3af2ad1e84f21d896abd4de3" [
          (s."color-name@~1.1.4")
          ];
        "color-convert@^1.9.0" = s."color-convert@1.9.3";
        "color-convert@^2.0.1" = s."color-convert@2.0.1";
        "color-name@1.1.3" = f "color-name" "1.1.3" y "a7d0558bd89c42f795dd42328f740831ca53bc25" [];
        "color-name@1.1.4" = f "color-name" "1.1.4" y "c2a09a87acbde69543de6f63fa3995c826c536a2" [];
        "color-name@~1.1.4" = s."color-name@1.1.4";
        "combined-stream@1.0.8" = f "combined-stream" "1.0.8" y "c3d45a8b34fd730631a110a8a2520682b31d5a7f" [
          (s."delayed-stream@~1.0.0")
          ];
        "combined-stream@^1.0.6" = s."combined-stream@1.0.8";
        "combined-stream@~1.0.6" = s."combined-stream@1.0.8";
        "commander@2.20.3" = f "commander" "2.20.3" y "fd485e84c03eb4881c20722ba48035e8531aeb33" [];
        "commander@2.x" = s."commander@2.20.3";
        "commander@^2.20.0" = s."commander@2.20.3";
        "commondir@1.0.1" = f "commondir" "1.0.1" y "ddd800da0c66127393cca5950ea968a3aaf1253b" [];
        "commondir@^1.0.1" = s."commondir@1.0.1";
        "component-emitter@1.3.0" = f "component-emitter" "1.3.0" y "16e4070fba8ae29b679f2215853ee181ab2eabc0" [];
        "component-emitter@^1.2.1" = s."component-emitter@1.3.0";
        "concat-map@0.0.1" = f "concat-map" "0.0.1" y "d8a96bd77fd68df7793a73036a3ba0d5405d477b" [];
        "concat-stream@1.5.2" = f "concat-stream" "1.5.2" y "708978624d856af41a5a741defdd261da752c266" [
          (s."inherits@~2.0.1")
          (s."readable-stream@~2.0.0")
          (s."typedarray@~0.0.5")
          ];
        "concat-stream@1.6.2" = f "concat-stream" "1.6.2" y "904bdf194cd3122fc675c77fc4ac3d4ff0fd1a34" [
          (s."buffer-from@^1.0.0")
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.2.2")
          (s."typedarray@^0.0.6")
          ];
        "concat-stream@^1.5.0" = s."concat-stream@1.6.2";
        "concat-stream@^1.6.2" = s."concat-stream@1.6.2";
        "console-browserify@1.2.0" = f "console-browserify" "1.2.0" y "67063cef57ceb6cf4993a2ab3a55840ae8c49336" [];
        "console-browserify@^1.1.0" = s."console-browserify@1.2.0";
        "constants-browserify@1.0.0" = f "constants-browserify" "1.0.0" y "c20b96d8c617748aaf1c16021760cd27fcb8cb75" [];
        "constants-browserify@^1.0.0" = s."constants-browserify@1.0.0";
        "content-disposition@0.5.3" = f "content-disposition" "0.5.3" y "e130caf7e7279087c5616c2007d0485698984fbd" [
          (s."safe-buffer@5.1.2")
          ];
        "content-type@1.0.4" = f "content-type" "1.0.4" y "e138cc75e040c727b1966fe5e5f8c9aee256fe3b" [];
        "content-type@~1.0.4" = s."content-type@1.0.4";
        "convert-source-map@1.7.0" = f "convert-source-map" "1.7.0" y "17a2cb882d7f77d3490585e2ce6c524424a3a442" [
          (s."safe-buffer@~5.1.1")
          ];
        "convert-source-map@^1.7.0" = s."convert-source-map@1.7.0";
        "cookie-signature@1.0.6" = f "cookie-signature" "1.0.6" y "e303a882b342cc3ee8ca513a79999734dab3ae2c" [];
        "cookie@0.4.0" = f "cookie" "0.4.0" y "beb437e7022b3b6d49019d088665303ebe9c14ba" [];
        "copy-concurrently@1.0.5" = f "copy-concurrently" "1.0.5" y "92297398cae34937fcafd6ec8139c18051f0b5e0" [
          (s."aproba@^1.1.1")
          (s."fs-write-stream-atomic@^1.0.8")
          (s."iferr@^0.1.5")
          (s."mkdirp@^0.5.1")
          (s."rimraf@^2.5.4")
          (s."run-queue@^1.0.0")
          ];
        "copy-concurrently@^1.0.0" = s."copy-concurrently@1.0.5";
        "copy-descriptor@0.1.1" = f "copy-descriptor" "0.1.1" y "676f6eb3c39997c2ee1ac3a924fd6124748f578d" [];
        "copy-descriptor@^0.1.0" = s."copy-descriptor@0.1.1";
        "core-js-compat@3.6.5" = f "core-js-compat" "3.6.5" y "2a51d9a4e25dfd6e690251aa81f99e3c05481f1c" [
          (s."browserslist@^4.8.5")
          (s."semver@7.0.0")
          ];
        "core-js-compat@^3.6.2" = s."core-js-compat@3.6.5";
        "core-js@2.6.11" = f "core-js" "2.6.11" y "38831469f9922bded8ee21c9dc46985e0399308c" [];
        "core-js@^2.4.0" = s."core-js@2.6.11";
        "core-util-is@1.0.2" = f "core-util-is" "1.0.2" y "b5fd54220aa2bc5ab57aab7140c940754503c1a7" [];
        "core-util-is@~1.0.0" = s."core-util-is@1.0.2";
        "create-ecdh@4.0.4" = f "create-ecdh" "4.0.4" y "d6e7f4bffa66736085a0762fd3a632684dabcc4e" [
          (s."bn.js@^4.1.0")
          (s."elliptic@^6.5.3")
          ];
        "create-ecdh@^4.0.0" = s."create-ecdh@4.0.4";
        "create-hash@1.2.0" = f "create-hash" "1.2.0" y "889078af11a63756bcfb59bd221996be3a9ef196" [
          (s."cipher-base@^1.0.1")
          (s."inherits@^2.0.1")
          (s."md5.js@^1.3.4")
          (s."ripemd160@^2.0.1")
          (s."sha.js@^2.4.0")
          ];
        "create-hash@^1.1.0" = s."create-hash@1.2.0";
        "create-hash@^1.1.2" = s."create-hash@1.2.0";
        "create-hash@^1.2.0" = s."create-hash@1.2.0";
        "create-hmac@1.1.7" = f "create-hmac" "1.1.7" y "69170c78b3ab957147b2b8b04572e47ead2243ff" [
          (s."cipher-base@^1.0.3")
          (s."create-hash@^1.1.0")
          (s."inherits@^2.0.1")
          (s."ripemd160@^2.0.0")
          (s."safe-buffer@^5.0.1")
          (s."sha.js@^2.4.8")
          ];
        "create-hmac@^1.1.0" = s."create-hmac@1.1.7";
        "create-hmac@^1.1.4" = s."create-hmac@1.1.7";
        "create-hmac@^1.1.7" = s."create-hmac@1.1.7";
        "cross-spawn@4.0.2" = f "cross-spawn" "4.0.2" y "7b9247621c23adfdd3856004a823cbe397424d41" [
          (s."lru-cache@^4.0.1")
          (s."which@^1.2.9")
          ];
        "cross-spawn@6.0.5" = f "cross-spawn" "6.0.5" y "4a5ec7c64dfae22c3a14124dbacdee846d80cbc4" [
          (s."nice-try@^1.0.4")
          (s."path-key@^2.0.1")
          (s."semver@^5.5.0")
          (s."shebang-command@^1.2.0")
          (s."which@^1.2.9")
          ];
        "cross-spawn@7.0.3" = f "cross-spawn" "7.0.3" y "f73a85b9d5d41d045551c177e2882d4ac85728a6" [
          (s."path-key@^3.1.0")
          (s."shebang-command@^2.0.0")
          (s."which@^2.0.1")
          ];
        "cross-spawn@^4.0.2" = s."cross-spawn@4.0.2";
        "cross-spawn@^6.0.5" = s."cross-spawn@6.0.5";
        "cross-spawn@^7.0.3" = s."cross-spawn@7.0.3";
        "crypto-browserify@3.12.0" = f "crypto-browserify" "3.12.0" y "396cf9f3137f03e4b8e532c58f698254e00f80ec" [
          (s."browserify-cipher@^1.0.0")
          (s."browserify-sign@^4.0.0")
          (s."create-ecdh@^4.0.0")
          (s."create-hash@^1.1.0")
          (s."create-hmac@^1.1.0")
          (s."diffie-hellman@^5.0.0")
          (s."inherits@^2.0.1")
          (s."pbkdf2@^3.0.3")
          (s."public-encrypt@^4.0.0")
          (s."randombytes@^2.0.0")
          (s."randomfill@^1.0.3")
          ];
        "crypto-browserify@^3.11.0" = s."crypto-browserify@3.12.0";
        "cyclist@1.0.1" = f "cyclist" "1.0.1" y "596e9698fd0c80e12038c2b82d6eb1b35b6224d9" [];
        "cyclist@^1.0.1" = s."cyclist@1.0.1";
        "dashdash@1.14.1" = f "dashdash" "1.14.1" y "853cfa0f7cbe2fed5de20326b8dd581035f6e2f0" [
          (s."assert-plus@^1.0.0")
          ];
        "dashdash@^1.12.0" = s."dashdash@1.14.1";
        "debug@2.6.9" = f "debug" "2.6.9" y "5d128515df134ff327e90a4c93f4e077a536341f" [
          (s."ms@2.0.0")
          ];
        "debug@3.2.6" = f "debug" "3.2.6" y "e83d17de16d8a7efb7717edbe5fb10135eee629b" [
          (s."ms@^2.1.1")
          ];
        "debug@4.2.0" = f "debug" "4.2.0" y "7f150f93920e94c58f5574c2fd01a3110effe7f1" [
          (s."ms@2.1.2")
          ];
        "debug@^2.2.0" = s."debug@2.6.9";
        "debug@^2.3.3" = s."debug@2.6.9";
        "debug@^2.6.9" = s."debug@2.6.9";
        "debug@^3.1.0" = s."debug@3.2.6";
        "debug@^4.1.0" = s."debug@4.2.0";
        "decamelize@1.2.0" = f "decamelize" "1.2.0" y "f6534d15148269b20352e7bee26f501f9a191290" [];
        "decamelize@^1.2.0" = s."decamelize@1.2.0";
        "decode-uri-component@0.2.0" = f "decode-uri-component" "0.2.0" y "eb3913333458775cb84cd1a1fae062106bb87545" [];
        "decode-uri-component@^0.2.0" = s."decode-uri-component@0.2.0";
        "define-properties@1.1.3" = f "define-properties" "1.1.3" y "cf88da6cbee26fe6db7094f61d870cbd84cee9f1" [
          (s."object-keys@^1.0.12")
          ];
        "define-properties@^1.1.3" = s."define-properties@1.1.3";
        "define-property@0.2.5" = f "define-property" "0.2.5" y "c35b1ef918ec3c990f9a5bc57be04aacec5c8116" [
          (s."is-descriptor@^0.1.0")
          ];
        "define-property@1.0.0" = f "define-property" "1.0.0" y "769ebaaf3f4a63aad3af9e8d304c9bbe79bfb0e6" [
          (s."is-descriptor@^1.0.0")
          ];
        "define-property@2.0.2" = f "define-property" "2.0.2" y "d459689e8d654ba77e02a817f8710d702cb16e9d" [
          (s."is-descriptor@^1.0.2")
          (s."isobject@^3.0.1")
          ];
        "define-property@^0.2.5" = s."define-property@0.2.5";
        "define-property@^1.0.0" = s."define-property@1.0.0";
        "define-property@^2.0.2" = s."define-property@2.0.2";
        "delayed-stream@1.0.0" = f "delayed-stream" "1.0.0" y "df3ae199acadfb7d440aaae0b29e2272b24ec619" [];
        "delayed-stream@~1.0.0" = s."delayed-stream@1.0.0";
        "depd@1.1.2" = f "depd" "1.1.2" y "9bcd52e14c097763e749b274c4346ed2e560b5a9" [];
        "depd@~1.1.2" = s."depd@1.1.2";
        "des.js@1.0.1" = f "des.js" "1.0.1" y "5382142e1bdc53f85d86d53e5f4aa7deb91e0843" [
          (s."inherits@^2.0.1")
          (s."minimalistic-assert@^1.0.0")
          ];
        "des.js@^1.0.0" = s."des.js@1.0.1";
        "destroy@1.0.4" = f "destroy" "1.0.4" y "978857442c44749e4206613e37946205826abd80" [];
        "destroy@~1.0.4" = s."destroy@1.0.4";
        "detect-file@1.0.0" = f "detect-file" "1.0.0" y "f0d66d03672a825cb1b73bdb3fe62310c8e552b7" [];
        "detect-file@^1.0.0" = s."detect-file@1.0.0";
        "diffie-hellman@5.0.3" = f "diffie-hellman" "5.0.3" y "40e8ee98f55a2149607146921c63e1ae5f3d2875" [
          (s."bn.js@^4.1.0")
          (s."miller-rabin@^4.0.0")
          (s."randombytes@^2.0.0")
          ];
        "diffie-hellman@^5.0.0" = s."diffie-hellman@5.0.3";
        "domain-browser@1.2.0" = f "domain-browser" "1.2.0" y "3d31f50191a6749dd1375a7f522e823d42e54eda" [];
        "domain-browser@^1.1.1" = s."domain-browser@1.2.0";
        "duplexify@3.7.1" = f "duplexify" "3.7.1" y "2a4df5317f6ccfd91f86d6fd25d8d8a103b88309" [
          (s."end-of-stream@^1.0.0")
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.0.0")
          (s."stream-shift@^1.0.0")
          ];
        "duplexify@^3.4.2" = s."duplexify@3.7.1";
        "duplexify@^3.6.0" = s."duplexify@3.7.1";
        "ecc-jsbn@0.1.2" = f "ecc-jsbn" "0.1.2" y "3a83a904e54353287874c564b7549386849a98c9" [
          (s."jsbn@~0.1.0")
          (s."safer-buffer@^2.1.0")
          ];
        "ecc-jsbn@~0.1.1" = s."ecc-jsbn@0.1.2";
        "ee-first@1.1.1" = f "ee-first" "1.1.1" y "590c61156b0ae2f4f0255732a158b266bc56b21d" [];
        "electron-to-chromium@1.3.560" = f "electron-to-chromium" "1.3.560" y "6c3f61fe50324770b75705300e9f98f29312ea8d" [];
        "electron-to-chromium@^1.3.47" = s."electron-to-chromium@1.3.560";
        "electron-to-chromium@^1.3.523" = s."electron-to-chromium@1.3.560";
        "elliptic@6.5.3" = f "elliptic" "6.5.3" y "cb59eb2efdaf73a0bd78ccd7015a62ad6e0f93d6" [
          (s."bn.js@^4.4.0")
          (s."brorand@^1.0.1")
          (s."hash.js@^1.0.0")
          (s."hmac-drbg@^1.0.0")
          (s."inherits@^2.0.1")
          (s."minimalistic-assert@^1.0.0")
          (s."minimalistic-crypto-utils@^1.0.0")
          ];
        "elliptic@^6.5.3" = s."elliptic@6.5.3";
        "elm-analyse@0.16.5" = g "elm-analyse" "0.16.5" "https://github.com/stil4m/elm-analyse" "701b8b4013a4f057b9382d368e42adc6fe08e14e" "0y0rxvw24zfsxjfjfcs3rgyl7rkar6rwz1lr3xs0y4aw67y77zbk" [
          (s."body-parser@1.19.0")
          (s."express@4.17.1")
          (s."express-ws@2.0.0")
          (s."find@0.2.7")
          (s."fs-extra@2.0.0")
          (s."lodash@^4.17.15")
          (s."minimist@^1.2.5")
          (s."node-watch@0.5.5")
          (s."opn@6.0.0")
          (s."os-homedir@1.0.2")
          (s."request@2.88.2")
          (s."sums@0.2.4")
          (s."ws@3.3.1")
          ];
        "elm-analyse@git+https://github.com/stil4m/elm-analyse.git" = s."elm-analyse@0.16.5";
        "elm-format@0.8.2" = f "elm-format" "0.8.2" y "402d32b96014476ccf98ab5c92f4d102f933565f" [
          (s."binwrap@^0.2.2")
          ];
        "elm-test@0.19.1-revision4" = f "elm-test" "0.19.1-revision4" y "ae7b39a892ae8267de53bd15821ee78971a160c9" [
          (s."chalk@^3.0.0")
          (s."chokidar@^3.4.2")
          (s."cross-spawn@^7.0.3")
          (s."elmi-to-json@^1.3.0")
          (s."fs-extra@^8.1.0")
          (s."glob@^7.1.6")
          (s."minimist@^1.2.5")
          (s."murmur-hash-js@^1.0.0")
          (s."split@^1.0.1")
          (s."temp@^0.9.1")
          (s."which@^2.0.2")
          (s."xmlbuilder@^15.1.0")
          ];
        "elm-test@^0.19.1-revision4" = s."elm-test@0.19.1-revision4";
        "elm@0.19.1" = f "elm" "0.19.1" y "66461c49146099b866fdaf8af681331e5bec703d" [
          (s."request@^2.88.0")
          ];
        "elm@^0.19.1" = s."elm@0.19.1";
        "elmi-to-json@1.3.0" = f "elmi-to-json" "1.3.0" y "9de59d66d1df50f775a83405610ad41861a4305a" [
          (s."binwrap@0.2.2")
          ];
        "elmi-to-json@^1.3.0" = s."elmi-to-json@1.3.0";
        "emoji-regex@7.0.3" = f "emoji-regex" "7.0.3" y "933a04052860c85e83c122479c4748a8e4c72156" [];
        "emoji-regex@^7.0.1" = s."emoji-regex@7.0.3";
        "emojis-list@3.0.0" = f "emojis-list" "3.0.0" y "5570662046ad29e2e916e71aae260abdff4f6a78" [];
        "emojis-list@^3.0.0" = s."emojis-list@3.0.0";
        "encodeurl@1.0.2" = f "encodeurl" "1.0.2" y "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59" [];
        "encodeurl@~1.0.2" = s."encodeurl@1.0.2";
        "end-of-stream@1.4.4" = f "end-of-stream" "1.4.4" y "5ae64a5f45057baf3626ec14da0ca5e4b2431eb0" [
          (s."once@^1.4.0")
          ];
        "end-of-stream@^1.0.0" = s."end-of-stream@1.4.4";
        "end-of-stream@^1.1.0" = s."end-of-stream@1.4.4";
        "enhanced-resolve@4.3.0" = f "enhanced-resolve" "4.3.0" y "3b806f3bfafc1ec7de69551ef93cca46c1704126" [
          (s."graceful-fs@^4.1.2")
          (s."memory-fs@^0.5.0")
          (s."tapable@^1.0.0")
          ];
        "enhanced-resolve@^4.1.1" = s."enhanced-resolve@4.3.0";
        "enhanced-resolve@^4.3.0" = s."enhanced-resolve@4.3.0";
        "errno@0.1.7" = f "errno" "0.1.7" y "4684d71779ad39af177e3f007996f7c67c852618" [
          (s."prr@~1.0.1")
          ];
        "errno@^0.1.1" = s."errno@0.1.7";
        "errno@^0.1.3" = s."errno@0.1.7";
        "errno@~0.1.7" = s."errno@0.1.7";
        "es-abstract@1.17.7" = f "es-abstract" "1.17.7" y "a4de61b2f66989fc7421676c1cb9787573ace54c" [
          (s."es-to-primitive@^1.2.1")
          (s."function-bind@^1.1.1")
          (s."has@^1.0.3")
          (s."has-symbols@^1.0.1")
          (s."is-callable@^1.2.2")
          (s."is-regex@^1.1.1")
          (s."object-inspect@^1.8.0")
          (s."object-keys@^1.1.1")
          (s."object.assign@^4.1.1")
          (s."string.prototype.trimstart@^1.0.1")
          ];
        "es-abstract@1.18.0-next.1" = f "es-abstract" "1.18.0-next.1" y "6e3a0a4bda717e5023ab3b8e90bec36108d22c68" [
          (s."es-to-primitive@^1.2.1")
          (s."function-bind@^1.1.1")
          (s."has@^1.0.3")
          (s."has-symbols@^1.0.1")
          (s."is-callable@^1.2.2")
          (s."is-negative-zero@^2.0.0")
          (s."is-regex@^1.1.1")
          (s."object-inspect@^1.8.0")
          (s."object-keys@^1.1.1")
          (s."object.assign@^4.1.1")
          (s."string.prototype.trimend@^1.0.1")
          (s."string.prototype.trimstart@^1.0.1")
          ];
        "es-abstract@^1.17.5" = s."es-abstract@1.17.7";
        "es-abstract@^1.18.0-next.0" = s."es-abstract@1.18.0-next.1";
        "es-to-primitive@1.2.1" = f "es-to-primitive" "1.2.1" y "e55cd4c9cdc188bcefb03b366c736323fc5c898a" [
          (s."is-callable@^1.1.4")
          (s."is-date-object@^1.0.1")
          (s."is-symbol@^1.0.2")
          ];
        "es-to-primitive@^1.2.1" = s."es-to-primitive@1.2.1";
        "es6-promise@4.2.8" = f "es6-promise" "4.2.8" y "4eb21594c972bc40553d276e510539143db53e0a" [];
        "es6-promise@^4.0.3" = s."es6-promise@4.2.8";
        "es6-promisify@5.0.0" = f "es6-promisify" "5.0.0" y "5109d62f3e56ea967c4b63505aef08291c8a5203" [
          (s."es6-promise@^4.0.3")
          ];
        "es6-promisify@^5.0.0" = s."es6-promisify@5.0.0";
        "escalade@3.0.2" = f "escalade" "3.0.2" y "6a580d70edb87880f22b4c91d0d56078df6962c4" [];
        "escalade@^3.0.2" = s."escalade@3.0.2";
        "escape-html@1.0.3" = f "escape-html" "1.0.3" y "0258eae4d3d0c0974de1c169188ef0051d1d1988" [];
        "escape-html@~1.0.3" = s."escape-html@1.0.3";
        "escape-string-regexp@1.0.5" = f "escape-string-regexp" "1.0.5" y "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4" [];
        "escape-string-regexp@^1.0.5" = s."escape-string-regexp@1.0.5";
        "eslint-scope@4.0.3" = f "eslint-scope" "4.0.3" y "ca03833310f6889a3264781aa82e63eb9cfe7848" [
          (s."esrecurse@^4.1.0")
          (s."estraverse@^4.1.1")
          ];
        "eslint-scope@^4.0.3" = s."eslint-scope@4.0.3";
        "esrecurse@4.3.0" = f "esrecurse" "4.3.0" y "7ad7964d679abb28bee72cec63758b1c5d2c9921" [
          (s."estraverse@^5.2.0")
          ];
        "esrecurse@^4.1.0" = s."esrecurse@4.3.0";
        "estraverse@4.3.0" = f "estraverse" "4.3.0" y "398ad3f3c5a24948be7725e83d11a7de28cdbd1d" [];
        "estraverse@5.2.0" = f "estraverse" "5.2.0" y "307df42547e6cc7324d3cf03c155d5cdb8c53880" [];
        "estraverse@^4.1.1" = s."estraverse@4.3.0";
        "estraverse@^5.2.0" = s."estraverse@5.2.0";
        "esutils@2.0.3" = f "esutils" "2.0.3" y "74d2eb4de0b8da1293711910d50775b9b710ef64" [];
        "esutils@^2.0.2" = s."esutils@2.0.3";
        "etag@1.8.1" = f "etag" "1.8.1" y "41ae2eeb65efa62268aebfea83ac7d79299b0887" [];
        "etag@~1.8.1" = s."etag@1.8.1";
        "events@3.2.0" = f "events" "3.2.0" y "93b87c18f8efcd4202a461aec4dfc0556b639379" [];
        "events@^3.0.0" = s."events@3.2.0";
        "evp_bytestokey@1.0.3" = f "evp_bytestokey" "1.0.3" y "7fcbdb198dc71959432efe13842684e0525acb02" [
          (s."md5.js@^1.3.4")
          (s."safe-buffer@^5.1.1")
          ];
        "evp_bytestokey@^1.0.0" = s."evp_bytestokey@1.0.3";
        "evp_bytestokey@^1.0.3" = s."evp_bytestokey@1.0.3";
        "expand-brackets@2.1.4" = f "expand-brackets" "2.1.4" y "b77735e315ce30f6b6eff0f83b04151a22449622" [
          (s."debug@^2.3.3")
          (s."define-property@^0.2.5")
          (s."extend-shallow@^2.0.1")
          (s."posix-character-classes@^0.1.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "expand-brackets@^2.1.4" = s."expand-brackets@2.1.4";
        "expand-tilde@2.0.2" = f "expand-tilde" "2.0.2" y "97e801aa052df02454de46b02bf621642cdc8502" [
          (s."homedir-polyfill@^1.0.1")
          ];
        "expand-tilde@^2.0.0" = s."expand-tilde@2.0.2";
        "expand-tilde@^2.0.2" = s."expand-tilde@2.0.2";
        "express-ws@2.0.0" = f "express-ws" "2.0.0" y "96d13fa41c8de8fa5dcbfa2dacace6f594272888" [
          (s."ws@^1.0.0")
          ];
        "express@4.17.1" = f "express" "4.17.1" y "4491fc38605cf51f8629d39c2b5d026f98a4c134" [
          (s."accepts@~1.3.7")
          (s."array-flatten@1.1.1")
          (s."body-parser@1.19.0")
          (s."content-disposition@0.5.3")
          (s."content-type@~1.0.4")
          (s."cookie@0.4.0")
          (s."cookie-signature@1.0.6")
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."etag@~1.8.1")
          (s."finalhandler@~1.1.2")
          (s."fresh@0.5.2")
          (s."merge-descriptors@1.0.1")
          (s."methods@~1.1.2")
          (s."on-finished@~2.3.0")
          (s."parseurl@~1.3.3")
          (s."path-to-regexp@0.1.7")
          (s."proxy-addr@~2.0.5")
          (s."qs@6.7.0")
          (s."range-parser@~1.2.1")
          (s."safe-buffer@5.1.2")
          (s."send@0.17.1")
          (s."serve-static@1.14.1")
          (s."setprototypeof@1.1.1")
          (s."statuses@~1.5.0")
          (s."type-is@~1.6.18")
          (s."utils-merge@1.0.1")
          (s."vary@~1.1.2")
          ];
        "extend-shallow@2.0.1" = f "extend-shallow" "2.0.1" y "51af7d614ad9a9f610ea1bafbb989d6b1c56890f" [
          (s."is-extendable@^0.1.0")
          ];
        "extend-shallow@3.0.2" = f "extend-shallow" "3.0.2" y "26a71aaf073b39fb2127172746131c2704028db8" [
          (s."assign-symbols@^1.0.0")
          (s."is-extendable@^1.0.1")
          ];
        "extend-shallow@^2.0.1" = s."extend-shallow@2.0.1";
        "extend-shallow@^3.0.0" = s."extend-shallow@3.0.2";
        "extend-shallow@^3.0.2" = s."extend-shallow@3.0.2";
        "extend@3.0.2" = f "extend" "3.0.2" y "f8b1136b4071fbd8eb140aff858b1019ec2915fa" [];
        "extend@~3.0.2" = s."extend@3.0.2";
        "extglob@2.0.4" = f "extglob" "2.0.4" y "ad00fe4dc612a9232e8718711dc5cb5ab0285543" [
          (s."array-unique@^0.3.2")
          (s."define-property@^1.0.0")
          (s."expand-brackets@^2.1.4")
          (s."extend-shallow@^2.0.1")
          (s."fragment-cache@^0.2.1")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "extglob@^2.0.4" = s."extglob@2.0.4";
        "extract-zip@1.7.0" = f "extract-zip" "1.7.0" y "556cc3ae9df7f452c493a0cfb51cc30277940927" [
          (s."concat-stream@^1.6.2")
          (s."debug@^2.6.9")
          (s."mkdirp@^0.5.4")
          (s."yauzl@^2.10.0")
          ];
        "extract-zip@^1.6.6" = s."extract-zip@1.7.0";
        "extsprintf@1.3.0" = f "extsprintf" "1.3.0" y "96918440e3041a7a414f8c52e3c574eb3c3e1e05" [];
        "extsprintf@1.4.0" = f "extsprintf" "1.4.0" y "e2689f8f356fad62cca65a3a91c5df5f9551692f" [];
        "extsprintf@^1.2.0" = s."extsprintf@1.4.0";
        "fast-deep-equal@3.1.3" = f "fast-deep-equal" "3.1.3" y "3a7d56b559d6cbc3eb512325244e619a65c6c525" [];
        "fast-deep-equal@^3.1.1" = s."fast-deep-equal@3.1.3";
        "fast-json-stable-stringify@2.1.0" = f "fast-json-stable-stringify" "2.1.0" y "874bf69c6f404c2b5d99c481341399fd55892633" [];
        "fast-json-stable-stringify@^2.0.0" = s."fast-json-stable-stringify@2.1.0";
        "fd-slicer@1.1.0" = f "fd-slicer" "1.1.0" y "25c7c89cb1f9077f8891bbe61d8f390eae256f1e" [
          (s."pend@~1.2.0")
          ];
        "fd-slicer@~1.1.0" = s."fd-slicer@1.1.0";
        "figgy-pudding@3.5.2" = f "figgy-pudding" "3.5.2" y "b4eee8148abb01dcf1d1ac34367d59e12fa61d6e" [];
        "figgy-pudding@^3.5.1" = s."figgy-pudding@3.5.2";
        "file-uri-to-path@1.0.0" = f "file-uri-to-path" "1.0.0" y "553a7b8446ff6f684359c445f1e37a05dacc33dd" [];
        "fill-range@4.0.0" = f "fill-range" "4.0.0" y "d544811d428f98eb06a63dc402d2403c328c38f7" [
          (s."extend-shallow@^2.0.1")
          (s."is-number@^3.0.0")
          (s."repeat-string@^1.6.1")
          (s."to-regex-range@^2.1.0")
          ];
        "fill-range@7.0.1" = f "fill-range" "7.0.1" y "1919a6a7c75fe38b2c7c77e5198535da9acdda40" [
          (s."to-regex-range@^5.0.1")
          ];
        "fill-range@^4.0.0" = s."fill-range@4.0.0";
        "fill-range@^7.0.1" = s."fill-range@7.0.1";
        "finalhandler@1.1.2" = f "finalhandler" "1.1.2" y "b7e7d000ffd11938d0fdb053506f6ebabe9f587d" [
          (s."debug@2.6.9")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."on-finished@~2.3.0")
          (s."parseurl@~1.3.3")
          (s."statuses@~1.5.0")
          (s."unpipe@~1.0.0")
          ];
        "finalhandler@~1.1.2" = s."finalhandler@1.1.2";
        "find-cache-dir@2.1.0" = f "find-cache-dir" "2.1.0" y "8d0f94cd13fe43c6c7c261a0d86115ca918c05f7" [
          (s."commondir@^1.0.1")
          (s."make-dir@^2.0.0")
          (s."pkg-dir@^3.0.0")
          ];
        "find-cache-dir@^2.1.0" = s."find-cache-dir@2.1.0";
        "find-up@3.0.0" = f "find-up" "3.0.0" y "49169f1d7993430646da61ecc5ae355c21c97b73" [
          (s."locate-path@^3.0.0")
          ];
        "find-up@^3.0.0" = s."find-up@3.0.0";
        "find@0.2.7" = f "find" "0.2.7" y "7afbd00f8f08c5b622f97cda6f714173d547bb3f" [
          (s."traverse-chain@~0.1.0")
          ];
        "findup-sync@3.0.0" = f "findup-sync" "3.0.0" y "17b108f9ee512dfb7a5c7f3c8b27ea9e1a9c08d1" [
          (s."detect-file@^1.0.0")
          (s."is-glob@^4.0.0")
          (s."micromatch@^3.0.4")
          (s."resolve-dir@^1.0.1")
          ];
        "findup-sync@^3.0.0" = s."findup-sync@3.0.0";
        "flush-write-stream@1.1.1" = f "flush-write-stream" "1.1.1" y "8dd7d873a1babc207d94ead0c2e0e44276ebf2e8" [
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.3.6")
          ];
        "flush-write-stream@^1.0.0" = s."flush-write-stream@1.1.1";
        "for-in@1.0.2" = f "for-in" "1.0.2" y "81068d295a8142ec0ac726c6e2200c30fb6d5e80" [];
        "for-in@^1.0.2" = s."for-in@1.0.2";
        "forever-agent@0.6.1" = f "forever-agent" "0.6.1" y "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91" [];
        "forever-agent@~0.6.1" = s."forever-agent@0.6.1";
        "form-data@2.3.3" = f "form-data" "2.3.3" y "dcce52c05f644f298c6a7ab936bd724ceffbf3a6" [
          (s."asynckit@^0.4.0")
          (s."combined-stream@^1.0.6")
          (s."mime-types@^2.1.12")
          ];
        "form-data@~2.3.2" = s."form-data@2.3.3";
        "forwarded@0.1.2" = f "forwarded" "0.1.2" y "98c23dab1175657b8c0573e8ceccd91b0ff18c84" [];
        "forwarded@~0.1.2" = s."forwarded@0.1.2";
        "fragment-cache@0.2.1" = f "fragment-cache" "0.2.1" y "4290fad27f13e89be7f33799c6bc5a0abfff0d19" [
          (s."map-cache@^0.2.2")
          ];
        "fragment-cache@^0.2.1" = s."fragment-cache@0.2.1";
        "fresh@0.5.2" = f "fresh" "0.5.2" y "3d8cadd90d976569fa835ab1f8e4b23a105605a7" [];
        "from2@2.3.0" = f "from2" "2.3.0" y "8bfb5502bde4a4d36cfdeea007fcca21d7e382af" [
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.0.0")
          ];
        "from2@^2.1.0" = s."from2@2.3.0";
        "fs-extra@2.0.0" = f "fs-extra" "2.0.0" y "337352bded4a0b714f3eb84de8cea765e9d37600" [
          (s."graceful-fs@^4.1.2")
          (s."jsonfile@^2.1.0")
          ];
        "fs-extra@8.1.0" = f "fs-extra" "8.1.0" y "49d43c45a88cd9677668cb7be1b46efdb8d2e1c0" [
          (s."graceful-fs@^4.2.0")
          (s."jsonfile@^4.0.0")
          (s."universalify@^0.1.0")
          ];
        "fs-extra@^8.1.0" = s."fs-extra@8.1.0";
        "fs-minipass@1.2.7" = f "fs-minipass" "1.2.7" y "ccff8570841e7fe4265693da88936c55aed7f7c7" [
          (s."minipass@^2.6.0")
          ];
        "fs-minipass@^1.2.5" = s."fs-minipass@1.2.7";
        "fs-write-stream-atomic@1.0.10" = f "fs-write-stream-atomic" "1.0.10" y "b47df53493ef911df75731e70a9ded0189db40c9" [
          (s."graceful-fs@^4.1.2")
          (s."iferr@^0.1.5")
          (s."imurmurhash@^0.1.4")
          (s."readable-stream@1 || 2")
          ];
        "fs-write-stream-atomic@^1.0.8" = s."fs-write-stream-atomic@1.0.10";
        "fs.realpath@1.0.0" = f "fs.realpath" "1.0.0" y "1504ad2523158caa40db4a2787cb01411994ea4f" [];
        "fs.realpath@^1.0.0" = s."fs.realpath@1.0.0";
        "fsevents@1.2.13" = f "fsevents" "1.2.13" y "f325cb0455592428bcf11b383370ef70e3bfcc38" [
          (s."bindings@^1.5.0")
          (s."nan@^2.12.1")
          ];
        "fsevents@2.1.3" = f "fsevents" "2.1.3" y "fb738703ae8d2f9fe900c33836ddebee8b97f23e" [];
        "fsevents@^1.2.7" = s."fsevents@1.2.13";
        "fsevents@~2.1.2" = s."fsevents@2.1.3";
        "function-bind@1.1.1" = f "function-bind" "1.1.1" y "a56899d3ea3c9bab874bb9773b7c5ede92f4895d" [];
        "function-bind@^1.1.1" = s."function-bind@1.1.1";
        "gensync@1.0.0-beta.1" = f "gensync" "1.0.0-beta.1" y "58f4361ff987e5ff6e1e7a210827aa371eaac269" [];
        "gensync@^1.0.0-beta.1" = s."gensync@1.0.0-beta.1";
        "get-caller-file@2.0.5" = f "get-caller-file" "2.0.5" y "4f94412a82db32f36e3b0b9741f8a97feb031f7e" [];
        "get-caller-file@^2.0.1" = s."get-caller-file@2.0.5";
        "get-value@2.0.6" = f "get-value" "2.0.6" y "dc15ca1c672387ca76bd37ac0a395ba2042a2c28" [];
        "get-value@^2.0.3" = s."get-value@2.0.6";
        "get-value@^2.0.6" = s."get-value@2.0.6";
        "getpass@0.1.7" = f "getpass" "0.1.7" y "5eff8e3e684d569ae4cb2b1282604e8ba62149fa" [
          (s."assert-plus@^1.0.0")
          ];
        "getpass@^0.1.1" = s."getpass@0.1.7";
        "glob-parent@3.1.0" = f "glob-parent" "3.1.0" y "9e6af6299d8d3bd2bd40430832bd113df906c5ae" [
          (s."is-glob@^3.1.0")
          (s."path-dirname@^1.0.0")
          ];
        "glob-parent@5.1.1" = f "glob-parent" "5.1.1" y "b6c1ef417c4e5663ea498f1c45afac6916bbc229" [
          (s."is-glob@^4.0.1")
          ];
        "glob-parent@^3.1.0" = s."glob-parent@3.1.0";
        "glob-parent@~5.1.0" = s."glob-parent@5.1.1";
        "glob@7.1.6" = f "glob" "7.1.6" y "141f33b81a7c2492e125594307480c46679278a6" [
          (s."fs.realpath@^1.0.0")
          (s."inflight@^1.0.4")
          (s."inherits@2")
          (s."minimatch@^3.0.4")
          (s."once@^1.3.0")
          (s."path-is-absolute@^1.0.0")
          ];
        "glob@7.x" = s."glob@7.1.6";
        "glob@^7.1.3" = s."glob@7.1.6";
        "glob@^7.1.4" = s."glob@7.1.6";
        "glob@^7.1.6" = s."glob@7.1.6";
        "global-modules@1.0.0" = f "global-modules" "1.0.0" y "6d770f0eb523ac78164d72b5e71a8877265cc3ea" [
          (s."global-prefix@^1.0.1")
          (s."is-windows@^1.0.1")
          (s."resolve-dir@^1.0.0")
          ];
        "global-modules@2.0.0" = f "global-modules" "2.0.0" y "997605ad2345f27f51539bea26574421215c7780" [
          (s."global-prefix@^3.0.0")
          ];
        "global-modules@^1.0.0" = s."global-modules@1.0.0";
        "global-modules@^2.0.0" = s."global-modules@2.0.0";
        "global-prefix@1.0.2" = f "global-prefix" "1.0.2" y "dbf743c6c14992593c655568cb66ed32c0122ebe" [
          (s."expand-tilde@^2.0.2")
          (s."homedir-polyfill@^1.0.1")
          (s."ini@^1.3.4")
          (s."is-windows@^1.0.1")
          (s."which@^1.2.14")
          ];
        "global-prefix@3.0.0" = f "global-prefix" "3.0.0" y "fc85f73064df69f50421f47f883fe5b913ba9b97" [
          (s."ini@^1.3.5")
          (s."kind-of@^6.0.2")
          (s."which@^1.3.1")
          ];
        "global-prefix@^1.0.1" = s."global-prefix@1.0.2";
        "global-prefix@^3.0.0" = s."global-prefix@3.0.0";
        "globals@11.12.0" = f "globals" "11.12.0" y "ab8795338868a0babd8525758018c2a7eb95c42e" [];
        "globals@^11.1.0" = s."globals@11.12.0";
        "graceful-fs@4.2.4" = f "graceful-fs" "4.2.4" y "2256bde14d3632958c465ebc96dc467ca07a29fb" [];
        "graceful-fs@^4.1.11" = s."graceful-fs@4.2.4";
        "graceful-fs@^4.1.15" = s."graceful-fs@4.2.4";
        "graceful-fs@^4.1.2" = s."graceful-fs@4.2.4";
        "graceful-fs@^4.1.6" = s."graceful-fs@4.2.4";
        "graceful-fs@^4.2.0" = s."graceful-fs@4.2.4";
        "har-schema@2.0.0" = f "har-schema" "2.0.0" y "a94c2224ebcac04782a0d9035521f24735b7ec92" [];
        "har-schema@^2.0.0" = s."har-schema@2.0.0";
        "har-validator@5.1.5" = f "har-validator" "5.1.5" y "1f0803b9f8cb20c0fa13822df1ecddb36bde1efd" [
          (s."ajv@^6.12.3")
          (s."har-schema@^2.0.0")
          ];
        "har-validator@~5.1.3" = s."har-validator@5.1.5";
        "has-flag@3.0.0" = f "has-flag" "3.0.0" y "b5d454dc2199ae225699f3467e5a07f3b955bafd" [];
        "has-flag@4.0.0" = f "has-flag" "4.0.0" y "944771fd9c81c81265c4d6941860da06bb59479b" [];
        "has-flag@^3.0.0" = s."has-flag@3.0.0";
        "has-flag@^4.0.0" = s."has-flag@4.0.0";
        "has-symbols@1.0.1" = f "has-symbols" "1.0.1" y "9f5214758a44196c406d9bd76cebf81ec2dd31e8" [];
        "has-symbols@^1.0.1" = s."has-symbols@1.0.1";
        "has-value@0.3.1" = f "has-value" "0.3.1" y "7b1f58bada62ca827ec0a2078025654845995e1f" [
          (s."get-value@^2.0.3")
          (s."has-values@^0.1.4")
          (s."isobject@^2.0.0")
          ];
        "has-value@1.0.0" = f "has-value" "1.0.0" y "18b281da585b1c5c51def24c930ed29a0be6b177" [
          (s."get-value@^2.0.6")
          (s."has-values@^1.0.0")
          (s."isobject@^3.0.0")
          ];
        "has-value@^0.3.1" = s."has-value@0.3.1";
        "has-value@^1.0.0" = s."has-value@1.0.0";
        "has-values@0.1.4" = f "has-values" "0.1.4" y "6d61de95d91dfca9b9a02089ad384bff8f62b771" [];
        "has-values@1.0.0" = f "has-values" "1.0.0" y "95b0b63fec2146619a6fe57fe75628d5a39efe4f" [
          (s."is-number@^3.0.0")
          (s."kind-of@^4.0.0")
          ];
        "has-values@^0.1.4" = s."has-values@0.1.4";
        "has-values@^1.0.0" = s."has-values@1.0.0";
        "has@1.0.3" = f "has" "1.0.3" y "722d7cbfc1f6aa8241f16dd814e011e1f41e8796" [
          (s."function-bind@^1.1.1")
          ];
        "has@^1.0.3" = s."has@1.0.3";
        "hash-base@3.1.0" = f "hash-base" "3.1.0" y "55c381d9e06e1d2997a883b4a3fddfe7f0d3af33" [
          (s."inherits@^2.0.4")
          (s."readable-stream@^3.6.0")
          (s."safe-buffer@^5.2.0")
          ];
        "hash-base@^3.0.0" = s."hash-base@3.1.0";
        "hash.js@1.1.7" = f "hash.js" "1.1.7" y "0babca538e8d4ee4a0f8988d68866537a003cf42" [
          (s."inherits@^2.0.3")
          (s."minimalistic-assert@^1.0.1")
          ];
        "hash.js@^1.0.0" = s."hash.js@1.1.7";
        "hash.js@^1.0.3" = s."hash.js@1.1.7";
        "hmac-drbg@1.0.1" = f "hmac-drbg" "1.0.1" y "d2745701025a6c775a6c545793ed502fc0c649a1" [
          (s."hash.js@^1.0.3")
          (s."minimalistic-assert@^1.0.0")
          (s."minimalistic-crypto-utils@^1.0.1")
          ];
        "hmac-drbg@^1.0.0" = s."hmac-drbg@1.0.1";
        "homedir-polyfill@1.0.3" = f "homedir-polyfill" "1.0.3" y "743298cef4e5af3e194161fbadcc2151d3a058e8" [
          (s."parse-passwd@^1.0.0")
          ];
        "homedir-polyfill@^1.0.1" = s."homedir-polyfill@1.0.3";
        "http-errors@1.7.2" = f "http-errors" "1.7.2" y "4f5029cf13239f31036e5b2e55292bcfbcc85c8f" [
          (s."depd@~1.1.2")
          (s."inherits@2.0.3")
          (s."setprototypeof@1.1.1")
          (s."statuses@>= 1.5.0 < 2")
          (s."toidentifier@1.0.0")
          ];
        "http-errors@1.7.3" = f "http-errors" "1.7.3" y "6c619e4f9c60308c38519498c14fbb10aacebb06" [
          (s."depd@~1.1.2")
          (s."inherits@2.0.4")
          (s."setprototypeof@1.1.1")
          (s."statuses@>= 1.5.0 < 2")
          (s."toidentifier@1.0.0")
          ];
        "http-errors@~1.7.2" = s."http-errors@1.7.3";
        "http-signature@1.2.0" = f "http-signature" "1.2.0" y "9aecd925114772f3d95b65a60abb8f7c18fbace1" [
          (s."assert-plus@^1.0.0")
          (s."jsprim@^1.2.2")
          (s."sshpk@^1.7.0")
          ];
        "http-signature@~1.2.0" = s."http-signature@1.2.0";
        "https-browserify@1.0.0" = f "https-browserify" "1.0.0" y "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73" [];
        "https-browserify@^1.0.0" = s."https-browserify@1.0.0";
        "https-proxy-agent@2.2.4" = f "https-proxy-agent" "2.2.4" y "4ee7a737abd92678a293d9b34a1af4d0d08c787b" [
          (s."agent-base@^4.3.0")
          (s."debug@^3.1.0")
          ];
        "https-proxy-agent@^2.2.1" = s."https-proxy-agent@2.2.4";
        "iconv-lite@0.4.24" = f "iconv-lite" "0.4.24" y "2022b4b25fbddc21d2f524974a474aafe733908b" [
          (s."safer-buffer@>= 2.1.2 < 3")
          ];
        "ieee754@1.1.13" = f "ieee754" "1.1.13" y "ec168558e95aa181fd87d37f55c32bbcb6708b84" [];
        "ieee754@^1.1.4" = s."ieee754@1.1.13";
        "iferr@0.1.5" = f "iferr" "0.1.5" y "c60eed69e6d8fdb6b3104a1fcbca1c192dc5b501" [];
        "iferr@^0.1.5" = s."iferr@0.1.5";
        "image-size@0.5.5" = f "image-size" "0.5.5" y "09dfd4ab9d20e29eb1c3e80b8990378df9e3cb9c" [];
        "image-size@~0.5.0" = s."image-size@0.5.5";
        "import-local@2.0.0" = f "import-local" "2.0.0" y "55070be38a5993cf18ef6db7e961f5bee5c5a09d" [
          (s."pkg-dir@^3.0.0")
          (s."resolve-cwd@^2.0.0")
          ];
        "import-local@^2.0.0" = s."import-local@2.0.0";
        "imurmurhash@0.1.4" = f "imurmurhash" "0.1.4" y "9218b9b2b928a238b13dc4fb6b6d576f231453ea" [];
        "imurmurhash@^0.1.4" = s."imurmurhash@0.1.4";
        "infer-owner@1.0.4" = f "infer-owner" "1.0.4" y "c4cefcaa8e51051c2a40ba2ce8a3d27295af9467" [];
        "infer-owner@^1.0.3" = s."infer-owner@1.0.4";
        "inflight@1.0.6" = f "inflight" "1.0.6" y "49bd6331d7d02d0c09bc910a1075ba8165b56df9" [
          (s."once@^1.3.0")
          (s."wrappy@1")
          ];
        "inflight@^1.0.4" = s."inflight@1.0.6";
        "inherits@2" = s."inherits@2.0.4";
        "inherits@2.0.1" = f "inherits" "2.0.1" y "b17d08d326b4423e568eff719f91b0b1cbdf69f1" [];
        "inherits@2.0.3" = f "inherits" "2.0.3" y "633c2c83e3da42a502f52466022480f4208261de" [];
        "inherits@2.0.4" = f "inherits" "2.0.4" y "0fa2c64f932917c3433a0ded55363aae37416b7c" [];
        "inherits@^2.0.1" = s."inherits@2.0.4";
        "inherits@^2.0.3" = s."inherits@2.0.4";
        "inherits@^2.0.4" = s."inherits@2.0.4";
        "inherits@~2.0.1" = s."inherits@2.0.4";
        "inherits@~2.0.3" = s."inherits@2.0.4";
        "ini@1.3.8" = f "ini" "1.3.8" y "a29da425b48806f34767a4efce397269af28432c" [];
        "ini@^1.3.4" = s."ini@1.3.8";
        "ini@^1.3.5" = s."ini@1.3.8";
        "interpret@1.4.0" = f "interpret" "1.4.0" y "665ab8bc4da27a774a40584e812e3e0fa45b1a1e" [];
        "interpret@^1.4.0" = s."interpret@1.4.0";
        "invariant@2.2.4" = f "invariant" "2.2.4" y "610f3c92c9359ce1db616e538008d23ff35158e6" [
          (s."loose-envify@^1.0.0")
          ];
        "invariant@^2.2.2" = s."invariant@2.2.4";
        "invariant@^2.2.4" = s."invariant@2.2.4";
        "ipaddr.js@1.9.1" = f "ipaddr.js" "1.9.1" y "bff38543eeb8984825079ff3a2a8e6cbd46781b3" [];
        "is-accessor-descriptor@0.1.6" = f "is-accessor-descriptor" "0.1.6" y "a9e12cb3ae8d876727eeef3843f8a0897b5c98d6" [
          (s."kind-of@^3.0.2")
          ];
        "is-accessor-descriptor@1.0.0" = f "is-accessor-descriptor" "1.0.0" y "169c2f6d3df1f992618072365c9b0ea1f6878656" [
          (s."kind-of@^6.0.0")
          ];
        "is-accessor-descriptor@^0.1.6" = s."is-accessor-descriptor@0.1.6";
        "is-accessor-descriptor@^1.0.0" = s."is-accessor-descriptor@1.0.0";
        "is-binary-path@1.0.1" = f "is-binary-path" "1.0.1" y "75f16642b480f187a711c814161fd3a4a7655898" [
          (s."binary-extensions@^1.0.0")
          ];
        "is-binary-path@2.1.0" = f "is-binary-path" "2.1.0" y "ea1f7f3b80f064236e83470f86c09c254fb45b09" [
          (s."binary-extensions@^2.0.0")
          ];
        "is-binary-path@^1.0.0" = s."is-binary-path@1.0.1";
        "is-binary-path@~2.1.0" = s."is-binary-path@2.1.0";
        "is-buffer@1.1.6" = f "is-buffer" "1.1.6" y "efaa2ea9daa0d7ab2ea13a97b2b8ad51fefbe8be" [];
        "is-buffer@^1.1.5" = s."is-buffer@1.1.6";
        "is-callable@1.2.2" = f "is-callable" "1.2.2" y "c7c6715cd22d4ddb48d3e19970223aceabb080d9" [];
        "is-callable@^1.1.4" = s."is-callable@1.2.2";
        "is-callable@^1.2.2" = s."is-callable@1.2.2";
        "is-data-descriptor@0.1.4" = f "is-data-descriptor" "0.1.4" y "0b5ee648388e2c860282e793f1856fec3f301b56" [
          (s."kind-of@^3.0.2")
          ];
        "is-data-descriptor@1.0.0" = f "is-data-descriptor" "1.0.0" y "d84876321d0e7add03990406abbbbd36ba9268c7" [
          (s."kind-of@^6.0.0")
          ];
        "is-data-descriptor@^0.1.4" = s."is-data-descriptor@0.1.4";
        "is-data-descriptor@^1.0.0" = s."is-data-descriptor@1.0.0";
        "is-date-object@1.0.2" = f "is-date-object" "1.0.2" y "bda736f2cd8fd06d32844e7743bfa7494c3bfd7e" [];
        "is-date-object@^1.0.1" = s."is-date-object@1.0.2";
        "is-descriptor@0.1.6" = f "is-descriptor" "0.1.6" y "366d8240dde487ca51823b1ab9f07a10a78251ca" [
          (s."is-accessor-descriptor@^0.1.6")
          (s."is-data-descriptor@^0.1.4")
          (s."kind-of@^5.0.0")
          ];
        "is-descriptor@1.0.2" = f "is-descriptor" "1.0.2" y "3b159746a66604b04f8c81524ba365c5f14d86ec" [
          (s."is-accessor-descriptor@^1.0.0")
          (s."is-data-descriptor@^1.0.0")
          (s."kind-of@^6.0.2")
          ];
        "is-descriptor@^0.1.0" = s."is-descriptor@0.1.6";
        "is-descriptor@^1.0.0" = s."is-descriptor@1.0.2";
        "is-descriptor@^1.0.2" = s."is-descriptor@1.0.2";
        "is-extendable@0.1.1" = f "is-extendable" "0.1.1" y "62b110e289a471418e3ec36a617d472e301dfc89" [];
        "is-extendable@1.0.1" = f "is-extendable" "1.0.1" y "a7470f9e426733d81bd81e1155264e3a3507cab4" [
          (s."is-plain-object@^2.0.4")
          ];
        "is-extendable@^0.1.0" = s."is-extendable@0.1.1";
        "is-extendable@^0.1.1" = s."is-extendable@0.1.1";
        "is-extendable@^1.0.1" = s."is-extendable@1.0.1";
        "is-extglob@2.1.1" = f "is-extglob" "2.1.1" y "a88c02535791f02ed37c76a1b9ea9773c833f8c2" [];
        "is-extglob@^2.1.0" = s."is-extglob@2.1.1";
        "is-extglob@^2.1.1" = s."is-extglob@2.1.1";
        "is-fullwidth-code-point@2.0.0" = f "is-fullwidth-code-point" "2.0.0" y "a3b30a5c4f199183167aaab93beefae3ddfb654f" [];
        "is-fullwidth-code-point@^2.0.0" = s."is-fullwidth-code-point@2.0.0";
        "is-glob@3.1.0" = f "is-glob" "3.1.0" y "7ba5ae24217804ac70707b96922567486cc3e84a" [
          (s."is-extglob@^2.1.0")
          ];
        "is-glob@4.0.1" = f "is-glob" "4.0.1" y "7567dbe9f2f5e2467bc77ab83c4a29482407a5dc" [
          (s."is-extglob@^2.1.1")
          ];
        "is-glob@^3.1.0" = s."is-glob@3.1.0";
        "is-glob@^4.0.0" = s."is-glob@4.0.1";
        "is-glob@^4.0.1" = s."is-glob@4.0.1";
        "is-glob@~4.0.1" = s."is-glob@4.0.1";
        "is-negative-zero@2.0.0" = f "is-negative-zero" "2.0.0" y "9553b121b0fac28869da9ed459e20c7543788461" [];
        "is-negative-zero@^2.0.0" = s."is-negative-zero@2.0.0";
        "is-number@3.0.0" = f "is-number" "3.0.0" y "24fd6201a4782cf50561c810276afc7d12d71195" [
          (s."kind-of@^3.0.2")
          ];
        "is-number@7.0.0" = f "is-number" "7.0.0" y "7535345b896734d5f80c4d06c50955527a14f12b" [];
        "is-number@^3.0.0" = s."is-number@3.0.0";
        "is-number@^7.0.0" = s."is-number@7.0.0";
        "is-plain-object@2.0.4" = f "is-plain-object" "2.0.4" y "2c163b3fafb1b606d9d17928f05c2a1c38e07677" [
          (s."isobject@^3.0.1")
          ];
        "is-plain-object@^2.0.3" = s."is-plain-object@2.0.4";
        "is-plain-object@^2.0.4" = s."is-plain-object@2.0.4";
        "is-regex@1.1.1" = f "is-regex" "1.1.1" y "c6f98aacc546f6cec5468a07b7b153ab564a57b9" [
          (s."has-symbols@^1.0.1")
          ];
        "is-regex@^1.1.1" = s."is-regex@1.1.1";
        "is-stream@1.1.0" = f "is-stream" "1.1.0" y "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44" [];
        "is-symbol@1.0.3" = f "is-symbol" "1.0.3" y "38e1014b9e6329be0de9d24a414fd7441ec61937" [
          (s."has-symbols@^1.0.1")
          ];
        "is-symbol@^1.0.2" = s."is-symbol@1.0.3";
        "is-typedarray@1.0.0" = f "is-typedarray" "1.0.0" y "e479c80858df0c1b11ddda6940f96011fcda4a9a" [];
        "is-typedarray@~1.0.0" = s."is-typedarray@1.0.0";
        "is-windows@1.0.2" = f "is-windows" "1.0.2" y "d1850eb9791ecd18e6182ce12a30f396634bb19d" [];
        "is-windows@^1.0.1" = s."is-windows@1.0.2";
        "is-windows@^1.0.2" = s."is-windows@1.0.2";
        "is-wsl@1.1.0" = f "is-wsl" "1.1.0" y "1f16e4aa22b04d1336b66188a66af3c600c3a66d" [];
        "is-wsl@^1.1.0" = s."is-wsl@1.1.0";
        "isarray@1.0.0" = f "isarray" "1.0.0" y "bb935d48582cba168c06834957a54a3e07124f11" [];
        "isarray@^1.0.0" = s."isarray@1.0.0";
        "isarray@~1.0.0" = s."isarray@1.0.0";
        "isexe@2.0.0" = f "isexe" "2.0.0" y "e8fbf374dc556ff8947a10dcb0572d633f2cfa10" [];
        "isexe@^2.0.0" = s."isexe@2.0.0";
        "isobject@2.1.0" = f "isobject" "2.1.0" y "f065561096a3f1da2ef46272f815c840d87e0c89" [
          (s."isarray@1.0.0")
          ];
        "isobject@3.0.1" = f "isobject" "3.0.1" y "4e431e92b11a9731636aa1f9c8d1ccbcfdab78df" [];
        "isobject@^2.0.0" = s."isobject@2.1.0";
        "isobject@^3.0.0" = s."isobject@3.0.1";
        "isobject@^3.0.1" = s."isobject@3.0.1";
        "isstream@0.1.2" = f "isstream" "0.1.2" y "47e63f7af55afa6f92e1500e690eb8b8529c099a" [];
        "isstream@~0.1.2" = s."isstream@0.1.2";
        "js-tokens@4.0.0" = f "js-tokens" "4.0.0" y "19203fb59991df98e3a287050d4647cdeaf32499" [];
        "js-tokens@^3.0.0 || ^4.0.0" = s."js-tokens@4.0.0";
        "js-tokens@^4.0.0" = s."js-tokens@4.0.0";
        "jsbn@0.1.1" = f "jsbn" "0.1.1" y "a5e654c2e5a2deb5f201d96cefbca80c0ef2f513" [];
        "jsbn@~0.1.0" = s."jsbn@0.1.1";
        "jsesc@0.5.0" = f "jsesc" "0.5.0" y "e7dee66e35d6fc16f710fe91d5cf69f70f08911d" [];
        "jsesc@2.5.2" = f "jsesc" "2.5.2" y "80564d2e483dacf6e8ef209650a67df3f0c283a4" [];
        "jsesc@^2.5.1" = s."jsesc@2.5.2";
        "jsesc@~0.5.0" = s."jsesc@0.5.0";
        "json-parse-better-errors@1.0.2" = f "json-parse-better-errors" "1.0.2" y "bb867cfb3450e69107c131d1c514bab3dc8bcaa9" [];
        "json-parse-better-errors@^1.0.2" = s."json-parse-better-errors@1.0.2";
        "json-schema-traverse@0.4.1" = f "json-schema-traverse" "0.4.1" y "69f6a87d9513ab8bb8fe63bdb0979c448e684660" [];
        "json-schema-traverse@^0.4.1" = s."json-schema-traverse@0.4.1";
        "json-schema@0.2.3" = f "json-schema" "0.2.3" y "b480c892e59a2f05954ce727bd3f2a4e882f9e13" [];
        "json-stringify-safe@5.0.1" = f "json-stringify-safe" "5.0.1" y "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb" [];
        "json-stringify-safe@~5.0.1" = s."json-stringify-safe@5.0.1";
        "json5@1.0.1" = f "json5" "1.0.1" y "779fb0018604fa854eacbf6252180d83543e3dbe" [
          (s."minimist@^1.2.0")
          ];
        "json5@2.1.3" = f "json5" "2.1.3" y "c9b0f7fa9233bfe5807fe66fcf3a5617ed597d43" [
          (s."minimist@^1.2.5")
          ];
        "json5@^1.0.1" = s."json5@1.0.1";
        "json5@^2.1.2" = s."json5@2.1.3";
        "jsonfile@2.4.0" = f "jsonfile" "2.4.0" y "3736a2b428b87bbda0cc83b53fa3d633a35c2ae8" [
          (s."graceful-fs@^4.1.6")
          ];
        "jsonfile@4.0.0" = f "jsonfile" "4.0.0" y "8771aae0799b64076b76640fca058f9c10e33ecb" [
          (s."graceful-fs@^4.1.6")
          ];
        "jsonfile@^2.1.0" = s."jsonfile@2.4.0";
        "jsonfile@^4.0.0" = s."jsonfile@4.0.0";
        "jsprim@1.4.1" = f "jsprim" "1.4.1" y "313e66bc1e5cc06e438bc1b7499c2e5c56acb6a2" [
          (s."assert-plus@1.0.0")
          (s."extsprintf@1.3.0")
          (s."json-schema@0.2.3")
          (s."verror@1.10.0")
          ];
        "jsprim@^1.2.2" = s."jsprim@1.4.1";
        "kind-of@3.2.2" = f "kind-of" "3.2.2" y "31ea21a734bab9bbb0f32466d893aea51e4a3c64" [
          (s."is-buffer@^1.1.5")
          ];
        "kind-of@4.0.0" = f "kind-of" "4.0.0" y "20813df3d712928b207378691a45066fae72dd57" [
          (s."is-buffer@^1.1.5")
          ];
        "kind-of@5.1.0" = f "kind-of" "5.1.0" y "729c91e2d857b7a419a1f9aa65685c4c33f5845d" [];
        "kind-of@6.0.3" = f "kind-of" "6.0.3" y "07c05034a6c349fa06e24fa35aa76db4580ce4dd" [];
        "kind-of@^3.0.2" = s."kind-of@3.2.2";
        "kind-of@^3.0.3" = s."kind-of@3.2.2";
        "kind-of@^3.2.0" = s."kind-of@3.2.2";
        "kind-of@^4.0.0" = s."kind-of@4.0.0";
        "kind-of@^5.0.0" = s."kind-of@5.1.0";
        "kind-of@^6.0.0" = s."kind-of@6.0.3";
        "kind-of@^6.0.2" = s."kind-of@6.0.3";
        "less-plugin-autoprefix@2.0.0" = f "less-plugin-autoprefix" "2.0.0" y "778e6f2fe56884381c4fc3c747b0879e944f8481" [
          (s."autoprefixer@^8.6.3")
          (s."postcss@^6.0.22")
          ];
        "less-plugin-autoprefix@^2.0.0" = s."less-plugin-autoprefix@2.0.0";
        "less@3.12.2" = f "less" "3.12.2" y "157e6dd32a68869df8859314ad38e70211af3ab4" [
          (s."tslib@^1.10.0")
          (s."errno@^0.1.1")
          (s."graceful-fs@^4.1.2")
          (s."image-size@~0.5.0")
          (s."make-dir@^2.1.0")
          (s."mime@^1.4.1")
          (s."native-request@^1.0.5")
          (s."source-map@~0.6.0")
          ];
        "less@^3.10.3" = s."less@3.12.2";
        "leven@3.1.0" = f "leven" "3.1.0" y "77891de834064cccba82ae7842bb6b14a13ed7f2" [];
        "leven@^3.1.0" = s."leven@3.1.0";
        "levenary@1.1.1" = f "levenary" "1.1.1" y "842a9ee98d2075aa7faeedbe32679e9205f46f77" [
          (s."leven@^3.1.0")
          ];
        "levenary@^1.1.1" = s."levenary@1.1.1";
        "loader-runner@2.4.0" = f "loader-runner" "2.4.0" y "ed47066bfe534d7e84c4c7b9998c2a75607d9357" [];
        "loader-runner@^2.4.0" = s."loader-runner@2.4.0";
        "loader-utils@1.4.0" = f "loader-utils" "1.4.0" y "c579b5e34cb34b1a74edc6c1fb36bfa371d5a613" [
          (s."big.js@^5.2.2")
          (s."emojis-list@^3.0.0")
          (s."json5@^1.0.1")
          ];
        "loader-utils@^1.2.3" = s."loader-utils@1.4.0";
        "loader-utils@^1.4.0" = s."loader-utils@1.4.0";
        "locate-path@3.0.0" = f "locate-path" "3.0.0" y "dbec3b3ab759758071b58fe59fc41871af21400e" [
          (s."p-locate@^3.0.0")
          (s."path-exists@^3.0.0")
          ];
        "locate-path@^3.0.0" = s."locate-path@3.0.0";
        "lodash@4.17.20" = f "lodash" "4.17.20" y "b44a9b6297bcb698f1c51a3545a2b3b368d59c52" [];
        "lodash@^4.17.15" = s."lodash@4.17.20";
        "lodash@^4.17.19" = s."lodash@4.17.20";
        "loose-envify@1.4.0" = f "loose-envify" "1.4.0" y "71ee51fa7be4caec1a63839f7e682d8132d30caf" [
          (s."js-tokens@^3.0.0 || ^4.0.0")
          ];
        "loose-envify@^1.0.0" = s."loose-envify@1.4.0";
        "lru-cache@4.1.5" = f "lru-cache" "4.1.5" y "8bbe50ea85bed59bc9e33dcab8235ee9bcf443cd" [
          (s."pseudomap@^1.0.2")
          (s."yallist@^2.1.2")
          ];
        "lru-cache@5.1.1" = f "lru-cache" "5.1.1" y "1da27e6710271947695daf6848e847f01d84b920" [
          (s."yallist@^3.0.2")
          ];
        "lru-cache@^4.0.1" = s."lru-cache@4.1.5";
        "lru-cache@^5.1.1" = s."lru-cache@5.1.1";
        "make-dir@2.1.0" = f "make-dir" "2.1.0" y "5f0310e18b8be898cc07009295a30ae41e91e6f5" [
          (s."pify@^4.0.1")
          (s."semver@^5.6.0")
          ];
        "make-dir@^2.0.0" = s."make-dir@2.1.0";
        "make-dir@^2.1.0" = s."make-dir@2.1.0";
        "map-cache@0.2.2" = f "map-cache" "0.2.2" y "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf" [];
        "map-cache@^0.2.2" = s."map-cache@0.2.2";
        "map-visit@1.0.0" = f "map-visit" "1.0.0" y "ecdca8f13144e660f1b5bd41f12f3479d98dfb8f" [
          (s."object-visit@^1.0.0")
          ];
        "map-visit@^1.0.0" = s."map-visit@1.0.0";
        "md5.js@1.3.5" = f "md5.js" "1.3.5" y "b5d07b8e3216e3e27cd728d72f70d1e6a342005f" [
          (s."hash-base@^3.0.0")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "md5.js@^1.3.4" = s."md5.js@1.3.5";
        "media-typer@0.3.0" = f "media-typer" "0.3.0" y "8710d7af0aa626f8fffa1ce00168545263255748" [];
        "memory-fs@0.4.1" = f "memory-fs" "0.4.1" y "3a9a20b8462523e447cfbc7e8bb80ed667bfc552" [
          (s."errno@^0.1.3")
          (s."readable-stream@^2.0.1")
          ];
        "memory-fs@0.5.0" = f "memory-fs" "0.5.0" y "324c01288b88652966d161db77838720845a8e3c" [
          (s."errno@^0.1.3")
          (s."readable-stream@^2.0.1")
          ];
        "memory-fs@^0.4.1" = s."memory-fs@0.4.1";
        "memory-fs@^0.5.0" = s."memory-fs@0.5.0";
        "merge-descriptors@1.0.1" = f "merge-descriptors" "1.0.1" y "b00aaa556dd8b44568150ec9d1b953f3f90cbb61" [];
        "methods@1.1.2" = f "methods" "1.1.2" y "5529a4d67654134edcc5266656835b0f851afcee" [];
        "methods@~1.1.2" = s."methods@1.1.2";
        "micromatch@3.1.10" = f "micromatch" "3.1.10" y "70859bc95c9840952f359a068a3fc49f9ecfac23" [
          (s."arr-diff@^4.0.0")
          (s."array-unique@^0.3.2")
          (s."braces@^2.3.1")
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."extglob@^2.0.4")
          (s."fragment-cache@^0.2.1")
          (s."kind-of@^6.0.2")
          (s."nanomatch@^1.2.9")
          (s."object.pick@^1.3.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.2")
          ];
        "micromatch@^3.0.4" = s."micromatch@3.1.10";
        "micromatch@^3.1.10" = s."micromatch@3.1.10";
        "micromatch@^3.1.4" = s."micromatch@3.1.10";
        "miller-rabin@4.0.1" = f "miller-rabin" "4.0.1" y "f080351c865b0dc562a8462966daa53543c78a4d" [
          (s."bn.js@^4.0.0")
          (s."brorand@^1.0.1")
          ];
        "miller-rabin@^4.0.0" = s."miller-rabin@4.0.1";
        "mime-db@1.44.0" = f "mime-db" "1.44.0" y "fa11c5eb0aca1334b4233cb4d52f10c5a6272f92" [];
        "mime-types@2.1.27" = f "mime-types" "2.1.27" y "47949f98e279ea53119f5722e0f34e529bec009f" [
          (s."mime-db@1.44.0")
          ];
        "mime-types@^2.1.12" = s."mime-types@2.1.27";
        "mime-types@~2.1.19" = s."mime-types@2.1.27";
        "mime-types@~2.1.24" = s."mime-types@2.1.27";
        "mime@1.6.0" = f "mime" "1.6.0" y "32cd9e5c64553bd58d19a568af452acff04981b1" [];
        "mime@2.4.6" = f "mime" "2.4.6" y "e5b407c90db442f2beb5b162373d07b69affa4d1" [];
        "mime@^1.4.1" = s."mime@1.6.0";
        "mime@^2.0.3" = s."mime@2.4.6";
        "minimalistic-assert@1.0.1" = f "minimalistic-assert" "1.0.1" y "2e194de044626d4a10e7f7fbc00ce73e83e4d5c7" [];
        "minimalistic-assert@^1.0.0" = s."minimalistic-assert@1.0.1";
        "minimalistic-assert@^1.0.1" = s."minimalistic-assert@1.0.1";
        "minimalistic-crypto-utils@1.0.1" = f "minimalistic-crypto-utils" "1.0.1" y "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a" [];
        "minimalistic-crypto-utils@^1.0.0" = s."minimalistic-crypto-utils@1.0.1";
        "minimalistic-crypto-utils@^1.0.1" = s."minimalistic-crypto-utils@1.0.1";
        "minimatch@3.0.4" = f "minimatch" "3.0.4" y "5166e286457f03306064be5497e8dbb0c3d32083" [
          (s."brace-expansion@^1.1.7")
          ];
        "minimatch@^3.0.4" = s."minimatch@3.0.4";
        "minimist@1.2.5" = f "minimist" "1.2.5" y "67d66014b66a6a8aaa0c083c5fd58df4e4e97602" [];
        "minimist@^1.2.0" = s."minimist@1.2.5";
        "minimist@^1.2.5" = s."minimist@1.2.5";
        "minipass@2.9.0" = f "minipass" "2.9.0" y "e713762e7d3e32fed803115cf93e04bca9fcc9a6" [
          (s."safe-buffer@^5.1.2")
          (s."yallist@^3.0.0")
          ];
        "minipass@^2.6.0" = s."minipass@2.9.0";
        "minipass@^2.8.6" = s."minipass@2.9.0";
        "minipass@^2.9.0" = s."minipass@2.9.0";
        "minizlib@1.3.3" = f "minizlib" "1.3.3" y "2290de96818a34c29551c8a8d301216bd65a861d" [
          (s."minipass@^2.9.0")
          ];
        "minizlib@^1.2.1" = s."minizlib@1.3.3";
        "mississippi@3.0.0" = f "mississippi" "3.0.0" y "ea0a3291f97e0b5e8776b363d5f0a12d94c67022" [
          (s."concat-stream@^1.5.0")
          (s."duplexify@^3.4.2")
          (s."end-of-stream@^1.1.0")
          (s."flush-write-stream@^1.0.0")
          (s."from2@^2.1.0")
          (s."parallel-transform@^1.1.0")
          (s."pump@^3.0.0")
          (s."pumpify@^1.3.3")
          (s."stream-each@^1.1.0")
          (s."through2@^2.0.0")
          ];
        "mississippi@^3.0.0" = s."mississippi@3.0.0";
        "mixin-deep@1.3.2" = f "mixin-deep" "1.3.2" y "1120b43dc359a785dce65b55b82e257ccf479566" [
          (s."for-in@^1.0.2")
          (s."is-extendable@^1.0.1")
          ];
        "mixin-deep@^1.2.0" = s."mixin-deep@1.3.2";
        "mkdirp@0.5.5" = f "mkdirp" "0.5.5" y "d91cefd62d1436ca0f41620e251288d420099def" [
          (s."minimist@^1.2.5")
          ];
        "mkdirp@^0.5.0" = s."mkdirp@0.5.5";
        "mkdirp@^0.5.1" = s."mkdirp@0.5.5";
        "mkdirp@^0.5.3" = s."mkdirp@0.5.5";
        "mkdirp@^0.5.4" = s."mkdirp@0.5.5";
        "move-concurrently@1.0.1" = f "move-concurrently" "1.0.1" y "be2c005fda32e0b29af1f05d7c4b33214c701f92" [
          (s."aproba@^1.1.1")
          (s."copy-concurrently@^1.0.0")
          (s."fs-write-stream-atomic@^1.0.8")
          (s."mkdirp@^0.5.1")
          (s."rimraf@^2.5.4")
          (s."run-queue@^1.0.3")
          ];
        "move-concurrently@^1.0.1" = s."move-concurrently@1.0.1";
        "ms@2.0.0" = f "ms" "2.0.0" y "5608aeadfc00be6c2901df5f9861788de0d597c8" [];
        "ms@2.1.1" = f "ms" "2.1.1" y "30a5864eb3ebb0a66f2ebe6d727af06a09d86e0a" [];
        "ms@2.1.2" = f "ms" "2.1.2" y "d09d1f357b443f493382a8eb3ccd183872ae6009" [];
        "ms@^2.1.1" = s."ms@2.1.2";
        "murmur-hash-js@1.0.0" = f "murmur-hash-js" "1.0.0" y "5041049269c96633c866386960b2f4289e75e5b0" [];
        "murmur-hash-js@^1.0.0" = s."murmur-hash-js@1.0.0";
        "mustache@3.2.1" = f "mustache" "3.2.1" y "89e78a9d207d78f2799b1e95764a25bf71a28322" [];
        "mustache@^3.0.1" = s."mustache@3.2.1";
        "nan@2.14.1" = f "nan" "2.14.1" y "d7be34dfa3105b91494c3147089315eff8874b01" [];
        "nan@^2.12.1" = s."nan@2.14.1";
        "nanomatch@1.2.13" = f "nanomatch" "1.2.13" y "b87a8aa4fc0de8fe6be88895b38983ff265bd119" [
          (s."arr-diff@^4.0.0")
          (s."array-unique@^0.3.2")
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."fragment-cache@^0.2.1")
          (s."is-windows@^1.0.2")
          (s."kind-of@^6.0.2")
          (s."object.pick@^1.3.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "nanomatch@^1.2.9" = s."nanomatch@1.2.13";
        "native-request@1.0.7" = f "native-request" "1.0.7" y "ff742dc555b4c8f2f1c14b548639ba174e573856" [];
        "native-request@^1.0.5" = s."native-request@1.0.7";
        "negotiator@0.6.2" = f "negotiator" "0.6.2" y "feacf7ccf525a77ae9634436a64883ffeca346fb" [];
        "neo-async@2.6.2" = f "neo-async" "2.6.2" y "b4aafb93e3aeb2d8174ca53cf163ab7d7308305f" [];
        "neo-async@^2.5.0" = s."neo-async@2.6.2";
        "neo-async@^2.6.1" = s."neo-async@2.6.2";
        "nice-try@1.0.5" = f "nice-try" "1.0.5" y "a3378a7696ce7d223e88fc9b764bd7ef1089e366" [];
        "nice-try@^1.0.4" = s."nice-try@1.0.5";
        "node-libs-browser@2.2.1" = f "node-libs-browser" "2.2.1" y "b64f513d18338625f90346d27b0d235e631f6425" [
          (s."assert@^1.1.1")
          (s."browserify-zlib@^0.2.0")
          (s."buffer@^4.3.0")
          (s."console-browserify@^1.1.0")
          (s."constants-browserify@^1.0.0")
          (s."crypto-browserify@^3.11.0")
          (s."domain-browser@^1.1.1")
          (s."events@^3.0.0")
          (s."https-browserify@^1.0.0")
          (s."os-browserify@^0.3.0")
          (s."path-browserify@0.0.1")
          (s."process@^0.11.10")
          (s."punycode@^1.2.4")
          (s."querystring-es3@^0.2.0")
          (s."readable-stream@^2.3.3")
          (s."stream-browserify@^2.0.1")
          (s."stream-http@^2.7.2")
          (s."string_decoder@^1.0.0")
          (s."timers-browserify@^2.0.4")
          (s."tty-browserify@0.0.0")
          (s."url@^0.11.0")
          (s."util@^0.11.0")
          (s."vm-browserify@^1.0.1")
          ];
        "node-libs-browser@^2.2.1" = s."node-libs-browser@2.2.1";
        "node-releases@1.1.60" = f "node-releases" "1.1.60" y "6948bdfce8286f0b5d0e5a88e8384e954dfe7084" [];
        "node-releases@^1.1.60" = s."node-releases@1.1.60";
        "node-version@1.2.0" = f "node-version" "1.2.0" y "34fde3ffa8e1149bd323983479dda620e1b5060d" [];
        "node-version@^1.0.0" = s."node-version@1.2.0";
        "node-watch@0.5.5" = f "node-watch" "0.5.5" y "34865ba8bc6861ab086acdcc3403e40ed55c3274" [];
        "normalize-path@2.1.1" = f "normalize-path" "2.1.1" y "1ab28b556e198363a8c1a6f7e6fa20137fe6aed9" [
          (s."remove-trailing-separator@^1.0.1")
          ];
        "normalize-path@3.0.0" = f "normalize-path" "3.0.0" y "0dcd69ff23a1c9b11fd0978316644a0388216a65" [];
        "normalize-path@^2.1.1" = s."normalize-path@2.1.1";
        "normalize-path@^3.0.0" = s."normalize-path@3.0.0";
        "normalize-path@~3.0.0" = s."normalize-path@3.0.0";
        "normalize-range@0.1.2" = f "normalize-range" "0.1.2" y "2d10c06bdfd312ea9777695a4d28439456b75942" [];
        "normalize-range@^0.1.2" = s."normalize-range@0.1.2";
        "num2fraction@1.2.2" = f "num2fraction" "1.2.2" y "6f682b6a027a4e9ddfa4564cd2589d1d4e669ede" [];
        "num2fraction@^1.2.2" = s."num2fraction@1.2.2";
        "oauth-sign@0.9.0" = f "oauth-sign" "0.9.0" y "47a7b016baa68b5fa0ecf3dee08a85c679ac6455" [];
        "oauth-sign@~0.9.0" = s."oauth-sign@0.9.0";
        "object-assign@4.1.1" = f "object-assign" "4.1.1" y "2109adc7965887cfc05cbbd442cac8bfbb360863" [];
        "object-assign@^4.1.1" = s."object-assign@4.1.1";
        "object-copy@0.1.0" = f "object-copy" "0.1.0" y "7e7d858b781bd7c991a41ba975ed3812754e998c" [
          (s."copy-descriptor@^0.1.0")
          (s."define-property@^0.2.5")
          (s."kind-of@^3.0.3")
          ];
        "object-copy@^0.1.0" = s."object-copy@0.1.0";
        "object-inspect@1.8.0" = f "object-inspect" "1.8.0" y "df807e5ecf53a609cc6bfe93eac3cc7be5b3a9d0" [];
        "object-inspect@^1.8.0" = s."object-inspect@1.8.0";
        "object-keys@1.1.1" = f "object-keys" "1.1.1" y "1c47f272df277f3b1daf061677d9c82e2322c60e" [];
        "object-keys@^1.0.12" = s."object-keys@1.1.1";
        "object-keys@^1.1.1" = s."object-keys@1.1.1";
        "object-visit@1.0.1" = f "object-visit" "1.0.1" y "f79c4493af0c5377b59fe39d395e41042dd045bb" [
          (s."isobject@^3.0.0")
          ];
        "object-visit@^1.0.0" = s."object-visit@1.0.1";
        "object.assign@4.1.1" = f "object.assign" "4.1.1" y "303867a666cdd41936ecdedfb1f8f3e32a478cdd" [
          (s."define-properties@^1.1.3")
          (s."has-symbols@^1.0.1")
          (s."object-keys@^1.1.1")
          ];
        "object.assign@^4.1.0" = s."object.assign@4.1.1";
        "object.assign@^4.1.1" = s."object.assign@4.1.1";
        "object.pick@1.3.0" = f "object.pick" "1.3.0" y "87a10ac4c1694bd2e1cbf53591a66141fb5dd747" [
          (s."isobject@^3.0.1")
          ];
        "object.pick@^1.3.0" = s."object.pick@1.3.0";
        "on-finished@2.3.0" = f "on-finished" "2.3.0" y "20f1336481b083cd75337992a16971aa2d906947" [
          (s."ee-first@1.1.1")
          ];
        "on-finished@~2.3.0" = s."on-finished@2.3.0";
        "once@1.4.0" = f "once" "1.4.0" y "583b1aa775961d4b113ac17d9c50baef9dd76bd1" [
          (s."wrappy@1")
          ];
        "once@^1.3.0" = s."once@1.4.0";
        "once@^1.3.1" = s."once@1.4.0";
        "once@^1.4.0" = s."once@1.4.0";
        "opn@6.0.0" = f "opn" "6.0.0" y "3c5b0db676d5f97da1233d1ed42d182bc5a27d2d" [
          (s."is-wsl@^1.1.0")
          ];
        "options@0.0.6" = f "options" "0.0.6" y "ec22d312806bb53e731773e7cdaefcf1c643128f" [];
        "options@>=0.0.5" = s."options@0.0.6";
        "os-browserify@0.3.0" = f "os-browserify" "0.3.0" y "854373c7f5c2315914fc9bfc6bd8238fdda1ec27" [];
        "os-browserify@^0.3.0" = s."os-browserify@0.3.0";
        "os-homedir@1.0.2" = f "os-homedir" "1.0.2" y "ffbc4988336e0e833de0c168c7ef152121aa7fb3" [];
        "os-tmpdir@1.0.2" = f "os-tmpdir" "1.0.2" y "bbe67406c79aa85c5cfec766fe5734555dfa1274" [];
        "os-tmpdir@~1.0.1" = s."os-tmpdir@1.0.2";
        "p-limit@2.3.0" = f "p-limit" "2.3.0" y "3dd33c647a214fdfffd835933eb086da0dc21db1" [
          (s."p-try@^2.0.0")
          ];
        "p-limit@^2.0.0" = s."p-limit@2.3.0";
        "p-locate@3.0.0" = f "p-locate" "3.0.0" y "322d69a05c0264b25997d9f40cd8a891ab0064a4" [
          (s."p-limit@^2.0.0")
          ];
        "p-locate@^3.0.0" = s."p-locate@3.0.0";
        "p-try@2.2.0" = f "p-try" "2.2.0" y "cb2868540e313d61de58fafbe35ce9004d5540e6" [];
        "p-try@^2.0.0" = s."p-try@2.2.0";
        "pako@1.0.11" = f "pako" "1.0.11" y "6c9599d340d54dfd3946380252a35705a6b992bf" [];
        "pako@~1.0.5" = s."pako@1.0.11";
        "parallel-transform@1.2.0" = f "parallel-transform" "1.2.0" y "9049ca37d6cb2182c3b1d2c720be94d14a5814fc" [
          (s."cyclist@^1.0.1")
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.1.5")
          ];
        "parallel-transform@^1.1.0" = s."parallel-transform@1.2.0";
        "parse-asn1@5.1.6" = f "parse-asn1" "5.1.6" y "385080a3ec13cb62a62d39409cb3e88844cdaed4" [
          (s."asn1.js@^5.2.0")
          (s."browserify-aes@^1.0.0")
          (s."evp_bytestokey@^1.0.0")
          (s."pbkdf2@^3.0.3")
          (s."safe-buffer@^5.1.1")
          ];
        "parse-asn1@^5.0.0" = s."parse-asn1@5.1.6";
        "parse-asn1@^5.1.5" = s."parse-asn1@5.1.6";
        "parse-passwd@1.0.0" = f "parse-passwd" "1.0.0" y "6d5b934a456993b23d37f40a382d6f1666a8e5c6" [];
        "parse-passwd@^1.0.0" = s."parse-passwd@1.0.0";
        "parseurl@1.3.3" = f "parseurl" "1.3.3" y "9da19e7bee8d12dff0513ed5b76957793bc2e8d4" [];
        "parseurl@~1.3.3" = s."parseurl@1.3.3";
        "pascalcase@0.1.1" = f "pascalcase" "0.1.1" y "b363e55e8006ca6fe21784d2db22bd15d7917f14" [];
        "pascalcase@^0.1.1" = s."pascalcase@0.1.1";
        "path-browserify@0.0.1" = f "path-browserify" "0.0.1" y "e6c4ddd7ed3aa27c68a20cc4e50e1a4ee83bbc4a" [];
        "path-dirname@1.0.2" = f "path-dirname" "1.0.2" y "cc33d24d525e099a5388c0336c6e32b9160609e0" [];
        "path-dirname@^1.0.0" = s."path-dirname@1.0.2";
        "path-exists@3.0.0" = f "path-exists" "3.0.0" y "ce0ebeaa5f78cb18925ea7d810d7b59b010fd515" [];
        "path-exists@^3.0.0" = s."path-exists@3.0.0";
        "path-is-absolute@1.0.1" = f "path-is-absolute" "1.0.1" y "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f" [];
        "path-is-absolute@^1.0.0" = s."path-is-absolute@1.0.1";
        "path-key@2.0.1" = f "path-key" "2.0.1" y "411cadb574c5a140d3a4b1910d40d80cc9f40b40" [];
        "path-key@3.1.1" = f "path-key" "3.1.1" y "581f6ade658cbba65a0d3380de7753295054f375" [];
        "path-key@^2.0.1" = s."path-key@2.0.1";
        "path-key@^3.1.0" = s."path-key@3.1.1";
        "path-parse@1.0.6" = f "path-parse" "1.0.6" y "d62dbb5679405d72c4737ec58600e9ddcf06d24c" [];
        "path-parse@^1.0.6" = s."path-parse@1.0.6";
        "path-to-regexp@0.1.7" = f "path-to-regexp" "0.1.7" y "df604178005f522f15eb4490e7247a1bfaa67f8c" [];
        "pbkdf2@3.1.1" = f "pbkdf2" "3.1.1" y "cb8724b0fada984596856d1a6ebafd3584654b94" [
          (s."create-hash@^1.1.2")
          (s."create-hmac@^1.1.4")
          (s."ripemd160@^2.0.1")
          (s."safe-buffer@^5.0.1")
          (s."sha.js@^2.4.8")
          ];
        "pbkdf2@^3.0.3" = s."pbkdf2@3.1.1";
        "pend@1.2.0" = f "pend" "1.2.0" y "7a57eb550a6783f9115331fcf4663d5c8e007a50" [];
        "pend@~1.2.0" = s."pend@1.2.0";
        "performance-now@2.1.0" = f "performance-now" "2.1.0" y "6309f4e0e5fa913ec1c69307ae364b4b377c9e7b" [];
        "performance-now@^2.1.0" = s."performance-now@2.1.0";
        "picomatch@2.2.2" = f "picomatch" "2.2.2" y "21f333e9b6b8eaff02468f5146ea406d345f4dad" [];
        "picomatch@^2.0.4" = s."picomatch@2.2.2";
        "picomatch@^2.2.1" = s."picomatch@2.2.2";
        "pify@4.0.1" = f "pify" "4.0.1" y "4b2cd25c50d598735c50292224fd8c6df41e3231" [];
        "pify@^4.0.1" = s."pify@4.0.1";
        "pkg-dir@3.0.0" = f "pkg-dir" "3.0.0" y "2749020f239ed990881b1f71210d51eb6523bea3" [
          (s."find-up@^3.0.0")
          ];
        "pkg-dir@^3.0.0" = s."pkg-dir@3.0.0";
        "posix-character-classes@0.1.1" = f "posix-character-classes" "0.1.1" y "01eac0fe3b5af71a2a6c02feabb8c1fef7e00eab" [];
        "posix-character-classes@^0.1.0" = s."posix-character-classes@0.1.1";
        "postcss-value-parser@3.3.1" = f "postcss-value-parser" "3.3.1" y "9ff822547e2893213cf1c30efa51ac5fd1ba8281" [];
        "postcss-value-parser@^3.2.3" = s."postcss-value-parser@3.3.1";
        "postcss@6.0.23" = f "postcss" "6.0.23" y "61c82cc328ac60e677645f979054eb98bc0e3324" [
          (s."chalk@^2.4.1")
          (s."source-map@^0.6.1")
          (s."supports-color@^5.4.0")
          ];
        "postcss@^6.0.22" = s."postcss@6.0.23";
        "postcss@^6.0.23" = s."postcss@6.0.23";
        "process-nextick-args@1.0.7" = f "process-nextick-args" "1.0.7" y "150e20b756590ad3f91093f25a4f2ad8bff30ba3" [];
        "process-nextick-args@2.0.1" = f "process-nextick-args" "2.0.1" y "7820d9b16120cc55ca9ae7792680ae7dba6d7fe2" [];
        "process-nextick-args@~1.0.6" = s."process-nextick-args@1.0.7";
        "process-nextick-args@~2.0.0" = s."process-nextick-args@2.0.1";
        "process@0.11.10" = f "process" "0.11.10" y "7332300e840161bda3e69a1d1d91a7d4bc16f182" [];
        "process@^0.11.10" = s."process@0.11.10";
        "progress@2.0.3" = f "progress" "2.0.3" y "7e8cf8d8f5b8f239c1bc68beb4eb78567d572ef8" [];
        "progress@^2.0.1" = s."progress@2.0.3";
        "promise-inflight@1.0.1" = f "promise-inflight" "1.0.1" y "98472870bf228132fcbdd868129bad12c3c029e3" [];
        "promise-inflight@^1.0.1" = s."promise-inflight@1.0.1";
        "promise-polyfill@6.1.0" = f "promise-polyfill" "6.1.0" y "dfa96943ea9c121fca4de9b5868cb39d3472e057" [];
        "promise-polyfill@^6.0.1" = s."promise-polyfill@6.1.0";
        "proxy-addr@2.0.6" = f "proxy-addr" "2.0.6" y "fdc2336505447d3f2f2c638ed272caf614bbb2bf" [
          (s."forwarded@~0.1.2")
          (s."ipaddr.js@1.9.1")
          ];
        "proxy-addr@~2.0.5" = s."proxy-addr@2.0.6";
        "proxy-from-env@1.1.0" = f "proxy-from-env" "1.1.0" y "e102f16ca355424865755d2c9e8ea4f24d58c3e2" [];
        "proxy-from-env@^1.0.0" = s."proxy-from-env@1.1.0";
        "prr@1.0.1" = f "prr" "1.0.1" y "d3fc114ba06995a45ec6893f484ceb1d78f5f476" [];
        "prr@~1.0.1" = s."prr@1.0.1";
        "pseudomap@1.0.2" = f "pseudomap" "1.0.2" y "f052a28da70e618917ef0a8ac34c1ae5a68286b3" [];
        "pseudomap@^1.0.2" = s."pseudomap@1.0.2";
        "psl@1.8.0" = f "psl" "1.8.0" y "9326f8bcfb013adcc005fdff056acce020e51c24" [];
        "psl@^1.1.28" = s."psl@1.8.0";
        "public-encrypt@4.0.3" = f "public-encrypt" "4.0.3" y "4fcc9d77a07e48ba7527e7cbe0de33d0701331e0" [
          (s."bn.js@^4.1.0")
          (s."browserify-rsa@^4.0.0")
          (s."create-hash@^1.1.0")
          (s."parse-asn1@^5.0.0")
          (s."randombytes@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "public-encrypt@^4.0.0" = s."public-encrypt@4.0.3";
        "pump@2.0.1" = f "pump" "2.0.1" y "12399add6e4cf7526d973cbc8b5ce2e2908b3909" [
          (s."end-of-stream@^1.1.0")
          (s."once@^1.3.1")
          ];
        "pump@3.0.0" = f "pump" "3.0.0" y "b4a2116815bde2f4e1ea602354e8c75565107a64" [
          (s."end-of-stream@^1.1.0")
          (s."once@^1.3.1")
          ];
        "pump@^2.0.0" = s."pump@2.0.1";
        "pump@^3.0.0" = s."pump@3.0.0";
        "pumpify@1.5.1" = f "pumpify" "1.5.1" y "36513be246ab27570b1a374a5ce278bfd74370ce" [
          (s."duplexify@^3.6.0")
          (s."inherits@^2.0.3")
          (s."pump@^2.0.0")
          ];
        "pumpify@^1.3.3" = s."pumpify@1.5.1";
        "punycode@1.3.2" = f "punycode" "1.3.2" y "9653a036fb7c1ee42342f2325cceefea3926c48d" [];
        "punycode@1.4.1" = f "punycode" "1.4.1" y "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e" [];
        "punycode@2.1.1" = f "punycode" "2.1.1" y "b58b010ac40c22c5657616c8d2c2c02c7bf479ec" [];
        "punycode@^1.2.4" = s."punycode@1.4.1";
        "punycode@^2.1.0" = s."punycode@2.1.1";
        "punycode@^2.1.1" = s."punycode@2.1.1";
        "puppeteer@1.20.0" = f "puppeteer" "1.20.0" y "e3d267786f74e1d87cf2d15acc59177f471bbe38" [
          (s."debug@^4.1.0")
          (s."extract-zip@^1.6.6")
          (s."https-proxy-agent@^2.2.1")
          (s."mime@^2.0.3")
          (s."progress@^2.0.1")
          (s."proxy-from-env@^1.0.0")
          (s."rimraf@^2.6.1")
          (s."ws@^6.1.0")
          ];
        "puppeteer@^1.20.0" = s."puppeteer@1.20.0";
        "qs@6.5.2" = f "qs" "6.5.2" y "cb3ae806e8740444584ef154ce8ee98d403f3e36" [];
        "qs@6.7.0" = f "qs" "6.7.0" y "41dc1a015e3d581f1621776be31afb2876a9b1bc" [];
        "qs@~6.5.2" = s."qs@6.5.2";
        "querystring-es3@0.2.1" = f "querystring-es3" "0.2.1" y "9ec61f79049875707d69414596fd907a4d711e73" [];
        "querystring-es3@^0.2.0" = s."querystring-es3@0.2.1";
        "querystring@0.2.0" = f "querystring" "0.2.0" y "b209849203bb25df820da756e747005878521620" [];
        "randombytes@2.1.0" = f "randombytes" "2.1.0" y "df6f84372f0270dc65cdf6291349ab7a473d4f2a" [
          (s."safe-buffer@^5.1.0")
          ];
        "randombytes@^2.0.0" = s."randombytes@2.1.0";
        "randombytes@^2.0.1" = s."randombytes@2.1.0";
        "randombytes@^2.0.5" = s."randombytes@2.1.0";
        "randombytes@^2.1.0" = s."randombytes@2.1.0";
        "randomfill@1.0.4" = f "randomfill" "1.0.4" y "c92196fc86ab42be983f1bf31778224931d61458" [
          (s."randombytes@^2.0.5")
          (s."safe-buffer@^5.1.0")
          ];
        "randomfill@^1.0.3" = s."randomfill@1.0.4";
        "range-parser@1.2.1" = f "range-parser" "1.2.1" y "3cf37023d199e1c24d1a55b84800c2f3e6468031" [];
        "range-parser@~1.2.1" = s."range-parser@1.2.1";
        "raw-body@2.4.0" = f "raw-body" "2.4.0" y "a1ce6fb9c9bc356ca52e89256ab59059e13d0332" [
          (s."bytes@3.1.0")
          (s."http-errors@1.7.2")
          (s."iconv-lite@0.4.24")
          (s."unpipe@1.0.0")
          ];
        "readable-stream@1 || 2" = s."readable-stream@2.3.7";
        "readable-stream@2.0.6" = f "readable-stream" "2.0.6" y "8f90341e68a53ccc928788dacfcd11b36eb9b78e" [
          (s."core-util-is@~1.0.0")
          (s."inherits@~2.0.1")
          (s."isarray@~1.0.0")
          (s."process-nextick-args@~1.0.6")
          (s."string_decoder@~0.10.x")
          (s."util-deprecate@~1.0.1")
          ];
        "readable-stream@2.3.7" = f "readable-stream" "2.3.7" y "1eca1cf711aef814c04f62252a36a62f6cb23b57" [
          (s."core-util-is@~1.0.0")
          (s."inherits@~2.0.3")
          (s."isarray@~1.0.0")
          (s."process-nextick-args@~2.0.0")
          (s."safe-buffer@~5.1.1")
          (s."string_decoder@~1.1.1")
          (s."util-deprecate@~1.0.1")
          ];
        "readable-stream@3.6.0" = f "readable-stream" "3.6.0" y "337bbda3adc0706bd3e024426a286d4b4b2c9198" [
          (s."inherits@^2.0.3")
          (s."string_decoder@^1.1.1")
          (s."util-deprecate@^1.0.1")
          ];
        "readable-stream@^2.0.0" = s."readable-stream@2.3.7";
        "readable-stream@^2.0.1" = s."readable-stream@2.3.7";
        "readable-stream@^2.0.2" = s."readable-stream@2.3.7";
        "readable-stream@^2.1.5" = s."readable-stream@2.3.7";
        "readable-stream@^2.2.2" = s."readable-stream@2.3.7";
        "readable-stream@^2.3.3" = s."readable-stream@2.3.7";
        "readable-stream@^2.3.6" = s."readable-stream@2.3.7";
        "readable-stream@^3.6.0" = s."readable-stream@3.6.0";
        "readable-stream@~2.0.0" = s."readable-stream@2.0.6";
        "readable-stream@~2.3.6" = s."readable-stream@2.3.7";
        "readdirp@2.2.1" = f "readdirp" "2.2.1" y "0e87622a3325aa33e892285caf8b4e846529a525" [
          (s."graceful-fs@^4.1.11")
          (s."micromatch@^3.1.10")
          (s."readable-stream@^2.0.2")
          ];
        "readdirp@3.4.0" = f "readdirp" "3.4.0" y "9fdccdf9e9155805449221ac645e8303ab5b9ada" [
          (s."picomatch@^2.2.1")
          ];
        "readdirp@3.5.0" = f "readdirp" "3.5.0" y "9ba74c019b15d365278d2e91bb8c48d7b4d42c9e" [
          (s."picomatch@^2.2.1")
          ];
        "readdirp@^2.2.1" = s."readdirp@2.2.1";
        "readdirp@~3.4.0" = s."readdirp@3.4.0";
        "readdirp@~3.5.0" = s."readdirp@3.5.0";
        "regenerate-unicode-properties@8.2.0" = f "regenerate-unicode-properties" "8.2.0" y "e5de7111d655e7ba60c057dbe9ff37c87e65cdec" [
          (s."regenerate@^1.4.0")
          ];
        "regenerate-unicode-properties@^8.2.0" = s."regenerate-unicode-properties@8.2.0";
        "regenerate@1.4.1" = f "regenerate" "1.4.1" y "cad92ad8e6b591773485fbe05a485caf4f457e6f" [];
        "regenerate@^1.4.0" = s."regenerate@1.4.1";
        "regenerator-runtime@0.13.7" = f "regenerator-runtime" "0.13.7" y "cac2dacc8a1ea675feaabaeb8ae833898ae46f55" [];
        "regenerator-runtime@0.9.6" = f "regenerator-runtime" "0.9.6" y "d33eb95d0d2001a4be39659707c51b0cb71ce029" [];
        "regenerator-runtime@^0.13.4" = s."regenerator-runtime@0.13.7";
        "regenerator-runtime@^0.9.5" = s."regenerator-runtime@0.9.6";
        "regenerator-transform@0.14.5" = f "regenerator-transform" "0.14.5" y "c98da154683671c9c4dcb16ece736517e1b7feb4" [
          (s."@babel/runtime@^7.8.4")
          ];
        "regenerator-transform@^0.14.2" = s."regenerator-transform@0.14.5";
        "regex-not@1.0.2" = f "regex-not" "1.0.2" y "1f4ece27e00b0b65e0247a6810e6a85d83a5752c" [
          (s."extend-shallow@^3.0.2")
          (s."safe-regex@^1.1.0")
          ];
        "regex-not@^1.0.0" = s."regex-not@1.0.2";
        "regex-not@^1.0.2" = s."regex-not@1.0.2";
        "regexpu-core@4.7.0" = f "regexpu-core" "4.7.0" y "fcbf458c50431b0bb7b45d6967b8192d91f3d938" [
          (s."regenerate@^1.4.0")
          (s."regenerate-unicode-properties@^8.2.0")
          (s."regjsgen@^0.5.1")
          (s."regjsparser@^0.6.4")
          (s."unicode-match-property-ecmascript@^1.0.4")
          (s."unicode-match-property-value-ecmascript@^1.2.0")
          ];
        "regexpu-core@^4.7.0" = s."regexpu-core@4.7.0";
        "regjsgen@0.5.2" = f "regjsgen" "0.5.2" y "92ff295fb1deecbf6ecdab2543d207e91aa33733" [];
        "regjsgen@^0.5.1" = s."regjsgen@0.5.2";
        "regjsparser@0.6.4" = f "regjsparser" "0.6.4" y "a769f8684308401a66e9b529d2436ff4d0666272" [
          (s."jsesc@~0.5.0")
          ];
        "regjsparser@^0.6.4" = s."regjsparser@0.6.4";
        "remove-trailing-separator@1.1.0" = f "remove-trailing-separator" "1.1.0" y "c24bce2a283adad5bc3f58e0d48249b92379d8ef" [];
        "remove-trailing-separator@^1.0.1" = s."remove-trailing-separator@1.1.0";
        "repeat-element@1.1.3" = f "repeat-element" "1.1.3" y "782e0d825c0c5a3bb39731f84efee6b742e6b1ce" [];
        "repeat-element@^1.1.2" = s."repeat-element@1.1.3";
        "repeat-string@1.6.1" = f "repeat-string" "1.6.1" y "8dcae470e1c88abc2d600fff4a776286da75e637" [];
        "repeat-string@^1.6.1" = s."repeat-string@1.6.1";
        "request-promise-core@1.1.4" = f "request-promise-core" "1.1.4" y "3eedd4223208d419867b78ce815167d10593a22f" [
          (s."lodash@^4.17.19")
          ];
        "request-promise@4.2.6" = f "request-promise" "4.2.6" y "7e7e5b9578630e6f598e3813c0f8eb342a27f0a2" [
          (s."bluebird@^3.5.0")
          (s."request-promise-core@1.1.4")
          (s."stealthy-require@^1.1.1")
          (s."tough-cookie@^2.3.3")
          ];
        "request-promise@^4.2.4" = s."request-promise@4.2.6";
        "request@2.88.2" = f "request" "2.88.2" y "d73c918731cb5a87da047e207234146f664d12b3" [
          (s."aws-sign2@~0.7.0")
          (s."aws4@^1.8.0")
          (s."caseless@~0.12.0")
          (s."combined-stream@~1.0.6")
          (s."extend@~3.0.2")
          (s."forever-agent@~0.6.1")
          (s."form-data@~2.3.2")
          (s."har-validator@~5.1.3")
          (s."http-signature@~1.2.0")
          (s."is-typedarray@~1.0.0")
          (s."isstream@~0.1.2")
          (s."json-stringify-safe@~5.0.1")
          (s."mime-types@~2.1.19")
          (s."oauth-sign@~0.9.0")
          (s."performance-now@^2.1.0")
          (s."qs@~6.5.2")
          (s."safe-buffer@^5.1.2")
          (s."tough-cookie@~2.5.0")
          (s."tunnel-agent@^0.6.0")
          (s."uuid@^3.3.2")
          ];
        "request@^2.86.0" = s."request@2.88.2";
        "request@^2.88.0" = s."request@2.88.2";
        "require-directory@2.1.1" = f "require-directory" "2.1.1" y "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42" [];
        "require-directory@^2.1.1" = s."require-directory@2.1.1";
        "require-main-filename@2.0.0" = f "require-main-filename" "2.0.0" y "d0b329ecc7cc0f61649f62215be69af54aa8989b" [];
        "require-main-filename@^2.0.0" = s."require-main-filename@2.0.0";
        "resolve-cwd@2.0.0" = f "resolve-cwd" "2.0.0" y "00a9f7387556e27038eae232caa372a6a59b665a" [
          (s."resolve-from@^3.0.0")
          ];
        "resolve-cwd@^2.0.0" = s."resolve-cwd@2.0.0";
        "resolve-dir@1.0.1" = f "resolve-dir" "1.0.1" y "79a40644c362be82f26effe739c9bb5382046f43" [
          (s."expand-tilde@^2.0.0")
          ];
        "resolve-dir@^1.0.0" = s."resolve-dir@1.0.1";
        "resolve-dir@^1.0.1" = s."resolve-dir@1.0.1";
        "resolve-from@3.0.0" = f "resolve-from" "3.0.0" y "b22c7af7d9d6881bc8b6e653335eebcb0a188748" [];
        "resolve-from@^3.0.0" = s."resolve-from@3.0.0";
        "resolve-url@0.2.1" = f "resolve-url" "0.2.1" y "2c637fe77c893afd2a663fe21aa9080068e2052a" [];
        "resolve-url@^0.2.1" = s."resolve-url@0.2.1";
        "resolve@1.17.0" = f "resolve" "1.17.0" y "b25941b54968231cc2d1bb76a79cb7f2c0bf8444" [
          (s."path-parse@^1.0.6")
          ];
        "resolve@^1.3.2" = s."resolve@1.17.0";
        "ret@0.1.15" = f "ret" "0.1.15" y "b8a4825d5bdb1fc3f6f53c2bc33f81388681c7bc" [];
        "ret@~0.1.10" = s."ret@0.1.15";
        "rimraf@2.6.3" = f "rimraf" "2.6.3" y "b2d104fe0d8fb27cf9e0a1cda8262dd3833c6cab" [
          (s."glob@^7.1.3")
          ];
        "rimraf@2.7.1" = f "rimraf" "2.7.1" y "35797f13a7fdadc566142c29d4f07ccad483e3ec" [
          (s."glob@^7.1.3")
          ];
        "rimraf@^2.5.4" = s."rimraf@2.7.1";
        "rimraf@^2.6.1" = s."rimraf@2.7.1";
        "rimraf@^2.6.3" = s."rimraf@2.7.1";
        "rimraf@~2.6.2" = s."rimraf@2.6.3";
        "ripemd160@2.0.2" = f "ripemd160" "2.0.2" y "a1c1a6f624751577ba5d07914cbc92850585890c" [
          (s."hash-base@^3.0.0")
          (s."inherits@^2.0.1")
          ];
        "ripemd160@^2.0.0" = s."ripemd160@2.0.2";
        "ripemd160@^2.0.1" = s."ripemd160@2.0.2";
        "run-queue@1.0.3" = f "run-queue" "1.0.3" y "e848396f057d223f24386924618e25694161ec47" [
          (s."aproba@^1.1.1")
          ];
        "run-queue@^1.0.0" = s."run-queue@1.0.3";
        "run-queue@^1.0.3" = s."run-queue@1.0.3";
        "safe-buffer@5.1.2" = f "safe-buffer" "5.1.2" y "991ec69d296e0313747d59bdfd2b745c35f8828d" [];
        "safe-buffer@5.2.1" = f "safe-buffer" "5.2.1" y "1eaf9fa9bdb1fdd4ec75f58f9cdb4e6b7827eec6" [];
        "safe-buffer@^5.0.1" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.0" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.1" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.2" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.2.0" = s."safe-buffer@5.2.1";
        "safe-buffer@~5.1.0" = s."safe-buffer@5.1.2";
        "safe-buffer@~5.1.1" = s."safe-buffer@5.1.2";
        "safe-buffer@~5.2.0" = s."safe-buffer@5.2.1";
        "safe-regex@1.1.0" = f "safe-regex" "1.1.0" y "40a3669f3b077d1e943d44629e157dd48023bf2e" [
          (s."ret@~0.1.10")
          ];
        "safe-regex@^1.1.0" = s."safe-regex@1.1.0";
        "safer-buffer@2.1.2" = f "safer-buffer" "2.1.2" y "44fa161b0187b9549dd84bb91802f9bd8385cd6a" [];
        "safer-buffer@>= 2.1.2 < 3" = s."safer-buffer@2.1.2";
        "safer-buffer@^2.0.2" = s."safer-buffer@2.1.2";
        "safer-buffer@^2.1.0" = s."safer-buffer@2.1.2";
        "safer-buffer@~2.1.0" = s."safer-buffer@2.1.2";
        "schema-utils@1.0.0" = f "schema-utils" "1.0.0" y "0b79a93204d7b600d4b2850d1f66c2a34951c770" [
          (s."ajv@^6.1.0")
          (s."ajv-errors@^1.0.0")
          (s."ajv-keywords@^3.1.0")
          ];
        "schema-utils@2.7.1" = f "schema-utils" "2.7.1" y "1ca4f32d1b24c590c203b8e7a50bf0ea4cd394d7" [
          (s."@types/json-schema@^7.0.5")
          (s."ajv@^6.12.4")
          (s."ajv-keywords@^3.5.2")
          ];
        "schema-utils@^1.0.0" = s."schema-utils@1.0.0";
        "schema-utils@^2.6.5" = s."schema-utils@2.7.1";
        "semver@5.7.1" = f "semver" "5.7.1" y "a954f931aeba508d307bbf069eff0c01c96116f7" [];
        "semver@7.0.0" = f "semver" "7.0.0" y "5f3ca35761e47e05b206c6daff2cf814f0316b8e" [];
        "semver@^5.4.1" = s."semver@5.7.1";
        "semver@^5.5.0" = s."semver@5.7.1";
        "semver@^5.6.0" = s."semver@5.7.1";
        "send@0.17.1" = f "send" "0.17.1" y "c1d8b059f7900f7466dd4938bdc44e11ddb376c8" [
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."destroy@~1.0.4")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."etag@~1.8.1")
          (s."fresh@0.5.2")
          (s."http-errors@~1.7.2")
          (s."mime@1.6.0")
          (s."ms@2.1.1")
          (s."on-finished@~2.3.0")
          (s."range-parser@~1.2.1")
          (s."statuses@~1.5.0")
          ];
        "serialize-javascript@4.0.0" = f "serialize-javascript" "4.0.0" y "b525e1238489a5ecfc42afacc3fe99e666f4b1aa" [
          (s."randombytes@^2.1.0")
          ];
        "serialize-javascript@^4.0.0" = s."serialize-javascript@4.0.0";
        "serve-static@1.14.1" = f "serve-static" "1.14.1" y "666e636dc4f010f7ef29970a88a674320898b2f9" [
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."parseurl@~1.3.3")
          (s."send@0.17.1")
          ];
        "set-blocking@2.0.0" = f "set-blocking" "2.0.0" y "045f9782d011ae9a6803ddd382b24392b3d890f7" [];
        "set-blocking@^2.0.0" = s."set-blocking@2.0.0";
        "set-value@2.0.1" = f "set-value" "2.0.1" y "a18d40530e6f07de4228c7defe4227af8cad005b" [
          (s."extend-shallow@^2.0.1")
          (s."is-extendable@^0.1.1")
          (s."is-plain-object@^2.0.3")
          (s."split-string@^3.0.1")
          ];
        "set-value@^2.0.0" = s."set-value@2.0.1";
        "set-value@^2.0.1" = s."set-value@2.0.1";
        "setimmediate@1.0.5" = f "setimmediate" "1.0.5" y "290cbb232e306942d7d7ea9b83732ab7856f8285" [];
        "setimmediate@^1.0.4" = s."setimmediate@1.0.5";
        "setprototypeof@1.1.1" = f "setprototypeof" "1.1.1" y "7e95acb24aa92f5885e0abef5ba131330d4ae683" [];
        "sha.js@2.4.11" = f "sha.js" "2.4.11" y "37a5cf0b81ecbc6943de109ba2960d1b26584ae7" [
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "sha.js@^2.4.0" = s."sha.js@2.4.11";
        "sha.js@^2.4.8" = s."sha.js@2.4.11";
        "shebang-command@1.2.0" = f "shebang-command" "1.2.0" y "44aac65b695b03398968c39f363fee5deafdf1ea" [
          (s."shebang-regex@^1.0.0")
          ];
        "shebang-command@2.0.0" = f "shebang-command" "2.0.0" y "ccd0af4f8835fbdc265b82461aaf0c36663f34ea" [
          (s."shebang-regex@^3.0.0")
          ];
        "shebang-command@^1.2.0" = s."shebang-command@1.2.0";
        "shebang-command@^2.0.0" = s."shebang-command@2.0.0";
        "shebang-regex@1.0.0" = f "shebang-regex" "1.0.0" y "da42f49740c0b42db2ca9728571cb190c98efea3" [];
        "shebang-regex@3.0.0" = f "shebang-regex" "3.0.0" y "ae16f1644d873ecad843b0307b143362d4c42172" [];
        "shebang-regex@^1.0.0" = s."shebang-regex@1.0.0";
        "shebang-regex@^3.0.0" = s."shebang-regex@3.0.0";
        "snapdragon-node@2.1.1" = f "snapdragon-node" "2.1.1" y "6c175f86ff14bdb0724563e8f3c1b021a286853b" [
          (s."define-property@^1.0.0")
          (s."isobject@^3.0.0")
          (s."snapdragon-util@^3.0.1")
          ];
        "snapdragon-node@^2.0.1" = s."snapdragon-node@2.1.1";
        "snapdragon-util@3.0.1" = f "snapdragon-util" "3.0.1" y "f956479486f2acd79700693f6f7b805e45ab56e2" [
          (s."kind-of@^3.2.0")
          ];
        "snapdragon-util@^3.0.1" = s."snapdragon-util@3.0.1";
        "snapdragon@0.8.2" = f "snapdragon" "0.8.2" y "64922e7c565b0e14204ba1aa7d6964278d25182d" [
          (s."base@^0.11.1")
          (s."debug@^2.2.0")
          (s."define-property@^0.2.5")
          (s."extend-shallow@^2.0.1")
          (s."map-cache@^0.2.2")
          (s."source-map@^0.5.6")
          (s."source-map-resolve@^0.5.0")
          (s."use@^3.1.0")
          ];
        "snapdragon@^0.8.1" = s."snapdragon@0.8.2";
        "source-list-map@2.0.1" = f "source-list-map" "2.0.1" y "3993bd873bfc48479cca9ea3a547835c7c154b34" [];
        "source-list-map@^2.0.0" = s."source-list-map@2.0.1";
        "source-map-resolve@0.5.3" = f "source-map-resolve" "0.5.3" y "190866bece7553e1f8f267a2ee82c606b5509a1a" [
          (s."atob@^2.1.2")
          (s."decode-uri-component@^0.2.0")
          (s."resolve-url@^0.2.1")
          (s."source-map-url@^0.4.0")
          (s."urix@^0.1.0")
          ];
        "source-map-resolve@^0.5.0" = s."source-map-resolve@0.5.3";
        "source-map-support@0.5.19" = f "source-map-support" "0.5.19" y "a98b62f86dcaf4f67399648c085291ab9e8fed61" [
          (s."buffer-from@^1.0.0")
          (s."source-map@^0.6.0")
          ];
        "source-map-support@~0.5.12" = s."source-map-support@0.5.19";
        "source-map-url@0.4.0" = f "source-map-url" "0.4.0" y "3e935d7ddd73631b97659956d55128e87b5084a3" [];
        "source-map-url@^0.4.0" = s."source-map-url@0.4.0";
        "source-map@0.5.7" = f "source-map" "0.5.7" y "8a039d2d1021d22d1ea14c80d8ea468ba2ef3fcc" [];
        "source-map@0.6.1" = f "source-map" "0.6.1" y "74722af32e9614e9c287a8d0bbde48b5e2f1a263" [];
        "source-map@^0.5.0" = s."source-map@0.5.7";
        "source-map@^0.5.6" = s."source-map@0.5.7";
        "source-map@^0.6.0" = s."source-map@0.6.1";
        "source-map@^0.6.1" = s."source-map@0.6.1";
        "source-map@~0.6.0" = s."source-map@0.6.1";
        "source-map@~0.6.1" = s."source-map@0.6.1";
        "split-string@3.1.0" = f "split-string" "3.1.0" y "7cb09dda3a86585705c64b39a6466038682e8fe2" [
          (s."extend-shallow@^3.0.0")
          ];
        "split-string@^3.0.1" = s."split-string@3.1.0";
        "split-string@^3.0.2" = s."split-string@3.1.0";
        "split@1.0.1" = f "split" "1.0.1" y "605bd9be303aa59fb35f9229fbea0ddec9ea07d9" [
          (s."through@2")
          ];
        "split@^1.0.1" = s."split@1.0.1";
        "sshpk@1.16.1" = f "sshpk" "1.16.1" y "fb661c0bef29b39db40769ee39fa70093d6f6877" [
          (s."asn1@~0.2.3")
          (s."assert-plus@^1.0.0")
          (s."bcrypt-pbkdf@^1.0.0")
          (s."dashdash@^1.12.0")
          (s."ecc-jsbn@~0.1.1")
          (s."getpass@^0.1.1")
          (s."jsbn@~0.1.0")
          (s."safer-buffer@^2.0.2")
          (s."tweetnacl@~0.14.0")
          ];
        "sshpk@^1.7.0" = s."sshpk@1.16.1";
        "ssri@6.0.1" = f "ssri" "6.0.1" y "2a3c41b28dd45b62b63676ecb74001265ae9edd8" [
          (s."figgy-pudding@^3.5.1")
          ];
        "ssri@^6.0.1" = s."ssri@6.0.1";
        "static-extend@0.1.2" = f "static-extend" "0.1.2" y "60809c39cbff55337226fd5e0b520f341f1fb5c6" [
          (s."define-property@^0.2.5")
          (s."object-copy@^0.1.0")
          ];
        "static-extend@^0.1.1" = s."static-extend@0.1.2";
        "statuses@1.5.0" = f "statuses" "1.5.0" y "161c7dac177659fd9811f43771fa99381478628c" [];
        "statuses@>= 1.5.0 < 2" = s."statuses@1.5.0";
        "statuses@~1.5.0" = s."statuses@1.5.0";
        "stealthy-require@1.1.1" = f "stealthy-require" "1.1.1" y "35b09875b4ff49f26a777e509b3090a3226bf24b" [];
        "stealthy-require@^1.1.1" = s."stealthy-require@1.1.1";
        "stream-browserify@2.0.2" = f "stream-browserify" "2.0.2" y "87521d38a44aa7ee91ce1cd2a47df0cb49dd660b" [
          (s."inherits@~2.0.1")
          (s."readable-stream@^2.0.2")
          ];
        "stream-browserify@^2.0.1" = s."stream-browserify@2.0.2";
        "stream-each@1.2.3" = f "stream-each" "1.2.3" y "ebe27a0c389b04fbcc233642952e10731afa9bae" [
          (s."end-of-stream@^1.1.0")
          (s."stream-shift@^1.0.0")
          ];
        "stream-each@^1.1.0" = s."stream-each@1.2.3";
        "stream-http@2.8.3" = f "stream-http" "2.8.3" y "b2d242469288a5a27ec4fe8933acf623de6514fc" [
          (s."builtin-status-codes@^3.0.0")
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.3.6")
          (s."to-arraybuffer@^1.0.0")
          (s."xtend@^4.0.0")
          ];
        "stream-http@^2.7.2" = s."stream-http@2.8.3";
        "stream-shift@1.0.1" = f "stream-shift" "1.0.1" y "d7088281559ab2778424279b0877da3c392d5a3d" [];
        "stream-shift@^1.0.0" = s."stream-shift@1.0.1";
        "string-width@3.1.0" = f "string-width" "3.1.0" y "22767be21b62af1081574306f69ac51b62203961" [
          (s."emoji-regex@^7.0.1")
          (s."is-fullwidth-code-point@^2.0.0")
          (s."strip-ansi@^5.1.0")
          ];
        "string-width@^3.0.0" = s."string-width@3.1.0";
        "string-width@^3.1.0" = s."string-width@3.1.0";
        "string.prototype.trimend@1.0.1" = f "string.prototype.trimend" "1.0.1" y "85812a6b847ac002270f5808146064c995fb6913" [
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.17.5")
          ];
        "string.prototype.trimend@^1.0.1" = s."string.prototype.trimend@1.0.1";
        "string.prototype.trimstart@1.0.1" = f "string.prototype.trimstart" "1.0.1" y "14af6d9f34b053f7cfc89b72f8f2ee14b9039a54" [
          (s."define-properties@^1.1.3")
          ];
        "string.prototype.trimstart@^1.0.1" = s."string.prototype.trimstart@1.0.1";
        "string_decoder@0.10.31" = f "string_decoder" "0.10.31" y "62e203bc41766c6c28c9fc84301dab1c5310fa94" [];
        "string_decoder@1.1.1" = f "string_decoder" "1.1.1" y "9cf1611ba62685d7030ae9e4ba34149c3af03fc8" [
          (s."safe-buffer@~5.1.0")
          ];
        "string_decoder@1.3.0" = f "string_decoder" "1.3.0" y "42f114594a46cf1a8e30b0a84f56c78c3edac21e" [
          (s."safe-buffer@~5.2.0")
          ];
        "string_decoder@^1.0.0" = s."string_decoder@1.3.0";
        "string_decoder@^1.1.1" = s."string_decoder@1.3.0";
        "string_decoder@~0.10.x" = s."string_decoder@0.10.31";
        "string_decoder@~1.1.1" = s."string_decoder@1.1.1";
        "strip-ansi@5.2.0" = f "strip-ansi" "5.2.0" y "8c9a536feb6afc962bdfa5b104a5091c1ad9c0ae" [
          (s."ansi-regex@^4.1.0")
          ];
        "strip-ansi@^5.0.0" = s."strip-ansi@5.2.0";
        "strip-ansi@^5.1.0" = s."strip-ansi@5.2.0";
        "strip-ansi@^5.2.0" = s."strip-ansi@5.2.0";
        "sums@0.2.4" = f "sums" "0.2.4" y "d78c14398297d604fe6588dc3b03deca7b91ba93" [
          (s."babel-runtime@6.18.0")
          (s."concat-stream@1.5.2")
          (s."is-stream@1.1.0")
          (s."through2@2.0.1")
          (s."tmp@0.0.31")
          ];
        "supports-color@5.5.0" = f "supports-color" "5.5.0" y "e2e69a44ac8772f78a1ec0b35b689df6530efc8f" [
          (s."has-flag@^3.0.0")
          ];
        "supports-color@6.1.0" = f "supports-color" "6.1.0" y "0764abc69c63d5ac842dd4867e8d025e880df8f3" [
          (s."has-flag@^3.0.0")
          ];
        "supports-color@7.2.0" = f "supports-color" "7.2.0" y "1b7dcdcb32b8138801b3e478ba6a51caa89648da" [
          (s."has-flag@^4.0.0")
          ];
        "supports-color@^5.3.0" = s."supports-color@5.5.0";
        "supports-color@^5.4.0" = s."supports-color@5.5.0";
        "supports-color@^6.1.0" = s."supports-color@6.1.0";
        "supports-color@^7.1.0" = s."supports-color@7.2.0";
        "tapable@1.1.3" = f "tapable" "1.1.3" y "a1fccc06b58db61fd7a45da2da44f5f3a3e67ba2" [];
        "tapable@^1.0.0" = s."tapable@1.1.3";
        "tapable@^1.1.3" = s."tapable@1.1.3";
        "tar@4.4.13" = f "tar" "4.4.13" y "43b364bc52888d555298637b10d60790254ab525" [
          (s."chownr@^1.1.1")
          (s."fs-minipass@^1.2.5")
          (s."minipass@^2.8.6")
          (s."minizlib@^1.2.1")
          (s."mkdirp@^0.5.0")
          (s."safe-buffer@^5.1.2")
          (s."yallist@^3.0.3")
          ];
        "tar@^4.4.10" = s."tar@4.4.13";
        "temp@0.9.1" = f "temp" "0.9.1" y "2d666114fafa26966cd4065996d7ceedd4dd4697" [
          (s."rimraf@~2.6.2")
          ];
        "temp@^0.9.1" = s."temp@0.9.1";
        "terser-webpack-plugin@1.4.5" = f "terser-webpack-plugin" "1.4.5" y "a217aefaea330e734ffacb6120ec1fa312d6040b" [
          (s."cacache@^12.0.2")
          (s."find-cache-dir@^2.1.0")
          (s."is-wsl@^1.1.0")
          (s."schema-utils@^1.0.0")
          (s."serialize-javascript@^4.0.0")
          (s."source-map@^0.6.1")
          (s."terser@^4.1.2")
          (s."webpack-sources@^1.4.0")
          (s."worker-farm@^1.7.0")
          ];
        "terser-webpack-plugin@^1.4.3" = s."terser-webpack-plugin@1.4.5";
        "terser@4.8.0" = f "terser" "4.8.0" y "63056343d7c70bb29f3af665865a46fe03a0df17" [
          (s."commander@^2.20.0")
          (s."source-map@~0.6.1")
          (s."source-map-support@~0.5.12")
          ];
        "terser@^4.1.2" = s."terser@4.8.0";
        "through2@2.0.1" = f "through2" "2.0.1" y "384e75314d49f32de12eebb8136b8eb6b5d59da9" [
          (s."readable-stream@~2.0.0")
          (s."xtend@~4.0.0")
          ];
        "through2@2.0.5" = f "through2" "2.0.5" y "01c1e39eb31d07cb7d03a96a70823260b23132cd" [
          (s."readable-stream@~2.3.6")
          (s."xtend@~4.0.1")
          ];
        "through2@^2.0.0" = s."through2@2.0.5";
        "through@2" = s."through@2.3.8";
        "through@2.3.8" = f "through" "2.3.8" y "0dd4c9ffaabc357960b1b724115d7e0e86a2e1f5" [];
        "timers-browserify@2.0.11" = f "timers-browserify" "2.0.11" y "800b1f3eee272e5bc53ee465a04d0e804c31211f" [
          (s."setimmediate@^1.0.4")
          ];
        "timers-browserify@^2.0.4" = s."timers-browserify@2.0.11";
        "tmp@0.0.31" = f "tmp" "0.0.31" y "8f38ab9438e17315e5dbd8b3657e8bfb277ae4a7" [
          (s."os-tmpdir@~1.0.1")
          ];
        "to-arraybuffer@1.0.1" = f "to-arraybuffer" "1.0.1" y "7d229b1fcc637e466ca081180836a7aabff83f43" [];
        "to-arraybuffer@^1.0.0" = s."to-arraybuffer@1.0.1";
        "to-fast-properties@2.0.0" = f "to-fast-properties" "2.0.0" y "dc5e698cbd079265bc73e0377681a4e4e83f616e" [];
        "to-fast-properties@^2.0.0" = s."to-fast-properties@2.0.0";
        "to-object-path@0.3.0" = f "to-object-path" "0.3.0" y "297588b7b0e7e0ac08e04e672f85c1f4999e17af" [
          (s."kind-of@^3.0.2")
          ];
        "to-object-path@^0.3.0" = s."to-object-path@0.3.0";
        "to-regex-range@2.1.1" = f "to-regex-range" "2.1.1" y "7c80c17b9dfebe599e27367e0d4dd5590141db38" [
          (s."is-number@^3.0.0")
          (s."repeat-string@^1.6.1")
          ];
        "to-regex-range@5.0.1" = f "to-regex-range" "5.0.1" y "1648c44aae7c8d988a326018ed72f5b4dd0392e4" [
          (s."is-number@^7.0.0")
          ];
        "to-regex-range@^2.1.0" = s."to-regex-range@2.1.1";
        "to-regex-range@^5.0.1" = s."to-regex-range@5.0.1";
        "to-regex@3.0.2" = f "to-regex" "3.0.2" y "13cfdd9b336552f30b51f33a8ae1b42a7a7599ce" [
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."regex-not@^1.0.2")
          (s."safe-regex@^1.1.0")
          ];
        "to-regex@^3.0.1" = s."to-regex@3.0.2";
        "to-regex@^3.0.2" = s."to-regex@3.0.2";
        "toidentifier@1.0.0" = f "toidentifier" "1.0.0" y "7e1be3470f1e77948bc43d94a3c8f4d7752ba553" [];
        "tough-cookie@2.5.0" = f "tough-cookie" "2.5.0" y "cd9fb2a0aa1d5a12b473bd9fb96fa3dcff65ade2" [
          (s."psl@^1.1.28")
          (s."punycode@^2.1.1")
          ];
        "tough-cookie@^2.3.3" = s."tough-cookie@2.5.0";
        "tough-cookie@~2.5.0" = s."tough-cookie@2.5.0";
        "traverse-chain@0.1.0" = f "traverse-chain" "0.1.0" y "61dbc2d53b69ff6091a12a168fd7d433107e40f1" [];
        "traverse-chain@~0.1.0" = s."traverse-chain@0.1.0";
        "traverse@0.3.9" = f "traverse" "0.3.9" y "717b8f220cc0bb7b44e40514c22b2e8bbc70d8b9" [];
        "traverse@>=0.3.0 <0.4" = s."traverse@0.3.9";
        "tslib@1.13.0" = f "tslib" "1.13.0" y "c881e13cc7015894ed914862d276436fa9a47043" [];
        "tslib@^1.10.0" = s."tslib@1.13.0";
        "tslib@^1.9.0" = s."tslib@1.13.0";
        "tty-browserify@0.0.0" = f "tty-browserify" "0.0.0" y "a157ba402da24e9bf957f9aa69d524eed42901a6" [];
        "tunnel-agent@0.6.0" = f "tunnel-agent" "0.6.0" y "27a5dea06b36b04a0a9966774b290868f0fc40fd" [
          (s."safe-buffer@^5.0.1")
          ];
        "tunnel-agent@^0.6.0" = s."tunnel-agent@0.6.0";
        "tweetnacl@0.14.5" = f "tweetnacl" "0.14.5" y "5ae68177f192d4456269d108afa93ff8743f4f64" [];
        "tweetnacl@^0.14.3" = s."tweetnacl@0.14.5";
        "tweetnacl@~0.14.0" = s."tweetnacl@0.14.5";
        "type-is@1.6.18" = f "type-is" "1.6.18" y "4e552cd05df09467dcbc4ef739de89f2cf37c131" [
          (s."media-typer@0.3.0")
          (s."mime-types@~2.1.24")
          ];
        "type-is@~1.6.17" = s."type-is@1.6.18";
        "type-is@~1.6.18" = s."type-is@1.6.18";
        "typedarray@0.0.6" = f "typedarray" "0.0.6" y "867ac74e3864187b1d3d47d996a78ec5c8830777" [];
        "typedarray@^0.0.6" = s."typedarray@0.0.6";
        "typedarray@~0.0.5" = s."typedarray@0.0.6";
        "uglify-js@3.10.3" = f "uglify-js" "3.10.3" y "f0d2f99736c14de46d2d24649ba328be3e71c3bf" [];
        "uglify-js@^3.6.3" = s."uglify-js@3.10.3";
        "ultron@1.0.2" = f "ultron" "1.0.2" y "ace116ab557cd197386a4e88f4685378c8b2e4fa" [];
        "ultron@1.0.x" = s."ultron@1.0.2";
        "ultron@1.1.1" = f "ultron" "1.1.1" y "9fe1536a10a664a65266a1e3ccf85fd36302bc9c" [];
        "ultron@~1.1.0" = s."ultron@1.1.1";
        "unicode-canonical-property-names-ecmascript@1.0.4" = f "unicode-canonical-property-names-ecmascript" "1.0.4" y "2619800c4c825800efdd8343af7dd9933cbe2818" [];
        "unicode-canonical-property-names-ecmascript@^1.0.4" = s."unicode-canonical-property-names-ecmascript@1.0.4";
        "unicode-match-property-ecmascript@1.0.4" = f "unicode-match-property-ecmascript" "1.0.4" y "8ed2a32569961bce9227d09cd3ffbb8fed5f020c" [
          (s."unicode-canonical-property-names-ecmascript@^1.0.4")
          (s."unicode-property-aliases-ecmascript@^1.0.4")
          ];
        "unicode-match-property-ecmascript@^1.0.4" = s."unicode-match-property-ecmascript@1.0.4";
        "unicode-match-property-value-ecmascript@1.2.0" = f "unicode-match-property-value-ecmascript" "1.2.0" y "0d91f600eeeb3096aa962b1d6fc88876e64ea531" [];
        "unicode-match-property-value-ecmascript@^1.2.0" = s."unicode-match-property-value-ecmascript@1.2.0";
        "unicode-property-aliases-ecmascript@1.1.0" = f "unicode-property-aliases-ecmascript" "1.1.0" y "dd57a99f6207bedff4628abefb94c50db941c8f4" [];
        "unicode-property-aliases-ecmascript@^1.0.4" = s."unicode-property-aliases-ecmascript@1.1.0";
        "union-value@1.0.1" = f "union-value" "1.0.1" y "0b6fe7b835aecda61c6ea4d4f02c14221e109847" [
          (s."arr-union@^3.1.0")
          (s."get-value@^2.0.6")
          (s."is-extendable@^0.1.1")
          (s."set-value@^2.0.1")
          ];
        "union-value@^1.0.0" = s."union-value@1.0.1";
        "unique-filename@1.1.1" = f "unique-filename" "1.1.1" y "1d69769369ada0583103a1e6ae87681b56573230" [
          (s."unique-slug@^2.0.0")
          ];
        "unique-filename@^1.1.1" = s."unique-filename@1.1.1";
        "unique-slug@2.0.2" = f "unique-slug" "2.0.2" y "baabce91083fc64e945b0f3ad613e264f7cd4e6c" [
          (s."imurmurhash@^0.1.4")
          ];
        "unique-slug@^2.0.0" = s."unique-slug@2.0.2";
        "universalify@0.1.2" = f "universalify" "0.1.2" y "b646f69be3942dabcecc9d6639c80dc105efaa66" [];
        "universalify@^0.1.0" = s."universalify@0.1.2";
        "unpipe@1.0.0" = f "unpipe" "1.0.0" y "b2bf4ee8514aae6165b4817829d21b2ef49904ec" [];
        "unpipe@~1.0.0" = s."unpipe@1.0.0";
        "unset-value@1.0.0" = f "unset-value" "1.0.0" y "8376873f7d2335179ffb1e6fc3a8ed0dfc8ab559" [
          (s."has-value@^0.3.1")
          (s."isobject@^3.0.0")
          ];
        "unset-value@^1.0.0" = s."unset-value@1.0.0";
        "unzip-stream@0.3.1" = f "unzip-stream" "0.3.1" y "2333b5cd035d29db86fb701ca212cf8517400083" [
          (s."binary@^0.3.0")
          (s."mkdirp@^0.5.1")
          ];
        "unzip-stream@^0.3.0" = s."unzip-stream@0.3.1";
        "upath@1.2.0" = f "upath" "1.2.0" y "8f66dbcd55a883acdae4408af8b035a5044c1894" [];
        "upath@^1.1.1" = s."upath@1.2.0";
        "uri-js@4.4.0" = f "uri-js" "4.4.0" y "aa714261de793e8a82347a7bcc9ce74e86f28602" [
          (s."punycode@^2.1.0")
          ];
        "uri-js@^4.2.2" = s."uri-js@4.4.0";
        "urix@0.1.0" = f "urix" "0.1.0" y "da937f7a62e21fec1fd18d49b35c2935067a6c72" [];
        "urix@^0.1.0" = s."urix@0.1.0";
        "url@0.11.0" = f "url" "0.11.0" y "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1" [
          (s."punycode@1.3.2")
          (s."querystring@0.2.0")
          ];
        "url@^0.11.0" = s."url@0.11.0";
        "use@3.1.1" = f "use" "3.1.1" y "d50c8cac79a19fbc20f2911f56eb973f4e10070f" [];
        "use@^3.1.0" = s."use@3.1.1";
        "util-deprecate@1.0.2" = f "util-deprecate" "1.0.2" y "450d4dc9fa70de732762fbd2d4a28981419a0ccf" [];
        "util-deprecate@^1.0.1" = s."util-deprecate@1.0.2";
        "util-deprecate@~1.0.1" = s."util-deprecate@1.0.2";
        "util@0.10.3" = f "util" "0.10.3" y "7afb1afe50805246489e3db7fe0ed379336ac0f9" [
          (s."inherits@2.0.1")
          ];
        "util@0.11.1" = f "util" "0.11.1" y "3236733720ec64bb27f6e26f421aaa2e1b588d61" [
          (s."inherits@2.0.3")
          ];
        "util@^0.11.0" = s."util@0.11.1";
        "utils-merge@1.0.1" = f "utils-merge" "1.0.1" y "9f95710f50a267947b2ccc124741c1028427e713" [];
        "uuid@3.4.0" = f "uuid" "3.4.0" y "b23e4358afa8a202fe7a100af1f5f883f02007ee" [];
        "uuid@^3.3.2" = s."uuid@3.4.0";
        "v8-compile-cache@2.1.1" = f "v8-compile-cache" "2.1.1" y "54bc3cdd43317bca91e35dcaf305b1a7237de745" [];
        "v8-compile-cache@^2.1.1" = s."v8-compile-cache@2.1.1";
        "vary@1.1.2" = f "vary" "1.1.2" y "2299f02c6ded30d4a5961b0b9f74524a18f634fc" [];
        "vary@~1.1.2" = s."vary@1.1.2";
        "verror@1.10.0" = f "verror" "1.10.0" y "3a105ca17053af55d6e270c1f8288682e18da400" [
          (s."assert-plus@^1.0.0")
          (s."core-util-is@1.0.2")
          (s."extsprintf@^1.2.0")
          ];
        "vm-browserify@1.1.2" = f "vm-browserify" "1.1.2" y "78641c488b8e6ca91a75f511e7a3b32a86e5dda0" [];
        "vm-browserify@^1.0.1" = s."vm-browserify@1.1.2";
        "watchpack-chokidar2@2.0.0" = f "watchpack-chokidar2" "2.0.0" y "9948a1866cbbd6cb824dea13a7ed691f6c8ddff0" [
          (s."chokidar@^2.1.8")
          ];
        "watchpack-chokidar2@^2.0.0" = s."watchpack-chokidar2@2.0.0";
        "watchpack@1.7.4" = f "watchpack" "1.7.4" y "6e9da53b3c80bb2d6508188f5b200410866cd30b" [
          (s."graceful-fs@^4.1.2")
          (s."neo-async@^2.5.0")
          (s."chokidar@^3.4.1")
          (s."watchpack-chokidar2@^2.0.0")
          ];
        "watchpack@^1.7.4" = s."watchpack@1.7.4";
        "webpack-cli@3.3.12" = f "webpack-cli" "3.3.12" y "94e9ada081453cd0aa609c99e500012fd3ad2d4a" [
          (s."chalk@^2.4.2")
          (s."cross-spawn@^6.0.5")
          (s."enhanced-resolve@^4.1.1")
          (s."findup-sync@^3.0.0")
          (s."global-modules@^2.0.0")
          (s."import-local@^2.0.0")
          (s."interpret@^1.4.0")
          (s."loader-utils@^1.4.0")
          (s."supports-color@^6.1.0")
          (s."v8-compile-cache@^2.1.1")
          (s."yargs@^13.3.2")
          ];
        "webpack-cli@^3.3.11" = s."webpack-cli@3.3.12";
        "webpack-sources@1.4.3" = f "webpack-sources" "1.4.3" y "eedd8ec0b928fbf1cbfe994e22d2d890f330a933" [
          (s."source-list-map@^2.0.0")
          (s."source-map@~0.6.1")
          ];
        "webpack-sources@^1.4.0" = s."webpack-sources@1.4.3";
        "webpack-sources@^1.4.1" = s."webpack-sources@1.4.3";
        "webpack@4.44.1" = f "webpack" "4.44.1" y "17e69fff9f321b8f117d1fda714edfc0b939cc21" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-module-context@1.9.0")
          (s."@webassemblyjs/wasm-edit@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          (s."acorn@^6.4.1")
          (s."ajv@^6.10.2")
          (s."ajv-keywords@^3.4.1")
          (s."chrome-trace-event@^1.0.2")
          (s."enhanced-resolve@^4.3.0")
          (s."eslint-scope@^4.0.3")
          (s."json-parse-better-errors@^1.0.2")
          (s."loader-runner@^2.4.0")
          (s."loader-utils@^1.2.3")
          (s."memory-fs@^0.4.1")
          (s."micromatch@^3.1.10")
          (s."mkdirp@^0.5.3")
          (s."neo-async@^2.6.1")
          (s."node-libs-browser@^2.2.1")
          (s."schema-utils@^1.0.0")
          (s."tapable@^1.1.3")
          (s."terser-webpack-plugin@^1.4.3")
          (s."watchpack@^1.7.4")
          (s."webpack-sources@^1.4.1")
          ];
        "webpack@^4.41.6" = s."webpack@4.44.1";
        "which-module@2.0.0" = f "which-module" "2.0.0" y "d9ef07dce77b9902b8a3a8fa4b31c3e3f7e6e87a" [];
        "which-module@^2.0.0" = s."which-module@2.0.0";
        "which@1.3.1" = f "which" "1.3.1" y "a45043d54f5805316da8d62f9f50918d3da70b0a" [
          (s."isexe@^2.0.0")
          ];
        "which@2.0.2" = f "which" "2.0.2" y "7c6a8dd0a636a0327e10b59c9286eee93f3f51b1" [
          (s."isexe@^2.0.0")
          ];
        "which@^1.2.14" = s."which@1.3.1";
        "which@^1.2.9" = s."which@1.3.1";
        "which@^1.3.1" = s."which@1.3.1";
        "which@^2.0.1" = s."which@2.0.2";
        "which@^2.0.2" = s."which@2.0.2";
        "worker-farm@1.7.0" = f "worker-farm" "1.7.0" y "26a94c5391bbca926152002f69b84a4bf772e5a8" [
          (s."errno@~0.1.7")
          ];
        "worker-farm@^1.7.0" = s."worker-farm@1.7.0";
        "wrap-ansi@5.1.0" = f "wrap-ansi" "5.1.0" y "1fd1f67235d5b6d0fee781056001bfb694c03b09" [
          (s."ansi-styles@^3.2.0")
          (s."string-width@^3.0.0")
          (s."strip-ansi@^5.0.0")
          ];
        "wrap-ansi@^5.1.0" = s."wrap-ansi@5.1.0";
        "wrappy@1" = s."wrappy@1.0.2";
        "wrappy@1.0.2" = f "wrappy" "1.0.2" y "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f" [];
        "ws@1.1.5" = f "ws" "1.1.5" y "cbd9e6e75e09fc5d2c90015f21f0c40875e0dd51" [
          (s."options@>=0.0.5")
          (s."ultron@1.0.x")
          ];
        "ws@3.3.1" = f "ws" "3.3.1" y "d97e34dee06a1190c61ac1e95f43cb60b78cf939" [
          (s."async-limiter@~1.0.0")
          (s."safe-buffer@~5.1.0")
          (s."ultron@~1.1.0")
          ];
        "ws@6.2.1" = f "ws" "6.2.1" y "442fdf0a47ed64f59b6a5d8ff130f4748ed524fb" [
          (s."async-limiter@~1.0.0")
          ];
        "ws@^1.0.0" = s."ws@1.1.5";
        "ws@^6.1.0" = s."ws@6.2.1";
        "xmlbuilder@15.1.1" = f "xmlbuilder" "15.1.1" y "9dcdce49eea66d8d10b42cae94a79c3c8d0c2ec5" [];
        "xmlbuilder@^15.1.0" = s."xmlbuilder@15.1.1";
        "xtend@4.0.2" = f "xtend" "4.0.2" y "bb72779f5fa465186b1f438f674fa347fdb5db54" [];
        "xtend@^4.0.0" = s."xtend@4.0.2";
        "xtend@~4.0.0" = s."xtend@4.0.2";
        "xtend@~4.0.1" = s."xtend@4.0.2";
        "y18n@4.0.0" = f "y18n" "4.0.0" y "95ef94f85ecc81d007c264e190a120f0a3c8566b" [];
        "y18n@^4.0.0" = s."y18n@4.0.0";
        "yallist@2.1.2" = f "yallist" "2.1.2" y "1c11f9218f076089a47dd512f93c6699a6a81d52" [];
        "yallist@3.1.1" = f "yallist" "3.1.1" y "dbb7daf9bfd8bac9ab45ebf602b8cbad0d5d08fd" [];
        "yallist@^2.1.2" = s."yallist@2.1.2";
        "yallist@^3.0.0" = s."yallist@3.1.1";
        "yallist@^3.0.2" = s."yallist@3.1.1";
        "yallist@^3.0.3" = s."yallist@3.1.1";
        "yargs-parser@13.1.2" = f "yargs-parser" "13.1.2" y "130f09702ebaeef2650d54ce6e3e5706f7a4fb38" [
          (s."camelcase@^5.0.0")
          (s."decamelize@^1.2.0")
          ];
        "yargs-parser@^13.1.2" = s."yargs-parser@13.1.2";
        "yargs@13.3.2" = f "yargs" "13.3.2" y "ad7ffefec1aa59565ac915f82dccb38a9c31a2dd" [
          (s."cliui@^5.0.0")
          (s."find-up@^3.0.0")
          (s."get-caller-file@^2.0.1")
          (s."require-directory@^2.1.1")
          (s."require-main-filename@^2.0.0")
          (s."set-blocking@^2.0.0")
          (s."string-width@^3.0.0")
          (s."which-module@^2.0.0")
          (s."y18n@^4.0.0")
          (s."yargs-parser@^13.1.2")
          ];
        "yargs@^13.3.2" = s."yargs@13.3.2";
        "yauzl@2.10.0" = f "yauzl" "2.10.0" y "c7eb17c93e112cb1086fa6d8e51fb0667b79a5f9" [
          (s."buffer-crc32@~0.2.3")
          (s."fd-slicer@~1.1.0")
          ];
        "yauzl@^2.10.0" = s."yauzl@2.10.0";
        }