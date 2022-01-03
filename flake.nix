{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };
        inherit (pkgs) lib;
        hsPkgs = pkgs.haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = "ghc8107";
          modules = [
            ({ lib, ... }: {
              dontStrip = false;
              dontPatchELF = false;
              enableDeadCodeElimination = true;
              packages.hellsmack.writeHieFiles = true;
              packages.hellsmack.components.library.preBuild = lib.mkIf (self ? rev) ''
                export HELLSMACK_REV=${self.rev}
              '';
            })
          ];
        };
        hellsmack = hsPkgs.hellsmack;
      in
      {
        defaultApp = flake-utils.lib.mkApp {
          drv = hellsmack.components.exes.hellsmack;
          exePath = "/bin/hellsmack";
        };
        checks = {
          cabal = pkgs.linkFarmFromDrvs "hellsmack-cabal-tests"
            (builtins.filter lib.isDerivation (lib.attrValues hellsmack.checks));
          weeder = pkgs.runCommand "hellsmack-weeder"
            { buildInputs = [ (hsPkgs.tool "weeder" "2.2.0") ]; } ''
            mkdir -p $out
            export XDG_CACHE_HOME=$TMPDIR/cache
            weeder --config ${./weeder.dhall} \
              --hie-directory ${hellsmack.components.library.hie} \
              --hie-directory ${hellsmack.components.tests.tasty.hie}
          '';
          hlint = pkgs.runCommand "hellsmack-hlint"
            { buildInputs = [ (hsPkgs.tool "hlint" "latest") ]; } ''
            mkdir -p $out
            cd ${./.}
            hlint src app test
          '';
        };
        devShell = hsPkgs.shellFor {
          tools = { cabal = { }; };
          withHoogle = false;
          exactDeps = true;
        };

        packages = {
          binaries-Linux = hsPkgs.projectCross.musl64.hsPkgs.hellsmack.components.exes.hellsmack;
          binaries-Windows = hsPkgs.projectCross.mingwW64.hsPkgs.hellsmack.components.exes.hellsmack;
        };
      });
}
