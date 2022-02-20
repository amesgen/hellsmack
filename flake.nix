{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, nur, pre-commit-hooks }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay nur.overlay ];
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
          pre-commit-check =
            let ormolu = pkgs.nur.repos.amesgen.ormolu; in
            pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                ormolu = {
                  enable = true;
                  entry = lib.mkForce "${ormolu}/bin/ormolu -e --mode inplace";
                };
                hlint.enable = true;
              };
              tools = {
                inherit ormolu;
                hlint = pkgs.nur.repos.amesgen.hlint;
              };
            };
        };
        devShell = hsPkgs.shellFor {
          tools = { cabal = { }; };
          buildInputs = [ pkgs.nur.repos.amesgen.cabal-docspec ];
          withHoogle = false;
          exactDeps = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

        packages =
          lib.optionalAttrs pkgs.stdenv.isLinux
            {
              binaries-Linux = hsPkgs.projectCross.musl64.hsPkgs.hellsmack.components.exes.hellsmack;
              binaries-Windows = hsPkgs.projectCross.mingwW64.hsPkgs.hellsmack.components.exes.hellsmack;
            }
          // lib.optionalAttrs pkgs.stdenv.isDarwin {
            binaries-macOS = pkgs.runCommand "hellsmack-macOS"
              { buildInputs = [ pkgs.macdylibbundler ]; } ''
              mkdir -p $out/bin
              cp ${hellsmack.components.exes.hellsmack}/bin/hellsmack $out/bin/hellsmack
              dylibbundler -b \
                -x $out/bin/hellsmack \
                -d $out/bin \
                -p '@executable_path'
            '';
          };
      });
  nixConfig = {
    extra-substituters = [
      "https://hydra.iohk.io"
      "https://hellsmack.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hellsmack.cachix.org-1:VI4cB7n7t1sAyJhfVc0ncIgInCaTfXbv1/kCOAuLFGQ="
    ];
  };
}
