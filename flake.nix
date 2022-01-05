{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.nur.url = "github:nix-community/NUR";
  # TODO remove
  inputs.nur-amesgen = {
    url = "github:amesgen/nur-packages";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix, nur, nur-amesgen, pre-commit-hooks }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            haskellNix.overlay
            (_: pkgs: {
              nur = import nur { nurpkgs = pkgs; inherit pkgs; }
                // { amesgen = nur-amesgen.packages.${system}; };
            })
          ];
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
            let ormolu = pkgs.nur.amesgen.ormolu; in
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
                hlint = hsPkgs.tool "hlint" "latest";
              };
            };
        };
        devShell = hsPkgs.shellFor {
          tools = { cabal = { }; };
          buildInputs = [ pkgs.nur.amesgen.cabal-docspec ];
          withHoogle = false;
          exactDeps = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

        packages = {
          binaries-Linux = hsPkgs.projectCross.musl64.hsPkgs.hellsmack.components.exes.hellsmack;
          binaries-Windows = hsPkgs.projectCross.mingwW64.hsPkgs.hellsmack.components.exes.hellsmack;
        };
      });
}
