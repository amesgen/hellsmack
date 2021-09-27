let
  pkgs = import ./nix/pkgs.nix;
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hellsmack";
      src = ./.;
      keepGitDir = true;
    };
    compiler-nix-name = "ghc8107";
    modules = [
      ({ pkgs, ... }: {
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        packages.hellsmack.components.library.build-tools = pkgs.lib.mkForce
          [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
        packages.hellsmack.components.library.extraSrcFiles = [ ".git/**/*" ];
        packages.hellsmack.writeHieFiles = true;
      })
    ];
  };
  hellsmack = hsPkgs.hellsmack;
  hellsmackExe = hellsmack.components.exes.hellsmack;
in {
  inherit hellsmackExe;
  hellsmackTests = hellsmack.checks;
  dev.shell = hsPkgs.shellFor {
    tools = {
      cabal = "latest";
      ghcid = "latest";
      weeder = "latest";
    };
    withHoogle = false;
    exactDeps = true;
  };
  weeder = pkgs.runCommand "hellsmack-weeder" {
    buildInputs = [ (hsPkgs.tool "weeder" "latest") ];
  } ''
    mkdir -p $out
    export XDG_CACHE_HOME=$TMPDIR/cache
    weeder --config ${./weeder.dhall} \
      --hie-directory ${hellsmack.components.library.hie} \
      --hie-directory ${hellsmack.components.tests.tasty.hie} \
      | tee $out/weeder-log.txt || true
    diff ${./test/weeder-log.txt} $out/weeder-log.txt
  '';
  binaries = {
    Linux =
      hsPkgs.projectCross.musl64.hsPkgs.hellsmack.components.exes.hellsmack;
    macOS = pkgs.runCommand "hellsmack-macOS" {
      buildInputs = [ pkgs.macdylibbundler ];
    } ''
      mkdir -p $out/bin
      cp ${hellsmackExe}/bin/hellsmack $out/bin/hellsmack
      chmod 755 $out/bin/hellsmack
      dylibbundler -b \
        -x $out/bin/hellsmack \
        -d $out/bin \
        -p '@executable_path'
    '';
    Windows =
      hsPkgs.projectCross.mingwW64.hsPkgs.hellsmack.components.exes.hellsmack;
  };
}
