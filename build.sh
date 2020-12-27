set -e

# these options are probably complete overkill...
CABAL_OPTIONS="-O2"
GHC_OPTIONS="-fspecialise-aggressively -fexpose-all-unfoldings -flate-specialise"
if [[ "$OSTYPE" == "linux-"* ]]; then
    GHC_OPTIONS+=" -split-sections"
fi
if [ -z "$STATIC_BUILD" ]; then
    CABAL_OPTIONS+=" -fuse-openssl"
else
    CABAL_OPTIONS+=" --enable-executable-static"
fi

cabal update
cabal clean
cabal build exe:hellsmack $CABAL_OPTIONS --ghc-options="$GHC_OPTIONS"

ARTIFACTS_DIR=artifacts
mkdir -p $ARTIFACTS_DIR
BIN_PATH=$(find dist-newstyle \( -name hellsmack -o -name hellsmack.exe \) -type f)
if [ -z "$STATIC_BUILD" ]; then
    cp "$BIN_PATH" $ARTIFACTS_DIR/hellsmack-dynamic
else
    STATIC_BIN_PATH=$ARTIFACTS_DIR/hellsmack-static
    cp "$BIN_PATH" $STATIC_BIN_PATH
    strip -s $STATIC_BIN_PATH
fi
