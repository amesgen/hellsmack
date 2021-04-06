set -e

# these options are probably complete overkill...
CABAL_OPTIONS="-O2"
GHC_OPTIONS="-fspecialise-aggressively -fexpose-all-unfoldings -flate-specialise"
if [[ "$OSTYPE" == "linux-"* ]]; then
    GHC_OPTIONS+=" -split-sections"
fi
if ! [ -z "$STATIC_BUILD" ]; then
    CABAL_OPTIONS+=" --enable-executable-static"
fi

cabal update
cabal clean
cabal build exe:hellsmack $CABAL_OPTIONS --ghc-options="$GHC_OPTIONS"

ARTIFACTS_DIR=artifacts
mkdir -p $ARTIFACTS_DIR
BIN_PATH=$ARTIFACTS_DIR/hellsmack
cp $(find dist-newstyle \( -name hellsmack -o -name hellsmack.exe \) -type f) $BIN_PATH
if [ -z "$STATIC_BUILD" ]; then
    echo built dynamic binary
else
    strip -s $BIN_PATH
    echo built static binary
fi
