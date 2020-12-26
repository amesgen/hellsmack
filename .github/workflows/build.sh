set -e

# these options are probably complete overkill...
CABAL_OPTIONS="-O2"
GHC_OPTIONS="-split-sections -fspecialise-aggressively -fexpose-all-unfoldings -flate-specialise"
if [ -z "$STATIC_BUILD" ]; then
    CABAL_OPTIONS+=" -fuse-openssl"
else
    CABAL_OPTIONS+=" --enable-executable-static -fuse-openssl"
fi

cabal update
cabal clean
cabal build exe:hellsmack $CABAL_OPTIONS --ghc-options="$GHC_OPTIONS"

ARTIFACTS_DIR=artifacts
mkdir -p $ARTIFACTS_DIR
BIN_PATH=$(find dist-newstyle -name 'hellsmack' -type f)
if [ -z "$STATIC_BUILD" ]; then
    UBUNTU_BIN_PATH=$ARTIFACTS_DIR/hellsmack-dynamic-ubuntu
    ARCH_BIN_PATH=$ARTIFACTS_DIR/hellsmack-dynamic-arch
    cp "$BIN_PATH" $UBUNTU_BIN_PATH
    cp "$BIN_PATH" $ARCH_BIN_PATH
    patchelf --replace-needed libpcre.so.3 libpcre.so.1 $ARCH_BIN_PATH
else
    STATIC_BIN_PATH=$ARTIFACTS_DIR/hellsmack-static
    cp "$BIN_PATH" $STATIC_BIN_PATH
    strip -s $STATIC_BIN_PATH
fi
