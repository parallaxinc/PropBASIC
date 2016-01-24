#!/bin/bash
set -e

BUILD_VERSION="0.0.0"
if [[ -n "$TRAVIS_TAG" ]]
then
    BUILD_VERSION="$TRAVIS_TAG"
fi

BUILD_DIR="propbasic-$BUILD_VERSION-$PLATFORM"

# build executable

pushd src/

rm -f *.o *.ppu propbasic PropBasic

case "$PLATFORM" in
"osx")
    fpc -Tdarwin -opropbasic PropBasic.lpr
    ;;
"linux")
    fpc -Tlinux -opropbasic PropBasic.lpr
    ;;
*)
    echo "Invalid PLATFORM"
    exit 1
    ;;
esac
popd

# copy resources

rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR

cp -f src/propbasic $BUILD_DIR
cp -f doc/* $BUILD_DIR

tar cvzf $BUILD_DIR.tgz $BUILD_DIR
