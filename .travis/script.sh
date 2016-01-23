#!/bin/bash
set -e

BUILD_VERSION="0.0.0"
if [[ -n "$TRAVIS_TAG" ]]
then
    BUILD_VERSION="$TRAVIS_TAG"
fi

pushd src/
lazbuild PropBasic.lpi
tar cvzf PropBasic-$BUILD_VERSION-$PLATFORM.tgz
mv *.tgz ..
