#!/bin/bash
set -e

git submodule update --init --recursive

case "$PLATFORM" in
"osx")
    brew install lazarus
    ;;
"linux")
    sudo apt-get update
    sudo -E apt-get -yq --no-install-suggests --no-install-recommends --force-yes install lazarus
    ;;
*)
    echo "Invalid PLATFORM"
    exit 1
    ;;
esac

