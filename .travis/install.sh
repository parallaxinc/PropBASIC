#!/bin/bash
set -e

git submodule update --init --recursive

case "$PLATFORM" in
"osx")
    brew cask install lazarus
    ;;
"linux")
    ;;
*)
    echo "Invalid PLATFORM"
    exit 1
    ;;
esac

