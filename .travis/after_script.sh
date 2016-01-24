#!/bin/bash
set -e

ls bin/*

case "$PLATFORM" in
"osx")
    ;;
"linux")
    ;;
*)
    echo "Invalid PLATFORM"
    exit 1
    ;;
esac
