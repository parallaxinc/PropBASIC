#!/bin/bash
set -e

ls src/PropBasic

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
