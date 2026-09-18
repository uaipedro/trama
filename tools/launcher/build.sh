#!/usr/bin/env bash
# tools/launcher/build.sh
set -euo pipefail
cd "$(dirname "$0")"

dist="build/dist"
mkdir -p "$dist"

wails build -platform windows/amd64 -clean
mv build/bin/launcher.exe "$dist/trama-launcher-windows-amd64.exe"

wails build -platform linux/amd64 -clean
mv build/bin/launcher "$dist/trama-launcher-linux-amd64"

echo "Binários gerados em $dist:"
ls -la "$dist"
