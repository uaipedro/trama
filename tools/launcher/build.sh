#!/usr/bin/env bash
# tools/launcher/build.sh
set -euo pipefail
cd "$(dirname "$0")"

dist="build/dist"
mkdir -p "$dist"

wails build -platform windows/amd64 -clean
mv build/bin/launcher.exe "$dist/trama-launcher-windows-amd64.exe"

# Distros that only ship libwebkit2gtk-4.1-dev (4.0 was dropped, e.g. Ubuntu
# 24.04+) need this tag so Wails links against webkit2gtk-4.1 instead of its
# webkit2gtk-4.0 default.
wails build -platform linux/amd64 -clean -tags webkit2_41
mv build/bin/launcher "$dist/trama-launcher-linux-amd64"

echo "Binários gerados em $dist:"
ls -la "$dist"
