#!/usr/bin/env bash
# tools/launcher/build.sh
set -euo pipefail
cd "$(dirname "$0")"

wails build -platform windows/amd64 -clean
mv build/bin/launcher.exe build/bin/trama-launcher-windows-amd64.exe

wails build -platform linux/amd64 -clean
mv build/bin/launcher build/bin/trama-launcher-linux-amd64
