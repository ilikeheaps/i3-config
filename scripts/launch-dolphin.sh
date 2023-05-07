#!/usr/bin/env bash
set -euo pipefail

dir="$PWD"

chosen=$(find "$dir" -maxdepth 1 -type d -printf "%P\n" | dmenu -p "$dir/")

dolphin --new-window "$chosen"
