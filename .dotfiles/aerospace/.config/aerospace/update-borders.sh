#!/usr/bin/env bash
set -euo pipefail

# Count windows on the *focused* workspace
count="$(aerospace list-windows --workspace focused --count 2>/dev/null || echo 0)"
is_fullscreen="$(aerospace list-windows --focused --format '%{window-is-fullscreen}' 2>/dev/null || echo false)"

if [[ "${count}" -le 1 || "${is_fullscreen}" == true ]]; then
  # Hide borders (width 0 is effectively off)
  borders width=0
else
  # Show borders (tweak colors/width to taste)
  borders active_color=0xffd126f1 inactive_color=0xff494d64 width=5.0
fi
