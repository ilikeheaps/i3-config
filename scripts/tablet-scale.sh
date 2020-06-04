#!/bin/sh
set -euo pipefail

# tablet-scale.sh -- select tablet area

# ------- CONSTANTS (set those before using) ------
# xinput device id/name; obtained from "xinput list", confirmed with "xinput test <id>"
# numeric IDs seem to be somewhat randomly assigned on startup so better use name
dev_id="HUION 420 Pen"
# tablet area in mm (any unit is fine, the ratio is what's important)
H=57
W=102
# display area in pixels (any unit is fine, the ratio is what's important)
X=1920
Y=1200

# ------- ARGUMENTS ------
scale=${1:-1}  # value from 1 to inf
# scale=1 will use maximal area of the tablet while maintaining ratio
# scale=k will scale that area by 1/k
# This can be used both for decreasing display area (scale>1) and decreasing tablet area (scale<1)

x_offset=${2:-0} # value from 0 to 1
# 0 will make the left tablet edge flush with the left display edge
# 1 will make the right tablet edge flush with the right display edge
# any value in between will shift the mapping linearly
y_offset=${3:-0} # value from 0 to 1
# 0 will make the upper tablet edge flush with the upper display edge
# 1 will make the lower tablet edge flush with the lower display edge
# any value in between will shift the mapping linearly

# ------- LOGIC -------
# adjusting aspect ratio (so that area on display will have the same ratio as tablet dimensions)
# if (W, H) is tablet area and (X, Y) is display area, then ratio scaling is given:
#   x' = x * max(1, W / X * Y / H)
#   y' = y * max(1, X / W * H / Y)
# Take maximum for cutting tablet area; minimum for cutting display area
x_scale=$(echo "s = ($W / $H * $Y / $X) ; if (s > 1) s else 1" | bc)
y_scale=$(echo "s = ($H / $W * $X / $Y) ; if (s > 1) s else 1" | bc)

# scale offsets from [0, 1] input range to correct values:
#   offset=(0,0) -> x=0, y=0 -> x'=0, y'=0
#   offset=(1,1) -> x=1, y=1 -> x'=1, y'=1
x_offset_m=$(echo "($scale - $x_scale) * ($x_offset)" | bc)
y_offset_m=$(echo "($scale - $y_scale) * ($y_offset)" | bc)

echo xinput set-float-prop $dev_id 157 $x_scale 0 $x_offset_m 0 $y_scale $y_offset_m 0 0 $scale
xinput set-float-prop "$dev_id" 157 $x_scale 0 $x_offset_m 0 $y_scale $y_offset_m 0 0 $scale
echo done
