#!/usr/bin/env fish
# (assume square pixels like in most modern displays)

## devices settings
# TODO would be nice to have these in separate file for sharing with other scripts etc
### tablet
set dev_id "XP-PEN STAR G640 Pen stylus"
set t_w 32000
set t_h 20000
### display
set d_w 3440
set d_h 1440

## helper functions
# "math max" is available in newer fish version (? > 3.1.2)
function max # evaluate two bc expression and return the bigger value
  set a "($argv[1])"
  set b "($argv[2])"
  set r (echo "if ($a > $b) $a else $b" | bc -l)
  echo $r
end

# "math min" is available in newer fish version (? > 3.1.2)
function min # evaluate two bc expression and return the bigger value
  set a "($argv[1])"
  set b "($argv[2])"
  set r (echo "if ($a < $b) $a else $b" | bc -l)
  echo $r
end

## selection
set sel (string split ' ' (xrectsel "%w %h %x %y"))
set s_w $sel[1]
set s_h $sel[2]
set s_x $sel[3]
set s_y $sel[4]

## adjust selection to match ratio
# enlarge to match tablet ratio
set s_w (max "$s_w" "$t_w / $t_h * $s_h")
set s_h (max "$s_h" "$t_h / $t_w * $s_w")
# shift to not exceed display bounds
set s_x (min "$s_x" "$d_w - $s_w")
set s_y (min "$s_y" "$d_h - $s_h")

## mapping
# (don't care about the ratio now, just map as it is)
# xinput matrix m: [0,1]^2 x R -> [0,1]^2 x R
# (affine transformation matrix)
# coordinates in vectors are: <x y scale>; usually scale=1
# (real coordinates: x/scale, y/scale)
#
# m(t_x/t_w, t_y/t_h, 1) = (d_x/d_w, d_y/d_h)
# m(0, 0, 1)             = (        s_x/d_w,       s_y/d_h)
# m(1, 1, 1)             = ((s_x + s_w)/d_w, (s_y+s_h)/d_h)
#
# following matrix will be enough:
#  / scale_x 0       offset_x \
#  | 0       scale_y offset_y |
#  \ 0       0       1        /
#
# offset_x + 0 * scale_x = s_x/d_w
# offset_x + 1 * scale_x = (s_x+s_w)/d_w
# (analogous for y)

set scale_x (math "$s_w / $d_w")
set scale_y (math "$s_h / $d_h")
set offset_x (math "$s_x / $d_w")
set offset_y (math "$s_y / $d_h")

## set xinput property to apply mapping
set prop_id "Coordinate Transformation Matrix"
xinput set-float-prop "$dev_id" "$prop_id" $scale_x 0 $offset_x 0 $scale_y $offset_y 0 0 1
