# settings for tablet scripts

# xinput device id/name; obtained from "xinput list", confirmed with "xinput test <id>"
# numeric IDs seem to be somewhat randomly assigned on startup so better use name
dev_id="XP-PEN STAR G640 Pen stylus"

# tablet area in points (any unit is fine, the ratio is what's important)
W=32000
H=20000
# these points are actual maximum reported values (which drivers/etc later map to [0,1] range)
# (btw actual area is 16x10cm, not 6x4 inches)

# display area in pixels (any unit is fine, the ratio is what's important)
X=3440
Y=1440 
