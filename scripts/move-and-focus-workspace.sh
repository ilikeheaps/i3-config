#!/bin/sh

# select-workspace.sh might steal focus (by dmenu window appearing over the cursor) so we mark the current container first
i3-msg "mark _switching_"

ws=$($(dirname "$0")/select-workspace.sh)

i3-msg "[con_mark=\"switching\"] focus; move container to workspace $ws; workspace $ws"
