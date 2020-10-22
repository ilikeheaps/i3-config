#!/bin/sh

ws=$($(dirname "$0")/select-workspace.sh)

i3-msg "move container to workspace $ws; workspace $ws"
