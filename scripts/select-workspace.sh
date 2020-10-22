#!/bin/sh

i3-msg -t get_workspaces | jq -r .[].name | dmenu -l 30 -p "                                                                                                                                     select workspace:"
