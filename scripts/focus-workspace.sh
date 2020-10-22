#!/bin/sh

i3-msg workspace $($(dirname "$0")/select-workspace.sh)
