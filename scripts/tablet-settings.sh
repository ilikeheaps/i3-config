# settings for tablet scripts

CONF="$(dirname $0)/tablet-configuration.json"

dev_id=$(jq -r ".tablet.device_id" "$CONF")

W=$(jq -r ".tablet.width" "$CONF")
H=$(jq -r ".tablet.height" "$CONF")

X=$(jq -r ".display.width" "$CONF")
Y=$(jq -r ".display.height" "$CONF")
