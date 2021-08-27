#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

# By default we connect to testnet, but if you like to connect to mainnet, you
# can set CARDANO_NETWORK to "mainnet" in your environment.
export CARDANO_NETWORK="${CARDANO_NETWORK:-"testnet"}";

# Note: the database can grow to several gigabytes. If you like to keep it on a
# separate disk, make state/cardano-* a symlink to the desired location.
exec cardano-node run \
  --config        "$CARDANO_CONFIGURATION/cardano/$CARDANO_NETWORK-config.json" \
  --topology      "$CARDANO_CONFIGURATION/cardano/$CARDANO_NETWORK-topology.json" \
  --database-path state/cardano-$CARDANO_NETWORK \
  --socket-path   state/cardano-node.socket