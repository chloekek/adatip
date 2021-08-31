#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

# By default we connect to testnet, but if you like to connect to mainnet, you
# can set CARDANO_NETWORK to "mainnet" in your environment.
export CARDANO_NETWORK="${CARDANO_NETWORK:-"testnet"}";

exec cardano-node run \
  --config        "$CARDANO_CONFIGURATION/$CARDANO_NETWORK-config.json" \
  --topology      "$CARDANO_CONFIGURATION/$CARDANO_NETWORK-topology.json" \
  `# Note: the database can grow to several gigabytes. If you like to keep it on
   # a separate disk, make state/cardano-* a symlink to the desired location.` \
  --database-path "state/cardano-$CARDANO_NETWORK" \
  --socket-path   state/cardano-node.socket \
  `# We use a non-standard port to not conflict with any nodes that
   # might already be running on the system.` \
  --port 8084
