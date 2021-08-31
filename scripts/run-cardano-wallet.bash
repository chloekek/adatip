#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

# By default we connect to testnet, but if you like to connect to mainnet, you
# can set CARDANO_NETWORK to "mainnet" in your environment.
export CARDANO_NETWORK="${CARDANO_NETWORK:-"testnet"}";

if [ $CARDANO_NETWORK = "mainnet" ]; then
  args=("--mainnet")
else
  args=("--testnet" "$CARDANO_CONFIGURATION/testnet-byron-genesis.json")
fi

exec cardano-wallet serve \
  --node-socket state/cardano-node.socket \
  --database "state/cardano-wallet-$CARDANO_NETWORK" \
  --port 8083 \
  "${args[@]}"
