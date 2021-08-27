#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

# -c: Path to configuration file.
# -p: Prefix path, used for state.
exec nginx -c "$PWD"/scripts/nginx.conf -p state/nginx
