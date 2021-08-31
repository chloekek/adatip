#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-only

# This script is used by Hivemind.

set -efuo pipefail

# -c: Path to configuration file.
# -p: Prefix path, used for state.
# -e: Log errors to this file. We can also set it in nginx.conf, but nginx will
#     try to open it’s compile-time default path /var/log/nginx/error.log before
#     it reads the config file, and emit an alert, so we set it here instead.
exec nginx -c "$PWD"/scripts/nginx.conf -p state/nginx -e /dev/stderr
