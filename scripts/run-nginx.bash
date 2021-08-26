#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

# Ok so for relative paths, Nginx uses the prefix path as prefix.
# But we want to have a path relative to the working directory.
# But inside the Nginx configuration, you can’t retrieve the working directory.
# So we generate a file that contains it absolute, and include it in nginx.conf.
echo "alias $PWD/adatipd/static/;" > state/nginx/alias_static.conf;

# -c: Path to configuration file.
# -p: Prefix path, used for state.
exec nginx -c "$PWD"/scripts/nginx.conf -p state/nginx
