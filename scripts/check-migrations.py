#!/usr/bin/env python3

"""
Verify that all migrations are numbered, and that there is only one migration
per number. This forces you to consider whether a migration needs to be updated
to be compatible with a new migration that was merged earlier.
"""

import os
import re
import sys

from typing import Dict

migrations: Dict[int, str] = {}

for fname in os.listdir('database'):
    if re.fullmatch('[0-9]{4}-.*\\.sql', fname) is None:
        print(f'Error: Migration "{fname}" has an invalid name.')
        print('Names must start with a 4-digit number and dash, e.g. "0001-add-widgets-table.sql".')
        sys.exit(1)

    rev = int(fname.split('-', maxsplit=1)[0])
    if rev in migrations:
        print(f'Error: Migration {rev:04} is not unique:')
        print(f'- {migrations[rev]}')
        print(f'- {fname}')
        sys.exit(1)

    migrations[rev] = fname

for rev in range(1, 1 + len(migrations)):
    if rev not in migrations:
        print(f'Error: Migration {rev:04} is missing.')
        sys.exit(1)
