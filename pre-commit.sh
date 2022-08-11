#!/bin/bash
#
# Place appropriately named executable hook scripts into this directory
# to intercept various actions that git takes.  See `git help hooks` for
# more information.

if [[ README.Rmd -nt README.md ]]; then
  echo "README.md is out of date; please re-knit README.Rmd"
  exit 1
fi
