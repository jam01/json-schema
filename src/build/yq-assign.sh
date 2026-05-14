#!/usr/bin/env bash
set -euo pipefail

# yq-assign.sh <yq-expr> <value> <file>
# Edits $3 in place, preserving comments/formatting via the yq+diff+patch trick.
# https://github.com/mikefarah/yq/issues/515#issuecomment-1574420861
# `diff` exits 1 when files differ — the expected case here — so neutralize that exit
# inside the pipeline. Real errors (diff exit ≥2, patch failure, yq failure) still abort.

yq "$1 = \"$2\"" "$3" \
  | { diff -U0 -wbB "$3" - || true; } \
  | patch "$3" -

# alt: sed
#sed -i -E "s|^(\s*version:) .* (# project\.version.*)$|\1 $2 \2|g" "$1"
