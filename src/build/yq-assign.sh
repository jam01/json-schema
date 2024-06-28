#!/usr/bin/env bash

# yq-assign.sh .version 1.0.0 pom.yaml

# https://github.com/mikefarah/yq/issues/515#issuecomment-1574420861
yq "$1 = \"$2\"" "$3" | diff -U0 -wbB "$3" - | patch "$3" -

# alt: sed
#sed -i -E "s|^(\s*version:) .* (# project\.version.*)$|\1 $2 \2|g" "$1"
