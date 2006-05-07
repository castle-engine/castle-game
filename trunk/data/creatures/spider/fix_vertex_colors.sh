#!/bin/bash
set -eu

# TODO: this should be rather corrected with EmacsLisp macros,
# just like for level.

sed --in-place --separate -e \
  's/MaterialBinding { value PER_VERTEX_INDEXED }/MaterialBinding { value PER_VERTEX }/ig' \
  *.wrl