#!/bin/bash
set -eu

# Make vertex painting disabled, like it was when blender
# generated model. Why ? Because vertex painting costs a lot
# of memory when building animations.

sed --in-place --separate -e \
  's/MaterialBinding { value PER_VERTEX }/MaterialBinding { value PER_VERTEX_INDEXED }/ig' \
  *.wrl