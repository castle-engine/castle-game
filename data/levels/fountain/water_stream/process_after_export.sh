#!/bin/bash
set -eu

# This makes blending look better
#sed --in-place --separate -e 's|solid FALSE|solid TRUE|' *.wrl

# Uncompressed files are really large, > 70 MB compared to 25~ MB compressed.
rm -f *.wrl.gz
gzip *.wrl
sed --in-place --separate -e 's|.wrl|.wrl.gz|' fountain.kanim
