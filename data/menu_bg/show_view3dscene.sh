#!/bin/bash
set -eu

# Temporary add ../levels/basic_castle_lights.wrl to ../levels/basic_castle_final.wrl,
# then run this -- this shows view used to generate current menu splash screen.

view3dscene ../levels/tower/basic_castle_final.wrl \
  --geometry 800x600