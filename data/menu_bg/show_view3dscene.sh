#!/bin/bash
set -eu

# Temporary add ../basic_castle_lights.wrl to ../basic_castle_final.wrl,
# then run this -- this shows view used to generate current menu splash screen.

view3dscene ../basic_castle.wrl \
  --camera-pos 6.020590782165527 -97.895431518554687 5.877294063568115 \
  --camera-dir -0.299134641885757 0.60658472776413 0.183122679591179 \
  --camera-up 0.115590639412403 -0.234395816922188 0.965244650840759 \
  --geometry 800x600