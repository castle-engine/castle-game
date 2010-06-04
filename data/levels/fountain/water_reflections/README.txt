About making water_environment_map.dds:

  # 1. Add fountain_lights.wrl to fountain_final.wrl, to have the level lit.
  #    *Do not* turn on headlight in view3dscene (you don't want to record
  #    view-dependent light in universal env map).

  # 2. Open fountain_final.wrl level removing level box, item/creature stub objects:
  cd ~/sources/vrmlengine/trunk/castle/data/levels/fountain/
  castle-process-3d-model fountain_final.wrl | view3dscene -

  # 3. Use view3dscene Edit->Remove Geometry Node to remove first (at the ground)
  #    fountain base object, and remove water surface.

  # 4. Stand in the middle and save by Display->Screenshot as Cube Map DDS.
  #    I use 512 size for now.

