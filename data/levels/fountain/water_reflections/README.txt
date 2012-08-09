About making water_environment_map.dds:

  # 1. Add fountain_lights.wrl to fountain_final.wrl, to have the level lit.
  #    *Do not* turn on headlight in view3dscene (you don't want to record
  #    view-dependent light in universal env map).

  # 2. Open fountain_final.wrl in view3dscene and remove level box
  #    (and item/creature and similar stub objects):
  #    use view3dscene menu item
  #    "Edit -> Remove placeholder nodes from "Castle Game Engine" levels".

  # 3. Use view3dscene "Edit->Remove Geometry Node" menu
  #    to remove first (at the ground)
  #    fountain base object, and also remove water surface.

  # 4. Stand in the middle and save by "Display->Screenshot as Cube Map DDS".
  #    I use 512 size for now.
