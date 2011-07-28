For automatic normalmap creation using mk_normal_maps.sh and
process_textures.scm:

  These scripts work Ok, but for now I don't include their results
  as I don't enable bump mapping everywhere on all levels.

  Bump mapping will need some improvements to work seamlessly
  with "the castle" original levels (fog and multiple lights must also be handled),
  and still the levels turn out darker with bump mapping (as usually
  happens when applying more physically correct methods, things go darker...).
  All in all, I'm not sure whether it's worthy to try to turn bump mapping on
  new levels --- maybe I should only focus on designing new levels from
  the beginning with bump mapping in mind.

For automatic compressed DDS creation using mk_dds.sh and
process_textures.scm:

  This is only for testing, just to compare how level looks before/after
  compression.

Kambi
