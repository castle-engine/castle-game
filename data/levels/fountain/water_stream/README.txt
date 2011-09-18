Export from Blender using kanim exporter
(http://castle-engine.sourceforge.net/kanim_format.php)
choosing "selected objects only" (when selected the fluid domain).

Run ./process_after_export.sh then.

Note that resulting kanim is not structurally equal (fluid simulation
doesn't produce such file), so it cannot be converted to a single VRML
like "kanim_to_interpolators coord_MOD_Cube water_stream.kanim water_stream_done.wrl".
This actually causes a lot of other troubles:

- You cannot easily add GLSL shader by VRML/X3D ComposedShader node
  (becase when kanim is not structurally equal, VRML/X3D node tree isn't
  merged, so with current implementation we would create new OpenGL
  GLSL resource for every animation frame).
  This could be fixed by sharing OpenGL GLSL names using shader filenames,
  but for now:

  A simple workaround is just to write your own rendering of the animation,
  that is: initialize and control your GLSL shader from Pascal code,
  not by the ComposedShader node.

  This is (TODO: will be) implemented in castleanimationtricks in
  TGLSLAnimation.

- Animation is blocky, and trying to remove middle frames from it
  (to conserve memory) makes it even more blocky. This is unfixable:
  since no structural equality, there's no way to interpolate between
  adjacent frames.

  So if you want to conserve kanim memory (or loading time) by removing
  frames, you have to simply make animation shorter. Which leads
  to the next problem...

- No way to make the animation looping. Again, the fact that there's
  no way to interpolate between animation frames rules out even some
  desperate ideas (like force looping by ease in/out ~10 first frames
  of animation into last 10 frames of animation).

  This is fixed by our TBlendedLoopingAnimation, which actually
  renders two copies of the animation with smart blending,
  such that the "seams" from lopping are hidden.
