fountain.kanim (and frames/ models)
were exported from Blender using kanim (castle-anim-frames now) exporter
( https://castle-engine.io/castle_animation_frames.php )
choosing "selected objects only" (when selected the fluid domain).

Note that resulting kanim is not structurally equal (fluid simulation
doesn't produce such file), so it cannot be converted to a single 3D model.
So:

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

  For some time, we had shader making cross-fading between 2 fountain
  models, to simulate seamless animation.
  No longer maintained, it wasn't that pretty anyway.
