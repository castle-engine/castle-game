{ Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is generally for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (under Debian, install libegl1-mesa-dev and libgles2-mesa-dev).
  Put "CastleGL" in uses clause to include the appropriate OpenGL API. }

{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef CASTLE_IOS} {$define OpenGLES} {$endif}
{$ifdef OpenGLES}
  {$define CastleGL := CastleGLES}
{$else}
  {$define CastleGL := GL, GLU, GLExt}
{$endif}

