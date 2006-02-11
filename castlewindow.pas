{ This keeps global Glw variable.

  This is just like GLW_Navigated, but with some specific customizations
  for this project. }

unit CastleWindow;

interface

uses GLWindow;

var
  { @noAutoLinkHere }
  Glw: TGLWindowNavigated;

implementation

uses SysUtils;

initialization
  Glw := TGLWindowNavigated.Create;
finalization
  FreeAndNil(Glw);
end.
