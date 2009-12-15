{
  Copyright 2006,2007 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ This keeps global Glw (window) variable. }

unit CastleWindow;

interface

uses GLWindow, VRMLOpenGLRenderer, OpenGLTTFonts;

var
  { @noAutoLinkHere }
  Glw: TGLWindowNavigated;

var
  GLContextCache: TVRMLOpenGLRendererContextCache;

  { Just a generally usable OpenGL outline (3D) font. }
  Font3d: TGLOutlineFont;

implementation

uses SysUtils, VRMLNodes, GLAntiAliasing;

{ initialization / finalization ---------------------------------------------- }

const
  Font3dFamily = ffSans;
  Font3dBold = false;
  Font3dItalic = false;

procedure GLWindowInit(Glwin: TGLWindow);
begin
  Font3d := GLContextCache.Fonts_IncReference(
    Font3dFamily, Font3dBold, Font3dItalic,
    TNodeFontStyle_2.ClassTTF_Font(Font3dFamily, Font3dBold, Font3dItalic));

  AntiAliasingGLInit;
  AntiAliasingEnable;
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  if (GLContextCache <> nil) and (Font3d <> nil) then
  begin
    GLContextCache.Fonts_DecReference(Font3dFamily, Font3dBold, Font3dItalic);
    Font3d := nil;
  end;
end;

initialization
  Glw := TGLWindowNavigated.Create(nil);
  Glw.SetDemoOptions(K_None, #0, false);

  GLContextCache := TVRMLOpenGLRendererContextCache.Create;

  Glw.OnInitList.Add(@GLWindowInit);
  Glw.OnCloseList.Add(@GLWindowClose);
finalization
  { Fonts_DecReference must be called before freeing GLContextCache.
    It's called from Glw.Close. But Glw.Close may be called when
    FreeAndNil(Glw) below, so to make sure we call Fonts_DecReference
    (by our GLWindowClose) right now. }
  GLWindowClose(Glw);

  FreeAndNil(GLContextCache);
  FreeAndNil(Glw);
end.
