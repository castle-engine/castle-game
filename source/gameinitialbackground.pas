{
  Copyright 2006-2014 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ }
unit GameInitialBackground;

interface

{ Sets initial OnResize and OnRender and draws intro. }
procedure RenderInitialBackground;

implementation

uses SysUtils, CastleWindow, CastleGLUtils, CastleGLImages, CastleUIControls,
  GameWindow, CastleFilesUtils, CastleImages;

var
  GLScreenImage: TGLImage;

procedure Render(Container: TUIContainer);
begin
  GLScreenImage.Draw(0, 0);
end;

procedure RenderInitialBackground;
begin
  Window.OnRender := @Render;
  Window.RenderStyle := rs2D;
  Window.Invalidate;
  Window.FlushRedisplay;
end;

{ initialization / finalization ---------------------------------------------- }

procedure ContextOpen;
begin
  GLScreenImage := TGLImage.Create(
    ApplicationData('menu_bg/initial_background.png'),
    [TRGBImage], Window.Width, Window.Height, riBilinear);
end;

procedure ContextClose;
begin
  FreeAndNil(GLScreenImage);
end;

initialization
  OnGLContextOpen.Add(@ContextOpen);
  OnGLContextClose.Add(@ContextClose);
finalization
end.
