{
  Copyright 2006-2013 Michalis Kamburelis.

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

{ Sets initial OnResize and OnDraw and draws intro. }
procedure DrawInitialBackground;

implementation

uses SysUtils, CastleWindow, CastleGLUtils, CastleGLImages, CastleUIControls,
  GameWindow, CastleFilesUtils, CastleImages;

var
  GLScreenImage: TGLImage;

procedure Resize(Window: TCastleWindowBase);
begin
  OrthoProjection(0, Window.Width, 0, Window.Height);
end;

procedure Draw(Window: TCastleWindowBase);
begin
  GLScreenImage.Draw(0, 0);
end;

procedure DrawInitialBackground;
begin
  Window.OnResize := @Resize;
  Window.OnDraw := @Draw;
  Window.EventResize;
  Window.PostRedisplay;
  Window.FlushRedisplay;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen;
begin
  GLScreenImage := TGLImage.Create(
    ApplicationData('menu_bg/initial_background.png'),
    [TRGBImage], Window.Width, Window.Height, riBilinear);
end;

procedure WindowClose;
begin
  FreeAndNil(GLScreenImage);
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
finalization
end.
