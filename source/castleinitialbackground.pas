{
  Copyright 2006-2011 Michalis Kamburelis.

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
unit CastleInitialBackground;

interface

{ Sets initial OnResize and OnDraw and draws intro. }
procedure DrawInitialBackground;

implementation

uses SysUtils, GL, GLU, GLWindow, KambiGLUtils, GLImages,
  CastleWindow, KambiFilesUtils, Images, CastleNotifications;

var
  GLList_ScreenImage: TGLuint;

procedure Resize(Window: TGLWindow);
begin
  ProjectionGLOrtho(0, Window.Width, 0, Window.Height);
end;

procedure Draw(Window: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);
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

procedure OpenWindow(Window: TGLWindow);
begin
  GLList_ScreenImage := LoadImageToDisplayList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'initial_background.png',
    [TRGBImage], [], Window.Width, Window.Height);
end;

procedure CloseWindow(Window: TGLWindow);
begin
  glFreeDisplayList(GLList_ScreenImage);
end;

initialization
  Window.OnOpenList.Add(@OpenWindow);
  Window.OnCloseList.Add(@CloseWindow);
finalization
end.