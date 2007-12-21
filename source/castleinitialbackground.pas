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

{ }
unit CastleInitialBackground;

interface

{ Sets initial OnResize and OnDraw and draws intro.

  OnDraw already does TimeMessagesDraw.
  You should remember to call TimeMessagesIdle if you will inherit
  our OnDraw (and e.g. draw start menu over it). }
procedure DrawInitialBackground;

implementation

uses SysUtils, OpenGLh, GLWindow, KambiGLUtils, GLImages,
  CastleWindow, KambiFilesUtils, Images, CastleTimeMessages;

var
  GLList_ScreenImage: TGLuint;

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);
  TimeMessagesDraw;
end;

procedure DrawInitialBackground;
begin
  Glw.OnResize := @Resize;
  Glw.OnDraw := @Draw;
  Glw.EventResize;
  Glw.PostRedisplay;
  Glw.FlushRedisplay;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  GLList_ScreenImage := LoadImageToDisplayList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'initial_background.png',
    [TRGBImage], [], Glw.Width, Glw.Height);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  glFreeDisplayList(GLList_ScreenImage);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.