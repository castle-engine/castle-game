{
  Copyright 2006 Michalis Kamburelis.

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
unit CastleChooseMenu;

interface

uses Classes, OpenGLh;

{ Allows user to choose one item from MenuItems.
  Displays menu using TCastleMenu with AGLList_ScreenImage background. }
function ChooseByMenu(AGLList_ScreenImage: TGLuint;
  MenuItems: TStringList): Integer;

implementation

uses SysUtils, GLWindow, GLWinModes, KambiGLUtils, CastleKeys, GLWinMessages,
  CastleWindow, CastleGeneralMenu, CastlePlay;

var
  Selected: boolean;
  SelectedIndex: Integer;

type
  TChooseMenu = class(TCastleMenu)
    procedure CurrentItemSelected; override;
  end;

procedure TChooseMenu.CurrentItemSelected;
begin
  inherited;
  
  Selected := true;
  SelectedIndex := CurrentItem;
end;

{ global things -------------------------------------------------------------- }

var
  GLList_ScreenImage: TGLuint;
  ChooseMenu: TChooseMenu;

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);

  ChooseMenu.Draw;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  ChooseMenu.KeyDown(Key, C);
  if CastleKey_SaveScreen.IsValue(Key) then
    SaveScreen;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  ChooseMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  ChooseMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  ChooseMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  ChooseMenu.Idle(Glwin.FpsCompSpeed);
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

function ChooseByMenu(AGLList_ScreenImage: TGLuint;
  MenuItems: TStringList): Integer;
var
  SavedMode: TGLMode;
begin
  GLList_ScreenImage := AGLList_ScreenImage;

  ChooseMenu.Items.Assign(MenuItems);
  ChooseMenu.FixItemsAreas(Glw.Width, Glw.Height);

  SavedMode := TGLMode.Create(Glw, GL_ENABLE_BIT, true);
  try
    SavedMode.FakeMouseDown := false;

    SetStandardGLWindowState(Glw, Draw, CloseQuery, Resize,
      nil, false, true { FPSActive is needed for FpsCompSpeed in Idle. },
      false, K_None, #0, false, false);

    Glw.OnKeyDown := KeyDown;
    Glw.OnMouseDown := MouseDown;
    Glw.OnMouseUp := MouseUp;
    Glw.OnMouseMove := MouseMove;
    Glw.OnIdle := Idle;

    Glw.EventResize;

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    GLWinMessagesTheme.RectColor[3] := 1.0;

    Selected := false;

    glDisable(GL_LIGHTING);

    repeat
      Glwm.ProcessMessage(true);
    until Selected;

    Result := SelectedIndex;
  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  ChooseMenu := TChooseMenu.Create;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  FreeAndNil(ChooseMenu);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.