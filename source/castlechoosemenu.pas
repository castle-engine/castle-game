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
unit CastleChooseMenu;

interface

uses Classes, GLWindow, GL, GLU, GLExt;

{ Allows user to choose one item from MenuItems.
  Displays menu using TCastleMenu with ADrawUnderMenu background. }
function ChooseByMenu(ADrawUnderMenu: TDrawFunc;
  MenuItems: TStringList): Integer;

implementation

uses SysUtils, GLWinModes, KambiGLUtils, CastleInputs, GLWinMessages,
  CastleWindow, CastleGeneralMenu, CastlePlay, VectorMath, CastleTimeMessages;

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
  DrawUnderMenu: TDrawFunc;
  ChooseMenu: TChooseMenu;

procedure Draw2d(Draw2DData: Pointer);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  ChooseMenu.Draw;
end;

procedure Draw(Glwin: TGLWindow);
begin
  DrawUnderMenu(Glwin);

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@Draw2d, nil,
      0, Glwin.Width, 0, Glwin.Height);
  glPopAttrib;
end;

procedure EventDown(MouseEvent: boolean; Key: TKey;
  AMouseButton: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  ChooseMenu.KeyDown(Key, C);
  EventDown(false, Key, mbLeft);
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  ChooseMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  ChooseMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
  EventDown(true, K_None, Button);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  ChooseMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
end;

procedure Idle(Glwin: TGLWindow);
begin
  ChooseMenu.Idle(Glwin.IdleSpeed);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

function ChooseByMenu(ADrawUnderMenu: TDrawFunc;
  MenuItems: TStringList): Integer;
var
  SavedMode: TGLMode;
begin
  DrawUnderMenu := ADrawUnderMenu;

  ChooseMenu.Items.Assign(MenuItems);
  ChooseMenu.FixItemsAreas(Glw.Width, Glw.Height);

  SavedMode := TGLMode.Create(Glw, 0, true);
  try
    { This shouldn't change projection matrix anyway. }
    SavedMode.RestoreProjectionMatrix := false;

    SetStandardGLWindowState(Glw, @Draw, @CloseQuery, Glw.OnResize,
      nil, false, true { FPSActive should not be needed anymore, but I leave it. },
      false, K_None, #0, false, false);

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnMouseUp := @MouseUp;
    Glw.OnMouseMove := @MouseMove;
    Glw.OnIdle := @Idle;

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    GLWinMessagesTheme.RectColor[3] := 1.0;

    Selected := false;

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