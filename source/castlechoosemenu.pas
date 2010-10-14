{
  Copyright 2006-2010 Michalis Kamburelis.

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
unit CastleChooseMenu;

interface

uses Classes, GLWindow, GL, GLU, UIControls;

{ Allows user to choose one item from MenuItems.
  Displays menu using TCastleMenu with ControlsUnder background. }
function ChooseByMenu(ControlsUnder: TUIControlList;
  MenuItems: TStringList): Integer;

implementation

uses SysUtils, GLWinModes, KambiGLUtils, CastleInputs, GLWinMessages, GLMenu,
  CastleWindow, CastleGeneralMenu, CastlePlay, VectorMath, CastleTimeMessages,
  KeysMouse;

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
  ChooseMenu: TChooseMenu;

procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0,
    AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
end;

procedure MouseWheel(Glwin: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
end;

procedure Idle(Glwin: TGLWindow);
begin
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

function ChooseByMenu(ControlsUnder: TUIControlList;
  MenuItems: TStringList): Integer;
var
  SavedMode: TGLMode;
begin
  ChooseMenu.Items.Assign(MenuItems);
  ChooseMenu.FixItemsAreas;

  SavedMode := TGLMode.CreateReset(Glw, 0, true,
    nil, Glw.OnResize, @CloseQuery,
    true { FPSActive should not be needed anymore, but I leave it. });
  try
    { This shouldn't change projection matrix anyway. }
    SavedMode.RestoreProjectionMatrix := false;

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnMouseWheel := @MouseWheel;
    Glw.OnIdle := @Idle;
    Glw.OnDrawStyle := ds3D;

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    GLWinMessagesTheme.RectColor[3] := 1.0;

    Glw.Controls.MakeSingle(TGLMenu, ChooseMenu);

    Glw.Controls.AddList(ControlsUnder);

    Selected := false;
    repeat
      Application.ProcessMessage(true);
    until Selected;

    Result := SelectedIndex;
  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  ChooseMenu := TChooseMenu.Create(nil);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  FreeAndNil(ChooseMenu);
end;

initialization
  Glw.OnInitList.Add(@InitGLW);
  Glw.OnCloseList.Add(@CloseGLW);
finalization
end.