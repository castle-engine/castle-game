{
  Copyright 2006-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit GameChooseMenu;

interface

uses Classes, CastleWindow, CastleUIControls, CastleUIState, CastleImages;

{ Allows user to choose one item from MenuItems.
  Displays menu using TCastleGameMenu. }
function ChooseByMenu(MenuItems: TStringList): Integer;

type
  TStateChooseMenu = class(TUIState)
  strict private
    OldThemeWindow: String;
  public
    procedure Start; override;
    procedure Stop; override;
  end;

var
  StateChooseMenu: TStateChooseMenu;

implementation

uses SysUtils, CastleControlsImages,
  CastleGLUtils, CastleInputs, CastleMessages,
  CastleOnScreenMenu, GameWindow, GameGeneralMenu, CastleVectors,
  CastleGameNotifications,
  CastleKeysMouse, CastleControls, CastleApplicationProperties;

var
  Selected: boolean;
  SelectedIndex: Integer;

type
  TChooseMenu = class(TCastleGameMenu)
  private
    procedure ClickItem(Sender: TObject);
  end;

procedure TChooseMenu.ClickItem(Sender: TObject);
begin
  Selected := true;
  SelectedIndex := CurrentItem;
end;

{ global things -------------------------------------------------------------- }

procedure TStateChooseMenu.Start;
begin
  inherited;

  OldThemeWindow := Theme.ImagesPersistent[tiWindow].Url;
  { Otherwise CastleMessages don't look good,
    as mesage text would be mixed with the menu text underneath. }
  Theme.ImagesPersistent[tiWindow].Url := 'castle-data:/theme/WindowDark.png';
end;

procedure TStateChooseMenu.Stop;
begin
  Theme.ImagesPersistent[tiWindow].Url := OldThemeWindow;
  inherited;
end;

var
  ChooseMenu: TChooseMenu;

function ChooseByMenu(MenuItems: TStringList): Integer;
var
  I: Integer;
  PreviousMenu: TCastleOnScreenMenu;
begin
  ChooseMenu.MenuItems.ClearControls;
  for I := 0 to MenuItems.Count - 1 do
    ChooseMenu.Add(MenuItems[I], @ChooseMenu.ClickItem);

  TUIState.Push(StateChooseMenu);
  try
    PreviousMenu := Window.Controls.MakeSingle(TCastleOnScreenMenu, ChooseMenu)
      as TCastleOnScreenMenu;

    Selected := false;
    repeat
      Application.ProcessMessage(true, true);
    until Selected;

    Window.Controls.MakeSingle(TCastleOnScreenMenu, PreviousMenu);

    Result := SelectedIndex;
  finally TUIState.Pop(StateChooseMenu) end;
end;

initialization
  ChooseMenu := TChooseMenu.Create(nil);
finalization
  FreeAndNil(ChooseMenu);
end.
