{
  Copyright 2006-2023 Michalis Kamburelis.

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

uses Classes, CastleWindow, CastleUIControls, CastleImages,
  CastleOnScreenMenu;

type
  { Index of the item chosen by the user in ChooseByMenu. }
  TChosenEvent = procedure (const ChosenIndex: Integer) of object;

{ Allows user to choose one item from MenuItems.
  Displays menu using TCastleGameMenu.

  This returns immediately, and ChosenEvent is called once the user chooses.
  We cannot wait for the choice in a
  "repeat Application.ProcessMessage until Selected" loop:
  such loop would just hang on the web, where the browser must control
  the main loop. See https://castle-engine.io/web , "Known problems". }
procedure ChooseByMenu(MenuItems: TStringList; const ChosenEvent: TChosenEvent);

type
  TStateChooseMenu = class(TCastleView)
  strict private
    OldThemeWindow: String;
    PreviousMenu: TCastleOnScreenMenu;
  public
    { Called when user chooses an item. }
    ChosenEvent: TChosenEvent;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
  end;

var
  StateChooseMenu: TStateChooseMenu;

implementation

uses SysUtils,
  CastleGLUtils, CastleInputs,
  GameWindow, GameGeneralMenu, CastleVectors,
  CastleGameNotifications,
  CastleKeysMouse, CastleControls, CastleApplicationProperties;

type
  TChooseMenu = class(TCastleGameMenu)
  private
    procedure ClickItem(Sender: TObject);
  end;

var
  Selected: boolean;
  SelectedIndex: Integer;
  ChooseMenu: TChooseMenu;

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
  { Otherwise the dialogs don't look good,
    as mesage text would be mixed with the menu text underneath. }
  Theme.ImagesPersistent[tiWindow].Url := 'castle-data:/theme/WindowDark.png';

  Selected := false;
  PreviousMenu := Window.Controls.MakeSingle(TCastleOnScreenMenu, ChooseMenu)
    as TCastleOnScreenMenu;
end;

procedure TStateChooseMenu.Stop;
begin
  Window.Controls.MakeSingle(TCastleOnScreenMenu, PreviousMenu);
  Theme.ImagesPersistent[tiWindow].Url := OldThemeWindow;
  inherited;
end;

procedure TStateChooseMenu.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  Event: TChosenEvent;
begin
  inherited;

  if Selected then
  begin
    Selected := false;
    Event := ChosenEvent;
    ChosenEvent := nil;
    Container.PopView(Self);
    if Assigned(Event) then
      Event(SelectedIndex);
  end;
end;

procedure ChooseByMenu(MenuItems: TStringList; const ChosenEvent: TChosenEvent);
var
  I: Integer;
begin
  ChooseMenu.MenuItems.ClearControls;
  for I := 0 to MenuItems.Count - 1 do
    ChooseMenu.Add(MenuItems[I], @ChooseMenu.ClickItem);

  StateChooseMenu.ChosenEvent := ChosenEvent;
  Window.Container.PushView(StateChooseMenu);
  { Note: we do not wait here until the user chooses. See ChooseByMenu docs. }
end;

initialization
  ChooseMenu := TChooseMenu.Create(nil);
finalization
  FreeAndNil(ChooseMenu);
end.
