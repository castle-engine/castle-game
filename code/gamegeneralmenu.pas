{
  Copyright 2006-2016 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit GameGeneralMenu;

interface

uses Classes,
  CastleVectors, CastleOnScreenMenu, CastleRectangles, CastleUIState;

type
  { On-screen menu suitable for castle.

    Note that CaptureAllEvents makes all events captured
    (covers the whole screen), as we don't need the focus / non-focus logic,
    menu is the only control used on the screen. }
  TCastleGameMenu = class(TCastleOnScreenMenu)
  public
  const
    DefaultCastleFontSize = 40;

    constructor Create(AOwner: TComponent); override;
    property DrawFocusedBorder default false;
    property CaptureAllEvents default true;
    property FontSize default DefaultCastleFontSize;

    { Sets position parameters.
      If Center = @false then sets position suitable for the background
      under start menu. Otherwise menu is in the center of the screen. }
    procedure SetPosition(const ACenter: boolean);
  end;

  TAbstractMenuState = class(TUIState)
  strict private
    FCurrentMenu: TCastleGameMenu;
    procedure SetCurrentMenu(const Value: TCastleGameMenu);
  protected
    { Set this to change currently displayed menu. }
    property CurrentMenu: TCastleGameMenu read FCurrentMenu write SetCurrentMenu;
  end;

implementation

uses SysUtils, CastleWindow, CastleUIControls, GameWindow;

{ TCastleGameMenu ---------------------------------------------------------------- }

constructor TCastleGameMenu.Create(AOwner: TComponent);
begin
  inherited;
  DrawFocusedBorder := false;
  CaptureAllEvents := true;
  FontSize := DefaultCastleFontSize;
  SetPosition(true); // center by default
end;

procedure TCastleGameMenu.SetPosition(const ACenter: boolean);
begin
  if ACenter then
  begin
    Anchor(hpMiddle);
    Anchor(vpMiddle);
  end else
  begin
    Anchor(hpLeft, 100);
    Anchor(vpTop, -120);
  end;
end;

{ TAbstractMenuState --------------------------------------------------------- }

procedure TAbstractMenuState.SetCurrentMenu(const Value: TCastleGameMenu);
begin
  if FCurrentMenu <> Value then
  begin
    if FCurrentMenu <> nil then
      RemoveControl(FCurrentMenu);
    FCurrentMenu := Value;
    if FCurrentMenu <> nil then
      InsertFront(FCurrentMenu);
  end;
end;

end.
