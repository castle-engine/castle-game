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
unit GameGeneralMenu;

interface

uses Classes,
  CastleVectors, CastleOnScreenMenu, CastleRectangles;

type
  { On-screen menu suitable for castle.

    Note that CaptureAllEvents makes all events captured
    (covers the whole screen), as we don't need the focus / non-focus logic,
    menu is the only control used on the screen.
    Also, it let's events further down to our callbacks, thanks
    to ExclusiveEvents being @false. }
  TCastleGameMenu = class(TCastleOnScreenMenu)
  public
  const
    DefaultCastleFontSize = 40;

    constructor Create(AOwner: TComponent); override;
    property ExclusiveEvents default false;
    property DrawFocusedBorder default false;
    property CaptureAllEvents default true;
    property FontSize default DefaultCastleFontSize;

    { Sets position parameters.
      If Center = @false then sets position suitable for the background
      under start menu. Otherwise menu is in the center of the screen. }
    procedure SetPosition(const ACenter: boolean);
  end;

{ Sets CurrentValue, taking care of adding this menu / removing existing menu
  (when new value is @nil) from Window.Controls.
  Also, returns previous TCastleOnScreenMenu present in Window.Controls (there can be
  only one). }
function SetCurrentMenu(var CurrentValue: TCastleGameMenu;
  const NewValue: TCastleGameMenu): TCastleGameMenu;

implementation

uses SysUtils, CastleWindow, CastleUIControls, GameWindow;

{ TCastleGameMenu ---------------------------------------------------------------- }

constructor TCastleGameMenu.Create(AOwner: TComponent);
begin
  inherited;
  ExclusiveEvents := false;
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

{ globals -------------------------------------------------------------------- }

function SetCurrentMenu(var CurrentValue: TCastleGameMenu;
  const NewValue: TCastleGameMenu): TCastleGameMenu;
begin
  CurrentValue := NewValue;
  Result := Window.Controls.MakeSingle(TCastleGameMenu, NewValue) as TCastleGameMenu;
end;

end.