{
  Copyright 2006-2012 Michalis Kamburelis.

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

uses Classes, OnScreenMenu;

type
  { On-screen menu suitable for castle.

    Full-screen display: PositioInside makes all events captured
    (covers the whole screen), as we don't need the focus / non-focus stuff,
    menu is the only control used here.
    Also, it let's events further down to our callbacks, thanks
    to ExclusiveEvents being @false. }
  TCastleGameMenu = class(TCastleOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
    property ExclusiveEvents default false;
    property DrawFocusedBorder default false;
    function PositionInside(const X, Y: Integer): boolean; override;
  end;

var
  DebugMenuDesigner: boolean = false;

{ Sets CurrentValue, taking care of adding this menu / removing existing menu
  (when new value is @nil) from Window.Controls.
  Also, returns previous TCastleOnScreenMenu present in Window.Controls (there can be
  only one). }
function SetCurrentMenu(var CurrentValue: TCastleGameMenu;
  const NewValue: TCastleGameMenu): TCastleGameMenu;

implementation

uses SysUtils, CastleWindow, CastleGameCache, GameSound, UIControls, GameWindow,
  ALSoundEngine;

{ TCastleGameMenu ---------------------------------------------------------------- }

constructor TCastleGameMenu.Create(AOwner: TComponent);
begin
  inherited;
  { Don't set DesignerModeWindow, we do tricks that make setting mouse
    position in OnScreenMenu not working. See TCastleOnScreenMenu.DesignerMode comments. }
  DesignerMode := DebugMenuDesigner;
  ExclusiveEvents := false;
  DrawFocusedBorder := false;
end;

function TCastleGameMenu.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

function SetCurrentMenu(var CurrentValue: TCastleGameMenu;
  const NewValue: TCastleGameMenu): TCastleGameMenu;
begin
  CurrentValue := NewValue;
  Result := Window.Controls.MakeSingle(TCastleGameMenu, NewValue) as TCastleGameMenu;
end;

end.