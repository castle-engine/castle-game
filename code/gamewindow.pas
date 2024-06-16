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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Global @link(Window) variable. }
unit GameWindow;

interface

uses Classes,
  CastleVectors, CastleWindow, CastleUIControls, CastleKeysMouse,
  CastleRectangles, CastleControls, CastleCameras;

type
  TGameWindow = class(TCastleWindowBase)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGlobalCatchInput = class(TUIControl)
    function Press(const Event: TInputPressRelease): boolean; override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
  end;

var
  { @noAutoLinkHere }
  Window: TGameWindow;

  { This way our "save screen" button works in game, all menus, credits
    and such. Make sure this is always on Window.Controls list. }
  GlobalCatchInput: TGlobalCatchInput;

implementation

uses SysUtils,
  CastleInputs, CastleGameNotifications, CastleFilesUtils, CastleSoundEngine,
  CastleUtils,
  GameSound, GameInputs;

{ TGlobalCatchInput ---------------------------------------------------------- }

function TGlobalCatchInput.Press(const Event: TInputPressRelease): boolean;

  { Saves a screen, causing also appropriate Notification and sound. }
  procedure AutoSaveScreen;
  var
    URL: string;
  begin
    URL := FileNameAutoInc(ApplicationName + '_screen_%d.png');
    Window.SaveScreen(URL);
    Notifications.Show('Screen saved to ' + URL);
    SoundEngine.Play(stSaveScreen);
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Input_SaveScreen.IsEvent(Event) then
  begin
    AutoSaveScreen;
    Result := true;
  end;
end;

function TGlobalCatchInput.CapturesEventsAtPosition(
  const Position: TVector2): boolean;
begin
  Result := true; // always catch input
end;

{ TGameWindow ---------------------------------------------------------------- }

constructor TGameWindow.Create(AOwner: TComponent);
begin
  inherited;
  GlobalCatchInput := TGlobalCatchInput.Create(Self);
  Controls.InsertBack(GlobalCatchInput);

  // Note that we have to set KeepInFront *before* inserting to Controls
  Notifications.KeepInFront := true;
  Controls.InsertFront(Notifications);
end;

initialization
  Window := TGameWindow.Create(nil);
finalization
  FreeAndNil(Window);
end.
