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

{ Global @link(Window) variable. }
unit GameWindow;

interface

uses CastleWindow, KeysMouse;

type
  { Window that automatically, always, can do save screen on CastleInput_SaveScreen
    press. This way our "save screen" button works in game, all menus, credits
    and such. }
  TGameWindow = class(TCastleWindowCustom)
  private
    procedure EventDown(AKey: TKey;
      AMousePress: boolean; AMouseButton: TMouseButton;
      AMouseWheel: TMouseWheelDirection);
  public
    procedure EventKeyDown(key: TKey; c: char); override;
    procedure EventMouseDown(Button: TMouseButton); override;
    procedure EventMouseWheel(const Scroll: Single; const Vertical: boolean); override;
  end;

var
  { @noAutoLinkHere }
  Window: TGameWindow;

implementation

uses SysUtils, CastleInputs, UIControls, CastleGameNotifications, CastleFilesUtils,
  CastleSoundEngine, GameSound, GameInputs;

procedure TGameWindow.EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);

  { Saves a screen, causing also appropriate Notification and sound. }
  procedure AutoSaveScreen;
  var
    FileName: string;
  begin
    FileName := FileNameAutoInc(ApplicationName + '_screen_%d.png');
    SaveScreen(FileName);
    Notifications.Show('Screen saved to ' + FileName);
    SoundEngine.Sound(stSaveScreen);
  end;

begin
  if CastleInput_SaveScreen.IsEvent(AKey, #0,
    AMousePress, AMouseButton, AMouseWheel) then
    AutoSaveScreen;
end;

procedure TGameWindow.EventKeyDown(key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);
  inherited;
end;

procedure TGameWindow.EventMouseDown(Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
  inherited;
end;

procedure TGameWindow.EventMouseWheel(const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
  inherited;
end;

initialization
  Window := TGameWindow.Create(nil);
  Window.OnDrawStyle := ds3D;
finalization
  FreeAndNil(Window);
end.
