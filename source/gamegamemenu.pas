{
  Copyright 2006-2013 Michalis Kamburelis.

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
unit GameGameMenu;

interface

uses CastleUIControls;

procedure ShowGameMenu(AControlsUnder: TUIControlList);

implementation

uses CastleControlsImages, CastleImages,
  SysUtils, Classes, CastleUtils, CastleStringUtils, CastleWindowModes,
  CastleGLUtils, CastleMessages, GameWindow, CastleVectors,
  CastleWindow, GameHelp, GamePlay, GameGeneralMenu, GameControlsMenu,
  CastleInputs, X3DNodes, CastleClassUtils, CastleSoundMenu, CastleKeysMouse,
  CastleGameNotifications, CastleControls;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TGameMenu = class(TCastleGameMenu)
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TGameSoundMenu = class(TCastleGameMenu)
  public
    SoundInfo: TSoundInfoMenuItem;
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleGameMenu;
  GameMenu: TGameMenu;
  GameSoundMenu: TGameSoundMenu;
  ControlsUnder: TUIControlList;

{ TGameMenu ------------------------------------------------------------ }

constructor TGameMenu.Create(AOwner: TComponent);
begin
  inherited;

  Items.Add('Back to game');
  Items.Add('View last game messages');
  Items.Add('Configure controls');
  Items.Add('Sound options');
  Items.Add('End game');
end;

procedure TGameMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: UserQuit := true;
    1: ViewGameMessages;
    2: ShowControlsMenuEscape(ControlsUnder, true, true, UserQuit);
    3: SetCurrentMenu(CurrentMenu, GameSoundMenu);
    4: { At first I did here GameCancel(false), but tests (with Mama)
         show that it's too easy to select this and accidentaly
         end the game. }
       GameCancel(true);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TGameSoundMenu ------------------------------------------------------------- }

constructor TGameSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  SoundInfo := TSoundInfoMenuItem.Create(Window, Self);
  SoundVolume := TSoundVolumeMenuItem.Create(Window, Self);
  MusicVolume := TMusicVolumeMenuItem.Create(Window, Self);
  Items.Add('Back to game menu');
end;

destructor TGameSoundMenu.Destroy;
begin
  FreeAndNil(SoundInfo);
  FreeAndNil(SoundVolume);
  FreeAndNil(MusicVolume);
  inherited;
end;

procedure TGameSoundMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: SoundInfo.Selected;
    1: ;
    2: ;
    3: SetCurrentMenu(CurrentMenu, GameMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TGameSoundMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    1: SoundVolume.AccessoryValueChanged;
    2: MusicVolume.AccessoryValueChanged;
  end;
end;

{ global things -------------------------------------------------------------- }

{$I gamemenucallbacks.inc}

procedure ShowGameMenu(AControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
  OldThemeWindow: TCastleImage;
begin
  ControlsUnder := AControlsUnder;

  GameSoundMenu.SoundVolume.RefreshAccessory;
  GameSoundMenu.MusicVolume.RefreshAccessory;

  OldThemeWindow := Theme.Images[tiWindow];
  SavedMode := TGLMode.CreateReset(Window, nil, Window.OnResize, @CloseQuery);
  try
    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    Theme.Images[tiWindow] := WindowDark;

    Window.OnPress := @Press;
    Window.OnDrawStyle := ds3D;

    SetCurrentMenu(CurrentMenu, GameMenu);

    Window.Controls.Add(Notifications);
    Window.Controls.AddList(ControlsUnder);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true, true);
    until GameEnded or UserQuit;
  finally
    FreeAndNil(SavedMode);
    Theme.Images[tiWindow] := OldThemeWindow;
  end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen;
begin
  GameMenu := TGameMenu.Create(Application);
  GameSoundMenu := TGameSoundMenu.Create(Application);
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
finalization
end.