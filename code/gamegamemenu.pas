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
unit GameGameMenu;

interface

uses Classes, CastleUIControls, CastleUIState, CastleKeysMouse,
  CastleImages;

type
  TStateGameMenu = class(TUIState)
  strict private
    OldThemeWindow: TCastleImage;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Pause; override;
   function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateGameMenu: TStateGameMenu;

implementation

uses SysUtils,
  CastleControlsImages, CastleUtils, CastleStringUtils,
  CastleGLUtils, CastleMessages, GameWindow, CastleVectors,
  CastleWindow, GameHelp, GamePlay, GameGeneralMenu, GameControlsMenu,
  CastleInputs, X3DNodes, CastleClassUtils, CastleSoundMenu,
  CastleGameNotifications, CastleControls, CastleApplicationProperties;

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
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  CurrentMenu: TCastleGameMenu;
  GameMenu: TGameMenu;
  GameSoundMenu: TGameSoundMenu;

{ TGameMenu ------------------------------------------------------------ }

constructor TGameMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Back to game');
  Add('View last game messages');
  Add('Configure controls');
  Add('Sound options');
  Add('End game');
end;

procedure TGameMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: TUIState.Pop(StateGameMenu);
    1: ViewGameMessages;
    2: begin
         StateControlsMenu.DrawFadeRect := true;
         StateControlsMenu.DrawCentered := true;
         StateControlsMenu.ExitWithEscapeAllowed := true;
         TUIState.Push(StateControlsMenu);
       end;
    3: SetCurrentMenu(CurrentMenu, GameSoundMenu);
    4: begin
         { At first I did here GameCancel(false), but tests (with Mama)
           show that it's too easy to select this and accidentaly
           end the game. }
         GameCancel(true);
         TUIState.Pop(StateGameMenu);
       end;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TGameSoundMenu ------------------------------------------------------------- }

constructor TGameSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  SoundInfo := TSoundInfoMenuItem.Create(Self);
  Add(SoundInfo);

  SoundVolume := TSoundVolumeMenuItem.Create(Self);
  Add(SoundVolume);

  MusicVolume := TMusicVolumeMenuItem.Create(Self);
  Add(MusicVolume);

  Add('Back to game menu');
end;

destructor TGameSoundMenu.Destroy;
begin
  inherited;
end;

procedure TGameSoundMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: ;
    1: ;
    2: ;
    3: SetCurrentMenu(CurrentMenu, GameMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TStateGameMenu -------------------------------------------------------------- }

constructor TStateGameMenu.Create(AOwner: TComponent);
begin
  inherited;
  GameMenu := TGameMenu.Create(Application);
  GameSoundMenu := TGameSoundMenu.Create(Application);
end;

function TStateGameMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(CharEscape) then
  begin
    TUIState.Pop(StateGameMenu);
    Result := true;
  end;
end;

procedure TStateGameMenu.Start;
begin
  inherited;

  GameSoundMenu.SoundVolume.Refresh;
  GameSoundMenu.MusicVolume.Refresh;

  OldThemeWindow := Theme.Images[tiWindow];
  { Otherwise messages don't look good, because the text is mixed
    with the menu text. }
  Theme.Images[tiWindow] := WindowDark;
end;

procedure TStateGameMenu.Stop;
begin
  Theme.Images[tiWindow] := OldThemeWindow;
  inherited;
end;

procedure TStateGameMenu.Resume;
begin
  inherited;
  SetCurrentMenu(CurrentMenu, GameMenu);
end;

procedure TStateGameMenu.Pause;
begin
  SetCurrentMenu(CurrentMenu, nil);
  inherited;
end;

end.
