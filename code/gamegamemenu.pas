{
  Copyright 2006-2022 Michalis Kamburelis.

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

uses Classes,
  CastleUIControls, CastleUIState, CastleKeysMouse, CastleImages,
  GameGeneralMenu;

type
  TStateGameMenu = class(TAbstractMenuState)
  strict private
    OldThemeWindow: String;
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
  CastleUtils, CastleStringUtils,
  CastleGLUtils, CastleMessages, GameWindow, CastleVectors,
  CastleWindow, GameHelp, GamePlay, GameControlsMenu,
  CastleInputs, X3DNodes, CastleClassUtils, CastleSoundMenu,
  CastleGameNotifications, CastleControls, CastleApplicationProperties;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TGameMenu = class(TCastleGameMenu)
  strict private
    procedure ClickBack(Sender: TObject);
    procedure ClickViewMessages(Sender: TObject);
    procedure ClickControls(Sender: TObject);
    procedure ClickSoundOptions(Sender: TObject);
    procedure ClickEndGame(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGameSoundMenu = class(TCastleGameMenu)
  strict private
    procedure ClickBack(Sender: TObject);
  public
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;
    constructor Create(AOwner: TComponent); override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  GameMenu: TGameMenu;
  GameSoundMenu: TGameSoundMenu;

{ TGameMenu ------------------------------------------------------------ }

constructor TGameMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Back to game', @ClickBack);
  Add('View last game messages', @ClickViewMessages);
  Add('Configure controls', @ClickControls);
  Add('Sound options', @ClickSoundOptions);
  Add('End game', @ClickEndGame);
end;

procedure TGameMenu.ClickBack(Sender: TObject);
begin
  TUIState.Pop(StateGameMenu);
end;

procedure TGameMenu.ClickViewMessages(Sender: TObject);
begin
  ViewGameMessages;
end;

procedure TGameMenu.ClickControls(Sender: TObject);
begin
  StateControlsMenu.DrawFadeRect := true;
  StateControlsMenu.DrawCentered := true;
  StateControlsMenu.ExitWithEscapeAllowed := true;
  TUIState.Push(StateControlsMenu);
end;

procedure TGameMenu.ClickSoundOptions(Sender: TObject);
begin
  StateGameMenu.CurrentMenu := GameSoundMenu;
end;

procedure TGameMenu.ClickEndGame(Sender: TObject);
begin
  GameCancel(true);
  TUIState.Pop(StateGameMenu);
end;

{ TGameSoundMenu ------------------------------------------------------------- }

constructor TGameSoundMenu.Create(AOwner: TComponent);
begin
  inherited;
  Add(TSoundInfoMenuItem.Create(Self));
  SoundVolume := TSoundVolumeMenuItem.Create(Self);
  Add(SoundVolume);
  MusicVolume := TMusicVolumeMenuItem.Create(Self);
  Add(MusicVolume);
  Add('Back to game menu', @ClickBack);
end;

procedure TGameSoundMenu.ClickBack(Sender: TObject);
begin
  StateGameMenu.CurrentMenu := GameMenu;
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

  OldThemeWindow := Theme.ImagesPersistent[tiWindow].Url;
  { Otherwise CastleMessages don't look good,
    as mesage text would be mixed with the menu text underneath. }
  Theme.ImagesPersistent[tiWindow].Url := 'castle-data:/theme/WindowDark.png';
end;

procedure TStateGameMenu.Stop;
begin
  Theme.ImagesPersistent[tiWindow].Url := OldThemeWindow;
  inherited;
end;

procedure TStateGameMenu.Resume;
begin
  inherited;
  CurrentMenu := GameMenu;
end;

procedure TStateGameMenu.Pause;
begin
  CurrentMenu := nil;
  inherited;
end;

end.
