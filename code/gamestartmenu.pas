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

{ Show start menu, ask user what to do (new game, exit...),
  do what the user wants (e.g. load level and call PlayGame). }
unit GameStartMenu;

interface

uses Classes,
  CastleUIState,
  GameGeneralMenu;

type
  TStateStartMenu = class(TAbstractMenuState)
  private
    GoingToAnotherMenu: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Pause; override;
  end;

var
  StateStartMenu: TStateStartMenu;

implementation

uses SysUtils, CastleUtils,
  CastleGLUtils, CastleMessages, GameWindow, CastleVectors, CastleImages,
  CastleFilesUtils, CastleLevels, CastlePlayer, CastleColors,
  CastleOnScreenMenu, CastleInputs, CastleRectangles,
  CastleKeysMouse, CastleOpenDocument,
  CastleStringUtils, CastleClassUtils, CastleGameNotifications,
  CastleUIControls, CastleSoundEngine, CastleSoundMenu, X3DNodes, CastleControls,
  CastleApplicationProperties, CastleWindow, X3DLoad,
  GamePlay, GameSound, GameControlsMenu, GameVideoOptions,
  GameHelp, GameBackgroundLevel, GameCredits;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleGameMenu)
  strict private
    procedure ClickNewGame(Sender: TObject);
    procedure ClickConfigureControls(Sender: TObject);
    procedure ClickVideoOptions(Sender: TObject);
    procedure ClickSoundOptions(Sender: TObject);
    procedure ClickCredits(Sender: TObject);
    procedure ClickVisitWebsite(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TAntiAliasingSlider = class(TCastleIntegerSlider)
    constructor Create(AOwner: TComponent); override;
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TVideoMenu = class(TSubMenu)
  strict private
    procedure ClickViewVideoInfo(Sender: TObject);
    procedure ClickAllowScreenChange(Sender: TObject);
    procedure ClickShadowVolumes(Sender: TObject);
    procedure ClickColorBits(Sender: TObject);
    procedure ClickVideoFrequency(Sender: TObject);
    procedure ClickRestoreDefaults(Sender: TObject);
    procedure ClickBack(Sender: TObject);
    procedure BakedAnimationSmoothnessChanged(Sender: TObject);
    procedure AntiAliasingChanged(Sender: TObject);
  public
    AllowScreenChangeToggle: TCastleOnScreenMenuItemToggle;
    ShadowVolumesToggle: TCastleOnScreenMenuItemToggle;
    BakedAnimationSmoothnessSlider: TCastleFloatSlider;
    ColorBitsToggle: TCastleOnScreenMenuItem;
    VideoFrequencyToggle: TCastleOnScreenMenuItem;
    AntiAliasingSlider: TAntiAliasingSlider;
    constructor Create(AOwner: TComponent); override;
    procedure SetAntiAliasing(Value: TAntiAliasing; UpdateSlider: boolean);
  end;

  TSoundMenu = class(TSubMenu)
  strict private
    procedure ClickSoundDeviceToggle(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;
    SoundDeviceToggle: TCastleOnScreenMenuItem;
    constructor Create(AOwner: TComponent); override;
  end;

  TChangeSoundDeviceMenu = class(TSubMenu)
  strict private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TChooseNewLevelMenu = class(TSubMenu)
  strict private
    procedure ClickBack(Sender: TObject);
  public
    FirstDemoLevelIndex: Cardinal;
    constructor Create(AOwner: TComponent); override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  MainMenu: TMainMenu;
  VideoMenu: TVideoMenu;
  SoundMenu: TSoundMenu;
  ChooseNewLevelMenu: TChooseNewLevelMenu;
  ChangeSoundDeviceMenu: TChangeSoundDeviceMenu;

{ NewGame -------------------------------------------------------------------- }

{ Just a wrapper that calls PlayGame.

  Before calling PlayGame it prepares some things (creating player)
  and after calling PlayGame is restores some things
  (menu item's values that could change during the game and music).

  The idea is that in the future there will be LoadGame procedure,
  that will also call PlayGame, but initializing player and level
  differently. }
procedure NewGame(Level: TLevelInfo);
begin
  SceneManager := TCastle1SceneManager.Create(nil);
  try
    SceneManager.ShadowVolumes := ShadowVolumes;
    SceneManager.ShadowVolumesRender := ShadowVolumesRender;
    SceneManager.ApproximateActivation := true;
    repeat
      Notifications.Clear;
      Player := TPlayer.Create(nil);
      try
        Player.LoadFromFile;
        Player.Camera.Input_Run.MakeClear; { speed in castle1 is so fast that we're always running }
        PlayerUpdateMouseLook(Player);

        SceneManager.Player := Player;

        SceneManager.LoadLevel(Level);
        PlayGame(true);
      finally FreeAndNil(Player) end;

      if GameEnded and (GameEndedWantsRestart <> '') then
        Level := Levels.FindName(GameEndedWantsRestart) else
        Break;
    until false;
  finally FreeAndNil(SceneManager) end;

  SoundEngine.MusicPlayer.Sound := stIntroMusic;
  SoundMenu.SoundVolume.Refresh;
  SoundMenu.MusicVolume.Refresh;
  Notifications.Clear;
end;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('New game', @ClickNewGame);
  Add('Configure controls', @ClickConfigureControls);
  Add('Video options', @ClickVideoOptions);
  Add('Sound options', @ClickSoundOptions);
  Add('Credits', @ClickCredits);
  Add('Visit our website', @ClickVisitWebsite);
  Add('Quit', @ClickQuit);

  SetPosition(false);

  DrawBackgroundRectangle := false;
end;

procedure TMainMenu.ClickNewGame(Sender: TObject);

  procedure SetChooseNewLevelMenu;
  begin
    { Recreate ChooseNewLevelMenu now, to refresh list of new levels. }
    FreeAndNil(ChooseNewLevelMenu);
    ChooseNewLevelMenu := TChooseNewLevelMenu.Create(Application);

    StateStartMenu.CurrentMenu := ChooseNewLevelMenu;
  end;

begin
  SoundEngine.Sound(stMenuClick);

  { Initially I had here code to show SetChooseNewLevelMenu only
    if I had more than 1 level with Played.

    But it turns out that this was confusing for users:
    they thought that each "New Level" will always restart from the 1st
    level. So they complained that "there is no restart from level
    functionality", before even trying to do "New Game" next time...

    This may be related to the fact that my game is small and some
    of the "quick testers" only run the game once, and they didn't
    even manage to get to 2nd level, or they assumed (wrong) that
    "Loading creatures" will be done again so they didn't even
    "risk" calling "New Game" again... In any case, I think that
    this falls under one of the UI usability rules:

    "Avoid hiding some functionality at runtime, based on some
    gllobal state, because this will confuse users (they don't know
    that there are some rules at runtime that will "unlock" some
    funtionality). It's better to make some items disabled
    (but still visible). It's even acceptable to show them
    a menu or a dialog or a combobox etc. where only 1 choice
    is possible --- this way users will know that *the choice
    is here always, just currently there is only 1 possibility*."

    So it was a bad idea to hide "Choose new level" menu. }

  SetChooseNewLevelMenu;
end;

procedure TMainMenu.ClickConfigureControls(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);

  StateControlsMenu.DrawFadeRect := false;
  StateControlsMenu.DrawCentered := false;
  StateControlsMenu.ExitWithEscapeAllowed := false;
  StateStartMenu.GoingToAnotherMenu := true;
  TUIState.Push(StateControlsMenu);
end;

procedure TMainMenu.ClickVideoOptions(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  StateStartMenu.CurrentMenu := VideoMenu;
end;

procedure TMainMenu.ClickSoundOptions(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  StateStartMenu.CurrentMenu := SoundMenu;
end;

procedure TMainMenu.ClickCredits(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  StateCredits.ControlsUnder := BackgroundControls;
  StateCredits.SceneManagerUnder := BackgroundSceneManager;
  TUIState.Push(StateCredits);
end;

procedure TMainMenu.ClickVisitWebsite(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  if not OpenURL(CastleURL) then MessageOK(Window, SCannotOpenURL);
end;

procedure TMainMenu.ClickQuit(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  Application.Terminate;
end;

{ TAntiAliasingSlider ------------------------------------------ }

constructor TAntiAliasingSlider.Create(AOwner: TComponent);
begin
  inherited;
  Min := Ord(Low(TAntiAliasing));
  Max := Ord(High(TAntiAliasing));
  Value := Ord(Window.AntiAliasing);
end;

function TAntiAliasingSlider.ValueToStr(
  const AValue: Integer): string;
begin
  Result := AntiAliasingNames[TAntiAliasing(AValue)];
end;

{ TVideoMenu ------------------------------------------------------------- }

const
  SRestartTheGame = 'You have to restart the game for the ' +
    'new settings to take effect.';

  SSystemDefault = 'System default';

  MinBakedAnimationSmoothness = 0.2;
  MaxBakedAnimationSmoothness = 1.2;

function ColorBitsToStr(const Value: Cardinal): string;
begin
  if Value = 0 then
    Result := SSystemDefault else
    Result := IntToStr(Value);
end;

function VideoFrequencyToStr(const Value: Cardinal): string;
begin
  if Value = 0 then
    Result := SSystemDefault else
    Result := IntToStr(Value);
end;

constructor TVideoMenu.Create(AOwner: TComponent);
begin
  inherited;

  AllowScreenChangeToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  AllowScreenChangeToggle.Caption := 'Allow screen settings change on startup';
  AllowScreenChangeToggle.Checked := AllowScreenChange;
  AllowScreenChangeToggle.OnClick := @ClickAllowScreenChange;

  ShadowVolumesToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  ShadowVolumesToggle.Caption := 'Shadow volumes';
  ShadowVolumesToggle.Checked := ShadowVolumes;
  ShadowVolumesToggle.OnClick := @ClickShadowVolumes;

  BakedAnimationSmoothnessSlider := TCastleFloatSlider.Create(Self);
  BakedAnimationSmoothnessSlider.Min := MinBakedAnimationSmoothness;
  BakedAnimationSmoothnessSlider.Max := MaxBakedAnimationSmoothness;
  BakedAnimationSmoothnessSlider.Value := BakedAnimationSmoothness;
  BakedAnimationSmoothnessSlider.OnChange := @BakedAnimationSmoothnessChanged;

  ColorBitsToggle := TCastleOnScreenMenuItem.Create(Self);
  ColorBitsToggle.Caption := 'Color bits';
  ColorBitsToggle.RightCaption := ColorBitsToStr(ColorBits);
  ColorBitsToggle.OnClick := @ClickColorBits;

  VideoFrequencyToggle := TCastleOnScreenMenuItem.Create(Self);
  VideoFrequencyToggle.Caption := 'Display frequency';
  VideoFrequencyToggle.RightCaption := VideoFrequencyToStr(VideoFrequency);
  VideoFrequencyToggle.OnClick := @ClickVideoFrequency;

  AntiAliasingSlider := TAntiAliasingSlider.Create(Self);

  Add('View video information', @ClickViewVideoInfo);
  Add(AllowScreenChangeToggle);
  Add(ShadowVolumesToggle);
  Add('Animation smoothness', BakedAnimationSmoothnessSlider);
  Add(ColorBitsToggle);
  Add(VideoFrequencyToggle);
  Add('Anti-aliasing', AntiAliasingSlider);
  Add('Restore to defaults', @ClickRestoreDefaults);
  Add('Back to main menu', @ClickBack);

  { Resigned ideas for menu options:

    - Texture minification quality: Initially done, but removed later,
      in practice useless --- mipmaps are best and work without problems,
      disabling them doesn't allow any noticeable mem/speed saving.

    - Texture magnification quality
      Resigned, because magnification GL_NEAREST will look too awful
      to be sensible.

    - Blending (for Attrib_Blending somewhere)
      Resigned, because without blending levels and items and creatures
      will really look too bad to be sensible.
  }

  SubMenuTitle := 'Video options';

  SubMenuAdditionalInfo := '';

  RegularSpaceBetweenItems := 5;
end;

procedure TVideoMenu.SetAntiAliasing(Value: TAntiAliasing;
  UpdateSlider: boolean);
begin
  if Window.AntiAliasing <> Value then
  begin
    Window.AntiAliasing := Value;
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  { do this regardless of "Window.AntiAliasing <> Value",
    since AntiAliasingSlider.Value doesn't have to be synchronized with
    Window.AntiAliasing now. }
  if UpdateSlider then
    AntiAliasingSlider.Value := Ord(Window.AntiAliasing);
end;

procedure TVideoMenu.ClickViewVideoInfo(Sender: TObject);
begin
  MessageOK(Window, GLInformationString);
end;

procedure TVideoMenu.ClickAllowScreenChange(Sender: TObject);
begin
  AllowScreenChange := not AllowScreenChange;
  AllowScreenChangeToggle.Checked := AllowScreenChange;
end;

procedure TVideoMenu.ClickShadowVolumes(Sender: TObject);
begin
  ShadowVolumes := not ShadowVolumes;
  ShadowVolumesToggle.Checked := ShadowVolumes;
  if (not GLFeatures.ShadowVolumesPossible) and ShadowVolumes then
    MessageOK(Window, 'Your OpenGL implementation doesn''t support stencil buffer necessary for shadow volumes. Shadows (by shadow volumes) will not actually work. Try updating graphic card drivers.');
end;

procedure TVideoMenu.ClickColorBits(Sender: TObject);
begin
  if ColorBits = 0 then
    ColorBits := 16 else
  if ColorBits = 16 then
    ColorBits := 24 else
    ColorBits := 0;
  ColorBitsToggle.RightCaption := ColorBitsToStr(ColorBits);
  SubMenuAdditionalInfo := SRestartTheGame;
end;

procedure TVideoMenu.ClickVideoFrequency(Sender: TObject);
var
  Value: Cardinal;
begin
  Value := VideoFrequency;
  if MessageInputQueryCardinal(Window,
    'What display frequency to use ?' +nl+ '("0" means "system default")',
    Value) and
    (Value <> VideoFrequency) then
  begin
    VideoFrequency := Value;
    VideoFrequencyToggle.RightCaption := VideoFrequencyToStr(VideoFrequency);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;
end;

procedure TVideoMenu.ClickRestoreDefaults(Sender: TObject);
begin
  AllowScreenChange := DefaultAllowScreenChange;
  AllowScreenChangeToggle.Checked := AllowScreenChange;

  ShadowVolumes := DefaultShadowVolumes;
  ShadowVolumesToggle.Checked := ShadowVolumes;

  if BakedAnimationSmoothness <> DefaultBakedAnimationSmoothness then
  begin
    BakedAnimationSmoothness := DefaultBakedAnimationSmoothness;
    BakedAnimationSmoothnessSlider.Value := BakedAnimationSmoothness;
    { You should SRestartTheGame to see the effect fully,
      also on items. But for most typical result, to see it on creatures,
      there's no need to restart the game. }
  end;

  if ColorBits <> DefaultColorBits then
  begin
    ColorBits := DefaultColorBits;
    ColorBitsToggle.RightCaption := ColorBitsToStr(DefaultColorBits);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  if VideoFrequency <> DefaultVideoFrequency then
  begin
    VideoFrequency := DefaultVideoFrequency;
    VideoFrequencyToggle.RightCaption := VideoFrequencyToStr(DefaultVideoFrequency);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  SetAntiAliasing(DefaultAntiAliasing, true);

  VisibleChange([chRender]);

  MessageOK(Window, 'All video settings restored to defaults.');
end;

procedure TVideoMenu.ClickBack(Sender: TObject);
begin
  StateStartMenu.CurrentMenu := MainMenu;
end;

procedure TVideoMenu.BakedAnimationSmoothnessChanged(Sender: TObject);
begin
  BakedAnimationSmoothness := BakedAnimationSmoothnessSlider.Value;
end;

procedure TVideoMenu.AntiAliasingChanged(Sender: TObject);
begin
  SetAntiAliasing(TAntiAliasing(AntiAliasingSlider.Value), false);
end;

{ TSoundMenu ------------------------------------------------------------- }

constructor TSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  SoundDeviceToggle := TCastleOnScreenMenuItem.Create(Self);
  SoundDeviceToggle.Caption := 'Sound output device';
  SoundDeviceToggle.RightCaption := SoundEngine.DeviceCaption;
  SoundDeviceToggle.OnClick := @ClickSoundDeviceToggle;

  Add(TSoundInfoMenuItem.Create(Self));
  SoundVolume := TSoundVolumeMenuItem.Create(Self);
  Add(SoundVolume);
  MusicVolume := TMusicVolumeMenuItem.Create(Self);
  Add(MusicVolume);
  Add(SoundDeviceToggle);
  Add('Back to main menu', @ClickBack);

  SubMenuTitle := 'Sound options';
end;

procedure TSoundMenu.ClickSoundDeviceToggle(Sender: TObject);
begin
  StateStartMenu.CurrentMenu := ChangeSoundDeviceMenu;
end;

procedure TSoundMenu.ClickBack(Sender: TObject);
begin
  StateStartMenu.CurrentMenu := MainMenu;
end;

{ TSoundDeviceMenuButton ---------------------------------------------------- }

type
  TSoundDeviceMenuButton = class(TCastleOnScreenMenuItem)
  public
    Device: TSoundDevice;
    procedure DoClick; override;
  end;

procedure TSoundDeviceMenuButton.DoClick;
begin
  inherited;

  SoundEngine.Device := Device.Name;
  SoundMenu.SoundDeviceToggle.RightCaption := SoundEngine.DeviceCaption;
  if not SoundEngine.ALActive then
    MessageOK(Window, SoundEngine.Information);

  StateStartMenu.CurrentMenu := SoundMenu;
end;

{ TChangeSoundDeviceMenu ---------------------------------------------------- }

constructor TChangeSoundDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
  D: TSoundDeviceMenuButton;
begin
  inherited;

  for I := 0 to SoundEngine.Devices.Count - 1 do
  begin
    D := TSoundDeviceMenuButton.Create(Self);
    D.Device := SoundEngine.Devices[I];
    D.Caption := D.Device.Caption;
    Add(D);
  end;

  Add('Cancel', @ClickBack);

  SubMenuTitle := 'Change sound output device';
end;

procedure TChangeSoundDeviceMenu.ClickBack(Sender: TObject);
begin
  StateStartMenu.CurrentMenu := SoundMenu;
end;

{ TNewLevelButton ---------------------------------------------------- }

type
  TNewLevelButton = class(TCastleOnScreenMenuItem)
  strict private
    Level: TLevelInfo;
  public
    constructor Create(AOwner: TComponent; ALevel: TLevelInfo); reintroduce;
    procedure DoClick; override;
  end;

constructor TNewLevelButton.Create(AOwner: TComponent; ALevel: TLevelInfo);

  function LevelCaption(const Level: TLevelInfo): string;
  begin
    { calculate nice Caption }
    Result := Format('%d: %s', [Level.Number, Level.Title]);
    if Level.TitleHint <> '' then
      Result += ' (' + Level.TitleHint + ')';
  end;

begin
  inherited Create(AOwner);
  Level := ALevel;
  Caption := LevelCaption(Level);
end;

procedure TNewLevelButton.DoClick;
begin
  inherited;
  NewGame(Level);
  StateStartMenu.CurrentMenu := MainMenu;
end;

{ TChooseNewLevelMenu ------------------------------------------------------- }

constructor TChooseNewLevelMenu.Create(AOwner: TComponent);
var
  I: Integer;
  Level: TLevelInfo;
  Spacer: TCastleUserInterface;
begin
  inherited;

  { Add non-demo levels }
  for I := 0 to Levels.Count - 1 do
  begin
    Level := Levels[I];
    if Level.Played and
       (Level.Name <> MenuBackgroundLevelName) and
       not Level.Demo then
      Add(TNewLevelButton.Create(Self, Level));
  end;

  Spacer := TCastleUserInterface.Create(Self);
  Spacer.Height := 10;
  Add(Spacer);

  Add('Bonus demo levels :');

  { Add demo levels }
  for I := 0 to Levels.Count - 1 do
  begin
    Level := Levels[I];
    if Level.Played and
       (Level.Name <> MenuBackgroundLevelName) and
       Level.Demo then
      Add(TNewLevelButton.Create(Self, Level));
  end;

  { Separator is needed before "Cancel" button,
    otherwise the button seems attached to the "demo" levels section. }
  Spacer := TCastleUserInterface.Create(Self);
  Spacer.Height := 10;
  Add(Spacer);

  Add('Cancel', @ClickBack);

  SubMenuTitle := 'Choose initial level';
end;

procedure TChooseNewLevelMenu.ClickBack(Sender: TObject);
begin
  StateStartMenu.CurrentMenu := MainMenu;
end;

{ global things -------------------------------------------------------------- }

constructor TStateStartMenu.Create(AOwner: TComponent);
begin
  inherited;
  MainMenu := TMainMenu.Create(Application);
  VideoMenu := TVideoMenu.Create(Application);
  SoundMenu := TSoundMenu.Create(Application);
  ChangeSoundDeviceMenu := TChangeSoundDeviceMenu.Create(Application);
end;

procedure TStateStartMenu.Start;
begin
  inherited;
  BackgroundCreate;
  SoundEngine.MusicPlayer.Sound := stIntroMusic;
end;

procedure TStateStartMenu.Stop;
begin
  BackgroundDestroy;
  inherited;
end;

procedure TStateStartMenu.Resume;
begin
  inherited;
  CurrentMenu := MainMenu;
  if not GoingToAnotherMenu then
    InsertBack(BackgroundControls);
  GoingToAnotherMenu := false;
end;

procedure TStateStartMenu.Pause;
begin
  CurrentMenu := nil;
  if not GoingToAnotherMenu then
    RemoveControl(BackgroundControls);
  inherited;
end;

end.
