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

{ Show start menu, ask user what to do (new game, exit...),
  do what the user wants (e.g. load level and call PlayGame). }
unit GameStartMenu;

interface

uses Classes, CastleUIState;

type
  TStateStartMenu = class(TUIState)
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

uses SysUtils, CastleUtils, CastleWindowModes,
  CastleGLUtils, CastleMessages, GameWindow, CastleVectors, CastleImages,
  CastleFilesUtils, CastleLevels, CastlePlayer, CastleColors,
  CastleOnScreenMenu, CastleInputs, CastleRectangles,
  CastleKeysMouse, CastleOpenDocument, CastlePrecalculatedAnimation,
  CastleStringUtils, CastleClassUtils, CastleGameNotifications,
  CastleUIControls, CastleSoundEngine, CastleSoundMenu, X3DNodes, CastleControls,
  CastleApplicationProperties, CastleWindow,
  GamePlay, GameSound, GameGeneralMenu, GameControlsMenu, GameVideoOptions,
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
  private
    procedure AnimationSmoothnessChanged(Sender: TObject);
    procedure AntiAliasingChanged(Sender: TObject);
  public
    AllowScreenChangeToggle: TCastleMenuToggle;
    ShadowVolumesToggle: TCastleMenuToggle;
    AnimationSmoothnessSlider: TCastleFloatSlider;
    ColorBitsToggle: TCastleMenuButton;
    VideoFrequencyToggle: TCastleMenuButton;
    ConserveResourcesToggle: TCastleMenuToggle;
    AntiAliasingSlider: TAntiAliasingSlider;
    constructor Create(AOwner: TComponent); override;
    procedure SetAntiAliasing(
      Value: TAntiAliasing;
      UpdateSlider: boolean);
    procedure Click; override;
  end;

  TSoundMenu = class(TSubMenu)
  public
    SoundInfo: TSoundInfoMenuItem;
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;

    OpenALDeviceToggle: TCastleMenuButton;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

  TChangeOpenALDeviceMenu = class(TSubMenu)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

  TChooseNewLevelMenu = class(TSubMenu)
  public
    LevelsNewGame: TLevelInfoList;
    FirstDemoLevelIndex: Cardinal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; override;
    procedure Render; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  CurrentMenu: TCastleGameMenu;
  MainMenu: TMainMenu;
  VideoMenu: TVideoMenu;
  SoundMenu: TSoundMenu;
  ChooseNewLevelMenu: TChooseNewLevelMenu;
  ChangeOpenALDeviceMenu: TChangeOpenALDeviceMenu;

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
        SceneManager.Items.Add(Player);

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
    { Recreate ChooseNewLevelMenu now, to refresh list of LevelsNewGame. }
    FreeAndNil(ChooseNewLevelMenu);
    ChooseNewLevelMenu := TChooseNewLevelMenu.Create(Application);

    SetCurrentMenu(CurrentMenu, ChooseNewLevelMenu);
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
  SetCurrentMenu(CurrentMenu, VideoMenu);
end;

procedure TMainMenu.ClickSoundOptions(Sender: TObject);
begin
  SoundEngine.Sound(stMenuClick);
  SetCurrentMenu(CurrentMenu, SoundMenu);
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

  MinAnimationSmoothness = 0.2;
  MaxAnimationSmoothness = 1.2;

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

  AllowScreenChangeToggle := TCastleMenuToggle.Create(Self);
  AllowScreenChangeToggle.Pressed := AllowScreenChange;

  ShadowVolumesToggle := TCastleMenuToggle.Create(Self);
  ShadowVolumesToggle.Pressed := ShadowVolumes;

  AnimationSmoothnessSlider := TCastleFloatSlider.Create(Self);
  AnimationSmoothnessSlider.Min := MinAnimationSmoothness;
  AnimationSmoothnessSlider.Max := MaxAnimationSmoothness;
  AnimationSmoothnessSlider.Value := AnimationSmoothness;
  AnimationSmoothnessSlider.OnChange := @AnimationSmoothnessChanged;

  ColorBitsToggle := TCastleMenuButton.Create(Self);
  ColorBitsToggle.Caption := ColorBitsToStr(ColorBits);

  VideoFrequencyToggle := TCastleMenuButton.Create(Self);
  VideoFrequencyToggle.Caption := VideoFrequencyToStr(VideoFrequency);

  AntiAliasingSlider := TAntiAliasingSlider.Create(Self);

  Add('View video information');
  Add('Allow screen settings change on startup', AllowScreenChangeToggle);
  Add('Shadow volumes', ShadowVolumesToggle);
  Add('Animation smoothness', AnimationSmoothnessSlider);
  Add('Color bits', ColorBitsToggle);
  Add('Display frequency', VideoFrequencyToggle);
  Add('Anti-aliasing', AntiAliasingSlider);
  Add('Restore to defaults');
  Add('Back to main menu');

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

procedure TVideoMenu.SetAntiAliasing(
  Value: TAntiAliasing;
  UpdateSlider: boolean);
begin
  if Window.AntiAliasing <> Value then
  begin
    Window.AntiAliasing := Value;
    if UpdateSlider then
      AntiAliasingSlider.Value := Ord(Window.AntiAliasing);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;
end;

procedure TVideoMenu.Click;

  procedure ViewVideoInfo;
  begin
    MessageOK(Window, GLInformationString);
  end;

  procedure ChangeColorBits;
  begin
    if ColorBits = 0 then
      ColorBits := 16 else
    if ColorBits = 16 then
      ColorBits := 24 else
      ColorBits := 0;
    ColorBitsToggle.Caption := ColorBitsToStr(ColorBits);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  procedure ChangeVideoFrequency;
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
      VideoFrequencyToggle.Caption := VideoFrequencyToStr(VideoFrequency);
      SubMenuAdditionalInfo := SRestartTheGame;
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: ViewVideoInfo;
    1: begin
         AllowScreenChange := not AllowScreenChange;
         AllowScreenChangeToggle.Pressed := AllowScreenChange;
       end;
    2: begin
         ShadowVolumes := not ShadowVolumes;
         ShadowVolumesToggle.Pressed := ShadowVolumes;
         if (not GLFeatures.ShadowVolumesPossible) and ShadowVolumes then
           MessageOK(Window, 'Your OpenGL implementation doesn''t support stencil buffer necessary for shadow volumes. Shadows (by shadow volumes) will not actually work. Try updating graphic card drivers.');
       end;
    3: ;
    4: ChangeColorBits;
    5: ChangeVideoFrequency;
    6: ;
    7: begin
         AllowScreenChange := DefaultAllowScreenChange;
         AllowScreenChangeToggle.Pressed := AllowScreenChange;

         ShadowVolumes := DefaultShadowVolumes;
         ShadowVolumesToggle.Pressed := ShadowVolumes;

         if AnimationSmoothness <> DefaultAnimationSmoothness then
         begin
           AnimationSmoothness := DefaultAnimationSmoothness;
           AnimationSmoothnessSlider.Value := AnimationSmoothness;
           { You should SRestartTheGame to see the effect fully,
             also on items. But for most typical result, to see it on creatures,
             there's no need to restart the game. }
         end;

         if ColorBits <> DefaultColorBits then
         begin
           ColorBits := DefaultColorBits;
           ColorBitsToggle.Caption := ColorBitsToStr(DefaultColorBits);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         if VideoFrequency <> DefaultVideoFrequency then
         begin
           VideoFrequency := DefaultVideoFrequency;
           VideoFrequencyToggle.Caption := VideoFrequencyToStr(DefaultVideoFrequency);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         SetAntiAliasing(DefaultAntiAliasing, true);

         VisibleChange;

         MessageOK(Window, 'All video settings restored to defaults.');
       end;
    8 : SetCurrentMenu(CurrentMenu, MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TVideoMenu.AnimationSmoothnessChanged(Sender: TObject);
begin
  AnimationSmoothness := AnimationSmoothnessSlider.Value;
end;

procedure TVideoMenu.AntiAliasingChanged(Sender: TObject);
begin
  SetAntiAliasing(TAntiAliasing(AntiAliasingSlider.Value), false);
end;

{ TSoundMenu ------------------------------------------------------------- }

constructor TSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  OpenALDeviceToggle := TCastleMenuButton.Create(Self);
  OpenALDeviceToggle.Caption := SoundEngine.DeviceNiceName;

  SoundInfo := TSoundInfoMenuItem.Create(Self);
  Add(SoundInfo);

  SoundVolume := TSoundVolumeMenuItem.Create(Self);
  Add(SoundVolume);

  MusicVolume := TMusicVolumeMenuItem.Create(Self);
  Add(MusicVolume);

  Add('Sound output device', OpenALDeviceToggle);
  Add('Back to main menu');

  SubMenuTitle := 'Sound options';
end;

destructor TSoundMenu.Destroy;
begin
  inherited;
end;

procedure TSoundMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: ;
    1: ;
    2: ;
    3: SetCurrentMenu(CurrentMenu, ChangeOpenALDeviceMenu);
    4: SetCurrentMenu(CurrentMenu, MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TChangeOpenALDeviceMenu ---------------------------------------------------- }

constructor TChangeOpenALDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  for I := 0 to SoundEngine.Devices.Count - 1 do
    Add(SoundEngine.Devices[I].NiceName);
  Add('Cancel');

  SubMenuTitle := 'Change sound output device';
end;

destructor TChangeOpenALDeviceMenu.Destroy;
begin
  inherited;
end;

procedure TChangeOpenALDeviceMenu.Click;
begin
  inherited;

  if CurrentItem < SoundEngine.Devices.Count then
  begin
    SoundEngine.Device := SoundEngine.Devices[CurrentItem].Name;
    { ALCDevice value changed now to new value. }
    SoundMenu.OpenALDeviceToggle.Caption := SoundEngine.Devices[CurrentItem].NiceName;
    if not SoundEngine.ALActive then
      MessageOK(Window, SoundEngine.SoundInitializationReport);
  end;

  SetCurrentMenu(CurrentMenu, SoundMenu);
end;

{ TChooseNewLevelMenu ------------------------------------------------------- }

constructor TChooseNewLevelMenu.Create(AOwner: TComponent);

  { Add level to LevelsNewGame and Items lists.
    Index is an index into Levels array for this level. }
  procedure AddLevel(Index: Integer);
  var
    S: string;
    L: TLevelInfo;
  begin
    L := Levels[Index];
    LevelsNewGame.Add(L);
    S := Format('%d: %s', [ L.Number, L.Title ]);
    if L.TitleHint <> '' then
      S += ' (' + L.TitleHint + ')';
    Add(S);
  end;

var
  I: Integer;
begin
  inherited;

  LevelsNewGame := TLevelInfoList.Create(false);

  { Add non-demo levels }
  for I := 0 to Levels.Count - 1 do
    if Levels[I].Played and
       (Levels[I].Name <> MenuBackgroundLevelName) and
       not Levels[I].Demo then
      AddLevel(I);

  FirstDemoLevelIndex := LevelsNewGame.Count;

  { Add demo levels }
  for I := 0 to Levels.Count - 1 do
    if Levels[I].Played and
       (Levels[I].Name <> MenuBackgroundLevelName) and
       Levels[I].Demo then
      AddLevel(I);

  Add('Cancel');

  SubMenuTitle := 'Choose initial level';
end;

destructor TChooseNewLevelMenu.Destroy;
begin
  FreeAndNil(LevelsNewGame);
  inherited;
end;

function TChooseNewLevelMenu.SpaceBetweenItems(
  const NextItemIndex: Cardinal): Cardinal;
begin
  Result := inherited SpaceBetweenItems(NextItemIndex);
  if NextItemIndex = FirstDemoLevelIndex then
    Result += Cardinal(SubMenuTitleFont.RowHeight) * 2 else
  if NextItemIndex = ControlsCount - 1 then
    { some separator is needed before "cancel" button now,
      since otherwise it seems to attached to "demo" levels section. }
    Result += 10;
end;

procedure TChooseNewLevelMenu.Render;
const
  SubMenuTextColor: TCastleColor = (0.7, 0.7, 0.7, 1.0);
var
  R: TRectangle;
begin
  inherited;
  R := ScreenRect;
  SubMenuTitleFont.Print(R.Left,
    R.Top - (FirstDemoLevelIndex + 2) *
      (Font.RowHeight + RegularSpaceBetweenItems) + 10,
    SubMenuTextColor, 'Bonus demo levels :');
end;

procedure TChooseNewLevelMenu.Click;
begin
  inherited;

  if CurrentItem = LevelsNewGame.Count then
  begin
    SetCurrentMenu(CurrentMenu, MainMenu);
  end else
  if LevelsNewGame[CurrentItem] = nil then
  begin
    { separator between non-demo and demo levels, do nothing }
  end else
  begin
    NewGame(LevelsNewGame[CurrentItem]);
    SetCurrentMenu(CurrentMenu, MainMenu);
  end;
end;

{ global things -------------------------------------------------------------- }

constructor TStateStartMenu.Create(AOwner: TComponent);
begin
  inherited;
  MainMenu := TMainMenu.Create(Application);
  VideoMenu := TVideoMenu.Create(Application);
  SoundMenu := TSoundMenu.Create(Application);
  ChangeOpenALDeviceMenu := TChangeOpenALDeviceMenu.Create(Application);
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
  SetCurrentMenu(CurrentMenu, MainMenu);
  if not GoingToAnotherMenu then
    InsertBack(BackgroundControls);
  GoingToAnotherMenu := false;
end;

procedure TStateStartMenu.Pause;
begin
  SetCurrentMenu(CurrentMenu, nil);
  if not GoingToAnotherMenu then
    RemoveControl(BackgroundControls);
  inherited;
end;

end.
