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
unit GameStartMenu;

interface

uses CastleWindow;

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayGame), when user wants to quit --- return. }
procedure ShowStartMenu;

implementation

uses SysUtils, Classes, CastleUtils, WindowModes,
  GL, GLU, CastleGLUtils, CastleMessages, GameWindow,
  VectorMath, Images, CastleFilesUtils,
  CastleLevels, GamePlay, GameSound, CastlePlayer,
  GameCreatures, CastleItems, GameGeneralMenu, OnScreenMenu,
  GameControlsMenu, CastleInputs, GameVideoOptions, GameHelp,
  CastleStringUtils, CastleClassUtils,
  CastleGameNotifications, GameBackgroundLevel, UIControls,
  CastleSoundEngine, CastleSoundMenu, X3DNodes, CastleResources,
  GameCredits, KeysMouse, CastleOpenDocument,
  PrecalculatedAnimation;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleGameMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TAntiAliasingSlider = class(TMenuIntegerSlider)
    constructor Create;
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TVideoMenu = class(TSubMenu)
  public
    AllowScreenChangeArgument: TMenuBooleanArgument;
    ShadowVolumesArgument: TMenuBooleanArgument;
    AnimationSmoothnessSlider: TMenuFloatSlider;
    ColorBitsArgument: TMenuArgument;
    VideoFrequencyArgument: TMenuArgument;
    ConserveResourcesArgument: TMenuBooleanArgument;
    AntiAliasingSlider: TAntiAliasingSlider;
    constructor Create(AOwner: TComponent); override;
    procedure SetAntiAliasing(
      Value: TAntiAliasing;
      UpdateSlider: boolean);
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TSoundMenu = class(TSubMenu)
  public
    SoundInfo: TSoundInfoMenuItem;
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;

    OpenALDeviceArgument: TMenuArgument;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TChangeOpenALDeviceMenu = class(TSubMenu)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

  TChooseNewLevelMenu = class(TSubMenu)
  public
    LevelsNewGame: TLevelAvailableList;
    FirstDemoLevelIndex: Cardinal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; override;
    procedure Draw; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  UserQuit: boolean;
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
  that will also call PlayGame, but creating / initializing
  TPlayer and TLevel instances differently. }
procedure NewGame(Level: TLevelAvailable);
begin
  SceneManager := TGameSceneManager.Create(nil);
  try
    SceneManager.ShadowVolumes := ShadowVolumes;
    SceneManager.ShadowVolumesDraw := ShadowVolumesDraw;
    SceneManager.ApproximateActivation := true;
    repeat
      Notifications.Clear;
      Player := TPlayer.Create(nil);
      try
        Player.Camera.Input_Run.MakeClear; { speed in castle1 is so fast that we're always running }
        PlayerUpdateMouseLook(Player);

        SceneManager.Player := Player;
        SceneManager.Items.Add(Player);

        SceneManager.LoadLevel(Level);
        PlayGame(true);
      finally FreeAndNil(Player) end;

      if GameEnded and (GameEndedWantsRestart <> '') then
        Level := LevelsAvailable.FindName(GameEndedWantsRestart) else
        Break;
    until false;
  finally FreeAndNil(SceneManager) end;

  SoundEngine.MusicPlayer.Sound := stIntroMusic;
  SoundMenu.SoundVolume.RefreshAccessory;
  SoundMenu.MusicVolume.RefreshAccessory;
  Notifications.Clear;
end;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create(AOwner: TComponent);
begin
  inherited;

  Items.Add('New game');
  Items.Add('Configure controls');
  Items.Add('Video options');
  Items.Add('Sound options');
  Items.Add('Credits');
  Items.Add('Visit our website');
  Items.Add('Quit');

  Position := Vector2Integer(20, 480);
  PositionRelativeScreenX := prLowerBorder;
  PositionRelativeScreenY := prLowerBorder;
  PositionRelativeMenuX := prLowerBorder;
  PositionRelativeMenuY := prHigherBorder;

  DrawBackgroundRectangle := false;
end;

procedure TMainMenu.Click;

  procedure ChooseNewGame;

    procedure SetChooseNewLevelMenu;
    begin
      { Recreate ChooseNewLevelMenu now, to refresh list of LevelsNewGame. }
      FreeAndNil(ChooseNewLevelMenu);
      ChooseNewLevelMenu := TChooseNewLevelMenu.Create(Application);

      SetCurrentMenu(CurrentMenu, ChooseNewLevelMenu);
    end;

  begin
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

begin
  inherited;

  case CurrentItem of
    0: ChooseNewGame;
    1: ShowControlsMenu(BackgroundControls, false, false);
    2: SetCurrentMenu(CurrentMenu, VideoMenu);
    3: SetCurrentMenu(CurrentMenu, SoundMenu);
    4: ShowCredits(BackgroundControls, BackgroundSceneManager);
    5: if not OpenURL(CastleURL) then MessageOK(Window, SCannotOpenURL, taMiddle);
    6: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TAntiAliasingSlider ------------------------------------------ }

constructor TAntiAliasingSlider.Create;
begin
  inherited Create(Ord(Low(TAntiAliasing)), Ord(High(TAntiAliasing)),
    Ord(Window.AntiAliasing));
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

  AllowScreenChangeArgument := TMenuBooleanArgument.Create(AllowScreenChange);
  ShadowVolumesArgument := TMenuBooleanArgument.Create(ShadowVolumes);
  AnimationSmoothnessSlider := TMenuFloatSlider.Create(
    MinAnimationSmoothness,
    MaxAnimationSmoothness,
    AnimationSmoothness);

  ColorBitsArgument := TMenuArgument.Create(
    TMenuArgument.TextWidth(SSystemDefault));
  ColorBitsArgument.Value := ColorBitsToStr(ColorBits);

  VideoFrequencyArgument := TMenuArgument.Create(
    TMenuArgument.TextWidth(SSystemDefault));
  VideoFrequencyArgument.Value := VideoFrequencyToStr(VideoFrequency);

  AntiAliasingSlider := TAntiAliasingSlider.Create;

  Items.Add('View video information');
  Items.AddObject('Allow screen settings change on startup', AllowScreenChangeArgument);
  Items.AddObject('Shadow volumes', ShadowVolumesArgument);
  Items.AddObject('Animation smoothness', AnimationSmoothnessSlider);
  Items.AddObject('Color bits', ColorBitsArgument);
  Items.AddObject('Display frequency', VideoFrequencyArgument);
  Items.AddObject('Anti-aliasing', AntiAliasingSlider);
  Items.Add('Restore to defaults');
  Items.Add('Back to main menu');

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
    MessageOK(Window,
      'Video information:' +nl+
      nl+
      Format('Field of view horizontal : %f', [ViewAngleDegX]) +nl+
      Format('Field of view vertical : %f', [ViewAngleDegY]) +nl+
      nl+
      GLInformationString,
      taLeft);
  end;

  procedure ChangeColorBits;
  begin
    if ColorBits = 0 then
      ColorBits := 16 else
    if ColorBits = 16 then
      ColorBits := 24 else
      ColorBits := 0;
    ColorBitsArgument.Value := ColorBitsToStr(ColorBits);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  procedure ChangeVideoFrequency;
  var
    Value: Cardinal;
  begin
    Value := VideoFrequency;
    if MessageInputQueryCardinal(Window,
      'What display frequency to use ?' +nl+ '("0" means "system default")',
      Value, taLeft) and
      (Value <> VideoFrequency) then
    begin
      VideoFrequency := Value;
      VideoFrequencyArgument.Value := VideoFrequencyToStr(VideoFrequency);
      SubMenuAdditionalInfo := SRestartTheGame;
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: ViewVideoInfo;
    1: begin
         AllowScreenChange := not AllowScreenChange;
         AllowScreenChangeArgument.Value := AllowScreenChange;
       end;
    2: begin
         ShadowVolumes := not ShadowVolumes;
         ShadowVolumesArgument.Value := ShadowVolumes;
         if (not GLShadowVolumesPossible) and ShadowVolumes then
           MessageOK(Window, 'Your OpenGL implementation doesn''t support stencil buffer necessary for shadow volumes. Shadows (by shadow volumes) will not actually work. Try updating graphic card drivers.', taLeft);
       end;
    3: ;
    4: ChangeColorBits;
    5: ChangeVideoFrequency;
    6: ;
    7: begin
         AllowScreenChange := DefaultAllowScreenChange;
         AllowScreenChangeArgument.Value := AllowScreenChange;

         ShadowVolumes := DefaultShadowVolumes;
         ShadowVolumesArgument.Value := ShadowVolumes;

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
           ColorBitsArgument.Value := ColorBitsToStr(DefaultColorBits);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         if VideoFrequency <> DefaultVideoFrequency then
         begin
           VideoFrequency := DefaultVideoFrequency;
           VideoFrequencyArgument.Value := VideoFrequencyToStr(DefaultVideoFrequency);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         SetAntiAliasing(DefaultAntiAliasing, true);

         VisibleChange;

         MessageOK(Window, 'All video settings restored to defaults.', taLeft);
       end;
    8 : SetCurrentMenu(CurrentMenu, MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TVideoMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    3: AnimationSmoothness := AnimationSmoothnessSlider.Value;
    6: SetAntiAliasing(TAntiAliasing(AntiAliasingSlider.Value), false);
  end;
end;

{ TSoundMenu ------------------------------------------------------------- }

constructor TSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  OpenALDeviceArgument := TMenuArgument.Create(450);
  OpenALDeviceArgument.Value := SoundEngine.DeviceNiceName;

  SoundInfo := TSoundInfoMenuItem.Create(Window, Self);
  SoundVolume := TSoundVolumeMenuItem.Create(Window, Self);
  MusicVolume := TMusicVolumeMenuItem.Create(Window, Self);
  Items.AddObject('Sound output device', OpenALDeviceArgument);
  Items.Add('Back to main menu');

  SubMenuTitle := 'Sound options';
end;

destructor TSoundMenu.Destroy;
begin
  FreeAndNil(SoundInfo);
  FreeAndNil(SoundVolume);
  FreeAndNil(MusicVolume);
  inherited;
end;

procedure TSoundMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: SoundInfo.Selected;
    1: ;
    2: ;
    3: SetCurrentMenu(CurrentMenu, ChangeOpenALDeviceMenu);
    4: SetCurrentMenu(CurrentMenu, MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TSoundMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    1: SoundVolume.AccessoryValueChanged;
    2: MusicVolume.AccessoryValueChanged;
  end;
end;

{ TChangeOpenALDeviceMenu ---------------------------------------------------- }

constructor TChangeOpenALDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  for I := 0 to SoundEngine.Devices.Count - 1 do
    Items.Add(SoundEngine.Devices[I].NiceName);
  Items.Add('Cancel');

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
    SoundMenu.OpenALDeviceArgument.Value := SoundEngine.Devices[CurrentItem].NiceName;
    if not SoundEngine.ALActive then
      MessageOK(Window, SoundEngine.SoundInitializationReport, taLeft);
  end;

  SetCurrentMenu(CurrentMenu, SoundMenu);
end;

{ TChooseNewLevelMenu ------------------------------------------------------- }

constructor TChooseNewLevelMenu.Create(AOwner: TComponent);

  { Add level to LevelsNewGame and Items lists.
    Index is an index into LevelsAvailable array for this level. }
  procedure AddLevel(Index: Integer);
  var
    S: string;
    L: TLevelAvailable;
  begin
    L := LevelsAvailable[Index];
    LevelsNewGame.Add(L);
    S := Format('%d: %s', [ L.Number, L.Title ]);
    if L.TitleHint <> '' then
      S += ' (' + L.TitleHint + ')';
    Items.Add(S);
  end;

var
  I: Integer;
begin
  inherited;

  LevelsNewGame := TLevelAvailableList.Create(false);

  LevelsAvailable.SortByNumber;

  { Add non-demo levels }
  for I := 0 to LevelsAvailable.Count - 1 do
    if LevelsAvailable[I].Played and
       (LevelsAvailable[I].Name <> MenuBackgroundLevelName) and
       not LevelsAvailable[I].Demo then
      AddLevel(I);

  FirstDemoLevelIndex := LevelsNewGame.Count;

  { Add demo levels }
  for I := 0 to LevelsAvailable.Count - 1 do
    if LevelsAvailable[I].Played and
       (LevelsAvailable[I].Name <> MenuBackgroundLevelName) and
       LevelsAvailable[I].Demo then
      AddLevel(I);

  Items.Add('Cancel');

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
  if NextItemIndex = Items.Count - 1 then
    { some separator is needed before "cancel" button now,
      since otherwise it seems to attached to "demo" levels section. }
    Result += 10;
end;

procedure TChooseNewLevelMenu.Draw;
const
  SubMenuTextColor: TVector3Single = (0.7, 0.7, 0.7);
begin
  inherited;

  glColorv(SubMenuTextColor);

  glPushMatrix;
    glTranslatef(Position[0],
      Rectangles.Items[FirstDemoLevelIndex].Y0 +
      Rectangles.Items[FirstDemoLevelIndex].Height + 5 { margin }, 0);
    glRasterPos2i(0, 0);
    SubMenuTitleFont.Print('Bonus demo levels :');
  glPopMatrix;
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

procedure CloseQuery(Window: TCastleWindowBase);
begin
  if MessageYesNo(Window, 'Are you sure you want to quit ?') then
    UserQuit := true;
end;

procedure ShowStartMenu;
var
  SavedMode: TGLMode;
begin
  BackgroundCreate;
  try
    SoundEngine.MusicPlayer.Sound := stIntroMusic;
    try
      SavedMode := TGLMode.CreateReset(Window, 0, false,
        nil, nil, @CloseQuery);
      try
        SavedMode.RestoreProjectionMatrix := false;

        SetCurrentMenu(CurrentMenu, MainMenu);

        Window.Controls.Add(Notifications);
        Window.Controls.AddList(BackgroundControls);

        UserQuit := false;
        repeat
          Application.ProcessMessage(true, true);
        until UserQuit;
      finally FreeAndNil(SavedMode); end;
    finally SoundEngine.MusicPlayer.Sound := stNone; end;
  finally
    BackgroundDestroy;
  end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(const Container: IUIContainer);
begin
  MainMenu := TMainMenu.Create(Application);
  VideoMenu := TVideoMenu.Create(Application);
  SoundMenu := TSoundMenu.Create(Application);
  ChangeOpenALDeviceMenu := TChangeOpenALDeviceMenu.Create(Application);
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
finalization
end.