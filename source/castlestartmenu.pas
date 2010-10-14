{
  Copyright 2006-2010 Michalis Kamburelis.

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
unit CastleStartMenu;

interface

uses GLWindow;

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayGame), when user wants to quit --- return. }
procedure ShowStartMenu;

implementation

uses SysUtils, Classes, KambiUtils, GLWinModes,
  GL, GLU, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, Images, KambiFilesUtils, UIControls,
  CastleLevel, CastlePlay, CastleSound, CastlePlayer,
  CastleCreatures, CastleItems, CastleGeneralMenu, GLMenu,
  CastleControlsMenu, CastleInputs, CastleVideoOptions,
  KambiStringUtils, ALUtils, KambiOpenAL, KambiClassUtils,
  CastleTimeMessages, CastleLevelAvailable, CastleBackgroundLevel,
  GameSoundEngine, GLSoundMenu,
  CastleRequiredResources, CastleCredits, GLAntiAliasing, KeysMouse;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure CurrentItemSelected; override;
  end;

  TTextureMinificationQualitySlider = class(TGLMenuIntegerSlider)
    constructor Create;
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TAntiAliasingSlider = class(TGLMenuIntegerSlider)
    constructor Create;
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TVideoMenu = class(TSubMenu)
  public
    TextureMinificationQualitySlider: TGLMenuIntegerSlider;
    AllowScreenChangeArgument: TGLMenuBooleanArgument;
    RenderShadowsArgument: TGLMenuBooleanArgument;
    CreatureAnimationSlider: TGLMenuIntegerSlider;
    ColorDepthArgument: TGLMenuItemArgument;
    VideoFrequencyArgument: TGLMenuItemArgument;
    ConserveResourcesArgument: TGLMenuBooleanArgument;
    BumpMappingArgument: TGLMenuBooleanArgument;
    AntiAliasingSlider: TAntiAliasingSlider;
    constructor Create(AOwner: TComponent); override;
    procedure SetTextureMinificationQuality(
      Value: TTextureMinificationQuality;
      UpdateSlider: boolean);
    procedure SetAntiAliasing(
      Value: TAntiAliasing;
      UpdateSlider: boolean);
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TSoundMenu = class(TSubMenu)
  public
    SoundInfo: TGLSoundInfoMenuItem;
    SoundVolume: TGLSoundVolumeMenuItem;
    MusicVolume: TGLMusicVolumeMenuItem;

    OpenALDeviceArgument: TGLMenuItemArgument;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TChangeOpenALDeviceMenu = class(TSubMenu)
  public
    OpenALDevices: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CurrentItemSelected; override;
  end;

  TChooseNewLevelMenu = class(TSubMenu)
  public
    LevelsAvailableForNewGame: TLevelsAvailableList;
    FirstDemoLevelIndex: Cardinal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CurrentItemSelected; override;
    function SpaceBetweenItems(const NextItemIndex: Cardinal): Cardinal; override;
    procedure Draw; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleMenu;
  MainMenu: TMainMenu;
  VideoMenu: TVideoMenu;
  SoundMenu: TSoundMenu;
  ChooseNewLevelMenu: TChooseNewLevelMenu;
  ChangeOpenALDeviceMenu: TChangeOpenALDeviceMenu;

{ NewGame ------------------------------------------------------------- }

{ Just a wrapper that calls PlayGame.

  Before calling PlayGame it prepares some things (creating player)
  and after calling PlayGame is restores some things
  (menu item's values that could change during the game and music).

  The idea is that in the future there will be LoadGame procedure,
  that will also call PlayGame, but creating / initializing
  TPlayer and TLevel instances differently. }
procedure NewGame(NewGameLevelAvailable: TLevelAvailable);
var
  LocalPlayer: TPlayer;
  LocalLevel, NewLocalLevel: TLevel;
  WantsStart: boolean;
begin
  { All kinds must be prepared before instances are created.
    TObjectKind constructors are allowed to depend on this.
    So we must prepare everything before creating the level
    (since TLevel constructor creates some creatures and items on the level). }
  RequireAllCreatures;
  ItemsKinds.PrepareRender;

  LocalLevel := NewGameLevelAvailable.CreateLevel;
  try

    { We loop here, using WantsStart, because user may want to restart
      the level. TPlayer instance can be created each time again when
      restarting, but the replace of LocalLevel with NewLocalLevel must
      be handled in appropriate order (so that restarting leve doesn't
      free and reload all creature animations again, in case of "conserve memory"). }

    WantsStart := true;
    while WantsStart do
    begin
      LocalPlayer := TPlayer.Create;
      try
        PlayGame(LocalLevel, LocalPlayer, true);

        WantsStart := GameEnded and (GameEndedWantsRestart <> '');
        if WantsStart then
        begin
          NewLocalLevel := LevelsAvailable.FindName(GameEndedWantsRestart).CreateLevel;
          FreeAndNil(LocalLevel);
          LocalLevel := NewLocalLevel;
        end;
      finally FreeAndNil(LocalPlayer) end;
    end;

  finally FreeAndNil(LocalLevel) end;

  SoundEngine.MusicPlayer.PlayedSound := stIntroMusic;
  SoundMenu.SoundVolume.RefreshAccessory;
  SoundMenu.MusicVolume.RefreshAccessory;
  TimeMessagesClear;
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
  Items.Add('Quit');

  Position.Init(20, 480);
  PositionRelativeScreenX := prLowerBorder;
  PositionRelativeScreenY := prLowerBorder;
  PositionRelativeMenuX := prLowerBorder;
  PositionRelativeMenuY := prHigherBorder;

  DrawBackgroundRectangle := false;
end;

procedure TMainMenu.CurrentItemSelected;

  procedure ChooseNewGame;

    procedure SetChooseNewLevelMenu;
    begin
      { Recreate ChooseNewLevelMenu now, to refresh list of levels AvailableForNewGame. }
      FreeAndNil(ChooseNewLevelMenu);
      ChooseNewLevelMenu := TChooseNewLevelMenu.Create(Application);

      SetCurrentMenu(CurrentMenu, ChooseNewLevelMenu);
    end;

  begin
    { Initially I had here code to show SetChooseNewLevelMenu only
      if I had more than 1 level with AvailableForNewGame.

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
    4: ShowCredits(BackgroundControls);
    5: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TAntiAliasingSlider ------------------------------------------ }

constructor TAntiAliasingSlider.Create;
begin
  inherited Create(0, MaxAntiAliasing, AntiAliasing);
end;

function TAntiAliasingSlider.ValueToStr(
  const AValue: Integer): string;
begin
  Result := AntiAliasingToStr(AValue);
end;

{ TTextureMinificationQualitySlider ------------------------------------------ }

constructor TTextureMinificationQualitySlider.Create;
begin
  inherited Create(
    Ord(Low(TTextureMinificationQuality)),
    Ord(High(TTextureMinificationQuality)),
    Ord(TextureMinificationQuality));
end;

function TTextureMinificationQualitySlider.ValueToStr(
  const AValue: Integer): string;
begin
  Result := TextureMinificationQualityToStr[
    TTextureMinificationQuality(AValue)];
end;

{ TVideoMenu ------------------------------------------------------------- }

const
  SRestartTheGame = 'You have to restart the game for the ' +
    'new settings to take effect.';

  SSystemDefault = 'System default';

function ColorDepthBitsToStr(const Value: Cardinal): string;
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

  TextureMinificationQualitySlider := TTextureMinificationQualitySlider.Create;
  AllowScreenChangeArgument := TGLMenuBooleanArgument.Create(AllowScreenChange);
  RenderShadowsArgument := TGLMenuBooleanArgument.Create(RenderShadows);
  CreatureAnimationSlider := TGLMenuIntegerSlider.Create(
    MinCreatureAnimationScenesPerTime,
    MaxCreatureAnimationScenesPerTime,
    CreatureAnimationScenesPerTime);

  ColorDepthArgument := TGLMenuItemArgument.Create(
    TGLMenuItemArgument.TextWidth(SSystemDefault));
  ColorDepthArgument.Value := ColorDepthBitsToStr(ColorDepthBits);

  VideoFrequencyArgument := TGLMenuItemArgument.Create(
    TGLMenuItemArgument.TextWidth(SSystemDefault));
  VideoFrequencyArgument.Value := VideoFrequencyToStr(VideoFrequency);

  ConserveResourcesArgument := TGLMenuBooleanArgument.Create(ConserveResources);

  BumpMappingArgument := TGLMenuBooleanArgument.Create(BumpMapping);

  AntiAliasingSlider := TAntiAliasingSlider.Create;

  Items.Add('View video information');
  Items.AddObject('Texture quality', TextureMinificationQualitySlider);
  Items.AddObject('Allow screen settings change on startup', AllowScreenChangeArgument);
  Items.AddObject('Shadows', RenderShadowsArgument);
  Items.AddObject('Creature animation smoothness', CreatureAnimationSlider);
  Items.AddObject('Color depth', ColorDepthArgument);
  Items.AddObject('Display frequency', VideoFrequencyArgument);
  Items.AddObject('Conserve memory', ConserveResourcesArgument);
  Items.AddObject('Bump mapping', BumpMappingArgument);
  Items.AddObject('Anti-aliasing', AntiAliasingSlider);
  Items.Add('Restore to defaults');
  Items.Add('Back to main menu');

  { Resigned ideas for menu options:

    - Texture magnification quality
      Resigned, because magnification GL_NEAREST will look too awful
      to be sensible.

    - Blending (for Attrib_Blending somewhere)
      Resigned, because without blending levels and items and creatures
      will really look too bad to be sensible.

    - Creature animation smoothness
      I actually implemented it.

      But I don't like this implementation. Why ?

      1. CastleCreatures implementation requires that the
         program must be restarted for new CreatureAnimationScenesPerTime
         value to take effect.

      2. Contrary to my expectations, setting it to
         MinCreatureAnimationScenesPerTime does *not* drastically
         reduce "Loading creatures" time. So the setting is not so
         meaningfull for the user.
  }

  SubMenuTitle := 'Video options';

  SubMenuAdditionalInfo := '';

  RegularSpaceBetweenItems := 5;
end;

procedure TVideoMenu.SetTextureMinificationQuality(
  Value: TTextureMinificationQuality;
  UpdateSlider: boolean);
begin
  if TextureMinificationQuality <> Value then
  begin
    TextureMinificationQuality := Value;
    if UpdateSlider then
      TextureMinificationQualitySlider.Value := Ord(TextureMinificationQuality);

    { All items and creatures must be reloaded after
      texture minification filter changed. }
    ItemsKinds.FreePrepareRender;
    UnRequireAllCreatures;
  end;
end;

procedure TVideoMenu.SetAntiAliasing(
  Value: TAntiAliasing;
  UpdateSlider: boolean);
begin
  if AntiAliasing <> Value then
  begin
    AntiAliasing := Value;
    if UpdateSlider then
      AntiAliasingSlider.Value := AntiAliasing;
    SubMenuAdditionalInfo := SRestartTheGame;
  end;
end;

procedure TVideoMenu.CurrentItemSelected;

  procedure ViewVideoInfo;
  begin
    MessageOK(Glw,
      'Video information:' +nl+
      nl+
      Format('Field of view horizontal : %f', [ViewAngleDegX]) +nl+
      Format('Field of view vertical : %f', [ViewAngleDegY]) +nl+
      nl+
      GLInformationString,
      taLeft);
  end;

  procedure ChangeColorDepthBits;
  begin
    if ColorDepthBits = 0 then
      ColorDepthBits := 16 else
    if ColorDepthBits = 16 then
      ColorDepthBits := 32 else
      ColorDepthBits := 0;
    ColorDepthArgument.Value := ColorDepthBitsToStr(ColorDepthBits);
    SubMenuAdditionalInfo := SRestartTheGame;
  end;

  procedure ChangeVideoFrequency;
  var
    Value: Cardinal;
  begin
    Value := VideoFrequency;
    if MessageInputQueryCardinal(Glw,
      'What display frequency to use ?' +nl+ '("0" means "system default")',
      Value, taLeft) and
      (Value <> VideoFrequency) then
    begin
      VideoFrequency := Value;
      VideoFrequencyArgument.Value := VideoFrequencyToStr(VideoFrequency);
      SubMenuAdditionalInfo := SRestartTheGame;
    end;
  end;

  procedure SetConserveResources(Value: boolean);
  begin
    if ConserveResources <> Value then
    begin
      ConserveResources := Value;
      ConserveResourcesArgument.Value := ConserveResources;
      SubMenuAdditionalInfo := SRestartTheGame;
    end;
  end;


begin
  inherited;

  case CurrentItem of
    0: ViewVideoInfo;
    1: ;
    2: begin
         AllowScreenChange := not AllowScreenChange;
         AllowScreenChangeArgument.Value := AllowScreenChange;
       end;
    3: begin
         RenderShadows := not RenderShadows;
         RenderShadowsArgument.Value := RenderShadows;
         if not RenderShadowsPossible then
         begin
           if RenderShadows then
           begin
             MessageOK(Glw, 'Note that shadows are disabled by --no-shadows ' +
               'command-line option. So you must restart the game to see the ' +
               'shadows.', taLeft);
             SubMenuAdditionalInfo := SRestartTheGame;
           end;
         end else
         begin
           if RenderShadows then
             MessageOK(Glw,
               'Warning: this feature requires a good graphic card, otherwise ' +
               'shadows will slow down the game noticeably. ' +
               'That said, with decent graphic card and OpenGL ' +
               'drivers, the main game levels are definitely playable ' +
               'with shadows. So enjoy !' +nl+
               nl+
               'Since 0.8.0 release, both z-fail and z-pass are implemented, ' +
               'so shadows should work 100% OK always. I would like to turn ' +
               'shadows on by default in the future, so please report any ' +
               'problems.', taLeft);
         end;
       end;
    4: ;
    5: ChangeColorDepthBits;
    6: ChangeVideoFrequency;
    7: SetConserveResources(not ConserveResources);
    8: begin
         BumpMapping := not BumpMapping;
         BumpMappingArgument.Value := BumpMapping;
       end;
    9: ;
    10:begin
         AllowScreenChange := DefaultAllowScreenChange;
         AllowScreenChangeArgument.Value := AllowScreenChange;

         RenderShadows := DefaultRenderShadows;
         RenderShadowsArgument.Value := RenderShadows;

         SetTextureMinificationQuality(DefaultTextureMinificationQuality, true);

         if CreatureAnimationScenesPerTime <> DefaultCreatureAnimationScenesPerTime then
         begin
           CreatureAnimationScenesPerTime := DefaultCreatureAnimationScenesPerTime;

           { TODO: FPC bug below ?
             When I change below DefaultCreatureAnimationScenesPerTime
             to CreatureAnimationScenesPerTime, the assignment below doesn't
             work anymore. Doing
               Writeln('Should be ', CreatureAnimationScenesPerTime);
             suddenly makes the assigment working.

             Steps to reproduce : run the game, change the slider value
             using mouse, press "Reset to defaults" and see if the slider
             is drawn back with default value.

             Happens only with FPC 2.0.2 (or 2.0.3 from 2006/03/26)
             with -dRELEASE on Linux.
             With -dDEBUG doesn't happen.
             On Win32 with -dRELEASE doesn't happen. }

           CreatureAnimationSlider.Value := DefaultCreatureAnimationScenesPerTime;
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         if ColorDepthBits <> DefaultColorDepthBits then
         begin
           ColorDepthBits := DefaultColorDepthBits;
           ColorDepthArgument.Value := ColorDepthBitsToStr(DefaultColorDepthBits);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         if VideoFrequency <> DefaultVideoFrequency then
         begin
           VideoFrequency := DefaultVideoFrequency;
           VideoFrequencyArgument.Value := VideoFrequencyToStr(DefaultVideoFrequency);
           SubMenuAdditionalInfo := SRestartTheGame;
         end;

         SetConserveResources(DefaultConserveResources);

         BumpMapping := DefaultBumpMapping;
         BumpMappingArgument.Value := BumpMapping;

         SetAntiAliasing(DefaultAntiAliasing, true);

         VisibleChange;

         MessageOK(Glw, 'All video settings restored to defaults.', taLeft);
       end;
    11: SetCurrentMenu(CurrentMenu, MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TVideoMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    1: begin
         SetTextureMinificationQuality(
           TTextureMinificationQuality(TextureMinificationQualitySlider.Value),
           false);
       end;
    4: begin
         if CreatureAnimationScenesPerTime <>
           Cardinal(CreatureAnimationSlider.Value) then
         begin
           CreatureAnimationScenesPerTime := CreatureAnimationSlider.Value;
           SubMenuAdditionalInfo := SRestartTheGame;
         end;
       end;
    9: SetAntiAliasing(AntiAliasingSlider.Value, false);
  end;
end;

{ TSoundMenu ------------------------------------------------------------- }

constructor TSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  OpenALDeviceArgument := TGLMenuItemArgument.Create(450);
  OpenALDeviceArgument.Value := ALCDeviceToNiceStr(ALCDevice);

  SoundInfo := TGLSoundInfoMenuItem.Create(Glw, Self, SoundEngine);
  SoundVolume := TGLSoundVolumeMenuItem.Create(Glw, Self, SoundEngine);
  MusicVolume := TGLMusicVolumeMenuItem.Create(Glw, Self, SoundEngine);
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

procedure TSoundMenu.CurrentItemSelected;
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

procedure TSoundMenu.CurrentItemAccessoryValueChanged;
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

  OpenALDevices := TStringList.Create;
  OpenALDevices.Append(''); { Default OpenAL device }
  GetOpenALDevices(OpenALDevices);

  for I := 0 to OpenALDevices.Count - 1 do
    Items.Add(ALCDeviceToNiceStr(OpenALDevices[I]));
  Items.Add('Cancel');

  SubMenuTitle := 'Change sound output device';
end;

destructor TChangeOpenALDeviceMenu.Destroy;
begin
  FreeAndNil(OpenALDevices);
  inherited;
end;

procedure TChangeOpenALDeviceMenu.CurrentItemSelected;
begin
  inherited;

  if CurrentItem < OpenALDevices.Count then
  begin
    SoundEngine.ALChangeDevice(OpenALDevices[CurrentItem]);
    { ALCDevice value changed now to new value. }
    SoundMenu.OpenALDeviceArgument.Value := ALCDeviceToNiceStr(ALCDevice);
    if not ALActive then
      MessageOK(Glw, SoundEngine.SoundInitializationReport, taLeft);
  end;

  SetCurrentMenu(CurrentMenu, SoundMenu);
end;

{ TChooseNewLevelMenu ------------------------------------------------------- }

constructor TChooseNewLevelMenu.Create(AOwner: TComponent);

  { Add level to LevelsAvailableForNewGame and Items lists.
    Index is an index into LevelsAvailable array for this level. }
  procedure AddLevel(Index: Integer);
  var
    S: string;
    L: TLevelAvailable;
  begin
    L := LevelsAvailable[Index];
    LevelsAvailableForNewGame.Add(L);
    S := Format('%d: %s', [ L.Number, L.Title ]);
    if L.TitleHint <> '' then
      S += ' (' + L.TitleHint + ')';
    Items.Add(S);
  end;

var
  I: Integer;
begin
  inherited;

  LevelsAvailableForNewGame := TLevelsAvailableList.Create;

  LevelsAvailable.SortByNumber;

  { Add non-demo levels }
  for I := 0 to LevelsAvailable.Count - 1 do
    if LevelsAvailable[I].AvailableForNewGame and
       not LevelsAvailable[I].Demo then
      AddLevel(I);

  FirstDemoLevelIndex := LevelsAvailableForNewGame.Count;

  { Add demo levels }
  for I := 0 to LevelsAvailable.Count - 1 do
    if LevelsAvailable[I].AvailableForNewGame and
       LevelsAvailable[I].Demo then
      AddLevel(I);

  Items.Add('Cancel');

  SubMenuTitle := 'Choose initial level';
end;

destructor TChooseNewLevelMenu.Destroy;
begin
  FreeAndNil(LevelsAvailableForNewGame);
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
    glTranslatef(Position.Data[0],
      Areas.Items[FirstDemoLevelIndex].Y0 +
      Areas.Items[FirstDemoLevelIndex].Height + 5 { margin }, 0);
    glRasterPos2i(0, 0);
    SubMenuTitleFont.Print('Bonus demo levels :');
  glPopMatrix;
end;

procedure TChooseNewLevelMenu.CurrentItemSelected;
begin
  inherited;

  if CurrentItem = LevelsAvailableForNewGame.Count then
  begin
    SetCurrentMenu(CurrentMenu, MainMenu);
  end else
  if LevelsAvailableForNewGame[CurrentItem] = nil then
  begin
    { separator between non-demo and demo levels, do nothing }
  end else
  begin
    NewGame(LevelsAvailableForNewGame[CurrentItem]);
    SetCurrentMenu(CurrentMenu, MainMenu);
  end;
end;

{ global things -------------------------------------------------------------- }

procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0,
    AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
end;

procedure MouseWheel(Glwin: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
end;

procedure Idle(Glwin: TGLWindow);
begin
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  if MessageYesNo(glwin, 'Are you sure you want to quit ?') then
    UserQuit := true;
end;

procedure ShowStartMenu;
var
  SavedMode: TGLMode;
begin
  BackgroundCreate;
  try
    SoundEngine.MusicPlayer.PlayedSound := stIntroMusic;
    try
      SavedMode := TGLMode.CreateReset(glw, 0, false,
        nil, nil, @CloseQuery,
        true { FPSActive should not be needed anymore, but I leave it. });
      try
        SavedMode.RestoreProjectionMatrix := false;

        Glw.OnKeyDown := @KeyDown;
        Glw.OnMouseDown := @MouseDown;
        Glw.OnMouseWheel := @MouseWheel;
        Glw.OnIdle := @Idle;

        SetCurrentMenu(CurrentMenu, MainMenu);

        Glw.Controls.AddList(BackgroundControls);

        UserQuit := false;
        repeat
          Application.ProcessMessage(true);
        until UserQuit;
      finally FreeAndNil(SavedMode); end;
    finally SoundEngine.MusicPlayer.PlayedSound := stNone; end;
  finally
    BackgroundDestroy;
  end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  MainMenu := TMainMenu.Create(Application);
  VideoMenu := TVideoMenu.Create(Application);
  SoundMenu := TSoundMenu.Create(Application);
  ChangeOpenALDeviceMenu := TChangeOpenALDeviceMenu.Create(Application);
end;

initialization
  Glw.OnInitList.Add(@InitGLW);
finalization
end.