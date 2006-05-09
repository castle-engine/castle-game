{
  Copyright 2006 Michalis Kamburelis.

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
}

{ }
unit CastleStartMenu;

interface

uses GLWindow;

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayGame), when user wants to quit -- return. }
procedure ShowStartMenu(ADrawUnderMenu: TDrawFunc);

implementation

uses SysUtils, Classes, KambiUtils, GLWinModes,
  OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, Images, KambiFilesUtils,
  CastleLevel, CastlePlay, CastleSound, CastlePlayer, CastleHelp,
  CastleCreatures, CastleItems, CastleGeneralMenu, GLMenu,
  CastleControlsMenu, CastleKeys, CastleVideoOptions,
  KambiStringUtils, ALUtils, OpenAL, KambiClassUtils, CastleSoundMenu,
  CastleTimeMessages;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TTextureMinificationQualitySlider = class(TGLMenuIntegerSlider)
    constructor Create;
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TVideoMenu = class(TSubMenu)
    TextureMinificationQualitySlider: TGLMenuIntegerSlider;
    AllowScreenChangeArgument: TGLMenuBooleanArgument;
    RenderShadowsArgument: TGLMenuBooleanArgument;
    CreatureAnimationSlider: TGLMenuIntegerSlider;
    ColorDepthArgument: TGLMenuItemArgument;
    VideoFrequencyArgument: TGLMenuItemArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TSoundMenu = class(TSubMenu)
    SoundVolumeSlider: TSoundVolumeSlider;
    MusicVolumeSlider: TSoundVolumeSlider;
    OpenALDeviceArgument: TGLMenuItemArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TChangeOpenALDeviceMenu = class(TSubMenu)
    OpenALDevices: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure CurrentItemSelected; override;
  end;

  TChooseNewLevelMenu = class(TSubMenu)
    LevelsAvailableForNewGame: TLevelsAvailableList;
    constructor Create;
    destructor Destroy; override;
    procedure CurrentItemSelected; override;
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
  DrawUnderMenu: TDrawFunc;

{ NewGame ------------------------------------------------------------- }

{ This is just a wrapper that calls PlayGame.

  Before calling PlayGame it prepares some things (creating player)
  and after calling PlayGame is restores some things
  (menu item's values that could change during the game and music).

  The idea is that in the future there will be LoadGame procedure,
  that will also call PlayGame, but creating / initializing
  TPlayer and TLevel instances differently. }
procedure NewGame(NewGameLevelClass: TLevelClass);
var
  LocalPlayer: TPlayer;
  LocalLevel: TLevel;
begin
  LocalLevel := NewGameLevelClass.Create;
  try
    LocalPlayer := TPlayer.Create;
    try
      PlayGame(LocalLevel, LocalPlayer, true);
    finally FreeAndNil(LocalPlayer) end;
  finally FreeAndNil(LocalLevel) end;

  MusicPlayer.PlayedSound := stIntroMusic;
  SoundMenu.SoundVolumeSlider.Value := SoundVolume;
  SoundMenu.MusicVolumeSlider.Value := MusicVolume;
  TimeMessagesClear;
end;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create;
begin
  inherited Create;

  Items.Add('New game');
  Items.Add('Configure controls');
  Items.Add('Video options');
  Items.Add('Sound options');
  Items.Add('Credits');
  Items.Add('Quit');

  Position := Vector2Single(20, 480);
  PositionRelativeX := prLowerBorder;
  PositionRelativeY := prHigherBorder;

  DrawBackgroundRectangle := false;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TMainMenu.CurrentItemSelected;

  procedure ChooseNewGame;

    procedure SetChooseNewLevelMenu;
    begin
      { Recreate ChooseNewLevelMenu now, to refresh list of levels AvailableForNewGame. }
      FreeAndNil(ChooseNewLevelMenu);
      ChooseNewLevelMenu := TChooseNewLevelMenu.Create;

      CurrentMenu := ChooseNewLevelMenu;
    end;

  var
    I: Integer;
    AvailableForNewGameLevelClass: TLevelClass;
  begin
    AvailableForNewGameLevelClass := nil;
    for I := 0 to LevelsAvailable.High do
      if LevelsAvailable[I].AvailableForNewGame then
      begin
        if AvailableForNewGameLevelClass <> nil then
        begin
          { So we have > 1 level AvailableForNewGame.
            So we have to use ChooseNewLevelMenu }
          SetChooseNewLevelMenu;
          Exit;
        end else
          AvailableForNewGameLevelClass := LevelsAvailable[I].LevelClass;
      end;

    if AvailableForNewGameLevelClass = nil then
      raise EInternalError.Create('No level available for play');

    NewGame(AvailableForNewGameLevelClass);
  end;

begin
  inherited;

  case CurrentItem of
    0: ChooseNewGame;
    1: ShowControlsMenu(DrawUnderMenu, false, false);
    2: CurrentMenu := VideoMenu;
    3: CurrentMenu := SoundMenu;
    4: ShowCreditsMessage;
    5: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
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

constructor TVideoMenu.Create;
begin
  inherited Create;

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

  Items.Add('View video information');
  Items.AddObject('Texture quality', TextureMinificationQualitySlider);
  Items.AddObject('Allow screen settings change on startup', AllowScreenChangeArgument);
  Items.AddObject('Shadows', RenderShadowsArgument);
  Items.AddObject('Creature animation smoothness', CreatureAnimationSlider);
  Items.AddObject('Color depth', ColorDepthArgument);
  Items.AddObject('Display frequency', VideoFrequencyArgument);
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

  FixItemsAreas(Glw.Width, Glw.Height);
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
      GLCapsString,
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
             MessageOK(Glw, 'Shadows are basically implemented and work, ' +
               'but they slow down rendering very much and there are some unresolved issues. ' +
               'That''s the reason why for PGD competition entry ' +
               'shadows are disabled by default. ' +
               'You have been warned!', taLeft);
         end;
       end;
    4: ;
    5: ChangeColorDepthBits;
    6: ChangeVideoFrequency;
    7: begin
         AllowScreenChange := DefaultAllowScreenChange;
         AllowScreenChangeArgument.Value := AllowScreenChange;

         RenderShadows := DefaultRenderShadows;
         RenderShadowsArgument.Value := RenderShadows;

         TextureMinificationQuality := DefaultTextureMinificationQuality;
         TextureMinificationQualitySlider.Value := Ord(TextureMinificationQuality);

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

         SomethingChanged;

         MessageOK(Glw, 'All video settings restored to defaults.', taLeft);
       end;
    8: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TVideoMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    1: TextureMinificationQuality :=
      TTextureMinificationQuality(TextureMinificationQualitySlider.Value);
    4: begin
         if CreatureAnimationScenesPerTime <>
           Cardinal(CreatureAnimationSlider.Value) then
         begin
           CreatureAnimationScenesPerTime := CreatureAnimationSlider.Value;
           SubMenuAdditionalInfo := SRestartTheGame;
         end;
       end;
  end;
end;

{ TSoundMenu ------------------------------------------------------------- }

constructor TSoundMenu.Create;
begin
  inherited Create;

  SoundVolumeSlider := TSoundVolumeSlider.Create(SoundVolume);
  MusicVolumeSlider := TSoundVolumeSlider.Create(MusicVolume);

  OpenALDeviceArgument := TGLMenuItemArgument.Create(450);
  OpenALDeviceArgument.Value := ALCDeviceToNiceStr(ALCDevice);

  Items.Add('View sound information');
  Items.AddObject('Volume', SoundVolumeSlider);
  Items.AddObject('Music volume', MusicVolumeSlider);
  Items.AddObject('Sound output device', OpenALDeviceArgument);
  Items.Add('Back to main menu');

  SubMenuTitle := 'Sound options';

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TSoundMenu.CurrentItemSelected;
begin
  inherited;

  case CurrentItem of
    0: ViewSoundInfo;
    1: ;
    2: ;
    3: CurrentMenu := ChangeOpenALDeviceMenu;
    4: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TSoundMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    1: SoundVolume := SoundVolumeSlider.Value;
    2: MusicVolume := MusicVolumeSlider.Value;
  end;
end;

{ TChangeOpenALDeviceMenu ---------------------------------------------------- }

constructor TChangeOpenALDeviceMenu.Create;
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

  FixItemsAreas(Glw.Width, Glw.Height);
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
    ALContextClose;

    OpenALRestart;

    ALCDevice := OpenALDevices[CurrentItem];
    SoundMenu.OpenALDeviceArgument.Value := ALCDeviceToNiceStr(ALCDevice);
    ALContextInit(false);
    if not ALActive then
      MessageOK(Glw, SoundInitializationReport, taLeft);
  end;

  CurrentMenu := SoundMenu;
end;

{ TChooseNewLevelMenu ------------------------------------------------------- }

constructor TChooseNewLevelMenu.Create;
var
  I: Integer;
begin
  inherited;

  LevelsAvailableForNewGame := TLevelsAvailableList.Create;

  LevelsAvailable.SortByNumber;
  for I := 0 to LevelsAvailable.Count - 1 do
    if LevelsAvailable[I].AvailableForNewGame then
    begin
      LevelsAvailableForNewGame.Add(LevelsAvailable[I]);
      Items.Add(Format('%d: %s',
        [ LevelsAvailable[I].LevelClass.Number,
          LevelsAvailable[I].LevelClass.Title ]));
    end;
  Items.Add('Cancel');

  SubMenuTitle := 'Choose initial level';

  FixItemsAreas(Glw.Width, Glw.Height);
end;

destructor TChooseNewLevelMenu.Destroy;
begin
  FreeAndNil(LevelsAvailableForNewGame);
  inherited;
end;

procedure TChooseNewLevelMenu.CurrentItemSelected;
begin
  inherited;

  if CurrentItem <> LevelsAvailableForNewGame.Count then
    NewGame(LevelsAvailableForNewGame[CurrentItem].LevelClass);

  CurrentMenu := MainMenu;
end;

{ global things -------------------------------------------------------------- }

procedure Draw2d(Draw2DData: Integer);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  CurrentMenu.Draw;
end;

procedure Draw(Glwin: TGLWindow);
begin
  DrawUnderMenu(Glwin);

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    ProjectionGLPushPop(Draw2d, 0, Ortho2dProjMatrix(
      0, RequiredScreenWidth, 0, RequiredScreenHeight));
  glPopAttrib;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  if CastleKey_SaveScreen.IsValue(Key) then
    SaveScreen;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.IdleCompSpeed);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  if MessageYesNo(glwin, 'Are you sure you want to quit ?') then
    UserQuit := true;
end;

procedure ShowStartMenu(ADrawUnderMenu: TDrawFunc);
var
  SavedMode: TGLMode;
begin
  DrawUnderMenu := ADrawUnderMenu;

  MusicPlayer.PlayedSound := stIntroMusic;
  try
    SavedMode := TGLMode.Create(glw, 0, false);
    try
      SavedMode.FakeMouseDown := false;
      { This shouldn't change projection matrix anyway. }
      SavedMode.RestoreProjectionMatrix := false;

      SetStandardGLWindowState(Glw, Draw, CloseQuery, Glw.OnResize,
        nil, false, true { FPSActive should not be needed anymore, but I leave it. },
        false, K_None, #0, false, false);

      Glw.OnKeyDown := KeyDown;
      Glw.OnMouseDown := MouseDown;
      Glw.OnMouseUp := MouseUp;
      Glw.OnMouseMove := MouseMove;
      Glw.OnIdle := Idle;

      UserQuit := false;

      repeat
        Glwm.ProcessMessage(true);
      until UserQuit;

    finally FreeAndNil(SavedMode); end;
  finally MusicPlayer.PlayedSound := stNone; end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  MainMenu := TMainMenu.Create;
  VideoMenu := TVideoMenu.Create;
  SoundMenu := TSoundMenu.Create;
  ChangeOpenALDeviceMenu := TChangeOpenALDeviceMenu.Create;
  CurrentMenu := MainMenu;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil; { just for safety }
  FreeAndNil(MainMenu);
  FreeAndNil(VideoMenu);
  FreeAndNil(SoundMenu);
  FreeAndNil(ChangeOpenALDeviceMenu);
  FreeAndNil(ChooseNewLevelMenu);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.