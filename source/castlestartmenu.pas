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

procedure DrawInitializationBackground;

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayLevel), when user wants to quit -- return. }
procedure ShowStartMenu;

implementation

uses SysUtils, Classes, KambiUtils, GLWindow, GLWinModes,
  OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, Images, KambiFilesUtils,
  CastleLevel, CastlePlay, CastleSound, CastlePlayer, CastleHelp,
  CastleCreatures, CastleItems, CastleGeneralMenu, GLMenu,
  CastleControlsMenu, CastleKeys, CastleVideoOptions,
  KambiStringUtils, ALUtils, OpenAL, KambiClassUtils, CastleSoundMenu;

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
    AllowScreenResizeArgument: TGLMenuItemArgument;
    RenderShadowsArgument: TGLMenuItemArgument;
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

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleMenu;
  MainMenu: TMainMenu;
  VideoMenu: TVideoMenu;
  SoundMenu: TSoundMenu;
  ChangeOpenALDeviceMenu: TChangeOpenALDeviceMenu;
  GLList_ScreenImage: TGLuint;

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

  procedure NewGame(LocalLevel: TLevel);
  var
    LocalPlayer: TPlayer;
  begin
    try
      CreaturesKinds.PrepareRender;
      ItemsKinds.PrepareRender;
      GameMessages.Clear;
      LocalPlayer := TPlayer.Create;
      try
        PlayLevel(LocalLevel, LocalPlayer);
      finally FreeAndNil(LocalPlayer) end;
    finally FreeAndNil(LocalLevel) end;

    MusicPlayer.PlayedSound := stIntroMusic;
    SoundMenu.SoundVolumeSlider.Value := SoundVolume;
    SoundMenu.MusicVolumeSlider.Value := MusicVolume;
  end;

begin
  inherited;

  case CurrentItem of
    0: NewGame(TGateLevel.Create);
    1: ShowControlsMenu(GLList_ScreenImage, false, false);
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

constructor TVideoMenu.Create;
begin
  inherited Create;

  TextureMinificationQualitySlider := TTextureMinificationQualitySlider.Create;

  AllowScreenResizeArgument := TGLMenuItemArgument.Create(
    TGLMenuItemArgument.TextWidth('WWW'));
  AllowScreenResizeArgument.Value := AllowScreenResizeToStr[AllowScreenResize];

  RenderShadowsArgument := TGLMenuItemArgument.Create(
    TGLMenuItemArgument.TextWidth('WWW'));
  RenderShadowsArgument.Value := BoolToStrYesNo[RenderShadows];

  Items.Add('View video information');
  Items.AddObject('Texture quality', TextureMinificationQualitySlider);
  Items.AddObject('Allow screen resize on startup', AllowScreenResizeArgument);
  Items.AddObject('Shadows', RenderShadowsArgument);
  Items.Add('Back to main menu');

  { Resigned ideas for menu options:

    - Texture magnification quality
      Resigned, because magnification GL_NEAREST will look too awful
      to be sensible.

    - Blending (for Attrib_Blending somewhere)
      Resigned, because without blending levels and items and creatures
      will really look too bad to be sensible.

    - Creature animation smoothness
      Resigned. I actually implemented it:

        CreatureAnimationSlider: TGLMenuIntegerSlider;
        CreatureAnimationSlider := TGLMenuIntegerSlider.Create(
          MinCreatureAnimationScenesPerTime,
          MaxCreatureAnimationScenesPerTime,
          CreatureAnimationScenesPerTime);
        Items.AddObject('Creature animation smoothness', CreatureAnimationSlider);
        2: CreatureAnimationScenesPerTime := CreatureAnimationSlider.Value;

      But I removed this implementation (ConfigFile setting stays,
      so it's kind of a "hidden setting" that user can change
      by manually editing .castle.conf file). Why ?

      1. CastleCreatures implementation requires that the
         program must be restarted for new CreatureAnimationScenesPerTime
         value to take effect.

      2. Contrary to my expectations, setting it to
         MinCreatureAnimationScenesPerTime does *not* drastically
         reduce "Loading creatures" time. So the setting is not so
         meaningfull for the user.
  }

  SubMenuTitle := 'Video options';

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

begin
  inherited;

  case CurrentItem of
    0: ViewVideoInfo;
    1: ;
    2: begin
         AllowScreenResize := not AllowScreenResize;
         AllowScreenResizeArgument.Value :=
           AllowScreenResizeToStr[AllowScreenResize];
       end;
    3: begin
         RenderShadows := not RenderShadows;
         RenderShadowsArgument.Value := BoolToStrYesNo[RenderShadows];
         if not RenderShadowsPossible then
         begin
           if RenderShadows then
             MessageOK(Glw, 'Note that shadows are disabled by --no-shadows ' +
               'command-line option. You must restart the game to see the ' +
               'shadows.', taLeft);
         end else
         begin
           if RenderShadows then
             MessageOK(Glw, 'Shadows are basically implemented and work, ' +
               'but they slow down rendering very much and there are some unresolved issues. ' +
               'That''s the reason why for PGD stage 5 shadows are disabled by default. ' +
               'You have been warned!', taLeft);
         end;
       end;
    4: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TVideoMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    1: TextureMinificationQuality :=
      TTextureMinificationQuality(TextureMinificationQualitySlider.Value);
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

{ global things -------------------------------------------------------------- }

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);
  CurrentMenu.Draw;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  if Key = CastleKey_SaveScreen.Value then
    SaveScreen;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.FpsCompSpeed);
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  if MessageYesNo(glwin, 'Are you sure you want to quit ?') then
    UserQuit := true;
end;

procedure DoDrawInitializationBackground(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);
end;

procedure DrawInitializationBackground;
begin
  Glw.OnResize := Resize;
  Glw.OnDraw := DoDrawInitializationBackground;
  Glw.EventResize;
  Glw.PostRedisplay;
  Glw.FlushRedisplay;
end;

procedure ShowStartMenu;
var
  SavedMode: TGLMode;
begin
  MusicPlayer.PlayedSound := stIntroMusic;
  try
    SavedMode := TGLMode.Create(glw, 0, false);
    try
      SavedMode.FakeMouseDown := false;

      SetStandardGLWindowState(Glw, Draw, CloseQuery, Resize,
        nil, false, true { FPSActive is needed for FpsCompSpeed in Idle. },
        false, K_None, #0, false, false);

      Glw.OnKeyDown := KeyDown;
      Glw.OnMouseDown := MouseDown;
      Glw.OnMouseUp := MouseUp;
      Glw.OnMouseMove := MouseMove;
      Glw.OnIdle := Idle;

      Glw.EventResize;

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
  GLList_ScreenImage := LoadImageToDispList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'menu_bg.png',
    [TRGBImage], [], Glw.Width, Glw.Height);

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
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.