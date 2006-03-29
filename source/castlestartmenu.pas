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

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayLevel), when user wants to quit -- return. }
procedure ShowStartMenu;

implementation

uses SysUtils, KambiUtils, GLWindow, GLWinModes,
  OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, Images, KambiFilesUtils,
  CastleLevel, CastlePlay, CastleSound, CastlePlayer, CastleHelp,
  CastleCreatures, CastleItems, CastleGeneralMenu, GLMenu,
  OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_m18_Unit, OpenGLFonts,
  CastleKeys, Keys;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TSubMenu = class(TCastleMenu)
    SubMenuTitle: string;
    constructor Create;
    procedure Draw; override;
  end;

  TKeysMenu = class(TSubMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TVideoMenu = class(TSubMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleMenu;
  MainMenu: TMainMenu;
  KeysMenu: TKeysMenu;
  VideoMenu: TVideoMenu;
  SubMenuTitleFont: TGLBitmapFont_Abstract;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create;
begin
  inherited Create;

  Items.Add('Read instructions');
  Items.Add('New game (Tower - just a test level)');
  Items.Add('New game (Castle Hall - new level for PGD stage 3)');
  Items.Add('Configure keys');
  Items.Add('Video options');
  Items.Add('Sound options');
  Items.Add('Quit');

  Position := Vector2Single(20, 210);
  PositionRelativeX := prLowerBorder;
  PositionRelativeY := prLowerBorder;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TMainMenu.CurrentItemSelected;

  procedure NewGame(Level: TLevel);
  var
    Player: TPlayer;
  begin
    try
      CreaturesKinds.PrepareRender;
      ItemsKinds.PrepareRender;
      GameMessages.Clear;
      Player := TPlayer.Create;
      try
        PlayLevel(Level, Player);
      finally Player.Free end;
    finally Level.Free end;
  end;

  procedure ViewSoundInfo;
  begin
    MessageOK(Glw,
      'Sound library (OpenAL) status:' +nl+
      nl+
      SoundInitializationReport +nl+
      nl+
      'TODO: for now, "The Castle" initializes OpenAL '+
      'but it''s not used. It will be used in the future, '+
      'and you will see here some controls to turn sound on/off '+
      'and change sound volume. See my older demo, ' +
      '[http://www.camelot.homedns.org/~michalis/lets_take_a_walk.php] '+
      'if you want to see how I''m dealing with OpenAL.',
      taLeft);
  end;

begin
  case CurrentItem of
    0: ShowHelpMessage;
    1: NewGame(
         TLevel.Create('basic_castle_final.wrl', 'basic_castle_lights.wrl'));
    2: NewGame(TCastleHallLevel.Create);
    3: CurrentMenu := KeysMenu;
    4: CurrentMenu := VideoMenu;
    5: ViewSoundInfo;
    6: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TSubMenu ------------------------------------------------------------- }

constructor TSubMenu.Create;
begin
  inherited Create;

  Position := Vector2Single(20, 440);
  PositionRelativeX := prLowerBorder;
  PositionRelativeY := prHigherBorder;
end;

procedure TSubMenu.Draw;
begin
  inherited;
  glPushMatrix;
    glTranslatef(Position[0], Position[1] - 20, 0);
    glColorv(LightGray3Single);
    glRasterPos2i(0, 0);
    SubMenuTitleFont.Print(SubMenuTitle + ' :');
  glPopMatrix;
end;

{ TKeysMenu ------------------------------------------------------------- }

constructor TKeysMenu.Create;

  function KeyArgument(const Key: TKey): TGLMenuItemArgument;
  begin
    Result := TGLMenuItemArgument.Create(
      TGLMenuItemArgument.TextWidth('WWWWWWWWWWWW'));
    Result.Value := KeyToStr(Key);
  end;

begin
  inherited Create;

  Items.AddObject('Attack', KeyArgument(CastleKey_Attack));
  Items.AddObject('Move forward', KeyArgument(CastleKey_Forward));
  Items.AddObject('Move backward', KeyArgument(CastleKey_Backward));
  Items.AddObject('Turn left', KeyArgument(CastleKey_LeftRot));
  Items.AddObject('Turn right', KeyArgument(CastleKey_RightRot));
  Items.AddObject('Move left', KeyArgument(CastleKey_LeftStrafe));
  Items.AddObject('Move right', KeyArgument(CastleKey_RightStrafe));
  Items.AddObject('Loop up', KeyArgument(CastleKey_UpRotate));
  Items.AddObject('Look down', KeyArgument(CastleKey_DownRotate));
  Items.AddObject('Look straight', KeyArgument(CastleKey_HomeUp));
  Items.AddObject('Jump (or fly up)', KeyArgument(CastleKey_UpMove));
  Items.AddObject('Crouch (or fly down)', KeyArgument(CastleKey_DownMove));
  Items.Add('Restore defaults');
  Items.Add('Back to main menu');

  SubMenuTitle := 'Configure keys';

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TKeysMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0..12: MessageOK(Glw, 'TODO: Not implemented yet');
    13: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TVideoMenu ------------------------------------------------------------- }

constructor TVideoMenu.Create;
begin
  inherited Create;

  Items.Add('View video information');
  Items.Add('Back to main menu');

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
  case CurrentItem of
    0: ViewVideoInfo;
    1: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ global things -------------------------------------------------------------- }

var
  ListBgDraw: TGLuint;

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(ListBgDraw);
  CurrentMenu.Draw;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  case Key of
    K_F5: SaveScreen;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
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

procedure ShowStartMenu;
var
  SavedMode: TGLMode;
begin
  SavedMode := TGLMode.Create(glw, 0, false);
  try
    SetStandardGLWindowState(Glw, Draw, CloseQuery, Resize,
      nil, false, true { FPSActive is needed for FpsCompSpeed in Idle. },
      false, K_None, #0, false, false);

    Glw.OnKeyDown := KeyDown;
    Glw.OnMouseDown := MouseDown;
    Glw.OnMouseMove := MouseMove;
    Glw.OnIdle := Idle;

    Glw.EventResize;

    UserQuit := false;

    repeat
      Glwm.ProcessMessage(true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  ListBgDraw := LoadImageToDispList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'menu_bg.png',
    [TRGBImage], [], Glw.Width, Glw.Height);

  MainMenu := TMainMenu.Create;
  VideoMenu := TVideoMenu.Create;
  KeysMenu := TKeysMenu.Create;
  CurrentMenu := MainMenu;
  SubMenuTitleFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m18);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil; { just for safety }
  FreeAndNil(MainMenu);
  FreeAndNil(VideoMenu);
  FreeAndNil(KeysMenu);
  FreeAndNil(SubMenuTitleFont);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.