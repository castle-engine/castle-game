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
  CastleControlsMenu;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TMainMenu = class(TCastleMenu)
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
  VideoMenu: TVideoMenu;
  GLList_ScreenImage: TGLuint;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create;
begin
  inherited Create;

  Items.Add('New game (The Gate - new level for PGD stage 4)');
  Items.Add('New game (Tower - just a test level)');
  Items.Add('New game (Castle Hall - new level for PGD stage 3)');
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
    0: NewGame(
         TLevel.Create('gate_final.wrl', 'gate_lights.wrl'));
    1: NewGame(
         TLevel.Create('basic_castle_final.wrl', 'basic_castle_lights.wrl'));
    2: NewGame(TCastleHallLevel.Create);
    3: ShowControlsMenu(GLList_ScreenImage, false, false);
    4: CurrentMenu := VideoMenu;
    5: ViewSoundInfo;
    6: ShowCreditsMessage;
    7: UserQuit := true;
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
  case Key of
    K_F5: SaveScreen;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY);
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
    Glw.OnMouseUp := MouseUp;
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
  GLList_ScreenImage := LoadImageToDispList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'menu_bg.png',
    [TRGBImage], [], Glw.Width, Glw.Height);

  MainMenu := TMainMenu.Create;
  VideoMenu := TVideoMenu.Create;
  CurrentMenu := MainMenu;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil; { just for safety }
  FreeAndNil(MainMenu);
  FreeAndNil(VideoMenu);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.