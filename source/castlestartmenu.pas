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

  TControlsMenu = class(TSubMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TBasicControlsMenu = class(TSubMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TItemsControlsMenu = class(TSubMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TOtherControlsMenu = class(TSubMenu)
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
  ControlsMenu: TControlsMenu;
  BasicControlsMenu: TBasicControlsMenu;
  ItemsControlsMenu: TItemsControlsMenu;
  OtherControlsMenu: TOtherControlsMenu;
  VideoMenu: TVideoMenu;
  SubMenuTitleFont: TGLBitmapFont_Abstract;

{ TMainMenu ------------------------------------------------------------ }

constructor TMainMenu.Create;
begin
  inherited Create;

  Items.Add('Read instructions');
  Items.Add('New game (The Gate - new level for PGD stage 4)');
  Items.Add('New game (Tower - just a test level)');
  Items.Add('New game (Castle Hall - new level for PGD stage 3)');
  Items.Add('Configure controls');
  Items.Add('Video options');
  Items.Add('Sound options');
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
    0: ShowHelpMessage;
    1: NewGame(
         TLevel.Create('gate_final.wrl', 'gate_lights.wrl'));
    2: NewGame(
         TLevel.Create('basic_castle_final.wrl', 'basic_castle_lights.wrl'));
    3: NewGame(TCastleHallLevel.Create);
    4: CurrentMenu := ControlsMenu;
    5: CurrentMenu := VideoMenu;
    6: ViewSoundInfo;
    7: UserQuit := true;
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

  DrawBackgroundRectangle := false;
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

{ TControlsMenu ------------------------------------------------------------- }

function KeyArgument(const Key: TKey): TGLMenuItemArgument;
begin
  Result := TGLMenuItemArgument.Create(
    TGLMenuItemArgument.TextWidth('WWWWWWWWWWWW'));
  Result.Value := KeyToStr(Key);
end;

constructor TControlsMenu.Create;
begin
  inherited Create;

  Items.Add('Basic controls');
  Items.Add('Items controls');
  Items.Add('Other controls');
  Items.Add('Back to main menu');

  SubMenuTitle := 'Configure controls';

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TControlsMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0: CurrentMenu := BasicControlsMenu;
    1: CurrentMenu := ItemsControlsMenu;
    2: CurrentMenu := OtherControlsMenu;
    3: CurrentMenu := MainMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TBasicControlsMenu ------------------------------------------------------------- }

constructor TBasicControlsMenu.Create;
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
  Items.Add('Back to controls menu');

  SubMenuTitle := 'Configure basic controls';

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TBasicControlsMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0..12: MessageOK(Glw, 'TODO: Not implemented yet');
    13: CurrentMenu := ControlsMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TItemsControlsMenu ------------------------------------------------------------- }

{ TODO: when drawing this, show text:

  Notes:
  - you pick items lying on the ground just by walking on them.
  - items are automatically unequipped when you drop them. }

constructor TItemsControlsMenu.Create;
begin
  inherited Create;

  Items.AddObject('Inventory show / hide', KeyArgument(CastleKey_InventoryShow));
  Items.AddObject('Select previous inventory item', KeyArgument(CastleKey_InventoryPrevious));
  Items.AddObject('Select next inventory item', KeyArgument(CastleKey_InventoryNext));
  Items.AddObject('Use (or equip) selected inventory item', KeyArgument(CastleKey_UseItem));
  Items.AddObject('Drop selected inventory item', KeyArgument(CastleKey_DropItem));
  Items.Add('Restore defaults');
  Items.Add('Back to controls menu');

  SubMenuTitle := 'Configure items controls';

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TItemsControlsMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0..5: MessageOK(Glw, 'TODO: Not implemented yet');
    6: CurrentMenu := ControlsMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TOtherControlsMenu ------------------------------------------------------------- }

{ TODO: when drawing this, show text:

  Notes:
  - cancel flying is useful only if you want to stop flying
    before flying spell will automatically wear off

  Escape = exit to game menu
  ` (backquote) = FPS show / hide

Mouse:
  left mouse click on anything =
    show information about pointed item on the level,
    or use pointed device (press button, move switch etc.)

Testing (cheating) keys:
  L / Shift + L = life increase/decrease
}

constructor TOtherControlsMenu.Create;
begin
  inherited Create;

  Items.AddObject('Show help', KeyArgument(CastleKey_ShowHelp));
  Items.AddObject('View all messages', KeyArgument(CastleKey_ViewMessages));
  Items.AddObject('Save screen', KeyArgument(CastleKey_SaveScreen));
  Items.AddObject('Cancel flying', KeyArgument(CastleKey_CancelFlying));
  Items.Add('Restore defaults');
  Items.Add('Back to controls menu');

  SubMenuTitle := 'Configure other controls';

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TOtherControlsMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0..4: MessageOK(Glw, 'TODO: Not implemented yet');
    5: CurrentMenu := ControlsMenu;
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
  ControlsMenu := TControlsMenu.Create;
  BasicControlsMenu := TBasicControlsMenu.Create;
  ItemsControlsMenu := TItemsControlsMenu.Create;
  OtherControlsMenu := TOtherControlsMenu.Create;
  CurrentMenu := MainMenu;
  SubMenuTitleFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m18);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil; { just for safety }
  FreeAndNil(MainMenu);
  FreeAndNil(VideoMenu);
  FreeAndNil(ControlsMenu);
  FreeAndNil(BasicControlsMenu);
  FreeAndNil(ItemsControlsMenu);
  FreeAndNil(OtherControlsMenu);
  FreeAndNil(SubMenuTitleFont);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.