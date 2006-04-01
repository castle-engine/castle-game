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
unit CastleControlsMenu;

interface

uses OpenGLh, CastleGeneralMenu;

type
  TSubMenu = class(TCastleMenu)
    SubMenuTitle: string;
    SubMenuAdditionalInfo: string;
    constructor Create;
    procedure Draw; override;
  end;

procedure ShowControlsMenu(AGLList_ScreenImage: TGLuint;
  ADrawFadeRect, ADrawCentered: boolean);

implementation

uses SysUtils, GLWindow, GLWinModes, KambiGLUtils, GLWinMessages, CastleWindow,
  GLMenu, OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_m18_Unit,
  OpenGLFonts, CastleKeys, Keys, VectorMath, KambiUtils, CastlePlay;

{ TCastleMenu descendants interface ------------------------------------------ }

type
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

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleMenu;
  ControlsMenu: TControlsMenu;
  BasicControlsMenu: TBasicControlsMenu;
  ItemsControlsMenu: TItemsControlsMenu;
  OtherControlsMenu: TOtherControlsMenu;
  SubMenuTitleFont: TGLBitmapFont_Abstract;

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
const
  SubMenuTextColor: TVector3Single = (0.7, 0.7, 0.7);
begin
  inherited;

  glColorv(SubMenuTextColor);

  glPushMatrix;
    glTranslatef(Position[0], Position[1] - 20, 0);
    glRasterPos2i(0, 0);
    SubMenuTitleFont.Print(SubMenuTitle + ' :');
  glPopMatrix;

  if SubMenuAdditionalInfo <> '' then
  begin
    glPushMatrix;
      glTranslatef(AllItemsArea.X0,
        AllItemsArea.Y0 - SubMenuTitleFont.RowHeight, 0);
      SubMenuTitleFont.PrintBrokenString(SubMenuAdditionalInfo,
        RequiredScreenWidth - 2 * Round(AllItemsArea.X0), 0, 0, true, 0);
    glPopMatrix;
  end;
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
    3: UserQuit := true;
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

{ TItemsControlsMenu --------------------------------------------------------- }

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

  SubMenuAdditionalInfo :=
    'Notes:' +nl+
    '- You pick items lying on the ground just by walking on them.' +nl+
    '- Items are automatically unequipped when you drop them.';

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

constructor TOtherControlsMenu.Create;
begin
  inherited Create;

  Items.AddObject('View all messages', KeyArgument(CastleKey_ViewMessages));
  Items.AddObject('Save screen', KeyArgument(CastleKey_SaveScreen));
  Items.AddObject('Cancel flying', KeyArgument(CastleKey_CancelFlying));
  Items.Add('Restore defaults');
  Items.Add('Back to controls menu');

  SubMenuTitle := 'Configure other controls';

  SubMenuAdditionalInfo :=
    'Non-configurable keys:' +nl+
    '  Escape = exit to game menu' +nl+
    '  ` (backquote) = FPS show / hide' +nl+
    '  L / Shift + L = (cheating) life increase/decrease' +nl+
    nl+
    'Mouse:' +nl+
    '  left mouse click on anything =' +nl+
    '    show information about pointed item on the level,' +nl+
    '    or use pointed device (press button, move switch etc.)' +nl+
    nl+
    'Note that the "Cancel flying" key is useful if you want to stop flying ' +
    'before flying spell will automatically wear off.';

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TOtherControlsMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0..3: MessageOK(Glw, 'TODO: Not implemented yet');
    4: CurrentMenu := ControlsMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ global things -------------------------------------------------------------- }

const
  WholeAreaX0 = 10;
  WholeAreaY0 = 20;
  WholeAreaX1 = 750;
  WholeAreaY1 = 450;

var
  GLList_ScreenImage: TGLuint;
  DrawFadeRect: boolean;
  GLList_DrawFadeRect: TGLuint;
  MoveX, MoveY: Single;

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);

  glTranslatef(MoveX, MoveY, 0);
  if DrawFadeRect then
    glCallList(GLList_DrawFadeRect);
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
  CurrentMenu.MouseMove(NewX - MoveX, Glwin.Height - NewY - MoveY);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX - MoveX,
    Glwin.Height - Glwin.MouseY - MoveY, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.FpsCompSpeed);
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

procedure ShowControlsMenu(AGLList_ScreenImage: TGLuint;
  ADrawFadeRect, ADrawCentered: boolean);
var
  SavedMode: TGLMode;
begin
  GLList_ScreenImage := AGLList_ScreenImage;
  DrawFadeRect := ADrawFadeRect;

  if ADrawCentered then
  begin
    MoveX := - WholeAreaX0 + (RequiredScreenWidth - (WholeAreaX1 - WholeAreaX0)) / 2;
    MoveY := - WholeAreaY0 + (RequiredScreenHeight - (WholeAreaY1 - WholeAreaY0)) / 2;
  end else
  begin
    MoveX := 0;
    MoveY := 0;
  end;

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
  if GLList_DrawFadeRect = 0 then
    GLList_DrawFadeRect := glGenLists(1);
  glNewList(GLList_DrawFadeRect, GL_COMPILE);
  try
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, 0.4);
      glRectf(WholeAreaX0, WholeAreaY0, WholeAreaX1, WholeAreaY1);
    glDisable(GL_BLEND);
  finally glEndList end;

  ControlsMenu := TControlsMenu.Create;
  BasicControlsMenu := TBasicControlsMenu.Create;
  ItemsControlsMenu := TItemsControlsMenu.Create;
  OtherControlsMenu := TOtherControlsMenu.Create;
  CurrentMenu := ControlsMenu;
  SubMenuTitleFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m18);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil; { just for safety }
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