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

  TControlsSubMenu = class(TSubMenu)
  private
    FGroup: TKeyGroup;
    procedure KeyChanged(KeyConfiguration: TKeyConfiguration);
  public
    constructor Create(AGroup: TKeyGroup);
    destructor Destroy; override;

    property Group: TKeyGroup read FGroup;
    procedure CurrentItemSelected; override;
  end;

  TBasicControlsMenu = class(TControlsSubMenu)
    constructor Create;
  end;

  TItemsControlsMenu = class(TControlsSubMenu)
    constructor Create;
  end;

  TOtherControlsMenu = class(TControlsSubMenu)
    constructor Create;
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

constructor TControlsMenu.Create;
begin
  inherited Create;

  Items.Add('Configure basic controls');
  Items.Add('Configure items controls');
  Items.Add('Configure other controls');
  Items.Add('Restore all keys to defaults');
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
    3: begin
         CastleAllKeys.RestoreDefaults;
         MessageOK(Glw, 'All keys restored to defaults.');
       end;
    4: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TControlsSubMenu ----------------------------------------------------------- }

constructor TControlsSubMenu.Create(AGroup: TKeyGroup);

  function KeyArgument(const Key: TKey): TGLMenuItemArgument;
  begin
    Result := TGLMenuItemArgument.Create(
      TGLMenuItemArgument.TextWidth('WWWWWWWWWWWW'));
    Result.Value := KeyToStr(Key);
  end;

var
  I: Integer;
begin
  inherited Create;
  FGroup := AGroup;

  for I := 0 to CastleGroupKeys[Group].High do
    Items.AddObject(
      CastleGroupKeys[Group].Items[I].Name,
      KeyArgument(CastleGroupKeys[Group].Items[I].Value));

  Items.Add('Back to controls menu');

  SpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);

  OnKeyChanged.AppendItem(KeyChanged);
end;

destructor TControlsSubMenu.Destroy;
begin
  if OnKeyChanged <> nil then
    OnKeyChanged.DeleteFirstEqual(KeyChanged);
  inherited;
end;

procedure TControlsSubMenu.CurrentItemSelected;
var
  KeyConfiguration, ConflictingKey: TKeyConfiguration;
  NewKey: TKey;
begin
  if Between(CurrentItem, 0, CastleGroupKeys[Group].High) then
  begin
    KeyConfiguration := CastleGroupKeys[Group].Items[CurrentItem];
    NewKey := MessageKey(Glw, Format(
      'Press the new key for "%s".' +nl+
      'Press Escape to cancel.', [KeyConfiguration.Name]), '', taLeft);

    if (NewKey <> K_Escape) and
       { We silently ignore situation when NewKey = KeyConfiguration.Value.
         This is meaningless, and otherwise would raise a message
         that NewKey conflicts with KeyConfiguration. }
       (NewKey <> KeyConfiguration.Value) then
    begin
      ConflictingKey := CastleAllKeys.SeekKeyByValue(NewKey);
      if ConflictingKey <> nil then
        MessageOK(Glw,
          Format('Conflict: Key "%s" is already assigned to action "%s"',
          [KeyToStr(NewKey), ConflictingKey.Name]), taLeft) else
        KeyConfiguration.Value := NewKey;
    end;
  end else
  if CurrentItem = CastleGroupKeys[Group].High + 1 then
  begin
    CurrentMenu := ControlsMenu;
  end else
    raise EInternalError.Create('Menu item unknown');
end;

procedure TControlsSubMenu.KeyChanged(KeyConfiguration: TKeyConfiguration);
var
  I: Integer;
begin
  { Refresh key names displayed in the menu. }

  for I := 0 to CastleGroupKeys[Group].High do
    TGLMenuItemArgument(Items.Objects[I]).Value :=
      KeyToStr(CastleGroupKeys[Group].Items[I].Value);
end;

{ TBasicControlsMenu ------------------------------------------------------------- }

constructor TBasicControlsMenu.Create;
begin
  inherited Create(kgBasic);

  SubMenuTitle := 'Configure basic controls';
end;

{ TItemsControlsMenu --------------------------------------------------------- }

constructor TItemsControlsMenu.Create;
begin
  inherited Create(kgItems);

  SubMenuTitle := 'Configure items controls';

  SubMenuAdditionalInfo :=
    'Notes:' +nl+
    '- You pick items lying on the ground just by walking on them.' +nl+
    '- Items are automatically unequipped when you drop them.';
end;

{ TOtherControlsMenu ------------------------------------------------------------- }

constructor TOtherControlsMenu.Create;
begin
  inherited Create(kgOther);

  SubMenuTitle := 'Configure other controls';

  SubMenuAdditionalInfo :=
    'Escape key:' +nl+
    '  This can be used in game to exit to game menu.' +nl+
    '  In many other cases it can be used to "exit".' +nl+
    '  This key is not configurable.' +nl+
    nl+
    'Mouse:' +nl+
    '  left mouse click on anything =' +nl+
    '    show information about pointed item on the level,' +nl+
    '    or use pointed device (press button, move switch etc.)';

    { Too much info, not needed, I think that player can figure this out:
    nl+
    'Note that the "Cancel flying" key is useful if you want to stop flying ' +
    'before flying spell will automatically wear off.'; }
end;

{ global things -------------------------------------------------------------- }

const
  WholeAreaX0 = 10;
  WholeAreaY0 = 80;
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
  if Key = CastleKey_SaveScreen.Value then
    SaveScreen;
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