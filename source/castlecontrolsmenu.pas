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

uses GLWindow, OpenGLh, CastleGeneralMenu, MatrixNavigation;

type
  TSubMenu = class(TCastleMenu)
    SubMenuTitle: string;
    { Note that you can freely change this at runtime. No need to call
      things like FixItemsAreas or something like that when you change
      the value of this property. }
    SubMenuAdditionalInfo: string;
    constructor Create;
    procedure Draw; override;
  end;

procedure ShowControlsMenu(ADrawUnderMenu: TDrawFunc;
  ADrawFadeRect, ADrawCentered: boolean);

{ This is like ShowControlsMenu, but user can quit with
  escape key. AExitWithEscape will be set to @true or @false,
  depending on whether user used escape to exit. }
procedure ShowControlsMenuEscape(ADrawUnderMenu: TDrawFunc;
  ADrawFadeRect, ADrawCentered: boolean;
  out AExitWithEscape: boolean);

const
  DefaultUseMouseLook = true;

var
  UseMouseLook: boolean;

  { Global mouse look sensitivity. Are controlled by ShowControlsMenu,
    and are saved/loaded to/from config file in this unit. }
  MouseLookHorizontalSensitivity: Single;
  MouseLookVerticalSensitivity: Single;

implementation

uses SysUtils, GLWinModes, KambiGLUtils, GLWinMessages, CastleWindow,
  GLMenu, OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_m18_Unit,
  OpenGLFonts, CastleKeys, Keys, VectorMath, KambiUtils, CastlePlay,
  CastleConfig, KambiStringUtils, CastleTimeMessages;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TControlsMenu = class(TSubMenu)
    MouseLookHorizontalSensitivitySlider: TGLMenuFloatSlider;
    MouseLookVerticalSensitivitySlider: TGLMenuFloatSlider;
    AutoOpenInventoryArgument: TGLMenuBooleanArgument;
    UseMouseLookArgument: TGLMenuBooleanArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
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

  MouseLookHorizontalSensitivitySlider := TGLMenuFloatSlider.Create(
    0.01, 0.3, MouseLookHorizontalSensitivity);
  MouseLookVerticalSensitivitySlider := TGLMenuFloatSlider.Create(
    0.01, 0.3, MouseLookVerticalSensitivity);
  AutoOpenInventoryArgument := TGLMenuBooleanArgument.Create(AutoOpenInventory);
  UseMouseLookArgument := TGLMenuBooleanArgument.Create(UseMouseLook);

  Items.Add('Configure basic controls');
  Items.Add('Configure items controls');
  Items.Add('Configure other controls');
  Items.AddObject('Use mouse look', UseMouseLookArgument);
  Items.AddObject('Mouse look horizontal sensitivity',
    MouseLookHorizontalSensitivitySlider);
  Items.AddObject('Mouse look vertical sensitivity',
    MouseLookVerticalSensitivitySlider);
  Items.AddObject('Auto show inventory on pickup',
    AutoOpenInventoryArgument);
  Items.Add('Restore to defaults');
  Items.Add('Back to main menu');

  SubMenuTitle := 'Configure controls';

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TControlsMenu.CurrentItemSelected;
begin
  inherited;

  case CurrentItem of
    0: CurrentMenu := BasicControlsMenu;
    1: CurrentMenu := ItemsControlsMenu;
    2: CurrentMenu := OtherControlsMenu;
    3: begin
         UseMouseLook := not UseMouseLook;
         UseMouseLookArgument.Value := UseMouseLook;
       end;
    4: ;
    5: ;
    6: begin
         AutoOpenInventory := not AutoOpenInventory;
         AutoOpenInventoryArgument.Value := AutoOpenInventory;
       end;
    7: begin
         CastleAllKeys.RestoreDefaults;

         UseMouseLook := DefaultUseMouseLook;
         UseMouseLookArgument.Value := UseMouseLook;

         MouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
         MouseLookVerticalSensitivity   := DefaultMouseLookVerticalSensitivity  ;
         MouseLookHorizontalSensitivitySlider.Value := MouseLookHorizontalSensitivity;
         MouseLookVerticalSensitivitySlider  .Value := MouseLookVerticalSensitivity  ;

         AutoOpenInventory := DefaultAutoOpenInventory;
         AutoOpenInventoryArgument.Value := AutoOpenInventory;

         MessageOK(Glw, 'All keys and settings restored to defaults.');
       end;
    8: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TControlsMenu.CurrentItemAccessoryValueChanged;
begin
  inherited;

  case CurrentItem of
    4: MouseLookHorizontalSensitivity :=
         MouseLookHorizontalSensitivitySlider.Value;
    5: MouseLookVerticalSensitivity :=
         MouseLookVerticalSensitivitySlider.Value;
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

  OnKeyChanged.AppendItem(@KeyChanged);
end;

destructor TControlsSubMenu.Destroy;
begin
  if OnKeyChanged <> nil then
    OnKeyChanged.DeleteFirstEqual(@KeyChanged);
  inherited;
end;

procedure TControlsSubMenu.CurrentItemSelected;
var
  KeyConfiguration, ConflictingKey: TKeyConfiguration;
  NewKey: TKey;
begin
  inherited;

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
      begin
        if MessageYesNo(Glw,
          Format('Conflict: Key "%s" is already assigned to action "%s".' + nl+
            nl+
            'Are you sure you want to assign this key to action "%s" (if yes, ' +
            'the assignment for action "%s" will be cleared) ?',
          [ KeyToStr(NewKey),
            ConflictingKey.Name,
            KeyConfiguration.Name,
            ConflictingKey.Name]), taLeft) then
        begin
          ConflictingKey.Value := K_None;
          KeyConfiguration.Value := NewKey;
        end;
      end else
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
    'Controlling game with mouse:' +nl+
    '  Left mouse click performs attack.' +nl+
    '  Right mouse click jumps.' +nl+
    '  Moving mouse rotates the view.';

    { Too much info, not needed, I think that player can figure this out:
    nl+
    'Note that the "Cancel flying" key is useful if you want to stop flying ' +
    'before flying spell will automatically wear off.'; }
end;

{ global things -------------------------------------------------------------- }

const
  WholeAreaX0 = 10;
  WholeAreaY0 = 50;
  WholeAreaX1 = 750;
  WholeAreaY1 = 450;

var
  DrawUnderMenu: TDrawFunc;
  DrawFadeRect: boolean;
  GLList_DrawFadeRect: TGLuint;
  MoveX, MoveY: Single;
  ExitWithEscapeAllowed: boolean;
  ExitWithEscape: boolean;

procedure Draw2d(Draw2DData: Integer);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);

  glTranslatef(MoveX, MoveY, 0);
  if DrawFadeRect then
    glCallList(GLList_DrawFadeRect);
  CurrentMenu.Draw;
end;

procedure Draw(Glwin: TGLWindow);
begin
  DrawUnderMenu(Glwin);

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@Draw2d, 0,
      0, RequiredScreenWidth, 0, RequiredScreenHeight);
  glPopAttrib;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  if CastleKey_SaveScreen.IsValue(Key) then
    SaveScreen else
  if ExitWithEscapeAllowed then
  case C of
    CharEscape:
      begin
        UserQuit := true;
        ExitWithEscape := true;
      end;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX - MoveX, Glwin.Height - NewY - MoveY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX - MoveX,
    Glwin.Height - Glwin.MouseY - MoveY, Button, Glwin.MousePressed);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX - MoveX,
    Glwin.Height - Glwin.MouseY - MoveY, Button, Glwin.MousePressed);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.IdleCompSpeed);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

procedure ShowControlsMenuCore(ADrawUnderMenu: TDrawFunc;
  ADrawFadeRect, ADrawCentered, AExitWithEscapeAllowed: boolean;
  out AExitWithEscape: boolean);
var
  SavedMode: TGLMode;
begin
  DrawUnderMenu := ADrawUnderMenu;
  DrawFadeRect := ADrawFadeRect;
  ExitWithEscapeAllowed := AExitWithEscapeAllowed;
  ExitWithEscape := false;

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
    SavedMode.FakeMouseDown := false;
    { This shouldn't change projection matrix anyway. }
    SavedMode.RestoreProjectionMatrix := false;

    SetStandardGLWindowState(Glw, @Draw, @CloseQuery, Glw.OnResize,
      nil, false, true { FPSActive should not be needed anymore, but I leave it. },
      false, K_None, #0, false, false);

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnMouseUp := @MouseUp;
    Glw.OnMouseMove := @MouseMove;
    Glw.OnIdle := @Idle;

    UserQuit := false;

    repeat
      Glwm.ProcessMessage(true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;

  AExitWithEscape := ExitWithEscape;
end;

procedure ShowControlsMenu(ADrawUnderMenu: TDrawFunc;
  ADrawFadeRect, ADrawCentered: boolean);
var
  Dummy: boolean;
begin
  ShowControlsMenuCore(ADrawUnderMenu, ADrawFadeRect, ADrawCentered,
    false, Dummy);
end;

procedure ShowControlsMenuEscape(ADrawUnderMenu: TDrawFunc;
  ADrawFadeRect, ADrawCentered: boolean;
  out AExitWithEscape: boolean);
begin
  ShowControlsMenuCore(ADrawUnderMenu, ADrawFadeRect, ADrawCentered,
    true, AExitWithEscape);
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  if GLList_DrawFadeRect = 0 then
    GLList_DrawFadeRect := glGenListsCheck(1, 'CastleControlsMenu.InitGLW');
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

  MouseLookHorizontalSensitivity := ConfigFile.GetFloat(
    'mouse/horizontal_sensitivity', DefaultMouseLookHorizontalSensitivity);
  MouseLookVerticalSensitivity := ConfigFile.GetFloat(
    'mouse/vertical_sensitivity', DefaultMouseLookVerticalSensitivity);
  UseMouseLook := ConfigFile.GetValue(
    'mouse/use_mouse_look', DefaultUseMouseLook);
finalization
  ConfigFile.SetDeleteFloat('mouse/horizontal_sensitivity',
    MouseLookHorizontalSensitivity, DefaultMouseLookHorizontalSensitivity);
  ConfigFile.SetDeleteFloat('mouse/vertical_sensitivity',
    MouseLookVerticalSensitivity, DefaultMouseLookVerticalSensitivity);
  ConfigFile.SetDeleteValue('mouse/use_mouse_look',
    UseMouseLook, DefaultUseMouseLook);
end.