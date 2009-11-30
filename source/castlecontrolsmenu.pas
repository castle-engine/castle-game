{
  Copyright 2006,2007 Michalis Kamburelis.

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

uses GLWindow, GL, GLU, GLExt, CastleGeneralMenu, Navigation,
  OpenGLFonts, OpenGLBmpFonts;

type
  TSubMenu = class(TCastleMenu)
    SubMenuTitle: string;
    { Note that you can freely change this at runtime. No need to call
      things like FixItemsAreas or something like that when you change
      the value of this property. }
    SubMenuAdditionalInfo: string;
    constructor Create;
    procedure Draw(const Focused: boolean); override;

    { Sets Position and PositionRelative* parameters.
      Sets position suitable for the StartScreen, and then shifts
      it by MoveX / MoveY. By default TSubMenu is positioned
      like (MoveX, MoveY) = (0, 0).

      If DoFixItemsAreas, also FixItemsAreas will be called afterwards. }
    procedure SetPosition(const MoveX, MoveY: Single; const DoFixItemsAreas: boolean);
  end;

{ Show menu that allows player to configure controls.
  AIdleUnderMenu may be @nil. }
procedure ShowControlsMenu(ADrawUnderMenu: TDrawFunc;
  AIdleUnderMenu: TGLWindowFunc;
  ADrawFadeRect, ADrawCentered: boolean);

{ This is like ShowControlsMenu, but user can quit with
  escape key. AExitWithEscape will be set to @true or @false,
  depending on whether user used escape to exit. }
procedure ShowControlsMenuEscape(ADrawUnderMenu: TDrawFunc;
  AIdleUnderMenu: TGLWindowFunc;
  ADrawFadeRect, ADrawCentered: boolean;
  out AExitWithEscape: boolean);

const
  DefaultUseMouseLook = true;
  DefaultInvertVerticalMouseLook = false;

var
  UseMouseLook: boolean;

  { Global mouse look sensitivity. Are controlled by ShowControlsMenu,
    and are saved/loaded to/from config file in this unit. }
  MouseLookHorizontalSensitivity: Single;
  MouseLookVerticalSensitivity: Single;

  InvertVerticalMouseLook: boolean;

  { Font used for menu SubMenuTitle.
    Initialized / finalized in GLWindow Init/Close here. }
  SubMenuTitleFont: TGLBitmapFont_Abstract;

implementation

uses SysUtils, GLWinModes, KambiGLUtils, GLWinMessages, CastleWindow,
  GLMenu, BFNT_BitstreamVeraSansMono_m18_Unit,
  CastleInputs, Keys, VectorMath, KambiUtils, CastlePlay,
  CastleConfig, KambiStringUtils, CastleTimeMessages;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TControlsMenu = class(TSubMenu)
    MouseLookHorizontalSensitivitySlider: TGLMenuFloatSlider;
    MouseLookVerticalSensitivitySlider: TGLMenuFloatSlider;
    AutoOpenInventoryArgument: TGLMenuBooleanArgument;
    UseMouseLookArgument: TGLMenuBooleanArgument;
    InvertVerticalMouseLookArgument: TGLMenuBooleanArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TControlsSubMenu = class(TSubMenu)
  private
    FGroup: TInputGroup;
    procedure InputChanged(InputConfiguration: TInputConfiguration);
  public
    constructor Create(AGroup: TInputGroup);
    destructor Destroy; override;

    property Group: TInputGroup read FGroup;
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

{ TSubMenu ------------------------------------------------------------- }

constructor TSubMenu.Create;
begin
  inherited Create;
  SetPosition(0, 0, false);
  DrawBackgroundRectangle := false;
end;

procedure TSubMenu.SetPosition(const MoveX, MoveY: Single; const DoFixItemsAreas: boolean);
begin
  Position.Init(20 + MoveX, 440 + MoveY);
  PositionRelativeScreenX := prLowerBorder;
  PositionRelativeScreenY := prLowerBorder;
  PositionRelativeMenuX := prLowerBorder;
  PositionRelativeMenuY := prHigherBorder;

  if DoFixItemsAreas then FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TSubMenu.Draw(const Focused: boolean);
const
  SubMenuTextColor: TVector3Single = (0.9, 0.9, 0.9);
begin
  inherited;

  glColorv(SubMenuTextColor);

  glPushMatrix;
    glTranslatef(Position.Data[0], Position.Data[1] - 20, 0);
    glRasterPos2i(0, 0);
    SubMenuTitleFont.Print(SubMenuTitle + ' :');
  glPopMatrix;

  if SubMenuAdditionalInfo <> '' then
  begin
    glPushMatrix;
      glTranslatef(AllItemsArea.X0,
        AllItemsArea.Y0 - SubMenuTitleFont.RowHeight, 0);
      SubMenuTitleFont.PrintBrokenString(SubMenuAdditionalInfo,
        Glw.Width - 2 * Round(AllItemsArea.X0), 0, 0, true, 0);
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
  InvertVerticalMouseLookArgument :=
    TGLMenuBooleanArgument.Create(InvertVerticalMouseLook);

  Items.Add('Configure basic controls');
  Items.Add('Configure items controls');
  Items.Add('Configure other controls');
  Items.AddObject('Use mouse look', UseMouseLookArgument);
  Items.AddObject('Mouse look horizontal sensitivity',
    MouseLookHorizontalSensitivitySlider);
  Items.AddObject('Mouse look vertical sensitivity',
    MouseLookVerticalSensitivitySlider);
  Items.AddObject('Invert vertical mouse look', InvertVerticalMouseLookArgument);
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
    0: SetCurrentMenu(CurrentMenu, BasicControlsMenu);
    1: SetCurrentMenu(CurrentMenu, ItemsControlsMenu);
    2: SetCurrentMenu(CurrentMenu, OtherControlsMenu);
    3: begin
         UseMouseLook := not UseMouseLook;
         UseMouseLookArgument.Value := UseMouseLook;
       end;
    4: ;
    5: ;
    6: begin
         InvertVerticalMouseLook := not InvertVerticalMouseLook;
         InvertVerticalMouseLookArgument.Value := InvertVerticalMouseLook;
       end;
    7: begin
         AutoOpenInventory := not AutoOpenInventory;
         AutoOpenInventoryArgument.Value := AutoOpenInventory;
       end;
    8: begin
         CastleAllInputs.RestoreDefaults;

         UseMouseLook := DefaultUseMouseLook;
         UseMouseLookArgument.Value := UseMouseLook;

         InvertVerticalMouseLook := DefaultInvertVerticalMouseLook;
         InvertVerticalMouseLookArgument.Value := InvertVerticalMouseLook;

         MouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
         MouseLookVerticalSensitivity   := DefaultMouseLookVerticalSensitivity  ;
         MouseLookHorizontalSensitivitySlider.Value := MouseLookHorizontalSensitivity;
         MouseLookVerticalSensitivitySlider  .Value := MouseLookVerticalSensitivity  ;

         AutoOpenInventory := DefaultAutoOpenInventory;
         AutoOpenInventoryArgument.Value := AutoOpenInventory;

         MessageOK(Glw, 'All keys and settings restored to defaults.');
       end;
    9: UserQuit := true;
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

const
  SNoneInput = '<none>';

constructor TControlsSubMenu.Create(AGroup: TInputGroup);

  function InputArgument(const S: string): TGLMenuItemArgument;
  begin
    Result := TGLMenuItemArgument.Create(
      400
      { This used to be
        TGLMenuItemArgument.TextWidth(
          'key "Page Down" or "Page Up" or mouse "medium"')
        But this is too long... Unfortunately, it seems that some
        key configurations just will not fit on screen. });
    Result.Value := S;
  end;

var
  I: Integer;
begin
  inherited Create;
  FGroup := AGroup;

  for I := 0 to CastleGroupInputs[Group].High do
    Items.AddObject(
      CastleGroupInputs[Group].Items[I].Name,
      InputArgument(CastleGroupInputs[Group].Items[I].Shortcut.
        Description(SNoneInput)));

  Items.Add('Back to controls menu');

  RegularSpaceBetweenItems := 2;

  FixItemsAreas(Glw.Width, Glw.Height);

  OnInputChanged.AppendItem(@InputChanged);
end;

destructor TControlsSubMenu.Destroy;
begin
  if OnInputChanged <> nil then
    OnInputChanged.DeleteFirstEqual(@InputChanged);
  inherited;
end;

procedure TControlsSubMenu.CurrentItemSelected;

  procedure ChangeKey(InputConfiguration: TInputConfiguration);
  var
    ConflictingKey: TInputConfiguration;
    NewMouseEvent: boolean;
    NewKey: TKey;
    NewMouseButton: TMouseButton;
  begin
    MessageKeyMouse(Glw, Format(
      'Press the new key or mouse button for "%s".', [InputConfiguration.Name]),
      'Cancel [Escape]' + nl + 'Clear [Backspace]', taLeft,
      NewMouseEvent, NewKey, NewMouseButton);

    if (not NewMouseEvent) and (NewKey = K_Backspace) then
    begin
      InputConfiguration.Shortcut.MakeClear;
    end else
    if (not NewMouseEvent) and (NewKey = K_Escape) then
    begin
      { Don't do anything. }
    end else
    if { We silently ignore situation when NewKey/NewMouse already
         match InputConfiguration.Shortcut. This is meaningless,
         and otherwise could unnecessarily swap Key1 and Key2 in AddShortcut. }
       (not InputConfiguration.Shortcut.IsEvent(
         NewMouseEvent, NewKey, #0, NewMouseButton)) then
    begin
      ConflictingKey := CastleAllInputs.SeekMatchingShortcut(NewMouseEvent,
        NewKey, NewMouseButton);

      if ConflictingKey <> nil then
      begin
        { I used to have here a confirmation before clearing ConflictingKey.
          But this was bad for user experience, as the message would have
          to be either about "clearing the whole shortcut" or just
          "clearing part of the shortcut" --- as each shortcut is
          2 key shortcuts and 1 mouse shortcut.
          Also, one of the rules is to avoid modal dialog boxes...
          So now I just uncoditionally remove conflicting key,
          and make a TimeMessage informing user about it. }
        if NewMouseEvent then
        begin
          TimeMessage(Format('Note: "%s" mouse shortcut cleared for action "%s"',
            [ MouseButtonStr[ConflictingKey.Shortcut.MouseButton],
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.MouseButtonUse := false;
        end else
        if ConflictingKey.Shortcut.Key1 = NewKey then
        begin
          TimeMessage(Format('Note: "%s" key shortcut cleared for action "%s"',
            [ KeyToStr(ConflictingKey.Shortcut.Key1),
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.Key1 := K_None;
        end else
        begin
          Assert(ConflictingKey.Shortcut.Key2 = NewKey);

          TimeMessage(Format('Note: "%s" key shortcut cleared for action "%s"',
            [ KeyToStr(ConflictingKey.Shortcut.Key2),
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.Key2 := K_None;
        end;
      end;

      InputConfiguration.AddShortcut(NewMouseEvent, NewKey, NewMouseButton);
    end;
  end;

begin
  inherited;

  if Between(CurrentItem, 0, CastleGroupInputs[Group].High) then
    ChangeKey(CastleGroupInputs[Group].Items[CurrentItem]) else
  if CurrentItem = CastleGroupInputs[Group].High + 1 then
  begin
    SetCurrentMenu(CurrentMenu, ControlsMenu);
  end else
    raise EInternalError.Create('Menu item unknown');
end;

procedure TControlsSubMenu.InputChanged(InputConfiguration: TInputConfiguration);
var
  I: Integer;
begin
  { Refresh key names displayed in the menu. }

  for I := 0 to CastleGroupInputs[Group].High do
    TGLMenuItemArgument(Items.Objects[I]).Value :=
      CastleGroupInputs[Group].Items[I].Shortcut.Description(SNoneInput);
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
    '  This key is not configurable.';

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
  IdleUnderMenu: TGLWindowFunc;
  DrawFadeRect: boolean;
  GLList_DrawFadeRect: TGLuint;
  ExitWithEscapeAllowed: boolean;
  ExitWithEscape: boolean;
  MoveX, MoveY: Single;

procedure Draw2d(Draw2DData: Pointer);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);

  if DrawFadeRect then
  begin
    glPushMatrix;
      glTranslatef(MoveX, MoveY, 0);
      glCallList(GLList_DrawFadeRect);
    glPopMatrix;
  end;

  { TGLMenu must be drawn with translation (0, 0), otherwise mouse positions
    in TGLMenu input events do not correspond to displayed menu positions.
    We will position menu appropriately (doing equivalent of MoveX/Y shift
    if needed) by initializing positions of all *ControlsMenu
    in ShowControlsMenuCore. }

  CurrentMenu.Draw(false);
end;

procedure Draw(Glwin: TGLWindow);
begin
  DrawUnderMenu(Glwin);

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@Draw2d, nil,
      0, Glwin.Width, 0, Glwin.Height);
  glPopAttrib;
end;

procedure EventDown(MouseEvent: boolean; Key: TKey;
  AMouseButton: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  EventDown(false, Key, mbLeft);

  if ExitWithEscapeAllowed then
    case C of
      CharEscape:
        begin
          UserQuit := true;
          ExitWithEscape := true;
        end;
    end;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(true, K_None, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  if Assigned(IdleUnderMenu) then IdleUnderMenu(Glwin);
  CurrentMenu.Idle(Glwin.Fps.IdleSpeed, nil, nil, []);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

procedure ShowControlsMenuCore(ADrawUnderMenu: TDrawFunc;
  AIdleUnderMenu: TGLWindowFunc;
  ADrawFadeRect, ADrawCentered, AExitWithEscapeAllowed: boolean;
  out AExitWithEscape: boolean);
var
  SavedMode: TGLMode;
  SavedMenu: TCastleMenu;
begin
  DrawUnderMenu := ADrawUnderMenu;
  IdleUnderMenu := AIdleUnderMenu;
  DrawFadeRect := ADrawFadeRect;
  ExitWithEscapeAllowed := AExitWithEscapeAllowed;
  ExitWithEscape := false;

  if ADrawCentered then
  begin
    MoveX := - WholeAreaX0 + (Glw.Width - (WholeAreaX1 - WholeAreaX0)) / 2;
    MoveY := - WholeAreaY0 + (Glw.Height - (WholeAreaY1 - WholeAreaY0)) / 2;
  end else
  begin
    MoveX := 0;
    MoveY := 0;
  end;

  ControlsMenu     .SetPosition(MoveX, MoveY, true);
  BasicControlsMenu.SetPosition(MoveX, MoveY, true);
  ItemsControlsMenu.SetPosition(MoveX, MoveY, true);
  OtherControlsMenu.SetPosition(MoveX, MoveY, true);

  SavedMode := TGLMode.Create(glw, 0, false);
  try
    { This shouldn't change projection matrix anyway. }
    SavedMode.RestoreProjectionMatrix := false;

    TGLWindowState.SetStandardState(Glw, @Draw, @CloseQuery, Glw.OnResize,
      nil, false, true { FPSActive should not be needed anymore, but I leave it. },
      false, K_None, #0, false, false);

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnIdle := @Idle;

    Glw.UseControls := true;
    SavedMenu := SetCurrentMenu(CurrentMenu, ControlsMenu);

    UserQuit := false;
    repeat
      Glwm.ProcessMessage(true);
    until UserQuit;

    Glw.Controls.MakeSingle(TCastleMenu, SavedMenu);
  finally FreeAndNil(SavedMode); end;

  AExitWithEscape := ExitWithEscape;
end;

procedure ShowControlsMenu(ADrawUnderMenu: TDrawFunc;
  AIdleUnderMenu: TGLWindowFunc;
  ADrawFadeRect, ADrawCentered: boolean);
var
  Dummy: boolean;
begin
  ShowControlsMenuCore(ADrawUnderMenu, AIdleUnderMenu,
    ADrawFadeRect, ADrawCentered,
    false, Dummy);
end;

procedure ShowControlsMenuEscape(ADrawUnderMenu: TDrawFunc;
  AIdleUnderMenu: TGLWindowFunc;
  ADrawFadeRect, ADrawCentered: boolean;
  out AExitWithEscape: boolean);
begin
  ShowControlsMenuCore(ADrawUnderMenu, AIdleUnderMenu,
    ADrawFadeRect, ADrawCentered,
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
  SubMenuTitleFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m18);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
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
  InvertVerticalMouseLook := ConfigFile.GetValue(
    'mouse/invert_vertical_mouse_look', DefaultInvertVerticalMouseLook);
finalization
  ConfigFile.SetDeleteFloat('mouse/horizontal_sensitivity',
    MouseLookHorizontalSensitivity, DefaultMouseLookHorizontalSensitivity);
  ConfigFile.SetDeleteFloat('mouse/vertical_sensitivity',
    MouseLookVerticalSensitivity, DefaultMouseLookVerticalSensitivity);
  ConfigFile.SetDeleteValue('mouse/use_mouse_look',
    UseMouseLook, DefaultUseMouseLook);
  ConfigFile.SetDeleteValue('mouse/invert_vertical_mouse_look',
    InvertVerticalMouseLook, DefaultInvertVerticalMouseLook);
end.