{
  Copyright 2006-2011 Michalis Kamburelis.

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
unit GameControlsMenu;

interface

uses Classes, CastleWindow, GL, GLU, GameGeneralMenu, Cameras,
  OpenGLFonts, OpenGLBmpFonts, UIControls;

type
  TSubMenu = class(TCastleGameMenu)
  public
    SubMenuTitle: string;
    { Note that you can freely change this at runtime. No need to call
      things like FixItemsRectangles or something like that when you change
      the value of this property. }
    SubMenuAdditionalInfo: string;
    constructor Create(AOwner: TComponent); override;
    procedure Draw; override;

    { Sets Position and PositionRelative* parameters.
      Sets position suitable for the StartScreen, and then shifts
      it by MoveX / MoveY. By default TSubMenu is positioned
      like (MoveX, MoveY) = (0, 0).

      If DoFixItemsRectangles, also FixItemsRectangles will be called afterwards. }
    procedure SetPosition(const MoveX, MoveY: Integer; const DoFixItemsRectangles: boolean);
  end;

{ Show menu that allows player to configure controls. }
procedure ShowControlsMenu(ControlsUnder: TUIControlList;
  ADrawFadeRect, ADrawCentered: boolean);

{ Like ShowControlsMenu, but user can quit with
  the escape key. AExitWithEscape will be set to @true or @false,
  depending on whether user used escape to exit. }
procedure ShowControlsMenuEscape(ControlsUnder: TUIControlList;
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
    Initialized / finalized in CastleWindow Open/Close here. }
  SubMenuTitleFont: TGLBitmapFont_Abstract;

implementation

uses SysUtils, WindowModes, CastleGLUtils, CastleMessages, GameWindow,
  OnScreenMenu, BFNT_BitstreamVeraSansMono_m18_Unit,
  GameInputs, KeysMouse, VectorMath, CastleUtils, GamePlay,
  GameConfig, CastleStringUtils, GameNotifications;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TControlsMenu = class(TSubMenu)
  public
    MouseLookHorizontalSensitivitySlider: TMenuFloatSlider;
    MouseLookVerticalSensitivitySlider: TMenuFloatSlider;
    AutoOpenInventoryArgument: TMenuBooleanArgument;
    UseMouseLookArgument: TMenuBooleanArgument;
    InvertVerticalMouseLookArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TControlsSubMenu = class(TSubMenu)
  private
    FGroup: TInputGroup;
    procedure InputChanged(InputConfiguration: TInputConfiguration);
  public
    constructor CreateControlsSubMenu(AOwner: TComponent; AGroup: TInputGroup);
    destructor Destroy; override;

    property Group: TInputGroup read FGroup;
    procedure Click; override;
  end;

  TBasicControlsMenu = class(TControlsSubMenu)
    constructor Create(AOwner: TComponent); override;
  end;

  TItemsControlsMenu = class(TControlsSubMenu)
    constructor Create(AOwner: TComponent); override;
  end;

  TOtherControlsMenu = class(TControlsSubMenu)
    constructor Create(AOwner: TComponent); override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  UserQuit: boolean;
  CurrentMenu: TCastleGameMenu;
  ControlsMenu: TControlsMenu;
  BasicControlsMenu: TBasicControlsMenu;
  ItemsControlsMenu: TItemsControlsMenu;
  OtherControlsMenu: TOtherControlsMenu;

{ TSubMenu ------------------------------------------------------------- }

constructor TSubMenu.Create(AOwner: TComponent);
begin
  inherited;
  SetPosition(0, 0, false);
  DrawBackgroundRectangle := false;
end;

procedure TSubMenu.SetPosition(const MoveX, MoveY: Integer; const DoFixItemsRectangles: boolean);
begin
  Position := Vector2Integer(20 + MoveX, 440 + MoveY);
  PositionRelativeScreenX := prLowerBorder;
  PositionRelativeScreenY := prLowerBorder;
  PositionRelativeMenuX := prLowerBorder;
  PositionRelativeMenuY := prHigherBorder;

  if DoFixItemsRectangles then FixItemsRectangles;
end;

procedure TSubMenu.Draw;
const
  SubMenuTextColor: TVector3Single = (0.9, 0.9, 0.9);
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
      glTranslatef(AllItemsRectangle.X0,
        AllItemsRectangle.Y0 - SubMenuTitleFont.RowHeight, 0);
      SubMenuTitleFont.PrintBrokenString(SubMenuAdditionalInfo,
        Window.Width - 2 * Round(AllItemsRectangle.X0), 0, 0, true, 0);
    glPopMatrix;
  end;
end;

{ TControlsMenu ------------------------------------------------------------- }

constructor TControlsMenu.Create(AOwner: TComponent);
begin
  inherited;

  MouseLookHorizontalSensitivitySlider := TMenuFloatSlider.Create(
    0.01, 0.3, MouseLookHorizontalSensitivity);
  MouseLookVerticalSensitivitySlider := TMenuFloatSlider.Create(
    0.01, 0.3, MouseLookVerticalSensitivity);
  AutoOpenInventoryArgument := TMenuBooleanArgument.Create(AutoOpenInventory);
  UseMouseLookArgument := TMenuBooleanArgument.Create(UseMouseLook);
  InvertVerticalMouseLookArgument :=
    TMenuBooleanArgument.Create(InvertVerticalMouseLook);

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
end;

procedure TControlsMenu.Click;
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

         MessageOK(Window, 'All keys and settings restored to defaults.');
       end;
    9: UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TControlsMenu.AccessoryValueChanged;
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

constructor TControlsSubMenu.CreateControlsSubMenu(AOwner: TComponent;
  AGroup: TInputGroup);

  function InputArgument(const S: string): TMenuArgument;
  begin
    Result := TMenuArgument.Create(
      400
      { This used to be
        TMenuArgument.TextWidth(
          'key "Page Down" or "Page Up" or mouse "medium"')
        But this is too long... Unfortunately, it seems that some
        key configurations just will not fit on screen. });
    Result.Value := S;
  end;

var
  I: Integer;
begin
  inherited Create(AOwner);

  FGroup := AGroup;

  for I := 0 to CastleGroupInputs[Group].Count - 1 do
    Items.AddObject(
      CastleGroupInputs[Group].Items[I].Name,
      InputArgument(CastleGroupInputs[Group].Items[I].Shortcut.
        Description(SNoneInput)));

  Items.Add('Back to controls menu');

  RegularSpaceBetweenItems := 2;

  OnInputChanged.Add(@InputChanged);
end;

destructor TControlsSubMenu.Destroy;
begin
  if OnInputChanged <> nil then
    OnInputChanged.Remove(@InputChanged);
  inherited;
end;

procedure TControlsSubMenu.Click;

  procedure ChangeKey(InputConfiguration: TInputConfiguration);
  var
    ConflictingKey: TInputConfiguration;
    NewKey: TKey;
    NewMousePress: boolean;
    NewMouseButton: TMouseButton;
    NewMouseWheel: TMouseWheelDirection;
  begin
    MessageKeyMouse(Window, Format(
      'Press the new key or mouse button or mouse wheel for "%s".', [InputConfiguration.Name]),
      'Cancel [Escape]' + nl + 'Clear [Backspace]', taLeft,
      NewKey, NewMousePress, NewMouseButton, NewMouseWheel);

    if NewKey = K_Backspace then
    begin
      InputConfiguration.Shortcut.MakeClear;
    end else
    if NewKey = K_Escape then
    begin
      { Don't do anything. }
    end else
    if { We silently ignore situation when NewKey/NewMouse already
         match InputConfiguration.Shortcut. This is meaningless,
         and otherwise could unnecessarily swap Key1 and Key2 in AddShortcut. }
       (not InputConfiguration.Shortcut.IsEvent(
         NewKey, #0, NewMousePress, NewMouseButton, NewMouseWheel)) then
    begin
      ConflictingKey := CastleAllInputs.SeekMatchingShortcut(
        NewKey, NewMousePress, NewMouseButton, NewMouseWheel);

      if ConflictingKey <> nil then
      begin
        { I used to have here a confirmation before clearing ConflictingKey.
          But this was bad for user experience, as the message would have
          to be either about "clearing the whole shortcut" or just
          "clearing part of the shortcut" --- as each shortcut is
          2 key shortcuts and 1 mouse shortcut.
          Also, one of the rules is to avoid modal dialog boxes...
          So now I just uncoditionally remove conflicting key,
          and make a Notification informing user about it. }
        if NewMousePress then
        begin
          Notifications.Show(Format('Note: "%s" mouse shortcut cleared for action "%s"',
            [ MouseButtonStr[ConflictingKey.Shortcut.MouseButton],
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.MouseButtonUse := false;
        end else
        if NewMouseWheel <> mwNone then
        begin
          Notifications.Show(Format('Note: "%s" mouse wheel cleared for action "%s"',
            [ MouseWheelDirectionStr[ConflictingKey.Shortcut.MouseWheel],
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.MouseWheel := mwNone;
        end else
        if ConflictingKey.Shortcut.Key1 = NewKey then
        begin
          Notifications.Show(Format('Note: "%s" key shortcut cleared for action "%s"',
            [ KeyToStr(ConflictingKey.Shortcut.Key1),
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.Key1 := K_None;
        end else
        begin
          Assert(ConflictingKey.Shortcut.Key2 = NewKey);

          Notifications.Show(Format('Note: "%s" key shortcut cleared for action "%s"',
            [ KeyToStr(ConflictingKey.Shortcut.Key2),
              ConflictingKey.Name ]));
          ConflictingKey.Shortcut.Key2 := K_None;
        end;
      end;

      InputConfiguration.AddShortcut(NewKey,
        NewMousePress, NewMouseButton, NewMouseWheel);
    end;
  end;

begin
  inherited;

  if Between(CurrentItem, 0, CastleGroupInputs[Group].Count - 1) then
    ChangeKey(CastleGroupInputs[Group].Items[CurrentItem]) else
  if CurrentItem = CastleGroupInputs[Group].Count then
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

  for I := 0 to CastleGroupInputs[Group].Count - 1 do
    TMenuArgument(Items.Objects[I]).Value :=
      CastleGroupInputs[Group].Items[I].Shortcut.Description(SNoneInput);
end;

{ TBasicControlsMenu ------------------------------------------------------------- }

constructor TBasicControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, kgBasic);

  SubMenuTitle := 'Configure basic controls';
end;

{ TItemsControlsMenu --------------------------------------------------------- }

constructor TItemsControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, kgItems);

  SubMenuTitle := 'Configure items controls';

  SubMenuAdditionalInfo :=
    'Notes:' +nl+
    '- You pick items lying on the ground just by walking on them.' +nl+
    '- Items are automatically unequipped when you drop them.';
end;

{ TOtherControlsMenu ------------------------------------------------------------- }

constructor TOtherControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, kgOther);

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
  WholeRectangleX0 = 10;
  WholeRectangleY0 = 50;
  WholeRectangleX1 = 750;
  WholeRectangleY1 = 450;

var
  GLList_DrawFadeRect: TGLuint;
  ExitWithEscapeAllowed: boolean;
  ExitWithEscape: boolean;
  MoveX, MoveY: Integer;

type
  TFadeRect = class(TUIControl)
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TFadeRect.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TFadeRect.Draw;
begin
  glPushMatrix;
    glTranslatef(MoveX, MoveY, 0);
    glCallList(GLList_DrawFadeRect);
  glPopMatrix;
end;

procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen;
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);

  if ExitWithEscapeAllowed then
    case C of
      CharEscape:
        begin
          UserQuit := true;
          ExitWithEscape := true;
        end;
    end;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
end;

procedure MouseWheel(Window: TCastleWindowBase; const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  MessageOK(Window, 'You can''t exit now.');
end;

procedure ShowControlsMenuCore(ControlsUnder: TUIControlList;
  ADrawFadeRect, ADrawCentered, AExitWithEscapeAllowed: boolean;
  out AExitWithEscape: boolean);
var
  SavedMode: TGLMode;
  FadeRect: TFadeRect;
begin
  ExitWithEscapeAllowed := AExitWithEscapeAllowed;
  ExitWithEscape := false;

  if ADrawCentered then
  begin
    MoveX := - WholeRectangleX0 + (Window.Width - (WholeRectangleX1 - WholeRectangleX0)) div 2;
    MoveY := - WholeRectangleY0 + (Window.Height - (WholeRectangleY1 - WholeRectangleY0)) div 2;
  end else
  begin
    MoveX := 0;
    MoveY := 0;
  end;

  ControlsMenu     .SetPosition(MoveX, MoveY, true);
  BasicControlsMenu.SetPosition(MoveX, MoveY, true);
  ItemsControlsMenu.SetPosition(MoveX, MoveY, true);
  OtherControlsMenu.SetPosition(MoveX, MoveY, true);

  FadeRect := nil;

  SavedMode := TGLMode.CreateReset(Window, 0, false,
    nil, Window.OnResize, @CloseQuery);
  try
    SavedMode.RestoreProjectionMatrix := false;

    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnMouseWheel := @MouseWheel;

    SetCurrentMenu(CurrentMenu, ControlsMenu);

    if ADrawFadeRect then
    begin
      FadeRect := TFadeRect.Create(nil);
      Window.Controls.Add(FadeRect);
    end;

    Window.Controls.Add(Notifications);
    Window.Controls.AddList(ControlsUnder);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true);
    until UserQuit;
  finally
    FreeAndNil(SavedMode);
    FreeAndNil(FadeRect);
  end;

  AExitWithEscape := ExitWithEscape;
end;

procedure ShowControlsMenu(ControlsUnder: TUIControlList;
  ADrawFadeRect, ADrawCentered: boolean);
var
  Dummy: boolean;
begin
  ShowControlsMenuCore(ControlsUnder, ADrawFadeRect, ADrawCentered,
    false, Dummy);
end;

procedure ShowControlsMenuEscape(ControlsUnder: TUIControlList;
  ADrawFadeRect, ADrawCentered: boolean;
  out AExitWithEscape: boolean);
begin
  ShowControlsMenuCore(ControlsUnder, ADrawFadeRect, ADrawCentered,
    true, AExitWithEscape);
end;

{ initialization / finalization ---------------------------------------------- }

procedure OpenWindow(Window: TCastleWindowBase);
begin
  if GLList_DrawFadeRect = 0 then
    GLList_DrawFadeRect := glGenListsCheck(1, 'CastleControlsMenu.OpenGLW');
  glNewList(GLList_DrawFadeRect, GL_COMPILE);
  try
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, 0.4);
      glRectf(WholeRectangleX0, WholeRectangleY0, WholeRectangleX1, WholeRectangleY1);
    glDisable(GL_BLEND);
  finally glEndList end;

  ControlsMenu := TControlsMenu.Create(Application);
  BasicControlsMenu := TBasicControlsMenu.Create(Application);
  ItemsControlsMenu := TItemsControlsMenu.Create(Application);
  OtherControlsMenu := TOtherControlsMenu.Create(Application);
  SubMenuTitleFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m18);
end;

procedure CloseWindow(Window: TCastleWindowBase);
begin
  FreeAndNil(SubMenuTitleFont);
end;

initialization
  Window.OnOpenList.Add(@OpenWindow);
  Window.OnCloseList.Add(@CloseWindow);

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