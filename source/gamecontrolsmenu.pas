{
  Copyright 2006-2013 Michalis Kamburelis.

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

uses Classes, CastleWindow, GameGeneralMenu, CastleCameras,
  CastleFonts, CastleUIControls, CastlePlayer;

type
  TSubMenu = class(TCastleGameMenu)
  public
    SubMenuTitle: string;
    { Note that you can freely change this at runtime. No need to call
      things like FixItemsRectangles or something like that when you change
      the value of this property. }
    SubMenuAdditionalInfo: string;
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;

    { Sets Position and PositionRelative* parameters.
      If Center = @false then sets position suitable for the background
      under start menu. Otherwise menu is shifted to be roughly in the center
      of the screen.
      If DoFixItemsRectangles, then FixItemsRectangles will be called afterwards. }
    procedure SetPosition(const Center, DoFixItemsRectangles: boolean);
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

var
  { Font used for menu SubMenuTitle.
    Initialized / finalized here. }
  SubMenuTitleFont: TCastleFont;

{ Update MouseLook-related player settings, based on what is chosen
  in "Confgure controls" menu. }
procedure PlayerUpdateMouseLook(Player: TPlayer);

implementation

uses SysUtils, CastleWindowModes, CastleGLUtils, CastleMessages,
  CastleOnScreenMenu, CastleConfig,
  CastleInputs, CastleKeysMouse, CastleVectors, CastleUtils, CastleRectangles,
  CastleStringUtils, CastleGameNotifications, GameWindow, CastleColors,
  CastleControls;

const
  DefaultMouseLook = true;
  DefaultInvertVerticalMouseLook = false;

var
  { Game player camera settings.
    Automatically saved/loaded from user preferences using CastleConfig.
    @groupBegin }
  MouseLook: boolean = DefaultMouseLook;
  InvertVerticalMouseLook: boolean = DefaultInvertVerticalMouseLook;
  MouseLookHorizontalSensitivity: Single;
  MouseLookVerticalSensitivity: Single;
  { @groupEnd }

procedure PlayerUpdateMouseLook(Player: TPlayer);
begin
  Player.Camera.MouseLookHorizontalSensitivity := MouseLookHorizontalSensitivity;
  Player.Camera.MouseLookVerticalSensitivity := MouseLookVerticalSensitivity;
  Player.Camera.InvertVerticalMouseLook := InvertVerticalMouseLook;
  { MouseLook is allowed always, even when player is dead.
    Just like rotation keys.
    Note that when Blocked, rotating will actually
    be disabled by Input := []. But still mouse look will cause mouse
    to remain hidden, which is good (why pop the mouse cursor on game
    win animation?). }
  Player.Camera.MouseLook := MouseLook;
end;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TControlsMenu = class(TSubMenu)
  public
    MouseLookHorizontalSensitivitySlider: TMenuFloatSlider;
    MouseLookVerticalSensitivitySlider: TMenuFloatSlider;
    AutoOpenInventoryArgument: TMenuBooleanArgument;
    MouseLookArgument: TMenuBooleanArgument;
    InvertVerticalMouseLookArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TControlsSubMenu = class(TSubMenu)
  private
    FGroup: TInputGroup;
    { Refresh shortcuts descriptions displayed in the menu
      from current values of CastleInput_Xxx variables. }
    procedure RefreshShortcuts;
  public
    constructor CreateControlsSubMenu(AOwner: TComponent; AGroup: TInputGroup);

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
  SetPosition(false, false);
  DrawBackgroundRectangle := false;
end;

procedure TSubMenu.SetPosition(const Center, DoFixItemsRectangles: boolean);
begin
  if Center then
  begin
    Position := Vector2Integer(0, 0);
    PositionRelativeScreenX := prMiddle;
    PositionRelativeScreenY := prMiddle;
    PositionRelativeMenuX := prMiddle;
    PositionRelativeMenuY := prMiddle;
  end else
  begin
    Position := Vector2Integer(20, -120);
    PositionRelativeScreenX := prLowerBorder;
    PositionRelativeScreenY := prHigherBorder;
    PositionRelativeMenuX := prLowerBorder;
    PositionRelativeMenuY := prHigherBorder;
  end;

  if DoFixItemsRectangles then FixItemsRectangles;
end;

procedure TSubMenu.Render;
const
  SubMenuTextColor: TCastleColor = (0.9, 0.9, 0.9, 1.0);
begin
  inherited;

  SubMenuTitleFont.Print(Position[0], Position[1] - 20,
    SubMenuTextColor, SubMenuTitle + ' :');

  if SubMenuAdditionalInfo <> '' then
    SubMenuTitleFont.PrintBrokenString(
      AllItemsRectangle.Left,
      AllItemsRectangle.Bottom - SubMenuTitleFont.RowHeight, SubMenuTextColor,
      SubMenuAdditionalInfo,
      Window.Width - 2 * Round(AllItemsRectangle.Left), true, 0);
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
  MouseLookArgument := TMenuBooleanArgument.Create(MouseLook);
  InvertVerticalMouseLookArgument :=
    TMenuBooleanArgument.Create(InvertVerticalMouseLook);

  Items.Add('Configure basic controls');
  Items.Add('Configure items controls');
  Items.Add('Configure other controls');
  Items.AddObject('Use mouse look', MouseLookArgument);
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
    0: begin BasicControlsMenu.RefreshShortcuts; SetCurrentMenu(CurrentMenu, BasicControlsMenu); end;
    1: begin ItemsControlsMenu.RefreshShortcuts; SetCurrentMenu(CurrentMenu, ItemsControlsMenu); end;
    2: begin OtherControlsMenu.RefreshShortcuts; SetCurrentMenu(CurrentMenu, OtherControlsMenu); end;
    3: begin
         MouseLook := not MouseLook;
         MouseLookArgument.Value := MouseLook;
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
         InputsAll.RestoreDefaults;

         MouseLook := DefaultMouseLook;
         MouseLookArgument.Value := MouseLook;

         InvertVerticalMouseLook := DefaultInvertVerticalMouseLook;
         InvertVerticalMouseLookArgument.Value := InvertVerticalMouseLook;

         MouseLookHorizontalSensitivity := TWalkCamera.DefaultMouseLookHorizontalSensitivity;
         MouseLookVerticalSensitivity   := TWalkCamera.DefaultMouseLookVerticalSensitivity  ;
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

  for I := 0 to InputsGroup[Group].Count - 1 do
    Items.AddObject(
      InputsGroup[Group].Items[I].Caption,
      InputArgument(InputsGroup[Group].Items[I].
        Description(SNoneInput)));

  Items.Add('Back to controls menu');

  RegularSpaceBetweenItems := 2;
end;

procedure TControlsSubMenu.Click;

  procedure ChangeKey(InputShortcut: TInputShortcut);
  var
    ConflictingInput: TInputShortcut;
    NewEvent: TInputPressRelease;
  begin
    MessageKeyMouse(Window, Format(
      'Press the new key or mouse button or mouse wheel for "%s".' + NL + NL +
      '[Escape] cancels.' + NL +
      '[Backspace] clears the shortcut.',
      [InputShortcut.Caption]), NewEvent);

    if NewEvent.IsKey(K_Backspace) then
    begin
      InputShortcut.MakeClear;
    end else
    if NewEvent.IsKey(K_Escape) then
    begin
      { Don't do anything. }
    end else
    { We silently ignore situation when NewEvent already
      matches InputShortcut. This is meaningless,
      and otherwise could unnecessarily swap Key1 and Key2 in InputShortcut. }
    if not InputShortcut.IsEvent(NewEvent) then
    begin
      ConflictingInput := InputsAll.SeekMatchingShortcut(NewEvent);

      if ConflictingInput <> nil then
      begin
        { I used to have here a confirmation before clearing ConflictingInput.
          But this was bad for user experience, as the message would have
          to be either about "clearing the whole shortcut" or just
          "clearing part of the shortcut" --- as each shortcut is
          2 key shortcuts and 1 mouse shortcut.
          Also, one of the rules is to avoid modal dialog boxes...
          So now I just uncoditionally remove conflicting key,
          and make a Notification informing user about it. }
        case NewEvent.EventType of
          itMouseButton:
            begin
              Notifications.Show(Format('Note: "%s" mouse shortcut cleared for action "%s"',
                [ MouseButtonStr[ConflictingInput.MouseButton],
                  ConflictingInput.Caption ]));
              ConflictingInput.MouseButtonUse := false;
            end;
          itMouseWheel:
            begin
              Notifications.Show(Format('Note: "%s" mouse wheel cleared for action "%s"',
                [ MouseWheelDirectionStr[ConflictingInput.MouseWheel],
                  ConflictingInput.Caption ]));
              ConflictingInput.MouseWheel := mwNone;
            end;
          itKey:
            if ConflictingInput.Key1 = NewEvent.Key then
            begin
              Notifications.Show(Format('Note: "%s" key shortcut cleared for action "%s"',
                [ KeyToStr(ConflictingInput.Key1),
                  ConflictingInput.Caption ]));
              ConflictingInput.Key1 := K_None;
            end else
            begin
              Assert(ConflictingInput.Key2 = NewEvent.Key);

              Notifications.Show(Format('Note: "%s" key shortcut cleared for action "%s"',
                [ KeyToStr(ConflictingInput.Key2),
                  ConflictingInput.Caption ]));
              ConflictingInput.Key2 := K_None;
            end;
          else raise EInternalError.Create('ConflictingInput: NewEvent.EventType?');
        end;
      end;

      InputShortcut.Add(NewEvent);
    end;

    RefreshShortcuts;
  end;

begin
  inherited;

  if Between(CurrentItem, 0, InputsGroup[Group].Count - 1) then
    ChangeKey(InputsGroup[Group].Items[CurrentItem]) else
  if CurrentItem = InputsGroup[Group].Count then
  begin
    SetCurrentMenu(CurrentMenu, ControlsMenu);
  end else
    raise EInternalError.Create('Menu item unknown');
end;

procedure TControlsSubMenu.RefreshShortcuts;
var
  I: Integer;
begin
  for I := 0 to InputsGroup[Group].Count - 1 do
    TMenuArgument(Items.Objects[I]).Value :=
      InputsGroup[Group].Items[I].Description(SNoneInput);
end;

{ TBasicControlsMenu ------------------------------------------------------------- }

constructor TBasicControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, igBasic);

  SubMenuTitle := 'Configure basic controls';
end;

{ TItemsControlsMenu --------------------------------------------------------- }

constructor TItemsControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, igItems);

  SubMenuTitle := 'Configure items controls';

  SubMenuAdditionalInfo :=
    'Notes:' +nl+
    '- You pick items lying on the ground just by walking on them.' +nl+
    '- Items are automatically unequipped when you drop them.';
end;

{ TOtherControlsMenu ------------------------------------------------------------- }

constructor TOtherControlsMenu.Create(AOwner: TComponent);
begin
  inherited CreateControlsSubMenu(AOwner, igOther);

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

var
  ExitWithEscapeAllowed: boolean;
  ExitWithEscape: boolean;

type
  TFadeRect = class(TUIControl)
    function RenderStyle: TRenderStyle; override;
    procedure Render; override;
  end;

function TFadeRect.RenderStyle: TRenderStyle;
begin
  Result := rs2D;
end;

procedure TFadeRect.Render;
const
  Width = 780;
  Height = 390;
begin
  DrawRectangle(Rectangle(
    (ContainerRect.Width - Width) div 2,
    (ContainerRect.Height - Height) div 2, Width, Height),
    Vector4Single(0, 0, 0, 0.4));
end;

procedure Press(Window: TCastleWindowBase; const Event: TInputPressRelease);
begin
  if ExitWithEscapeAllowed and Event.IsKey(CharEscape) then
  begin
    UserQuit := true;
    ExitWithEscape := true;
  end;
end;

procedure CloseQuery(Sender: TCastleWindowBase);
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

  ControlsMenu     .SetPosition(ADrawCentered, true);
  BasicControlsMenu.SetPosition(ADrawCentered, true);
  ItemsControlsMenu.SetPosition(ADrawCentered, true);
  OtherControlsMenu.SetPosition(ADrawCentered, true);

  FadeRect := nil;

  SavedMode := TGLMode.CreateReset(Window, nil, Window.OnResize, @CloseQuery);
  try
    Window.OnPress := @Press;

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
      Application.ProcessMessage(true, true);
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

procedure ContextOpen;
begin
  ControlsMenu := TControlsMenu.Create(Application);
  BasicControlsMenu := TBasicControlsMenu.Create(Application);
  ItemsControlsMenu := TItemsControlsMenu.Create(Application);
  OtherControlsMenu := TOtherControlsMenu.Create(Application);
  SubMenuTitleFont := Theme.GLMessageFont;
end;

{ TConfigOptions ------------------------------------------------------------- }

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  MouseLookHorizontalSensitivity := Config.GetFloat(
    'mouse/horizontal_sensitivity', TWalkCamera.DefaultMouseLookHorizontalSensitivity);
  MouseLookVerticalSensitivity := Config.GetFloat(
    'mouse/vertical_sensitivity', TWalkCamera.DefaultMouseLookVerticalSensitivity);
  MouseLook := Config.GetValue(
    'mouse/use_mouse_look', DefaultMouseLook);
  InvertVerticalMouseLook := Config.GetValue(
    'mouse/invert_vertical_mouse_look', DefaultInvertVerticalMouseLook);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteFloat('mouse/horizontal_sensitivity',
    MouseLookHorizontalSensitivity, TWalkCamera.DefaultMouseLookHorizontalSensitivity);
  Config.SetDeleteFloat('mouse/vertical_sensitivity',
    MouseLookVerticalSensitivity, TWalkCamera.DefaultMouseLookVerticalSensitivity);
  Config.SetDeleteValue('mouse/use_mouse_look',
    MouseLook, DefaultMouseLook);
  Config.SetDeleteValue('mouse/invert_vertical_mouse_look',
    InvertVerticalMouseLook, DefaultInvertVerticalMouseLook);
end;

initialization
  OnGLContextOpen.Add(@ContextOpen);

  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
