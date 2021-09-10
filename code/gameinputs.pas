{
  Copyright 2006-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Inputs specific to this game. }
unit GameInputs;

interface

uses CastleInputs;

var
  Input_UseLifePotion: TInputShortcut;

  Input_ViewMessages: TInputShortcut;
  Input_SaveScreen: TInputShortcut;
  Input_FPSShow: TInputShortcut;
  Input_DebugMenu: TInputShortcut;

implementation

uses CastleKeysMouse, CastleViewport, CastlePlayer, CastleLevels;

initialization
  { Change defaults, to make "attack" on mouse click.
    Consequently, interact must be something else --- key "e" is consistent
    with other 3D games. }
  PlayerInput_Attack.Assign(keyCtrl, keyNone, #0, true, buttonLeft);
  Input_Interact.Assign(keyE, keyNone, #0, false, buttonLeft);
  PlayerInput_Jump.Assign(keySpace, keyNone, #0, true, buttonRight); // add rmb for jump
  { These by default don't have any shortcut }
  PlayerInput_InventoryShow.Assign(keyI, keyNone, #0, false, buttonLeft);
  PlayerInput_DropItem.Assign(keyR, keyNone, #0, false, buttonLeft);
  PlayerInput_CancelFlying.Assign(keyQ, keyNone, #0, false, buttonLeft);

  { Items shortcuts. }
  Input_UseLifePotion := TInputShortcut.Create(nil, 'Use life potion', 'life_potion_use', igItems);
  Input_UseLifePotion.Assign(keyL, keyNone, #0, false, buttonLeft);

  { Other shortcuts. }
  Input_ViewMessages := TInputShortcut.Create(nil, 'View all messages', 'view_messages', igOther);
  Input_ViewMessages.Assign(keyM, keyNone, #0, false, buttonLeft);
  Input_SaveScreen := TInputShortcut.Create(nil, 'Save screen', 'save_screen', igOther);
  Input_SaveScreen.Assign(keyF5, keyNone, #0, false, buttonLeft);
  Input_FPSShow := TInputShortcut.Create(nil, 'FPS show / hide', 'fps_toggle', igOther);
  Input_FPSShow.Assign(keyTab, keyNone, #0, false, buttonLeft);
  Input_DebugMenu := TInputShortcut.Create(nil, 'Debug menu', 'debug_menu', igOther);
  Input_DebugMenu.Assign(keyBackQuote, keyNone, #0, false, buttonLeft);
end.

