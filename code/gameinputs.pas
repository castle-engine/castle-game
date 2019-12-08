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

uses CastleKeysMouse, CastleSceneManager, CastlePlayer, CastleLevels;

initialization
  { Change defaults, to make "attack" on mouse click.
    Consequently, interact must be something else --- key "e" is consistent
    with other 3D games. }
  Input_Attack.Assign(K_Ctrl, K_None, #0, true, mbLeft);
  Input_Interact.Assign(K_E, K_None, #0, false, mbLeft);
  PlayerInput_Jump.Assign(K_Space, K_None, #0, true, mbRight); // add rmb for jump
  { These by default don't have any shortcut }
  Input_InventoryShow.Assign(K_I, K_None, #0, false, mbLeft);
  Input_DropItem.Assign(K_R, K_None, #0, false, mbLeft);
  Input_CancelFlying.Assign(K_Q, K_None, #0, false, mbLeft);

  { Items shortcuts. }
  Input_UseLifePotion := TInputShortcut.Create(nil, 'Use life potion', 'life_potion_use', igItems);
  Input_UseLifePotion.Assign(K_L, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  Input_ViewMessages := TInputShortcut.Create(nil, 'View all messages', 'view_messages', igOther);
  Input_ViewMessages.Assign(K_M, K_None, #0, false, mbLeft);
  Input_SaveScreen := TInputShortcut.Create(nil, 'Save screen', 'save_screen', igOther);
  Input_SaveScreen.Assign(K_F5, K_None, #0, false, mbLeft);
  Input_FPSShow := TInputShortcut.Create(nil, 'FPS show / hide', 'fps_toggle', igOther);
  Input_FPSShow.Assign(K_Tab, K_None, #0, false, mbLeft);
  Input_DebugMenu := TInputShortcut.Create(nil, 'Debug menu', 'debug_menu', igOther);
  Input_DebugMenu.Assign(K_BackQuote, K_None, #0, false, mbLeft);
end.

