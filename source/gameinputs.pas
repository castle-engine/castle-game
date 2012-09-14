{
  Copyright 2006-2012 Michalis Kamburelis.

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

{ Inputs specific to this game. }
unit GameInputs;

interface

uses CastleInputs;

var
  { Basic shortcuts. }
  CastleInput_Attack: TInputShortcut;

  { Items shortcuts. }
  CastleInput_InventoryShow: TInputShortcut;
  CastleInput_InventoryPrevious: TInputShortcut;
  CastleInput_InventoryNext: TInputShortcut;
  CastleInput_UseItem: TInputShortcut;
  CastleInput_UseLifePotion: TInputShortcut;
  CastleInput_DropItem: TInputShortcut;

  { Other shortcuts. }
  CastleInput_ViewMessages: TInputShortcut;
  CastleInput_SaveScreen: TInputShortcut;
  CastleInput_CancelFlying: TInputShortcut;
  CastleInput_FPSShow: TInputShortcut;
  CastleInput_DebugMenu: TInputShortcut;

implementation

uses KeysMouse;

initialization
  { Basic shortcuts. }
  CastleInput_Attack := TInputShortcut.Create(nil, 'Attack', 'attack', igBasic);
  CastleInput_Attack.Assign(K_Ctrl, K_None, #0, true, mbLeft);
  CastleInput_Attack.GroupOrder := -100; { before other shortcuts }

  { Items shortcuts. }
  CastleInput_InventoryShow := TInputShortcut.Create(nil, 'Inventory show / hide', 'inventory_toggle', igItems);
  CastleInput_InventoryShow.Assign(K_I, K_None, #0, false, mbLeft);
  CastleInput_InventoryPrevious := TInputShortcut.Create(nil, 'Select previous inventory item', 'inventory_previous', igItems);
  CastleInput_InventoryPrevious.Assign(K_LeftBracket, K_None, #0, false, mbLeft, mwUp);
  CastleInput_InventoryNext := TInputShortcut.Create(nil, 'Select next inventory item', 'inventory_next', igItems);
  CastleInput_InventoryNext.Assign(K_RightBracket, K_None, #0, false, mbLeft, mwDown);
  CastleInput_UseItem := TInputShortcut.Create(nil, 'Use (or equip) selected inventory item', 'item_use', igItems);
  CastleInput_UseItem.Assign(K_Enter, K_None, #0, false, mbLeft);
  CastleInput_UseLifePotion := TInputShortcut.Create(nil, 'Use life potion', 'life_potion_use', igItems);
  CastleInput_UseLifePotion.Assign(K_L, K_None, #0, false, mbLeft);
  CastleInput_DropItem := TInputShortcut.Create(nil, 'Drop selected inventory item', 'item_drop', igItems);
  CastleInput_DropItem.Assign(K_R, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  CastleInput_ViewMessages := TInputShortcut.Create(nil, 'View all messages', 'view_messages', igOther);
  CastleInput_ViewMessages.Assign(K_M, K_None, #0, false, mbLeft);
  CastleInput_SaveScreen := TInputShortcut.Create(nil, 'Save screen', 'save_screen', igOther);
  CastleInput_SaveScreen.Assign(K_F5, K_None, #0, false, mbLeft);
  CastleInput_CancelFlying := TInputShortcut.Create(nil, 'Cancel flying spell', 'cancel_flying', igOther);
  CastleInput_CancelFlying.Assign(K_Q, K_None, #0, false, mbLeft);
  CastleInput_FPSShow := TInputShortcut.Create(nil, 'FPS show / hide', 'fps_toggle', igOther);
  CastleInput_FPSShow.Assign(K_Tab, K_None, #0, false, mbLeft);
  CastleInput_DebugMenu := TInputShortcut.Create(nil, 'Debug menu', 'debug_menu', igOther);
  CastleInput_DebugMenu.Assign(K_BackQuote, K_None, #0, false, mbLeft);
end.

