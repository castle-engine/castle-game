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
  CastleInput_Attack: TInputConfiguration;

  { Items shortcuts. }
  CastleInput_InventoryShow: TInputConfiguration;
  CastleInput_InventoryPrevious: TInputConfiguration;
  CastleInput_InventoryNext: TInputConfiguration;
  CastleInput_UseItem: TInputConfiguration;
  CastleInput_UseLifePotion: TInputConfiguration;
  CastleInput_DropItem: TInputConfiguration;

  { Other shortcuts. }
  CastleInput_ViewMessages: TInputConfiguration;
  CastleInput_SaveScreen: TInputConfiguration;
  CastleInput_CancelFlying: TInputConfiguration;
  CastleInput_FPSShow: TInputConfiguration;
  CastleInput_DebugMenu: TInputConfiguration;

implementation

uses KeysMouse;

initialization
  { Basic shortcuts. }
  CastleInput_Attack := TInputConfiguration.Create('Attack', 'attack', igBasic,
    K_Ctrl, K_None, #0, true, mbLeft);
  CastleInput_Attack.GroupOrder := -100; { before other shortcuts }

  { Items shortcuts. }
  CastleInput_InventoryShow := TInputConfiguration.Create('Inventory show / hide', 'inventory_toggle', igItems,
    K_I, K_None, #0, false, mbLeft);
  CastleInput_InventoryPrevious := TInputConfiguration.Create('Select previous inventory item', 'inventory_previous', igItems,
    K_LeftBracket, K_None, #0, false, mbLeft, mwUp);
  CastleInput_InventoryNext := TInputConfiguration.Create('Select next inventory item', 'inventory_next', igItems,
    K_RightBracket, K_None, #0, false, mbLeft, mwDown);
  CastleInput_UseItem := TInputConfiguration.Create('Use (or equip) selected inventory item', 'item_use', igItems,
    K_Enter, K_None, #0, false, mbLeft);
  CastleInput_UseLifePotion := TInputConfiguration.Create('Use life potion', 'life_potion_use', igItems,
    K_L, K_None, #0, false, mbLeft);
  CastleInput_DropItem := TInputConfiguration.Create('Drop selected inventory item', 'item_drop', igItems,
    K_R, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  CastleInput_ViewMessages := TInputConfiguration.Create('View all messages', 'view_messages', igOther,
    K_M, K_None, #0, false, mbLeft);
  CastleInput_SaveScreen := TInputConfiguration.Create('Save screen', 'save_screen', igOther,
    K_F5, K_None, #0, false, mbLeft);
  CastleInput_CancelFlying := TInputConfiguration.Create('Cancel flying spell', 'cancel_flying', igOther,
    K_Q, K_None, #0, false, mbLeft);
  CastleInput_FPSShow := TInputConfiguration.Create('FPS show / hide', 'fps_toggle', igOther,
    K_Tab, K_None, #0, false, mbLeft);
  CastleInput_DebugMenu := TInputConfiguration.Create('Debug menu', 'debug_menu', igOther,
    K_BackQuote, K_None, #0, false, mbLeft);
end.

