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

{ Keys of the game. }
unit CastleKeys;

interface

uses Keys, MatrixNavigation, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TKeyGroup = (kgBasic, kgItems, kgOther);

  TKeyConfiguration = class
  private
    FName: string;
    FGroup: TKeyGroup;
    FDefaultValue: TKey;
    FValue: TKey;
    procedure SetValue(NewValue: TKey);
  public
    constructor Create(const AName: string;
      const AGroup: TKeyGroup;
      const ADefaultValue: TKey);

    property Name: string read FName;
    property Group: TKeyGroup read FGroup;
    property DefaultValue: TKey read FDefaultValue;

    { Current key value. Initially equal to DefaultValue.
      On every change of Value (not inside TKeyConfiguration
      constructor), OnKeyChanged callbacks will be called. }
    property Value: TKey read FValue write SetValue;
  end;

  TObjectsListItem_3 = TKeyConfiguration;
  {$I objectslist_3.inc}
  TKeyConfigurationsList = class(TObjectsList_3)
  public
    function SeekKeyByValue(KeyValue: TKey): TKeyConfiguration;
    procedure RestoreDefaults;
  end;

  TKeyChangedEvent = procedure (KeyConfiguration: TKeyConfiguration) of object;
  PKeyChangedEvent = ^TKeyChangedEvent;

  TDynArrayItem_1 = TKeyChangedEvent;
  PDynArrayItem_1 = PKeyChangedEvent;
  {$define DYNARRAY_1_IS_FUNCTION}
  {$I dynarray_1.inc}
  TDynKeyChangedEventArray = class(TDynArray_1)
  public
    procedure ExecuteAll(KeyConfiguration: TKeyConfiguration);
  end;

var
  { Basic keys. }
  CastleKey_Attack: TKeyConfiguration;
  CastleKey_Forward: TKeyConfiguration;
  CastleKey_Backward: TKeyConfiguration;
  CastleKey_LeftRot: TKeyConfiguration;
  CastleKey_RightRot: TKeyConfiguration;
  CastleKey_LeftStrafe: TKeyConfiguration;
  CastleKey_RightStrafe: TKeyConfiguration;
  CastleKey_UpRotate: TKeyConfiguration;
  CastleKey_DownRotate: TKeyConfiguration;
  CastleKey_HomeUp: TKeyConfiguration;
  CastleKey_UpMove: TKeyConfiguration;
  CastleKey_DownMove: TKeyConfiguration;

  { Items keys. }
  CastleKey_InventoryShow: TKeyConfiguration;
  CastleKey_InventoryPrevious: TKeyConfiguration;
  CastleKey_InventoryNext: TKeyConfiguration;
  CastleKey_UseItem: TKeyConfiguration;
  CastleKey_DropItem: TKeyConfiguration;

  { Other keys. }
  CastleKey_ViewMessages: TKeyConfiguration;
  CastleKey_SaveScreen: TKeyConfiguration;
  CastleKey_CancelFlying: TKeyConfiguration;

  { List of all configurable keys.
    Will be created in initialization and freed in finalization of this unit.
    All TKeyConfiguration instances will automatically add to this. }
  CastleAllKeys: TKeyConfigurationsList;
  CastleGroupKeys: array[TKeyGroup] of TKeyConfigurationsList;

  OnKeyChanged: TDynKeyChangedEventArray;

{$undef read_interface}

implementation

uses SysUtils;

{$define read_implementation}
{$I dynarray_1.inc}
{$I objectslist_3.inc}

{ TKeyConfigurationsList ----------------------------------------------------- }

function TKeyConfigurationsList.SeekKeyByValue(KeyValue: TKey): TKeyConfiguration;
var
  I: Integer;
begin
  for i := 0 to High do
  begin
    Result := Items[I];
    if Result.Value = KeyValue then
      Exit;
  end;
  Result := nil;
end;

procedure TKeyConfigurationsList.RestoreDefaults;
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Value := Items[I].DefaultValue;
end;

{ TDynKeyChangedEventArray -------------------------------------------------- }

procedure TDynKeyChangedEventArray.ExecuteAll(
  KeyConfiguration: TKeyConfiguration);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I](KeyConfiguration);
end;

{ TKeyConfiguration ---------------------------------------------------------- }

constructor TKeyConfiguration.Create(const AName: string;
  const AGroup: TKeyGroup; const ADefaultValue: TKey);
begin
  inherited Create;
  FName := AName;
  FGroup := AGroup;
  FDefaultValue := ADefaultValue;
  FValue := DefaultValue;

  CastleAllKeys.Add(Self);
  CastleGroupKeys[Group].Add(Self);
end;

procedure TKeyConfiguration.SetValue(NewValue: TKey);
begin
  if Value <> NewValue then
  begin
    FValue := NewValue;
    OnKeyChanged.ExecuteAll(Self);
  end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure DoInitialization;
var
  KeyGroup: TKeyGroup;
begin
  OnKeyChanged := TDynKeyChangedEventArray.Create;
  CastleAllKeys := TKeyConfigurationsList.Create;

  for KeyGroup := Low(KeyGroup) to High(KeyGroup) do
    CastleGroupKeys[KeyGroup] := TKeyConfigurationsList.Create;

  { Order of creation below is significant: it determines the order
    of menu entries in "Configure controls". }

  { Basic keys. }
  CastleKey_Attack := TKeyConfiguration.Create('Attack', kgBasic, K_Ctrl);
  CastleKey_Forward := TKeyConfiguration.Create('Move forward', kgBasic, WalkerDefaultKey_Forward);
  CastleKey_Backward := TKeyConfiguration.Create('Move backward', kgBasic, WalkerDefaultKey_Backward);
  CastleKey_LeftRot := TKeyConfiguration.Create('Turn left', kgBasic, WalkerDefaultKey_LeftRot);
  CastleKey_RightRot := TKeyConfiguration.Create('Turn right', kgBasic, WalkerDefaultKey_RightRot);
  CastleKey_LeftStrafe := TKeyConfiguration.Create('Move left', kgBasic, WalkerDefaultKey_LeftStrafe);
  CastleKey_RightStrafe := TKeyConfiguration.Create('Move right', kgBasic, WalkerDefaultKey_RightStrafe);
  CastleKey_UpRotate := TKeyConfiguration.Create('Loop up', kgBasic, WalkerDefaultKey_UpRotate);
  CastleKey_DownRotate := TKeyConfiguration.Create('Look down', kgBasic, WalkerDefaultKey_DownRotate);
  CastleKey_HomeUp := TKeyConfiguration.Create('Look straight', kgBasic, WalkerDefaultKey_HomeUp);
  CastleKey_UpMove := TKeyConfiguration.Create('Jump (or fly up)', kgBasic, WalkerDefaultKey_Jump);
  CastleKey_DownMove := TKeyConfiguration.Create('Crouch (or fly down)', kgBasic, WalkerDefaultKey_Crouch);

  { Items keys. }
  CastleKey_InventoryShow := TKeyConfiguration.Create('Inventory show / hide', kgItems, K_I);
  CastleKey_InventoryPrevious := TKeyConfiguration.Create('Select previous inventory item', kgItems, K_LeftBracket);
  CastleKey_InventoryNext := TKeyConfiguration.Create('Select next inventory item', kgItems, K_RightBracket);
  CastleKey_UseItem := TKeyConfiguration.Create('Use (or equip) selected inventory item', kgItems, K_Enter);
  CastleKey_DropItem := TKeyConfiguration.Create('Drop selected inventory item', kgItems, K_D);

  { Other keys. }
  CastleKey_ViewMessages := TKeyConfiguration.Create('View all messages', kgOther, K_M);
  CastleKey_SaveScreen := TKeyConfiguration.Create('Save screen', kgOther, K_F5);
  CastleKey_CancelFlying := TKeyConfiguration.Create('Cancel flying', kgOther, K_C);
end;

procedure DoFinalization;
var
  KeyGroup: TKeyGroup;
begin
  for KeyGroup := Low(KeyGroup) to High(KeyGroup) do
    FreeAndNil(CastleGroupKeys[KeyGroup]);

  FreeWithContentsAndNil(CastleAllKeys);
  FreeAndNil(OnKeyChanged);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.