{ Keys of the game.
  TODO: make keys configurable in game. }
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
    FDefaultvalue: TKey;
    FValue: TKey;
  public
    constructor Create(const AName: string;
      const AGroup: TKeyGroup;
      const ADefaultvalue: TKey);

    property Name: string read FName;
    property Group: TKeyGroup read FGroup;
    property Defaultvalue: TKey read FDefaultvalue;

    { Current key value. Initially equal to DefaultValue. }
    property Value: TKey read FValue write FValue;
  end;

  TObjectsListItem_3 = TKeyConfiguration;
  {$I objectslist_3.inc}
  TKeyConfigurationsList = class(TObjectsList_3)
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
  CastleKeys: TKeyConfigurationsList;
  CastleGroupKeys: array[TKeyGroup] of TKeyConfigurationsList;

{$undef read_interface}

implementation

uses SysUtils;

{$define read_implementation}
{$I objectslist_3.inc}

{ TKeyConfiguration ---------------------------------------------------------- }

constructor TKeyConfiguration.Create(const AName: string;
  const AGroup: TKeyGroup; const ADefaultvalue: TKey);
begin
  inherited Create;
  FName := AName;
  FGroup := AGroup;
  FDefaultvalue := ADefaultvalue;
  FValue := DefaultValue;

  CastleKeys.Add(Self);
  CastleGroupKeys[Group].Add(Self);
end;

{ initialization / finalization ---------------------------------------------- }

procedure DoInitialization;
var
  KeyGroup: TKeyGroup;
begin
  CastleKeys := TKeyConfigurationsList.Create;

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

  FreeWithContentsAndNil(CastleKeys);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.