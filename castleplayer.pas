unit CastlePlayer;

interface

uses Boxes3d, MatrixNavigation, CastleItems;

const
  DefaultMaxLife = 100;

type
  { Player class.

    Note that this is designed in such way that it doesn't require current
    game to actually run. This means that various things like view saved player,
    edit initial player before game starts etc. are possible. }
  TPlayer = class
  private
    FLife: Single;
    FMaxLife: Single;
    FFlyingMode: boolean;
    FNavigator: TMatrixWalker;
    FItems: TItemsList;
    FEquippedWeapon: TItem;
    procedure SetFlyingMode(const Value: boolean);
    procedure UpdateNavigatorFromFlyingMode;
  public
    constructor Create;
    destructor Destroy; override;

    property Life: Single read FLife write FLife default DefaultMaxLife;
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultMaxLife;
    property FlyingMode: boolean read FFlyingMode write SetFlyingMode default false;

    { Inventory, items owned by the player.

      Do not add to this manually --- always use PickItem.
      Items are owned by this class --- when destroing Items,
      we also destroy all Items.Items[].

      Do not directly delete from this list --- always use
      DropItem or DeleteItem. }
    property Items: TItemsList read FItems;

    { Each player object always has related Navigator object.

      Some things are synchronized between player properties and this
      Navigator object --- e.g. player's FlyingMode is synchronized
      with Gravity and some Key_Xxx properties of this navigator.
      In general, this player object "cooperates" in various ways
      with it's Navigator object, that's why it was most comfortable
      to just put Navigator inside TPlayer.

      In general you *must not* operate directly on this Navigator properties
      or call it's methods (TPlayer instance should do this for you),
      with some exceptions.

      You are allowed to read and write:
      - Navigator.CameraPos, CameraPosDir, CameraPosUp and HomeCameraXxx ---
        these are exactly player's camera settings.
      - Navigator.CameraPreferredHeight. In fact, it's OK to just call
        Navigator.Init.
      - Navigator.ProjectionMatrix, to update it in game's OnResize or such.
      - You can call Navigator.KeyDown and Idle.
        It's OK to just assign Navigator to Glw.Navigator.
      - You can assign things to Navigator.OnMatrixChanged,

      You are allowed to read:
      - Navigator.RotationOnlyMatrix, Matrixm, Frustum.
    }
    property Navigator: TMatrixWalker read FNavigator;

    { This adds Item to Items, with appropriate GameMessage }
    procedure PickItem(Item: TItem);

    { Drops given item. ItemIndex must be valid (between 0 and Items.Count - 1).
      Returns nil if player somehow cancelled operation and nothing is dropped.
      You *must* take care of returned TItem object (otherwise you will
      get memory leak !). }
    function DropItem(ItemIndex: Integer): TItem;

    { Deletes given item from the list. Note that this is different
      than DropItem: it's more low-level, which means that

      1. it doesn't ask or care about item's quantity --- it will always
         delete it as a whole, no matter what quantity is has
      2. it doesn't do any nice GameMessage that you dropped an item.

      However, it does check whether the deleted item was EquippedWeapon
      and if it was, it will set EquippedWeapon to nil and give to player
      a message that he's no longer using that weapon. }
    function DeleteItem(ItemIndex: Integer): TItem;

    { Calculates what can be considered "bounding box of the player",
      taking into account global Level.CameRadius. Use for collision
      detection etc. }
    function BoundingBox: TBox3d;

    { Weapon the player is using right now, or nil if none.

      EquippedWeapon.Kind must be TItemWeaponKind.

      You can set this property only to some item existing on Items.
      When dropping items and current weapon will be dropped,
      we will automatically set this back to nil. }
    property EquippedWeapon: TItem read FEquippedWeapon write FEquippedWeapon;
  end;

implementation

uses KambiClassUtils, SysUtils, Keys, CastlePlay, GLWinMessages,
  CastleWindow, KambiUtils;

constructor TPlayer.Create;
begin
  inherited Create;
  FLife := DefaultMaxLife;
  FMaxLife := DefaultMaxLife;;
  FItems := TItemsList.Create;

  FNavigator := TMatrixWalker.Create(nil);
  Navigator.Key_MoveSpeedInc := K_None; { turn key off }
  Navigator.Key_MoveSpeedDec := K_None; { turn key off }

  UpdateNavigatorFromFlyingMode;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(FNavigator);
  FreeWithContentsAndNil(FItems);
  inherited;
end;

procedure TPlayer.UpdateNavigatorFromFlyingMode;
begin
  Navigator.Gravity := not FlyingMode;
  if FlyingMode then
  begin
    Navigator.Key_Jump := K_None;
    Navigator.Key_Crouch := K_None;
    Navigator.Key_UpMove := K_A;
    Navigator.Key_DownMove := K_Z;
  end else
  begin
    Navigator.Key_Jump := K_A;
    Navigator.Key_Crouch := K_Z;
    Navigator.Key_UpMove := K_None;
    Navigator.Key_DownMove := K_None;
  end;
end;

procedure TPlayer.SetFlyingMode(const Value: boolean);
const
  FlyingModeActivated: array[boolean] of string =
  ( 'Flying mode OFF',
    'Flying mode ON' );
begin
  if FFlyingMode <> Value then
  begin
    FFlyingMode := Value;
    UpdateNavigatorFromFlyingMode;
    GameMessage(FlyingModeActivated[FlyingMode]);
  end;
end;

procedure TPlayer.PickItem(Item: TItem);
var
  S: string;
begin
  Items.Add(Item);

  { TODO: Optional stacking (adding quantity with existing item) should occur here }
  { TODO: Equiping weapon/armor if not wearing one should occur here }

  S := Format('You pick "%s"', [Item.Kind.Name]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  GameMessage(S);
end;

function TPlayer.DropItem(ItemIndex: Integer): TItem;
var
  SelectedItem: TItem;
  DropQuantity: Cardinal;
  S: string;
begin
  SelectedItem := Items[ItemIndex];

  if SelectedItem.Quantity > 1 then
  begin
    DropQuantity := SelectedItem.Quantity;

    if not MessageInputQueryCardinal(Glw,
      Format('You have %d items "%s". How many of them do you want to drop ?',
        [SelectedItem.Quantity, SelectedItem.Kind.Name]),
      DropQuantity, taLeft) then
      Exit(nil);

    if not Between(DropQuantity, 1, SelectedItem.Quantity) then
    begin
      GameMessage(Format('You cannot drop %d items', [DropQuantity]));
      Exit(nil);
    end;
  end else
    DropQuantity := 1;

  if DropQuantity = SelectedItem.Quantity then
  begin
    Result := SelectedItem;
    DeleteItem(ItemIndex);
  end else
  begin
    Result := SelectedItem.Split(DropQuantity);
  end;

  S := Format('You drop "%s"', [Result.Kind.Name]);
  if Result.Quantity <> 1 then
    S += Format(' (quantity %d)', [Result.Quantity]);
  GameMessage(S);
end;

function TPlayer.DeleteItem(ItemIndex: Integer): TItem;
begin
  Result := Items[ItemIndex];
  Items.Delete(ItemIndex);
  if Result = EquippedWeapon then
  begin
    FEquippedWeapon := nil;
    GameMessage('You''re no longer using your weapon');
  end;
end;

function TPlayer.BoundingBox: TBox3d;
var
  PlayerSize: Single;
begin
  Result[0] := Player.Navigator.CameraPos;
  Result[1] := Player.Navigator.CameraPos;

  PlayerSize := Level.CameraRadius;

  Result[0, 0] -= PlayerSize;
  Result[0, 1] -= PlayerSize;
  Result[0, 2] -= Navigator.RealCameraPreferredHeight;

  Result[1, 0] += PlayerSize;
  Result[1, 1] += PlayerSize;
  Result[1, 2] += Level.CameraRadius;
end;

end.