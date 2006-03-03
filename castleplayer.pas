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
    procedure SetFlyingMode(const Value: boolean);
    procedure UpdateNavigatorFromFlyingMode;
  public
    constructor Create;
    destructor Destroy; override;

    property Life: Single read FLife write FLife default DefaultMaxLife;
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultMaxLife;
    property FlyingMode: boolean read FFlyingMode write SetFlyingMode default false;

    { Do not add to this manually --- always use PickItem.
      Items are owned by this class --- when destroing Items,
      we also destroy all Items.Items[]. }
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

    { Calculates what can be considered "bounding box of the player",
      taking into account global Level.CameRadius. Use for collision
      detection etc. }
    function BoundingBox: TBox3d;
  end;

implementation

uses KambiClassUtils, SysUtils, Keys, CastlePlay;

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

  S := Format('You pick "%s"', [Item.Kind.Name]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  GameMessage(S);
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