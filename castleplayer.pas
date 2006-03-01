unit CastlePlayer;

interface

uses MatrixNavigation;

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
    FFlyMode: boolean;
    FNavigator: TMatrixWalker;
    procedure SetFlyMode(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Life: Single read FLife write FLife default DefaultMaxLife;
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultMaxLife;
    property FlyMode: boolean read FFlyMode write SetFlyMode default false;

    { Each player object always has related Navigator object.

      Some things are synchronized between player properties and this
      Navigator object --- e.g. player's FlyMode is synchronized
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
  end;

implementation

uses SysUtils, Keys;

constructor TPlayer.Create;
begin
  inherited Create;
  FLife := DefaultMaxLife;
  FMaxLife := DefaultMaxLife;;

  FNavigator := TMatrixWalker.Create(nil);
  Navigator.Key_MoveSpeedInc := K_None; { turn key off }
  Navigator.Key_MoveSpeedDec := K_None; { turn key off }

  { yes, trigger SetFlyMode. This will also update Navigator properties. }
  FFlyMode := true;
  FlyMode := false;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(FNavigator);
  inherited;
end;

procedure TPlayer.SetFlyMode(const Value: boolean);
begin
  if FFlyMode <> Value then
  begin
    FFlyMode := Value;

    Navigator.Gravity := not FlyMode;
    if FlyMode then
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
end;

end.