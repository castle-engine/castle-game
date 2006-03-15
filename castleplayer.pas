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

{ }
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
    FNavigator: TMatrixWalker;
    FItems: TItemsList;
    FEquippedWeapon: TItem;
    FFlyingModeTimeOut: Single; { > 0 means he's flying. In seconds. }
    function GetFlyingMode: boolean;
    procedure UpdateNavigatorFromFlyingMode;
    procedure SetEquippedWeapon(Value: TItem);
  public
    constructor Create;
    destructor Destroy; override;

    property Life: Single read FLife write FLife default DefaultMaxLife;
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultMaxLife;

    property FlyingMode: boolean read GetFlyingMode;

    { Start FlyingMode, for TimeOut time (TimeOut time is in seconds).
      After TimeOut time, flying mode will stop.
      Call this only with TimeOut > 0. }
    procedure FlyingModeTimeoutBegin(const TimeOut: Single);

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
      DeleteItem will automatically set this back to nil.

      When setting this property (to nil or non-nil) player may get
      GameMessage about using/not using a weapon. }
    property EquippedWeapon: TItem read FEquippedWeapon write SetEquippedWeapon;

    { Render 2D things of player. }
    procedure Render2D;

    { Adjust some things based on passing time.
      For now, this is for things like FlyingModeTimeout to "wear out". }
    procedure Idle(const CompSpeed: Single);
  end;

implementation

uses Math, SysUtils, KambiClassUtils, Keys, CastlePlay, GLWinMessages,
  CastleWindow, KambiUtils, OpenGLBmpFonts, OpenGLFonts,
  BFNT_BitstreamVeraSans_Unit, OpenGLh, GLWindow, KambiGLUtils,
  Images, VectorMath, KambiFilesUtils;

var
  PlayerInfoFont: TGLBitmapFont;
  GLList_BlankIndicatorImage: TGLuint;
  GLList_RedIndicatorImage: TGLuint;
  GLList_BlueIndicatorImage: TGLuint;

{ TPlayer -------------------------------------------------------------------- }

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

function TPlayer.GetFlyingMode: boolean;
begin
  Result := FFlyingModeTimeOut > 0;
end;

procedure TPlayer.FlyingModeTimeoutBegin(const TimeOut: Single);
begin
  if FFlyingModeTimeOut <= 0 then
    GameMessage('You start flying');

  { It's possible that FlyingModeTimeoutBegin is called when
    FFlyingModeTimeOut is already > 0. In this case, we set
    FFlyingModeTimeOut to maximum of current FFlyingModeTimeOut and TimeOut
    --- i.e. the effect that will allow player to fly longer wins. }
  FFlyingModeTimeOut := Max(FFlyingModeTimeOut, TimeOut);

  UpdateNavigatorFromFlyingMode;
end;

procedure TPlayer.PickItem(Item: TItem);
var
  S: string;
  StackIndex: Integer;
begin
  S := Format('You pick "%s"', [Item.Kind.Name]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  GameMessage(S);

  StackIndex := Items.Stackable(Item);
  if StackIndex <> -1 then
  begin
    { Stack Item with existing item }
    Items[StackIndex].Quantity := Items[StackIndex].Quantity + Item.Quantity;
    FreeAndNil(Item);
    Item := Items[StackIndex];
  end else
    Items.Add(Item);

  { Automatically equip the weapon. }
  if (Item.Kind is TItemWeaponKind) and (EquippedWeapon = nil) then
    EquippedWeapon := Item;
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
    EquippedWeapon := nil;
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

procedure TPlayer.SetEquippedWeapon(Value: TItem);
begin
  if Value <> FEquippedWeapon then
  begin
    FEquippedWeapon := Value;
    if EquippedWeapon = nil then
      GameMessage('You''re no longer using your weapon') else
      GameMessage(Format('You''re using weapon "%s" now',
        [EquippedWeapon.Kind.Name]));
  end;
end;

procedure TPlayer.Render2D;

  procedure RenderLifeIndicator;
  const
    IndicatorHeight = 120;
    IndicatorMargin = 5;
  var
    PlayerLifeMapped: Integer;
  begin
    glRasterPos2i(IndicatorMargin, IndicatorMargin);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

      PlayerLifeMapped :=
        Round(MapRange(Player.Life, 0, Player.MaxLife, 0, IndicatorHeight));

      { Note that Player.Life may be > Player.MaxLife, and
        Player.Life may be < 0. }
      if PlayerLifeMapped >= IndicatorHeight then
        glCallList(GLList_RedIndicatorImage) else
      if PlayerLifeMapped < 0 then
        glCallList(GLList_BlankIndicatorImage) else
      begin
        glEnable(GL_SCISSOR_TEST);
          glScissor(IndicatorMargin, IndicatorMargin, RequiredScreenWidth, PlayerLifeMapped);
          glCallList(GLList_RedIndicatorImage);
          glScissor(IndicatorMargin, IndicatorMargin + PlayerLifeMapped,
            RequiredScreenWidth, RequiredScreenHeight);
          glCallList(GLList_BlankIndicatorImage);
        glDisable(GL_SCISSOR_TEST);
      end;
    glDisable(GL_BLEND);
  end;

begin
  RenderLifeIndicator;

  if FlyingMode then
  begin
    glColorv(White3Single);
    glRasterPos2i(0, RequiredScreenHeight -
      PlayerInfoFont.RowHeight - 5 { margin });
    PlayerInfoFont.Print(Format('Flying (%d more seconds)',
      [Floor(FFlyingModeTimeout)]));
  end;
end;

procedure TPlayer.Idle(const CompSpeed: Single);
begin
  if FFlyingModeTimeOut > 0 { FlyingMode } then
  begin
    FFlyingModeTimeOut := FFlyingModeTimeOut - CompSpeed / 50;
    if FFlyingModeTimeOut <= 0 { not FlyingMode } then
    begin
      GameMessage('You''re no longer flying');
      UpdateNavigatorFromFlyingMode;
    end;
  end;
end;

{ GLWindow init / close ------------------------------------------------------ }

procedure GLWindowInit(Glwin: TGLWindow);

  function PlayerControlFileName(const BaseName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'player_controls' + PathDelim + BaseName;
  end;

  function LoadPlayerControlToDisplayList(const BaseName: string): TGLuint;
  begin
    Result := LoadImageToDispList(
      PlayerControlFileName(BaseName), [TAlphaImage], [], 0, 0);
  end;

begin
  GLList_BlankIndicatorImage := LoadPlayerControlToDisplayList('blank.png');
  GLList_RedIndicatorImage := LoadPlayerControlToDisplayList('red.png');
  GLList_BlueIndicatorImage := LoadPlayerControlToDisplayList('blue.png');

  PlayerInfoFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  FreeAndNil(PlayerInfoFont);

  glFreeDisplayList(GLList_BlankIndicatorImage);
  glFreeDisplayList(GLList_RedIndicatorImage);
  glFreeDisplayList(GLList_BlueIndicatorImage);
end;

initialization
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
end.