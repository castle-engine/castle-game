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

uses Boxes3d, MatrixNavigation, CastleItems, VectorMath, OpenGLh,
  VRMLSceneWaypoints, CastleKeys;

const
  DefaultMaxLife = 100;

type
  { Player class.

    Note that this is designed in such way that it doesn't require current
    game to actually run. This means that various things like view saved player,
    edit initial player before game starts etc. are possible.

    ------------------------------------------------------------
    Notes about dying:

    Dead player actually behaves (from the point
    of view of this class...) much like alive player. This means that
    e.g. dead player still has a Navigator, and it's CameraPos may still
    change (e.g. because player was killed when he was flying, or player
    corpse lays on some moving object of the level). It just cannot change
    because of player keys like up/down/rotate etc.
    This class automatically takes care of all things related to dying.

    Code using this class should just make sure to not do some
    forbidden things when player is dead --- right now this includes:
    @unorderedList(
      @item Calling PickItem, DeleteItem, DropItem and generally modifying Items.
      @item(Increasing Life (further decreasing Life is OK).
        Note that this means that once Player is Dead, (s)he cannot
        be alive again.)
      @item Changing EquippedWeapon, calling Attack.
    )

    Some other things in other units are also forbidden, see there for docs.
    In general, my strategy is that "if some method doesn't explicitly
    state that Player must be alive to call it --- then I'm allowed
    to call this method even when Player is Dead".
  }
  TPlayer = class
  private
    FLife: Single;
    FMaxLife: Single;
    FNavigator: TMatrixWalker;
    FItems: TItemsList;
    FEquippedWeapon: TItem;
    FFlyingModeTimeOut: Single; { > 0 means he's flying. In seconds. }
    function GetFlyingMode: boolean;
    procedure SetEquippedWeapon(Value: TItem);

    { blackout things }
    BlackOutIntensity: TGLfloat;
    BlackOutColor: TVector3f;

    { This updates Navigator properties.
      Call this always when FlyingMode or Dead or some key values change. }
    procedure UpdateNavigator;

    procedure FalledDown(Navigator: TMatrixWalker; const FallenHeight: Single);
    procedure SetLife(const Value: Single);

    { This means that weapon AttackAnimation is being done.
      This also means that EquippedWeapon <> nil. }
    Attacking: boolean;
    { If Attacking, then this is time of attack start, from Level.AnimationTime. }
    AttackStartTime: Single;
    { If Attacking, then this says whether EquippedWeapon.Kind.ActualAttack
      was already called. }
    ActualAttackDone: boolean;

    HintEscapeKeyShown: boolean;

    { Shortcut for TItemWeaponKind(EquippedWeapon.Kind).
      Call this only when EquippedWeapon <> nil. }
    function EquippedWeaponKind: TItemWeaponKind;

    procedure KeyChanged(KeyConfiguration: TKeyConfiguration);
  public
    constructor Create;
    destructor Destroy; override;

    property Life: Single read FLife write SetLife default DefaultMaxLife;
    property MaxLife: Single read FMaxLife write FMaxLife default DefaultMaxLife;

    property FlyingMode: boolean read GetFlyingMode;

    { Start FlyingMode, for TimeOut time (TimeOut time is in seconds).
      After TimeOut time, flying mode will stop.
      Call this only with TimeOut > 0. }
    procedure FlyingModeTimeoutBegin(const TimeOut: Single);

    { Cancel FlyingMode. Useful if you're in the FlyingMode that will
      automatically wear off, but you don't want to wait and you
      want to cancel flying *now*. Ignored if not in FlyingMode.

      Note that while you can call this when Dead, this will
      be always ignored (because when Dead, FlyingMode is always false). }
    procedure CancelFlying;

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

    { Return the one of Level.Sectors that contains Navigator.CameraPos.
      Nil if none. Yes, this is just a shortcut for
      Level.Sectors.SectorWithPoint(Navigator.CameraPos). }
    function CameraPosSector: TSceneSector;

    { This adds Item to Items, with appropriate GameMessage.
      Returns index inside Items to this item (note that this
      may be actually an index to some other TItem instance
      that was stacked with given Item). }
    function PickItem(Item: TItem): Integer;

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

    { Like BoundingBox, but assumes that Navigator.CameraPos is as specified. }
    function BoundingBoxAssuming(const AssumeCameraPos: TVector3Single): TBox3d;

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

    { Render 2D things (but not weapon) of the player. }
    procedure Render2D;

    { Render 2D weapon of the player. }
    procedure RenderWeapon2D;

    { Adjust some things based on passing time.
      For now, this is for things like FlyingModeTimeout to "wear out". }
    procedure Idle(const CompSpeed: Single);

    { Make blackout with given Color (so it's not really a "black"out,
      it's fadeout + fadein with given Color; e.g. pass here red
      to get "redout").
      @noAutoLinkHere }
    procedure BlackOut(const Color: TVector3f);

    { Shortcut for BlackOut with red color.
      @noAutoLinkHere }
    procedure RedOut;

    { Just a shortcut for Life <= 0. }
    function Dead: boolean;

    procedure Attack;

    { This will render player's weapon attacking.
      This is a 3D rendering. Note that this may clear depth buffer
      and set matrix to identity. }
    procedure RenderAttack;
  end;

implementation

uses Math, SysUtils, KambiClassUtils, Keys, CastlePlay, GLWinMessages,
  CastleWindow, KambiUtils, OpenGLBmpFonts, OpenGLFonts,
  GLWindow, KambiGLUtils, Images, KambiFilesUtils,
  CastleSound, VRMLGLAnimation;

var
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
  Navigator.AllowSlowerRotations := false;
  Navigator.OnFalledDown := FalledDown;

  HintEscapeKeyShown := false;

  OnKeyChanged.AppendItem(KeyChanged);

  { Although it will be called in every OnIdle anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Navigator has already good values. }
  UpdateNavigator;
end;

destructor TPlayer.Destroy;
begin
  if OnKeyChanged <> nil then
    OnKeyChanged.DeleteFirstEqual(KeyChanged);

  FreeAndNil(FNavigator);
  FreeWithContentsAndNil(FItems);
  inherited;
end;

function TPlayer.GetFlyingMode: boolean;
begin
  Result := (FFlyingModeTimeOut > 0) and (not Dead);
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
end;

procedure TPlayer.CancelFlying;
begin
  if FlyingMode then
  begin
    FFlyingModeTimeOut := 0;
    GameMessage('You''re no longer flying');
  end;
end;

function TPlayer.PickItem(Item: TItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Kind.Name]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  GameMessage(S);

  Sound(stPlayerPickItem);

  Result := Items.Stackable(Item);
  if Result <> -1 then
  begin
    { Stack Item with existing item }
    Items[Result].Quantity := Items[Result].Quantity + Item.Quantity;
    FreeAndNil(Item);
    Item := Items[Result];
  end else
  begin
    Items.Add(Item);
    Result := Items.High;
  end;

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

  Sound(stPlayerDropItem);
end;

function TPlayer.DeleteItem(ItemIndex: Integer): TItem;
begin
  Result := Items[ItemIndex];
  Items.Delete(ItemIndex);
  if Result = EquippedWeapon then
    EquippedWeapon := nil;
end;

function TPlayer.BoundingBoxAssuming(const AssumeCameraPos: TVector3Single):
  TBox3d;
var
  PlayerSize: Single;
begin
  Result[0] := AssumeCameraPos;
  Result[1] := AssumeCameraPos;

  PlayerSize := Level.CameraRadius;

  Result[0, 0] -= PlayerSize;
  Result[0, 1] -= PlayerSize;
  Result[0, 2] -= Navigator.RealCameraPreferredHeight;

  Result[1, 0] += PlayerSize;
  Result[1, 1] += PlayerSize;
  Result[1, 2] += Level.CameraRadius;
end;

function TPlayer.BoundingBox: TBox3d;
begin
  Result := BoundingBoxAssuming(Player.Navigator.CameraPos);
end;

procedure TPlayer.SetEquippedWeapon(Value: TItem);
begin
  if Value <> FEquippedWeapon then
  begin
    FEquippedWeapon := Value;
    if EquippedWeapon = nil then
      GameMessage('You''re no longer using your weapon') else
    begin
      GameMessage(Format('You''re using weapon "%s" now',
        [EquippedWeapon.Kind.Name]));
      Assert(EquippedWeapon.Kind is TItemWeaponKind);
    end;

    { Any attack done with previous weapon must be stopped now. }
    Attacking := false;
  end;
end;

procedure TPlayer.Render2D;

  procedure RenderLifeIndicator;
  const
    IndicatorHeight = 120;
    IndicatorWidth = 40;
    IndicatorMargin = 5;
  var
    PlayerLifeMapped: Integer;
    LifeTextPosition: Integer;
    LifeText: string;
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

    glColorv(Vector3Single(0.8, 0.8, 0.8));
    LifeText := Format('%d', [Round(Life)]);
    LifeTextPosition := IndicatorMargin +
      (IndicatorWidth - Font_BFNT_BitstreamVeraSans.TextWidth(LifeText)) div 2;
    MaxTo1st(LifeTextPosition, IndicatorMargin);
    glRasterPos2i(LifeTextPosition, IndicatorMargin + IndicatorHeight div 2);
    Font_BFNT_BitstreamVeraSans.Print(LifeText);
  end;

begin
  RenderLifeIndicator;

  if FlyingMode then
  begin
    glColorv(White3Single);
    glRasterPos2i(0, RequiredScreenHeight -
      Font_BFNT_BitstreamVeraSans.RowHeight - 5 { margin });
    Font_BFNT_BitstreamVeraSans.Print(Format('Flying (%d more seconds)',
      [Floor(FFlyingModeTimeout)]));
  end;

  glLoadIdentity;
  if Dead then
    DrawGLBlackOutRect(Red3Single, 1.0, 0, 0,
      RequiredScreenWidth, RequiredScreenHeight) else
    DrawGLBlackOutRect(BlackOutColor, BlackOutIntensity, 0, 0,
      RequiredScreenWidth, RequiredScreenHeight);
end;

procedure TPlayer.UpdateNavigator;
begin
  Navigator.Gravity := not FlyingMode;

  if Dead then
  begin
    Navigator.Key_Jump := K_None;
    Navigator.Key_Crouch := K_None;
    Navigator.Key_UpMove := K_None;
    Navigator.Key_DownMove := K_None;

    Navigator.Key_Forward := K_None;
    Navigator.Key_Backward := K_None;
    Navigator.Key_LeftRot := K_None;
    Navigator.Key_RightRot := K_None;
    Navigator.Key_LeftStrafe := K_None;
    Navigator.Key_RightStrafe := K_None;
    Navigator.Key_UpRotate := K_None;
    Navigator.Key_DownRotate := K_None;
    Navigator.Key_HomeUp := K_None;
  end else
  begin
    if FlyingMode then
    begin
      Navigator.Key_Jump := K_None;
      Navigator.Key_Crouch := K_None;
      Navigator.Key_UpMove := CastleKey_UpMove.Value;
      Navigator.Key_DownMove := CastleKey_DownMove.Value;
    end else
    begin
      Navigator.Key_Jump := CastleKey_UpMove.Value;
      Navigator.Key_Crouch := CastleKey_DownMove.Value;
      Navigator.Key_UpMove := K_None;
      Navigator.Key_DownMove := K_None;
    end;

    Navigator.Key_Forward := CastleKey_Forward.Value;
    Navigator.Key_Backward := CastleKey_Backward.Value;
    Navigator.Key_LeftRot := CastleKey_LeftRot.Value;
    Navigator.Key_RightRot := CastleKey_RightRot.Value;
    Navigator.Key_LeftStrafe := CastleKey_LeftStrafe.Value;
    Navigator.Key_RightStrafe := CastleKey_RightStrafe.Value;
    Navigator.Key_UpRotate := CastleKey_UpRotate.Value;
    Navigator.Key_DownRotate := CastleKey_DownRotate.Value;
    Navigator.Key_HomeUp := CastleKey_HomeUp.Value;
  end;
end;

procedure TPlayer.Idle(const CompSpeed: Single);
begin
  if FlyingMode then
  begin
    FFlyingModeTimeOut := FFlyingModeTimeOut - CompSpeed / 50;
    if not FlyingMode then
    begin
      GameMessage('You''re no longer flying');
    end;
  end;

  UpdateNavigator;

  if BlackOutIntensity > 0 then
    BlackOutIntensity -= 0.04 * Glw.FpsCompSpeed;

  if Attacking and (not ActualAttackDone) and (Level.AnimationTime -
    AttackStartTime >= EquippedWeaponKind.ActualAttackTime) then
  begin
    ActualAttackDone := true;
    EquippedWeaponKind.ActualAttack(EquippedWeapon);
  end;

  if not HintEscapeKeyShown then
  begin
    HintEscapeKeyShown := true;
    GameMessage('Hint: press "Escape" for game menu');
  end;
end;

procedure TPlayer.BlackOut(const Color: TVector3f);
begin
  BlackOutColor := Color;
  BlackOutIntensity := 1;
end;

procedure TPlayer.RedOut;
begin
  BlackOut(Red3Single);
end;

procedure TPlayer.FalledDown(Navigator: TMatrixWalker;
  const FallenHeight: Single);
begin
  if FallenHeight > 4 then
    Life := Life - Max(0, FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));

  { Tests: GameMessage(Format('Falled down from %f', [FallenHeight])); }
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    GameMessage('You die');
    Sound(stPlayerDies);
    Navigator.HeadBobbing := 0.0;
    Navigator.CameraPreferredHeight := Level.CameraRadius * 1.01;
    Navigator.FallOnTheGround;
  end else
  if (Life - Value) > 10 then
  begin
    RedOut;
    Sound(stPlayerSuddenPain);
  end;
  FLife := Value;
end;

function TPlayer.Dead: boolean;
begin
  Result := Life <= 0;
end;

procedure TPlayer.Attack;
begin
  if not Attacking then
  begin
    if EquippedWeapon <> nil then
    begin
      Sound(EquippedWeaponKind.SoundAttackStart);
      AttackStartTime := Level.AnimationTime;
      Attacking := true;
      ActualAttackDone := false;
    end else
      { TODO: maybe I should allow him to do some "punch" / "kick" here ? }
      GameMessage('No weapon equipped');
  end;
end;

procedure TPlayer.RenderWeapon2D;
begin
  if (EquippedWeapon <> nil) and (not Attacking) then
    glCallList(EquippedWeaponKind.GLList_DrawScreenImage);
end;

procedure TPlayer.RenderAttack;
var
  AttackTime: Single;
  Anim: TVRMLGLAnimation;
begin
  if Attacking then
  begin
    AttackTime := Level.AnimationTime - AttackStartTime;
    Anim := EquippedWeaponKind.AttackAnimation;
    if AttackTime <= Anim.TimeEnd then
    begin
      glClear(GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      Anim.SceneFromTime(AttackTime).Render(nil);
    end else
      Attacking := false;
  end;
end;

function TPlayer.EquippedWeaponKind: TItemWeaponKind;
begin
  Result := TItemWeaponKind(EquippedWeapon.Kind);
end;

function TPlayer.CameraPosSector: TSceneSector;
begin
  Result := Level.Sectors.SectorWithPoint(Navigator.CameraPos);
end;

procedure TPlayer.KeyChanged(KeyConfiguration: TKeyConfiguration);
begin
  UpdateNavigator;
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
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  glFreeDisplayList(GLList_BlankIndicatorImage);
  glFreeDisplayList(GLList_RedIndicatorImage);
  glFreeDisplayList(GLList_BlueIndicatorImage);
end;

initialization
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
end.