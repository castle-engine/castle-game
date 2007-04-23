{
  Copyright 2006,2007 Michalis Kamburelis.

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
  VRMLSceneWaypoints, CastleInputs, ALSourceAllocator, CastleSound,
  VRMLTriangleOctree, CastleTextures;

const
  DefaultMaxLife = 100;

type
  TPlayerSwimming = (psNo, psAboveWater, psUnderWater);

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

    The same thing applied to GameWin = @true state.
    Generally all things are allowed except things that are explicitly
    forbidden.
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
      Call this always when FlyingMode or Dead or some key values
      or Swimming or GameWin change. }
    procedure UpdateNavigator;

    procedure FalledDown(Navigator: TMatrixWalker; const FallenHeight: Single);
    procedure SetLife(const Value: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color). }
    procedure SetLifeCustomBlackOut(const Value: Single;
      const Color: TVector3Single);

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

    procedure InputChanged(InputConfiguration: TInputConfiguration);

    { If Swimming = psUnderWater, then this is the time (from Level.AnimationTime)
      of setting Swimming to psUnderWater. }
    SwimBeginTime: Single;
    { If Swimming = psUnderWater, this is the time of last
      drowning (or 0.0 if there was no drowning yet in this swimming session). }
    SwimLastDrownTime: Single;
    { If Swimming = psUnderWater, this is the time of last stPlayerSwimming sound
      (or 0.0 if there was no stPlayerSwimming played yet in this
      swimming session). }
    SwimLastSoundTime: Single;

    { Did last Idle detected that we're on lava ? }
    IsLava: boolean;
    { Relevant if IsLava, this is Level.AnimationTime when
      last time lava damage was done. When player steps on lava for the
      first time, he immediately gets damage, so LavaLastDamageTime is
      always valid when IsLava. }
    LavaLastDamageTime: Single;

    AllocatedSwimmingChangeSource: TALAllocatedSource;
    procedure AllocatedSwimmingChangeSourceUsingEnd(Sender: TALAllocatedSource);

    AllocatedSwimmingSource: TALAllocatedSource;
    procedure AllocatedSwimmingSourceUsingEnd(Sender: TALAllocatedSource);

    FSwimming: TPlayerSwimming;
    procedure SetSwimming(const Value: TPlayerSwimming);

    { Did last Idle detected that we are on the ground. }
    IsOnTheGround: boolean;
    { <> @nil if IsOnTheGround and last ground had some TTextureRule. }
    GroundRule: TTextureRule;
    ReallyIsOnTheGroundTime: Single;

    FGround: POctreeItem;

    { There always must be satisfied:
        AllocatedFootstepsSource <> nil
      if and only if
        FootstepsSoundPlaying <> stNone. }
    AllocatedFootstepsSource: TALAllocatedSource;
    FootstepsSoundPlaying: TSoundType;
    ReallyWalkingOnTheGroundTime: Single;
    procedure AllocatedFootstepsSourceUsingEnd(Sender: TALAllocatedSource);

    { FKnockbackDistance <= 0 means "no knockback currently" }
    FKnockbackDistance: Single;
    { This must be valid and non-zero when FKnockbackDistance > 0 }
    FKnockbackDirection: TVector3Single;
    KnockBackSpeed: Single;
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

      Note that while you can call this when Dead or GameWin, this will
      be always ignored (because when Dead or GameWin,
      FlyingMode is always false). }
    procedure CancelFlying;

    { Inventory, items owned by the player.

      Do not add to this manually --- always use PickItem.
      Items are owned by this class --- when destroing Items,
      we also destroy all Items.Items[].

      Do not directly delete from this list --- always use
      DropItem or DeleteItem.

      @noAutoLinkHere }
    property Items: TItemsList read FItems;

    { Each player object always has related Navigator object.

      Some things are synchronized between player properties and this
      Navigator object --- e.g. player's FlyingMode is synchronized
      with Gravity and some Input_Xxx properties of this navigator.
      In general, this player object "cooperates" in various ways
      with it's Navigator object, that's why it was most comfortable
      to just put Navigator inside TPlayer.

      In general you @italic(must not) operate directly on this Navigator
      properties or call it's methods (TPlayer instance should do this for you),
      with some exceptions.

      You are allowed to read and write:
      @unorderedList(
        @item(Navigator.CameraPos, CameraDir, CameraUp and InitialCameraXxx ---
          these are exactly player's camera settings.)
        @item(Navigator.CameraPreferredHeight. In fact, it's OK to just call
          Navigator.Init.)
        @item(Navigator.RotationHorizontal/VerticalSpeed
          (you can read and write this --- although it should be
          only for testing/debug purposes, in real game rotation speeds
          should stay constant).)
        @item(Navigator.ProjectionMatrix, to update it in game's
          OnResize or such.)
        @item(You can call Navigator.KeyDown, MouseDown, Idle.
          In fact it's OK to just assign Navigator to Glw.Navigator.)
        @item(You can assign things to Navigator.OnMatrixChanged.)
      )

      You are allowed to read:
      @unorderedList(
        @item Navigator.RotationOnlyMatrix, Matrixm, Frustum.
      )
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

      @orderedList(
        @item(it doesn't ask or care about item's quantity --- it will always
         delete it as a whole, no matter what quantity is has)
        @item(it doesn't do any nice GameMessage that you dropped an item.)
      )

      However, it does check whether the deleted item was EquippedWeapon
      and if it was, it will set EquippedWeapon to nil and give to player
      a message that he's no longer using that weapon. }
    function DeleteItem(ItemIndex: Integer): TItem;

    { Like BoundingBox, but assumes that Navigator.CameraPos is as specified. }
    function BoundingBoxAssuming(const AssumeCameraPos: TVector3Single;
      Tall: boolean = true): TBox3d;

    { Calculates what can be considered "bounding box of the player",
      taking into account global Level.CameRadius around current CameraPos.
      Use for collision detection etc.

      If Tall then the returned box uses current camera height
      (i.e. Navigator.RealCameraPreferredHeight).

      If not Tall, then the box is just CameraRadius around
      CameraPos, so it could be more accurately described
      as a sphere with CameraRadius around CameraPos.
      In this case, the box doesn't really represent player
      (you can say that player's "legs" are not included in the box).
      However, not Tall box can still be useful (e.g. when checking for
      collision with creatures, because then the player will "grow"
      anyway (using GetCameraHeight), so Navigator.RealCameraPreferredHeight
      will be taken into account but in a different way. }
    function BoundingBox(Tall: boolean = true): TBox3d;

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
      For now, this is for things like FlyingModeTimeout to "wear out".
      @noAutoLinkHere }
    procedure Idle(const CompSpeed: Single);

    { Make blackout with given Color (so it's not really a "black"out,
      it's fadeout + fadein with given Color; e.g. pass here red
      to get "redout").
      @noAutoLinkHere }
    procedure BlackOut(const Color: TVector3f);

    { Shortcut for BlackOut with red color.
      @noAutoLinkHere }
    procedure RedOut;

    { Just a shortcut for Life <= 0.
      @noAutoLinkHere }
    function Dead: boolean;

    { @noAutoLinkHere }
    procedure Attack;

    { This will render player's weapon attacking.
      This is a 3D rendering. Note that this may clear depth buffer
      and set matrix to identity. }
    procedure RenderAttack;

    { You should set this property as appropriate.
      This object will just use this property (changing it's Navigator
      properties etc.). }
    property Swimming: TPlayerSwimming read FSwimming write SetSwimming;

    { Do knockback. KnockbackDistance must be > 0.
      KnockbackDirection must be normalized (so, obviously, also non-zero)
      vector.

      This also lowers player's life (exactly like
      Player.Life := Player.Life - LifeLoss), because usually knockback
      is caused by life loss, so usually you want to lower life
      along with doing knockback. }
    procedure Knockback(const LifeLoss: Single;
      const AKnockbackDistance: Single;
      const AKnockbackDirection: TVector3Single);

    { This loads player's properties from player.xml file.
      This is called from constructor, you can also call this
      later (for debug purposes, if you changed something). }
    procedure LoadFromFile;

    property Ground: POctreeItem read FGround write FGround;

    procedure LevelChanged;
  end;

implementation

uses Math, SysUtils, KambiClassUtils, Keys, CastlePlay, GLWinMessages,
  CastleWindow, KambiUtils, OpenGLBmpFonts, OpenGLFonts,
  GLWindow, KambiGLUtils, Images, KambiFilesUtils,
  VRMLGLAnimation, ALUtils, OpenAL, VRMLNodes, CastleControlsMenu,
  CastleTimeMessages, KambiXMLCfg, VRMLFlatSceneGL;

var
  GLList_BlankIndicatorImage: TGLuint;
  GLList_RedIndicatorImage: TGLuint;
  GLList_BlueIndicatorImage: TGLuint;
  GLList_DrawWaterRect: TGLuint;
  GLList_BossIndicatorImage: TGLuint;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create;
begin
  inherited Create;
  FLife := DefaultMaxLife;
  FMaxLife := DefaultMaxLife;
  FItems := TItemsList.Create;

  FNavigator := TMatrixWalker.Create(nil);
  Navigator.Input_MoveSpeedInc.MakeClear; { turn key off }
  Navigator.Input_MoveSpeedDec.MakeClear; { turn key off }
  Navigator.CheckModsDown := false;
  Navigator.OnFalledDown := @FalledDown;

  HintEscapeKeyShown := false;

  OnInputChanged.AppendItem(@InputChanged);

  LoadFromFile;

  { Although it will be called in every OnIdle anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Navigator has already good values. }
  UpdateNavigator;
end;

destructor TPlayer.Destroy;
begin
  if OnInputChanged <> nil then
    OnInputChanged.DeleteFirstEqual(@InputChanged);

  FreeAndNil(FNavigator);
  FreeWithContentsAndNil(FItems);

  if AllocatedFootstepsSource <> nil then
    AllocatedFootstepsSource.DoUsingEnd;

  if AllocatedSwimmingChangeSource <> nil then
    AllocatedSwimmingChangeSource.DoUsingEnd;

  if AllocatedSwimmingSource <> nil then
    AllocatedSwimmingSource.DoUsingEnd;

  inherited;
end;

function TPlayer.GetFlyingMode: boolean;
begin
  Result := (FFlyingModeTimeOut > 0) and (not Dead) and (not GameWin);
end;

procedure TPlayer.FlyingModeTimeoutBegin(const TimeOut: Single);
begin
  if FFlyingModeTimeOut <= 0 then
    TimeMessage('You start flying');

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
    TimeMessage('You''re no longer flying');
  end;
end;

function TPlayer.PickItem(Item: TItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Kind.Name]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  TimeMessage(S);

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
      TimeMessage(Format('You cannot drop %d items', [DropQuantity]));
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
  TimeMessage(S);

  Sound(stPlayerDropItem);
end;

function TPlayer.DeleteItem(ItemIndex: Integer): TItem;
begin
  Result := Items[ItemIndex];
  Items.Delete(ItemIndex);
  if Result = EquippedWeapon then
    EquippedWeapon := nil;
end;

function TPlayer.BoundingBoxAssuming(const AssumeCameraPos: TVector3Single;
  Tall: boolean): TBox3d;
var
  PlayerSize: Single;
begin
  Result[0] := AssumeCameraPos;
  Result[1] := AssumeCameraPos;

  PlayerSize := Level.CameraRadius;

  Result[0, 0] -= PlayerSize;
  Result[0, 1] -= PlayerSize;
  if Tall then
    Result[0, 2] -= Navigator.RealCameraPreferredHeight else
    Result[0, 2] -= Level.CameraRadius;

  Result[1, 0] += PlayerSize;
  Result[1, 1] += PlayerSize;
  Result[1, 2] += Level.CameraRadius;
end;

function TPlayer.BoundingBox(Tall: boolean): TBox3d;
begin
  Result := BoundingBoxAssuming(Player.Navigator.CameraPos, Tall);
end;

procedure TPlayer.SetEquippedWeapon(Value: TItem);
begin
  if Value <> FEquippedWeapon then
  begin
    FEquippedWeapon := Value;
    if EquippedWeapon = nil then
      TimeMessage('You''re no longer using your weapon') else
    begin
      TimeMessage(Format('You''re using weapon "%s" now',
        [EquippedWeapon.Kind.Name]));
      Assert(EquippedWeapon.Kind is TItemWeaponKind);
      Sound(EquippedWeaponKind.EquippingSound);
    end;

    { Any attack done with previous weapon must be stopped now. }
    Attacking := false;
  end;
end;

  procedure RenderLifeIndicator(const ALife, AMaxLife: Single;
    const GLList_FullIndicatorImage: TGLuint;
    const XMove: Integer; const PrintText: boolean);
  const
    IndicatorHeight = 120;
    IndicatorWidth = 40;
    IndicatorMargin = 5;
  var
    LifeMapped: Integer;
    LifeTextPosition: Integer;
    LifeText: string;
  begin
    glRasterPos2i(XMove + IndicatorMargin, IndicatorMargin);
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.5);

      LifeMapped := Round(MapRange(ALife, 0, AMaxLife, 0, IndicatorHeight));

      { Note that Life may be > MaxLife, and
        Life may be < 0. }
      if LifeMapped >= IndicatorHeight then
        glCallList(GLList_FullIndicatorImage) else
      if LifeMapped < 0 then
        glCallList(GLList_BlankIndicatorImage) else
      begin
        glEnable(GL_SCISSOR_TEST);
          glScissor(IndicatorMargin, IndicatorMargin, Glw.Width, LifeMapped);
          glCallList(GLList_FullIndicatorImage);
          glScissor(IndicatorMargin, IndicatorMargin + LifeMapped,
            Glw.Width, Glw.Height);
          glCallList(GLList_BlankIndicatorImage);
        glDisable(GL_SCISSOR_TEST);
      end;

    glDisable(GL_ALPHA_TEST);

    if PrintText then
    begin
      glColorv(Vector3Single(0.8, 0.8, 0.8));
      LifeText := Format('%d', [Round(ALife)]);
      LifeTextPosition := XMove + IndicatorMargin +
        (IndicatorWidth - Font_BFNT_BitstreamVeraSans.TextWidth(LifeText)) div 2;
      MaxTo1st(LifeTextPosition, IndicatorMargin);
      glRasterPos2i(LifeTextPosition, IndicatorMargin + IndicatorHeight div 2);
      Font_BFNT_BitstreamVeraSans.Print(LifeText);
    end;
  end;

procedure TPlayer.Render2D;
var
  BossCreatureIndicator: boolean;
  BossCreatureLife: Single;
  BossCreatureMaxLife: Single;
begin
  RenderLifeIndicator(Life, MaxLife, GLList_RedIndicatorImage, 0, true);

  BossCreatureIndicator := Level.BossCreatureIndicator(
    BossCreatureLife, BossCreatureMaxLife);
  if BossCreatureIndicator then
  begin
    RenderLifeIndicator(
      BossCreatureLife,
      BossCreatureMaxLife,
      GLList_BossIndicatorImage, Glw.Width - 150, false);
  end;

  if FlyingMode then
  begin
    glColorv(White3Single);
    glRasterPos2i(0, Glw.Height -
      Font_BFNT_BitstreamVeraSans.RowHeight - 5 { margin });
    Font_BFNT_BitstreamVeraSans.Print(Format('Flying (%d more seconds)',
      [Floor(FFlyingModeTimeout)]));
  end;

  glLoadIdentity;
  if Dead then
    DrawGLBlackOutRect(Red3Single, 1.0, 0, 0,
      Glw.Width, Glw.Height) else
  begin
    { The problem with drawing such water screen:
      Player eyes may be equal to water level,
      and then camera near plane cuts some water, and then player
      simultaneously sees things under the water (but not looking
      through the water surface, so he doesn't see blended water surface)
      and above the water.

      So effect like "show fog when player pos under the water" will look
      bad when player is exactly at the water surface: then he will
      be able to see some part water clearly (without the water fog,
      and without the blended water surface).

      I checked how this looks in quake2, and they simply ignored the
      problem (i.e. it is there...). And it's not so noticeable...
      So I can ignore this problem too :)

      Note: once I had an idea (an I actually did it in 1st szklane_lasy
      version) to mark water by the blueish fog. This looks cool,
      but in this case I can't use OpenGL fog, as it may be used
      by the level itself (as it is, actually, for the 'Gate" level). }

    if Swimming = psUnderWater then
      glCallList(GLList_DrawWaterRect);

    { Apply black out effect on the possibly watery effect.
      Yes, they both must mix. }
    DrawGLBlackOutRect(BlackOutColor, BlackOutIntensity, 0, 0,
      Glw.Width, Glw.Height);
  end;
end;

procedure TPlayer.UpdateNavigator;
begin
  Navigator.Gravity := (not FlyingMode) and (not GameWin);
  { Note that when not Navigator.Gravity then FallingDownEffect will not
    work anyway. }
  Navigator.FallingDownEffect := Swimming = psNo;

  Navigator.MouseLookHorizontalSensitivity := MouseLookHorizontalSensitivity;
  Navigator.MouseLookVerticalSensitivity := MouseLookVerticalSensitivity;
  Navigator.InvertVerticalMouseLook := InvertVerticalMouseLook;

  if GameWin then
  begin
    { Note that the change from MouseLook true to false here
      will not immediately cause UpdateMouseLook call in CastlePlay,
      and that's a good thing actually. No need to immediately
      bother user with mouse cursor displayed over beatiful
      game ending sequence. }
    Navigator.MouseLook := false;
    Navigator.Input_LeftRot.MakeClear;
    Navigator.Input_RightRot.MakeClear;
    Navigator.Input_UpRotate.MakeClear;
    Navigator.Input_DownRotate.MakeClear;
    Navigator.Input_GravityUp.MakeClear;
  end else
  begin
    { MouseLook is turned on always, even when player is dead.
      Just like rotation keys. }
    Navigator.MouseLook := UseMouseLook;

    { Rotation keys work always, even when player is dead.
      Initially I disabled them, but after some thought:
      let them work. They work a little strangely (because CameraUp
      is orthogonal to GravityUp), but they still work and player
      can figure it out. }
    Navigator.Input_LeftRot.Assign(CastleInput_LeftRot.Shortcut, false);
    Navigator.Input_RightRot.Assign(CastleInput_RightRot.Shortcut, false);
    Navigator.Input_UpRotate.Assign(CastleInput_UpRotate.Shortcut, false);
    Navigator.Input_DownRotate.Assign(CastleInput_DownRotate.Shortcut, false);
    Navigator.Input_GravityUp.Assign(CastleInput_GravityUp.Shortcut, false);
  end;

  if GameWin then
  begin
    { PreferGravityUpXxx should be ignored actually, because rotations
      don't work now. }
    Navigator.PreferGravityUpForMoving := true;
    Navigator.PreferGravityUpForRotations := false;

    Navigator.Input_Jump.MakeClear;
    Navigator.Input_Crouch.MakeClear;
    Navigator.Input_UpMove.MakeClear;
    Navigator.Input_DownMove.MakeClear;

    Navigator.Input_Forward.MakeClear;
    Navigator.Input_Backward.MakeClear;
    Navigator.Input_LeftStrafe.MakeClear;
    Navigator.Input_RightStrafe.MakeClear;

    Navigator.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Navigator.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
    Navigator.HeadBobbing := 0.0;
    if Level <> nil then
      Navigator.CameraPreferredHeight := Level.CameraRadius * 1.01 else
      Navigator.CameraPreferredHeight := 0;

    Navigator.MoveSpeed := 1.0;
    Navigator.MoveVertSpeed := 1.0;
  end else
  if Dead then
  begin
    Navigator.PreferGravityUpForMoving := true;
    { This is the only case when PreferGravityUpForRotations := false
      is sensible. }
    Navigator.PreferGravityUpForRotations := false;

    Navigator.Input_Jump.MakeClear;
    Navigator.Input_Crouch.MakeClear;
    Navigator.Input_UpMove.MakeClear;
    Navigator.Input_DownMove.MakeClear;

    Navigator.Input_Forward.MakeClear;
    Navigator.Input_Backward.MakeClear;
    Navigator.Input_LeftStrafe.MakeClear;
    Navigator.Input_RightStrafe.MakeClear;

    Navigator.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Navigator.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
    Navigator.HeadBobbing := 0.0;
    if Level <> nil then
      Navigator.CameraPreferredHeight := Level.CameraRadius * 1.01 else
      Navigator.CameraPreferredHeight := 0;

    Navigator.MoveSpeed := 1.0;
    Navigator.MoveVertSpeed := 1.0;
  end else
  begin
    if FlyingMode then
    begin
      Navigator.PreferGravityUpForMoving := false;
      Navigator.PreferGravityUpForRotations := true;

      Navigator.Input_Jump.MakeClear;
      Navigator.Input_Crouch.MakeClear;
      Navigator.Input_UpMove.Assign(CastleInput_UpMove.Shortcut, false);
      Navigator.Input_DownMove.Assign(CastleInput_DownMove.Shortcut, false);

      { Navigator.HeadBobbing and
        Navigator.CameraPreferredHeight and
        Navigator.FallingDownStartSpeed and
        Navigator.FallingDownSpeedIncrease
        ... don't matter here, because Gravity is false. }

      Navigator.MoveSpeed := 1.0;
      Navigator.MoveVertSpeed := 1.0;
    end else
    begin
      if Swimming <> psNo then
      begin
        Navigator.PreferGravityUpForMoving := false;
        Navigator.PreferGravityUpForRotations := true;

        Navigator.Input_Jump.MakeClear;
        Navigator.Input_Crouch.MakeClear;
        Navigator.Input_UpMove.Assign(CastleInput_UpMove.Shortcut, false);
        Navigator.Input_DownMove.Assign(CastleInput_DownMove.Shortcut, false);

        Navigator.FallingDownStartSpeed := DefaultFallingDownStartSpeed / 6;
        Navigator.FallingDownSpeedIncrease := 1.0;
        Navigator.HeadBobbing := 0.0;
        if Level <> nil then
          Navigator.CameraPreferredHeight := Level.CameraRadius * 1.01 else
          Navigator.CameraPreferredHeight := 0;

        Navigator.MoveSpeed := 0.5;
        Navigator.MoveVertSpeed := 0.5;
      end else
      begin
        Navigator.PreferGravityUpForMoving := true;
        Navigator.PreferGravityUpForRotations := true;

        Navigator.Input_Jump.Assign(CastleInput_UpMove.Shortcut, false);
        Navigator.Input_Crouch.Assign(CastleInput_DownMove.Shortcut, false);
        Navigator.Input_UpMove.MakeClear;
        Navigator.Input_DownMove.MakeClear;

        Navigator.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
        Navigator.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
        Navigator.HeadBobbing := DefaultHeadBobbing;
        if Level <> nil then
          Navigator.CameraPreferredHeight := Level.CameraPreferredHeight else
          Navigator.CameraPreferredHeight := 0;

        Navigator.MoveSpeed := 1.0;
        Navigator.MoveVertSpeed := 1.0;
      end;
    end;

    Navigator.Input_Forward.Assign(CastleInput_Forward.Shortcut, false);
    Navigator.Input_Backward.Assign(CastleInput_Backward.Shortcut, false);
    Navigator.Input_LeftStrafe.Assign(CastleInput_LeftStrafe.Shortcut, false);
    Navigator.Input_RightStrafe.Assign(CastleInput_RightStrafe.Shortcut, false);
  end;
end;

procedure TPlayer.AllocatedFootstepsSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = AllocatedFootstepsSource);
  AllocatedFootstepsSource.OnUsingEnd := nil;
  AllocatedFootstepsSource := nil;
  FootstepsSoundPlaying := stNone;
end;

procedure TPlayer.AllocatedSwimmingChangeSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = AllocatedSwimmingChangeSource);
  AllocatedSwimmingChangeSource.OnUsingEnd := nil;
  AllocatedSwimmingChangeSource := nil;
end;

procedure TPlayer.AllocatedSwimmingSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = AllocatedSwimmingSource);
  AllocatedSwimmingSource.OnUsingEnd := nil;
  AllocatedSwimmingSource := nil;
end;

procedure TPlayer.Idle(const CompSpeed: Single);

  { Perform various things related to player swimming. }
  procedure UpdateSwimming;
  const
    { How many seconds you can swin before you start to drown ? }
    SwimBreathSeconds = 30.0;
    { How many seconds between each drown ? }
    SwimDrownPauseSeconds = 5.0;
    { Pause between playing stPlayerSwimming sound.
      Remember to set this so that it will *not* easily synchronize
      with stPlayerDrowning. }
    SwimSoundPauseSeconds = 3.11111111;
  begin
    if Swimming = psUnderWater then
    begin
      { Take care of drowning. }
      if not Dead then
      begin
        if Level.AnimationTime - SwimBeginTime > SwimBreathSeconds then
        begin
          if (SwimLastDrownTime = 0.0) or
             (Level.AnimationTime - SwimLastDrownTime > SwimDrownPauseSeconds) then
          begin
            if SwimLastDrownTime = 0.0 then
              TimeMessage('You''re drowning');
            SwimLastDrownTime := Level.AnimationTime;
            Life := Life - (5 + Random(10));
            Sound(stPlayerDrowning);
          end;
        end;
      end;

      { Take care of playing stPlayerSwimming }
      { See comments at creation of AllocatedSwimmingChangeSource
        for reasons why I should safeguard here and play this sound
        only when AllocatedSwimmingSource = nil. }
      if (AllocatedSwimmingSource = nil) and
         ( (SwimLastSoundTime = 0.0) or
           (Level.AnimationTime - SwimLastSoundTime > SwimSoundPauseSeconds) ) then
      begin
        SwimLastSoundTime := Level.AnimationTime;
        AllocatedSwimmingSource := Sound(stPlayerSwimming);
        if AllocatedSwimmingSource <> nil then
          AllocatedSwimmingSource.OnUsingEnd :=
            @AllocatedSwimmingSourceUsingEnd;
      end;
    end;
  end;

  { Update IsOnTheGround and related variables. }
  procedure UpdateIsOnTheGround;
  const
    { TimeToChangeOnTheGround and ReallyIsOnTheGroundTime play here the
      analogous role as ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSoundPlaying, see UpdateFootstepsSoundPlaying. }
    TimeToChangeIsOnTheGround = 0.5;
  begin
    if Level = nil then
    begin
      GroundRule := nil;
      IsOnTheGround := false;
    end else
    if Navigator.IsOnTheGround then
    begin
      ReallyIsOnTheGroundTime := Level.AnimationTime;
      IsOnTheGround := true;
      GroundRule := TextureRulesList.GroundRule(Ground);
    end else
    if Level.AnimationTime - ReallyIsOnTheGroundTime >
      TimeToChangeIsOnTheGround then
    begin
      GroundRule := nil;
      IsOnTheGround := false;
    end else
    begin
      { Leave GroundRule and IsOnTheGround unchanged. }
    end;
  end;

  { Update IsLava and related variables, hurt player if on lava.
    Must be called after UpdateIsOnTheGround (depends on GroundRule). }
  procedure UpdateLava;
  var
    NewIsLava: boolean;
  begin
    NewIsLava := (GroundRule <> nil) and GroundRule.Lava;
    if NewIsLava then
    begin
      if (not IsLava) or
         (Level.AnimationTime - LavaLastDamageTime
           > GroundRule.LavaDamageTime) then
      begin
        LavaLastDamageTime := Level.AnimationTime;
        if not Dead then
        begin
          Sound(stPlayerLavaPain);
          SetLifeCustomBlackout(Life - (GroundRule.LavaDamageConst +
            Random * GroundRule.LavaDamageRandom), Green3Single);
        end;
      end;
    end;
    IsLava := NewIsLava;
  end;

  { Update FootstepsSoundPlaying and related variables.
    Must be called after UpdateIsOnTheGround (depends on GroundRule). }
  procedure UpdateFootstepsSoundPlaying;
  const
    TimeToChangeFootstepsSoundPlaying = 0.5;
  var
    NewFootstepsSoundPlaying: TSoundType;
  begin
    { The meaning of ReallyWalkingOnTheGroundTime and
      TimeToChangeFootstepsSoundPlaying:
      Navigator.IsWalkingOnTheGround can change quite rapidly
      (when player quickly presses and releases up/down keys,
      or when he're walking up the stairs, or when he's walking
      on un-flat terrain --- then Navigator.IsWalkingOnTheGround
      switches between @true and @false quite often).
      But it is undesirable to change FootstepsSoundPlaying
      so often, as this causes footsteps to suddenly stop, then play,
      then stop again etc. --- this doesn't sound good.

      So I use ReallyWalkingOnTheGroundTime to mark myself
      the time when Navigator.IsWalkingOnTheGround was true.
      In normal situation I would set NewFootstepsSoundPlaying to stNone
      when Navigator.IsWalkingOnTheGround = @false.
      But now I set NewFootstepsSoundPlaying to stNone only if
      Navigator.IsWalkingOnTheGround = @false
      for at least TimeToChangeFootstepsSoundPlaying seconds. }

    { calculate NewFootstepsSoundPlaying }
    if Level = nil then
      NewFootstepsSoundPlaying := stNone else
    if Navigator.IsWalkingOnTheGround then
    begin
      ReallyWalkingOnTheGroundTime := Level.AnimationTime;
      { Since Navigator.IsWalkingOnTheGroundm then for sure
        Navigator.IsOnTheGround, so UpdateIsOnTheGround updated
        GroundRule field. }
      if (GroundRule <> nil) and GroundRule.HasFootstepsSound then
        NewFootstepsSoundPlaying := GroundRule.FootstepsSound else
        NewFootstepsSoundPlaying := Level.FootstepsSound;
    end else
    if Level.AnimationTime - ReallyWalkingOnTheGroundTime >
      TimeToChangeFootstepsSoundPlaying then
      NewFootstepsSoundPlaying := stNone else
      NewFootstepsSoundPlaying := FootstepsSoundPlaying;

    { Once I had an idea here to use AL_LOOPING sound for footsteps.
      But this is not good, because then I would have to manually
      stop this sound whenever player stops walking. This is not so easy.
      This occurs when FootstepsSoundPlaying changes from non-stNone to stNone,
      but this occurs also when CastlePlay starts loading new level, enters
      game menu etc. --- in all these cases player footsteps must stop.
      So it's better (simpler) to simply use non-looping sound for footsteps.
      Whenever old sound for footsteps will end, this procedure will just
      allocate and start new footsteps sound. }

    if FootstepsSoundPlaying <> NewFootstepsSoundPlaying then
    begin
      if FootstepsSoundPlaying <> stNone then
      begin
        { Stop footsteps sound. }
        AllocatedFootstepsSource.DoUsingEnd;
        { AllocatedFootstepsSourceUsingEnd should set this to nil. }
        Assert(AllocatedFootstepsSource = nil);
      end;

      if NewFootstepsSoundPlaying <> stNone then
      begin
        { Start footsteps sound. }
        AllocatedFootstepsSource := Sound(NewFootstepsSoundPlaying, false);
        if AllocatedFootstepsSource <> nil then
        begin
          { Lower the position, to be on our feet. }
          alSourceVector3f(AllocatedFootstepsSource.ALSource,
            AL_POSITION, Vector3Single(0, 0, -1.0));
          AllocatedFootstepsSource.OnUsingEnd :=
            @AllocatedFootstepsSourceUsingEnd;
        end else
          { Failed to allocate source, so force new
            NewFootstepsSoundPlaying to stNone. }
          NewFootstepsSoundPlaying := stNone;
      end;

      FootstepsSoundPlaying := NewFootstepsSoundPlaying;
    end else
    if FootstepsSoundPlaying <> stNone then
    begin
      { So FootstepsSoundPlaying = NewFootstepsSoundPlaying for sure.
        Make sure that the AL sound is really playing.

        The decision to not use looping sound means that
        end of footsteps sound should be detected
        almost immediately (otherwise player will hear a little pause
        in footsteps, due to the time of OnTimer that calls RefreshUsed
        of source allocator --- it's very short pause, but it's noticeable,
        since footsteps should be rhytmic). I prefer to not rely on RefreshUsed
        for this and instead just check this here. }
      if not alSourcePlayingOrPaused(AllocatedFootstepsSource.ALSource) then
        alSourcePlay(AllocatedFootstepsSource.ALSource);
    end;

    Assert(
      (AllocatedFootstepsSource <> nil) =
      (FootstepsSoundPlaying <> stNone));
  end;

  procedure DoKnockback;
  var
    ProposedNewPosition, NewPosition: TVector3Single;
    CurrentKnockBackDistance: Single;
  begin
    if FKnockbackDistance > 0 then
    begin
      { Calculate CurrentKnockBackDistance, update KnockedBackDistance }
      CurrentKnockBackDistance := KnockBackSpeed * CompSpeed;
      MinTo1st(CurrentKnockBackDistance, FKnockbackDistance);
      FKnockbackDistance -= CurrentKnockBackDistance;

      ProposedNewPosition := VectorAdd(Navigator.CameraPos,
        VectorScale(FKnockbackDirection, CurrentKnockBackDistance));

      if Level.PlayerMoveAllowed(Navigator, ProposedNewPosition,
        NewPosition, false) then
        Navigator.CameraPos := NewPosition;
    end;
  end;

begin
  if FlyingMode then
  begin
    FFlyingModeTimeOut := FFlyingModeTimeOut - CompSpeed / 50;
    if not FlyingMode then
    begin
      TimeMessage('You''re no longer flying');
    end;
  end;

  UpdateNavigator;

  UpdateSwimming;

  if BlackOutIntensity > 0 then
    BlackOutIntensity -= 0.04 * CompSpeed;

  if Attacking and (not ActualAttackDone) and (Level.AnimationTime -
    AttackStartTime >= EquippedWeaponKind.ActualAttackTime) then
  begin
    ActualAttackDone := true;
    EquippedWeaponKind.ActualAttack(EquippedWeapon);
  end;

  if not HintEscapeKeyShown then
  begin
    HintEscapeKeyShown := true;
    TimeMessage('Hint: press "Escape" for game menu');
  end;

  UpdateIsOnTheGround;
  UpdateLava;
  UpdateFootstepsSoundPlaying;

  DoKnockback;
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
  if (Swimming = psNo) and (FallenHeight > 4.0) then
  begin
    Sound(stPlayerFalledDown);
    if FallenHeight > Navigator.MaxJumpDistance * 1.5 then
      Life := Life - Max(0, FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));
  end;
end;

procedure TPlayer.SetLifeCustomBlackOut(const Value: Single;
  const Color: TVector3Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    TimeMessage('You die');
    Sound(stPlayerDies);
    Navigator.FallOnTheGround;
  end else
  if (Life - Value) > 1 then
  begin
    BlackOut(Color);
    Sound(stPlayerSuddenPain);
  end;
  FLife := Value;
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  SetLifeCustomBlackOut(Value, Red3Single);
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
      TimeMessage('No weapon equipped');
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
      Anim.SceneFromTime(AttackTime).Render(nil, tgAll);
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

procedure TPlayer.InputChanged(InputConfiguration: TInputConfiguration);
begin
  UpdateNavigator;
end;

procedure TPlayer.SetSwimming(const Value: TPlayerSwimming);
begin
  if Value <> FSwimming then
  begin
    { If "Swimming = psUnderWater" state changed then play a sound. }
    if (FSwimming = psUnderWater) <>
       (Value = psUnderWater) then
    begin
      { If AllocatedSwimmingChangeSource <> nil, then the
        stPlayerSwimmingChange sound is already played (this may be caused
        when player tries to stay above the water --- he will then repeatedly
        go under and above the water). So do not start it again, to avoid
        bad sound atrifacts (the same sound playing a couple times on top
        of each other). }
      if AllocatedSwimmingChangeSource = nil then
      begin
        AllocatedSwimmingChangeSource := Sound(stPlayerSwimmingChange);
        if AllocatedSwimmingChangeSource <> nil then
          AllocatedSwimmingChangeSource.OnUsingEnd :=
            @AllocatedSwimmingChangeSourceUsingEnd;
      end;
    end;

    if (FSwimming = psNo) and (Value <> psNo) then
    begin
      { Cancel falling down, otherwise he will fall down into the water
        with the high speed (because in the air FallingDownStartSpeed
        is high and it's increased, but in the water it's much lower
        and not increased at all right now). }
      Navigator.CancelFallingDown;
    end;

    FSwimming := Value;

    if Swimming = psUnderWater then
    begin
      SwimBeginTime := Level.AnimationTime;
      SwimLastDrownTime := 0.0;
      SwimLastSoundTime := 0.0;
    end;

    { Although UpdateNavigator will be called in nearest Player.Idle anyway,
      I want to call it *now*. That's because I want to set
      Navigator.FallingDownStartSpeed to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Navigator.CancelFallingDown above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFallingDown ! So next falling down definitely *can*
      happen before next Player.Idle. Actually we may be in the middle
      of falling down right now. Fortunately Navigator.Idle
      and Navigator.CancelFallingDown are implemented (or rather fixed :)
      to honour calling CancelFallingDown and setting FallingDownStartSpeed now.

      So the safeguard below is needed. }
    UpdateNavigator;
  end;
end;

procedure TPlayer.Knockback(const LifeLoss: Single;
  const AKnockbackDistance: Single;
  const AKnockbackDirection: TVector3Single);
begin
  Life := Life - LifeLoss;
  FKnockbackDistance := AKnockbackDistance;
  FKnockbackDirection := AKnockbackDirection;
end;

procedure TPlayer.LoadFromFile;
var
  PlayerConfig: TKamXMLConfig;
begin
  PlayerConfig := TKamXMLConfig.Create(nil);
  try
    PlayerConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'player.xml';

    KnockBackSpeed :=
      PlayerConfig.GetFloat('player/knock_back_speed', 0.3);
    Navigator.MaxJumpHeight :=
      PlayerConfig.GetFloat('player/jump/max_height',
      DefaultMaxJumpHeight);
    Navigator.JumpSpeedMultiply :=
      PlayerConfig.GetFloat('player/jump/speed_multiply',
      DefaultJumpSpeedMultiply);
    Navigator.JumpPower :=
      PlayerConfig.GetFloat('player/jump/power',
      DefaultJumpPower);
    Navigator.HeadBobbingDistance :=
      PlayerConfig.GetFloat('player/head_bobbing_distance',
      DefaultHeadBobbingDistance);
  finally SysUtils.FreeAndNil(PlayerConfig); end;
end;

procedure TPlayer.LevelChanged;
begin
  { Without this, ReallyWalkingOnTheGroundTime could pretend that
    player is walking on the ground, while in fact the player is just
    standing still after new level loaded. }
  ReallyWalkingOnTheGroundTime := -1000.0;

  if FootstepsSoundPlaying <> stNone then
  begin
    { Stop footsteps sound. }
    AllocatedFootstepsSource.DoUsingEnd;
    { AllocatedFootstepsSourceUsingEnd should set this to nil. }
    Assert(AllocatedFootstepsSource = nil);

    FootstepsSoundPlaying := stNone;
  end;

  ReallyIsOnTheGroundTime := -1000;
  IsOnTheGround := false;
  GroundRule := nil;

  IsLava := false;
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
      PlayerControlFileName(BaseName),
      { We want alpha channel, and we expect it to be recorded in the file }
      [TAlphaImage], [ilcAlphaAdd], 0, 0);
  end;

begin
  GLList_BlankIndicatorImage := LoadPlayerControlToDisplayList('blank.png');
  GLList_RedIndicatorImage := LoadPlayerControlToDisplayList('red.png');
  GLList_BlueIndicatorImage := LoadPlayerControlToDisplayList('blue.png');
  GLList_BossIndicatorImage := LoadPlayerControlToDisplayList('boss.png');

  GLList_DrawWaterRect := glGenListsCheck(1, 'CastlePlayer.GLWindowInit');
  glNewList(GLList_DrawWaterRect, GL_COMPILE);
  try
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT);
      glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_SRC_ALPHA);
        glColorv(Vector4Single(0, 0, 0.1, 0.5));
        glRectf(0, 0, Glwin.Width, Glwin.Height);
      glDisable(GL_BLEND);
    glPopAttrib;
  finally glEndList; end;
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  glFreeDisplayList(GLList_BlankIndicatorImage);
  glFreeDisplayList(GLList_RedIndicatorImage);
  glFreeDisplayList(GLList_BlueIndicatorImage);
  glFreeDisplayList(GLList_BossIndicatorImage);
end;

initialization
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
end.