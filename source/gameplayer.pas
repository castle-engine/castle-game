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

{ }
unit GamePlayer;

interface

uses Boxes3D, Cameras, GameItems, VectorMath, GL, GLU, GLExt,
  SceneWaypoints, GameInputs, ALSoundAllocator, GameSound, GameObjectKinds,
  Triangle, GameTextures, XmlSoundEngine, Classes, Base3D,
  CastleGLUtils, CastleColors;

const
  DefaultPlayerLife = 100;

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
    e.g. dead player still has a Camera, and it's Position may still
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
  TPlayer = class(T3DAlive)
  private
    FCamera: TWalkCamera;
    FItems: TItemList;
    FEquippedWeapon: TItem;
    FFlyingModeTimeOut: Single; { > 0 means flying. In seconds. }

    { blackout things }
    BlackOutIntensity: TGLfloat;
    BlackOutColor: TVector3f;

    { This means that weapon AttackAnimation is being done.
      This also means that EquippedWeapon <> nil. }
    Attacking: boolean;
    { If Attacking, then this is time of attack start, from Level.AnimationTime. }
    AttackStartTime: Single;
    { If Attacking, then this says whether EquippedWeapon.Kind.ActualAttack
      was already called. }
    ActualAttackDone: boolean;

    HintEscapeKeyShown: boolean;

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
    FSwimming: TPlayerSwimming;

    { Did last Idle detected that we're on lava ? }
    IsLava: boolean;
    { Relevant if IsLava, this is Level.AnimationTime when
      last time lava damage was done. When player steps on lava for the
      first time, he immediately gets damage, so LavaLastDamageTime is
      always valid when IsLava. }
    LavaLastDamageTime: Single;

    AllocatedSwimmingChangeSource: TALSound;
    AllocatedSwimmingSource: TALSound;

    { Did last Idle detected that we are on the ground. }
    IsOnTheGround: boolean;
    { <> @nil if IsOnTheGround and last ground had some TTextureRule. }
    GroundRule: TTextureRule;
    ReallyIsOnTheGroundTime: Single;

    { There always must be satisfied:
        AllocatedFootstepsSource <> nil
      if and only if
        FootstepsSoundPlaying <> stNone. }
    AllocatedFootstepsSource: TALSound;
    FootstepsSoundPlaying: TSoundType;
    ReallyWalkingOnTheGroundTime: Single;

    FInventoryCurrentItem: Integer;
    FSickProjectionSpeed: Single;

    FResources: T3DResourceList;

    function GetFlyingMode: boolean;
    procedure SetEquippedWeapon(Value: TItem);

    { Update Camera properties, including inputs.
      Also updates Level.Input_PointingDeviceActivate, it's suitable to do it here.
      Call this always when FlyingMode or Dead or some key values
      or Swimming or GameWin change. }
    procedure UpdateCamera;

    procedure FalledDown(Camera: TWalkCamera; const FallenHeight: Single);

    { This sets life, just like SetLife.
      But in case of life loss, the fadeout is done with specified
      Color (while SetLife always uses red color). }
    procedure SetLifeCustomBlackOut(const Value: Single;
      const Color: TVector3Single);

    { Shortcut for TItemWeaponKind(EquippedWeapon.Kind).
      Call this only when EquippedWeapon <> nil. }
    function EquippedWeaponKind: TItemWeaponKind;

    procedure InputChanged(InputConfiguration: TInputConfiguration);

    procedure AllocatedSwimmingChangeSourceUsingEnd(Sender: TALSound);
    procedure AllocatedSwimmingSourceUsingEnd(Sender: TALSound);
    procedure SetSwimming(const Value: TPlayerSwimming);

    procedure AllocatedFootstepsSourceUsingEnd(Sender: TALSound);
  protected
    procedure SetLife(const Value: Single); override;
    function GetChild: T3D; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Height(const APosition, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    property Items: TItemList read FItems;

    { Each player object always has related Camera object.

      Some things are synchronized between player properties and this
      Camera object --- e.g. player's FlyingMode is synchronized
      with Gravity and some Input_Xxx properties of this camera.
      In general, this player object "cooperates" in various ways
      with it's Camera object, that's why it was most comfortable
      to just put Camera inside TPlayer.

      In general you @italic(must not) operate directly on this Camera
      properties or call it's methods (TPlayer instance should do this for you),
      with some exceptions.

      You are allowed to read and write:
      @unorderedList(
        @item(Camera.Position, Direction, Up and InitialCameraXxx ---
          these are exactly player's camera settings.)
        @item(Camera.PreferredHeight. In fact, it's OK to just call
          Camera.Init.)
        @item(Camera.RotationHorizontal/VerticalSpeed
          (you can read and write this --- although it should be
          only for testing/debug purposes, in real game rotation speeds
          should stay constant).)
        @item(Camera.ProjectionMatrix, to update it in game's
          OnResize or such.)
        @item(You can call Camera.KeyDown, MouseDown, Idle.
          In fact it's OK to just assign Camera to Window.Camera.)
        @item(You can assign things to Camera.OnMatrixChanged.)
      )

      You are allowed to read:
      @unorderedList(
        @item Camera.RotationMatrix, Matrix, Frustum.
      )
    }
    property Camera: TWalkCamera read FCamera;

    { Return the one of Level.Sectors that contains Camera.Position.
      Nil if none. Yes, this is just a shortcut for
      Level.Sectors.SectorWithPoint(Camera.Position). }
    function PositionSector: TSceneSector;

    { This adds Item to Items, with appropriate GameMessage.
      Returns index inside Items to this item (note that this
      may be actually an index to some other TItem instance
      that was stacked with given Item).

      This takes care of adjusting InventoryCurrentItem if needed
      (if no item was selected, then newly picked item becomes selected). }
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

    { Weapon the player is using right now, or nil if none.

      EquippedWeapon.Kind must be TItemWeaponKind.

      You can set this property only to some item existing on Items.
      When dropping items and current weapon will be dropped,
      DeleteItem will automatically set this back to nil.

      When setting this property (to nil or non-nil) player may get
      GameMessage about using/not using a weapon. }
    property EquippedWeapon: TItem read FEquippedWeapon write SetEquippedWeapon;

    { Render 2D things of the player. }
    procedure Render2D;

    { Adjust some things based on passing time.
      For now, this is for things like FlyingModeTimeout to "wear out".
      @noAutoLinkHere }
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { Make blackout with given Color (so it's not really a "black"out,
      it's fadeout + fadein with given Color; e.g. pass here red
      to get "redout").
      @noAutoLinkHere }
    procedure BlackOut(const Color: TVector3f);

    { Shortcut for BlackOut with red color.
      @noAutoLinkHere }
    procedure RedOut;

    { @noAutoLinkHere }
    procedure Attack;

    { You should set this property as appropriate.
      This object will just use this property (changing it's Camera
      properties etc.). }
    property Swimming: TPlayerSwimming read FSwimming write SetSwimming;

    { This loads player's properties from player.xml file.
      This is called from constructor, you can also call this
      later (for debug purposes, if you changed something). }
    procedure LoadFromFile;

    function Ground: PTriangle;

    procedure LevelChanged;

    { Currently selected inventory item.

      Note: while we try to always sensibly update InventoryCurrentItem,
      to keep the assumptions that
      @orderedList(
        @item(Items.Count = 0 => InventoryCurrentItem = -1)
        @item(Items.Count > 0 =>
         InventoryCurrentItem between 0 and Items.Count - 1))

      but you should @italic(nowhere) depend on these assuptions.
      That's because I want to allow myself freedom to modify Items
      in various situations, so InventoryCurrentItem can become
      invalid in many situations.

      So every code should check that
      @unorderedList(
        @item(If InventoryCurrentItem between 0 and Items.Count - 1
          then InventoryCurrentItem is selected)
        @item(Else no item is selected (possibly Items.Count = 0,
          possibly not)))
    }
    property InventoryCurrentItem: Integer
      read FInventoryCurrentItem write FInventoryCurrentItem
      default -1;

    property SickProjectionSpeed: Single
      read FSickProjectionSpeed write FSickProjectionSpeed;

    { Resource that have to be prepared for mere presence of player on a level.
      This is a place for any creatures that may be created by player
      actions. For example player may always have a bow and shoot an arrow,
      so this should contain Arrow creature.

      It's loaded from player.xml }
    property Resources: T3DResourceList read FResources;

    property Pushable default true;
    procedure Translate(const T: TVector3Single); override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const LineOfSight: boolean): boolean; override;
    function Sphere(out Radius: Single): boolean; override;
  end;

implementation

uses Math, SysUtils, CastleClassUtils, GamePlay, CastleMessages,
  GameWindow, CastleUtils, X3DNodes,
  CastleWindow, Images, CastleFilesUtils,
  PrecalculatedAnimation, ALUtils, CastleOpenAL, GameControlsMenu,
  GameNotifications, CastleXMLConfig, GLImages;

var
  GLList_BlankIndicatorImage: TGLuint;
  GLList_RedIndicatorImage: TGLuint;
  GLList_BlueIndicatorImage: TGLuint;
  GLList_DrawWaterRect: TGLuint;
  GLList_BossIndicatorImage: TGLuint;

{ TPlayerBox ----------------------------------------------------------------- }

type
  { Invisible box, that is added to make TPlayer collidable thanks to default
    T3DOrient (actually T3DList) methods. Owner must be TPlayer. }
  TPlayerBox = class(T3D)
  public
    function BoundingBox: TBox3D; override;
  end;

function TPlayerBox.BoundingBox: TBox3D;
var
  Camera: TWalkCamera;
begin
  if GetExists then
  begin
    Camera := TPlayer(Owner).Camera;
    Result.Data[0, 0] := -Camera.Radius;
    Result.Data[0, 1] := -Camera.Radius;
    Result.Data[0, 2] := -Camera.RealPreferredHeight;

    Result.Data[1, 0] := Camera.Radius;
    Result.Data[1, 1] := Camera.Radius;
    Result.Data[1, 2] := Camera.Radius;
  end else
    Result := EmptyBox3D;
end;

{ TPlayer -------------------------------------------------------------------- }

constructor TPlayer.Create(AOwner: TComponent);
var
  BaseLights: TLightInstancesList;
begin
  inherited Create(AOwner);
  Pushable := true;
  Life := DefaultPlayerLife;
  MaxLife := DefaultPlayerLife;

  Add(TPlayerBox.Create(Self));

  FItems := TItemList.Create(false);
  FInventoryCurrentItem := -1;

  FCamera := TWalkCamera.Create(nil);

  { turn off keys that are totally unavailable for the player }
  Camera.Input_MoveSpeedInc.MakeClear;
  Camera.Input_MoveSpeedDec.MakeClear;
  Camera.Input_IncreasePreferredHeight.MakeClear;
  Camera.Input_DecreasePreferredHeight.MakeClear;
  Camera.Input_Run.MakeClear; { speed in castle is so fast that we're always running }

  Camera.CheckModsDown := false;
  Camera.OnFalledDown := @FalledDown;

  HintEscapeKeyShown := false;

  OnInputChanged.Add(@InputChanged);

  FResources := T3DResourceList.Create(false);

  LoadFromFile;

  { TODO: not nice to initialize BaseLights here?
    player creatures should be required/released at each level start probably. }
  BaseLights := TLightInstancesList.Create;
  try
    Resources.Prepare(BaseLights);
  finally FreeAndNil(BaseLights) end;

  { Although it will be called in every OnIdle anyway,
    we also call it here to be sure that right after TPlayer constructor
    finished, Camera has already good values. }
  UpdateCamera;
end;

destructor TPlayer.Destroy;
begin
  EquippedWeapon := nil; { unregister free notification }

  if OnInputChanged <> nil then
    OnInputChanged.Remove(@InputChanged);

  FreeAndNil(FCamera);
  if FItems <> nil then
  begin
    FItems.FreeObjects := true;
    FreeAndNil(FItems);
  end;

  if AllocatedFootstepsSource <> nil then
    AllocatedFootstepsSource.DoUsingEnd;

  if AllocatedSwimmingChangeSource <> nil then
    AllocatedSwimmingChangeSource.DoUsingEnd;

  if AllocatedSwimmingSource <> nil then
    AllocatedSwimmingSource.DoUsingEnd;

  if Resources <> nil then
  begin
    Resources.Release;
    FreeAndNil(FResources);
  end;

  inherited;
end;

function TPlayer.GetFlyingMode: boolean;
begin
  Result := (FFlyingModeTimeOut > 0) and (not Dead) and (not GameWin);
end;

procedure TPlayer.FlyingModeTimeoutBegin(const TimeOut: Single);
begin
  if FFlyingModeTimeOut <= 0 then
    Notifications.Show('You start flying');

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
    Notifications.Show('You''re no longer flying');
  end;
end;

function TPlayer.PickItem(Item: TItem): Integer;
var
  S: string;
begin
  S := Format('You pick "%s"', [Item.Kind.Caption]);
  if Item.Quantity <> 1 then
    S += Format(' (quantity %d)', [Item.Quantity]);
  Notifications.Show(S);

  SoundEngine.Sound(stPlayerPickItem);

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
    Result := Items.Count - 1;
  end;

  { Automatically equip the weapon. }
  if (Item.Kind is TItemWeaponKind) and (EquippedWeapon = nil) then
    EquippedWeapon := Item;

  { Update InventoryCurrentItem. }
  if not Between(InventoryCurrentItem, 0, Items.Count - 1) then
    InventoryCurrentItem := Result;
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

    if not MessageInputQueryCardinal(Window,
      Format('You have %d items "%s". How many of them do you want to drop ?',
        [SelectedItem.Quantity, SelectedItem.Kind.Caption]),
      DropQuantity, taLeft) then
      Exit(nil);

    if not Between(DropQuantity, 1, SelectedItem.Quantity) then
    begin
      Notifications.Show(Format('You cannot drop %d items', [DropQuantity]));
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

  S := Format('You drop "%s"', [Result.Kind.Caption]);
  if Result.Quantity <> 1 then
    S += Format(' (quantity %d)', [Result.Quantity]);
  Notifications.Show(S);

  SoundEngine.Sound(stPlayerDropItem);
end;

function TPlayer.DeleteItem(ItemIndex: Integer): TItem;
begin
  Result := Items[ItemIndex];
  Items.Delete(ItemIndex);
  if Result = EquippedWeapon then
    EquippedWeapon := nil;
end;

procedure TPlayer.SetEquippedWeapon(Value: TItem);
begin
  if Value <> FEquippedWeapon then
  begin
    if FEquippedWeapon <> nil then
      FEquippedWeapon.RemoveFreeNotification(Self);

    FEquippedWeapon := Value;

    if FEquippedWeapon <> nil then
    begin
      Notifications.Show(Format('You''re using weapon "%s" now',
        [EquippedWeapon.Kind.Caption]));
      Assert(EquippedWeapon.Kind is TItemWeaponKind);
      SoundEngine.Sound(EquippedWeaponKind.EquippingSound);
      FEquippedWeapon.FreeNotification(Self);
    end else
      Notifications.Show('You''re no longer using your weapon');

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
          glScissor(IndicatorMargin, IndicatorMargin, Window.Width, LifeMapped);
          glCallList(GLList_FullIndicatorImage);
          glScissor(IndicatorMargin, IndicatorMargin + LifeMapped,
            Window.Width, Window.Height);
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
      GLList_BossIndicatorImage, Window.Width - 150, false);
  end;

  if FlyingMode then
  begin
    glColorv(White3Single);
    glRasterPos2i(0, Window.Height -
      Font_BFNT_BitstreamVeraSans.RowHeight - 5 { margin });
    Font_BFNT_BitstreamVeraSans.Print(Format('Flying (%d more seconds)',
      [Floor(FFlyingModeTimeout)]));
  end;

  glLoadIdentity;
  if Dead then
    DrawGLBlackOutRect(Red3Single, 1.0, 0, 0,
      Window.Width, Window.Height) else
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
      Window.Width, Window.Height);
  end;
end;

procedure TPlayer.UpdateCamera;
const
  CastleCameraInput = [ciNormal, ci3dMouse]; { do not include ciMouseDragging }
var
  LevelMoveHorizontalSpeed: Single;
  LevelMoveVerticalSpeed: Single;
  LevelCameraPreferredHeight: Single;
begin
  if Level <> nil then
  begin
    LevelMoveHorizontalSpeed := Level.MoveHorizontalSpeed;
    LevelMoveVerticalSpeed := Level.MoveVerticalSpeed;
    LevelCameraPreferredHeight := Level.CameraPreferredHeight;
  end else
  begin
    { This must work even when Level = nil. So we secure ourselves here,
      if Level = nil then we just apply some default values. They don't matter
      much, as user will not be able to play anyway without a Level loaded. }
    LevelMoveHorizontalSpeed := 1.0;
    LevelMoveVerticalSpeed := 1.0;
    LevelCameraPreferredHeight := 0.0;
  end;

  Camera.Gravity := (not FlyingMode) and (not GameWin);
  { Note that when not Camera.Gravity then FallingDownEffect will not
    work anyway. }
  Camera.FallingDownEffect := Swimming = psNo;

  Camera.MouseLookHorizontalSensitivity := MouseLookHorizontalSensitivity;
  Camera.MouseLookVerticalSensitivity := MouseLookVerticalSensitivity;
  Camera.InvertVerticalMouseLook := InvertVerticalMouseLook;

  { MouseLook is allowed always, even when player is dead.
    Just like rotation keys.

    Note that when GameWin, rotating will actually
    be disabled by Input := []. But still mouse look will cause mouse
    to remain hidden, which is good (why pop the mouse cursor on game
    win animation?). }
  Camera.MouseLook := UseMouseLook;

  if GameWin then
  begin
    { When GameWin, we navigate camera by code. }
    Camera.Input := [];
  end else
  begin
    Camera.Input := CastleCameraInput;

    { Rotation keys work always, even when player is dead.
      Initially I disabled them, but after some thought:
      let them work. They work a little strangely (because Up
      is orthogonal to GravityUp), but they still work and player
      can figure it out. }
    Camera.Input_LeftRot.Assign(CastleInput_LeftRot.Shortcut, false);
    Camera.Input_RightRot.Assign(CastleInput_RightRot.Shortcut, false);
    Camera.Input_UpRotate.Assign(CastleInput_UpRotate.Shortcut, false);
    Camera.Input_DownRotate.Assign(CastleInput_DownRotate.Shortcut, false);
    Camera.Input_GravityUp.Assign(CastleInput_GravityUp.Shortcut, false);
  end;

  if GameWin then
  begin
    { PreferGravityUpXxx should be ignored actually, because rotations
      don't work now. }
    Camera.PreferGravityUpForMoving := true;
    Camera.PreferGravityUpForRotations := false;

    { No need to do MakeClear now on any inputs, as we already set
      Input := []. }

    Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
    Camera.HeadBobbing := 0.0;
    Camera.PreferredHeight := Camera.Radius * 1.01;

    Camera.MoveHorizontalSpeed := LevelMoveHorizontalSpeed;
    Camera.MoveVerticalSpeed := LevelMoveVerticalSpeed;

    if Level <> nil then
      Level.Input_PointingDeviceActivate.MakeClear;
  end else
  if Dead then
  begin
    Camera.PreferGravityUpForMoving := true;
    { This is the only case when PreferGravityUpForRotations := false
      is sensible. }
    Camera.PreferGravityUpForRotations := false;

    Camera.Input_Jump.MakeClear;
    Camera.Input_Crouch.MakeClear;
    Camera.Input_UpMove.MakeClear;
    Camera.Input_DownMove.MakeClear;

    Camera.Input_Forward.MakeClear;
    Camera.Input_Backward.MakeClear;
    Camera.Input_LeftStrafe.MakeClear;
    Camera.Input_RightStrafe.MakeClear;

    Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
    Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
    Camera.HeadBobbing := 0.0;
    Camera.PreferredHeight := Camera.Radius * 1.01;

    Camera.MoveHorizontalSpeed := LevelMoveHorizontalSpeed;
    Camera.MoveVerticalSpeed := LevelMoveVerticalSpeed;

    if Level <> nil then
      Level.Input_PointingDeviceActivate.MakeClear;
  end else
  begin
    if FlyingMode then
    begin
      Camera.PreferGravityUpForMoving := false;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.MakeClear;
      Camera.Input_Crouch.MakeClear;
      Camera.Input_UpMove.Assign(CastleInput_UpMove.Shortcut, false);
      Camera.Input_DownMove.Assign(CastleInput_DownMove.Shortcut, false);

      { Camera.HeadBobbing and
        Camera.PreferredHeight and
        Camera.FallingDownStartSpeed and
        Camera.FallingDownSpeedIncrease
        ... don't matter here, because Gravity is false. }

      Camera.MoveHorizontalSpeed := LevelMoveHorizontalSpeed;
      Camera.MoveVerticalSpeed := LevelMoveVerticalSpeed;
    end else
    if Swimming <> psNo then
    begin
      Camera.PreferGravityUpForMoving := false;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.MakeClear;
      Camera.Input_Crouch.MakeClear;
      Camera.Input_UpMove.Assign(CastleInput_UpMove.Shortcut, false);
      Camera.Input_DownMove.Assign(CastleInput_DownMove.Shortcut, false);

      Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed / 6;
      Camera.FallingDownSpeedIncrease := 1.0;
      Camera.HeadBobbing := 0.0;
      Camera.PreferredHeight := Camera.Radius * 1.01;

      Camera.MoveHorizontalSpeed := LevelMoveHorizontalSpeed / 2;
      Camera.MoveVerticalSpeed := LevelMoveVerticalSpeed / 2;
    end else
    begin
      Camera.PreferGravityUpForMoving := true;
      Camera.PreferGravityUpForRotations := true;

      Camera.Input_Jump.Assign(CastleInput_UpMove.Shortcut, false);
      Camera.Input_Crouch.Assign(CastleInput_DownMove.Shortcut, false);
      Camera.Input_UpMove.MakeClear;
      Camera.Input_DownMove.MakeClear;

      Camera.FallingDownStartSpeed := DefaultFallingDownStartSpeed;
      Camera.FallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
      Camera.HeadBobbing := DefaultHeadBobbing;
      Camera.PreferredHeight := LevelCameraPreferredHeight;

      Camera.MoveHorizontalSpeed := LevelMoveHorizontalSpeed;
      Camera.MoveVerticalSpeed := LevelMoveVerticalSpeed;
    end;

    Camera.Input_Forward.Assign(CastleInput_Forward.Shortcut, false);
    Camera.Input_Backward.Assign(CastleInput_Backward.Shortcut, false);
    Camera.Input_LeftStrafe.Assign(CastleInput_LeftStrafe.Shortcut, false);
    Camera.Input_RightStrafe.Assign(CastleInput_RightStrafe.Shortcut, false);

    if Level <> nil then
      Level.Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);
  end;
end;

procedure TPlayer.AllocatedFootstepsSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = AllocatedFootstepsSource);
  AllocatedFootstepsSource.OnUsingEnd := nil;
  AllocatedFootstepsSource := nil;
  FootstepsSoundPlaying := stNone;
end;

procedure TPlayer.AllocatedSwimmingChangeSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = AllocatedSwimmingChangeSource);
  AllocatedSwimmingChangeSource.OnUsingEnd := nil;
  AllocatedSwimmingChangeSource := nil;
end;

procedure TPlayer.AllocatedSwimmingSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = AllocatedSwimmingSource);
  AllocatedSwimmingSource.OnUsingEnd := nil;
  AllocatedSwimmingSource := nil;
end;

procedure TPlayer.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

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
    if Level.AboveWaterBox.PointInside(Camera.Position) then
      Swimming := psAboveWater else
    if Level.WaterBox.PointInside(Camera.Position) then
      Swimming := psUnderWater else
      Swimming := psNo;

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
              Notifications.Show('You''re drowning');
            SwimLastDrownTime := Level.AnimationTime;
            Life := Life - (5 + Random(10));
            SoundEngine.Sound(stPlayerDrowning);
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
        AllocatedSwimmingSource := SoundEngine.Sound(stPlayerSwimming);
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
    if Camera.IsOnTheGround then
    begin
      ReallyIsOnTheGroundTime := Level.AnimationTime;
      IsOnTheGround := true;
      GroundRule := TextureRules.GroundRule(Ground);
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
          SoundEngine.Sound(stPlayerLavaPain);
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
      Camera.IsWalkingOnTheGround can change quite rapidly
      (when player quickly presses and releases up/down keys,
      or when he're walking up the stairs, or when he's walking
      on un-flat terrain --- then Camera.IsWalkingOnTheGround
      switches between @true and @false quite often).
      But it is undesirable to change FootstepsSoundPlaying
      so often, as this causes footsteps to suddenly stop, then play,
      then stop again etc. --- this doesn't sound good.

      So I use ReallyWalkingOnTheGroundTime to mark myself
      the time when Camera.IsWalkingOnTheGround was true.
      In normal situation I would set NewFootstepsSoundPlaying to stNone
      when Camera.IsWalkingOnTheGround = @false.
      But now I set NewFootstepsSoundPlaying to stNone only if
      Camera.IsWalkingOnTheGround = @false
      for at least TimeToChangeFootstepsSoundPlaying seconds. }

    { calculate NewFootstepsSoundPlaying }
    if Level = nil then
      NewFootstepsSoundPlaying := stNone else
    if Camera.IsWalkingOnTheGround then
    begin
      ReallyWalkingOnTheGroundTime := Level.AnimationTime;
      { Since Camera.IsWalkingOnTheGroundm then for sure
        Camera.IsOnTheGround, so UpdateIsOnTheGround updated
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
        AllocatedFootstepsSource := SoundEngine.Sound(NewFootstepsSoundPlaying, false);
        if AllocatedFootstepsSource <> nil then
        begin
          { Lower the position, to be on our feet. }
          AllocatedFootstepsSource.Position := Vector3Single(0, 0, -1.0);
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

const
  BlackOutSpeed = 2.0;
begin
  inherited;
  if FlyingMode then
  begin
    FFlyingModeTimeOut := FFlyingModeTimeOut - CompSpeed;
    if not FlyingMode then
    begin
      Notifications.Show('You''re no longer flying');
    end;
  end;

  UpdateCamera;

  UpdateSwimming;

  if BlackOutIntensity > 0 then
    BlackOutIntensity -= BlackOutSpeed * CompSpeed;

  if Attacking and (not ActualAttackDone) and (Level.AnimationTime -
    AttackStartTime >= EquippedWeaponKind.ActualAttackTime) then
  begin
    ActualAttackDone := true;
    EquippedWeaponKind.ActualAttack(EquippedWeapon, World);
  end;

  if not HintEscapeKeyShown then
  begin
    HintEscapeKeyShown := true;
    Notifications.Show('Hint: press "Escape" for game menu');
  end;

  UpdateIsOnTheGround;
  UpdateLava;
  UpdateFootstepsSoundPlaying;
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

procedure TPlayer.FalledDown(Camera: TWalkCamera;
  const FallenHeight: Single);
begin
  if (Swimming = psNo) and (FallenHeight > 4.0) then
  begin
    SoundEngine.Sound(stPlayerFalledDown);
    if FallenHeight > Camera.MaxJumpDistance * 1.5 then
      Life := Life - Max(0, FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));
  end;
end;

procedure TPlayer.SetLifeCustomBlackOut(const Value: Single;
  const Color: TVector3Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    Notifications.Show('You die');
    SoundEngine.Sound(stPlayerDies);
    Camera.FallOnTheGround;
  end else
  if (Life - Value) > 1 then
  begin
    BlackOut(Color);
    SoundEngine.Sound(stPlayerSuddenPain);
  end;
  inherited SetLife(Value);
end;

procedure TPlayer.SetLife(const Value: Single);
begin
  SetLifeCustomBlackOut(Value, Red3Single);
end;

procedure TPlayer.Attack;
begin
  if not Attacking then
  begin
    if EquippedWeapon <> nil then
    begin
      SoundEngine.Sound(EquippedWeaponKind.SoundAttackStart);
      AttackStartTime := Level.AnimationTime;
      Attacking := true;
      ActualAttackDone := false;
    end else
      { TODO: maybe I should allow him to do some "punch" / "kick" here ? }
      Notifications.Show('No weapon equipped');
  end;
end;

function TPlayer.EquippedWeaponKind: TItemWeaponKind;
begin
  Result := TItemWeaponKind(EquippedWeapon.Kind);
end;

function TPlayer.PositionSector: TSceneSector;
begin
  Result := Level.Sectors.SectorWithPoint(Camera.Position);
end;

procedure TPlayer.InputChanged(InputConfiguration: TInputConfiguration);
begin
  UpdateCamera;
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
        AllocatedSwimmingChangeSource := SoundEngine.Sound(stPlayerSwimmingChange);
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
      Camera.CancelFallingDown;
    end;

    FSwimming := Value;

    if Swimming = psUnderWater then
    begin
      SwimBeginTime := Level.AnimationTime;
      SwimLastDrownTime := 0.0;
      SwimLastSoundTime := 0.0;
    end;

    { Although UpdateCamera will be called in nearest Player.Idle anyway,
      I want to call it *now*. That's because I want to set
      Camera.FallingDownStartSpeed to low speed (suitable for moving
      under the water) before next falling down will happen.
      Why ? See comments about Camera.CancelFallingDown above.

      And next falling down will happen... actually SetSwimming
      is called from OnMatrixChanged that may be called
      from TryFallingDown ! So next falling down definitely *can*
      happen before next Player.Idle. Actually we may be in the middle
      of falling down right now. Fortunately Camera.Idle
      and Camera.CancelFallingDown are implemented (or rather fixed :)
      to honour calling CancelFallingDown and setting FallingDownStartSpeed now.

      So the safeguard below is needed. }
    UpdateCamera;
  end;
end;

procedure TPlayer.LoadFromFile;
var
  PlayerConfig: TCastleConfig;
begin
  PlayerConfig := TCastleConfig.Create(nil);
  try
    PlayerConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'player.xml';

    KnockBackSpeed :=
      PlayerConfig.GetFloat('player/knock_back_speed', DefaultKnockBackSpeed);
    Camera.MaxJumpHeight :=
      PlayerConfig.GetFloat('player/jump/max_height',
      DefaultMaxJumpHeight);
    Camera.JumpSpeedMultiply :=
      PlayerConfig.GetFloat('player/jump/speed_multiply',
      DefaultJumpSpeedMultiply);
    Camera.JumpPower :=
      PlayerConfig.GetFloat('player/jump/power',
      DefaultJumpPower);
    Camera.HeadBobbingTime :=
      PlayerConfig.GetFloat('player/head_bobbing_time',
      DefaultHeadBobbingTime);
    SickProjectionSpeed := PlayerConfig.GetFloat('player/sick_projection_speed',
      10.0);

    FResources.LoadResources(PlayerConfig.PathElement('player'));
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

  Attacking := false;
end;

function TPlayer.Ground: PTriangle;
begin
  Result := PTriangle(Camera.AboveGround);
end;

{TODO: this should disappear, we should depend on T3DOrient doing this}
procedure TPlayer.Translate(const T: TVector3Single);
begin
  Camera.Position := Camera.Position + T;
end;

function TPlayer.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const LineOfSight: boolean): boolean;
begin
  if LineOfSight then
    { Player box is collidable (creatures cannot enter on player),
      but is not visible, so LineOfSight ignores it.
      This allows creatures to see player's middle point. }
    Result := false else
    Result := inherited;
end;

function TPlayer.Sphere(out Radius: Single): boolean;
begin
  Result := true;
  Radius := Camera.Radius;
end;

function TPlayer.GetChild: T3D;
var
  AttackTime: Single;
  AttackAnim: TCastlePrecalculatedAnimation;
begin
  Result := nil;
  if (EquippedWeapon <> nil) and
    EquippedWeaponKind.Prepared then
  begin
    AttackAnim := EquippedWeaponKind.AttackAnimation;
    AttackTime := Level.AnimationTime - AttackStartTime;
    if Attacking and (AttackTime <= AttackAnim.TimeEnd) then
    begin
      Result := AttackAnim.SceneFromTime(AttackTime);
    end else
    begin
      { turn off Attacking, if AttackTime passed }
      Attacking := false;
      { although current weapons animations are just static,
        we use Level.AnimationTime to enable weapon animation
        (weapon swaying) in the future. }
      Result :=  EquippedWeaponKind.ReadyAnimation.SceneFromTime(
        Level.AnimationTime);
    end;
  end;

  if Result <> nil then
    Result.CastShadowVolumes := false; { they look bad for our weapon models }
end;

procedure TPlayer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FEquippedWeapon) then
    FEquippedWeapon := nil;
end;

function TPlayer.Height(const APosition, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  { instead of allowing inherited to do the work (and allow other stuff
    like items and creatures to stand on player's head), for now just
    make player non-collidable for Height. Otherwise, when trying really
    hard to walk "into" a creature, sometimes the creature may start to
    raise up and stand on your own (player's) head.
    This doesn't have any adverse effects for castle, after all
    items can fall down through us (they are immediately picked up then),
    and creature never fall down because of gravity on our head. }

  Result := false;
  AboveHeight := MaxSingle;
  AboveGround := nil;
end;

{ CastleWindow open / close ------------------------------------------------------ }

procedure WindowOpen(Window: TCastleWindowBase);

  function PlayerControlFileName(const BaseName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'player_controls' + PathDelim + BaseName;
  end;

  function LoadPlayerControlToDisplayList(const BaseName: string): TGLuint;
  begin
    Result := LoadImageToDisplayList(
      PlayerControlFileName(BaseName),
      { We want alpha channel, and we expect it to be recorded in the file }
      [TRGBAlphaImage], [ilcAlphaAdd], 0, 0);
  end;

begin
  GLList_BlankIndicatorImage := LoadPlayerControlToDisplayList('blank.png');
  GLList_RedIndicatorImage := LoadPlayerControlToDisplayList('red.png');
  GLList_BlueIndicatorImage := LoadPlayerControlToDisplayList('blue.png');
  GLList_BossIndicatorImage := LoadPlayerControlToDisplayList('boss.png');

  GLList_DrawWaterRect := glGenListsCheck(1, 'CastlePlayer.WindowOpen');
  glNewList(GLList_DrawWaterRect, GL_COMPILE);
  try
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT);
      glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_SRC_ALPHA);
        glColorv(Vector4Single(0, 0, 0.1, 0.5));
        glRectf(0, 0, Window.Width, Window.Height);
      glDisable(GL_BLEND);
    glPopAttrib;
  finally glEndList; end;
end;

procedure WindowClose(Window: TCastleWindowBase);
begin
  glFreeDisplayList(GLList_BlankIndicatorImage);
  glFreeDisplayList(GLList_RedIndicatorImage);
  glFreeDisplayList(GLList_BlueIndicatorImage);
  glFreeDisplayList(GLList_BossIndicatorImage);
end;

initialization
  Window.OnOpenList.Add(@WindowOpen);
  Window.OnCloseList.Add(@WindowClose);
end.
