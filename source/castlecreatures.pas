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
unit CastleCreatures;

interface

uses VectorMath, VRMLGLAnimation, Boxes3d, KambiClassUtils, KambiUtils,
  VRMLGLAnimationInfo, VRMLFlatSceneGL, CastleSound;

{$define read_interface}

type
  TCreatureKind = class
  private
    FFlying: boolean;
    FCameraRadius: Single;
    FSoundSuddenPain: TSoundType;
  public
    constructor Create;

    { Prepare anything needed when starting new game.
      It can call Progress.Step PrepareRenderSteps times.
      In this class, PrepareRender initializes CameraRadius from CurrentScene
      (so you may need to call "inherited" at the *end* in subclasses). }
    procedure PrepareRender; virtual;

    function PrepareRenderSteps: Cardinal; virtual;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;

    { If @true, then the creature flies. Otherwise it always tries to move only
      horizontally (which means that Direction is always orthogonal
      to Level.HomeCameraUp), and it falls down when Position is above
      the ground. }
    property Flying: boolean read FFlying write FFlying default false;

    { Camera radius when moving. By default it's 0.
      You should initialize it to something larger, for collision detection.
      You can do it in PrepareRender. }
    property CameraRadius: Single read FCameraRadius write FCameraRadius
      default 0;

    property SoundSuddenPain: TSoundType
      read FSoundSuddenPain write FSoundSuddenPain default stNone;
  end;

  TObjectsListItem_2 = TCreatureKind;
  {$I objectslist_2.inc}
  TCreaturesKindsList = class(TObjectsList_2)
    { Calls PrepareRender for all items.
      This does Progress.Init, Step, Fini. }
    procedure PrepareRender;
  end;

const
  DefaultMoveSpeed = 0.2;
  DefaultMinDelayBetweenAttacks = 5.0;
  DefaultAttackDistance = 50.0 * 0.7;
  DefaultMissileMoveSpeed = 1.0 * 0.7;
  DefaultMaxKnockedBackDistance = 6.0 * 0.7;

type
  { This is a TCreatureKind that has simple states:
    standing stil, walking (aka running), performing an attack and dying.
    Note that you should specify all animation times in seconds
    (just like Level.AnimationTime). }
  TWalkAttackCreatureKind = class(TCreatureKind)
  private
    FStandAnimation: TVRMLGLAnimation;
    FStandToWalkAnimation: TVRMLGLAnimation;
    FWalkAnimation: TVRMLGLAnimation;
    FAttackAnimation: TVRMLGLAnimation;
    FDyingAnimation: TVRMLGLAnimation;
    FHurtAnimation: TVRMLGLAnimation;

    FStandAnimationInfo: TVRMLGLAnimationInfo;
    FStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
    FWalkAnimationInfo: TVRMLGLAnimationInfo;
    FAttackAnimationInfo: TVRMLGLAnimationInfo;
    FDyingAnimationInfo: TVRMLGLAnimationInfo;
    FHurtAnimationInfo: TVRMLGLAnimationInfo;

    FMoveSpeed: Single;
    FMinDelayBetweenAttacks: Single;
    FAttackDistance: Single;
    FActualAttackTime: Single;
    FMaxKnockedBackDistance: Single;

    FSoundAttackStart: TSoundType;
  public
    constructor Create(
      AStandAnimationInfo: TVRMLGLAnimationInfo;
      AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
      AWalkAnimationInfo: TVRMLGLAnimationInfo;
      AAttackAnimationInfo: TVRMLGLAnimationInfo;
      ADyingAnimationInfo: TVRMLGLAnimationInfo;
      AHurtAnimationInfo: TVRMLGLAnimationInfo);

    destructor Destroy; override;

    { Make all TVRMLGLAnimation properties non-nil. I.e. load them from their
      XxxInfo counterparts. }
    procedure PrepareRender; override;

    function PrepareRenderSteps: Cardinal; override;

    procedure CloseGL; override;

    { This is an animation of standing still.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property StandAnimation: TVRMLGLAnimation read FStandAnimation;

    { This is an animation when he changes from standing still to walking.
      Beginning must be on time 0.
      It's beginnig must glue with beginning of StandAnimation,
      it's ending must glue with beginning of WalkAnimation. }
    property StandToWalkAnimation: TVRMLGLAnimation read FStandToWalkAnimation;

    { This is an animation of walking.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property WalkAnimation: TVRMLGLAnimation read FWalkAnimation;

    { This is an animation of attacking.
      Beginning must be on time 0.
      Beginning and end of it should roughly glue with frames WalkAnimation
      and StandAnimation.

      I used to have here property like AttacksWhenWalking for the creature,
      to indicate whether creature changes state like
      "wasWalk -> wasAttack -> wasWalk" or
      "wasStand -> wasAttack -> wasStand". But this wasn't good.
      Intelligent creature sometimes attacks when walking (e.g. if it just
      made the distance to the player closer) or when standing
      (when the distance was already close enough). And after performing
      the attack, the creature doesn't need to go back to state
      before the attack. }
    property AttackAnimation: TVRMLGLAnimation read FAttackAnimation;

    { This is an animation of dying.
      Beginning must be on time 0.
      Beginning should *more-or-less* look like any point of the stand/attack/walk
      animations. Note that we can display this animation infinitely,
      so it must work good after Time > it's TimeEnd. }
    property DyingAnimation: TVRMLGLAnimation read FDyingAnimation;

    { Animation when the creature will be hurt.
      Beginning must be on time 0.
      Beginning and end should *more-or-less* look like
      any point of the stand/attack/walk animations.
      Note that this animation will not loop, it will be played
      for TimeDurationWithBack time. }
    property HurtAnimation: TVRMLGLAnimation read FHurtAnimation;

    { This is moving speed --- how much Direction vector will be scaled
      when moving in wasWalk. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMoveSpeed;

    { Minimum delay between one attack and the other, in seconds.
      Note that actually setting this to 0 doesn't do much ---
      because minumum delay will still be bounded by the duration
      of AttackAnimation. }
    property MinDelayBetweenAttacks: Single
      read FMinDelayBetweenAttacks write FMinDelayBetweenAttacks
      default DefaultMinDelayBetweenAttacks;

    { Maximum distance between player and creature to allow creature
      to start attack. More precisely, this is measured between
      Player.Navigator.CameraPos and creature's HeadPosition. }
    property AttackDistance: Single
      read FAttackDistance write FAttackDistance
      default DefaultAttackDistance;

    { This is the time point within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime default 0.0;

    { When this creature is knocked back by something (i.e. goes to wasHurt
      state), then it's forced to move in general LastAttackDirection
      for the time HurtAnimation.TimeDurationWithBack, until
      HurtAnimation (and wasHurt state) ends, or until it was forced to move
      by MaxKnockedBackDistance. }
    property MaxKnockedBackDistance: Single
      read FMaxKnockedBackDistance write FMaxKnockedBackDistance
      default DefaultMaxKnockedBackDistance;

    { This will be played at HeadPosition when entering wasAttack state.
      Sometimes you may prefer to rather play a sound at ActualAttack
      --- then just do it in overriden ActualAttack. }
    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;
  end;

  { This is a missile. As you can see, this is also treated as a creature
    --- it's just a very dumb creature, that just moves into the given
    direction and explodes on any collision. }
  TMissileCreatureKind = class(TCreatureKind)
  private
    FAnimation: TVRMLGLAnimation;
    FAnimationInfo: TVRMLGLAnimationInfo;
    FMoveSpeed: Single;
    FSoundExplosion: TSoundType;
  public
    constructor Create(AAnimationInfo: TVRMLGLAnimationInfo);
    destructor Destroy; override;

    procedure PrepareRender; override;
    function PrepareRenderSteps: Cardinal; override;
    procedure CloseGL; override;

    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TVRMLGLAnimation read FAnimation;

    { This is moving speed --- how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMissileMoveSpeed;

    { Missile must be generally considered as Flying, otherwise
      it doesn't have much sense... Don't set Flying to false
      for this class (because TMissileCreature may depend on it and never
      cares to keep Direction horizontal). }
    property Flying default true;

    property SoundExplosion: TSoundType
      read FSoundExplosion write FSoundExplosion default stNone;
  end;

  TCreature = class
  private
    FKind: TCreatureKind;
    FLegsPosition: TVector3Single;
    FDirection: TVector3Single;
    FLife: Single;
    FMaxLife: Single;
    FLastAttackDirection: TVector3Single;
    procedure SetLastAttackDirection(const Value: TVector3Single);

    { For gravity work. }
    FallingDownStartHeight: Single;
    FIsFallingDown: boolean;
  protected
    { Return matrix that takes into account current LegsPosition and Direction.
      Multiply CurrentScene geometry by this matrix to get current geometry. }
    function SceneTransform: TMatrix4Single;

    { Like SceneTransform, but assumes that LegsPosition and Direction
      is as specified. }
    function SceneTransformAssuming(
      const AssumeLegsPosition, AssumeDirection: TVector3Single): TMatrix4Single;

    { Returns BoundingBox, assuming that LegsPosition and Direction are
      as specified here. }
    function BoundingBoxAssuming(
      const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3d;

    { This checks collision with Player.BoundingBox, assuming that HeadPosition
      (and implied LegsPosition) is as given. }
    function HeadCollisionWithPlayer(
      const AssumeHeadPosition: TVector3Single): boolean;

    function LegsCollisionWithPlayer(
      const AssumeLegsPosition: TVector3Single): boolean;

    procedure SetLife(const Value: Single); virtual;
  public
    { Constructor. Note for AnimationTime: usually I will take
      AnimationTime from global Level.AnimationTime, but in the case of
      constructor it's safer to just take it as param. }
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single);

    { Current Life. Initially set from MaxLife. }
    property Life: Single read FLife write SetLife;

    property MaxLife: Single read FMaxLife write FMaxLife;

    property Kind: TCreatureKind read FKind;

    function BoundingBox: TBox3d; virtual;

    procedure Render(const Frustum: TFrustum); virtual;

    procedure Idle(const CompSpeed: Single); virtual;

    function CurrentScene: TVRMLFlatSceneGL; virtual; abstract;

    { This is the position of the (0, 0, 0) point of creature model
      (or rather, currently used model! Creatures are animated after all). }
    property LegsPosition: TVector3Single read FLegsPosition write FLegsPosition;

    { Just a shortcut for CurrentScene.BoundingBox[1, 2].
      Note that while CurrentScene may change, this also may change. }
    function Height: Single;

    { This is the position of the head. Actually, it's the place where
      (0, 0, Height) point of creatures bounding box currently is
      (i.e., translated by LegsPosition).

      Note that all collision detection (MoveAllowed, GetCameraHeight)
      when not Flying
      should be done using HeadPosition, and then appropriately translated
      back to LegsPosition. Why ? Because this avoids the problems
      of collisions with ground objects. Legs are (for creatures that
      are not Flying and have already fallen down on the ground) on the
      same level as the ground, so checking collisions versus LegsPosition
      is always vulnerable to accidentaly finding collision between LegsPosition
      and the ground.

      For Flying creatures this is not a problem, they can use LegsPosition
      surrounded by CameraRadius for collision detection. }
    function HeadPosition: TVector3Single;

    { Direction the creature is facing.
      It always must be normalized. }
    property Direction: TVector3Single read FDirection write FDirection;

    { When this function returns true, the creature will be removed
      from Level.Creatures list (this will be done in game's OnIdle,
      right before calling Idle of all the creatures).

      In this class this returns always false. Yes, this means that
      even dead creatures *may* still exist on level (e.g. to
      show DyingAnimation, or missile's explosion animation etc.) }
    function RemoveMeFromLevel: boolean; virtual;

    { Shortcut for Life <= 0. }
    function Dead: boolean;

    { Each time you decrease life of this creature, set this property.
      This is the direction from where the attack came.
      You can set this to (0, 0, 0) if there was no specific direction
      of attack.

      On set, this vector will be normalized. }
    property LastAttackDirection: TVector3Single
      read FLastAttackDirection write SetLastAttackDirection;
  end;

  TObjectsListItem_1 = TCreature;
  {$I objectslist_1.inc}
  TCreaturesList = class(TObjectsList_1)
    procedure Render(const Frustum: TFrustum);
    procedure Idle(const CompSpeed: Single);
    { Remove from this list all creatures that return
      RemoveMeFromLevel = @true. }
    procedure RemoveFromLevel;
  end;

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack, wasDying, wasHurt);

  { This is TCreature that has a kind always of TWalkAttackCreatureKind. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TWalkAttackCreatureState;
    procedure SetState(Value: TWalkAttackCreatureState);

    { last FState change time, taken from Level.AnimationTime. }
    StateChangeTime: Single;

    { time of last FState change to wasAttack, taken from Level.AnimationTime. }
    LastAttackTime: Single;
    { Set to true each time you enter wasAttack, set back to false
      if ActualAttack was called. }
    ActualAttackDone: boolean;

    { Set to 0 each time FState changes to wasHurt }
    KnockedBackDistance: Single;

    HasLastSeenPlayer: boolean;
    LastSeenPlayer: TVector3Single;
  protected
    procedure SetLife(const Value: Single); override;
  public
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single);

    { Shortcut for TWalkAttackCreatureKind(Kind). }
    function WAKind: TWalkAttackCreatureKind;

    property State: TWalkAttackCreatureState read FState
      default wasStand;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TVRMLFlatSceneGL; override;

    { This is the method where you must actually do your attack
      --- fire a missile, lower player's life etc.

      This happens in the middle of AttackAnimation,
      see also ActualAttackTime. Of course you should use
      current creature LegsPosition, HeadPosition, Direction
      etc. to determine things like missile starting position
      and direction.

      If creature is doing some short-range attack
      you can also just lower here player's Life. Remember in this
      case to check that player is close enough; in general situation,
      you can't depend that player is still within AttackDistance
      --- if ActualAttackTime is large, then player had some time
      to back off between AttackAnimation was started and ActualAttack
      is called. }
    procedure ActualAttack; virtual; abstract;
  end;

  TBallThrowerCreature = class(TWalkAttackCreature)
  public
    procedure ActualAttack; override;
  end;

  TWerewolfCreature = class(TWalkAttackCreature)
  public
    procedure Idle(const CompSpeed: Single); override;
    procedure ActualAttack; override;
  end;

  { This is TCreature that has a kind always of TMissileCreatureKind. }
  TMissileCreature = class(TCreature)
  private
    procedure ExplodeCore;
    procedure ExplodeWithPlayer;
    procedure ExplodeWithLevel;
  public
    { Shortcut for TMissileCreatureKind(Kind). }
    function MissileKind: TMissileCreatureKind;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TVRMLFlatSceneGL; override;

    function RemoveMeFromLevel: boolean; override;
  end;

var
  CreaturesKinds: TCreaturesKindsList;

  Alien: TWalkAttackCreatureKind;
  Werewolf: TWalkAttackCreatureKind;
  BallMissile: TMissileCreatureKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, OpenGLh, CastleWindow, GLWindow,
  VRMLNodes, KambiFilesUtils, KambiGLUtils, ProgressUnit, CastlePlay,
  CastleLevel;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create;
begin
  inherited Create;
  CreaturesKinds.Add(Self);
  FFlying := false;
end;

procedure TCreatureKind.PrepareRender;
begin
  { Nothing to do in this class. }
end;

function TCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := 0;
end;

procedure TCreatureKind.CloseGL;
begin
  { Nothing to do in this class. }
end;

{ TCreaturesKindsList -------------------------------------------------------- }

procedure TCreaturesKindsList.PrepareRender;
var
  I: Integer;
  PrepareRenderSteps: Cardinal;
begin
  PrepareRenderSteps := 0;
  for I := 0 to High do
    PrepareRenderSteps +=  Items[I].PrepareRenderSteps;

  Progress.Init(PrepareRenderSteps, 'Loading creatures');
  try
    for I := 0 to High do
      Items[I].PrepareRender;
  finally Progress.Fini; end;
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(
  AStandAnimationInfo: TVRMLGLAnimationInfo;
  AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
  AWalkAnimationInfo: TVRMLGLAnimationInfo;
  AAttackAnimationInfo: TVRMLGLAnimationInfo;
  ADyingAnimationInfo: TVRMLGLAnimationInfo;
  AHurtAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create;

  FStandAnimationInfo := AStandAnimationInfo;
  FStandToWalkAnimationInfo := AStandToWalkAnimationInfo;
  FWalkAnimationInfo := AWalkAnimationInfo;
  FAttackAnimationInfo := AAttackAnimationInfo;
  FDyingAnimationInfo := ADyingAnimationInfo;
  FHurtAnimationInfo := AHurtAnimationInfo;

  MoveSpeed := DefaultMoveSpeed;
  FMinDelayBetweenAttacks := DefaultMinDelayBetweenAttacks;
  FAttackDistance := DefaultAttackDistance;
  FMaxKnockedBackDistance := DefaultMaxKnockedBackDistance;
end;

destructor TWalkAttackCreatureKind.Destroy;
begin
  FreeAndNil(FStandAnimation);
  FreeAndNil(FStandToWalkAnimation);
  FreeAndNil(FWalkAnimation);
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FDyingAnimation);
  FreeAndNil(FHurtAnimation);

  FreeAndNil(FStandAnimationInfo);
  FreeAndNil(FStandToWalkAnimationInfo);
  FreeAndNil(FWalkAnimationInfo);
  FreeAndNil(FAttackAnimationInfo);
  FreeAndNil(FDyingAnimationInfo);
  FreeAndNil(FHurtAnimationInfo);

  inherited;
end;

procedure TWalkAttackCreatureKind.PrepareRender;

  procedure CreateIfNeeded(var Anim: TVRMLGLAnimation;
    AnimInfo: TVRMLGLAnimationInfo);
  begin
    if Anim = nil then
      Anim := AnimInfo.CreateAnimation;
    Progress.Step;
    Anim.PrepareRender(false, true);
    Progress.Step;
  end;

begin
  inherited;

  CreateIfNeeded(FStandAnimation      , FStandAnimationInfo      );
  CreateIfNeeded(FStandToWalkAnimation, FStandToWalkAnimationInfo);
  CreateIfNeeded(FWalkAnimation       , FWalkAnimationInfo       );
  CreateIfNeeded(FAttackAnimation     , FAttackAnimationInfo     );
  CreateIfNeeded(FDyingAnimation      , FDyingAnimationInfo      );
  CreateIfNeeded(FHurtAnimation       , FHurtAnimationInfo       );

  CameraRadius := Box3dXYRadius(StandAnimation.Scenes[0].BoundingBox);
end;

function TWalkAttackCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 12;
end;

procedure TWalkAttackCreatureKind.CloseGL;
begin
  inherited;
  if StandAnimation <> nil then StandAnimation.CloseGL;
  if StandToWalkAnimation <> nil then StandToWalkAnimation.CloseGL;
  if WalkAnimation <> nil then WalkAnimation.CloseGL;
  if AttackAnimation <> nil then AttackAnimation.CloseGL;
  if DyingAnimation <> nil then DyingAnimation.CloseGL;
  if HurtAnimation <> nil then HurtAnimation.CloseGL;
end;

{ TMissileCreatureKind ---------------------------------------------------- }

constructor TMissileCreatureKind.Create(
  AAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create;
  FAnimationInfo := AAnimationInfo;
  Flying := true;
  FMoveSpeed := DefaultMissileMoveSpeed;
end;

destructor TMissileCreatureKind.Destroy;
begin
  FreeAndNil(FAnimation);
  FreeAndNil(FAnimationInfo);
  inherited;
end;

procedure TMissileCreatureKind.PrepareRender;

  procedure CreateIfNeeded(var Anim: TVRMLGLAnimation;
    AnimInfo: TVRMLGLAnimationInfo);
  begin
    if Anim = nil then
      Anim := AnimInfo.CreateAnimation;
    Progress.Step;
    Anim.PrepareRender(false, true);
    Progress.Step;
  end;

begin
  inherited;
  CreateIfNeeded(FAnimation, FAnimationInfo);
  CameraRadius := Box3dXYRadius(Animation.Scenes[0].BoundingBox);
end;

function TMissileCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TMissileCreatureKind.CloseGL;
begin
  inherited;
  if Animation <> nil then Animation.CloseGL;
end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AKind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single;
  const AnimationTime: Single);
begin
  inherited Create;

  FKind := AKind;
  FLegsPosition := ALegsPosition;
  FDirection := ADirection;
  FMaxLife := AMaxLife;

  FLife := MaxLife;
end;

function TCreature.Height: Single;
begin
  Result := CurrentScene.BoundingBox[1, 2];
end;

function TCreature.HeadPosition: TVector3Single;
begin
  Result := LegsPosition;
  Result[2] += Height;
end;

function TCreature.SceneTransformAssuming(
  const AssumeLegsPosition, AssumeDirection: TVector3Single): TMatrix4Single;
var
  GoodCameraUp: TVector3Single;
begin
  GoodCameraUp := UnitVector3Single[2];
  { If not Flying, then we know that GoodCameraUp is already
    orthogonal to AssumeDirection. }
  if Kind.Flying then
    MakeVectorsOrthoOnTheirPlane(GoodCameraUp, AssumeDirection);

  { Note that actually I could do here TransformToCoordsNoScaleMatrix,
    as obviously I don't want any scaling. But in this case I know
    that AssumeDirection length = 1 and GoodCameraUp = 1 (so their product
    length is also = 1), so no need to do
    TransformToCoordsNoScaleMatrix here (and I can avoid wasting my time
    on Sqrts needed inside TransformToCoordsNoScaleMatrix). }

  Result := TransformToCoordsMatrix(AssumeLegsPosition,
    AssumeDirection, VectorProduct(GoodCameraUp, AssumeDirection), GoodCameraUp);
end;

function TCreature.SceneTransform: TMatrix4Single;
begin
  Result := SceneTransformAssuming(LegsPosition, Direction);
end;

function TCreature.BoundingBoxAssuming(
  const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3d;
begin
  Result := BoundingBoxTransform(CurrentScene.BoundingBox,
    SceneTransformAssuming(AssumeLegsPosition, AssumeDirection));
end;

function TCreature.BoundingBox: TBox3d;
begin
  Result := BoundingBoxAssuming(LegsPosition, Direction);
end;

procedure TCreature.Render(const Frustum: TFrustum);
begin
  if FrustumBox3dCollisionPossibleSimple(Frustum, BoundingBox) then
  begin
    glPushMatrix;
      glMultMatrix(SceneTransform);
      CurrentScene.Render(nil);
    glPopMatrix;
  end;
end;

function TCreature.HeadCollisionWithPlayer(
  const AssumeHeadPosition: TVector3Single): boolean;
var
  AssumeLegsPosition: TVector3Single;
begin
  AssumeLegsPosition := AssumeHeadPosition;
  AssumeLegsPosition[2] -= Height;
  Result := Boxes3dCollision(
    BoundingBoxAssuming(AssumeLegsPosition, Direction), Player.BoundingBox);
end;

function TCreature.LegsCollisionWithPlayer(
  const AssumeLegsPosition: TVector3Single): boolean;
begin
  Result := Boxes3dCollision(
    BoundingBoxAssuming(AssumeLegsPosition, Direction), Player.BoundingBox);
end;

procedure TCreature.Idle(const CompSpeed: Single);

  procedure DoGravity;

    procedure FalledDown;
    var
      FallenHeight, LifeLoss: Single;
    begin
      FallenHeight := FallingDownStartHeight - LegsPosition[2];
      if FallenHeight > 1.0 then
      begin
        LifeLoss := Max(0, FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));
        Life := Life - LifeLoss;
        LastAttackDirection := ZeroVector3Single;
        { Tests: GameMessage(Format('Creature fallen down from %f, lost %f life',
          [FallenHeight, LifeLoss])); }
      end;
    end;

  var
    OldHeadPosition: TVector3Single;

    function MoveVertical(const Distance: Single): boolean;
    var
      NewHeadPosition, ProposedNewHeadPosition: TVector3Single;
    begin
      ProposedNewHeadPosition := OldHeadPosition;
      ProposedNewHeadPosition[2] += Distance;

      Result := Level.MoveAllowed(OldHeadPosition, ProposedNewHeadPosition,
        NewHeadPosition, true, Kind.CameraRadius) and
        (not HeadCollisionWithPlayer(NewHeadPosition));

      if Result then
      begin
        FLegsPosition := NewHeadPosition;
        FLegsPosition[2] -= Height;
      end;
    end;

  const
    FallingDownSpeed = 1.0;
    { Beware: GrowingUpSpeed is not only a graphical effect. Too large
      GrowingUpSpeed will allow creature to climb walls that are at high
      (almost-vertical) angle. }
    GrowingUpSpeed = 0.1;
    { HeightMargin is used to safeguard against floating point inaccuracy.
      Without this, creature would too often be considered "falling down"
      or "growing up". }
    HeightMargin = 1.01;
  var
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround, HeightAboveTheGround: Single;
    OldIsFallingDown: boolean;
  begin
    { Gravity does it's work here.
      This is extremely simplified version of Gravity work in MatrixNavigation.
      (simplified, because creature doesn't need all these effects). }

    { Note that also here we do collision detection using HeadPosition,
      not LegsPosition. See HeadPosition docs for reasoning. }

    OldIsFallingDown := FIsFallingDown;
    OldHeadPosition := HeadPosition;

    Level.GetCameraHeight(OldHeadPosition, IsAboveTheGround,
      SqrHeightAboveTheGround);
    { We will need it anyway. OK, I'll pay this Sqrt. }
    HeightAboveTheGround := Sqrt(SqrHeightAboveTheGround);

    if (not IsAboveTheGround) or
      (HeightAboveTheGround > Height * HeightMargin) then
    begin
      { Fall down }
      if not FIsFallingDown then
        FallingDownStartHeight := LegsPosition[2];

      FIsFallingDown := true;
      if not MoveVertical(-Min(FallingDownSpeed * CompSpeed,
        HeightAboveTheGround - Height)) then
        FIsFallingDown := false;
    end else
    begin
      FIsFallingDown := false;

      if IsAboveTheGround and
        (HeightAboveTheGround < Height / HeightMargin) then
      begin
        { Growing up }
        MoveVertical(Min(GrowingUpSpeed * CompSpeed,
          Height - HeightAboveTheGround));
      end;
    end;

    if OldIsFallingDown and (not FIsFallingDown) then
      FalledDown;
  end;

begin
  if not Kind.Flying then
    DoGravity;
end;

function TCreature.RemoveMeFromLevel: boolean;
begin
  Result := false;
end;

procedure TCreature.SetLife(const Value: Single);
begin
  if (Life - Value) > MaxLife / 10 then
  begin
    Sound3d(Kind.SoundSuddenPain, HeadPosition);
  end;
  FLife := Value;
end;

function TCreature.Dead: boolean;
begin
  Result := Life <= 0;
end;

procedure TCreature.SetLastAttackDirection(const Value: TVector3Single);
begin
  FLastAttackDirection := Normalized(Value);
end;

{ TCreatures ----------------------------------------------------------------- }

procedure TCreaturesList.Render(const Frustum: TFrustum);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Render(Frustum);
end;

procedure TCreaturesList.Idle(const CompSpeed: Single);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Idle(CompSpeed);
end;

procedure TCreaturesList.RemoveFromLevel;
var
  I: Integer;
begin
  for I := 0 to High do
    if Items[I].RemoveMeFromLevel then
      FreeAndNil(I);
  DeleteAll(nil);
end;

{ TWalkAttackCreature -------------------------------------------------------- }

constructor TWalkAttackCreature.Create(AKind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single;
  const AnimationTime: Single);
begin
  inherited Create(AKind, ALegsPosition, ADirection, AMaxLife, AnimationTime);
  FState := wasStand;
  StateChangeTime := AnimationTime;
end;

function TWalkAttackCreature.WAKind: TWalkAttackCreatureKind;
begin
  Result := TWalkAttackCreatureKind(Kind);
end;

procedure TWalkAttackCreature.SetState(Value: TWalkAttackCreatureState);
begin
  if FState <> Value then
  begin
    FState := Value;
    StateChangeTime := Level.AnimationTime;
    { Some states require special initialization here. }
    case FState of
      wasAttack:
        begin
          Sound3d(WAKind.SoundAttackStart, HeadPosition);
          LastAttackTime := StateChangeTime;
          ActualAttackDone := false;
        end;
      wasHurt:
        KnockedBackDistance := 0.0;
    end;
  end;
end;

procedure TWalkAttackCreature.Idle(const CompSpeed: Single);
var
  SeesPlayer: boolean;
  SqrDistanceToLastSeenPlayer: Single;

  { Is attack allowed ? }
  function AttackAllowed: boolean;
  const
    MaxAngleToAttack = Pi / 6 { 30 degrees };
  var
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    Result := SeesPlayer and
      (Level.AnimationTime - LastAttackTime > WAKind.MinDelayBetweenAttacks) and
      (SqrDistanceToLastSeenPlayer <= Sqr(WAKind.AttackDistance));

    { GameMessage(Format('dist is now %f (sqr is %f), needed sqr is %f',
      [ PointsDistance(Player.Navigator.CameraPos, HeadPosition),
        PointsDistanceSqr(Player.Navigator.CameraPos, HeadPosition),
        Sqr(WAKind.AttackDistance) ])); }

    if Result then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        VectorSubtract(LastSeenPlayer, HeadPosition),
        Direction);
      Result := AngleRadBetweenTheDirectionToPlayer <= MaxAngleToAttack;
    end;
  end;

  procedure CalculateDirectionToTarget(
    const Target: TVector3Single;
    var DirectionToTarget: TVector3Single;
    var AngleRadBetweenDirectionToTarget: Single);
  begin
    { calculate DirectionToTarget }
    DirectionToTarget := VectorSubtract(Target, HeadPosition);
    if not Kind.Flying then
      MakeVectorsOrthoOnTheirPlane(DirectionToTarget, Level.HomeCameraUp);

    { calculate AngleRadBetweenDirectionToTarget }
    AngleRadBetweenDirectionToTarget :=
      AngleRadBetweenVectors(DirectionToTarget, Direction);
  end;

  { Call this only when HasLastSeenPlayer }
  procedure CalculateDirectionToPlayer(var DirectionToPlayer: TVector3Single;
    var AngleRadBetweenDirectionToTarget: Single);
  begin
    CalculateDirectionToTarget(LastSeenPlayer,
      DirectionToPlayer, AngleRadBetweenDirectionToTarget);
  end;

  { This changes Direction to be closer to DirectionToTarget.
    Note that it requires the value of AngleRadBetweenDirectionToTarget
    effectively }
  procedure RotateDirectionToFaceTarget(const DirectionToTarget: TVector3Single;
    const AngleRadBetweenDirectionToTarget: Single);
  const
    AngleRadChangeSpeed = 0.1;
  var
    AngleRadChange: Single;
  begin
    if not VectorsParallel(DirectionToTarget, Direction) then
    begin
      { Rotate Direction, to be closer to DirectionToTarget }

      { calculate AngleRadChange }
      AngleRadChange := AngleRadChangeSpeed * CompSpeed;
      MinTo1st(AngleRadChange, AngleRadBetweenDirectionToTarget);

      Direction := RotatePointAroundAxisRad(AngleRadChange, Direction,
        VectorProduct(Direction, DirectionToTarget));

      { From time to time it's good to fix Direction, to make sure it's
        1. normalized,
        2. and orthogonal to HomeCameraUp if not Flying
        Otherwise rounding errors could accumulate and cause some nasty things.

        Actually, I didn't observe anything bad caused by the above,
        but I'm safeguarding anyway, for safety. }
      if not Kind.Flying then
        MakeVectorsOrthoOnTheirPlane(FDirection, Level.HomeCameraUp);
      NormalizeTo1st(FDirection);
    end;
  end;

  { Is it wanted to get closer to the LastSeenPlayer ?
    And (if it's wanted) is it sensible to do this by moving
    along current Direction ?
    Call this only if HasLastSeenPlayer. }
  function WantToWalk(const AngleRadBetweenDirectionToPlayer: Single): boolean;
  const
    MaxAngleToMoveForward = Pi / 3 { 60 degrees };
  begin
    Result :=
      { Is it wanted to get closer to the LastSeenPlayer ?

        Yes --- only if it will help make AttackAllowed from false to true.
        See AttackAllowed implementation.

        If SeesPlayer and SqrDistanceToLastSeenPlayer is small enough,
        there's no point in getting closer to the player. In fact, it would
        be bad to get closer to player in this case, as this would allow
        player to easier attack (shorter distance --- easier to reach with
        short-range weapon, or easier to aim with long-range weapon). }
      ( (not SeesPlayer) or
        (SqrDistanceToLastSeenPlayer > Sqr(WAKind.AttackDistance))
      ) and

      { If AngleRadBetweenDirectionToPlayer is too large, there is not much point
        in moving in given direction anyway. We should just change our Direction. }
      (AngleRadBetweenDirectionToPlayer <= MaxAngleToMoveForward) and

      { If creature is ideally under/above the player and if creature can't
        move vertically, then there is no way to get to the player.
        So there is no sense in moving or rotating creature now. }
      ( WAKind.Flying or
        { I initially wanted to check this like
          VectorsParallel(DirectionToPlayer, Level.HomeCameraUp)
          (before "fixing" DirectionToPlayer to be horizontal).
          But this was not a good solution --- DirectionToPlayer, like HeadPosition,
          is too "sensitive" to rounding errors, and VectorsParallel was returning
          false when I wanted it to return true.
          Using Boxes3dXYCollision below is a better solution --- faster,
          and it's not sensitive to any float inaccuracy. }
        (not Boxes3dXYCollision(BoundingBox,
          Player.BoundingBoxAssuming(LastSeenPlayer)))
      );
  end;

  procedure DoStand;
  var
    DirectionToPlayer: TVector3Single;
    AngleRadBetweenDirectionToPlayer: Single;
  begin
    if HasLastSeenPlayer then
    begin
      CalculateDirectionToPlayer(DirectionToPlayer, AngleRadBetweenDirectionToPlayer);

      if AttackAllowed then
        SetState(wasAttack) else
      if WantToWalk(AngleRadBetweenDirectionToPlayer) then
        SetState(wasWalk) else
      begin
        { Continue wasStand state }
        RotateDirectionToFaceTarget(DirectionToPlayer,
          AngleRadBetweenDirectionToPlayer);
      end;
    end;
  end;

  procedure DoWalk;

    { This performs the real move, which means that it changes LegsPosition
      and HeadPosition along the Direction vector.

      This doesn't check whether this is a sensible move, so use this
      only if you know that the creature really wants to go in this Direction.

      This checks only the basic (i.e. always wanted) things:
      - Collision detection (with level, player and other creatures)
      - For not Flying creatures, also the check to not fall down from high
        is done. }
    function MoveAlongTheDirection: boolean;

      { Don't be stupid, and don't walk where you see you will fall down. }
      function TooHighAboveTheGround(const NewHeadPosition: TVector3Single):
        boolean;
      const
        MaxHeightAcceptableToFall = 2.0 * 0.7;
      var
        IsAboveTheGround: boolean;
        SqrHeightAboveTheGround: Single;
      begin
        Result := false;
        if not Kind.Flying then
        begin
          Level.GetCameraHeight(NewHeadPosition, IsAboveTheGround,
            SqrHeightAboveTheGround);
          if (not IsAboveTheGround) or
            (SqrHeightAboveTheGround > Sqr(MaxHeightAcceptableToFall + Height)) then
            Result := true;
        end;
      end;

    var
      OldHeadPosition, NewHeadPosition, ProposedNewHeadPosition: TVector3Single;
    begin
      OldHeadPosition := HeadPosition;
      ProposedNewHeadPosition := VectorAdd(OldHeadPosition,
        VectorScale(Direction, WAKind.MoveSpeed * CompSpeed));

      Result :=
        { First check to not step into some deep fall.
          Note that I'm not using here NewHeadPosition
          (that will be calculated later by Level.MoveAllowed)
          or ProposedNewHeadPosition, because they are too close
          to OldHeadPosition to be good to test against.
          I'm calculating here where I would get after 0.2 second
          (WAKind.MoveSpeed * 0.2 * 50). }
        (not TooHighAboveTheGround(VectorAdd(OldHeadPosition,
          VectorScale(Direction, WAKind.MoveSpeed * 0.2 * 50)))) and

        Level.MoveAllowed(OldHeadPosition, ProposedNewHeadPosition,
          NewHeadPosition, false, Kind.CameraRadius) and
        (not HeadCollisionWithPlayer(NewHeadPosition));

      if Result then
      begin
        FLegsPosition := NewHeadPosition;
        FLegsPosition[2] -= Height;
      end;
    end;

  var
    DirectionToPlayer: TVector3Single;
    AngleRadBetweenDirectionToPlayer: Single;
  begin
    if not HasLastSeenPlayer then
    begin
      { Nowhere to go; so just stay here. }
      SetState(wasStand);
      Exit;
    end;

    CalculateDirectionToPlayer(DirectionToPlayer, AngleRadBetweenDirectionToPlayer);

    if WantToWalk(AngleRadBetweenDirectionToPlayer) then
    begin
      if not MoveAlongTheDirection then
      begin
        { TODO: Seek alt way here, instead of just giving up. }
        SetState(wasStand);
        Exit;
      end;
    end else
    begin
      { I don't want to walk anymore. So just stand stil. }
      SetState(wasStand);
      Exit;
    end;

    RotateDirectionToFaceTarget(DirectionToPlayer,
      AngleRadBetweenDirectionToPlayer);

    if AttackAllowed then
      SetState(wasAttack);
  end;

  procedure DoAttack;
  var
    StateTime: Single;
  begin
    StateTime := Level.AnimationTime - StateChangeTime;

    if (not ActualAttackDone) and (StateTime >= WAKind.ActualAttackTime) then
    begin
      ActualAttackDone := true;
      ActualAttack;
    end;

    if StateTime > WAKind.AttackAnimation.TimeEnd then
      { wasStand will quickly change to wasWalk if it will want to walk. }
      SetState(wasStand);
  end;

  procedure DoHurt;
  const
    KnockedBackSpeed = 1.0 * 0.7;
  var
    StateTime: Single;
    OldHeadPosition, ProposedNewHeadPosition, NewHeadPosition: TVector3Single;
    CurrentKnockBackDistance: Single;
  begin
    StateTime := Level.AnimationTime - StateChangeTime;

    if StateTime > WAKind.HurtAnimation.TimeDurationWithBack then
      SetState(wasStand) else
    if KnockedBackDistance <= WAKind.MaxKnockedBackDistance then
    begin
      OldHeadPosition := HeadPosition;

      { Calculate CurrentKnockBackDistance, update KnockedBackDistance }
      CurrentKnockBackDistance := KnockedBackSpeed * CompSpeed;
      if CurrentKnockBackDistance >
        WAKind.MaxKnockedBackDistance - KnockedBackDistance then
      begin
        CurrentKnockBackDistance :=
          WAKind.MaxKnockedBackDistance - KnockedBackDistance;
        KnockedBackDistance := WAKind.MaxKnockedBackDistance;
      end else
      begin
        KnockedBackDistance += CurrentKnockBackDistance;
      end;

      ProposedNewHeadPosition := VectorAdd(OldHeadPosition,
        VectorScale(LastAttackDirection, CurrentKnockBackDistance));

      if Level.MoveAllowed(OldHeadPosition, ProposedNewHeadPosition,
        NewHeadPosition, false, Kind.CameraRadius) and
        (not HeadCollisionWithPlayer(NewHeadPosition)) then
      begin
        FLegsPosition := NewHeadPosition;
        FLegsPosition[2] -= Height;
      end;
    end;
  end;

begin
  inherited;

  if Dead then
  begin
    SetState(wasDying);
    Exit;
  end;

  SeesPlayer := Level.LineOfSight(HeadPosition, Player.Navigator.CameraPos);
  if SeesPlayer then
  begin
    HasLastSeenPlayer := true;
    LastSeenPlayer := Player.Navigator.CameraPos;
  end;

  if HasLastSeenPlayer then
  begin
    SqrDistanceToLastSeenPlayer :=
      PointsDistanceSqr(LastSeenPlayer, HeadPosition);
  end;

  case FState of
    wasStand: DoStand;
    wasWalk: DoWalk;
    wasAttack: DoAttack;
    wasDying: ;
    wasHurt: DoHurt;
    else raise EInternalError.Create('FState ?');
  end;
end;

function TWalkAttackCreature.CurrentScene: TVRMLFlatSceneGL;
var
  StateTime: Single;
begin
  { Time from the change to this state. }
  StateTime := Level.AnimationTime - StateChangeTime;

  case FState of
    wasStand:
      Result := WAKind.StandAnimation.SceneFromTime(StateTime);
    wasWalk:
      if StateTime < WAKind.StandToWalkAnimation.TimeEnd then
        Result := WAKind.StandToWalkAnimation.SceneFromTime(StateTime) else
        Result := WAKind.WalkAnimation.SceneFromTime(
          StateTime - WAKind.StandToWalkAnimation.TimeEnd);
    wasAttack:
      Result := WAKind.AttackAnimation.SceneFromTime(StateTime);
    wasDying:
      Result := WAKind.DyingAnimation.SceneFromTime(StateTime);
    wasHurt:
      Result := WAKind.HurtAnimation.SceneFromTime(StateTime);
    else raise EInternalError.Create('FState ?');
  end;
end;

procedure TWalkAttackCreature.SetLife(const Value: Single);
begin
  if (not Dead) and (Value < Life) then
    SetState(wasHurt);
  inherited;
end;

{ TBallThrowerCreature ------------------------------------------------------- }

procedure TBallThrowerCreature.ActualAttack;
const
  FiringMissileHeight = 0.6;
  MissileDefaultLife = 1.0;
var
  Missile: TMissileCreature;
begin
  if HasLastSeenPlayer then
  begin
    Missile := TMissileCreature.Create(BallMissile,
      VLerp(FiringMissileHeight, LegsPosition, HeadPosition),
      Normalized(VectorSubtract(LastSeenPlayer, HeadPosition)),
      MissileDefaultLife, Level.AnimationTime);

    Level.Creatures.Add(Missile);

    Sound3d(stBallMissileFired, Missile.LegsPosition);
  end;
end;

{ TWerewolfCreature ---------------------------------------------------------- }

procedure TWerewolfCreature.ActualAttack;
begin
  if Boxes3dCollision(Box3dTranslate(BoundingBox,
    VectorScale(Direction, WAKind.AttackDistance)), Player.BoundingBox) then
  begin
    Sound3d(stWerewolfActualAttackHit, HeadPosition);
    Player.Life := Player.Life - 20 - Random(20);
  end;
end;

procedure TWerewolfCreature.Idle(const CompSpeed: Single);
begin
  inherited;

  if (FState = wasDying) and
    (Level.AnimationTime - StateChangeTime >
      WAKind.DyingAnimation.TimeEnd + 5
      { + 5 seconds, to allow player look at the dead werewolf }) then
  begin
    LevelFinished(nil);
  end;
end;

{ TMissileCreature ----------------------------------------------------------- }

function TMissileCreature.MissileKind: TMissileCreatureKind;
begin
  Result := TMissileCreatureKind(Kind);
end;

procedure TMissileCreature.Idle(const CompSpeed: Single);
var
  NewLegsPosition: TVector3Single;
begin
  inherited;

  NewLegsPosition := VectorAdd(LegsPosition,
    VectorScale(Direction, MissileKind.MoveSpeed * CompSpeed));

  if Level.MoveAllowedSimple(LegsPosition, NewLegsPosition,
    false, Kind.CameraRadius) then
  begin
    FLegsPosition := NewLegsPosition;
  end else
    ExplodeWithLevel;

  if LegsCollisionWithPlayer(LegsPosition) then
    ExplodeWithPlayer;

  { TODO: if collides with other creature, explode also there. }
end;

function TMissileCreature.CurrentScene: TVRMLFlatSceneGL;
begin
  Result := MissileKind.Animation.SceneFromTime(Level.AnimationTime);
end;

function TMissileCreature.RemoveMeFromLevel: boolean;
begin
  { TODO: do some missile explosion animation for some missiles. }
  Result := Life <= 0.0;
end;

procedure TMissileCreature.ExplodeCore;
begin
  { TODO: for some missiles, their explosion may hurt everyone around.
    So do here additional checks for collision and hurt player and creatures. }

  Sound3d(MissileKind.SoundExplosion, LegsPosition);

  Life := 0.0;

  { Actually in this case setting LastAttackDirection is not really needed
    (TMissileCreature doesn't use it anyway), but I'm doing it for
    consistency. }
  LastAttackDirection := ZeroVector3Single;
end;

procedure TMissileCreature.ExplodeWithPlayer;
begin
  ExplodeCore;
  Player.Life := Player.Life - 20 - Random(20);
end;

procedure TMissileCreature.ExplodeWithLevel;
begin
  ExplodeCore;
end;

{ initialization / finalization ---------------------------------------------- }

procedure GLWindowClose(Glwin: TGLWindow);
var
  I: Integer;
begin
  { In fact, CreaturesKinds will always be nil here, because
    GLWindowClose will be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on CreaturesKinds.Items, and this implicitly did CloseGL,
    so everything is OK. }

  if CreaturesKinds <> nil then
  begin
    for I := 0 to CreaturesKinds.Count - 1 do
      TCreatureKind(CreaturesKinds.Items[I]).CloseGL;
  end;
end;

procedure DoInitialization;
const
  AnimScenesPerTime = 30;
  AnimOptimization = roSceneAsAWhole;

  function CreatureFileName(const FileName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'creatures' + PathDelim + FileName;
  end;

  function AlienFileName(const FileName: string): string;
  begin
    Result := CreatureFileName('alien' + PathDelim + FileName);
  end;

begin
  Glw.OnCloseList.AppendItem(@GLWindowClose);

  CreaturesKinds := TCreaturesKindsList.Create;

  Alien := TWalkAttackCreatureKind.Create(
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_still_final.wrl'),
        AlienFileName('alien_walk_1_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_walk_1_final.wrl'),
        AlienFileName('alien_walk_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_still_final.wrl'),
        AlienFileName('alien_attack_2_final.wrl'),
        AlienFileName('alien_attack_1_final.wrl'),
        AlienFileName('alien_still_final.wrl') ],
      [ 0, 0.3, 0.6, 1.0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_still_final.wrl'),
        AlienFileName('alien_dying_1_final.wrl'),
        AlienFileName('alien_dying_2_final.wrl') ],
      [ 0.0, 0.1, 0.5 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ AlienFileName('alien_still_final.wrl'),
        AlienFileName('alien_dying_1_final.wrl') ],
      [ 0.0, 0.1 ],
      AnimScenesPerTime, AnimOptimization, false, true)
    );
  Alien.ActualAttackTime := 0.4;
  Alien.SoundSuddenPain := stAlienSuddenPain;

  Werewolf := TWalkAttackCreatureKind.Create(
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_walk_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_attack_1_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_attack_2_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl') ],
      [ 0, 0.3, 0.6, 1.0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_hurt_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_dead_final.wrl') ],
      [ 0.0, 0.3, 0.8 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('werewolf' + PathDelim + 'werewolf_still_final.wrl'),
        CreatureFileName('werewolf' + PathDelim + 'werewolf_hurt_final.wrl') ],
      [ 0.0, 0.3 ],
      AnimScenesPerTime, AnimOptimization, false, true)
    );
  Werewolf.ActualAttackTime := 0.5;
  Werewolf.AttackDistance := 6.0 * 0.7;
  Werewolf.MinDelayBetweenAttacks := 2.0;
  Werewolf.SoundSuddenPain := stWerewolfSuddenPain;
  Werewolf.SoundAttackStart := stWerewolfAttackStart;

  BallMissile := TMissileCreatureKind.Create(
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ball_missile' + PathDelim + 'ball_missile_1_final.wrl'),
        CreatureFileName('ball_missile' + PathDelim + 'ball_missile_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, true, false)
    );
  BallMissile.SoundExplosion := stBallMissileExplode;
end;

procedure DoFinalization;
var
  I: Integer;
begin
  for I := 0 to CreaturesKinds.Count - 1 do
    TCreatureKind(CreaturesKinds.Items[I]).Free;
  FreeAndNil(CreaturesKinds);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.