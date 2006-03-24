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

unit CastleCreatures;

interface

uses VectorMath, VRMLGLAnimation, Boxes3d, KambiClassUtils, KambiUtils,
  VRMLGLAnimationInfo, VRMLFlatSceneGL;

{$define read_interface}

type
  TCreatureKind = class
  private
    FFlying: boolean;
  public
    constructor Create;

    { Prepare anything needed when starting new game.
      It can call Progress.Step PrepareRenderSteps times. }
    procedure PrepareRender; virtual;

    function PrepareRenderSteps: Cardinal; virtual;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;

    { If @true, then the creature flies. Otherwise it always tries to move only
      horizontally (which means that Direction is always orthogonal
      to Level.HomeCameraUp), and it falls down when Position is above
      the ground. }
    property Flying: boolean read FFlying default false;
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

    FStandAnimationInfo: TVRMLGLAnimationInfo;
    FStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
    FWalkAnimationInfo: TVRMLGLAnimationInfo;
    FAttackAnimationInfo: TVRMLGLAnimationInfo;
    FDyingAnimationInfo: TVRMLGLAnimationInfo;

    FAttacksWhenWalking: boolean;
    FMoveSpeed: Single;
    FCameraRadius: Single;
  public
    constructor Create(
      AStandAnimationInfo: TVRMLGLAnimationInfo;
      AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
      AWalkAnimationInfo: TVRMLGLAnimationInfo;
      AAttackAnimationInfo: TVRMLGLAnimationInfo;
      ADyingAnimationInfo: TVRMLGLAnimationInfo);

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
      If AttacksWhenWalking then beginning and end of it must glue
      with frame 0 of WalkAnimation.
      Else beginning and end of it must glue
      with frame 0 of StandAnimation. }
    property AttackAnimation: TVRMLGLAnimation read FAttackAnimation;

    { This is an animation of dying.
      Beginning must be on time 0.
      Beginning should *more-or-less* look like any point of the stand/attack/walk
      animations. Note that we can display this animation infinitely,
      so it must work good after Time > it's TimeEnd. }
    property DyingAnimation: TVRMLGLAnimation read FDyingAnimation;

    { See @link(AttackAnimation). }
    property AttacksWhenWalking: boolean
      read FAttacksWhenWalking write FAttacksWhenWalking default false;

    { This is moving speed --- how much Direction vector will be scaled
      when moving in wasWalk. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMoveSpeed;

    { Camera radius when moving. Initialized in PrepareRender,
      from StandAnimation.Scenes[0].BoundingBox. You can adjust it. }
    property CameraRadius: Single read FCameraRadius write FCameraRadius;
  end;

  TCreature = class
  private
    FKind: TCreatureKind;
    FLegsPosition: TVector3Single;
    FDirection: TVector3Single;
    FLife: Single;
    FMaxLife: Single;

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
  public
    { Constructor. Note for AnimationTime: usually I will take
      AnimationTime from global Level.AnimationTime, but in the case of
      constructor it's safer to just take it as param. }
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single);

    { Current Life. Initially set from MaxLife.
      TODO: check when setting Life, optionally then change state to dying
      for WalkAttackCreature. }
    property Life: Single read FLife write FLife;

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
      should be done using HeadPosition, and then appropriately translated
      back to LegsPosition. Why ? Because this avoids the problems
      of collisions with ground objects. Legs are (for creatures that
      are not Flying and have already fallen down on the ground) on the
      same level as the ground, so checking collisions versus LegsPosition
      is always vulnerable to accidentaly finding collision between LegsPosition
      and the ground. }
    function HeadPosition: TVector3Single;

    { Direction the creature is facing.
      It always must be normalized. }
    property Direction: TVector3Single read FDirection write FDirection;
  end;

  TObjectsListItem_1 = TCreature;
  {$I objectslist_1.inc}
  TCreaturesList = class(TObjectsList_1)
    procedure Render(const Frustum: TFrustum);
    procedure Idle(const CompSpeed: Single);
  end;

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack, wasDying);

  { This is TCreature that has a kind always of TWalkAttackCreatureKind. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TWalkAttackCreatureState;
    procedure SetState(Value: TWalkAttackCreatureState);

    { last FState change time, taken from Level.AnimationTime. }
    StateChangeTime: Single;
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
  end;

var
  CreaturesKinds: TCreaturesKindsList;

  Alien: TWalkAttackCreatureKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, OpenGLh, CastleWindow, GLWindow,
  VRMLNodes, KambiFilesUtils, KambiGLUtils, ProgressUnit, CastlePlay;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create;
begin
  inherited Create;
  CreaturesKinds.Add(Self);
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

  { I'm turning UseDescribePosition to false, because it's confusing for
    the user (because each creature is conted as PrepareRenderSteps steps. }
  Progress.UseDescribePosition := false;
  try
    Progress.Init(PrepareRenderSteps, 'Loading creatures');
    try
      for I := 0 to High do
        Items[I].PrepareRender;
    finally Progress.Fini; end;
  finally Progress.UseDescribePosition := true; end;
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(
  AStandAnimationInfo: TVRMLGLAnimationInfo;
  AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
  AWalkAnimationInfo: TVRMLGLAnimationInfo;
  AAttackAnimationInfo: TVRMLGLAnimationInfo;
  ADyingAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create;

  FStandAnimationInfo := AStandAnimationInfo;
  FStandToWalkAnimationInfo := AStandToWalkAnimationInfo;
  FWalkAnimationInfo := AWalkAnimationInfo;
  FAttackAnimationInfo := AAttackAnimationInfo;
  FDyingAnimationInfo := ADyingAnimationInfo;

  FAttacksWhenWalking := false;
  MoveSpeed := DefaultMoveSpeed;
  FFlying := false;
end;

destructor TWalkAttackCreatureKind.Destroy;
begin
  FreeAndNil(FStandAnimation);
  FreeAndNil(FStandToWalkAnimation);
  FreeAndNil(FWalkAnimation);
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FDyingAnimation);

  FreeAndNil(FStandAnimationInfo);
  FreeAndNil(FStandToWalkAnimationInfo);
  FreeAndNil(FWalkAnimationInfo);
  FreeAndNil(FAttackAnimationInfo);
  FreeAndNil(FDyingAnimationInfo);

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

var
  Box: TBox3d;
begin
  CreateIfNeeded(FStandAnimation      , FStandAnimationInfo      );
  CreateIfNeeded(FStandToWalkAnimation, FStandToWalkAnimationInfo);
  CreateIfNeeded(FWalkAnimation       , FWalkAnimationInfo       );
  CreateIfNeeded(FAttackAnimation     , FAttackAnimationInfo     );
  CreateIfNeeded(FDyingAnimation      , FDyingAnimationInfo      );

  Box := StandAnimation.Scenes[0].BoundingBox;

  FCameraRadius := Sqrt(Max(Max(
    VectorLenSqr(Vector2Single(Box[0, 0], Box[0, 1])),
    VectorLenSqr(Vector2Single(Box[1, 0], Box[0, 1])),
    VectorLenSqr(Vector2Single(Box[1, 0], Box[1, 1]))),
    VectorLenSqr(Vector2Single(Box[0, 0], Box[1, 1]))));
end;

function TWalkAttackCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := 10;
end;

procedure TWalkAttackCreatureKind.CloseGL;
begin
  inherited;
  if StandAnimation <> nil then StandAnimation.CloseGL;
  if StandToWalkAnimation <> nil then StandToWalkAnimation.CloseGL;
  if WalkAnimation <> nil then WalkAnimation.CloseGL;
  if AttackAnimation <> nil then AttackAnimation.CloseGL;
  if DyingAnimation <> nil then DyingAnimation.CloseGL;
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

procedure TCreature.Idle(const CompSpeed: Single);
begin
  { TODO: When not Flying, gravity should drag monsters down.
    Falling down should cause them some life loss. }
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
  FState := Value;
  StateChangeTime := Level.AnimationTime;
end;

procedure TWalkAttackCreature.Idle(const CompSpeed: Single);

  { Is attack allowed. Assumes that creature sees the player. }
  function AttackAllowed: boolean;
  begin
    { TODO: from last attack more than MinDelayBetweenAttacks lasted,
      and Player.Navigator.CameraPos is closer than AttackDistance }
    Result := false;
  end;

  function SeesPlayer: boolean;
  begin
    Result := Level.LineOfSight(HeadPosition, Player.Navigator.CameraPos);
  end;

  procedure DoWalk;
  var
    PlayerBoundingBox: TBox3d;

    { TODO: do also collision detection when *player* moves. }
    function PlayerVSCreatureCollision(const AssumeHeadPosition: TVector3Single):
      boolean;
    var
      AssumeLegsPosition: TVector3Single;
    begin
      AssumeLegsPosition := AssumeHeadPosition;
      AssumeLegsPosition[2] -= Height;
      Result := Boxes3dCollision(
        BoundingBoxAssuming(AssumeLegsPosition, Direction), PlayerBoundingBox);
    end;

  const
    AngleRadChangeSpeed = 0.1;
    MaxAngleToMoveForward = Pi / 3 { 60 degrees };
  var
    OldHeadPosition, NewHeadPosition, ProposedNewHeadPosition: TVector3Single;
    DirectionToPlayer: TVector3Single;
    AngleRadBetweenGoodDirection, AngleRadChange: Single;
  begin
    { calculate DirectionToPlayer }
    DirectionToPlayer := VectorSubtract(Player.Navigator.CameraPos, HeadPosition);
    if not WAKind.Flying then
      MakeVectorsOrthoOnTheirPlane(DirectionToPlayer, Level.HomeCameraUp);

    PlayerBoundingBox := Player.BoundingBox;

    { If creature is ideally under/above the player and if creature can't
      move vertically, then there is no way to get to the player.
      So there is no sense in moving or rotating creature now. }
    if (not WAKind.Flying) and
       { I initially wanted to check this like
         VectorsParallel(DirectionToPlayer, Level.HomeCameraUp)
         (before "fixing" DirectionToPlayer to be horizontal).
         But this was not a good solution --- DirectionToPlayer, like HeadPosition,
         is too "sensitive" to rounding errors, and VectorsParallel was returning
         false when I wanted it to return true.
         Using Boxes3dXYCollision below is a better solution --- faster,
         and it's not sensitive to any float inaccuracy. }
       Boxes3dXYCollision(BoundingBox, PlayerBoundingBox) then
    begin
      SetState(wasStand);
      Exit;
    end;

    AngleRadBetweenGoodDirection := AngleRadBetweenVectors(DirectionToPlayer,
      Direction);

    { If AngleRadBetweenGoodDirection is too large, there is not much point
      in moving in given direction anyway. We should just change our Direction. }
    if AngleRadBetweenGoodDirection < MaxAngleToMoveForward then
    begin
      OldHeadPosition := HeadPosition;
      ProposedNewHeadPosition := VectorAdd(OldHeadPosition,
        VectorScale(Direction, WAKind.MoveSpeed * CompSpeed));

      if Level.MoveAllowed(OldHeadPosition, ProposedNewHeadPosition,
        NewHeadPosition, false, WAKind.CameraRadius) and
        (not PlayerVSCreatureCollision(NewHeadPosition)) then
      begin
        FLegsPosition := NewHeadPosition;
        FLegsPosition[2] -= Height;
      end else
      begin
        { TODO: Seek alt way here, instead of just giving up. }
        SetState(wasStand);
        Exit;
      end;
    end;

    if not VectorsParallel(DirectionToPlayer, Direction) then
    begin
      { Rotate Direction, to be closer to DirectionToPlayer }

      { calculate AngleRadChange }
      AngleRadChange := AngleRadChangeSpeed * CompSpeed;
      MinTo1st(AngleRadChange, AngleRadBetweenGoodDirection);

      Direction := RotatePointAroundAxisRad(AngleRadChange, Direction,
        VectorProduct(Direction, DirectionToPlayer));

      { From time to time it's good to fix Direction, to make sure it's
        1. normalized,
        2. and orthogonal to HomeCameraUp if not Flying
        Otherwise rounding errors could accumulate and cause some nasty things.

        Actually, I didn't observe anything bad caused by the above,
        but I'm safeguarding anyway, for safety. }
      if not Kind.Flying then
        MakeVectorsOrthoOnTheirPlane(Direction, Level.HomeCameraUp);
      NormalizeTo1st(Direction);
    end;

    if not SeesPlayer then
      SetState(wasStand) else
    if AttackAllowed then
      SetState(wasAttack);
  end;

begin
  { TODO: as you see, actually AttacksWhenWalking is not used when going
    to wasAttack state. It's used only when going out from wasAttack state.
    Fix, or change docs. }

  inherited;

  case FState of
    wasStand:
      if SeesPlayer then
      begin
        if AttackAllowed then
          SetState(wasAttack) else
          SetState(wasWalk);
      end;
    wasWalk: DoWalk;
    wasAttack:
      begin
        { TODO: at some time when attacking, something should happen:
          - maybe some missile should be created (somewhere between
            HeadPosition and LegsPosition. Missile has Direction.)
          - maybe player should lose some life (in this case, I should
            check once again here that player is close enough) }

        if StateChangeTime > WAKind.AttackAnimation.TimeEnd then
          if WAKind.AttacksWhenWalking then
            SetState(wasWalk) else
            SetState(wasStand);
      end;
    wasDying: ;
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
      { TODO: transition from walk to stand smooth }
    wasAttack:
      { TODO: transition from walk/stand to attack and back smooth }
      Result := WAKind.AttackAnimation.SceneFromTime(StateTime);
    wasDying:
      Result := WAKind.DyingAnimation.SceneFromTime(StateTime);
    else raise EInternalError.Create('FState ?');
  end;
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
      [ AlienFileName('alien_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false)
      { TODO -- dying animation }
    );
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