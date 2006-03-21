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

uses VectorMath, VRMLGLAnimation, Boxes3d, KambiClassUtils, KambiUtils;

{$define read_interface}

type
  TCreatureKind = class
  public
    constructor Create;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;
  end;

  { This is a TCreatureKind that has simple states:
    standing stil, walking (aka running), performing an attack and dying. }
  TWalkAttackCreatureKind = class(TCreatureKind)
  private
    FStandAnimation: TVRMLGLAnimation;
    FStandToWalkAnimation: TVRMLGLAnimation;
    FWalkAnimation: TVRMLGLAnimation;
    FAttackAnimation: TVRMLGLAnimation;
    FDyingAnimation: TVRMLGLAnimation;
    FAttacksWhenWalking: boolean;
  public
    constructor Create(
      AStandAnimation: TVRMLGLAnimation;
      AStandToWalkAnimation: TVRMLGLAnimation;
      AWalkAnimation: TVRMLGLAnimation;
      AAttackAnimation: TVRMLGLAnimation;
      ADyingAnimation: TVRMLGLAnimation);

    destructor Destroy; override;

    procedure CloseGL; override;

    { This is an animation of standing still.
      Beginning and end of it must glue together. }
    property StandAnimation: TVRMLGLAnimation read FStandAnimation;

    { This is an animation when he changes from standing still to walking.
      It's frame 0 must glue with frame 0 of StandAnimation,
      it's last frame must glue with frame 0 of WalkAnimation. }
    property StandToWalkAnimation: TVRMLGLAnimation read FStandToWalkAnimation;

    { This is an animation of walking.
      Beginning and end of it must glue together. }
    property WalkAnimation: TVRMLGLAnimation read FWalkAnimation;

    { This is an animation of attacking.
      If AttacksWhenWalking then beginning and end of it must glue
      with frame 0 of WalkAnimation.
      Else beginning and end of it must glue
      with frame 0 of StandAnimation. }
    property AttackAnimation: TVRMLGLAnimation read FAttackAnimation;

    { This is an animation of dying. }
    property DyingAnimation: TVRMLGLAnimation read FDyingAnimation;

    { See @link(AttackAnimation). }
    property AttacksWhenWalking: boolean
      read FAttacksWhenWalking write FAttacksWhenWalking default false;
  end;

  TCreature = class
  private
    FKind: TCreatureKind;
    FPosition: TVector3Single;
    FDirection: TVector3Single;
    FLife: Single;
    FMaxLife: Single;
  public
    constructor Create(AKind: TCreatureKind;
      const APosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single);

    { Current Life. Initially set from MaxLife.
      TODO: check when setting Life, optionally then change state to dying
      for WalkAttackCreature. }
    property Life: Single read FLife write FLife;

    property MaxLife: Single read FMaxLife write FMaxLife;

    property Kind: TCreatureKind read FKind;

    function BoundingBox: TBox3d; virtual; abstract;

    procedure Render(const Frustum: TFrustum); virtual; abstract;

    procedure Idle(const CompSpeed: Single); virtual; abstract;

    { This is the position of the (0, 0, 0) point of creature model
      (or rather, currently used model! Creatures are animated after all). }
    property Position: TVector3Single read FPosition write FPosition;

    { Direction the creature is facing. }
    property Direction: TVector3Single read FDirection write FDirection;
  end;

  TObjectsListItem_1 = TCreature;
  {$I objectslist_1.inc}
  TCreaturesList = class(TObjectsList_1)
    procedure Render(const Frustum: TFrustum);
    procedure Idle(const CompSpeed: Single);
  end;

  TWalkAttackCreatureState = (wasStand, wasStandToWalk, wasWalk,
    wasAttack, wasDying);

  { This is TCreature that has a kind always of TWalkAttackCreatureKind. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TWalkAttackCreatureState;
  public
    constructor Create(AKind: TCreatureKind;
      const APosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single);

    property State: TWalkAttackCreatureState read FState
      default wasStand;

    function BoundingBox: TBox3d; override;

    procedure Render(const Frustum: TFrustum); override;

    procedure Idle(const CompSpeed: Single); override;
  end;

var
  Alien: TWalkAttackCreatureKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, OpenGLh, CastleWindow, GLWindow, VRMLFlatSceneGL,
  VRMLNodes, KambiFilesUtils, KambiGLUtils;

{$define read_implementation}
{$I objectslist_1.inc}

var
  CreaturesKinds: TList;

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create;
begin
  inherited Create;
  CreaturesKinds.Add(Self);
end;

procedure TCreatureKind.CloseGL;
begin
  { Nothing to do in this class. }
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(
  AStandAnimation: TVRMLGLAnimation;
  AStandToWalkAnimation: TVRMLGLAnimation;
  AWalkAnimation: TVRMLGLAnimation;
  AAttackAnimation: TVRMLGLAnimation;
  ADyingAnimation: TVRMLGLAnimation);
begin
  inherited Create;
  FStandAnimation := AStandAnimation;
  FStandToWalkAnimation := AStandToWalkAnimation;
  FWalkAnimation := AWalkAnimation;
  FAttackAnimation := AAttackAnimation;
  FDyingAnimation := ADyingAnimation;
end;

destructor TWalkAttackCreatureKind.Destroy;
begin
  FreeAndNil(FStandAnimation);
  FreeAndNil(FStandToWalkAnimation);
  FreeAndNil(FWalkAnimation);
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FDyingAnimation);
  inherited;
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
  const APosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single);
begin
  inherited Create;

  FKind := AKind;
  FPosition := APosition;
  FDirection := ADirection;
  FMaxLife := AMaxLife;

  FLife := MaxLife;
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
  const APosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single);
begin
  inherited Create(AKind, APosition, ADirection, AMaxLife);
  FState := wasStand;
end;

function TWalkAttackCreature.BoundingBox: TBox3d;
begin
  { TODO } Result := EmptyBox3d;
end;

procedure TWalkAttackCreature.Render(const Frustum: TFrustum);
begin
  if { TODO FrustumBox3dCollisionPossibleSimple(Frustum, BoundingBox) } true then
  begin
    glPushMatrix;
      glTranslatev(Position);
      { TODO: take into account Direction }
      { TODO: choose animation type wisely (basing on State),
        and animation time wisely (helping with Idle) }
      TWalkAttackCreatureKind(Kind).StandAnimation.Scenes[0].Render(nil);
    glPopMatrix;
  end;
end;

procedure TWalkAttackCreature.Idle(const CompSpeed: Single);
begin
  { TODO }
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
  AnimScenesPerTime = 100;
  AnimOptimization = roSceneAsAWhole;

  function LoadCreatureModel(const FileName: string): TVRMLNode;
  begin
    Result := ParseVRMLFile(ProgramDataPath + 'data' + PathDelim +
      'creatures' + PathDelim + FileName, false);
  end;

  function LoadAlienModel(const FileName: string): TVRMLNode;
  begin
    Result := LoadCreatureModel('alien' + PathDelim + FileName);
  end;

begin
  Glw.OnCloseList.AppendItem(@GLWindowClose);

  CreaturesKinds := TList.Create;

  Alien := TWalkAttackCreatureKind.Create(
    TVRMLGLAnimation.Create(
      [ LoadAlienModel('alien_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization),
    TVRMLGLAnimation.Create(
      [ LoadAlienModel('alien_still_final.wrl'),
        LoadAlienModel('alien_walk_1_final.wrl') ],
      [ 0, 1 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimation.Create(
      [ LoadAlienModel('alien_walk_1_final.wrl'),
        LoadAlienModel('alien_walk_2_final.wrl') ],
      [ 0, 1 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimation.Create(
      [ LoadAlienModel('alien_still_final.wrl'),
        LoadAlienModel('alien_attack_2_final.wrl'),
        LoadAlienModel('alien_attack_1_final.wrl'),
        LoadAlienModel('alien_still_final.wrl') ],
      [ 0, 1, 2, 3 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimation.Create(
      [ LoadAlienModel('alien_still_final.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization)
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