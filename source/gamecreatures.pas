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
unit GameCreatures;

interface

uses Classes, VectorMath, PrecalculatedAnimation, CastleClassUtils, CastleUtils,
  CastleScene, CastleResources, CastleXMLConfig, Base3D, X3DNodes,
  CastleCreatures;

type
  TAlienCreatureKind = class(TWalkAttackCreatureKind)
  public
    function CreatureClass: TCreatureClass; override;
  end;

  TWerewolfKind = class(TWalkAttackCreatureKind)
  public
    function CreatureClass: TCreatureClass; override;
  end;

  TSpiderKind = class(TWalkAttackCreatureKind)
  public
    function CreatureClass: TCreatureClass; override;
  end;

  TSpiderQueenKind = class(TWalkAttackCreatureKind)
  private
    FThrowWebAttackAnimation: TCastlePrecalculatedAnimation;
    FThrowWebAttackAnimationFile: string;

    FMinDelayBetweenThrowWebAttacks: Single;
    FMaxThrowWebAttackDistance: Single;
    FMaxAngleToThrowWebAttack: Single;
    FActualThrowWebAttackTime: Single;
  protected
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
      const DoProgress: boolean); override;
    function PrepareCoreSteps: Cardinal; override;
    procedure ReleaseCore; override;
  public
    function CreatureClass: TCreatureClass; override;

    property MinDelayBetweenThrowWebAttacks: Single
      read FMinDelayBetweenThrowWebAttacks
      write FMinDelayBetweenThrowWebAttacks default 0;

    property MaxThrowWebAttackDistance: Single
      read FMaxThrowWebAttackDistance
      write FMaxThrowWebAttackDistance default 0;

    property MaxAngleToThrowWebAttack: Single
      read FMaxAngleToThrowWebAttack
      write FMaxAngleToThrowWebAttack default 0;

    property ActualThrowWebAttackTime: Single
      read FActualThrowWebAttackTime
      write FActualThrowWebAttackTime default 0;

    property ThrowWebAttackAnimation: TCastlePrecalculatedAnimation
      read FThrowWebAttackAnimation;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TGhostKind = class(TWalkAttackCreatureKind)
  public
    function CreatureClass: TCreatureClass; override;
  end;

  TAlienCreature = class(TWalkAttackCreature)
  public
    procedure ActualAttack; override;
  end;

  TWerewolfCreature = class(TWalkAttackCreature)
  private
    NextHowlTime: Single;
  public
    constructor Create(AOwner: TComponent; const AMaxLife: Single); override;

    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    procedure Howl(ForceHowl: boolean);
  end;

  TSpiderCreature = class(TWalkAttackCreature)
  public
    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  TSpiderQueenCreature = class(TWalkAttackCreature)
  private
    LastThrowWebAttackTime: Single;
    ActualThrowWebAttackDone: boolean;
    function SQKind: TSpiderQueenKind;
    procedure ActualThrowWebAttack;
  protected
    procedure SetState(Value: TWalkAttackCreatureState); override;
    function GetChild: T3D; override;
  public
    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  TGhostCreature = class(TWalkAttackCreature)
  protected
    procedure SetState(Value: TWalkAttackCreatureState); override;
  public
    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

var
  Werewolf: TWerewolfKind;
  BallMissile: TMissileCreatureKind;
  Ghost: TGhostKind;
  Spider: TSpiderKind;
  SpiderQueen: TSpiderQueenKind;
  ThrownWeb: TMissileCreatureKind;
  Arrow: TMissileCreatureKind;

procedure CreaturesKindsInit;

implementation

uses SysUtils, DOM, GL, GLU, CastleWindow, CastleFilesUtils, CastleGLUtils,
  ProgressUnit, GameSound;

{ TAlienCreatureKind --------------------------------------------------- }

function TAlienCreatureKind.CreatureClass: TCreatureClass;
begin
  Result := TAlienCreature;
end;

{ TWerewolfKind -------------------------------------------------------------- }

function TWerewolfKind.CreatureClass: TCreatureClass;
begin
  Result := TWerewolfCreature;
end;

{ TSpiderKind -------------------------------------------------------------- }

function TSpiderKind.CreatureClass: TCreatureClass;
begin
  Result := TSpiderCreature;
end;

{ TSpiderQueenKind -------------------------------------------------------- }

procedure TSpiderQueenKind.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
begin
  inherited;
  PreparePrecalculatedAnimation(FThrowWebAttackAnimation, FThrowWebAttackAnimationFile, BaseLights, DoProgress);
end;

function TSpiderQueenKind.PrepareCoreSteps: Cardinal;
begin
  Result := (inherited PrepareCoreSteps) + 2;
end;

procedure TSpiderQueenKind.ReleaseCore;
begin
  FThrowWebAttackAnimation := nil;
  inherited;
end;

function TSpiderQueenKind.CreatureClass: TCreatureClass;
begin
  Result := TSpiderQueenCreature;
end;

procedure TSpiderQueenKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  MinDelayBetweenThrowWebAttacks :=
    KindsConfig.GetFloat('throw_web/min_delay_between_attacks', 0.0);
  MaxThrowWebAttackDistance :=
    KindsConfig.GetFloat('throw_web/max_attack_distance', 0.0);
  MaxAngleToThrowWebAttack :=
    KindsConfig.GetFloat('throw_web/max_angle_to_attack', 0.0);
  ActualThrowWebAttackTime :=
    KindsConfig.GetFloat('throw_web/actual_attack_time', 0.0);

  FThrowWebAttackAnimationFile := KindsConfig.GetFileName('throw_web_attack_animation');
end;

{ TGhostKind ------------------------------------------------------------- }

function TGhostKind.CreatureClass: TCreatureClass;
begin
  Result := TGhostCreature;
end;

{ TAlienCreature ------------------------------------------------------- }

procedure TAlienCreature.ActualAttack;
const
  FiringMissileHeight = 0.6;
var
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3Single;
begin
  if HasLastSeenPlayer then
  begin
    MissilePosition := LerpLegsMiddle(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := BallMissile.CreateCreature(World, MissilePosition, MissileDirection);
    Missile.Sound3d(stBallMissileFired, 0.0);
  end;
end;

{ TWerewolfCreature ---------------------------------------------------------- }

constructor TWerewolfCreature.Create(AOwner: TComponent; const AMaxLife: Single);
begin
  inherited;
  NextHowlTime := Random * 60.0;
end;

procedure TWerewolfCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    Sound3d(stWerewolfActualAttackHit, 1.0);
    AttackHurt;
  end;
end;

procedure TWerewolfCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if (not Dead) and (LifeTime > NextHowlTime) then
    Howl(false);
end;

procedure TWerewolfCreature.Howl(ForceHowl: boolean);
begin
  { Howl only if player was seen, and only while walking/standing
    (not in the middle of attack e.g., since that would sound stupid). }
  if ForceHowl or (HasLastSeenPlayer and (State in [wasWalk, wasStand])) then
    Sound3d(stWerewolfHowling, 1.0);

  { Whether you actually howled or not, schedule next howl. }
  NextHowlTime := LifeTime + Random * 60.0;
end;

{ TSpiderCreature ---------------------------------------------------------- }

procedure TSpiderCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    Sound3d(stSpiderActualAttackHit, 1.0);
    AttackHurt;
  end;
end;

procedure TSpiderCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { Spiders must be removed from level, otherwise too many of them
    stay on Cages level and slow down the rendering.
    Dying animation scales the model down, to make it gradually disappear. }
  if (State = wasDying) and
    (LifeTime - StateChangeTime > WAKind.DyingAnimation.TimeEnd) then
    RemoveMe := rtRemoveAndFree;
end;

{ TSpiderQueenCreature ---------------------------------------------------- }

function TSpiderQueenCreature.SQKind: TSpiderQueenKind;
begin
  Result := TSpiderQueenKind(Kind);
end;

procedure TSpiderQueenCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    Sound3d(stSpiderQueenActualAttackHit, 1.0);
    AttackHurt;
  end;
end;

procedure TSpiderQueenCreature.SetState(Value: TWalkAttackCreatureState);
begin
  if (State <> Value) and (Value = wasSpecial1) then
  begin
    LastThrowWebAttackTime := LifeTime;
    ActualThrowWebAttackDone := false;
  end;

  inherited;
end;

procedure TSpiderQueenCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);

  procedure DoMaybeSwitchToThrowWebAttack;
  var
    ThrowWebAttackAllowed: boolean;
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    ThrowWebAttackAllowed :=
      IdleSeesPlayer and
      (LifeTime - LastThrowWebAttackTime >
        SQKind.MinDelayBetweenThrowWebAttacks) and
      (IdleSqrDistanceToLastSeenPlayer <=
        Sqr(SQKind.MaxThrowWebAttackDistance));

    if ThrowWebAttackAllowed then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        LastSeenPlayer - Middle, Direction);
      ThrowWebAttackAllowed := AngleRadBetweenTheDirectionToPlayer <=
        SQKind.MaxAngleToThrowWebAttack;

      if ThrowWebAttackAllowed then
        SetState(wasSpecial1);
    end;
  end;

  procedure DoThrowWebAttack;
  var
    StateTime: Single;
  begin
    StateTime := LifeTime - StateChangeTime;

    if (not ActualThrowWebAttackDone) and
       (StateTime >= SQKind.ActualThrowWebAttackTime) then
    begin
      ActualThrowWebAttackDone := true;
      ActualThrowWebAttack;
    end;

    if StateTime > SQKind.ThrowWebAttackAnimation.TimeEnd then
      { wasStand will quickly change to wasWalk if it will want to walk. }
      SetState(wasStand);
  end;

begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if not Dead then
    case State of
      wasStand, wasWalk: DoMaybeSwitchToThrowWebAttack;
      wasSpecial1: DoThrowWebAttack;
    end;
end;

function TSpiderQueenCreature.GetChild: T3D;
var
  StateTime: Single;
begin
  if not Kind.Prepared then Exit(nil);

  if State = wasSpecial1 then
  begin
    { Time from the change to this state. }
    StateTime := LifeTime - StateChangeTime;

    Result := SQKind.ThrowWebAttackAnimation.SceneFromTime(StateTime);
  end else
    Result := inherited;

  { self-shadows on creatures look bad, esp. see werewolves at the end
    of "castle hall" level. Changing XxxShadowVolumes here
    is a little hacky (would be cleaner to do it at loading), but easy. }
  TCastleScene(Result).ReceiveShadowVolumes := false;
end;

procedure TSpiderQueenCreature.ActualThrowWebAttack;
const
  FiringMissileHeight = 0.6;
var
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3Single;
begin
  if HasLastSeenPlayer then
  begin
    MissilePosition := LerpLegsMiddle(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := ThrownWeb.CreateCreature(World, MissilePosition, MissileDirection);
    Missile.Sound3d(stThrownWebFired, 0.0);
  end;
end;

{ TGhostCreature ---------------------------------------------------------- }

procedure TGhostCreature.SetState(Value: TWalkAttackCreatureState);
begin
  inherited;

  { Ghosts dead animation is quite unique, so we will not check
    collisions with ghost when it's in dying state.
    Ghost is blended anyway, so checking for collisions with him
    is not really necessary anyway. }
  if Value = wasDying then Collides := false;
end;

procedure TGhostCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    AttackHurt;
  end;
end;

procedure TGhostCreature.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if (State = wasDying) and
    (LifeTime - StateChangeTime > WAKind.DyingAnimation.TimeEnd) then
    RemoveMe := rtRemoveAndFree;
end;

{ initialization / finalization ---------------------------------------------- }

procedure CreaturesKindsInit;
begin
  Werewolf := Resources.FindName('Werewolf') as TWerewolfKind;
  BallMissile := Resources.FindName('BallMissile') as TMissileCreatureKind;
  Ghost := Resources.FindName('Ghost') as TGhostKind;
  Spider := Resources.FindName('Spider') as TSpiderKind;
  SpiderQueen := Resources.FindName('SpiderQueen') as TSpiderQueenKind;
  ThrownWeb := Resources.FindName('ThrownWeb') as TMissileCreatureKind;
  Arrow := Resources.FindName('Arrow') as TMissileCreatureKind;
end;

procedure DoInitialization;
begin
  RegisterResourceClass(TAlienCreatureKind, 'Alien');
  RegisterResourceClass(TWerewolfKind, 'Werewolf');
  RegisterResourceClass(TMissileCreatureKind, 'Missile');
  RegisterResourceClass(TGhostKind, 'Ghost');
  RegisterResourceClass(TSpiderKind, 'Spider');
  RegisterResourceClass(TSpiderQueenKind, 'SpiderQueen');
  RegisterResourceClass(TStillCreatureKind, 'Still');
end;

initialization
  DoInitialization;
end.
