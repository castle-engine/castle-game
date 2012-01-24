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

uses Classes, VectorMath, PrecalculatedAnimation, Boxes3D, CastleClassUtils, CastleUtils,
  PrecalculatedAnimationInfo, CastleScene, GameSound, SceneWaypoints,
  GameObjectKinds, ALSoundAllocator, CastleXMLConfig, Base3D,
  XmlSoundEngine, GLShadowVolumeRenderer, Triangle, Frustum, X3DNodes,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif}, CastleColors;

const
  { Default value for TCreatureKind.DefaultMaxLife.
    Yes, it's not a typo, this identifier starts with "DefaultDefault". }
  DefaultDefaultMaxLife = 100.0;

  DefaultFlying = false;

  DefaultMoveSpeed = 0.2;
  DefaultMinDelayBetweenAttacks = 5.0;
  DefaultMaxAttackDistance = 50.0 * 0.7;
  DefaultPreferredAttackDistance = 30.0 * 0.7;
  DefaultMissileMoveSpeed = 1.0 * 0.7;
  DefaultMaxKnockedBackDistance = 6.0 * 0.7;
  DefaultLifeToRunAway = 0.3;
  DefaultActualAttackTime = 0.0;
  DefaultMaxAngleToAttack = Pi / 6 { 30 degrees };

  DefaultSoundDyingTiedToCreature = true;

  DefaultMinLifeLossToHurt = 0.0;
  DefaultChanceToHurt = 1.0;

  DefaultCloseDirectionToPlayer = false;
  DefaultCloseDirectionToTargetSpeed = 0.1;

  DefaultShortRangeAttackDamageConst = 10.0;
  DefaultShortRangeAttackDamageRandom = 10.0;
  DefaultShortRangeAttackKnockbackDistance = 0.1;

  DefaultMaxHeightAcceptableToFall = 2.0 * 0.7;
  DefaultFallDownLifeLossScale = 1.0;

  DefaultCreatureRandomWalkDistance = 10.0;

  DefaultPauseBetweenSoundIdle = 2.5;

  DefaultHitsPlayer = true;
  DefaultHitsCreatures = false;
  DefaultFallsDown = false;
  DefaultFallsDownSpeed = 0.003;

  DefaultMiddlePositionHeight = 1.0;

  DefaultCastShadowVolumes = true;

type
  TCreature = class;

  TCreatureKind = class(TObjectKind)
  private
    FFlying: boolean;
    FSoundSuddenPain: TSoundType;
    FSoundDying: TSoundType;
    FSoundDyingTiedToCreature: boolean;
    FDefaultMaxLife: Single;

    CameraRadiusFromFile: Single;

    FShortRangeAttackDamageConst: Single;
    FShortRangeAttackDamageRandom: Single;
    FShortRangeAttackKnockbackDistance: Single;

    FFallDownLifeLossScale: Single;

    FMiddlePositionHeight: Single;

    FCastShadowVolumes: boolean;
    FRequiredCount: Cardinal;
  protected
    { In descendants only PrepareRender can (and should!) set this. }
    CameraRadiusFromPrepareRender: Single;

    { Like @inherited, but it passes proper values for boolean parameters
      specifying what to prepare. }
    procedure CreateAnimationIfNeeded(
      const AnimationName: string;
      var Anim: TCastlePrecalculatedAnimation;
      AnimInfo: TCastlePrecalculatedAnimationInfo;
      const BaseLights: TLightInstancesList);

    procedure AnimationFromConfig(var AnimInfo: TCastlePrecalculatedAnimationInfo;
      KindsConfig: TCastleConfig; const AnimationName: string;
      NilIfNoElement: boolean = false); override;
  public
    constructor Create(const AShortName: string);

    { If @true, then the creature flies. Otherwise it always tries to move only
      horizontally (which means that Direction is always orthogonal
      to Level.GravityUp), and it falls down when Position is above
      the ground. }
    property Flying: boolean read FFlying write FFlying default DefaultFlying;

    { Camera radius when moving.
      You should make sure that it's something <> 0 for collision detection.

      This is always calculated like:
      If CameraRadiusFromFile <> 0, then take it.
      Otherwise take CameraRadiusFromPrepareRender.
      So there are 2 ways to initialize this:
      1. Set this in creatures/kinds.xml file.
      2. Set CameraRadiusFromPrepareRender properly.

      Setting this in creatures/kinds.xml file to non-zero will effectively
      ignore any CameraRadiusFromPrepareRender value.

      And PrepareRender should always set CameraRadiusFromPrepareRender
      to something non-zero. So note that before PrepareRender was called,
      CameraRadius may remain zero. But you can depend on the fact that
      it's non-zero after PrepareRender.

      Note that this is always measured from MiddlePosition of given
      creature. So take this into account when calcuating
      CameraRadiusFromPrepareRender or writing it in kinds.xml file. }
    function CameraRadius: Single;

    property SoundSuddenPain: TSoundType
      read FSoundSuddenPain write FSoundSuddenPain default stNone;

    property SoundDying: TSoundType
      read FSoundDying write FSoundDying default stNone;

    { See TCreature.Sound3d TiedToCreature parameter docs.
      You can set this to false if you want SoundDying to last even
      after the creature object was destroyed. }
    property SoundDyingTiedToCreature: boolean
      read FSoundDyingTiedToCreature write FSoundDyingTiedToCreature
      default DefaultSoundDyingTiedToCreature;

    { The default MaxLife for this Kind.

      You cannot depend that every creature has this MaxLife ---
      caller can pass any MaxLife value when creating creature by
      CreateDefaultCreature or by any other means.
      This is only a "suggested" default for MaxLife of this creature. }
    property DefaultMaxLife: Single
      read FDefaultMaxLife write FDefaultMaxLife default DefaultDefaultMaxLife;

    { This creates the "default creature instance" with Kind = Self.
      It uses the TCreature descendant most suitable for this TCreatureKind.

      Note that you don't have to use this function to create
      creature instances with this Kind. You can freely create
      TCreature instances on your own, using all flexibility.
      (be careful though as some TCreature desdendants require
      particular TCreatureKind descendants).
      This function just serves as a comfortable way to create
      creature instance as easily as possible. }
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; virtual; abstract;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    { These will be used only by creatures doing ShortRangeAttackHurt
      in their ActualAttack implementation.

      ShortRangeAttackDamageConst and ShortRangeAttackDamageRandom
      and ShortRangeAttackKnockbackDistance must be >= 0.

      ShortRangeAttackKnockbackDistance = 0 means no knockback.

      For now exploding missiles also use these properties.

      @groupBegin }
    property ShortRangeAttackDamageConst: Single
      read FShortRangeAttackDamageConst
      write FShortRangeAttackDamageConst
      default DefaultShortRangeAttackDamageConst;

    property ShortRangeAttackDamageRandom: Single
      read FShortRangeAttackDamageRandom
      write FShortRangeAttackDamageRandom
      default DefaultShortRangeAttackDamageRandom;

    property ShortRangeAttackKnockbackDistance: Single
      read FShortRangeAttackKnockbackDistance
      write FShortRangeAttackKnockbackDistance
      default DefaultShortRangeAttackKnockbackDistance;
    { @groupEnd }

    property FallDownLifeLossScale: Single
      read FFallDownLifeLossScale
      write FFallDownLifeLossScale
      default DefaultFallDownLifeLossScale;

    { This determines how the creature's MiddlePosition will be calculated.
      Actually, this determines how the HeightBetweenLegsAndMiddle
      will be calculated, and this is used to calculate MiddlePosition.
      HeightBetweenLegsAndMiddle is just current scene box height
      multiplied by this, so 1.0 means that MiddlePosition is at the top
      of the box, 0.0 means at the bottom, 0.5 means at the middle etc. }
    property MiddlePositionHeight: Single
      read FMiddlePositionHeight
      write FMiddlePositionHeight
      default DefaultMiddlePositionHeight;

    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes
      default DefaultCastShadowVolumes;

    { Used by RequireCreatures, UnRequireCreatures to count
      how many times this kind is required. Idea is that when this drops
      to zero, we can FreePrepareRender to free resources. }
    property RequiredCount: Cardinal
      read FRequiredCount write FRequiredCount default 0;
  end;

  TCreatureKindList = class(specialize TFPGObjectList<TCreatureKind>)
  public
    { Find item with given ShortName.
      @raises Exception if not found. }
    function FindByShortName(const AShortName: string): TCreatureKind;

    { This opens creatures/kinds.xml file and calls LoadFromFile for
      all existing TCreatureKind instances. }
    procedure LoadFromFile;
  end;

  { A TCreatureKind that has simple states:
    standing stil, walking (aka running), performing an attack and dying.
    Note that you should specify all animation times in seconds
    (just like Level.AnimationTime). }
  TWalkAttackCreatureKind = class(TCreatureKind)
  private
    FStandAnimation: TCastlePrecalculatedAnimation;
    FStandToWalkAnimation: TCastlePrecalculatedAnimation;
    FWalkAnimation: TCastlePrecalculatedAnimation;
    FAttackAnimation: TCastlePrecalculatedAnimation;
    FDyingAnimation: TCastlePrecalculatedAnimation;
    FDyingBackAnimation: TCastlePrecalculatedAnimation;
    FHurtAnimation: TCastlePrecalculatedAnimation;

    FStandAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FStandToWalkAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FWalkAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FAttackAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FDyingAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FDyingBackAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FHurtAnimationInfo: TCastlePrecalculatedAnimationInfo;

    FMoveSpeed: Single;
    FMinDelayBetweenAttacks: Single;
    FMaxAttackDistance: Single;
    FPreferredAttackDistance: Single;
    FActualAttackTime: Single;
    FMaxKnockedBackDistance: Single;

    FSoundAttackStart: TSoundType;
    FLifeToRunAway: Single;
    FMaxAngleToAttack: Single;
    FMinLifeLossToHurt: Single;
    FChanceToHurt: Single;
    FMaxHeightAcceptableToFall: Single;
    FRandomWalkDistance: Single;
  protected
    { Make all TCastlePrecalculatedAnimation properties non-nil. I.e. load them from their
      XxxInfo counterparts.

      Also calculates CameraRadiusFromPrepareRender
      from StandAnimation.Scenes[0]. }
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
    constructor Create(const AShortName: string);

    destructor Destroy; override;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;
    procedure GLContextClose; override;

    { An animation of standing still.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property StandAnimation: TCastlePrecalculatedAnimation read FStandAnimation;

    { An animation when creature changes from standing still to walking.
      Beginning must be on time 0.
      It's beginnig must glue with beginning of StandAnimation,
      it's ending must glue with beginning of WalkAnimation. }
    property StandToWalkAnimation: TCastlePrecalculatedAnimation read FStandToWalkAnimation;

    { An animation of walking.
      Beginning must be on time 0.
      Beginning and end of it must glue together. }
    property WalkAnimation: TCastlePrecalculatedAnimation read FWalkAnimation;

    { An animation of attacking.
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
    property AttackAnimation: TCastlePrecalculatedAnimation read FAttackAnimation;

    { An animation of dying.
      Beginning must be on time 0.
      Beginning should *more-or-less* look like any point of the stand/attack/walk
      animations. Note that we can display this animation infinitely,
      so it must work good after Time > it's TimeEnd. }
    property DyingAnimation: TCastlePrecalculatedAnimation read FDyingAnimation;

    { An optional dying animation. May be @nil, and corresponding
      DyingBackAnimationInfo may be @nil. If not @nil, this will be used
      if creature is killed by hitting it in the back (and normal
      DyingAnimation is used only when it's killed by hitting from the front).

      The direction of last hit is taken from LastAttackDirection. }
    property DyingBackAnimation: TCastlePrecalculatedAnimation read FDyingBackAnimation;

    { Animation when the creature will be hurt.
      Beginning must be on time 0.
      Beginning and end should *more-or-less* look like
      any point of the stand/attack/walk animations.
      Note that this animation will not loop, it will be played
      for TimeDurationWithBack time. }
    property HurtAnimation: TCastlePrecalculatedAnimation read FHurtAnimation;

    { The moving speed: how much Direction vector will be scaled
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
      Player.Camera.Position and creature's MiddlePosition. }
    property MaxAttackDistance: Single
      read FMaxAttackDistance write FMaxAttackDistance
      default DefaultMaxAttackDistance;

    { The preferred distance between player and the creature
      to perform the attack. This must always be <= MaxAttackDistance.
      The idea is that the creature can attack player from MaxAttackDistance,
      but still it will walk closer to the player --- until the distance
      is PreferredAttackDistance. }
    property PreferredAttackDistance: Single
      read FPreferredAttackDistance write FPreferredAttackDistance
      default DefaultPreferredAttackDistance;

    { The time point within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime
      default DefaultActualAttackTime;

    { When this creature is knocked back by something (i.e. goes to wasHurt
      state), then it's forced to move in general LastAttackDirection
      for the time HurtAnimation.TimeDurationWithBack, until
      HurtAnimation (and wasHurt state) ends, or until it was forced to move
      by MaxKnockedBackDistance. }
    property MaxKnockedBackDistance: Single
      read FMaxKnockedBackDistance write FMaxKnockedBackDistance
      default DefaultMaxKnockedBackDistance;

    { This will be played at MiddlePosition when entering wasAttack state.
      Sometimes you may prefer to rather play a sound at ActualAttack
      --- then just do it in overriden ActualAttack. }
    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;

    { If @code(Life <= MaxLife * LifeToRunAway) and distance to the
      player is too short (shorter than MaxAttackDistance / 4),
      the creature runs away. }
    property LifeToRunAway: Single
      read FLifeToRunAway write FLifeToRunAway default DefaultLifeToRunAway;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    { Because most of the creatures will have their weapon
      on their front (teeth, shooting hands, claws, whatever),
      so they can attack player only when their Direction is somewhat
      close to the direction to player.

      More precisely, the attack is allowed to start only when
      the angle between current Direction and the vector
      from creature's MiddlePosition to the player's Position
      is <= MaxAngleToAttack.

      This is in radians. }
    property MaxAngleToAttack: Single
      read FMaxAngleToAttack write FMaxAngleToAttack
      default DefaultMaxAngleToAttack;

    { When creature is wounded for more than MaxLife * MinLifeLossToHurt
      points and moreover Random < ChanceToHurt then creature will
      change to wasHurt state and be knocked back.
      Changing to wasHurt state means that any other state will be
      interrupted (e.g. player can interrupt
      creature's attack this way if ActualAttackTime > 0).

      It's expected that "tougher" creatures will have MinLifeLossToHurt
      somewhat higher than DefaultMinLifeLossToHurt and ChanceToHurt
      significantly lower than DefaultChanceToHurt. }
    property MinLifeLossToHurt: Single
      read FMinLifeLossToHurt write FMinLifeLossToHurt
      default DefaultMinLifeLossToHurt;

    { See MinLifeLossToHurt. }
    property ChanceToHurt: Single
      read FChanceToHurt write FChanceToHurt
      default DefaultChanceToHurt;

    property MaxHeightAcceptableToFall: Single
      read FMaxHeightAcceptableToFall
      write FMaxHeightAcceptableToFall
      default DefaultMaxHeightAcceptableToFall;

    property RandomWalkDistance: Single
      read FRandomWalkDistance
      write FRandomWalkDistance
      default DefaultCreatureRandomWalkDistance;
  end;

  TBallThrowerCreatureKind = class(TWalkAttackCreatureKind)
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;
  end;

  TWerewolfKind = class(TWalkAttackCreatureKind)
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;
  end;

  TSpiderKind = class(TWalkAttackCreatureKind)
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;
  end;

  TSpiderQueenKind = class(TWalkAttackCreatureKind)
  private
    FThrowWebAttackAnimation: TCastlePrecalculatedAnimation;
    FThrowWebAttackAnimationInfo: TCastlePrecalculatedAnimationInfo;

    FMinDelayBetweenThrowWebAttacks: Single;
    FMaxThrowWebAttackDistance: Single;
    FMaxAngleToThrowWebAttack: Single;
    FActualThrowWebAttackTime: Single;
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
    destructor Destroy; override;

    procedure GLContextClose; override;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;

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
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;
  end;

  { A missile. As you can see, this is also treated as a creature
    --- it's just a very dumb creature, that just moves into the given
    direction and explodes on any collision.

    Missile must be generally considered as Flying, otherwise
    it doesn't have much sense... Don't set Flying to false
    for this class (because TMissileCreature may depend on it and never
    cares to keep Direction horizontal). }
  TMissileCreatureKind = class(TCreatureKind)
  private
    FAnimation: TCastlePrecalculatedAnimation;
    FAnimationInfo: TCastlePrecalculatedAnimationInfo;
    FMoveSpeed: Single;
    FSoundExplosion: TSoundType;
    FCloseDirectionToPlayer: boolean;
    FCloseDirectionToTargetSpeed: Single;
    FPauseBetweenSoundIdle: Single;
    FSoundIdle: TSoundType;
    FHitsPlayer: boolean;
    FHitsCreatures: boolean;
    FFallsDown: boolean;
    FFallsDownSpeed: Single;
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
    constructor Create(const AShortName: string);
    destructor Destroy; override;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;
    procedure GLContextClose; override;

    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TCastlePrecalculatedAnimation read FAnimation;

    { The moving speed: how much Direction vector will be scaled
      when moving. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      default DefaultMissileMoveSpeed;

    property SoundExplosion: TSoundType
      read FSoundExplosion write FSoundExplosion default stNone;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    property CloseDirectionToPlayer: boolean
      read FCloseDirectionToPlayer
      write FCloseDirectionToPlayer
      default DefaultCloseDirectionToPlayer;

    { How fast direction to the target is corrected.
      Used only if CloseDirectionToPlayer. }
    property CloseDirectionToTargetSpeed: Single
      read FCloseDirectionToTargetSpeed
      write FCloseDirectionToTargetSpeed
      default DefaultCloseDirectionToTargetSpeed;

    { Sound just played when the missile is going. }
    property SoundIdle: TSoundType
      read FSoundIdle write FSoundIdle default stNone;

    { This should be synchonized with length of SoundIdle sound. }
    property PauseBetweenSoundIdle: Single
      read FPauseBetweenSoundIdle write FPauseBetweenSoundIdle
      default DefaultPauseBetweenSoundIdle;

    property HitsPlayer: boolean
      read FHitsPlayer write FHitsPlayer default DefaultHitsPlayer;
    property HitsCreatures: boolean
      read FHitsCreatures write FHitsCreatures default DefaultHitsCreatures;

    property FallsDown: boolean
      read FFallsDown write FFallsDown default DefaultFallsDown;

    property FallsDownSpeed: Single
      read FFallsDownSpeed write FFallsDownSpeed default DefaultFallsDownSpeed;
  end;

  { A really dumb creature that just stays still during the whole
    game. Basically this is just TCastlePrecalculatedAnimation that is displayed as
    a creature. }
  TStillCreatureKind = class(TCreatureKind)
  private
    FAnimation: TCastlePrecalculatedAnimation;
    FAnimationInfo: TCastlePrecalculatedAnimationInfo;
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
    constructor Create(const AShortName: string);
    destructor Destroy; override;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;
    procedure GLContextClose; override;

    { Missile uses the same animation all the time.
      In the simplest case, you can just place here a single scene. }
    property Animation: TCastlePrecalculatedAnimation read FAnimation;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList;
      const MaxLife: Single): TCreature; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TCreature = class(T3DCustomTransform)
  private
    FKind: TCreatureKind;
    FLegsPosition: TVector3Single;
    FDirection: TVector3Single;
    FLife: Single;
    FMaxLife: Single;
    FLastAttackDirection: TVector3Single;
    procedure SetLastAttackDirection(const Value: TVector3Single);
  private
    { For gravity work. }
    FallingDownStartHeight: Single;
    FIsFallingDown: boolean;

    UsedSounds: TALSoundList;

    procedure SoundSourceUsingEnd(Sender: TALSound);

    procedure SetLegsPosition(const Value: TVector3Single);
    procedure SetDirection(const Value: TVector3Single);
  private
    { It's faster to keep FBoundingBox precalculated (and only update
      it each time we change Direction or LegsPosition)
      instead of completely recalculating it in each BoundingBox call.

      This is especially noticeable when there are many creatures on the level:
      then a lot of time is wasted in DoGravity, and main time of this
      is inside TCreatureList.GetHeightAbove, and main time of this
      would be spend within BoundingBox calculations. Yes, this is
      checked with profiler. }
    FBoundingBox: TBox3D;
    procedure RecalculateBoundingBox;
  private
    FSoundDyingEnabled: boolean;
  protected
    function GetExists: boolean; override;

    { Like TransformMatricesMult, but assumes that LegsPosition and Direction
      is as specified. }
    procedure TransformAssuming(
      const AssumeLegsPosition, AssumeDirection: TVector3Single;
      out M, MInverse: TMatrix4Single);
    procedure TransformMatricesMult(var M, MInverse: TMatrix4Single); override;
    function OnlyTranslation: boolean; override;

    { These define exactly what "MiddlePosition" means for this creature.

      LegsPositionFromMiddle must always specify reverse function.

      LerpLegsMiddlePosition must always be equal to
      Lerp(A, LegsPosition, MiddlePosition)
      (but usually can be calculated more efficiently than calling Lerp).

      In this class they calculate MiddlePosition as LegsPosition
      moved higher than HeightBetweenLegsAndMiddle.
      So if you want to change the meaning of these functions
      you can simply override only HeightBetweenLegsAndMiddle
      (and all things below will still work OK).
      *Or* you can change all 4 functions (3 functions below and
      HeightBetweenLegsAndMiddle), to keep them "synchronized".

      @groupBegin }
    function MiddlePositionFromLegs(
      const AssumeLegsPosition: TVector3Single): TVector3Single; virtual;
    function LegsPositionFromMiddle(
      const AssumeMiddlePosition: TVector3Single): TVector3Single; virtual;
    function LerpLegsMiddlePosition(
      const A: Single): TVector3Single; virtual;
    { @groupEnd }

    { Returns BoundingBox, assuming that LegsPosition and Direction are
      as specified here. }
    function BoundingBoxAssumingLegs(
      const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3D;

    function BoundingBoxAssumingMiddle(
      const AssumeMiddlePosition, AssumeDirection: TVector3Single): TBox3D;

    { This checks collision with Player.BoundingBox, assuming that MiddlePosition
      (and implied LegsPosition) is as given. }
    function MiddleCollisionWithPlayer(
      const AssumeMiddlePosition: TVector3Single): boolean;

    function LegsCollisionWithPlayer(
      const AssumeLegsPosition: TVector3Single): boolean;

    procedure SetLife(const Value: Single); virtual;

    { Tries to move from OldMiddlePosition to ProposedNewMiddlePosition.
      Returns true and sets NewMiddlePosition if some move is allowed.

      Note that OldMiddlePosition @italic(must be equal to MiddlePosition).
      It's passed here only for speed.

      Check collisions with the level, with player, and with other
      creatures. }
    function Move(
      const OldMiddlePosition, ProposedNewMiddlePosition: TVector3Single;
      out NewMiddlePosition: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    { Like @link(Move), but this is only a "yes/no" collision check. }
    function MoveSimple(
      const OldMiddlePosition, NewMiddlePosition: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    { Checks AssumeMiddlePosition height above the level and other creatures.

      I don't check height above the player, this is not needed
      (GetHeightAbove is needed only for "growing up" and "falling down";
      in case of "growing up", creature doesn't have to "grow up"
      when standing on player's head. In case of "falling down" ---
      we don't have to take this into account. Things will work correctly
      anyway.) }
    procedure GetHeightAboveOthers(
      const AssumeMiddlePosition: TVector3Single;
      out IsAbove: boolean; out AboveHeight: Single);

    procedure ShortRangeAttackHurt;

    function DebugCaption: string; virtual;
  public
    { Constructor. Note for AnimationTime: usually I will take
      AnimationTime from global Level.AnimationTime, but in the case of
      constructor it's safer to just take it as param. }
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList); reintroduce;

    destructor Destroy; override;

    { Current Life. Initially set from MaxLife. }
    property Life: Single read FLife write SetLife;

    property MaxLife: Single read FMaxLife write FMaxLife;

    property Kind: TCreatureKind read FKind;

    property Collides default false;

    function BoundingBox: TBox3D; override;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    procedure Idle(const CompSpeed: Single); override;

    { Return current scene to be rendered.
      Note that this is called at the end of our constructor
      (through RecalculateBoundingBox),
      so it must be implemented to work even when Level is not assigned yet. }
    function CurrentScene: TCastleScene; virtual; abstract;

    { The position of the (0, 0, 0) point of creature model
      (or rather, currently used model! Creatures are animated after all). }
    property LegsPosition: TVector3Single read FLegsPosition
      write SetLegsPosition;

    { The height of MiddlePosition above LegsPosition.
      Calculated in this class using CurrentScene.BoundingBox.Data[1, 2]
      and Kind.MiddlePositionHeight.
      Note that while CurrentScene may change, this also may change. }
    function HeightBetweenLegsAndMiddle: Single; virtual;

    { The "middle" position of the creature.
      For some creatures it can be considered the position of their "heads".
      How precisely this is calculated for given creature depends
      on MiddlePositionFromLegs implementation in this class.

      All collision detection (MoveAllowed, GetHeightAbove)
      should be done using MiddlePosition, and then appropriately translated
      back to LegsPosition. Why ? Because this avoids the problems
      of collisions with ground objects. Legs are (for creatures that
      are not Flying and have already fallen down on the ground) on the
      same level as the ground, so checking collisions versus LegsPosition
      is always vulnerable to accidentaly finding collision between LegsPosition
      and the ground.

      So for creatures not Flying, MiddlePosition should
      be always higher (at least by CameraRadius) than LegsPosition.

      For Flying creatures this is not a problem, they could use LegsPosition
      surrounded by CameraRadius for collision detection. But this would
      be inefficient --- since LegsPosition surrounded by CameraRadius
      would unnecessarily block creature's moves. So for Flying creatures
      it's best to actually set MiddlePosition right in the middle of
      creature's model, and so you can make CameraRadius slightly smaller. }
    function MiddlePosition: TVector3Single;

    { Return the one of Level.Sectors that contains MiddlePosition.
      Nil if none. Yes, this is just a shortcut for
      Level.Sectors.SectorWithPoint(MiddlePosition). }
    function MiddlePositionSector: TSceneSector;

    { Direction the creature is facing.
      It always must be normalized.

      In constructor, when setting this from ADirection, we normalize it. }
    property Direction: TVector3Single read FDirection write SetDirection;

    { When this function returns true, the creature will be removed
      from Level.Creatures list (this will be done in game's OnIdle,
      right before calling Idle of all the creatures).

      In this class this returns always false. Yes, this means that
      even dead creatures *may* still exist on level (e.g. to
      show DyingAnimation, or missile's explosion animation etc.) }
    function RemoveMeFromLevel: boolean; virtual;

    { Shortcut for Life <= 0. }
    function Dead: boolean;

    { You can set this to @false to force the creature to die without
      making any sound. This is really seldom needed, usefull only to avoid
      a loud shriek noise when you kill many creatures at once.
      Primarily for use by debug menu "kill all creatures" and similar things. }
    property SoundDyingEnabled: boolean read FSoundDyingEnabled
      write FSoundDyingEnabled default true;

    { Each time you decrease life of this creature, set this property.
      This is the direction from where the attack came.
      You can set this to (0, 0, 0) (ZeroVector3Single)
      if there was no specific direction of attack.

      On set, this vector will be normalized. }
    property LastAttackDirection: TVector3Single
      read FLastAttackDirection write SetLastAttackDirection;

    { If @false, then TCreatureList.MoveAllowedSimple and
      TCreatureList.GetHeightAbove will ignore this
      creature, which means that collisions between this creature
      and player/other creatures will not be checked.
      You should set this to @false only in exceptional situations,
      when you're doing collision checking elsewhere
      (e.g. for TMissileCreatureKind, collision is done when
      the missile moves).

      For all "normal" creatures this should be left as default implementation
      that always returns @true. }
    function CollisionsWithCreaturesAndPlayer: boolean; virtual;

    { Play SoundType where the creature's position is.

      Exactly, the position is between LegsPosition and MiddlePosition
      --- SoundHeight = 0 means LegsPosition, SoundHeight = 1 means MiddlePosition,
      SoundHeight between means ... well, between LegsPosition and MiddlePosition.
      This can also be higher than 1 or lower than 0, should be treated like
      lerp between LegsPosition and MiddlePosition.

      If TiedToCreature then the sounds position will be updated
      as the creature will move, and when the creature object will
      be destroyed, sound will stop. If not TiedToCreature, then
      the sound will simply be done at creature's position, but then
      it will continue to be played independent of this creature. }
    procedure Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
      TiedToCreature: boolean = true);

    { Is using bounding sphere (sphere centered at MiddlePosition with
      radius Kind.CameraRadius) allowed ?

      If @false, then all collision
      detection routines @italic(must) use bounding box instead of the
      sphere. Since UseBoundingSphere = @false usually means that
      Kind.CameraRadius is not appropriate for this creature state.

      In this class, this is implemented to return @code(not Dead).
      This is usually sensible, since only alive creatures need bounding
      sphere advantages (stairs climbing), and using sphere with dead
      creatures would unnecessarily force the sphere radius to be small
      and MiddlePosition to be high. }
    function UseBoundingSphere: boolean; virtual;

    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; override;

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;

    procedure Translate(const T: TVector3Single); override;
  end;

  TCreatureList = class(specialize TFPGObjectList<TCreature>)
    { Remove from this list all creatures that return
      RemoveMeFromLevel = @true. }
    procedure RemoveFromLevel;

    { This checks whether someone (player or creature) moving from
      OldBoundingBox to NewBoundingBox(and OldPosition to NewPosition
      --- appropriate positions should be within their bounding boxes)
      collides with any creature on the the list.

      Returns nil if no collision or reference of (one of) colliding
      creatures.

      You can pass IgnoreCreature <> nil if you want to ignore
      collisions with given creature (this will obviously be useful
      when checking for collisions for this creature). }
    function MoveAllowedSimple(
      const OldBoundingBox, NewBoundingBox: TBox3D;
      const OldPosition, NewPosition: TVector3Single;
      IgnoreCreature: TCreature): TCreature;

    { Height of Position over creatures' bounding boxes.

      Assumes IsAbove and other Above* parameters are already initialized.
      It will update them. Note that AboveGround is updated to @nil if
      camera will be found standing over one of the creatures
      (since creatures are not represented as any P3DTriangle).

      You can pass IgnoreCreature <> nil if you want to ignore
      collisions with given creature (this will obviously be useful
      when checking for collisions for this creature). }
    procedure GetHeightAbove(const Position: TVector3Single;
      var IsAbove: boolean; var AboveHeight: Single;
      var AboveGround: P3DTriangle;
      IgnoreCreature: TCreature);

    { Searches for item of given Kind. Returns index of first found,
      or -1 if not found. }
    function FindKind(Kind: TCreatureKind): Integer;
  end;

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack,
    wasDying, wasDyingBack, wasHurt, wasSpecial1);

  { A TCreature that has a kind always of TWalkAttackCreatureKind. }
  TWalkAttackCreature = class(TCreature)
  private
    FState: TWalkAttackCreatureState;

    FStateChangeTime: Single;

    { time of last FState change to wasAttack, taken from Level.AnimationTime. }
    LastAttackTime: Single;
    { Set to true each time you enter wasAttack, set back to false
      if ActualAttack was called. }
    ActualAttackDone: boolean;

    { Set to 0 each time FState changes to wasHurt }
    KnockedBackDistance: Single;

    HasLastSeenPlayer: boolean;
    LastSeenPlayer: TVector3Single;
    LastSeenPlayerSector: TSceneSector;

    HasAlternativeTarget: boolean;
    AlternativeTarget: TVector3Single;
    { Time of last setting HasAlternativeTarget to true and AlternativeTarget
      value, taken from Level.AnimationTime. Used to not fall into loop
      when the creature tries to walk to AlternativeTarget, and is not
      permanently blocked (so MoveAllowed returns true all the time)
      but at the same time the creature can't get close enough to the
      AlternativeTarget. In such case we use this variable to resign from
      AlternativeTarget after some time. }
    AlternativeTargetTime: Single;

    WaypointsSaved_Begin: TSceneSector;
    WaypointsSaved_End: TSceneSector;
    WaypointsSaved: TSceneWaypointList;
  protected
    procedure SetState(Value: TWalkAttackCreatureState); virtual;

    { Use this in ActualAttack for short range creatures. }
    function ShortRangeActualAttackHits: boolean;
  protected
    { Set by Idle in this class, may be used by descendants
      in their Idle calls (to not calculate the same thing twice). }
    IdleSeesPlayer: boolean;
    IdleSqrDistanceToLastSeenPlayer: Single;

    procedure SetLife(const Value: Single); override;

    function DebugCaption: string; override;
  public
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList);

    destructor Destroy; override;

    { Shortcut for TWalkAttackCreatureKind(Kind). }
    function WAKind: TWalkAttackCreatureKind;

    property State: TWalkAttackCreatureState read FState
      default wasStand;

    { Last State change time, taken from Level.AnimationTime. }
    property StateChangeTime: Single read FStateChangeTime;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TCastleScene; override;

    { The method where you must actually do your attack
      --- fire a missile, lower player's life etc.

      This happens in the middle of AttackAnimation,
      see also ActualAttackTime. Of course you should use
      current creature LegsPosition, MiddlePosition, Direction
      etc. to determine things like missile starting position
      and direction.

      If creature is doing some short-range attack
      you can also just lower here player's Life. Remember in this
      case to check that player is close enough; in general situation,
      you can't depend that player is still within MaxAttackDistance
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
  private
    NextHowlTime: Single;
  public
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single; const BaseLights: TLightInstancesList);

    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single); override;

    procedure Howl(ForceHowl: boolean);
  end;

  TSpiderCreature = class(TWalkAttackCreature)
  public
    procedure ActualAttack; override;

    function RemoveMeFromLevel: boolean; override;
  end;

  TSpiderQueenCreature = class(TWalkAttackCreature)
  private
    LastThrowWebAttackTime: Single;
    ActualThrowWebAttackDone: boolean;
    function SQKind: TSpiderQueenKind;
    procedure ActualThrowWebAttack;
  protected
    procedure SetState(Value: TWalkAttackCreatureState); override;
    procedure SetLife(const Value: Single); override;
  public
    procedure ActualAttack; override;
    procedure Idle(const CompSpeed: Single); override;
    function CurrentScene: TCastleScene; override;
  end;

  TGhostCreature = class(TWalkAttackCreature)
  public
    procedure ActualAttack; override;

    { Ghosts dead animation is quite unique, so we will not check
      collisions with ghost when it's in dying state.
      Ghost is blended anyway, so checking for collisions with him
      is not really necessary anyway. }
    function CollisionsWithCreaturesAndPlayer: boolean; override;

    function RemoveMeFromLevel: boolean; override;
  end;

  { A TCreature that has a kind always of TMissileCreatureKind. }
  TMissileCreature = class(TCreature)
  private
    procedure ExplodeCore;
    procedure ExplodeWithPlayer;
    procedure ExplodeWithLevel;
    procedure ExplodeWithCreature(Creature: TCreature);
  private
    LastSoundIdleTime: Single;
  public
    { Shortcut for TMissileCreatureKind(Kind). }
    function MissileKind: TMissileCreatureKind;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TCastleScene; override;

    { Missiles return @false here.
      We will check for collisions when missile moves. }
    function CollisionsWithCreaturesAndPlayer: boolean; override;

    function RemoveMeFromLevel: boolean; override;
  end;

  TStillCreature = class(TCreature)
  public
    { Shortcut for TStillCreatureKind(Kind). }
    function StillKind: TStillCreatureKind;

    function CurrentScene: TCastleScene; override;

    function RemoveMeFromLevel: boolean; override;
  end;

var
  CreaturesKinds: TCreatureKindList;

  Alien: TBallThrowerCreatureKind;
  Werewolf: TWerewolfKind;
  BallMissile: TMissileCreatureKind;
  Ghost: TGhostKind;
  Spider: TSpiderKind;
  SpiderQueen: TSpiderQueenKind;
  ThrownWeb: TMissileCreatureKind;
  Arrow: TMissileCreatureKind;
  Barrel: TStillCreatureKind;

implementation

uses SysUtils, DOM, GL, GLU, GameWindow, CastleWindow,
  CastleFilesUtils, CastleGLUtils, ProgressUnit, GamePlay,
  GameVideoOptions, GameNotifications, GameRequiredResources;

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AShortName: string);
begin
  inherited Create(AShortName);
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FSoundDyingTiedToCreature := DefaultSoundDyingTiedToCreature;
  FShortRangeAttackDamageConst := DefaultShortRangeAttackDamageConst;
  FShortRangeAttackDamageRandom := DefaultShortRangeAttackDamageRandom;
  FShortRangeAttackKnockbackDistance := DefaultShortRangeAttackKnockbackDistance;
  FFallDownLifeLossScale := DefaultFallDownLifeLossScale;
  FMiddlePositionHeight := DefaultMiddlePositionHeight;
  FCastShadowVolumes := DefaultCastShadowVolumes;
  CreaturesKinds.Add(Self);
end;

procedure TCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  Flying := KindsConfig.GetValue(ShortName + '/flying',
    DefaultFlying);
  SoundDyingTiedToCreature :=
    KindsConfig.GetValue(ShortName + '/sound_dying_tied_to_creature',
    DefaultSoundDyingTiedToCreature);
  DefaultMaxLife := KindsConfig.GetFloat(ShortName + '/default_max_life',
    DefaultDefaultMaxLife);
  CameraRadiusFromFile := KindsConfig.GetFloat(ShortName + '/camera_radius',
    0.0);
  ShortRangeAttackDamageConst :=
    KindsConfig.GetFloat(ShortName + '/short_range_attack/damage/const',
    DefaultShortRangeAttackDamageConst);
  ShortRangeAttackDamageRandom :=
    KindsConfig.GetFloat(ShortName + '/short_range_attack/damage/random',
    DefaultShortRangeAttackDamageRandom);
  ShortRangeAttackKnockbackDistance :=
    KindsConfig.GetFloat(ShortName + '/short_range_attack/knockback_distance',
    DefaultShortRangeAttackKnockbackDistance);

  FallDownLifeLossScale :=
    KindsConfig.GetFloat(ShortName + '/fall_down_life_loss_scale',
    DefaultFallDownLifeLossScale);

  MiddlePositionHeight :=
    KindsConfig.GetFloat(ShortName + '/middle_position_height',
    DefaultMiddlePositionHeight);

  CastShadowVolumes :=
    KindsConfig.GetValue(ShortName + '/casts_shadow', DefaultCastShadowVolumes);

  SoundSuddenPain := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_sudden_pain', ''));
  SoundDying := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_dying', ''));
end;

function TCreatureKind.CameraRadius: Single;
begin
  if CameraRadiusFromFile <> 0 then
    Result := CameraRadiusFromFile else
    Result := CameraRadiusFromPrepareRender;
end;

procedure TCreatureKind.CreateAnimationIfNeeded(
  const AnimationName: string;
  var Anim: TCastlePrecalculatedAnimation;
  AnimInfo: TCastlePrecalculatedAnimationInfo;
  const BaseLights: TLightInstancesList);
var
  Options: TPrepareResourcesOptions;
begin
  Options := [prRender, prBoundingBox];
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  inherited CreateAnimationIfNeeded(AnimationName, Anim, AnimInfo,
    Options, BaseLights);
end;

procedure TCreatureKind.AnimationFromConfig(var AnimInfo: TCastlePrecalculatedAnimationInfo;
  KindsConfig: TCastleConfig; const AnimationName: string;
  NilIfNoElement: boolean);
begin
  inherited;
  if AnimInfo <> nil then
    AnimInfo.ScenesPerTime :=
      AnimInfo.ScenesPerTime * CreatureAnimationScenesPerTime;
end;

{ TCreatureKindList -------------------------------------------------------- }

function TCreatureKindList.FindByShortName(
  const AShortName: string): TCreatureKind;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ShortName = AShortName then
      Exit;
  end;

  raise Exception.CreateFmt('Not existing creature kind name "%s"',
    [AShortName]);
end;

procedure TCreatureKindList.LoadFromFile;
var
  I: Integer;
  KindsConfig: TCastleConfig;
begin
  KindsConfig := TCastleConfig.Create(nil);
  try
    KindsConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'creatures' + PathDelim + 'kinds.xml';

    for I := 0 to Count - 1 do
    begin
      Items[I].LoadFromFile(KindsConfig);
    end;
  finally SysUtils.FreeAndNil(KindsConfig); end;
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(const AShortName: string);
begin
  inherited Create(AShortName);

  MoveSpeed := DefaultMoveSpeed;
  FMinDelayBetweenAttacks := DefaultMinDelayBetweenAttacks;
  FMaxAttackDistance := DefaultMaxAttackDistance;
  FPreferredAttackDistance := DefaultPreferredAttackDistance;
  FMaxKnockedBackDistance := DefaultMaxKnockedBackDistance;
  FLifeToRunAway := DefaultLifeToRunAway;
  FActualAttackTime := DefaultActualAttackTime;
  FMaxAngleToAttack := DefaultMaxAngleToAttack;
  FMinLifeLossToHurt := DefaultMinLifeLossToHurt;
  FChanceToHurt := DefaultChanceToHurt;
  FMaxHeightAcceptableToFall := DefaultMaxHeightAcceptableToFall;
  FRandomWalkDistance := DefaultCreatureRandomWalkDistance;
end;

destructor TWalkAttackCreatureKind.Destroy;
begin
  FreeAndNil(FStandAnimation);
  FreeAndNil(FStandToWalkAnimation);
  FreeAndNil(FWalkAnimation);
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FDyingAnimation);
  FreeAndNil(FDyingBackAnimation);
  FreeAndNil(FHurtAnimation);

  FreeAndNil(FStandAnimationInfo);
  FreeAndNil(FStandToWalkAnimationInfo);
  FreeAndNil(FWalkAnimationInfo);
  FreeAndNil(FAttackAnimationInfo);
  FreeAndNil(FDyingAnimationInfo);
  FreeAndNil(FDyingBackAnimationInfo);
  FreeAndNil(FHurtAnimationInfo);

  inherited;
end;

procedure TWalkAttackCreatureKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  inherited;

  AddFirstRootNodesPool(FStandAnimationInfo      );
  AddFirstRootNodesPool(FStandToWalkAnimationInfo);
  AddFirstRootNodesPool(FWalkAnimationInfo       );
  AddFirstRootNodesPool(FAttackAnimationInfo     );
  AddFirstRootNodesPool(FDyingAnimationInfo      );
  AddFirstRootNodesPool(FDyingBackAnimationInfo  );
  AddFirstRootNodesPool(FHurtAnimationInfo       );

  CreateAnimationIfNeeded('Stand'      , FStandAnimation      , FStandAnimationInfo      , BaseLights);
  AddManifoldEdgesPool(FStandAnimationInfo, FStandAnimation);
  CreateAnimationIfNeeded('StandToWalk', FStandToWalkAnimation, FStandToWalkAnimationInfo, BaseLights);
  CreateAnimationIfNeeded('Walk'       , FWalkAnimation       , FWalkAnimationInfo       , BaseLights);
  CreateAnimationIfNeeded('Attack'     , FAttackAnimation     , FAttackAnimationInfo     , BaseLights);
  CreateAnimationIfNeeded('Dying'      , FDyingAnimation      , FDyingAnimationInfo      , BaseLights);
  CreateAnimationIfNeeded('DyingBack'  , FDyingBackAnimation  , FDyingBackAnimationInfo  , BaseLights);
  CreateAnimationIfNeeded('Hurt'       , FHurtAnimation       , FHurtAnimationInfo       , BaseLights);

  CameraRadiusFromPrepareRender :=
    Min(StandAnimation.Scenes[0].BoundingBox.XYRadius,
        StandAnimation.Scenes[0].BoundingBox.Data[1, 2] * 0.75);
end;

function TWalkAttackCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 12;
end;

procedure TWalkAttackCreatureKind.FreePrepareRender;
begin
  FreeAndNil(FStandAnimation);
  FreeAndNil(FStandToWalkAnimation);
  FreeAndNil(FWalkAnimation);
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FDyingAnimation);
  FreeAndNil(FDyingBackAnimation);
  FreeAndNil(FHurtAnimation);

  inherited;
end;

procedure TWalkAttackCreatureKind.GLContextClose;
begin
  inherited;
  if StandAnimation <> nil then StandAnimation.GLContextClose;
  if StandToWalkAnimation <> nil then StandToWalkAnimation.GLContextClose;
  if WalkAnimation <> nil then WalkAnimation.GLContextClose;
  if AttackAnimation <> nil then AttackAnimation.GLContextClose;
  if DyingAnimation <> nil then DyingAnimation.GLContextClose;
  if DyingBackAnimation <> nil then DyingBackAnimation.GLContextClose;
  if HurtAnimation <> nil then HurtAnimation.GLContextClose;
end;

procedure TWalkAttackCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  ActualAttackTime :=
    KindsConfig.GetFloat(ShortName + '/actual_attack_time',
    DefaultActualAttackTime);
  MoveSpeed :=
    KindsConfig.GetFloat(ShortName + '/move_speed',
    DefaultMoveSpeed);
  MaxAttackDistance :=
    KindsConfig.GetFloat(ShortName + '/max_attack_distance',
    DefaultMaxAttackDistance);
  PreferredAttackDistance :=
    KindsConfig.GetFloat(ShortName + '/preferred_attack_distance',
    DefaultPreferredAttackDistance);
  MinDelayBetweenAttacks :=
    KindsConfig.GetFloat(ShortName + '/min_delay_between_attacks',
    DefaultMinDelayBetweenAttacks);
  LifeToRunAway :=
    KindsConfig.GetFloat(ShortName + '/life_to_run_away',
    DefaultLifeToRunAway);
  MaxKnockedBackDistance :=
    KindsConfig.GetFloat(ShortName + '/max_knocked_back_distance',
    DefaultMaxKnockedBackDistance);
  MaxAngleToAttack :=
    KindsConfig.GetFloat(ShortName + '/max_angle_to_attack',
    DefaultMaxAngleToAttack);
  MinLifeLossToHurt :=
    KindsConfig.GetFloat(ShortName + '/min_life_loss_to_hurt',
    DefaultMinLifeLossToHurt);
  ChanceToHurt :=
    KindsConfig.GetFloat(ShortName + '/chance_to_hurt',
    DefaultChanceToHurt);
  MaxHeightAcceptableToFall :=
    KindsConfig.GetFloat(ShortName + '/max_height_acceptable_to_fall',
    DefaultMaxHeightAcceptableToFall);
  RandomWalkDistance :=
    KindsConfig.GetFloat(ShortName + '/random_walk_distance',
    DefaultCreatureRandomWalkDistance);

  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_attack_start', ''));

  AnimationFromConfig(FStandAnimationInfo, KindsConfig, 'stand');
  AnimationFromConfig(FStandToWalkAnimationInfo, KindsConfig, 'stand_to_walk');
  AnimationFromConfig(FWalkAnimationInfo, KindsConfig, 'walk');
  AnimationFromConfig(FAttackAnimationInfo, KindsConfig, 'attack');
  AnimationFromConfig(FDyingAnimationInfo, KindsConfig, 'dying');
  AnimationFromConfig(FDyingBackAnimationInfo, KindsConfig, 'dying_back', true);
  AnimationFromConfig(FHurtAnimationInfo, KindsConfig, 'hurt');
end;

{ TBallThrowerCreatureKind --------------------------------------------------- }

function TBallThrowerCreatureKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TBallThrowerCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

{ TWerewolfKind -------------------------------------------------------------- }

function TWerewolfKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TWerewolfCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

{ TSpiderKind -------------------------------------------------------------- }

function TSpiderKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TSpiderCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

{ TSpiderQueenKind -------------------------------------------------------- }

destructor TSpiderQueenKind.Destroy;
begin
  FreeAndNil(FThrowWebAttackAnimation);
  FreeAndNil(FThrowWebAttackAnimationInfo);
  inherited;
end;

procedure TSpiderQueenKind.GLContextClose;
begin
  inherited;
  if ThrowWebAttackAnimation <> nil then ThrowWebAttackAnimation.GLContextClose;
end;

procedure TSpiderQueenKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  inherited;
  CreateAnimationIfNeeded('ThrowWebAttack',
    FThrowWebAttackAnimation, FThrowWebAttackAnimationInfo, BaseLights);
end;

function TSpiderQueenKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TSpiderQueenKind.FreePrepareRender;
begin
  FreeAndNil(FThrowWebAttackAnimation);
  inherited;
end;

function TSpiderQueenKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TSpiderQueenCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

procedure TSpiderQueenKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  MinDelayBetweenThrowWebAttacks :=
    KindsConfig.GetFloat(ShortName + '/throw_web/min_delay_between_attacks', 0.0);
  MaxThrowWebAttackDistance :=
    KindsConfig.GetFloat(ShortName + '/throw_web/max_attack_distance', 0.0);
  MaxAngleToThrowWebAttack :=
    KindsConfig.GetFloat(ShortName + '/throw_web/max_angle_to_attack', 0.0);
  ActualThrowWebAttackTime :=
    KindsConfig.GetFloat(ShortName + '/throw_web/actual_attack_time', 0.0);

  AnimationFromConfig(FThrowWebAttackAnimationInfo, KindsConfig, 'throw_web_attack');
end;

{ TGhostKind ------------------------------------------------------------- }

procedure TGhostKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
var
  ReferenceScene: TCastleScene;
begin
  inherited;

  { For Flying creatures, larger CameraRadius (that *really* surrounds whole
    model from MiddlePosition) is better. }
  ReferenceScene := StandAnimation.Scenes[0];

  CameraRadiusFromPrepareRender :=
    Max(ReferenceScene.BoundingBox.XYRadius,
    { I can do here "/ 2" thanks to the fact that middle_position_height
      of ghost is 0.5 (so I have room for another "BoundingBox.Data[1, 2] / 2"
      for radius). }
    ReferenceScene.BoundingBox.Data[1, 2] / 2);
end;

function TGhostKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TGhostCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

{ TMissileCreatureKind ---------------------------------------------------- }

constructor TMissileCreatureKind.Create(const AShortName: string);
begin
  inherited Create(AShortName);
  Flying := true;
  FMoveSpeed := DefaultMissileMoveSpeed;
  FCloseDirectionToPlayer := DefaultCloseDirectionToPlayer;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
  FFallsDown := DefaultFallsDown;
  FFallsDownSpeed := DefaultFallsDownSpeed;
end;

destructor TMissileCreatureKind.Destroy;
begin
  FreeAndNil(FAnimation);
  FreeAndNil(FAnimationInfo);
  inherited;
end;

procedure TMissileCreatureKind.FreePrepareRender;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

procedure TMissileCreatureKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  inherited;
  CreateAnimationIfNeeded('Move', FAnimation, FAnimationInfo, BaseLights);

  CameraRadiusFromPrepareRender := Animation.Scenes[0].BoundingBox.XYRadius;
end;

function TMissileCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TMissileCreatureKind.GLContextClose;
begin
  inherited;
  if Animation <> nil then Animation.GLContextClose;
end;

function TMissileCreatureKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TMissileCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

procedure TMissileCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  MoveSpeed :=
    KindsConfig.GetFloat(ShortName + '/move_speed',
    DefaultMissileMoveSpeed);
  CloseDirectionToPlayer :=
    KindsConfig.GetValue(ShortName + '/close_direction_to_player',
    DefaultCloseDirectionToPlayer);
  CloseDirectionToTargetSpeed :=
    KindsConfig.GetFloat(ShortName + '/close_direction_to_target_speed',
    DefaultCloseDirectionToTargetSpeed);
  PauseBetweenSoundIdle :=
    KindsConfig.GetFloat(ShortName + '/pause_between_sound_idle',
    DefaultPauseBetweenSoundIdle);
  HitsPlayer :=
    KindsConfig.GetValue(ShortName + '/hits_player',
    DefaultHitsPlayer);
  HitsCreatures :=
    KindsConfig.GetValue(ShortName + '/hits_creatures',
    DefaultHitsCreatures);
  FFallsDown :=
    KindsConfig.GetValue(ShortName + '/falls_down', DefaultFallsDown);
  FFallsDownSpeed :=
    KindsConfig.GetFloat(ShortName + '/falls_down_speed',
    DefaultFallsDownSpeed);

  SoundExplosion := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_explosion', ''));
  SoundIdle := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_idle', ''));

  AnimationFromConfig(FAnimationInfo, KindsConfig, 'fly');
end;

{ TStillCreatureKind ---------------------------------------------------- }

constructor TStillCreatureKind.Create(const AShortName: string);
begin
  inherited Create(AShortName);
end;

destructor TStillCreatureKind.Destroy;
begin
  FreeAndNil(FAnimation);
  FreeAndNil(FAnimationInfo);
  inherited;
end;

procedure TStillCreatureKind.FreePrepareRender;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

procedure TStillCreatureKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  inherited;
  CreateAnimationIfNeeded('Stand', FAnimation, FAnimationInfo, BaseLights);

  CameraRadiusFromPrepareRender := Animation.Scenes[0].BoundingBox.XYRadius;
end;

function TStillCreatureKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TStillCreatureKind.GLContextClose;
begin
  inherited;
  if Animation <> nil then Animation.GLContextClose;
end;

function TStillCreatureKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList;
  const MaxLife: Single): TCreature;
begin
  Result := TStillCreature.Create(Self, ALegsPosition, ADirection,
    MaxLife, AnimationTime, BaseLights);
end;

procedure TStillCreatureKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  AnimationFromConfig(FAnimationInfo, KindsConfig, 'stand');
end;

{ TCreatureSoundSourceData --------------------------------------------------- }

type
  TCreatureSoundSourceData = class
  public
    { TODO: unused ? }
    SoundHeight: Single;
  end;

{ TCreature ------------------------------------------------------------------ }

constructor TCreature.Create(AKind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList);
begin
  inherited Create(nil);

  FKind := AKind;

  { TODO: we resolve collisions ourselves, without the help of T3D and scene
    manager methods. For them, we're not collidable, for now.
    In the future, we should just switch to use T3D and scene manager methods. }
  Collides := false;

  Collision := ctCreature;

  { This is only needed if you used --debug-no-creatures or forgot
    to add creature to <required_resources> }
  RequireCreature(BaseLights, Kind);

  FLegsPosition := ALegsPosition;
  FDirection := Normalized(ADirection);

  FMaxLife := AMaxLife;
  FLife := MaxLife;

  FSoundDyingEnabled := true;

  UsedSounds := TALSoundList.Create(false);

  { FLegsPosition and FDirection changed, so RecalculateBoundingBox must be
    called. At this point CurrentScene is needed (by RecalculateBoundingBox),
    and CurrentScene may require some animations to be loaded -- so we check
    PrepareRenderDone. }
  if not Kind.PrepareRenderDone then
    raise EInternalError.CreateFmt('PrepareRender of creature kind "%s" not done',
      [Kind.ShortName]);
  RecalculateBoundingBox;
end;

function TCreature.GetExists: boolean;
begin
  Result := (inherited GetExists) and (not GameWin);
end;

destructor TCreature.Destroy;
var
  I: Integer;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      UsedSounds[I].UserData.Free;
      UsedSounds[I].UserData := nil;

      { Otherwise OnUsingEnd would call TCreature.SoundSourceUsingEnd,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here. }
      UsedSounds[I].OnUsingEnd := nil;
      UsedSounds[I].DoUsingEnd;
    end;
    FreeAndNil(UsedSounds);
  end;

  if Kind <> nil then
    UnRequireCreature(Kind);

  inherited;
end;

function TCreature.BoundingBox: TBox3D;
begin
  if GetExists then
    Result := FBoundingBox else
    Result := EmptyBox3D;
end;

procedure TCreature.SoundSourceUsingEnd(Sender: TALSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  Sender.OnUsingEnd := nil;
  UsedSounds.Remove(Sender);
end;

procedure TCreature.Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
  TiedToCreature: boolean);
var
  NewSource: TALSound;
  SoundPosition: TVector3Single;
begin
  SoundPosition := LerpLegsMiddlePosition(SoundHeight);
  NewSource := SoundEngine.Sound3d(SoundType, SoundPosition);
  if TiedToCreature and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnUsingEnd := @SoundSourceUsingEnd;
    NewSource.UserData := TCreatureSoundSourceData.Create;
  end;
end;

function TCreature.HeightBetweenLegsAndMiddle: Single;
begin
  Result := CurrentScene.BoundingBox.Data[1, 2] * Kind.MiddlePositionHeight;
end;

function TCreature.MiddlePositionFromLegs(
  const AssumeLegsPosition: TVector3Single): TVector3Single;
begin
  Result := AssumeLegsPosition;
  Result[2] += HeightBetweenLegsAndMiddle;
end;

function TCreature.LegsPositionFromMiddle(
  const AssumeMiddlePosition: TVector3Single): TVector3Single;
begin
  Result := AssumeMiddlePosition;
  Result[2] -= HeightBetweenLegsAndMiddle;
end;

function TCreature.LerpLegsMiddlePosition(const A: Single): TVector3Single;
begin
  Result := LegsPosition;
  Result[2] += HeightBetweenLegsAndMiddle * A;
end;

function TCreature.MiddlePosition: TVector3Single;
begin
  Result := MiddlePositionFromLegs(LegsPosition);
end;

procedure TCreature.TransformAssuming(
  const AssumeLegsPosition, AssumeDirection: TVector3Single;
  out M, MInverse: TMatrix4Single);
var
  GoodUp, Side: TVector3Single;
begin
  GoodUp := UnitVector3Single[2];
  { If not Flying, then we know that GoodUp is already
    orthogonal to AssumeDirection. }
  if Kind.Flying then
    MakeVectorsOrthoOnTheirPlane(GoodUp, AssumeDirection);

  Side := VectorProduct(GoodUp, AssumeDirection);

  { Note that actually I could do here TransformToCoordsNoScaleMatrix,
    as obviously I don't want any scaling. But in this case I know
    that AssumeDirection length = 1 and GoodUp = 1 (so their product
    length is also = 1), so no need to do
    TransformToCoordsNoScaleMatrix here (and I can avoid wasting my time
    on Sqrts needed inside TransformToCoordsNoScaleMatrix). }

  M := TransformToCoordsMatrix(AssumeLegsPosition,
    AssumeDirection, Side, GoodUp);
  MInverse := TransformFromCoordsMatrix(AssumeLegsPosition,
    AssumeDirection, Side, GoodUp);
end;

procedure TCreature.TransformMatricesMult(var M, MInverse: TMatrix4Single);
var
  NewM, NewMInverse: TMatrix4Single;
begin
  TransformAssuming(LegsPosition, Direction, NewM, NewMInverse);
  M := M * NewM;
  MInverse := NewMInverse * MInverse;
end;

function TCreature.OnlyTranslation: boolean;
begin
  Result := false;
end;

function TCreature.BoundingBoxAssumingLegs(
  const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3D;
var
  M, MInverse: TMatrix4Single;
begin
  TransformAssuming(AssumeLegsPosition, AssumeDirection, M, MInverse);
  Result := CurrentScene.BoundingBox.Transform(M);
end;

function TCreature.BoundingBoxAssumingMiddle(
  const AssumeMiddlePosition, AssumeDirection: TVector3Single): TBox3D;
var
  AssumeLegsPosition: TVector3Single;
begin
  AssumeLegsPosition := LegsPositionFromMiddle(AssumeMiddlePosition);
  Result := BoundingBoxAssumingLegs(AssumeLegsPosition, AssumeDirection);
end;

procedure TCreature.SetLegsPosition(const Value: TVector3Single);
begin
  FLegsPosition := Value;
  RecalculateBoundingBox;
end;

procedure TCreature.SetDirection(const Value: TVector3Single);
begin
  FDirection := Value;
  RecalculateBoundingBox;
end;

procedure TCreature.RecalculateBoundingBox;
begin
  FBoundingBox := BoundingBoxAssumingLegs(LegsPosition, Direction);
end;

procedure TCreature.Render(const Frustum: TFrustum;
  const Params: TRenderParams);

  procedure RenderBoundingGeometry;
  var
    Q: PGLUQuadric;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);

      glDrawBox3DWire(BoundingBox);

      glPushMatrix;
        glTranslatev(MiddlePosition);
        Q := NewGLUQuadric(false, GLU_NONE, GLU_OUTSIDE, GLU_LINE);
        try
          gluSphere(Q, Kind.CameraRadius, 10, 10);
        finally gluDeleteQuadric(Q); end;
      glPopMatrix;
    glPopAttrib;
  end;

  procedure DoRenderDebugCaptions;
  const
    FontSize = 0.5;
  begin
    glPushMatrix;
      glTranslatef(0, 0, CurrentScene.BoundingBox.Data[1, 2]);
      glRotatef(90, 0, 0, 1);
      glRotatef(90, 1, 0, 0);
      glScalef(FontSize / Font3d.RowHeight, FontSize / Font3d.RowHeight, 1);

      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
        glColorv(White3Single);
        Font3d.PrintAndMove(DebugCaption);
      glPopAttrib;
    glPopMatrix;
  end;

begin
  { self-shadows on creatures look bad, esp. see werewolves at the end
    of "castle hall" level. Changing XxxShadowVolumes here
    is a little hacky (would be cleaner to do it at loading), but easy. }
  CurrentScene.ReceiveShadowVolumes := false;
  CurrentScene.CastShadowVolumes := Kind.CastShadowVolumes;

  { Make sure our List contains exactly CurrentScene }
  if Count = 1 then
    List[0] := CurrentScene else
    Add(CurrentScene);

  inherited;

  if GetExists and Frustum.Box3DCollisionPossibleSimple(BoundingBox) then
  begin
    glPushMatrix;
      glMultMatrix(Transform);
      if RenderDebugCaptions and
         (not Params.Transparent) and Params.ShadowVolumesReceivers then
        DoRenderDebugCaptions;
    glPopMatrix;

    if RenderBoundingBoxes and
       (not Params.Transparent) and Params.ShadowVolumesReceivers then
      RenderBoundingGeometry;
  end;
end;

function TCreature.DebugCaption: string;
begin
  Result := Format('%s [%s / %s]',
    [Kind.ShortName, FloatToNiceStr(Life), FloatToNiceStr(MaxLife)]);
end;

function TCreature.MiddleCollisionWithPlayer(
  const AssumeMiddlePosition: TVector3Single): boolean;
begin
  Result := BoundingBoxAssumingMiddle(AssumeMiddlePosition, Direction).Collision(
    Player.BoundingBox);
end;

function TCreature.LegsCollisionWithPlayer(
  const AssumeLegsPosition: TVector3Single): boolean;
begin
  Result := BoundingBoxAssumingLegs(AssumeLegsPosition, Direction).Collision(
    Player.BoundingBox);
end;

function TCreature.Move(
  const OldMiddlePosition, ProposedNewMiddlePosition: TVector3Single;
  out NewMiddlePosition: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  { Check creature<->level collision. }
  if UseBoundingSphere then
  begin
    Result := Level.MoveAllowed(
      OldMiddlePosition, ProposedNewMiddlePosition, NewMiddlePosition,
      BecauseOfGravity, Kind.CameraRadius);
  end else
  begin
    NewMiddlePosition := ProposedNewMiddlePosition;
    Result := Level.MoveBoxAllowedSimple(
      OldMiddlePosition, NewMiddlePosition,
      BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
      BecauseOfGravity);
  end;

  if Result then
    Result :=
      { Check creature<->player collision. }
      (not MiddleCollisionWithPlayer(NewMiddlePosition)) and
      { Check creature<->other creatures collision. }
      (Level.Creatures.MoveAllowedSimple(
        BoundingBox,
        BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
        OldMiddlePosition, NewMiddlePosition, Self) = nil);
end;

function TCreature.MoveSimple(
  const OldMiddlePosition, NewMiddlePosition: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  { Check creature<->level collision. }
  if UseBoundingSphere then
  begin
    Result := Level.MoveAllowedSimple(
      OldMiddlePosition, NewMiddlePosition,
      BecauseOfGravity, Kind.CameraRadius);
  end else
  begin
    Result := Level.MoveBoxAllowedSimple(
      OldMiddlePosition, NewMiddlePosition,
      BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
      BecauseOfGravity);
  end;

  if Result then
    Result :=
      { Check creature<->player collision. }
      (not MiddleCollisionWithPlayer(NewMiddlePosition)) and
      { Check creature<->other creatures collision. }
      (Level.Creatures.MoveAllowedSimple(
        BoundingBox,
        BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
        OldMiddlePosition, NewMiddlePosition, Self) = nil);
end;

procedure TCreature.GetHeightAboveOthers(
  const AssumeMiddlePosition: TVector3Single;
  out IsAbove: boolean; out AboveHeight: Single);
var
  AboveGround: PTriangle;
begin
  { Check creature<->level collision. }
  Level.GetHeightAbove(AssumeMiddlePosition, IsAbove, AboveHeight, AboveGround);

  { Check creature<->other creatures collision. }
  Level.Creatures.GetHeightAbove(AssumeMiddlePosition,
    IsAbove, AboveHeight, AboveGround, Self);

  { I ignore calculated AboveGround for now.
    I could return it... but for now, nothing needs it. }
end;

procedure TCreature.Idle(const CompSpeed: Single);

  procedure UpdateUsedSounds;
  var
    I: Integer;
    SoundPosition: TVector3Single;
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      SoundPosition := LerpLegsMiddlePosition(
        TCreatureSoundSourceData(UsedSounds[I].UserData).SoundHeight);
      UsedSounds[I].Position := SoundPosition;
    end;
  end;

  procedure DoGravity;

    procedure FalledDown;
    var
      FallenHeight, LifeLoss: Single;
    begin
      FallenHeight := FallingDownStartHeight - LegsPosition[2];
      if FallenHeight > 1.0 then
      begin
        Sound3d(stCreatureFalledDown, 0.1, false);
        if FallenHeight > 4.0 then
        begin
          LifeLoss := Max(0,
            Kind.FallDownLifeLossScale *
            FallenHeight * MapRange(Random, 0.0, 1.0, 0.8, 1.2));
          Life := Life - LifeLoss;
          LastAttackDirection := ZeroVector3Single;
        end;
      end;
    end;

  var
    OldMiddlePosition: TVector3Single;

    function MoveVertical(const Distance: Single): boolean;
    var
      NewMiddlePosition, ProposedNewMiddlePosition: TVector3Single;
    begin
      ProposedNewMiddlePosition := OldMiddlePosition;
      ProposedNewMiddlePosition[2] += Distance;

      Result := Move(OldMiddlePosition, ProposedNewMiddlePosition,
        NewMiddlePosition, true);

      if Result then
        LegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
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
    IsAbove: boolean;
    AboveHeight: Single;
    OldIsFallingDown: boolean;
    FallingDownDistance, MaximumFallingDownDistance: Single;
  begin
    { Gravity does it's work here.
      This is extremely simplified version of Gravity work in TWalkCamera.
      (simplified, because creature doesn't need all these effects). }

    { Note that also here we do collision detection using MiddlePosition,
      not LegsPosition. See MiddlePosition docs for reasoning. }

    OldIsFallingDown := FIsFallingDown;
    OldMiddlePosition := MiddlePosition;

    GetHeightAboveOthers(OldMiddlePosition, IsAbove, AboveHeight);

    if AboveHeight > HeightBetweenLegsAndMiddle * HeightMargin then
    begin
      { Fall down }
      if not FIsFallingDown then
        FallingDownStartHeight := LegsPosition[2];

      FIsFallingDown := true;

      FallingDownDistance := FallingDownSpeed * CompSpeed * 50;
      if IsAbove then
      begin
        MaximumFallingDownDistance :=
          AboveHeight - HeightBetweenLegsAndMiddle;

        { If you will fall down by exactly
          AboveHeight - HeightBetweenLegsAndMiddle,
          then you will get exatly into collision with the ground.
          So actually this is too large MaximumFallingDownDistance.

          But actually it's OK when UseBoundingSphere, because then
          MoveVertical (actually MoveAllowed) can correct new position,
          so actually it will be slightly above the ground. So falling
          down will work.

          But when not UseBoundingSphere, the situation is worse,
          because then MoveAllowed actually calls MoveAllowedSimple.
          And MoveAllowedSimple will always simply reject such move
          with MaximumFallingDownDistance.
          If FPS is low (so we would like to fall down at once
          by large distance), this is noticeable: in such case, instead
          of falling down, creature hangs over the ground,
          because MoveAllowedSimple simply doesn't allow it fall
          exactly by AboveHeight - HeightBetweenLegsAndMiddle.
          So MaximumFallingDownDistance has to be a little smaller in this case.
          In particular, this was noticeable for the initially dead alien
          creature on "Doom" level, when shadows were on (when shadows were on,
          FPS is low, that's why the bug was noticeable only with shadows = on).

          TODO: the better version would be to improve
          MoveAllowed for not UseBoundingSphere case, instead of
          workarounding it here with this epsilon,
          but this requires implementing "wall-sliding" for the whole box3d
          in our octree. }
        if not UseBoundingSphere then
          MaximumFallingDownDistance -= 0.01;
        MinTo1st(FallingDownDistance, MaximumFallingDownDistance);
      end;

      if not MoveVertical(-FallingDownDistance) then
        FIsFallingDown := false;
    end else
    begin
      FIsFallingDown := false;

      if AboveHeight < HeightBetweenLegsAndMiddle / HeightMargin then
      begin
        { Growing up }
        MoveVertical(Min(GrowingUpSpeed * CompSpeed * 50,
          HeightBetweenLegsAndMiddle - AboveHeight));
      end;
    end;

    if OldIsFallingDown and (not FIsFallingDown) then
      FalledDown;
  end;

begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  { CurrentScene (possibly) changed, since Level.AnimationTime changed now.
    So recalculate bounding box.

    Note that this is somewhat too late:
    Level.AnimationTime changed earlier (some other code possibly already
    checked our BoundingBox and got wrong answer, because our Idle
    didn't update BoundingBoxyet). But in practice this is not a problem,
    since BoundingBox doesn't change too rapidly --- and we're always
    prepared for unpredictable changes of BoundingBox anyway (since they
    are caused by animation frame changes). So we don't depend on
    "BoundingBox of the creature doesn't collide with others", even if
    we try to achieve this. }
  RecalculateBoundingBox;

  UpdateUsedSounds;

  if not Kind.Flying then
    DoGravity;
end;

function TCreature.RemoveMeFromLevel: boolean;
begin
  Result := false;
end;

procedure TCreature.SetLife(const Value: Single);
begin
  if (Life > 0) and (Value <= 0) then
  begin
    { When dies, we don't play SoundSuddenPain sound. We will play SoundDying. }
    if SoundDyingEnabled then
      Sound3d(Kind.SoundDying, 1.0, Kind.SoundDyingTiedToCreature);
  end else
  if (Life > 0) and (Life - Value > 5) then
  begin
    Sound3d(Kind.SoundSuddenPain, 1.0);
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

function TCreature.MiddlePositionSector: TSceneSector;
begin
  Result := Level.Sectors.SectorWithPoint(MiddlePosition);
end;

function TCreature.CollisionsWithCreaturesAndPlayer: boolean;
begin
  Result := true;
end;

procedure TCreature.ShortRangeAttackHurt;
var
  Damage: Single;
begin
  Damage := Kind.ShortRangeAttackDamageConst +
    Random * Kind.ShortRangeAttackDamageRandom;

  if Kind.ShortRangeAttackKnockbackDistance <> 0 then
    Player.Knockback(Damage, Kind.ShortRangeAttackKnockbackDistance,
      Direction) else
    Player.Life := Player.Life - Damage;
end;

function TCreature.UseBoundingSphere: boolean;
begin
  Result := not Dead;
end;

function TCreature.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  Intersection: TVector3Single;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
begin
  { Overridden, to resolve collision by looking at bounding box.
    No need to look at actual scene geometry, no need for octree inside scene. }

  if GetExists and BoundingBox.TryRayClosestIntersection(
    Intersection, IntersectionDistance, RayOrigin, RayDirection) then
  begin
    Result := TRayCollision.Create;
    Result.Distance := IntersectionDistance;
    NewNode := Result.Add;
    NewNode^.Item := Self;
    NewNode^.Point := Intersection;
    NewNode^.Triangle := nil;
    NewNode^.RayOrigin := RayOrigin;
    NewNode^.RayDirection := RayDirection;
  end else
    Result := nil;
end;

function TCreature.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
const
  VisibleItemDistance = 60.0;
var
  S: string;
begin
  Result := Active;
  if not Result then Exit;

  if Distance <= VisibleItemDistance then
  begin
    S := Format('You see a creature "%s"', [Kind.ShortName]);

    if Life >= MaxLife then
      S += ' (not wounded)' else
    if Life >= MaxLife / 3 then
      S += ' (wounded)' else
    if Life > 0 then
      S += ' (very wounded)' else
      S += ' (dead)';

    Notifications.Show(S);
  end else
    Notifications.Show('You see some creature, but it''s too far to tell exactly what it is');
end;

procedure TCreature.Translate(const T: TVector3Single);
begin
  LegsPosition := LegsPosition + T;
end;

{ TCreatureList -------------------------------------------------------------- }

procedure TCreatureList.RemoveFromLevel;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].RemoveMeFromLevel then
    begin
      Level.Items.Remove(Items[I]);
      FPGObjectList_FreeAndNilItem(Self, I);
    end;
  FPGObjectList_RemoveNils(Self);
end;

function TCreatureList.MoveAllowedSimple(
  const OldBoundingBox, NewBoundingBox: TBox3D;
  const OldPosition, NewPosition: TVector3Single;
  IgnoreCreature: TCreature): TCreature;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if (Result <> IgnoreCreature) and
      Result.CollisionsWithCreaturesAndPlayer then
    begin
      if NewBoundingBox.Collision(Result.BoundingBox) then
      begin
        { Strictly thinking, now I know that I have a collision with creature
          and I should exit with false. But it's not that simple.

          Note that there is weakness in collision checking with creatures,
          because when AnimationTime changes then effectively creature's
          CurrentScene.BoundingBox changes, and there is no way how I can
          check for collision there (what could I do ? Stop the animation ?
          Throw the creature back ? None of this seems sensible...)
          This means that we cannot prevent the situation when someone's
          (player's or creature's) and other creature's bounding boxes collide.

          So we must take precautions to not make someone "stuck"
          with this creature (because any potential move collides
          with this creature).

          That's the reasoning behind using OldBoundingBox and checks below.
          I disallow the collision only if there was no collision before
          (so the pathologic situation doesn't occur) or if someone
          tries to get closer to the creature (so if the pathologic situation
          occurs, someone can't make it worse, and can't "abuse" this
          by entering into creature's bounding box). }
        if (not OldBoundingBox.Collision(Result.BoundingBox)) or
           ( PointsDistanceSqr(NewPosition, Result.MiddlePosition) <
             PointsDistanceSqr(OldPosition, Result.MiddlePosition) ) then
          Exit;
      end else
      { If NewBoundingBox doesn't collide with Result.BoundingBox,
        and OldBoundingBox also doesn't collide (so pathological
        situation above occurs) we know that segment between
        OldPosition and NewPosition cannot collide with Result.BoundingBox.

        Without this check, player could get on the other side
        of the creature if the creature is slim (e.g. Alien) and player
        tries very hard, and he has large speed. }
      if (not OldBoundingBox.Collision(Result.BoundingBox)) and
         Result.BoundingBox.IsSegmentCollision(OldPosition, NewPosition) then
        Exit;
    end;
  end;

  Result := nil;
end;

procedure TCreatureList.GetHeightAbove(
  const Position: TVector3Single;
  var IsAbove: boolean; var AboveHeight: Single;
  var AboveGround: P3DTriangle;
  IgnoreCreature: TCreature);

  { If the Point is inside the Box then it answers IsAboveTheBox := false. }
  procedure GetPointHeightAboveBox3D(const Point: TVector3Single;
    const Box: TBox3D;
    out IsAboveTheBox: boolean; out HeightAboveTheBox: Single);
  begin
    { We use here the assumption that GravityUp is (0, 0, 1). }

    IsAboveTheBox := (not Box.IsEmpty) and
      (Box.Data[0, 0] <= Point[0]) and (Point[0] <= Box.Data[1, 0]) and
      (Box.Data[0, 1] <= Point[1]) and (Point[1] <= Box.Data[1, 1]) and
      (Point[2] >= Box.Data[1, 2]);

    if IsAboveTheBox then
      HeightAboveTheBox := Point[2] - Box.Data[1, 2] else
      HeightAboveTheBox := MaxSingle;
  end;

var
  I: Integer;
  IsAboveTheBox: boolean;
  HeightAboveTheBox: Single;
begin
  for I := 0 to Count - 1 do
    if (Items[I] <> IgnoreCreature) and
       (Items[I].CollisionsWithCreaturesAndPlayer) then
    begin
      GetPointHeightAboveBox3D(Position, Items[I].BoundingBox,
        IsAboveTheBox, HeightAboveTheBox);

      if HeightAboveTheBox < AboveHeight then
      begin
        IsAbove := IsAboveTheBox;
        AboveHeight := HeightAboveTheBox;
        AboveGround := nil;
      end;
    end;
end;

function TCreatureList.FindKind(Kind: TCreatureKind): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Kind = Kind then
      Exit;
  Result := -1;
end;

{ TWalkAttackCreature -------------------------------------------------------- }

constructor TWalkAttackCreature.Create(AKind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList);
begin
  inherited Create(AKind, ALegsPosition, ADirection, AMaxLife, AnimationTime, BaseLights);

  if AMaxLife > 0 then
  begin
    FState := wasStand;
    FStateChangeTime := AnimationTime;
  end else
  begin
    { This means that the creature is created already in dead state...
      So we start with wasDying state and set FStateChangeTime to fake
      the fact that creature was killed long time ago.

      This way the creature is created as a dead corpse, without making
      any kind of dying (or wounded) sound or animation. }
    FState := wasDying;
    FStateChangeTime := AnimationTime - 1000;
  end;

  WaypointsSaved := TSceneWaypointList.Create(false);
end;

destructor TWalkAttackCreature.Destroy;
begin
  FreeAndNil(WaypointsSaved);
  inherited;
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
    FStateChangeTime := Level.AnimationTime;
    { Some states require special initialization here. }
    case FState of
      wasAttack:
        begin
          Sound3d(WAKind.SoundAttackStart, 1.0);
          LastAttackTime := StateChangeTime;
          ActualAttackDone := false;
        end;
      wasHurt:
        KnockedBackDistance := 0.0;
    end;
  end;
end;

procedure TWalkAttackCreature.Idle(const CompSpeed: Single);

  { Is attack allowed ? }
  function AttackAllowed: boolean;
  var
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    Result := IdleSeesPlayer and
      (Level.AnimationTime - LastAttackTime > WAKind.MinDelayBetweenAttacks) and
      (IdleSqrDistanceToLastSeenPlayer <= Sqr(WAKind.MaxAttackDistance));

    if Result then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        VectorSubtract(LastSeenPlayer, MiddlePosition),
        Direction);
      Result := AngleRadBetweenTheDirectionToPlayer <= WAKind.MaxAngleToAttack;
    end;
  end;

  procedure CalculateDirectionToTarget(
    const Target: TVector3Single;
    out DirectionToTarget: TVector3Single;
    out AngleRadBetweenDirectionToTarget: Single);
  begin
    { calculate DirectionToTarget }
    DirectionToTarget := VectorSubtract(Target, MiddlePosition);
    if not Kind.Flying then
      MakeVectorsOrthoOnTheirPlane(DirectionToTarget, Level.GravityUp);

    { calculate AngleRadBetweenDirectionToTarget }
    AngleRadBetweenDirectionToTarget :=
      AngleRadBetweenVectors(DirectionToTarget, Direction);
  end;

  { Call this only when HasLastSeenPlayer }
  procedure CalculateDirectionToPlayer(out DirectionToPlayer: TVector3Single;
    out AngleRadBetweenDirectionToPlayer: Single);
  begin
    CalculateDirectionToTarget(LastSeenPlayer,
      DirectionToPlayer, AngleRadBetweenDirectionToPlayer);
  end;

  procedure CalculateDirectionFromPlayer(
    var DirectionFromPlayer: TVector3Single;
    var AngleRadBetweenDirectionFromPlayer: Single);
  begin
    CalculateDirectionToPlayer(
      DirectionFromPlayer, AngleRadBetweenDirectionFromPlayer);
    VectorNegateTo1st(DirectionFromPlayer);
    AngleRadBetweenDirectionFromPlayer :=
      Pi - AngleRadBetweenDirectionFromPlayer;
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
      AngleRadChange := AngleRadChangeSpeed * CompSpeed * 50;
      MinTo1st(AngleRadChange, AngleRadBetweenDirectionToTarget);

      Direction := RotatePointAroundAxisRad(AngleRadChange, Direction,
        VectorProduct(Direction, DirectionToTarget));

      { From time to time it's good to fix Direction, to make sure it's
        1. normalized,
        2. and orthogonal to GravityUp if not Flying
        Otherwise rounding errors could accumulate and cause some nasty things.

        Actually, I didn't observe anything bad caused by the above,
        but I'm safeguarding anyway, for safety. }
      if not Kind.Flying then
        MakeVectorsOrthoOnTheirPlane(FDirection, Level.GravityUp);
      NormalizeTo1st(FDirection);
      RecalculateBoundingBox;
    end;
  end;

  function CloseEnoughToTarget(const Target: TVector3Single): boolean;
  const
    MinDistanceToTarget = 0.1;
  var
    SqrDistanceToTarget: Single;
  begin
    if WAKind.Flying then
      SqrDistanceToTarget := PointsDistanceSqr(MiddlePosition, Target) else
      SqrDistanceToTarget := PointsDistanceXYSqr(MiddlePosition, Target);
    Result :=
      { If creature is ideally at the target
        (for not Flying creatures, this means "ideally under/above the target"),
        then there is no way to get closer to the target.

        We check this with some "epsilon" (MinDistanceToTarget), as usual, to

        1. Avoid the unnecessary moving when MiddlePosition is in fact
           close enough to the target, but lack of floating precision
           can't move it really ideally to Target.

        2. In fact, it's not desirable to get exactly at (or under/above)
           the target, because this could cause undesirable rotations
           of the creature Direction (we usually try to make it in
           the Target direction, so when we stand (almost) exactly
           at Target, creature could try to stupidly rotate around itself). }
      SqrDistanceToTarget <= Sqr(MinDistanceToTarget);
  end;

  { Assuming that I want to walk in DesiredDirection direction,
    is it sensible to do this by moving along current Direction ? }
  function WantToWalkInDesiredDirection(
    const AngleRadBetweenDesiredDirection: Single): boolean;
  const
    MaxAngleToMoveForward = Pi / 3 { 60 degrees };
  begin
    Result :=
      { If AngleRadBetweenDesiredDirection is too large, there is not much point
        in moving in given direction anyway. We should just change our Direction. }
      (AngleRadBetweenDesiredDirection <= MaxAngleToMoveForward);
  end;

  { Assuming that I want to get to Target position, is it sensible
    to do this by moving along current Direction ?
    This checks whether current Direction points roughly in the
    direction of the Target, and if were not already as close as possible
    to Target. }
  function WantToWalkToTarget(
    const Target: TVector3Single;
    const AngleRadBetweenDirectionToTarget: Single): boolean;
  begin
    Result :=
      WantToWalkInDesiredDirection(AngleRadBetweenDirectionToTarget) and
      { See comments in CloseEnoughToTarget for reasoning why this is needed. }
      (not CloseEnoughToTarget(Target));
  end;

  { This doesn't take into account current Direction,
    it only looks at current Position and LastSeenPlayer position,
    and asks "do I want to get closer" ?
    Use only if HasLastSeenPlayer. }
  function WantToShortenDistanceToPlayer: boolean;
  begin
    Result :=
      { Is it wanted to get closer to the LastSeenPlayer ?

        Yes --- only if it will help make AttackAllowed from false to true.
        See AttackAllowed implementation.

        If IdleSeesPlayer and IdleSqrDistanceToLastSeenPlayer is small enough,
        there's no point in getting closer to the player. In fact, it would
        be bad to get closer to player in this case, as this would allow
        player to easier attack (shorter distance --- easier to reach with
        short-range weapon, or easier to aim with long-range weapon). }
      ( (not IdleSeesPlayer) or
        (IdleSqrDistanceToLastSeenPlayer > Sqr(WAKind.PreferredAttackDistance))
      );
  end;

  { Is it wanted to get closer to the LastSeenPlayer ?
    And (if it's wanted) is it sensible to do this by moving
    along current Direction ?
    Call this only if HasLastSeenPlayer. }
  function WantToWalkToPlayer(
    const AngleRadBetweenDirectionToPlayer: Single): boolean;
  begin
    Result := WantToShortenDistanceToPlayer and
      WantToWalkToTarget(LastSeenPlayer, AngleRadBetweenDirectionToPlayer);
  end;

  function WantToRunAway: boolean;
  begin
    Result := IdleSeesPlayer and
      (Life <= MaxLife * WAKind.LifeToRunAway) and
      (IdleSqrDistanceToLastSeenPlayer < Sqr(WAKind.MaxAttackDistance / 4));
  end;

  procedure InitAlternativeTarget;
  var
    Distance: Single;
  begin
    Distance := WAKind.RandomWalkDistance;

    AlternativeTarget := MiddlePosition;
    AlternativeTarget[0] += Random * Distance * 2 - Distance;
    AlternativeTarget[1] += Random * Distance * 2 - Distance;
    if WAKind.Flying then
      AlternativeTarget[2] += Random * Distance * 2 - Distance;

    HasAlternativeTarget := true;

    AlternativeTargetTime := Level.AnimationTime;
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
      if WantToRunAway or
         WantToWalkToPlayer(AngleRadBetweenDirectionToPlayer) then
        SetState(wasWalk) else
      if (not Kind.Flying) and
         (AngleRadBetweenDirectionToPlayer < 0.01) and
         (BoundingBox.Data[0][0] <= LastSeenPlayer[0]) and
         (BoundingBox.Data[1][0] >= LastSeenPlayer[0]) and
         (BoundingBox.Data[0][1] <= LastSeenPlayer[1]) and
         (BoundingBox.Data[1][1] >= LastSeenPlayer[1]) then
      begin
        { Then the player (or it's LastSeenPlayer) is right above or below us.
          Since we can't fly, we can't get there. Standing in place
          is one possibility, but it's not really good
          - We become easier target to shoot for player with the bow.
          - Most importantly, this way player can stand on our head and
            slash us with a sword without any risk. (This was almost
            a standard technique on killing Werewolf or SpiderQueen bosses).
          So we move a little --- just for the sake of moving. }
        SetState(wasWalk);
        InitAlternativeTarget;
      end else
      begin
        { Continue wasStand state }
        RotateDirectionToFaceTarget(DirectionToPlayer,
          AngleRadBetweenDirectionToPlayer);
      end;
    end;
  end;

  procedure DoWalk;

    { This performs the real move, which means that it changes LegsPosition
      and MiddlePosition along the Direction vector.

      This doesn't check whether this is a sensible move, so use this
      only if you know that the creature really wants to go in this Direction.

      This checks only the basic (i.e. always wanted) things:
      - Collision detection (with level, player and other creatures)
      - For not Flying creatures, also the check to not fall down from high
        is done. }
    function MoveAlongTheDirection: boolean;

      { Don't be stupid, and don't walk where you see you will fall down. }
      function TooHighAboveTheGround(const NewMiddlePosition: TVector3Single):
        boolean;
      var
        IsAbove: boolean;
        AboveHeight: Single;
      begin
        Result := false;
        if not Kind.Flying then
        begin
          GetHeightAboveOthers(NewMiddlePosition, IsAbove, AboveHeight);
          if AboveHeight > WAKind.MaxHeightAcceptableToFall +
              HeightBetweenLegsAndMiddle then
            Result := true;
        end;
      end;

    var
      OldMiddlePosition, NewMiddlePosition: TVector3Single;
    begin
      OldMiddlePosition := MiddlePosition;
      NewMiddlePosition := VectorAdd(OldMiddlePosition,
        VectorScale(Direction, WAKind.MoveSpeed * CompSpeed * 50));

      Result :=
        { First check to not step into some deep fall.
          Note that I'm not using here NewMiddlePosition
          (that will be calculated later by Level.MoveAllowed)
          or ProposedNewMiddlePosition, because they are too close
          to OldMiddlePosition to be good to test against.
          I'm calculating here where I would get after 0.2 second
          (WAKind.MoveSpeed * 0.2 * 50). }
        (not TooHighAboveTheGround(VectorAdd(OldMiddlePosition,
          VectorScale(Direction, WAKind.MoveSpeed * 0.2 * 50)))) and

        { MoveAllowed is free to just return true and set
          NewMiddlePosition to OldMiddlePosition (or something very close)
          instead of returning false. But this is not good for
          MoveAlongTheDirection, as things using MoveAlongTheDirection
          depend on the fact that MoveAlongTheDirection will return false
          if no further way is possible.

          That's why I use MoveAllowedSimple below.
          Our trick with "AlternativeTarget" should handle
          eventual problems with the track of creature, so MoveAllowed
          should not be needed. }
        MoveSimple(OldMiddlePosition, NewMiddlePosition, false);

      if Result then
        LegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
    end;

    { Go the way to LastSeenPlayer, *not* by using waypoints.
      Assumes HasLastSeenPlayer. }
    procedure WalkNormal;
    var
      DirectionToTarget: TVector3Single;
      AngleRadBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToPlayer(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToWalkToPlayer(AngleRadBetweenDirectionToTarget) then
      begin
        if not MoveAlongTheDirection then
        begin
          { Not able to get to player this way ? Maybe there exists
            some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end else
      begin
        { I don't want to walk anymore. So just stand stil. }
        SetState(wasStand);
        Exit;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);
    end;

    procedure WalkToWaypoint(const Target: TVector3Single);
    var
      DirectionToTarget: TVector3Single;
      AngleRadBetweenDirectionToTarget: Single;
    begin
      CalculateDirectionToTarget(Target, DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToShortenDistanceToPlayer then
      begin
        if not MoveAlongTheDirection then
        begin
          { Not able to get to waypoint this way ? Maybe there exists
            some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end else
      begin
        { I don't want to walk anymore. So just stand stil. }
        SetState(wasStand);
        Exit;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);
    end;

  const
    ProbabilityToTryAnotherAlternativeTarget = 0.5;
    AngleRadBetweenDirectionToTargetToResign = Pi / 180 { 1 degree };
    MaxTimeForAlternativeTarget = 5.0;
  var
    DirectionToTarget: TVector3Single;
    AngleRadBetweenDirectionToTarget: Single;
    MiddlePositionSectorNow: TSceneSector;
    UseWalkNormal: boolean;
  begin
    if HasAlternativeTarget then
    begin
      if CloseEnoughToTarget(AlternativeTarget) or
         (Level.AnimationTime - AlternativeTargetTime > MaxTimeForAlternativeTarget) then
      begin
        HasAlternativeTarget := false;
        Exit;
      end;

      CalculateDirectionToTarget(AlternativeTarget,
        DirectionToTarget, AngleRadBetweenDirectionToTarget);

      if WantToWalkToTarget(AlternativeTarget,
        AngleRadBetweenDirectionToTarget) then
      begin
        { Note that MoveAlongTheDirection returns false when
          moving along the current Direction is not good.
          But maybe moving along the DirectionToTarget is possible ?
          So we shouldn't just resign from current AlternativeTarget
          so fast --- maybe it's good, but we have to adjust
          our Direction a little more. That's why I use
          AngleRadBetweenDirectionToTargetToResign.

          Note that for normal moving (i.e. toward LastSeenPlayer,
          not AlternativeTarget) we in this case just change state
          to wasStand, and this allows creature to rotate in wasStand
          state. }
        if (not MoveAlongTheDirection) and
           (AngleRadBetweenDirectionToTarget <=
             AngleRadBetweenDirectionToTargetToResign) then
        begin
          if Random <= ProbabilityToTryAnotherAlternativeTarget then
          begin
            { Try yet another alternative way. }
            InitAlternativeTarget;
            Exit;
          end else
          begin
            HasAlternativeTarget := false;
            Exit;
          end;
        end;
      end else
      begin
        { We know that WantToWalkToTarget may return false only because
          were not directed enough for AlternativeTarget.
          (because we already eliminated CloseEnoughToTarget case above).
          In each DoWalk call we will gradually fix this,
          by RotateDirectionToFaceTarget below.
          So do nothing now. Just stay in wasWalk mode,
          and do RotateDirectionToFaceTarget below. }
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);
    end else
    if WantToRunAway then
    begin
      CalculateDirectionFromPlayer(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);

      if WantToWalkInDesiredDirection(AngleRadBetweenDirectionToTarget) then
      begin
        if (not MoveAlongTheDirection) and
           (AngleRadBetweenDirectionToTarget <=
             AngleRadBetweenDirectionToTargetToResign) then
        begin
          { Maybe there exists some alternative way, not straight. Lets try. }
          InitAlternativeTarget;
          Exit;
        end;
      end;

      RotateDirectionToFaceTarget(DirectionToTarget,
        AngleRadBetweenDirectionToTarget);
    end else
    begin
      if not HasLastSeenPlayer then
      begin
        { Nowhere to go; so just stay here. }
        SetState(wasStand);
        Exit;
      end;

      UseWalkNormal := true;

      MiddlePositionSectorNow := MiddlePositionSector;
      if (MiddlePositionSectorNow <> LastSeenPlayerSector) and
         (MiddlePositionSectorNow <> nil) and
         (LastSeenPlayerSector <> nil) then
      begin
        { The way to LastSeenPlayer is using waypoints. }

        { Recalculate WaypointsSaved.
          Note that I recalculate only when MiddlePositionSectorNow or
          LastSeenPlayerSector changed. }
        if (MiddlePositionSectorNow <> WaypointsSaved_Begin) or
           (LastSeenPlayerSector <> WaypointsSaved_End) then
        begin
          WaypointsSaved_Begin := MiddlePositionSectorNow;
          WaypointsSaved_End := LastSeenPlayerSector;
          TSceneSectorList.FindWay(WaypointsSaved_Begin, WaypointsSaved_End,
            WaypointsSaved);
        end;

        if WaypointsSaved.Count <> 0 then
        begin
          { There is a space around the waypoint that is within
            more than one sector. SectorWithPoint will then answer
            with any (it's not specified which) sector that has
            given position. This is problematic, because this means
            that the creature will be forced to go once again to the same
            waypoint that it's already at... This way there could arise
            a situation when the creature gets stuck at some waypoint,
            because we constantly detect that it must pass through this
            waypoint. The check for CloseEnoughToTarget below prevents this. }
          if CloseEnoughToTarget(WaypointsSaved[0].Position) then
          begin
            if WaypointsSaved.Count > 1 then
            begin
              WalkToWaypoint(WaypointsSaved[1].Position);
              UseWalkNormal := false;
            end;
          end else
          begin
            WalkToWaypoint(WaypointsSaved[0].Position);
            UseWalkNormal := false;
          end;
        end;
      end;

      if UseWalkNormal then
        WalkNormal;
    end;

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
    OldMiddlePosition, ProposedNewMiddlePosition, NewMiddlePosition: TVector3Single;
    CurrentKnockBackDistance: Single;
  begin
    StateTime := Level.AnimationTime - StateChangeTime;

    if StateTime > WAKind.HurtAnimation.TimeDurationWithBack then
      SetState(wasStand) else
    if KnockedBackDistance <= WAKind.MaxKnockedBackDistance then
    begin
      OldMiddlePosition := MiddlePosition;

      { Calculate CurrentKnockBackDistance, update KnockedBackDistance }
      CurrentKnockBackDistance := KnockedBackSpeed * CompSpeed * 50;
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

      ProposedNewMiddlePosition := VectorAdd(OldMiddlePosition,
        VectorScale(LastAttackDirection, CurrentKnockBackDistance));

      if Move(OldMiddlePosition, ProposedNewMiddlePosition,
        NewMiddlePosition, false) then
        LegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
    end;
  end;

  { @true if last attack was from the back of the creature,
    @false if from the front or unknown (when LastAttackDirection is zero). }
  function WasLackAttackBack: boolean;
  begin
    try
      Result := AngleRadBetweenVectors(LastAttackDirection, Direction) < Pi/2;
    except
      on EVectorMathInvalidOp do Result := false;
    end;
  end;

begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if Dead and not (State in [wasDying, wasDyingBack]) then
  begin
    if (WAKind.DyingBackAnimation <> nil) and
       WasLackAttackBack then
      SetState(wasDyingBack) else
      SetState(wasDying);
    Exit;
  end;

  IdleSeesPlayer := Level.LineOfSight(MiddlePosition, Player.Camera.Position);
  if IdleSeesPlayer then
  begin
    HasLastSeenPlayer := true;
    LastSeenPlayer := Player.Camera.Position;
    LastSeenPlayerSector := Player.PositionSector;
  end;

  if HasLastSeenPlayer then
  begin
    IdleSqrDistanceToLastSeenPlayer :=
      PointsDistanceSqr(LastSeenPlayer, MiddlePosition);
  end;

  case FState of
    wasStand: DoStand;
    wasWalk: DoWalk;
    wasAttack: DoAttack;
    wasDying, wasDyingBack: ;
    wasHurt: DoHurt;
    wasSpecial1: { Should be handled in descendants. };
    else raise EInternalError.Create('FState ?');
  end;
end;

function TWalkAttackCreature.CurrentScene: TCastleScene;
var
  StateTime: Single;
begin
  { Time from the change to this state. }
  if Level <> nil then
    StateTime := Level.AnimationTime - StateChangeTime else
    StateTime := 0;

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
    wasDyingBack:
      Result := WAKind.DyingBackAnimation.SceneFromTime(StateTime);
    wasHurt:
      Result := WAKind.HurtAnimation.SceneFromTime(StateTime);
    else raise EInternalError.Create('FState ?');
  end;
end;

procedure TWalkAttackCreature.SetLife(const Value: Single);
begin
  if (not Dead) and
    (Life - Value > WAKind.MinLifeLossToHurt * MaxLife) and
    ( (WAKind.ChanceToHurt = 1.0) or
      (Random < WAKind.ChanceToHurt) ) then
    SetState(wasHurt);
  inherited;
end;

function TWalkAttackCreature.ShortRangeActualAttackHits: boolean;
var
  B, PB: TBox3D;
  DistanceLength, DistanceIncrease: Single;
begin
  B := BoundingBox;
  PB := Player.BoundingBox;

  { We would like to check collision between PB and our B translated
    by our Direction now, i.e.
      Boxes3DCollision(Box3DTranslate(B, VectorScale(Direction, ???)), PB)
    But how much should be scale Direction, i.e. what to put for "???" ?
    It must be large enough to compensate even large WAKind.MaxAttackDistance,
    it must be small enough so that player should not be able to avoid
    our attacks just by standing very close to the creature.

    So we have to check a couple of bounding boxes.
    If we move our boxes by Box3DMinSize(B), we're sure that
    each box will stick to the previous and next. But maybe
    there will be some areas around the sticking points ?
    So B.MinSize / 2 seems safe. }
  DistanceIncrease := B.MinSize / 2;

  DistanceLength := DistanceIncrease;
  while DistanceLength < WAKind.MaxAttackDistance do
  begin
    if B.Translate(VectorScale(Direction, DistanceLength)).Collision(PB) then
      Exit(true);
    DistanceLength += DistanceIncrease;
  end;

  { Check one last time for WAKind.MaxAttackDistance }
  Result := B.Translate(
    VectorScale(Direction, WAKind.MaxAttackDistance)).Collision(PB);
end;

function TWalkAttackCreature.DebugCaption: string;
const
  StateName: array [TWalkAttackCreatureState] of string =
  ( 'Stand', 'Walk', 'Attack', 'Dying', 'DyingBack', 'Hurt', 'Special1' );
begin
  Result := (inherited DebugCaption) + ' ' + StateName[State];
end;

{ TBallThrowerCreature ------------------------------------------------------- }

procedure TBallThrowerCreature.ActualAttack;
const
  FiringMissileHeight = 0.6;
var
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3Single;
begin
  if HasLastSeenPlayer then
  begin
    MissilePosition := LerpLegsMiddlePosition(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := BallMissile.CreateDefaultCreature(
      MissilePosition, MissileDirection,
      Level.AnimationTime, Level.BaseLights, BallMissile.DefaultMaxLife);

    Level.AddCreature(Missile);

    Missile.Sound3d(stBallMissileFired, 0.0);
  end;
end;

{ TWerewolfCreature ---------------------------------------------------------- }

constructor TWerewolfCreature.Create(AKind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AMaxLife: Single;
  const AnimationTime: Single; const BaseLights: TLightInstancesList);
begin
  inherited;
  NextHowlTime := AnimationTime + Random * 60.0;
end;

procedure TWerewolfCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    Sound3d(stWerewolfActualAttackHit, 1.0);
    ShortRangeAttackHurt;
  end;
end;

procedure TWerewolfCreature.Idle(const CompSpeed: Single);
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  if (not Dead) and (Level.AnimationTime > NextHowlTime) then
    Howl(false);
end;

procedure TWerewolfCreature.Howl(ForceHowl: boolean);
begin
  { Howl only if player was seen, and only while walking/standing
    (not in the middle of attack e.g., since that would sound stupid). }
  if ForceHowl or (HasLastSeenPlayer and (State in [wasWalk, wasStand])) then
    Sound3d(stWerewolfHowling, 1.0);

  { Whether you actually howled or not, schedule next howl. }
  NextHowlTime := Level.AnimationTime + Random * 60.0;
end;

{ TSpiderCreature ---------------------------------------------------------- }

procedure TSpiderCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    Sound3d(stSpiderActualAttackHit, 1.0);
    ShortRangeAttackHurt;
  end;
end;

function TSpiderCreature.RemoveMeFromLevel: boolean;
begin
  { Spiders must be removed from level, otherwise too many of them
    stay on Cages level and slow down the rendering.
    dying animation is adjusted to scale the model down. }
  Result := (State = wasDying) and
    (Level.AnimationTime - StateChangeTime > WAKind.DyingAnimation.TimeEnd);
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
    ShortRangeAttackHurt;
  end;
end;

procedure TSpiderQueenCreature.SetState(Value: TWalkAttackCreatureState);
begin
  if (State <> Value) and (Value = wasSpecial1) then
  begin
    LastThrowWebAttackTime := Level.AnimationTime;
    ActualThrowWebAttackDone := false;
  end;

  inherited;
end;

procedure TSpiderQueenCreature.Idle(const CompSpeed: Single);

  procedure DoMaybeSwitchToThrowWebAttack;
  var
    ThrowWebAttackAllowed: boolean;
    AngleRadBetweenTheDirectionToPlayer: Single;
  begin
    ThrowWebAttackAllowed :=
      IdleSeesPlayer and
      (Level.AnimationTime - LastThrowWebAttackTime >
        SQKind.MinDelayBetweenThrowWebAttacks) and
      (IdleSqrDistanceToLastSeenPlayer <=
        Sqr(SQKind.MaxThrowWebAttackDistance));

    if ThrowWebAttackAllowed then
    begin
      { Calculate and check AngleRadBetweenTheDirectionToPlayer. }
      AngleRadBetweenTheDirectionToPlayer := AngleRadBetweenVectors(
        VectorSubtract(LastSeenPlayer, MiddlePosition),
        Direction);
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
    StateTime := Level.AnimationTime - StateChangeTime;

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

function TSpiderQueenCreature.CurrentScene: TCastleScene;
var
  StateTime: Single;
begin
  if State = wasSpecial1 then
  begin
    { Time from the change to this state. }
    if Level <> nil then
      StateTime := Level.AnimationTime - StateChangeTime else
      StateTime := 0;

    Result := SQKind.ThrowWebAttackAnimation.SceneFromTime(StateTime);
  end else
    Result := inherited;
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
    MissilePosition := LerpLegsMiddlePosition(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := ThrownWeb.CreateDefaultCreature(
      MissilePosition, MissileDirection,
      Level.AnimationTime, Level.BaseLights, ThrownWeb.DefaultMaxLife);

    Level.AddCreature(Missile);

    Missile.Sound3d(stThrownWebFired, 0.0);
  end;
end;

procedure TSpiderQueenCreature.SetLife(const Value: Single);
begin
  inherited;
end;

{ TGhostCreature ---------------------------------------------------------- }

procedure TGhostCreature.ActualAttack;
begin
  if ShortRangeActualAttackHits then
  begin
    ShortRangeAttackHurt;
  end;
end;

function TGhostCreature.CollisionsWithCreaturesAndPlayer: boolean;
begin
  Result := State <> wasDying;
end;

function TGhostCreature.RemoveMeFromLevel: boolean;
begin
  Result := (State = wasDying) and
    (Level.AnimationTime - StateChangeTime > WAKind.DyingAnimation.TimeEnd);
end;

{ TMissileCreature ----------------------------------------------------------- }

function TMissileCreature.MissileKind: TMissileCreatureKind;
begin
  Result := TMissileCreatureKind(Kind);
end;

procedure TMissileCreature.Idle(const CompSpeed: Single);
var
  NewMiddlePosition: TVector3Single;
  AngleBetween, AngleChange: Single;
  NewDirection, TargetDirection: TVector3Single;
  I: Integer;
begin
  inherited;
  if (not GetExists) or DebugTimeStopForCreatures then Exit;

  { Note that for now missiles are removed from level as soon as they are dead,
    so I don't bother with checking here Dead. Or UseBoundingSphere
    --- you can safely assume that UseBoundingSphere is @true here,
    and use Level.MoveAllowedSimple instead of Level.MoveBoxAllowedSimple. }

  NewMiddlePosition := VectorAdd(MiddlePosition,
    VectorScale(Direction, MissileKind.MoveSpeed * CompSpeed * 50));

  if Level.MoveAllowedSimple(MiddlePosition, NewMiddlePosition,
    false, Kind.CameraRadius) then
  begin
    LegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
  end else
    ExplodeWithLevel;

  { Check bounding Box of the missile <-> player's BoundingBox.
    Maybe I'll switch to using bounding Sphere here one day ?  }
  if MissileKind.HitsPlayer and LegsCollisionWithPlayer(LegsPosition) then
    ExplodeWithPlayer;

  if MissileKind.HitsCreatures then
  begin
    { Check bounding Sphere of the missile <-> creature's BoundingBox.
      Bounding Sphere is better for arrow, that has very large geometry
      but small enough bounding Sphere (because bounding Sphere radius
      is adjusted by creatures/kinds.xml). }
    for I := 0 to Level.Creatures.Count - 1 do
      if (Level.Creatures[I] <> Self) and
         (Level.Creatures[I].CollisionsWithCreaturesAndPlayer) and
         (Level.Creatures[I].BoundingBox.SphereSimpleCollision(
           MiddlePosition, Kind.CameraRadius)) then
      begin
        ExplodeWithCreature(Level.Creatures[I]);
        { TODO: projectiles shouldn't do here "break". }
        break;
      end;
  end;

  if MissileKind.FallsDown and
     (MissileKind.FallsDownSpeed <> 0) then
  begin
    NewDirection := Direction;
    NewDirection[2] -= MissileKind.FallsDownSpeed * CompSpeed * 50;

    { Above makes Direction potentially not normalized, but very slowly
      (MissileKind.FallsDownSpeed is very small...) so it would be a waste
      of time to call Normalize(Sqrt) each Idle. Call it only when it's really
      much needed. }
    if not Between(VectorLenSqr(NewDirection), Sqr(0.8), Sqr(1.2)) then
      NormalizeTo1st(NewDirection);

    Direction := NewDirection;
  end;

  if MissileKind.CloseDirectionToPlayer and
     (MissileKind.CloseDirectionToTargetSpeed <> 0) then
  begin
    TargetDirection := VectorSubtract(Player.Camera.Position,
      LegsPosition);
    AngleBetween := AngleRadBetweenVectors(TargetDirection, Direction);
    AngleChange := MissileKind.CloseDirectionToTargetSpeed * CompSpeed * 50;
    if AngleBetween <= AngleChange then
      Direction := Normalized(TargetDirection) else
    begin
      NewDirection := Direction;
      MakeVectorsAngleRadOnTheirPlane(NewDirection, TargetDirection,
        AngleBetween - AngleChange);
      Direction := Normalized(NewDirection);
    end;
  end;

  if (LastSoundIdleTime = 0) or
     (Level.AnimationTime - LastSoundIdleTime >
       MissileKind.PauseBetweenSoundIdle) then
  begin
    LastSoundIdleTime := Level.AnimationTime;
    Sound3d(MissileKind.SoundIdle, 0.0);
  end;
end;

function TMissileCreature.CurrentScene: TCastleScene;
begin
  if Level <> nil then
    Result := MissileKind.Animation.SceneFromTime(Level.AnimationTime) else
    Result := MissileKind.Animation.SceneFromTime(0);
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

  { This sound is done using GameSound.Sound3d, not our Sound3d
    --- because when the creature will be destroyed (and missile will
    be destroyed in nearest RemoveFromLevel pass), we want this sound
    to go on. }
  SoundEngine.Sound3d(MissileKind.SoundExplosion, LegsPosition);

  Life := 0.0;

  { Actually in this case setting LastAttackDirection is not really needed
    (TMissileCreature doesn't use it anyway), but I'm doing it for
    consistency. }
  LastAttackDirection := ZeroVector3Single;
end;

procedure TMissileCreature.ExplodeWithPlayer;
begin
  ExplodeCore;
  ShortRangeAttackHurt;
end;

procedure TMissileCreature.ExplodeWithLevel;
begin
  ExplodeCore;
end;

procedure TMissileCreature.ExplodeWithCreature(Creature: TCreature);
begin
  ExplodeCore;
  { TODO: knockback for creatures should be done here. }
  Creature.LastAttackDirection := Direction;
  Creature.Life := Creature.Life -
    (Kind.ShortRangeAttackDamageConst +
      Random * Kind.ShortRangeAttackDamageRandom);
end;

function TMissileCreature.CollisionsWithCreaturesAndPlayer: boolean;
begin
  Result := false;
end;

{ TStillCreature ----------------------------------------------------------- }

function TStillCreature.StillKind: TStillCreatureKind;
begin
  Result := TStillCreatureKind(Kind);
end;

function TStillCreature.CurrentScene: TCastleScene;
begin
  if Level <> nil then
    Result := StillKind.Animation.SceneFromTime(Level.AnimationTime) else
    Result := StillKind.Animation.SceneFromTime(0);
end;

function TStillCreature.RemoveMeFromLevel: boolean;
begin
  { TODO: do explosion anim. }
  Result := Life <= 0.0;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowClose(Window: TCastleWindowBase);
var
  I: Integer;
begin
  { In fact, CreaturesKinds will always be nil here, because
    WindowClose will be called from GameWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on CreaturesKinds.Items, and this implicitly did GLContextClose,
    so everything is OK. }

  if CreaturesKinds <> nil then
  begin
    for I := 0 to CreaturesKinds.Count - 1 do
      TCreatureKind(CreaturesKinds.Items[I]).GLContextClose;
  end;
end;

procedure DoInitialization;
begin
  Window.OnCloseList.Add(@WindowClose);

  CreaturesKinds := TCreatureKindList.Create(true);

  Alien := TBallThrowerCreatureKind.Create('Alien');
  Werewolf := TWerewolfKind.Create('Werewolf');
  BallMissile := TMissileCreatureKind.Create('BallMissile');
  Ghost := TGhostKind.Create('Ghost');
  Spider := TSpiderKind.Create('Spider');
  SpiderQueen := TSpiderQueenKind.Create('SpiderQueen');
  ThrownWeb := TMissileCreatureKind.Create('ThrownWeb');
  Arrow := TMissileCreatureKind.Create('Arrow');
  Barrel := TStillCreatureKind.Create('DoomBarrel');

  CreaturesKinds.LoadFromFile;
end;

procedure DoFinalization;
begin
  FreeAndNil(CreaturesKinds);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.