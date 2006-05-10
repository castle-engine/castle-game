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
  VRMLGLAnimationInfo, VRMLFlatSceneGL, CastleSound, VRMLSceneWaypoints,
  CastleObjectKinds, ALSourceAllocator, KambiXMLCfg;

{$define read_interface}

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
  protected
    { In descendants only PrepareRender can (and should!) set this. }
    CameraRadiusFromPrepareRender: Single;
  public
    constructor Create(const AVRMLNodeName: string);

    { If @true, then the creature flies. Otherwise it always tries to move only
      horizontally (which means that Direction is always orthogonal
      to Level.HomeCameraUp), and it falls down when Position is above
      the ground. }
    property Flying: boolean read FFlying write FFlying default DefaultFlying;

    { Camera radius when moving.
      You should make sure that it's something <> 0 for collision detection.

      This is always calculated like:
      Of CameraRadiusFromFile <> 0, then take it.
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

    { This will be used by CreateDefaultCreature to initialize
      MaxLife of the creature. Which means that not all instances
      of TCreature with this Kind *must* have this MaxLife,
      because you don't have to use CreateDefaultCreature to create
      new creatures... It's just some default, "suggested" MaxLife
      for this Kind. }
    property DefaultMaxLife: Single
      read FDefaultMaxLife write FDefaultMaxLife default DefaultDefaultMaxLife;

    { This creates the "default creature instance" with Kind = Self.
      This uses DefaultMaxLife. It uses the TCreature descendant
      most suitable for this TCreatureKind.

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
      const AnimationTime: Single): TCreature; virtual; abstract;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;

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
  end;

  TObjectsListItem_2 = TCreatureKind;
  {$I objectslist_2.inc}
  TCreaturesKindsList = class(TObjectsList_2)
  public
    { Calls PrepareRender for all items.
      This does Progress.Init, Step, Fini. }
    procedure PrepareRender;

    { Find item with given VRMLNodeName.
      @raises Exception if not found. }
    function FindByVRMLNodeName(const AVRMLNodeName: string): TCreatureKind;

    { This opens creatures/kinds.xml file and calls LoadFromFile for
      all existing TCreatureKind instances. }
    procedure LoadFromFile;
  end;

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
    procedure FreePrepareRender; override;
  public
    constructor Create(
      const AVRMLNodeName: string;
      AStandAnimationInfo: TVRMLGLAnimationInfo;
      AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
      AWalkAnimationInfo: TVRMLGLAnimationInfo;
      AAttackAnimationInfo: TVRMLGLAnimationInfo;
      ADyingAnimationInfo: TVRMLGLAnimationInfo;
      AHurtAnimationInfo: TVRMLGLAnimationInfo);

    destructor Destroy; override;

    { Make all TVRMLGLAnimation properties non-nil. I.e. load them from their
      XxxInfo counterparts.

      Also calculates CameraRadiusFromPrepareRender
      from StandAnimation.Scenes[0]. }
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
      Player.Navigator.CameraPos and creature's MiddlePosition. }
    property MaxAttackDistance: Single
      read FMaxAttackDistance write FMaxAttackDistance
      default DefaultMaxAttackDistance;

    { This is the preferred distance between player and the creature
      to perform the attack. This must always be <= MaxAttackDistance.
      The idea is that the creature can attack player from MaxAttackDistance,
      but still it will walk closer to the player --- until the distance
      is PreferredAttackDistance. }
    property PreferredAttackDistance: Single
      read FPreferredAttackDistance write FPreferredAttackDistance
      default DefaultPreferredAttackDistance;

    { This is the time point within AttackAnimation
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

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;

    { Because most of the creatures will have their weapon
      on their front (teeth, shooting hands, claws, whatever),
      so they can attack player only when their Direction is somewhat
      close to the direction to player.

      More precisely, the attack is allowed to start only when
      the angle between current Direction and the vector
      from creature's MiddlePosition to the player's CameraPos
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
      const AnimationTime: Single): TCreature; override;
  end;

  TWerewolfKind = class(TWalkAttackCreatureKind)
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single): TCreature; override;
  end;

  TSpiderKind = class(TWalkAttackCreatureKind)
  public
    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single): TCreature; override;
  end;

  TSpiderQueenKind = class(TWalkAttackCreatureKind)
  private
    FThrowWebAttackAnimation: TVRMLGLAnimation;
    FThrowWebAttackAnimationInfo: TVRMLGLAnimationInfo;

    FMinDelayBetweenThrowWebAttacks: Single;
    FMaxThrowWebAttackDistance: Single;
    FMaxAngleToThrowWebAttack: Single;
    FActualThrowWebAttackTime: Single;
  public
    constructor Create(
      const AVRMLNodeName: string;
      AStandAnimationInfo: TVRMLGLAnimationInfo;
      AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
      AWalkAnimationInfo: TVRMLGLAnimationInfo;
      AAttackAnimationInfo: TVRMLGLAnimationInfo;
      ADyingAnimationInfo: TVRMLGLAnimationInfo;
      AHurtAnimationInfo: TVRMLGLAnimationInfo;
      AThrowWebAttackAnimationInfo: TVRMLGLAnimationInfo);

    destructor Destroy; override;

    procedure CloseGL; override;

    procedure PrepareRender; override;
    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single): TCreature; override;

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

    property ThrowWebAttackAnimation: TVRMLGLAnimation
      read FThrowWebAttackAnimation;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;
  end;

  TGhostKind = class(TWalkAttackCreatureKind)
    procedure PrepareRender; override;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single): TCreature; override;
  end;

  { This is a missile. As you can see, this is also treated as a creature
    --- it's just a very dumb creature, that just moves into the given
    direction and explodes on any collision.

    Missile must be generally considered as Flying, otherwise
    it doesn't have much sense... Don't set Flying to false
    for this class (because TMissileCreature may depend on it and never
    cares to keep Direction horizontal). }
  TMissileCreatureKind = class(TCreatureKind)
  private
    FAnimation: TVRMLGLAnimation;
    FAnimationInfo: TVRMLGLAnimationInfo;
    FMoveSpeed: Single;
    FSoundExplosion: TSoundType;
    FCloseDirectionToPlayer: boolean;
    FCloseDirectionToTargetSpeed: Single;
    FPauseBetweenSoundIdle: Single;
    FSoundIdle: TSoundType;
    FHitsPlayer: boolean;
    FHitsCreatures: boolean;
  protected
    procedure FreePrepareRender; override;
  public
    constructor Create(
      const AVRMLNodeName: string;
      AAnimationInfo: TVRMLGLAnimationInfo);
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

    property SoundExplosion: TSoundType
      read FSoundExplosion write FSoundExplosion default stNone;

    function CreateDefaultCreature(
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AnimationTime: Single): TCreature; override;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;

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

    { We use our own SavedShadowQuads, to not waste so much memory
      (otherwise each CurrentScene would use it's own
      DefaultSavedShadowQuads instance) }
    SavedShadowQuads: TDynQuad3SingleArray;

    UsedSounds: TALAllocatedSourcesList;

    procedure SoundSourceUsingEnd(Sender: TALAllocatedSource);
  protected
    { Return matrix that takes into account current LegsPosition and Direction.
      Multiply CurrentScene geometry by this matrix to get current geometry. }
    function SceneTransform: TMatrix4Single;

    { Like SceneTransform, but assumes that LegsPosition and Direction
      is as specified. }
    function SceneTransformAssuming(
      const AssumeLegsPosition, AssumeDirection: TVector3Single): TMatrix4Single;

    { These define exactly what "MiddlePosition" means for this creature.

      LegsPositionFromMiddle must always specify reverse function.

      VLerpLegsMiddlePosition must always be equal to
      VLerp(A, LegsPosition, MiddlePosition)
      (but usually can be calculated more efficiently than calling VLerp).

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
    function VLerpLegsMiddlePosition(
      const A: Single): TVector3Single; virtual;
    { @groupEnd }

    { Returns BoundingBox, assuming that LegsPosition and Direction are
      as specified here. }
    function BoundingBoxAssumingLegs(
      const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3d;

    function BoundingBoxAssumingMiddle(
      const AssumeMiddlePosition, AssumeDirection: TVector3Single): TBox3d;

    { This checks collision with Player.BoundingBox, assuming that MiddlePosition
      (and implied LegsPosition) is as given. }
    function MiddleCollisionWithPlayer(
      const AssumeMiddlePosition: TVector3Single): boolean;

    function LegsCollisionWithPlayer(
      const AssumeLegsPosition: TVector3Single): boolean;

    procedure SetLife(const Value: Single); virtual;

    { Tries to move from OldMiddlePosition to ProposedNewMiddlePosition.
      Returns true and sets NewMiddlePosition if some move is allowed.

      Note that OldMiddlePosition *must be equal to MiddlePosition*.
      It's passed here only for speed.

      Check collisions with the level, with player, and with other
      creatures. }
    function MoveAllowed(
      const OldMiddlePosition, ProposedNewMiddlePosition: TVector3Single;
      var NewMiddlePosition: TVector3Single;
      BecauseOfGravity: boolean): boolean;

    { Like MoveAllowed, but this is only a "yes/no" collision check. }
    function MoveAllowedSimple(
      const OldMiddlePosition, NewMiddlePosition: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    { Checks AssumeMiddlePosition height above the level and other creatures.

      I don't check height above the player, this is not needed
      (GetCameraHeight is needed only for "growing up" and "falling down";
      in case of "growing up", creature doesn't have to "grow up"
      when standing on player's head. In case of "falling down" ---
      we don't have to take this into account. Things will work correctly
      anyway.) }
    procedure GetCameraHeight(
      const AssumeMiddlePosition: TVector3Single;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);

    procedure ShortRangeAttackHurt;
  public
    { Constructor. Note for AnimationTime: usually I will take
      AnimationTime from global Level.AnimationTime, but in the case of
      constructor it's safer to just take it as param. }
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single);

    destructor Destroy; override;

    { Current Life. Initially set from MaxLife. }
    property Life: Single read FLife write SetLife;

    property MaxLife: Single read FMaxLife write FMaxLife;

    property Kind: TCreatureKind read FKind;

    function BoundingBox: TBox3d; virtual;

    procedure Render(const Frustum: TFrustum); virtual;

    procedure RenderFrontShadowQuads(
      const LightPosition, CameraPosition: TVector3Single); virtual;

    procedure RenderBackShadowQuads; virtual;

    procedure Idle(const CompSpeed: Single); virtual;

    function CurrentScene: TVRMLFlatSceneGL; virtual; abstract;

    { This is the position of the (0, 0, 0) point of creature model
      (or rather, currently used model! Creatures are animated after all). }
    property LegsPosition: TVector3Single read FLegsPosition write FLegsPosition;

    { This is the height of MiddlePosition above LegsPosition.
      In this class, just a shortcut for CurrentScene.BoundingBox[1, 2].
      Note that while CurrentScene may change, this also may change. }
    function HeightBetweenLegsAndMiddle: Single; virtual;

    { This is the "middle" position of the creature.
      For some creatures it can be considered the position of their "heads".
      How precisely this is calculated for given creature depends
      on MiddlePositionFromLegs implementation in this class.

      All collision detection (MoveAllowed, GetCameraHeight)
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
      You can set this to (0, 0, 0) (ZeroVector3Single)
      if there was no specific direction of attack.

      On set, this vector will be normalized. }
    property LastAttackDirection: TVector3Single
      read FLastAttackDirection write SetLastAttackDirection;

    { If @false, then TCreaturesList.MoveAllowedSimple and
      TCreaturesList.GetCameraHeight will ignore this
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
      vlerp between LegsPosition and MiddlePosition.

      If TiedToCreature then the sounds position will be updated
      as the creature will move, and when the creature object will
      be destroyed, sound will stop. If not TiedToCreature, then
      the sound will simply be done at creature's position, but then
      it will continue to be played independent of this creature. }
    procedure Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
      TiedToCreature: boolean = true);
  end;

  TObjectsListItem_1 = TCreature;
  {$I objectslist_1.inc}
  TCreaturesList = class(TObjectsList_1)
    procedure Render(const Frustum: TFrustum; const Transparent: boolean);
    procedure Idle(const CompSpeed: Single);

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
      const OldBoundingBox, NewBoundingBox: TBox3d;
      const OldPosition, NewPosition: TVector3Single;
      IgnoreCreature: TCreature): TCreature;

    { Height of Position over creatures' bounding boxes.

      Assumes IsAboveTheGround is already initialized
      (and if true, then SqrHeightAboveTheGround is already initialized too).
      It will update them.

      You can pass IgnoreCreature <> nil if you want to ignore
      collisions with given creature (this will obviously be useful
      when checking for collisions for this creature). }
    procedure GetCameraHeight(const Position: TVector3Single;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single;
      IgnoreCreature: TCreature);

    { Searches for item of given Kind. Returns index of first found,
      or -1 if not found. }
    function FindKind(Kind: TCreatureKind): Integer;
  end;

  TWalkAttackCreatureState = (wasStand, wasWalk, wasAttack, wasDying, wasHurt,
    wasSpecial1);

  { This is TCreature that has a kind always of TWalkAttackCreatureKind. }
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
    WaypointsSaved: TSceneWaypointsList;
  protected
    procedure SetState(Value: TWalkAttackCreatureState); virtual;

    { Use this in ActualAttack for short range creatures. }
    function ShortRangeActualAttackHits: boolean;

    { Set by Idle in this class, may be used by descendants
      in their Idle calls (to not calculate the same thing twice). }
    IdleSeesPlayer: boolean;
    IdleSqrDistanceToLastSeenPlayer: Single;

    procedure SetLife(const Value: Single); override;
  public
    constructor Create(AKind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single;
      const AMaxLife: Single;
      const AnimationTime: Single);

    destructor Destroy; override;

    { Shortcut for TWalkAttackCreatureKind(Kind). }
    function WAKind: TWalkAttackCreatureKind;

    property State: TWalkAttackCreatureState read FState
      default wasStand;

    { Last State change time, taken from Level.AnimationTime. }
    property StateChangeTime: Single read FStateChangeTime;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TVRMLFlatSceneGL; override;

    { This is the method where you must actually do your attack
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
  public
    procedure Idle(const CompSpeed: Single); override;
    procedure ActualAttack; override;
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
    function CurrentScene: TVRMLFlatSceneGL; override;
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

    function HeightBetweenLegsAndMiddle: Single; override;
  end;

  { This is TCreature that has a kind always of TMissileCreatureKind. }
  TMissileCreature = class(TCreature)
  private
    procedure ExplodeCore;
    procedure ExplodeWithPlayer;
    procedure ExplodeWithLevel;
    procedure ExplodeWithCreature(Creature: TCreature);
    LastSoundIdleTime: Single;
  public
    { Shortcut for TMissileCreatureKind(Kind). }
    function MissileKind: TMissileCreatureKind;

    procedure Idle(const CompSpeed: Single); override;

    function CurrentScene: TVRMLFlatSceneGL; override;

    { Missiles return @false here.
      We will check for collisions when missile moves. }
    function CollisionsWithCreaturesAndPlayer: boolean; override;

    function RemoveMeFromLevel: boolean; override;
  end;

var
  CreaturesKinds: TCreaturesKindsList;

  Alien: TBallThrowerCreatureKind;
  Werewolf: TWerewolfKind;
  BallMissile: TMissileCreatureKind;
  Ghost: TGhostKind;
  Spider: TSpiderKind;
  SpiderQueen: TSpiderQueenKind;
  ThrownWeb: TMissileCreatureKind;
  Arrow: TMissileCreatureKind;

  WasParam_DebugNoCreatures: boolean = false;

{$undef read_interface}

implementation

uses SysUtils, Classes, OpenGLh, CastleWindow, GLWindow,
  VRMLNodes, KambiFilesUtils, KambiGLUtils, ProgressUnit, CastlePlay,
  CastleLevel, CastleVideoOptions, OpenAL, ALUtils,
  CastleTimeMessages, CastleItems;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TCreatureKind -------------------------------------------------------------- }

constructor TCreatureKind.Create(const AVRMLNodeName: string);
begin
  inherited Create(AVRMLNodeName);
  FFlying := DefaultFlying;
  FDefaultMaxLife := DefaultDefaultMaxLife;
  FSoundDyingTiedToCreature := DefaultSoundDyingTiedToCreature;
  FShortRangeAttackDamageConst := DefaultShortRangeAttackDamageConst;
  FShortRangeAttackDamageRandom := DefaultShortRangeAttackDamageRandom;
  FShortRangeAttackKnockbackDistance := DefaultShortRangeAttackKnockbackDistance;
  FFallDownLifeLossScale := DefaultFallDownLifeLossScale;
  CreaturesKinds.Add(Self);
end;

procedure TCreatureKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  inherited;

  Flying := KindsConfig.GetValue(VRMLNodeName + '/flying',
    DefaultFlying);
  SoundDyingTiedToCreature :=
    KindsConfig.GetValue(VRMLNodeName + '/sound_dying_tied_to_creature',
    DefaultSoundDyingTiedToCreature);
  DefaultMaxLife := KindsConfig.GetFloat(VRMLNodeName + '/default_max_life',
    DefaultDefaultMaxLife);
  CameraRadiusFromFile := KindsConfig.GetFloat(VRMLNodeName + '/camera_radius',
    0.0);
  ShortRangeAttackDamageConst :=
    KindsConfig.GetFloat(VRMLNodeName + '/short_range_attack/damage/const',
    DefaultShortRangeAttackDamageConst);
  ShortRangeAttackDamageRandom :=
    KindsConfig.GetFloat(VRMLNodeName + '/short_range_attack/damage/random',
    DefaultShortRangeAttackDamageRandom);
  ShortRangeAttackKnockbackDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/short_range_attack/knockback_distance',
    DefaultShortRangeAttackKnockbackDistance);

  FallDownLifeLossScale :=
    KindsConfig.GetFloat(VRMLNodeName + '/fall_down_life_loss_scale',
    DefaultFallDownLifeLossScale)
end;

function TCreatureKind.CameraRadius: Single;
begin
  if CameraRadiusFromFile <> 0 then
    Result := CameraRadiusFromFile else
    Result := CameraRadiusFromPrepareRender;
end;

{ TCreaturesKindsList -------------------------------------------------------- }

{ $define WRITELN_ANIMATION_INFO}

procedure TCreaturesKindsList.PrepareRender;

  {$ifdef WRITELN_ANIMATION_INFO}
  procedure WritelnAnimationsInfo;
  var
    TrianglesCount: Cardinal;

    procedure WritelnAnimInfo(Animation: TVRMLGLAnimation; Name: string);
    begin
      Writeln('    ', Name: 20, ': ',
        Animation.ScenesCount: 3, ' scenes * ',
        Animation.Scenes[0].TrianglesCount(true): 8, ' triangles');
      TrianglesCount += Animation.ScenesCount *
        Animation.Scenes[0].TrianglesCount(true);
    end;

  var
    I: Integer;
  begin
    Writeln('Animations in memory:');
    for I := 0 to High do
    begin
      TrianglesCount := 0;
      Writeln('  ', Items[I].VRMLNodeName);
      if Items[I] is TWalkAttackCreatureKind then
      begin
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).StandAnimation, 'Stand');
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).StandToWalkAnimation, 'StandToWalk');
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).WalkAnimation, 'Walk');
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).AttackAnimation, 'Attack');
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).DyingAnimation, 'Dying');
        WritelnAnimInfo(TWalkAttackCreatureKind(Items[I]).HurtAnimation, 'Hurt');
        if Items[I] is TSpiderQueenKind then
          WritelnAnimInfo(TSpiderQueenKind(Items[I]).ThrowWebAttackAnimation, 'ThrowWebAttack');
      end;
      if Items[I] is TMissileCreatureKind then
        WritelnAnimInfo(TMissileCreatureKind(Items[I]).Animation, '(Standard)');
      Writeln('  Total ', TrianglesCount);
    end;
  end;
  {$endif WRITELN_ANIMATION_INFO}

var
  I: Integer;
  PrepareRenderSteps: Cardinal;
begin
  if not WasParam_DebugNoCreatures then
  begin
    PrepareRenderSteps := 0;
    for I := 0 to High do
      PrepareRenderSteps +=  Items[I].PrepareRenderSteps;

    Progress.Init(PrepareRenderSteps, 'Loading creatures');
    try
      for I := 0 to High do
        Items[I].PrepareRender;
    finally Progress.Fini; end;

    { Tests : }
    {$ifdef WRITELN_ANIMATION_INFO}
    WritelnAnimationsInfo;
    {$endif}
  end;
end;

function TCreaturesKindsList.FindByVRMLNodeName(
  const AVRMLNodeName: string): TCreatureKind;
var
  I: Integer;
begin
  for I := 0 to High do
  begin
    Result := Items[I];
    if Result.VRMLNodeName = AVRMLNodeName then
      Exit;
  end;

  raise Exception.CreateFmt('Not existing creature kind name "%s"',
    [AVRMLNodeName]);
end;

procedure TCreaturesKindsList.LoadFromFile;
var
  I: Integer;
  KindsConfig: TKamXMLConfig;
begin
  KindsConfig := TKamXMLConfig.Create(nil);
  try
    KindsConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'creatures' + PathDelim + 'kinds.xml';

    for I := 0 to High do
    begin
      Items[I].LoadFromFile(KindsConfig);
    end;
  finally SysUtils.FreeAndNil(KindsConfig); end;
end;

{ TWalkAttackCreatureKind ---------------------------------------------------- }

constructor TWalkAttackCreatureKind.Create(
  const AVRMLNodeName: string;
  AStandAnimationInfo: TVRMLGLAnimationInfo;
  AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
  AWalkAnimationInfo: TVRMLGLAnimationInfo;
  AAttackAnimationInfo: TVRMLGLAnimationInfo;
  ADyingAnimationInfo: TVRMLGLAnimationInfo;
  AHurtAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create(AVRMLNodeName);

  FStandAnimationInfo := AStandAnimationInfo;
  FStandToWalkAnimationInfo := AStandToWalkAnimationInfo;
  FWalkAnimationInfo := AWalkAnimationInfo;
  FAttackAnimationInfo := AAttackAnimationInfo;
  FDyingAnimationInfo := ADyingAnimationInfo;
  FHurtAnimationInfo := AHurtAnimationInfo;

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
    AnimationAttributesSet(Anim.Attributes);
    Anim.PrepareRender(false, true, RenderShadowsPossible, false, false);
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

  CameraRadiusFromPrepareRender :=
    Min(Box3dXYRadius(StandAnimation.Scenes[0].BoundingBox),
        StandAnimation.Scenes[0].BoundingBox[1, 2] * 0.75);
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
  FreeAndNil(FHurtAnimation);

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
  if HurtAnimation <> nil then HurtAnimation.CloseGL;
end;

procedure TWalkAttackCreatureKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  inherited;

  ActualAttackTime :=
    KindsConfig.GetFloat(VRMLNodeName + '/actual_attack_time',
    DefaultActualAttackTime);
  MoveSpeed :=
    KindsConfig.GetFloat(VRMLNodeName + '/move_speed',
    DefaultMoveSpeed);
  MaxAttackDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/max_attack_distance',
    DefaultMaxAttackDistance);
  PreferredAttackDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/preferred_attack_distance',
    DefaultPreferredAttackDistance);
  MinDelayBetweenAttacks :=
    KindsConfig.GetFloat(VRMLNodeName + '/min_delay_between_attacks',
    DefaultMinDelayBetweenAttacks);
  LifeToRunAway :=
    KindsConfig.GetFloat(VRMLNodeName + '/life_to_run_away',
    DefaultLifeToRunAway);
  MaxKnockedBackDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/max_knocked_back_distance',
    DefaultMaxKnockedBackDistance);
  MaxAngleToAttack :=
    KindsConfig.GetFloat(VRMLNodeName + '/max_angle_to_attack',
    DefaultMaxAngleToAttack);
  MinLifeLossToHurt :=
    KindsConfig.GetFloat(VRMLNodeName + '/min_life_loss_to_hurt',
    DefaultMinLifeLossToHurt);
  ChanceToHurt :=
    KindsConfig.GetFloat(VRMLNodeName + '/chance_to_hurt',
    DefaultChanceToHurt);
  MaxHeightAcceptableToFall :=
    KindsConfig.GetFloat(VRMLNodeName + '/max_height_acceptable_to_fall',
    DefaultMaxHeightAcceptableToFall);
  RandomWalkDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/random_walk_distance',
    DefaultCreatureRandomWalkDistance);
end;

{ TBallThrowerCreatureKind --------------------------------------------------- }

function TBallThrowerCreatureKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TBallThrowerCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

{ TWerewolfKind -------------------------------------------------------------- }

function TWerewolfKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TWerewolfCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

{ TSpiderKind -------------------------------------------------------------- }

function TSpiderKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TSpiderCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

{ TSpiderQueenKind -------------------------------------------------------- }

constructor TSpiderQueenKind.Create(
  const AVRMLNodeName: string;
  AStandAnimationInfo: TVRMLGLAnimationInfo;
  AStandToWalkAnimationInfo: TVRMLGLAnimationInfo;
  AWalkAnimationInfo: TVRMLGLAnimationInfo;
  AAttackAnimationInfo: TVRMLGLAnimationInfo;
  ADyingAnimationInfo: TVRMLGLAnimationInfo;
  AHurtAnimationInfo: TVRMLGLAnimationInfo;
  AThrowWebAttackAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create(
    AVRMLNodeName,
    AStandAnimationInfo,
    AStandToWalkAnimationInfo,
    AWalkAnimationInfo,
    AAttackAnimationInfo,
    ADyingAnimationInfo,
    AHurtAnimationInfo);

  FThrowWebAttackAnimationInfo := AThrowWebAttackAnimationInfo;
end;

destructor TSpiderQueenKind.Destroy;
begin
  FreeAndNil(FThrowWebAttackAnimation);
  FreeAndNil(FThrowWebAttackAnimationInfo);
  inherited;
end;

procedure TSpiderQueenKind.CloseGL;
begin
  inherited;
  if ThrowWebAttackAnimation <> nil then ThrowWebAttackAnimation.CloseGL;
end;

procedure TSpiderQueenKind.PrepareRender;

  procedure CreateIfNeeded(var Anim: TVRMLGLAnimation;
    AnimInfo: TVRMLGLAnimationInfo);
  begin
    if Anim = nil then
      Anim := AnimInfo.CreateAnimation;
    Progress.Step;
    AnimationAttributesSet(Anim.Attributes);
    Anim.PrepareRender(false, true, RenderShadowsPossible, false, false);
    Progress.Step;
  end;

begin
  inherited;
  CreateIfNeeded(FThrowWebAttackAnimation, FThrowWebAttackAnimationInfo);
end;

function TSpiderQueenKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TSpiderQueenKind.FreePrepareRender;
begin
  FreeAndNil(ThrowWebAttackAnimation);
  inherited;
end;

function TSpiderQueenKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TSpiderQueenCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

procedure TSpiderQueenKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  inherited;

  MinDelayBetweenThrowWebAttacks :=
    KindsConfig.GetFloat(VRMLNodeName + '/throw_web/min_delay_between_attacks', 0.0);
  MaxThrowWebAttackDistance :=
    KindsConfig.GetFloat(VRMLNodeName + '/throw_web/max_attack_distance', 0.0);
  MaxAngleToThrowWebAttack :=
    KindsConfig.GetFloat(VRMLNodeName + '/throw_web/max_angle_to_attack', 0.0);
  ActualThrowWebAttackTime :=
    KindsConfig.GetFloat(VRMLNodeName + '/throw_web/actual_attack_time', 0.0);
end;

{ TGhostKind ------------------------------------------------------------- }

procedure TGhostKind.PrepareRender;
var
  ReferenceScene: TVRMLFlatSceneGL;
begin
  inherited;

  { For Flying creatures, larger CameraRadius (that *really* surrounds whole
    model from MiddlePosition) is better. }
  ReferenceScene := StandAnimation.Scenes[0];

  CameraRadiusFromPrepareRender :=
    Max(Box3dXYRadius(ReferenceScene.BoundingBox),
    { I can do here "/ 2" thanks to implementation of
      TGhostCreature.HeightBetweenLegsAndMiddle, that sets uses
      CurrentScene.BoundingBox[1, 2] / 2. }
    ReferenceScene.BoundingBox[1, 2] / 2);
end;

function TGhostKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TGhostCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

{ TMissileCreatureKind ---------------------------------------------------- }

constructor TMissileCreatureKind.Create(
  const AVRMLNodeName: string;
  AAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create(AVRMLNodeName);
  FAnimationInfo := AAnimationInfo;
  Flying := true;
  FMoveSpeed := DefaultMissileMoveSpeed;
  FCloseDirectionToPlayer := DefaultCloseDirectionToPlayer;
  FCloseDirectionToTargetSpeed := DefaultCloseDirectionToTargetSpeed;
  FPauseBetweenSoundIdle := DefaultPauseBetweenSoundIdle;
  FHitsPlayer := DefaultHitsPlayer;
  FHitsCreatures := DefaultHitsCreatures;
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

procedure TMissileCreatureKind.PrepareRender;

  procedure CreateIfNeeded(var Anim: TVRMLGLAnimation;
    AnimInfo: TVRMLGLAnimationInfo);
  begin
    if Anim = nil then
      Anim := AnimInfo.CreateAnimation;
    Progress.Step;
    AnimationAttributesSet(Anim.Attributes);
    Anim.PrepareRender(false, true, RenderShadowsPossible, false, false);
    Progress.Step;
  end;

begin
  inherited;
  CreateIfNeeded(FAnimation, FAnimationInfo);

  CameraRadiusFromPrepareRender :=
    Box3dXYRadius(Animation.Scenes[0].BoundingBox);
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

function TMissileCreatureKind.CreateDefaultCreature(
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single;
  const AnimationTime: Single): TCreature;
begin
  Result := TMissileCreature.Create(Self, ALegsPosition, ADirection,
    DefaultMaxLife, AnimationTime);
end;

procedure TMissileCreatureKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  inherited;

  MoveSpeed :=
    KindsConfig.GetFloat(VRMLNodeName + '/move_speed',
    DefaultMissileMoveSpeed);
  CloseDirectionToPlayer :=
    KindsConfig.GetValue(VRMLNodeName + '/close_direction_to_player',
    DefaultCloseDirectionToPlayer);
  CloseDirectionToTargetSpeed :=
    KindsConfig.GetFloat(VRMLNodeName + '/close_direction_to_target_speed',
    DefaultCloseDirectionToTargetSpeed);
  PauseBetweenSoundIdle :=
    KindsConfig.GetFloat(VRMLNodeName + '/pause_between_sound_idle',
    DefaultPauseBetweenSoundIdle);
  HitsPlayer :=
    KindsConfig.GetValue(VRMLNodeName + '/hits_player',
    DefaultHitsPlayer);
  HitsCreatures :=
    KindsConfig.GetValue(VRMLNodeName + '/hits_creatures',
    DefaultHitsCreatures);
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
  const AnimationTime: Single);
begin
  inherited Create;

  { If --debug-no-creatures, then we actually load the creature now.
    If not, then we depend on CreaturesKinds.PrepareRender call
    to call our PrepareRender (this *may* happen after TCreature creation
    --- when loading new creatures designed on a level, TLevel constructor
    creates creatures and then PlayGame calls CreaturesKinds.PrepareRender). }
  if WasParam_DebugNoCreatures then
    if not AKind.PrepareRenderDone then
      AKind.RedoPrepareRender;

  FKind := AKind;
  FLegsPosition := ALegsPosition;
  FDirection := Normalized(ADirection);
  FMaxLife := AMaxLife;

  FLife := MaxLife;

  SavedShadowQuads := TDynQuad3SingleArray.Create;

  UsedSounds := TALAllocatedSourcesList.Create;
end;

destructor TCreature.Destroy;
var
  I: Integer;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.High do
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

  FreeAndNil(SavedShadowQuads);
  inherited;
end;

procedure TCreature.SoundSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  Sender.OnUsingEnd := nil;
  UsedSounds.Delete(Sender);
end;

procedure TCreature.Sound3d(const SoundType: TSoundType; const SoundHeight: Single;
  TiedToCreature: boolean);
var
  NewSource: TALAllocatedSource;
  SoundPosition: TVector3Single;
begin
  SoundPosition := VLerpLegsMiddlePosition(SoundHeight);
  NewSource := CastleSound.Sound3d(SoundType, SoundPosition);
  if TiedToCreature and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnUsingEnd := SoundSourceUsingEnd;
    NewSource.UserData := TCreatureSoundSourceData.Create;
  end;
end;

function TCreature.HeightBetweenLegsAndMiddle: Single;
begin
  Result := CurrentScene.BoundingBox[1, 2];
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

function TCreature.VLerpLegsMiddlePosition(const A: Single): TVector3Single;
begin
  Result := LegsPosition;
  Result[2] += HeightBetweenLegsAndMiddle * A;
end;

function TCreature.MiddlePosition: TVector3Single;
begin
  Result := MiddlePositionFromLegs(LegsPosition);
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

function TCreature.BoundingBoxAssumingLegs(
  const AssumeLegsPosition, AssumeDirection: TVector3Single): TBox3d;
begin
  Result := BoundingBoxTransform(CurrentScene.BoundingBox,
    SceneTransformAssuming(AssumeLegsPosition, AssumeDirection));
end;

function TCreature.BoundingBoxAssumingMiddle(
  const AssumeMiddlePosition, AssumeDirection: TVector3Single): TBox3d;
var
  AssumeLegsPosition: TVector3Single;
begin
  AssumeLegsPosition := LegsPositionFromMiddle(AssumeMiddlePosition);
  Result := BoundingBoxAssumingLegs(AssumeLegsPosition, AssumeDirection);
end;

function TCreature.BoundingBox: TBox3d;
begin
  Result := BoundingBoxAssumingLegs(LegsPosition, Direction);
end;

procedure TCreature.Render(const Frustum: TFrustum);

  procedure RenderBoundingGeometry;
  var
    Q: PGLUQuadric;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);

      DrawGLBoxWire(BoundingBox, 0, 0, 0, true);

      glPushMatrix;
        glTranslatev(MiddlePosition);
        Q := NewGLUQuadric(GL_FALSE, GLU_NONE, GLU_OUTSIDE, GLU_LINE);
        try
          gluSphere(Q, Kind.CameraRadius, 10, 10);
        finally gluDeleteQuadric(Q); end;
      glPopMatrix;
    glPopAttrib;
  end;

begin
  if FrustumBox3dCollisionPossibleSimple(Frustum, BoundingBox) then
  begin
    glPushMatrix;
      glMultMatrix(SceneTransform);
      CurrentScene.Render(nil);
    glPopMatrix;

    if RenderBoundingBoxes then
      RenderBoundingGeometry;
  end;
end;

procedure TCreature.RenderFrontShadowQuads(
  const LightPosition, CameraPosition: TVector3Single);
begin
  CurrentScene.RenderFrontShadowQuads(LightPosition, CameraPosition,
    SceneTransform, SavedShadowQuads);
end;

procedure TCreature.RenderBackShadowQuads;
begin
  CurrentScene.RenderBackShadowQuads(SavedShadowQuads);
end;

function TCreature.MiddleCollisionWithPlayer(
  const AssumeMiddlePosition: TVector3Single): boolean;
begin
  Result := Boxes3dCollision(
    BoundingBoxAssumingMiddle(AssumeMiddlePosition, Direction), Player.BoundingBox);
end;

function TCreature.LegsCollisionWithPlayer(
  const AssumeLegsPosition: TVector3Single): boolean;
begin
  Result := Boxes3dCollision(
    BoundingBoxAssumingLegs(AssumeLegsPosition, Direction), Player.BoundingBox);
end;

function TCreature.MoveAllowed(
  const OldMiddlePosition, ProposedNewMiddlePosition: TVector3Single;
  var NewMiddlePosition: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    { Check creature<->level collision. }
    Level.MoveAllowed(
      OldMiddlePosition, ProposedNewMiddlePosition, NewMiddlePosition,
      BecauseOfGravity, Kind.CameraRadius) and
    { Check creature<->player collision. }
    (not MiddleCollisionWithPlayer(NewMiddlePosition)) and
    { Check creature<->other creatures collision. }
    (Level.Creatures.MoveAllowedSimple(
      BoundingBox,
      BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
      OldMiddlePosition, NewMiddlePosition, Self) = nil);
end;

function TCreature.MoveAllowedSimple(
  const OldMiddlePosition, NewMiddlePosition: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    { Check creature<->level collision. }
    Level.MoveAllowedSimple(OldMiddlePosition, NewMiddlePosition,
      BecauseOfGravity, Kind.CameraRadius) and
    { Check creature<->player collision. }
    (not MiddleCollisionWithPlayer(NewMiddlePosition)) and
    { Check creature<->other creatures collision. }
    (Level.Creatures.MoveAllowedSimple(
      BoundingBox,
      BoundingBoxAssumingMiddle(NewMiddlePosition, Direction),
      OldMiddlePosition, NewMiddlePosition, Self) = nil);
end;

procedure TCreature.GetCameraHeight(
  const AssumeMiddlePosition: TVector3Single;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
begin
  { Check creature<->level collision. }
  Level.GetCameraHeight(AssumeMiddlePosition,
    IsAboveTheGround, SqrHeightAboveTheGround);

  { Check creature<->other creatures collision. }
  Level.Creatures.GetCameraHeight(AssumeMiddlePosition,
    IsAboveTheGround, SqrHeightAboveTheGround, Self);
end;

procedure TCreature.Idle(const CompSpeed: Single);

  procedure UpdateUsedSounds;
  var
    I: Integer;
    SoundPosition: TVector3Single;
  begin
    for I := 0 to UsedSounds.High do
    begin
      SoundPosition := VLerpLegsMiddlePosition(
        TCreatureSoundSourceData(UsedSounds[I].UserData).SoundHeight);
      alSourceVector3f(UsedSounds[I].ALSource, AL_POSITION, SoundPosition);
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

      Result := MoveAllowed(OldMiddlePosition, ProposedNewMiddlePosition,
        NewMiddlePosition, true);

      if Result then
        FLegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
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
    FallingDownDistance: Single;
  begin
    { Gravity does it's work here.
      This is extremely simplified version of Gravity work in MatrixNavigation.
      (simplified, because creature doesn't need all these effects). }

    { Note that also here we do collision detection using MiddlePosition,
      not LegsPosition. See MiddlePosition docs for reasoning. }

    OldIsFallingDown := FIsFallingDown;
    OldMiddlePosition := MiddlePosition;

    GetCameraHeight(OldMiddlePosition, IsAboveTheGround,
      SqrHeightAboveTheGround);
    { We will need it anyway. OK, I'll pay this Sqrt. }
    HeightAboveTheGround := Sqrt(SqrHeightAboveTheGround);

    if (not IsAboveTheGround) or
      (HeightAboveTheGround > HeightBetweenLegsAndMiddle * HeightMargin) then
    begin
      { Fall down }
      if not FIsFallingDown then
        FallingDownStartHeight := LegsPosition[2];

      FIsFallingDown := true;

      FallingDownDistance := FallingDownSpeed * CompSpeed;
      if IsAboveTheGround then
        MinTo1st(FallingDownDistance,
          HeightAboveTheGround - HeightBetweenLegsAndMiddle);

      if not MoveVertical(-FallingDownDistance) then
        FIsFallingDown := false;
    end else
    begin
      FIsFallingDown := false;

      if IsAboveTheGround and
        (HeightAboveTheGround < HeightBetweenLegsAndMiddle / HeightMargin) then
      begin
        { Growing up }
        MoveVertical(Min(GrowingUpSpeed * CompSpeed,
          HeightBetweenLegsAndMiddle - HeightAboveTheGround));
      end;
    end;

    if OldIsFallingDown and (not FIsFallingDown) then
      FalledDown;
  end;

begin
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

{ TCreatures ----------------------------------------------------------------- }

procedure TCreaturesList.Render(const Frustum: TFrustum;
  const Transparent: boolean);
var
  I: Integer;
begin
  for I := 0 to High do
    if Items[I].Kind.Transparent = Transparent then
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

function TCreaturesList.MoveAllowedSimple(
  const OldBoundingBox, NewBoundingBox: TBox3d;
  const OldPosition, NewPosition: TVector3Single;
  IgnoreCreature: TCreature): TCreature;
var
  I: Integer;
begin
  for I := 0 to High do
  begin
    Result := Items[I];
    if (Result <> IgnoreCreature) and
      Result.CollisionsWithCreaturesAndPlayer then
    begin
      if Boxes3dCollision(NewBoundingBox, Result.BoundingBox) then
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
        if (not Boxes3dCollision(OldBoundingBox, Result.BoundingBox)) or
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
      if (not Boxes3dCollision(OldBoundingBox, Result.BoundingBox)) and
         IsBox3dSegmentCollision(Result.BoundingBox, OldPosition, NewPosition) then
        Exit;
    end;
  end;

  Result := nil;
end;

procedure TCreaturesList.GetCameraHeight(
  const Position: TVector3Single;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single;
  IgnoreCreature: TCreature);

  { If the Point is inside the Box then it answers IsAboveTheBox := false. }
  procedure GetPointHeightAboveBox3d(const Point: TVector3Single;
    const Box: TBox3d;
    var IsAboveTheBox: boolean; var SqrHeightAboveTheBox: Single);
  begin
    { We use here the assumption that HomeCameraUp is (0, 0, 1). }

    IsAboveTheBox := (not IsEmptyBox3d(Box)) and
      (Box[0, 0] <= Point[0]) and (Point[0] <= Box[1, 0]) and
      (Box[0, 1] <= Point[1]) and (Point[1] <= Box[1, 1]) and
      (Point[2] >= Box[1, 2]);

    if IsAboveTheBox then
      SqrHeightAboveTheBox := Sqr(Point[2] - Box[1, 2]);
  end;

var
  I: Integer;
  IsAboveTheBox: boolean;
  SqrHeightAboveTheBox: Single;
begin
  for I := 0 to High do
    if (Items[I] <> IgnoreCreature) and
       (Items[I].CollisionsWithCreaturesAndPlayer) then
    begin
      GetPointHeightAboveBox3d(Position, Items[I].BoundingBox,
        IsAboveTheBox, SqrHeightAboveTheBox);

      if IsAboveTheBox then
      begin
        if not IsAboveTheGround then
        begin
          IsAboveTheGround := true;
          SqrHeightAboveTheGround := SqrHeightAboveTheBox;
        end else
        if SqrHeightAboveTheBox < SqrHeightAboveTheGround then
        begin
          SqrHeightAboveTheGround := SqrHeightAboveTheBox;
        end;
      end;
    end;
end;

function TCreaturesList.FindKind(Kind: TCreatureKind): Integer;
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
  const AnimationTime: Single);
begin
  inherited Create(AKind, ALegsPosition, ADirection, AMaxLife, AnimationTime);
  FState := wasStand;
  FStateChangeTime := AnimationTime;
  WaypointsSaved := TSceneWaypointsList.Create;
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
    var DirectionToTarget: TVector3Single;
    var AngleRadBetweenDirectionToTarget: Single);
  begin
    { calculate DirectionToTarget }
    DirectionToTarget := VectorSubtract(Target, MiddlePosition);
    if not Kind.Flying then
      MakeVectorsOrthoOnTheirPlane(DirectionToTarget, Level.HomeCameraUp);

    { calculate AngleRadBetweenDirectionToTarget }
    AngleRadBetweenDirectionToTarget :=
      AngleRadBetweenVectors(DirectionToTarget, Direction);
  end;

  { Call this only when HasLastSeenPlayer }
  procedure CalculateDirectionToPlayer(var DirectionToPlayer: TVector3Single;
    var AngleRadBetweenDirectionToPlayer: Single);
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
        IsAboveTheGround: boolean;
        SqrHeightAboveTheGround: Single;
      begin
        Result := false;
        if not Kind.Flying then
        begin
          GetCameraHeight(NewMiddlePosition, IsAboveTheGround,
            SqrHeightAboveTheGround);
          if (not IsAboveTheGround) or
            (SqrHeightAboveTheGround > Sqr(WAKind.MaxHeightAcceptableToFall +
              HeightBetweenLegsAndMiddle)) then
            Result := true;
        end;
      end;

    var
      OldMiddlePosition, NewMiddlePosition: TVector3Single;
    begin
      OldMiddlePosition := MiddlePosition;
      NewMiddlePosition := VectorAdd(OldMiddlePosition,
        VectorScale(Direction, WAKind.MoveSpeed * CompSpeed));

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
        MoveAllowedSimple(OldMiddlePosition, NewMiddlePosition, false);

      if Result then
        FLegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
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
          TSceneSectorsList.FindWay(WaypointsSaved_Begin, WaypointsSaved_End,
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

      ProposedNewMiddlePosition := VectorAdd(OldMiddlePosition,
        VectorScale(LastAttackDirection, CurrentKnockBackDistance));

      if MoveAllowed(OldMiddlePosition, ProposedNewMiddlePosition,
        NewMiddlePosition, false) then
        FLegsPosition := LegsPositionFromMiddle(NewMiddlePosition);
    end;
  end;

begin
  inherited;

  if Dead then
  begin
    SetState(wasDying);
    Exit;
  end;

  IdleSeesPlayer := Level.LineOfSight(MiddlePosition, Player.Navigator.CameraPos);
  if IdleSeesPlayer then
  begin
    HasLastSeenPlayer := true;
    LastSeenPlayer := Player.Navigator.CameraPos;
    LastSeenPlayerSector := Player.CameraPosSector;
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
    wasDying: ;
    wasHurt: DoHurt;
    wasSpecial1: { Should be handled in descendants. };
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
  if (not Dead) and
    (Life - Value > WAKind.MinLifeLossToHurt * MaxLife) and
    ( (WAKind.ChanceToHurt = 1.0) or
      (Random < WAKind.ChanceToHurt) ) then
    SetState(wasHurt);
  inherited;
end;

function TWalkAttackCreature.ShortRangeActualAttackHits: boolean;
var
  B, PB: TBox3d;
  DistanceLength, DistanceIncrease: Single;
begin
  B := BoundingBox;
  PB := Player.BoundingBox;

  { We would like to check collision between PB and our B translated
    by our Direction now, i.e.
      Boxes3dCollision(Box3dTranslate(B, VectorScale(Direction, ???)), PB)
    But how much should be scale Direction, i.e. what to put for "???" ?
    It must be large enough to compensate even large WAKind.MaxAttackDistance,
    it must be small enough so that player should not be able to avoid
    our attacks just by standing very close to the creature.

    So we have to check a couple of bounding boxes.
    If we move our boxes by Box3dMinSize(B), we're sure that
    each box will stick to the previous and next. But maybe
    there will be some areas around the sticking points ?
    So Box3dMinSize(B) / 2 seems safe. }
  DistanceIncrease := Box3dMinSize(B) / 2;

  DistanceLength := DistanceIncrease;
  while DistanceLength < WAKind.MaxAttackDistance do
  begin
    if Boxes3dCollision(Box3dTranslate(B,
       VectorScale(Direction, DistanceLength)), PB) then
      Exit(true);
    DistanceLength += DistanceIncrease;
  end;

  { Check one last time for WAKind.MaxAttackDistance }
  Result := Boxes3dCollision(Box3dTranslate(B,
    VectorScale(Direction, WAKind.MaxAttackDistance)), PB);
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
    MissilePosition := VLerpLegsMiddlePosition(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := BallMissile.CreateDefaultCreature(
      MissilePosition, MissileDirection,
      Level.AnimationTime);

    Level.Creatures.Add(Missile);

    Missile.Sound3d(stBallMissileFired, 0.0);
  end;
end;

{ TWerewolfCreature ---------------------------------------------------------- }

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

  if (FState = wasDying) and
     (Level is TCastleHallLevel) then
    TCastleHallLevel(Level).DestroyStairsBlocker;
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

  if not Dead then
    case State of
      wasStand, wasWalk: DoMaybeSwitchToThrowWebAttack;
      wasSpecial1: DoThrowWebAttack;
    end;
end;

function TSpiderQueenCreature.CurrentScene: TVRMLFlatSceneGL;
var
  StateTime: Single;
begin
  if State = wasSpecial1 then
  begin
    { Time from the change to this state. }
    StateTime := Level.AnimationTime - StateChangeTime;

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
    MissilePosition := VLerpLegsMiddlePosition(FiringMissileHeight);
    MissileDirection := VectorSubtract(LastSeenPlayer, MissilePosition);
    Missile := ThrownWeb.CreateDefaultCreature(
      MissilePosition, MissileDirection,
      Level.AnimationTime);

    Level.Creatures.Add(Missile);

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

function TGhostCreature.HeightBetweenLegsAndMiddle: Single;
begin
  Result := CurrentScene.BoundingBox[1, 2] / 2;
end;

{ TMissileCreature ----------------------------------------------------------- }

function TMissileCreature.MissileKind: TMissileCreatureKind;
begin
  Result := TMissileCreatureKind(Kind);
end;

procedure TMissileCreature.Idle(const CompSpeed: Single);
var
  NewLegsPosition: TVector3Single;
  AngleBetween, AngleChange: Single;
  NewDirection, TargetDirection: TVector3Single;
  Box: TBox3d;
  I: Integer;
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

  if MissileKind.HitsPlayer and LegsCollisionWithPlayer(LegsPosition) then
    ExplodeWithPlayer;

  if MissileKind.HitsCreatures then
  begin
    Box := BoundingBox;
    for I := 0 to Level.Creatures.Count - 1 do
      if (Level.Creatures[I] <> Self) and
         (Level.Creatures[I].CollisionsWithCreaturesAndPlayer) and
         (Boxes3dCollision(Box, Level.Creatures[I].BoundingBox)) then
      begin
        ExplodeWithCreature(Level.Creatures[I]);
        { TODO: projectiles shouldn't do here "break". }
        break;
      end;
  end;

  if MissileKind.CloseDirectionToPlayer and
     (MissileKind.CloseDirectionToTargetSpeed <> 0) then
  begin
    TargetDirection := VectorSubtract(Player.Navigator.CameraPos,
      LegsPosition);
    AngleBetween := AngleRadBetweenVectors(TargetDirection, Direction);
    AngleChange := MissileKind.CloseDirectionToTargetSpeed * CompSpeed;
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

  { This sound is done using CastleSound.Sound3d, not our Sound3d
    --- because when the creature will be destroyed (and missile will
    be destroyed in nearest RemoveFromLevel pass), we want this sound
    to go on. }
  CastleSound.Sound3d(MissileKind.SoundExplosion, LegsPosition);

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
  Creature.Life := Creature.Life -
    (Kind.ShortRangeAttackDamageConst +
      Random * Kind.ShortRangeAttackDamageRandom);
end;

function TMissileCreature.CollisionsWithCreaturesAndPlayer: boolean;
begin
  Result := false;
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

var
  AnimScenesPerTime: Cardinal;
begin
  AnimScenesPerTime := CreatureAnimationScenesPerTime;

  Glw.OnCloseList.AppendItem(@GLWindowClose);

  CreaturesKinds := TCreaturesKindsList.Create;

  Alien := TBallThrowerCreatureKind.Create(
    'Alien',
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
  Alien.SoundSuddenPain := stAlienSuddenPain;
  Alien.SoundDying := stAlienDying;

  Werewolf := TWerewolfKind.Create(
    'Werewolf',
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
      [ 0, 0.3 / 2, 0.6 / 2, 1.0 / 2 ],
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
  Werewolf.SoundSuddenPain := stWerewolfSuddenPain;
  Werewolf.SoundAttackStart := stWerewolfAttackStart;
  Werewolf.SoundDying := stWerewolfDying;

  BallMissile := TMissileCreatureKind.Create(
    'BallMissile',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ball_missile' + PathDelim + 'ball_missile_1_final.wrl'),
        CreatureFileName('ball_missile' + PathDelim + 'ball_missile_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, true, false)
    );
  BallMissile.SoundExplosion := stBallMissileExplode;
  BallMissile.SoundIdle := stBallMissileIdle;

  Ghost := TGhostKind.Create(
    'Ghost',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_stand_2_final.wrl') ],
      [ 0, 1 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_walk_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_walk_2_final.wrl') ],
      [ 0, 0.5 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_attack_2_final.wrl') ],
      [ 0, 0.2 ],
      AnimScenesPerTime, AnimOptimization, false, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_dying_2_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_dying_3_final.wrl') ],
      [ 0.0, 0.3, 1.0 ],
      AnimScenesPerTime, AnimOptimization, false, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('ghost' + PathDelim + 'ghost_stand_1_final.wrl'),
        CreatureFileName('ghost' + PathDelim + 'ghost_walk_2_final.wrl') ],
      [ 0.0, 0.3 ],
      AnimScenesPerTime, AnimOptimization, false, true)
    );
  Ghost.SoundSuddenPain := stGhostSuddenPain;
  Ghost.SoundAttackStart := stGhostAttackStart;
  Ghost.SoundDying := stGhostDying;

  Spider := TSpiderKind.Create(
    'Spider',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { WalkAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_walk_2.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_walk_3.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_walk_4.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_walk_5.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_stand.wrl') ],
      [   0 / 5,
        0.2 / 5,
        0.4 / 5,
        0.6 / 5,
        0.8 / 5,
        1 / 5 ],
      { This animation really needs more frames to look smoothly. }
      AnimScenesPerTime * 5, AnimOptimization, true, false),
    { AttackAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_attack_2.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_attack_3.wrl') ],
      [ 0, 0.125, 0.25 ],
      AnimScenesPerTime, AnimOptimization, false, true),
    { DyingAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_hurt_2.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_dying_3.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_dying_4.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_dying_5.wrl') ],
      [ 0, 0.3, 0.6, 1.0, 2.0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { HurtAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider' + PathDelim + 'spider_stand.wrl'),
        CreatureFileName('spider' + PathDelim + 'spider_hurt_2.wrl') ],
      [ 0, 0.3 ],
      AnimScenesPerTime, AnimOptimization, false, true)
    );
  Spider.SoundSuddenPain := stSpiderSuddenPain;
  Spider.SoundAttackStart := stSpiderAttackStart;
  Spider.SoundDying := stSpiderDying;

  SpiderQueen := TSpiderQueenKind.Create(
    'SpiderQueen',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, true, true),
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { WalkAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_walk_2.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_walk_3.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_walk_4.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_walk_5.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      [   0 / 5,
        0.2 / 5,
        0.4 / 5,
        0.6 / 5,
        0.8 / 5,
        1 / 5 ],
      { This animation really needs more frames to look smoothly. }
      AnimScenesPerTime * 5, AnimOptimization, true, false),
    { AttackAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_attack_2.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_attack_3.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      [ 0, 0.2, 0.4, 0.6 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { DyingAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_dying_2.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_dying_3.wrl') ],
      [ 0, 0.5, 1.5 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { HurtAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      { Non-zero anim to have a little knockback. }
      [ 0, 0.1 ],
      AnimScenesPerTime, AnimOptimization, false, false),
    { ThrowWebAttackAnimation }
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_attack_2.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_attack_alt.wrl'),
        CreatureFileName('spider_queen' + PathDelim + 'spider_queen_stand.wrl') ],
      [ 0, 0.3, 0.6, 1.0 ],
      AnimScenesPerTime, AnimOptimization, false, false)
    );
  SpiderQueen.SoundSuddenPain := stSpiderQueenSuddenPain;
  SpiderQueen.SoundAttackStart := stSpiderQueenAttackStart;
  SpiderQueen.SoundDying := stSpiderQueenDying;

  ThrownWeb := TMissileCreatureKind.Create(
    'ThrownWeb',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('web' + PathDelim + 'web.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false)
    );
  ThrownWeb.SoundExplosion := stThrownWebHit;
  ThrownWeb.SoundIdle := stThrownWebIdle;

  Arrow := TMissileCreatureKind.Create(
    'Arrow',
    TVRMLGLAnimationInfo.Create(
      [ CreatureFileName('arrow' + PathDelim + 'arrow.wrl') ],
      [ 0 ],
      AnimScenesPerTime, AnimOptimization, false, false)
    );
  Arrow.SoundExplosion := stArrowHit;

  CreaturesKinds.LoadFromFile;
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