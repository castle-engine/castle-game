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

{ TLevel class and some specialized descendants.

  About usage of T3D stuff in castle:

  @unorderedList(
    @item(
      Owner of T3D must be always set to the containing TLevel.
      ParentLevel simply returns Owner, typecasted to TLevel.
      This wasn't really necessary (I could introduce separate ParentLevel)
      but in practice it's the simplest and Ok for castle for now.)

  )
}

unit GameLevel;

interface

uses VectorMath, CastleSceneCore, CastleScene, Boxes3D,
  X3DNodes, X3DFields, GameItems, Cameras, Triangle,
  GameCreatures, SceneWaypoints, GameSound,
  CastleUtils, CastleClassUtils, GamePlayer, GameThunder,
  ProgressUnit, PrecalculatedAnimation, ALSoundAllocator,
  Background, DOM, XmlSoundEngine, Base3D, Shape,
  Classes, CastleTimeUtils, CastleSceneManager, GLRendererShader;

{$define read_interface}

const
  DefaultGlobalAmbientLight: TVector4Single = (0.2, 0.2, 0.2, 1.0);

type
  TLevel = class;

  TCastleSceneClass = class of TCastleScene;
  TCastlePrecalculatedAnimationClass = class of TCastlePrecalculatedAnimation;

  { 3D object moving and potentially pushing other 3D objects.
    Good for elevators, doors and such.

    Other 3D objects may be pushed, if @link(Pushes).
    There are two methods of pushing available, see @link(PushesEverythingInside).
    Only the 3D objects with @link(T3D.Pushable) are ever pushed by this object
    (the rest of 3D world is treated as static, does not interact with
    elevators / doors or such).

    You can also stop/reverse the move to prevent some collisions
    from occuring at all. This way you can e.g. prevent the door
    from automatically closing, if someone/something blocks the way.
    You do this by overriding BeforeTimeIncrease.
    See TDoomLevelDoor.BeforeTimeIncrease in "The Castle" for example how to
    do this. }
  T3DMoving = class(T3DCustomTransform)
  private
    FPushes: boolean;
    FPushesEverythingInside: boolean;
    FAnimationTime: TFloatTime;
  protected
    { Local object time, always increasing, used to track animations. }
    property AnimationTime: TFloatTime read FAnimationTime;

    { Implements T3D.GetTranslation by always calling
      GetTranslationFromTime(AnimationTime).
      Descendants should only override GetTranslationFromTime. }
    function GetTranslation: TVector3Single; override;
    function OnlyTranslation: boolean; override;

    function GetTranslationFromTime(const AnAnimationTime: TFloatTime):
      TVector3Single; virtual; abstract;

    { Do something right before animation progresses.
      Called at the beginning of our @link(Idle),
      @italic(right before) AnimationTime changes to NewAnimationTime.

      Useful for taking care of collision detection issues,
      as our assumption always is that "nothing collides". Which means
      that if you don't want your T3DMoving to collide
      with e.g. player or creatures or items, then you should
      prevent the collision @italic(before it happens).
      This is the place to do it. }
    procedure BeforeTimeIncrease(const NewAnimationTime: TFloatTime); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function ParentLevel: TLevel;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  published
    { Are other 3D objects pushed when this object moves.
      Only the 3D objects with @link(T3D.Pushable) are ever pushed by this object
      (the rest of 3D world is treated as static, does not interact with
      elevators / doors or such).

      Only relevant if GetCollides. Non-colliding objects never push others. }
    property Pushes: boolean read FPushes write FPushes default true;

    { If @link(Pushes) is @true, this determines how pushing actually works.
      There two methods:

      @orderedList(
        @item(PushesEverythingInside = @true: We move every
          pushable 3D object that is inside our bounding box.
          This is sensible if we can reasonably assume that things
          inside our box are standing. For example if this is
          a (vertical or horizontal) elevator, then creatures/items
          are usually standing/lying inside, and naturally move with
          the same speed (and direction) as the elevator.)

        @item(When PushesEverythingInside = @false: We check precise
          collision between pushable 3D objects and our triangle mesh.
          Actually, we use T3DList.BoxCollision / T3DList.SphereCollsion,
          that will use children's T3D.BoxCollision / T3D.SphereCollsion;
          they check collisions with triangle mesh in case of TCastleScene
          with Spatial containing e.g. ssDynamicCollisions.)
      )

      Neither method is really perfect.

      PushesEverythingInside = @false seems like a more precise check,
      as it actually compares the triangle mesh, taking into account
      the interior of (this) moving 3D object. PushesEverythingInside = @true
      just approximates the moving 3D object by it's bounding box.

      On the other hand, PushesEverythingInside = @true makes the elevator
      more "sticky". With PushesEverythingInside = @false,
      when player hits the floor, it takes them some time to raise up.
      This creates a "bouncing camera" effect when the elevator goes up
      quickly: player constantly falls to the ground, tries to get up,
      but elevator moves up and player falls to it's ground again.
      When the elevator goes down, the player/creature constantly falls
      down on it because of gravity, which again causes artifacts
      as gravity may work significantly slower/faster than elavator moving speed.
      When the elevator is a horizontal moving platform, it will "slip"
      from under the player/creature, leaving the poor fella suddenly hanging
      in the air, and falling down because of gravity in the next second.

      In practice: PushesEverythingInside should be @true for small
      containers, when you can reasonably assume that things (creatures,
      player, items) stand inside, and when you intend to use it for transport
      of 3D stuff. For very large moving stuff, that possibly
      interacts with flying players/creatures in some creative way,
      PushesEverythingInside may be @false. }
    property PushesEverythingInside: boolean
      read FPushesEverythingInside write FPushesEverythingInside default true;
  end;

  { This is a T3DMoving that moves with a constant speed
    from Translation (0, 0, 0) to Translation TranslationEnd.
    They are called @italic(begin position) and @italic(end position).

    In practice, this is less flexible than T3DMoving but often
    more comfortable: you get easy to use GoBeginPosition, GoEndPosition
    properties, you can easily set sounds by SoundGoBeginPosition and
    SoundGoEndPosition etc.
  }
  T3DLinearMoving = class(T3DMoving)
  private
    FEndPosition: boolean;
    FEndPositionStateChangeTime: Single;

    FSoundGoBeginPosition: TSoundType;
    FSoundGoEndPosition: TSoundType;
    FSoundGoBeginPositionLooping: boolean;
    FSoundGoEndPositionLooping: boolean;
    FSoundTracksCurrentPosition: boolean;

    UsedSound: TALSound;
    procedure SoundSourceUsingEnd(Sender: TALSound);
    function SoundPosition: TVector3Single;
    procedure PlaySound(SoundType: TSoundType; Looping: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Is this object in @italic(end position), or going to it ?
      If @false, then this object is in @italic(begin position)
      or going to it. See also CompletelyEndPosion and CompletelyBeginPosition.

      Initially this is @false, and EndPositionStateChangeTime is set such that
      we're sure that we're in CompletelyBeginPosion, }
    property EndPosition: boolean read FEndPosition;

    { Last time EndPosition changed. }
    property EndPositionStateChangeTime: Single read FEndPositionStateChangeTime;

    function CompletelyEndPosition: boolean;
    function CompletelyBeginPosition: boolean;

    { This starts going to @italic(begin position), assuming that
      currently we're in @italic(end position) (i.e. CompletelyEndPosion). }
    procedure GoBeginPosition;

    { This starts going to @italic(end position), assuming that
      currently we're in @italic(begin position) (i.e. CompletelyBeginPosion). }
    procedure GoEndPosition;

    { Call this to stop going from @italic(end position) to @italic(begin position)
      and go back to @italic(end position). Call this only when currently
      EndPosition is @false and we were in the middle of going to
      @italic(begin position).

      If you don't understand this description, here's an example:
      this is what happens when door on DOOM level gets blocked.
      In the middle of closing (which ig going to @italic(begin position))
      it will realize that something blocks it, and open back
      (go back to @italic(end position)).  }
    procedure RevertGoEndPosition;

    { Just like RevertGoEndPosition, but this should be used in the middle
      of the move from @italic(begin position) to @italic(end position),
      to go back to @italic(begin position). }
    procedure RevertGoBeginPosition;

    { This goes to the @italic(other) position.
      Which means that if we're completely in @italic(end position)
      or in the middle of move to @italic(end position), this goes
      back to @italic(begin position). And if we're in @italic(begin position),
      this goes back to @italic(end position). }
    procedure GoOtherPosition;

    property SoundGoBeginPosition: TSoundType
      read FSoundGoBeginPosition write FSoundGoBeginPosition default stNone;
    property SoundGoEndPosition: TSoundType
      read FSoundGoEndPosition write FSoundGoEndPosition default stNone;

    property SoundGoBeginPositionLooping: boolean
      read FSoundGoBeginPositionLooping write FSoundGoBeginPositionLooping
      default false;
    property SoundGoEndPositionLooping: boolean
      read FSoundGoEndPositionLooping write FSoundGoEndPositionLooping
      default false;

    { If @true then the sound (set by SoundGoBeginPosition or
      SoundGoEndPosition) 3D position changes as the 3D position of the object
      changes.

      Otherwise (default) sound is initially made at initial
      3D position of this object, and then the sound position doesn't change
      (even if the position of the object changes). }
    property SoundTracksCurrentPosition: boolean
      read FSoundTracksCurrentPosition write FSoundTracksCurrentPosition
      default false;
  public
    MoveTime: Single;
    TranslationEnd: TVector3Single;

    function GetTranslationFromTime(const AnAnimationTime: TFloatTime):
      TVector3Single; override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  { This is an abstract class for a special group of objects:
    they define an invisible and non-colliding areas on the level
    that...well, have @italic(some purpose). What exactly this
    "purpose" is, is defined in each TLevelArea descendant.

    This class defines only a properties to define the area.
    For now, each area is just one TBox3D. }
  TLevelArea = class(T3D)
  private
    FVRMLName: string;
    FBox: TBox3D;

    { Area. Default value is EmptyBox3D. }
    property Box: TBox3D read FBox write FBox;
  public
    constructor Create(AOwner: TComponent); override;

    function ParentLevel: TLevel;

    { Name used to recognize this object's area in level VRML file.

      If this object is present during ChangeLevelScene call from
      TLevel constructor then the shape with a parent named like VRMLName
      will be removed from VRML file, and it's BoundingBox will be used
      as Box3D of this object.

      This way you can easily configure area of this object in Blender:
      just add a cube, set it's mesh name to match with this VRMLName,
      and then this cube defines Box3D of this object. }
    property VRMLName: string read FVRMLName write FVRMLName;

    function PointInside(const Point: TVector3Single): boolean;

    function BoundingBox: TBox3D; override;

    { Called from TLevel constructor. This is the place when you
      can modify ParentLevel.MainScene.RootNode, e.g. by calling
      RemoveBoxNode. }
    procedure ChangeLevelScene(AParentLevel: TLevel);
  end;

  { This defines area on the level that causes
    a Notification to be displayed when player enters inside.
    The natural use for it is to display various hint messages when player
    is close to something. }
  TLevelHintArea = class(TLevelArea)
  private
    FMessage: string;
    FMessageDone: boolean;
  public
    { Message to this display when player enters Box3D.
      Some formatting strings are allowed inside:
      @unorderedList(
        @item(%% produces InteractInputDescription in the message.)
        @item(%% produces one % in the message.)
      )
      @noAutoLinkHere }
    property Message: string read FMessage write FMessage;

    { Was the @link(Message) already displayed ? If @true,
      then it will not be displayed again (unless you will
      reset MessageDone to @false from your TLevel descendant code). }
    property MessageDone: boolean read FMessageDone write FMessageDone
      default false;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  TLevel = class(TCastleSceneManager)
  private
    FCameraRadius: Single;
    FCameraPreferredHeight: Single;
    FLevelProjectionNear: Single;
    FLevelProjectionFar: Single;
    FMoveHorizontalSpeed: Single;
    FMoveVerticalSpeed: Single;
    FSickProjection: boolean;
    FSickProjectionSpeed: TFloatTime;

    { Used only within constructor.
      We will process the scene graph, and sometimes it's not comfortable
      to remove the items while traversing --- so we will instead
      put them on this list.

      Be careful: never add here two nodes such that one may be parent
      of another, otherwise freeing one could free the other one too
      early. }
    ItemsToRemove: TX3DNodeList;

    procedure TraverseForItems(Shape: TShape);
    procedure SetSickProjection(const Value: boolean);
    procedure SetSickProjectionSpeed(const Value: TFloatTime);
  private
    FCreatures: TCreatureList;
    procedure TraverseForCreatures(Shape: TShape);
  private
    FInitialPosition: TVector3Single;
    FInitialDirection: TVector3Single;
    FInitialUp: TVector3Single;
    FGravityUp: TVector3Single;
    FMoveSpeed: Single;

    FAnimationTime: TFloatTime;

    FSectors: TSceneSectorList;
    FWaypoints: TSceneWaypointList;

    FWaterBox: TBox3D;
    FAboveWaterBox: TBox3D;

    FPlayedMusicSound: TSoundType;
    FGlobalAmbientLight: TVector4Single;
    FThunderEffect: TThunderEffect;
  private
    FName: string;
    FSceneFileName: string;
    FTitle: string;
    FTitleHint: string;
    FNumber: Integer;

    procedure LoadFromDOMElement(Element: TDOMElement);
  private
    FMenuBackground: boolean;
    FSceneDynamicShadows: boolean;

    FRequiredCreatures: TStringList;
  protected
    FBossCreature: TCreature;
    FFootstepsSound: TSoundType;

    { See [http://castle-engine.sourceforge.net/castle-development.php]
      for description of CameraBox and WaterBox trick.
      Remember that this may change MainScene.BoundingBox (in case we will
      find and remove the node from Scene). }
    function RemoveBoxNode(out Box: TBox3D; const NodeName: string): boolean;

    { Like RemoveBoxNode, but raise EInternalError if not found. }
    procedure RemoveBoxNodeCheck(out Box: TBox3D; const NodeName: string);

    { This will be called from our constructor before initializing
      our octrees. Even before initializing creatures and items.

      You can override this to do here some operations
      that change the MainScene.RootNode (e.g. you can do here tricks like
      extracting some specific objects using RemoveBoxNode).
      Be very cautious what you do here --- remember that this is called
      while TLevel.Create constructor did not finish it's work yet !

      This is your only chance to insert into @link(Items) list some
      object that has meaningfull ChangeLevelScene method. }
    procedure ChangeLevelScene; virtual;

    { Load TCastlePrecalculatedAnimation from *.kanim file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes enabled by RenderShadowsPossible))
        @item FreeExternalResources, since they will not be needed anymore
        @item TimePlaying is by default @false, so the animation is not playing.
      ) }
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean;
      const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;

    procedure RenderFromViewEverything; override;
    procedure InitializeLights(const Lights: TLightInstancesList); override;
    procedure ApplyProjection; override;
    procedure PointingDeviceActivateFailed(const Active: boolean); override;
  public

    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this. }
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); reintroduce; virtual;

    destructor Destroy; override;

    { This is an internal name for this level.
      Should follow the same
      rules as Pascal identifier: 1st character (_ or letter),
      then any number of letters or _ or digits.

      All available levels must have different Name.

      @noAutoLinkHere }
    property Name: string read FName;

    { These will be used in constructor to load level.
      @groupBegin }
    property SceneFileName: string read FSceneFileName;
    { @groupEnd }

    { }
    property CameraRadius: Single read FCameraRadius;
    property CameraPreferredHeight: Single read FCameraPreferredHeight;
    property LevelProjectionNear: Single read FLevelProjectionNear;
    property LevelProjectionFar: Single read FLevelProjectionFar;
    { LevelProjectionFar or infinity (if shadow volumes are used). }
    function LevelProjectionFarFinal: Single;
    property MoveHorizontalSpeed: Single read FMoveHorizontalSpeed;
    property MoveVerticalSpeed: Single read FMoveVerticalSpeed;

    property WaterBox: TBox3D read FWaterBox;

    { This is always calculated as a box with X, Y borders just like
      the WaterBox and the Z borders placed such that AboveWaterBox
      is exactly above WaterBox (top of WaterBox is equal to bottom of
      AboveWaterBox) and the height of AboveWaterBox is "rather small".

      The intention of "rather small" is such that when player Position
      is within AboveWaterBox then the player is floating slightly
      above the water --- not immediately falling down, but also
      not drowning.

      In other words, this means that player head is above the water surface
      but his feet are in the water. In some sense he/she is swimming,
      in some not. }
    property AboveWaterBox: TBox3D read FAboveWaterBox;

    { LineOfSight, MoveAllowed and GetHeightAbove perform
      collision detection with the level @link(Items).

      Note that MoveAllowed and GetHeightAbove treat transparent
      objects as others --- i.e., they collide. You have to override
      CollisionIgnoreItem to eventually change this for some items
      (transparent or opaque) to make them not colliding.

      But LineOfSight checks just collision between segment (Pos1, Pos2)
      and it *does ignore transparent materials*. This means that
      e.g. creatures can see through glass --- even though they
      can't walk through it. CollisionIgnoreItem doesn't matter
      for LineOfSight.

      Note about AboveGround: it is set to @nil if the ground item
      can't be represented as any octree item. Right now, this means that
      something stands on another creature/player/item.

      @groupBegin }
    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
      virtual;

    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D): boolean;

    function MoveAllowed(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D): boolean;

    procedure GetHeightAbove(const Position: TVector3Single;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: PTriangle);
    { @groupEnd }

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { This is the time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: TFloatTime read FAnimationTime;

    property InitialPosition : TVector3Single read FInitialPosition;
    property InitialDirection: TVector3Single read FInitialDirection;
    property InitialUp       : TVector3Single read FInitialUp;
    property MoveSpeed: Single read FMoveSpeed;

    { Actually, this must be (0, 0, 1) for this game.
      Some things in this game are prepared to handle any
      GravityUp value --- but some not (for simplicity, and sometimes
      code efficiency). }
    property GravityUp: TVector3Single read FGravityUp;

    property Sectors: TSceneSectorList read FSectors;
    property Waypoints: TSceneWaypointList read FWaypoints;

    property PlayedMusicSound: TSoundType
      read FPlayedMusicSound write FPlayedMusicSound default stNone;

    { This is read from level XML file, stPlayerFootstepsConcrete by default. }
    property FootstepsSound: TSoundType
      read FFootstepsSound write FFootstepsSound;

    { This is the nice name of the level. }
    property Title: string read FTitle;

    property TitleHint: string read FTitleHint;

    { This is level number, shown for the player in the menu.
      This *does not* determine the order in which levels are played,
      as levels do not have to be played in linear order.
      However, they are displayed in menu in linear order, and that's
      why this is needed. }
    property Number: Integer read FNumber;

    { This will be called when new player starts game on this level.
      This is supposed to equip the player with some basic weapon/items.

      Usually level design assumes that player came to level from some
      other level in the game, so he already owns some weapon / items etc.
      But when player uses "New Game" command to get to some already
      AvailableForNewGame non-first level, this method will be called and it should
      give player some basic weapon / items suitable for starting this level.

      In TLevel class implementation of this does nothing.  }
    procedure PrepareNewPlayer(NewPlayer: TPlayer); virtual;

    { This controls OpenGL GL_LIGHT_MODEL_AMBIENT setting.
      You can change it while the level is loaded (just remember to refresh
      OpenGL setting if needed).

      By default it's DefaultGlobalAmbientLight, which is equal to OpenGL's
      default GL_LIGHT_MODEL_AMBIENT setting. }
    property GlobalAmbientLight: TVector4Single
      read FGlobalAmbientLight write FGlobalAmbientLight;

    { For thunder effect. nil if no thunder effect should be done for this level.

      Descendants can set this in their constructor.
      We will call it's Idle, GamePlay will call it's InitGLLight and Render,
      our destructor will free it. }
    property ThunderEffect: TThunderEffect
      read FThunderEffect write FThunderEffect;

    { This returns whether and what to show on boss creature indicator.
      Default implementation in this class uses BossCreature property:
      if it's non-nil and BossCreature is alive, then indicator shows
      BossCreature life. }
    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; virtual;

    property MenuBackground: boolean read FMenuBackground write FMenuBackground;

    { If @true, we will render dynamic shadows (shadow volumes) for
      all scene geometry. This allows the whole level to use dynamic
      shadows. It's normally read from data/levels/index.xml,
      attribute scene_dynamic_shadows. }
    property SceneDynamicShadows: boolean
      read FSceneDynamicShadows write FSceneDynamicShadows default false;

    { Just load TCastleScene from file, doing some common tasks:
      @unorderedList(
        @item sets Attributes according to AttributesSet
        @item optionally create triangle octree
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes enabled by RenderShadowsPossible), optionally
          with prBackground)
        @item FreeExternalResources, since they will not be needed anymore
      ) }
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean;
      const SceneClass: TCastleSceneClass): TCastleScene;
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean): TCastleScene;

    procedure BeforeDraw; override;
    function CreateDefaultCamera(AOwner: TComponent): TCamera; override;

    { Check Player.Camera collisions with world.
      @groupBegin }
    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; override;
    procedure CameraGetHeight(ACamera: TWalkCamera;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: P3DTriangle); override;
    function CameraRay(const RayOrigin, RayDirection: TVector3Single): TRayCollision; override;
    { @groupEnd }

    property SickProjection: boolean
      read FSickProjection write SetSickProjection;
    property SickProjectionSpeed: TFloatTime
      read FSickProjectionSpeed write SetSickProjectionSpeed;

    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;

    { Comfortably create and add creature to level.
      @groupBegin }
    function CreateCreature(const Kind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single; const MaxLife: Single): TCreature;
    function CreateCreature(const Kind: TCreatureKind;
      const ALegsPosition: TVector3Single;
      const ADirection: TVector3Single): TCreature;
    { @groupEnd }
  end;

  TLevelClass = class of TLevel;

{$undef read_interface}

implementation

uses SysUtils, GL,
  GamePlay, CastleGLUtils, CastleFilesUtils, CastleStringUtils,
  GameVideoOptions, GameConfig, GameNotifications,
  GameInputs, GameWindow, CastleXMLUtils,
  GameRequiredResources, GLRenderer, RenderingCameraUnit, Math;

{$define read_implementation}

{ T3DMoving --------------------------------------------------------- }

constructor T3DMoving.Create(AOwner: TComponent);
begin
  inherited;
  FPushes := true;
  FPushesEverythingInside := true;
  FAnimationTime := 0;
end;

function T3DMoving.ParentLevel: TLevel;
begin
  Result := Owner as TLevel;
end;

function T3DMoving.GetTranslation: TVector3Single;
begin
  Result := GetTranslationFromTime(AnimationTime);
end;

function T3DMoving.OnlyTranslation: boolean;
begin
  Result := true; { T3DMoving always uses only translation }
end;

{ Note: When pushing the creature/player/item, right now
  we don't check whether the creature/player/item will not be
  pushed into collision with something else.

  For now, design your level to minimize the chance that it will ever happen.
  Although in theory you cannot design your level to guarantee
  that it will never happen (because e.g. a creature may be pushed
  into collision with other creature, and since creatures move
  on their own they can arrange themselves (in theory) in all manners of
  funny configurations...). But in practice it's not so difficult,
  just make sure that there is enough space on the way of move.
}

procedure T3DMoving.BeforeTimeIncrease(
  const NewAnimationTime: TFloatTime);

  function BoundingBoxAssumeTranslation(
    const AssumeTranslation: TVector3Single): TBox3D;
  begin
    if GetCollides then
      Result := (inherited BoundingBox).Translate(AssumeTranslation) else
      Result := EmptyBox3D;
  end;

  function SphereCollisionAssumeTranslation(
    const AssumeTranslation: TVector3Single;
    const Pos: TVector3Single; const Radius: Single;
    const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
  begin
    Result := GetCollides;
    if Result then
    begin
      { We use the same trick as in T3DCustomTransform.MoveAllowed to
        use "inherited SphereCollsion" with Translation. }

      Result := inherited SphereCollision(
        Pos - AssumeTranslation, Radius, TrianglesToIgnoreFunc);
    end;
  end;

  function BoxCollisionAssumeTranslation(
    const AssumeTranslation: TVector3Single;
    const Box: TBox3D;
    const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
  begin
    Result := GetCollides;
    if Result then
    begin
      { We use the same trick as in T3DCustomTransform.MoveAllowed to
        use "inherited BoxCollision" with Translation. }

      Result := inherited BoxCollision(
        Box.AntiTranslate(AssumeTranslation), TrianglesToIgnoreFunc);
    end;
  end;

var
  CurrentBox, NewBox, Box: TBox3D;
  I: Integer;
  Move: TVector3Single;
  CurrentTranslation, NewTranslation: TVector3Single;
  SphereC: TVector3Single;
  SphereR: Single;
  Item: T3D;
begin
  if GetCollides and Pushes then
  begin
    CurrentTranslation := GetTranslationFromTime(AnimationTime);
    NewTranslation := GetTranslationFromTime(NewAnimationTime);

    { It often happens that T3DMoving doesn't move at all,
      and then Translation doesn't change at all
      (even when compared precisely, without usual epsilon used to compare
      floats). So the check below may be worth the time, we expect
      it will avoid doing actual work. }

    if not VectorsPerfectlyEqual(CurrentTranslation, NewTranslation) then
    begin
      Move := NewTranslation - CurrentTranslation;

      { TODO: it may be sensible to add a pushing method when we compare
        other object's bounding box (never a sphere, and be sure to use
        the "tall" box for player, including it's legs) with octree
        (that is, using inherited BoxCollision).
        This can have the advantages of both PushesEverythingInside=true
        (reacts more sticky, more eager to move colliding stuff with
        the same speed as elevator)
        and PushesEverythingInside=false (takes into account triangle mesh,
        not just our bounding volume). }

      if PushesEverythingInside then
      begin
        CurrentBox := BoundingBox;
        NewBox := BoundingBoxAssumeTranslation(NewTranslation);
        for I := 0 to ParentLevel.Items.Count - 1 do
        begin
          Item := ParentLevel.Items[I];
          if Item.Pushable then
          begin
            { This case doesn't really use Item.UseSphere. But it's not really
              terribly important design decision, we may use Item.UseSphere
              one day here. It's most comfortable to just use
              here Item.BoundingBox, as we perform collisions with our box. }
            Box := Item.BoundingBox;
            if Box.Collision(NewBox) or
               Box.Collision(CurrentBox) then
              Item.Translate(Move);
          end;
        end;
      end else
      begin
        for I := 0 to ParentLevel.Items.Count - 1 do
        begin
          Item := ParentLevel.Items[I];
          if Item.Pushable then
            if Item.UseSphere then
            begin
              Item.Sphere(SphereC, SphereR);
              if SphereCollisionAssumeTranslation(NewTranslation, SphereC, SphereR,
                @ParentLevel.CollisionIgnoreItem) then
                Item.Translate(Move);
            end else
            begin
              if BoxCollisionAssumeTranslation(NewTranslation,
                Item.BoundingBox,
                @ParentLevel.CollisionIgnoreItem) then
                Item.Translate(Move);
            end;
        end;
      end;
    end;
  end;
end;

procedure T3DMoving.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  NewAnimationTime: TFloatTime;
begin
  inherited;

  NewAnimationTime := AnimationTime + CompSpeed;
  BeforeTimeIncrease(NewAnimationTime);
  FAnimationTime := NewAnimationTime;
end;

{ T3DLinearMoving --------------------------------------------------- }

constructor T3DLinearMoving.Create(AOwner: TComponent);
begin
  inherited;

  FSoundGoEndPosition := stNone;
  FSoundGoBeginPosition := stNone;

  FEndPosition := false;

  { We set FEndPositionStateChangeTime to a past time, to be sure
    that we don't treat the door as "closing right now". }
  FEndPositionStateChangeTime := -1000.0; { TODO: should be implemented better... }

  UsedSound := nil;
end;

destructor T3DLinearMoving.Destroy;
begin
  { Otherwise, if you exit from the game while some sound was played,
    and the sound was e.g. looping (like the elevator on "Tower" level),
    the sound will never get stopped. }
  if UsedSound <> nil then
    UsedSound.DoUsingEnd;

  inherited;
end;

procedure T3DLinearMoving.SoundSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = UsedSound);
  UsedSound.OnUsingEnd := nil;
  UsedSound := nil;
end;

function T3DLinearMoving.SoundPosition: TVector3Single;
begin
  Result := BoundingBox.Middle;
end;

procedure T3DLinearMoving.PlaySound(SoundType: TSoundType;
  Looping: boolean);
begin
  { The object can play only one sound (going to begin or end position)
    at a time. }
  if UsedSound <> nil then
    UsedSound.DoUsingEnd;
  UsedSound := SoundEngine.Sound3d(SoundType, SoundPosition, Looping);

  if UsedSound <> nil then
    UsedSound.OnUsingEnd := @SoundSourceUsingEnd;
end;

procedure T3DLinearMoving.GoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure T3DLinearMoving.GoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoBeginPosition, SoundGoBeginPositionLooping);
end;

procedure T3DLinearMoving.RevertGoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure T3DLinearMoving.RevertGoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoBeginPositionLooping);
end;

procedure T3DLinearMoving.GoOtherPosition;
begin
  if CompletelyEndPosition then
    GoBeginPosition else
  if CompletelyBeginPosition then
    GoEndPosition else
  begin
    if EndPosition then
      RevertGoBeginPosition else
      RevertGoEndPosition;
  end;
end;

function T3DLinearMoving.GetTranslationFromTime(
  const AnAnimationTime: TFloatTime): TVector3Single;
begin
  if not EndPosition then
  begin
    if AnAnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely closed. }
      Result := ZeroVector3Single else
      { During closing. }
      Result := TranslationEnd *
        (1 - (AnAnimationTime - EndPositionStateChangeTime) / MoveTime);
  end else
  begin
    if AnAnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely open. }
      Result := TranslationEnd else
      { During opening. }
      Result := TranslationEnd *
        ((AnAnimationTime - EndPositionStateChangeTime) / MoveTime);
  end;
end;

function T3DLinearMoving.CompletelyEndPosition: boolean;
begin
  Result := EndPosition and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

function T3DLinearMoving.CompletelyBeginPosition: boolean;
begin
  Result := (not EndPosition) and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

procedure T3DLinearMoving.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  { Update sound position when object is moving }
  if (UsedSound <> nil) and SoundTracksCurrentPosition then
    UsedSound.Position := SoundPosition;

  { If the SoundGoBegin/EndPosition is longer than the MoveTime
    (or it's looping),
    stop this sound once we're completely in Begin/EndPosition. }
  if (AnimationTime - EndPositionStateChangeTime > MoveTime) and
    (UsedSound <> nil) then
    UsedSound.DoUsingEnd;
end;

{ TLevelArea ----------------------------------------------------------------- }

constructor TLevelArea.Create(AOwner: TComponent);
begin
  inherited;
  FBox := EmptyBox3D;
  { Actually, the fact that our BoundingBox is empty also prevents collisions.
    For for some methods, knowing that Collides = false allows them to exit
    faster. }
  Collides := false;
end;

function TLevelArea.BoundingBox: TBox3D;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3D;
end;

procedure TLevelArea.ChangeLevelScene(AParentLevel: TLevel);
begin
  inherited;
  AParentLevel.RemoveBoxNodeCheck(FBox, VRMLName);
end;

function TLevelArea.PointInside(const Point: TVector3Single): boolean;
begin
  Result := Box.PointInside(Point);
end;

function TLevelArea.ParentLevel: TLevel;
begin
  Result := Owner as TLevel;
end;

{ TLevelHintArea ----------------------------------------------------------- }

procedure TLevelHintArea.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  ReplaceInteractInput: TPercentReplace;
begin
  inherited;
  if (not MessageDone) and
     (Player <> nil) and
     PointInside(Player.Camera.Position) then
  begin
    ReplaceInteractInput.C := 'i';
    ReplaceInteractInput.S := InteractInputDescription;
    Notifications.Show(SPercentReplace(Message, [ReplaceInteractInput], true));
    MessageDone := true;
  end;
end;

{ TLevel --------------------------------------------------------------------- }

const
  ssCollisionOctree =
    {TODO:ssDynamicCollisions.
       Works fine with ssDynamicCollisions too, and fast.
       But just needs more tests with ssDynamicCollisions.
       And spiders in lair (with red key) on "cages" level are under
       floor level - fixit.
       For now, it can use ssCollidableTriangles. }
    ssCollidableTriangles;

constructor TLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    MainScene.BeforeNodesFree;
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
    MainScene.ChangedAll;
  end;

const
  SectorsMargin = 0.5;
var
  NavigationNode: TNavigationInfoNode;
  NavigationSpeed: Single;
  Options: TPrepareResourcesOptions;
  NewCameraBox: TBox3D;
  SI: TShapeTreeIterator;
begin
  inherited Create(nil);

  UseGlobalLights := true;
  ApproximateActivation := true;
  Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);

  FName := AName;
  FSceneFileName := ASceneFileName;
  FTitle := ATitle;
  FTitleHint := ATitleHint;
  FNumber := ANumber;
  FMenuBackground := AMenuBackground;
  FRequiredCreatures := TStringList.Create;
  FRequiredCreatures.Assign(ARequiredCreatures);

  if not DebugNoCreatures then
    RequireCreatures(BaseLights, FRequiredCreatures);

  Progress.Init(1, 'Loading level "' + Title + '"');
  try
    MainScene := TCastleScene.CreateCustomCache(Self, GLContextCache);
    MainScene.Load(SceneFileName);

    { initialize FAnimationTime. Must be initialized before creating creatures. }
    FAnimationTime := 0.0;

    AttributesSet(MainScene.Attributes, btIncrease);
    MainScene.Attributes.UseSceneLights := true;
    if BumpMapping then
      MainScene.Attributes.BumpMapping := bmBasic else
      MainScene.Attributes.BumpMapping := bmNone;
    MainScene.Attributes.UseOcclusionQuery := UseOcclusionQuery;

    { Calculate InitialPosition, InitialDirection, InitialUp.
      Must be done before initializing creatures, as they right now
      use InitialPosition. FInitialDirection, FInitialUp will be
      actually changed later in this procedure. }
    MainScene.GetPerspectiveViewpoint(FInitialPosition,
      FInitialDirection, FInitialUp, FGravityUp);

    { Scene must be the first one on Items, this way MoveAllowed will
      use Scene for wall-sliding (see T3DList.MoveAllowed implementation). }
    Items.Add(MainScene);

    LoadFromDOMElement(DOMElement);

    ChangeLevelScene;

    ItemsToRemove := TX3DNodeList.Create(false);
    try
      { Initialize Items }
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForItems(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;

      { Initialize Creatures }
      FCreatures := TCreatureList.Create(true);
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForCreatures(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;

      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    { Calculate CameraBox. }
    if not RemoveBoxNode(NewCameraBox, 'LevelBox') then
    begin
      { Set CameraBox to MainScene.BoundingBox, and make maximum Z larger. }
      NewCameraBox := MainScene.BoundingBox;
      NewCameraBox.Data[1, 2] += 4 * (NewCameraBox.Data[1, 2] - NewCameraBox.Data[0, 2]);
    end;
    CameraBox := NewCameraBox;

    if RemoveBoxNode(FWaterBox, 'WaterBox') then
    begin
      FAboveWaterBox := FWaterBox;
      FAboveWaterBox.Data[0, 2] := FWaterBox.Data[1, 2];
      FAboveWaterBox.Data[1, 2] := FAboveWaterBox.Data[0, 2] + 0.4;
    end else
    begin
      FWaterBox := EmptyBox3D;
      FAboveWaterBox := EmptyBox3D;
    end;

    { calculate Sectors and Waypoints }
    FSectors := TSceneSectorList.Create(true);
    FWaypoints := TSceneWaypointList.Create(true);
    Waypoints.ExtractPositions(MainScene);
    Sectors.ExtractBoundingBoxes(MainScene);
    Sectors.LinkToWaypoints(Waypoints, SectorsMargin);

    NavigationNode := MainScene.NavigationInfoStack.Top as TNavigationInfoNode;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      FCameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      FCameraRadius := MainScene.BoundingBox.AverageSize(false, 1) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      FCameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      FCameraPreferredHeight := FCameraRadius * 5;
    CorrectCameraPreferredHeight(FCameraPreferredHeight, CameraRadius,
      DefaultCrouchHeight, DefaultHeadBobbing);

    if NavigationNode <> nil then
      NavigationSpeed := NavigationNode.FdSpeed.Value else
      NavigationSpeed := 1.0;

    FLevelProjectionNear := CameraRadius * 0.75;
    FLevelProjectionFar := MainScene.BoundingBox.MaxSize(false, 1.0) * 5;

    { Fix InitialDirection length, and set MoveXxxSpeed.

      We want to have horizontal and vertical speeds controlled independently,
      so we just normalize InitialDirection and set speeds in appropriate
      MoveXxxSpeed. }
    NormalizeTo1st(FInitialDirection);
    FMoveSpeed := 1;
    FMoveHorizontalSpeed := NavigationSpeed;
    FMoveVerticalSpeed := 20;

    { Check and fix GravityUp. }
    if not VectorsEqual(Normalized(GravityUp),
             Vector3Single(0, 0, 1), 0.001) then
      raise EInternalError.CreateFmt(
        'Gravity up vector must be +Z, but is %s',
        [ VectorToRawStr(Normalized(GravityUp)) ]) else
      { Make GravityUp = (0, 0, 1) more "precisely" }
      FGravityUp := Vector3Single(0, 0, 1);

    MainScene.BackgroundSkySphereRadius := TBackground.NearFarToSkySphereRadius
      (LevelProjectionNear, LevelProjectionFar);

    MainScene.CastShadowVolumes := SceneDynamicShadows;

    { calculate Options for PrepareResources }
    Options := [prRender, prBackground, prBoundingBox];
    if RenderShadowsPossible and SceneDynamicShadows then
      Options := Options + prShadowVolume;

    MainScene.PrepareResources(Options, false, BaseLights);

    MainScene.FreeResources([frTextureDataInNodes]);

    FGlobalAmbientLight := DefaultGlobalAmbientLight;

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { Loading octree have their own Progress, so we load them outside our
    progress. }

  if not MenuBackground then
  begin
    MainScene.TriangleOctreeProgressTitle := 'Loading level (triangle octree)';
    MainScene.ShapeOctreeProgressTitle := 'Loading level (Shape octree)';
    MainScene.Spatial := [ssRendering, ssCollisionOctree];
    MainScene.PrepareResources([prSpatial], false, BaseLights);

    { TrianglesList was created for triangle octree. We don't need it anymore.

      Hm, for now we actually don't create ssCollidableTriangles,
      so TrianglesList is not actually used.
    Scene.FreeResources([frTrianglesListNotOverTriangulate]);
    }
  end;

  MainScene.ProcessEvents := true;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunderEffect);
  FreeAndNil(FSectors);
  FreeAndNil(FWaypoints);
  FreeAndNil(FCreatures);
  if (FRequiredCreatures <> nil) and not DebugNoCreatures then
    UnRequireCreatures(FRequiredCreatures);
  FreeAndNil(FRequiredCreatures);
  inherited;
end;

procedure TLevel.LoadFromDOMElement(Element: TDOMElement);

  procedure MissingRequiredAttribute(const AttrName, ElementName: string);
  begin
    raise Exception.CreateFmt(
      'Missing required attribute "%s" of <%s> element', [AttrName, ElementName]);
  end;

  function LevelHintAreaFromDOMElement(Element: TDOMElement): TLevelHintArea;
  begin
    Result := TLevelHintArea.Create(Self);
    Result.Message := DOMGetTextData(Element);
  end;

  function LevelAreaFromDOMElement(Element: TDOMElement): TLevelArea;
  var
    Child: TDOMElement;
  begin
    Child := DOMGetOneChildElement(Element);
    if Child.TagName = 'hint' then
      Result := LevelHintAreaFromDOMElement(Child) else
      raise Exception.CreateFmt('Not allowed children element of <area>: "%s"',
        [Child.TagName]);

    if not DOMGetAttribute(Element, 'vrml_name', Result.FVRMLName) then
      MissingRequiredAttribute('vrml_name', 'area');
  end;

  function LevelObjectFromDOMElement(Element: TDOMElement): T3D;
  begin
    if Element.TagName = 'area' then
      Result := LevelAreaFromDOMElement(Element) else
    if (Element.TagName = 'required_resources') or
       (Element.TagName = 'bump_mapping_light') then
    begin
      { These are handled elsewhere, and don't produce any T3D. }
      Result := nil;
    end else
      raise Exception.CreateFmt('Not allowed children element of <level>: "%s"',
        [Element.TagName]);
  end;

var
  SoundName: string;
  I: TXMLElementIterator;
  NewObject: T3D;
begin
  { Load Objects }
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      NewObject := LevelObjectFromDOMElement(I.Current);
      if NewObject <> nil then
        Items.Add(NewObject);
    end;
  finally FreeAndNil(I) end;

  { Load other level properties (that are not read in
    TLevelAvailable.LoadFromDOMElement) }

  if DOMGetAttribute(Element, 'played_music_sound', SoundName) then
    PlayedMusicSound := SoundEngine.SoundFromName(SoundName) else
    PlayedMusicSound := stNone;

  if DOMGetAttribute(Element, 'footsteps_sound', SoundName) then
    FootstepsSound := SoundEngine.SoundFromName(SoundName) else
    FootstepsSound := stPlayerFootstepsConcrete;

  FSceneDynamicShadows := false; { default value }
  DOMGetBooleanAttribute(Element, 'scene_dynamic_shadows', FSceneDynamicShadows);
end;

function TLevel.RemoveBoxNode(out Box: TBox3D; const NodeName: string): boolean;
var
  BoxShape: TShape;
begin
  BoxShape := MainScene.Shapes.FindBlenderMesh(NodeName);
  Result := BoxShape <> nil;
  if Result then
  begin
    { When node with name NodeName is found, then we calculate our
      Box from this node (and we delete this node from the scene,
      as it should not be visible).
      This way we can comfortably set such boxes from Blender. }
    Box := BoxShape.BoundingBox;
    MainScene.RemoveShapeGeometry(BoxShape);
  end;
end;

procedure TLevel.RemoveBoxNodeCheck(out Box: TBox3D; const NodeName: string);
begin
  if not RemoveBoxNode(Box, NodeName) then
    raise EInternalError.CreateFmt('Error in level "%s": no box named "%s" found',
      [Title, NodeName]);
end;

procedure TLevel.ChangeLevelScene;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TLevelArea then
      TLevelArea(Items[I]).ChangeLevelScene(Self);
  end;
end;

procedure TLevel.TraverseForItems(Shape: TShape);

  procedure CreateNewItem(const ItemNodeName: string);
  var
    ItemKind: TItemKind;
    IgnoredBegin, ItemQuantityBegin: Integer;
    ItemKindQuantity, ItemKindShortName: string;
    ItemQuantity: Cardinal;
    ItemStubBoundingBox: TBox3D;
    ItemPosition: TVector3Single;
  begin
    { Calculate ItemKindQuantity }
    IgnoredBegin := Pos('_', ItemNodeName);
    if IgnoredBegin = 0 then
      ItemKindQuantity := ItemNodeName else
      ItemKindQuantity := Copy(ItemNodeName, 1, IgnoredBegin - 1);

    { Calculate ItemKindShortName, ItemQuantity }
    ItemQuantityBegin := CharsPos(['0'..'9'], ItemKindQuantity);
    if ItemQuantityBegin = 0 then
    begin
      ItemKindShortName := ItemKindQuantity;
      ItemQuantity := 1;
    end else
    begin
      ItemKindShortName := Copy(ItemKindQuantity, 1, ItemQuantityBegin - 1);
      ItemQuantity := StrToInt(SEnding(ItemKindQuantity, ItemQuantityBegin));
    end;

    ItemKind := ItemKindWithShortName(ItemKindShortName);
    if ItemKind = nil then
      raise Exception.CreateFmt('Item kind with ShortName "%s" doesn''t exist',
        [ItemKindShortName]);

    ItemStubBoundingBox := Shape.BoundingBox;
    ItemPosition[0] := (ItemStubBoundingBox.Data[0, 0] + ItemStubBoundingBox.Data[1, 0]) / 2;
    ItemPosition[1] := (ItemStubBoundingBox.Data[0, 1] + ItemStubBoundingBox.Data[1, 1]) / 2;
    ItemPosition[2] := ItemStubBoundingBox.Data[0, 2];

    Items.Add(TItemOnLevel.Create(Self, TItem.Create(ItemKind, ItemQuantity),
      ItemPosition));
  end;

const
  ItemPrefix = 'Item';
begin
  if IsPrefix(ItemPrefix, Shape.BlenderMeshName) then
  begin
    { For MenuBackground, item models may be not loaded yet }
    if not MenuBackground then
      CreateNewItem(SEnding(Shape.BlenderMeshName, Length(ItemPrefix) + 1));
    { Don't remove BlenderObjectNode now --- will be removed later.
      This avoids problems with removing nodes while traversing. }
    ItemsToRemove.Add(Shape.BlenderObjectNode);
  end;
end;

procedure TLevel.TraverseForCreatures(Shape: TShape);

  procedure CreateNewCreature(const CreatureNodeName: string);
  var
    StubBoundingBox: TBox3D;
    CreaturePosition, CreatureDirection: TVector3Single;
    CreatureKind: TCreatureKind;
    CreatureKindName: string;
    IgnoredBegin: Integer;
    MaxLifeBegin: Integer;
    IsMaxLife: boolean;
    MaxLife: Single;
  begin
    { calculate CreatureKindName }
    IgnoredBegin := Pos('_', CreatureNodeName);
    if IgnoredBegin = 0 then
      CreatureKindName := CreatureNodeName else
      CreatureKindName := Copy(CreatureNodeName, 1, IgnoredBegin - 1);

    { possibly calculate MaxLife by truncating last part of CreatureKindName }
    MaxLifeBegin := CharsPos(['0'..'9'], CreatureKindName);
    IsMaxLife := MaxLifeBegin <> 0;
    if IsMaxLife then
    begin
      MaxLife := StrToFloat(SEnding(CreatureKindName, MaxLifeBegin));
      CreatureKindName := Copy(CreatureKindName, 1, MaxLifeBegin - 1);
    end;

    { calculate CreaturePosition }
    StubBoundingBox := Shape.BoundingBox;
    CreaturePosition[0] := (StubBoundingBox.Data[0, 0] + StubBoundingBox.Data[1, 0]) / 2;
    CreaturePosition[1] := (StubBoundingBox.Data[0, 1] + StubBoundingBox.Data[1, 1]) / 2;
    CreaturePosition[2] := StubBoundingBox.Data[0, 2];

    { calculate CreatureKind }
    CreatureKind := CreaturesKinds.FindByShortName(CreatureKindName);
    { The creature kind may be unprepared here only because
      --debug-no-creatures was specified. In this case, leave this
      creature kind unprepared and don't add this creature. }
    if not CreatureKind.PrepareRenderDone then
    begin
      Assert(DebugNoCreatures);
      Exit;
    end;

    { calculate CreatureDirection }
    { TODO --- CreatureDirection configurable.
      Right now, it just points to the player start pos --- this is
      more-or-less sensible, usually. }
    CreatureDirection := VectorSubtract(InitialPosition, CreaturePosition);
    if not CreatureKind.Flying then
      MakeVectorsOrthoOnTheirPlane(CreatureDirection, GravityUp);

    { make sure that MaxLife is initialized now }
    if not IsMaxLife then
    begin
      IsMaxLife := true;
      MaxLife := CreatureKind.DefaultMaxLife;
    end;

    CreateCreature(CreatureKind, CreaturePosition, CreatureDirection, MaxLife);
  end;

const
  CreaturePrefix = 'Crea';
begin
  if IsPrefix(CreaturePrefix, Shape.BlenderMeshName) then
  begin
    { For MenuBackground, creature models may be not loaded yet }
    if not MenuBackground then
      CreateNewCreature(SEnding(Shape.BlenderMeshName, Length(CreaturePrefix) + 1));
    { Don't remove BlenderObjectNode now --- will be removed later.
      This avoids problems with removing nodes while traversing. }
    ItemsToRemove.Add(Shape.BlenderObjectNode);
  end;
end;

function TLevel.CreateCreature(const Kind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single; const MaxLife: Single): TCreature;
begin
  Result := Kind.CreateCreature(Self, ALegsPosition,
    ADirection, BaseLights, MaxLife);
  Items.Add(Result);
end;

function TLevel.CreateCreature(const Kind: TCreatureKind;
  const ALegsPosition: TVector3Single;
  const ADirection: TVector3Single): TCreature;
begin
  Result := CreateCreature(Kind, ALegsPosition, ADirection, Kind.DefaultMaxLife);
end;

function TLevel.LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := not Items.SegmentCollision(Pos1, Pos2,
    @MainScene.OctreeCollisions.IgnoreTransparentItem)
end;

function TLevel.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D): boolean;
begin
  Result := Items.MoveAllowed(OldPos, ProposedNewPos, NewPos,
    IsRadius, Radius, OldBox, NewBox, @CollisionIgnoreItem);
  if Result then
    Result := CameraBox.PointInside(NewPos);
end;

function TLevel.MoveAllowed(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D): boolean;
begin
  Result := Items.MoveAllowed(OldPos, NewPos,
    IsRadius, Radius, OldBox, NewBox, @CollisionIgnoreItem);
  if Result then
    Result := CameraBox.PointInside(NewPos);
end;

procedure TLevel.GetHeightAbove(const Position: TVector3Single;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: PTriangle);
begin
  Items.GetHeightAbove(Position, GravityUp, @CollisionIgnoreItem,
    IsAbove, AboveHeight, AboveGround);
end;

function TLevel.CameraMoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if Player <> nil then Player.Disable;
  try
    Result := inherited CameraMoveAllowed(ACamera, ProposedNewPos, NewPos, BecauseOfGravity);
  finally
    if Player <> nil then Player.Enable;
  end;
end;

function TLevel.CameraRay(const RayOrigin, RayDirection: TVector3Single): TRayCollision;
begin
  if Player <> nil then Player.Disable;
  try
    Result := inherited CameraRay(RayOrigin, RayDirection);
  finally
    if Player <> nil then Player.Enable;
  end;
end;

procedure TLevel.CameraGetHeight(ACamera: TWalkCamera;
  out IsAbove: boolean; out AboveHeight: Single; out AboveGround: P3DTriangle);
begin
  if Player <> nil then Player.Disable;
  try
    inherited CameraGetHeight(ACamera, IsAbove, AboveHeight, AboveGround);
  finally
    if Player <> nil then Player.Enable;
  end;
end;

procedure TLevel.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  if not Paused then
    FAnimationTime += CompSpeed;

  inherited;

  if (not Paused) and (ThunderEffect <> nil) then
    ThunderEffect.Idle;
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean;
  const SceneClass: TCastleSceneClass): TCastleScene;
var
  Options: TPrepareResourcesOptions;
begin
  Result := SceneClass.CreateCustomCache(Self, GLContextCache);
  Result.Load(FileName);
  AttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if PrepareBackground then
    Include(Options, prBackground);
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareResources(Options, false, BaseLights);

  if CreateOctreeCollisions then
    Result.Spatial := [ssCollisionOctree];

  Result.FreeResources([frTextureDataInNodes]);

  Result.ProcessEvents := true;
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean): TCastleScene;
begin
  Result := LoadLevelScene(FileName, CreateOctreeCollisions, PrepareBackground,
    TCastleScene);
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean;
  const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
var
  Options: TPrepareResourcesOptions;
begin
  Result := AnimationClass.CreateCustomCache(Self, GLContextCache);
  Result.LoadFromFile(FileName, false, true);

  AnimationAttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareResources(Options, false, BaseLights);

  if CreateFirstOctreeCollisions then
    Result.FirstScene.Spatial := [ssCollisionOctree];

  if CreateLastOctreeCollisions then
    Result.LastScene.Spatial := [ssCollisionOctree];

  Result.FreeResources([frTextureDataInNodes]);

  Result.TimePlaying := false;
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
begin
  Result := LoadLevelAnimation(FileName,
    CreateFirstOctreeCollisions, CreateLastOctreeCollisions,
    TCastlePrecalculatedAnimation);
end;

function TLevel.BossCreatureIndicator(out Life, MaxLife: Single): boolean;
begin
  Result := (BossCreature <> nil) and (not BossCreature.Dead);
  if Result then
  begin
    Life := BossCreature.Life;
    MaxLife := BossCreature.MaxLife;
  end;
end;

procedure TLevel.InitializeLights(const Lights: TLightInstancesList);
begin
  inherited;

  if ThunderEffect <> nil then
    ThunderEffect.AddLight(Lights);
end;

procedure TLevel.RenderFromViewEverything;
begin
  ShadowVolumesDraw := DebugRenderShadowVolume;
  ShadowVolumesPossible := RenderShadowsPossible;
  ShadowVolumes := RenderShadows;

  { Actually, this is needed only when "(not MenuBackground) and ShowDebugInfo".
    But it's practically free, time use isn't really noticeable. }
  ShadowVolumeRenderer.Count := true;

  inherited;
end;

procedure TLevel.BeforeDraw;
begin
  ShadowVolumesDraw := DebugRenderShadowVolume;
  ShadowVolumesPossible := RenderShadowsPossible;
  ShadowVolumes := RenderShadows;

  inherited;
end;

function TLevel.LevelProjectionFarFinal: Single;
begin
  if RenderShadowsPossible and RenderShadows then
    Result := ZFarInfinity else
    Result := LevelProjectionFar;
end;

procedure TLevel.ApplyProjection;
var
  S, C: Extended;
  Fov, Aspect: Single;
begin
  if Camera = nil then
    Camera := CreateDefaultCamera;

  ShadowVolumesDraw := DebugRenderShadowVolume;
  ShadowVolumesPossible := RenderShadowsPossible;
  ShadowVolumes := RenderShadows;

  Fov := ViewAngleDegY;
  Aspect := ContainerWidth / ContainerHeight;

  if SickProjection then
  begin
    SinCos(AnimationTime * SickProjectionSpeed, S, C);
    Fov := Fov + Fov * C * 0.03;
    Aspect := Aspect + Aspect * S * 0.03;
  end;

  { Below is actually quite similar to what "inherited" would do,
    with some limitations (e.g. we always want perspective),
    and we always use our ViewAngleDegY.
    Also, we apply SickProjection. }
  { update glViewport and projection }
  glViewport(0, 0, ContainerWidth, ContainerHeight);
  Camera.ProjectionMatrix := PerspectiveProjection(
    Fov, Aspect, LevelProjectionNear, LevelProjectionFarFinal);
end;

function TLevel.CreateDefaultCamera(AOwner: TComponent): TCamera;
begin
  { This camera is suitable for background level and castle-view-level.
    For actual game, camera will be taken from Player.Camera. }

  Result := TWalkCamera.Create(AOwner);
  (Result as TWalkCamera).Init(
    InitialPosition, InitialDirection, InitialUp, GravityUp,
    0, 0 { unused, we don't use Gravity here });
  (Result as TWalkCamera).MoveSpeed := MoveSpeed;
end;

procedure TLevel.SetSickProjection(const Value: boolean);
begin
  if FSickProjection <> Value then
  begin
    FSickProjection := Value;
    ApplyProjectionNeeded := true;
  end;
end;

procedure TLevel.SetSickProjectionSpeed(const Value: TFloatTime);
begin
  if FSickProjectionSpeed <> Value then
  begin
    FSickProjectionSpeed := Value;
    if SickProjection then ApplyProjectionNeeded := true;
  end;
end;

procedure TLevel.PointingDeviceActivateFailed(const Active: boolean);
begin
  inherited;
  if Active then
    SoundEngine.Sound(stPlayerInteractFailed);
end;

end.
