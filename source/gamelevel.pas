{
  Copyright 2006-2011 Michalis Kamburelis.

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

uses VectorMath, SceneCore, Scene, Boxes3D,
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

  { See T3DCustomTranslated

    Note that if Translation actually changes over time then
    you have to think of at least one additional thing yourself:
    what happens when collision occurs because of the move of this object ?
    Should the moving object push other objects (player, creatures, items),
    like the elevator going up ?
    That's the default behavior, but you can turn it off by setting
    MovePushesOthers to @false.
    See e.g. the DoomLevelDoor.BeforeTimeIncrease method to see how the behavior
    "something blocks the move" can be handled. }
  TLevelMovingObject = class(T3DCustomTranslated)
  private
    FMovePushesOthers: boolean;
    FMovePushesOthersUsesBoxes: boolean;
    FAnimationTime: TFloatTime;
  public
    constructor Create(AOwner: TComponent); override;

    { Local object time, always increasing, used to track animations. }
    property AnimationTime: TFloatTime read FAnimationTime;

    function ParentLevel: TLevel;

    { Implements T3D.GetTranslation by always calling
      GetTranslationFromTime(AnimationTime).
      Descendants should override GetTranslationFromTime. }
    function GetTranslation: TVector3Single; override;

    function GetTranslationFromTime(const AnAnimationTime: TFloatTime):
      TVector3Single; virtual; abstract;

    { This will be called at the beginning of our @link(Idle),
      @italic(right before) AnimationTime changes to NewAnimationTime.

      This is usefull for taking care of collision detection issues,
      as our assumption always is that "nothing collides". Which means
      that if you don't want your TLevelMovingObject to collide
      with e.g. player or creatures or items, then you should
      prevent the collision @italic(before it happens).
      This is the place to do it. }
    procedure BeforeTimeIncrease(const NewAnimationTime: TFloatTime); virtual;

    { If @true (and Exists and Collides are also @true)
      then moving this object moves everything on it's way.
      I mean, if you put a creature/player/item on it's way of move,
      the creature/player/item will be pushed by this object.
      This is probably a good thing for things like elevators etc.

      Be aware of some limitations of this collision checking:
      @unorderedList(
        @item(Collisions between this level object and the basic
          level geometry and other level objects is not done.
          Just design your level to make sure that it doesn't ever happen.)

        @item(When pushing the creature/player/item, right now
          we don't check whether the creature/player/item will not be
          pushed into collision with something else.

          For now, design your level to minimize the chance that it will ever happen.
          Although in theory you cannot design your level to guarantee
          that it will never happen (because e.g. a creature may be pushed
          into collision with other creature, and since creatures move
          on their own they can arrange themselves (in theory) in all manners of
          funny configurations...). But in practice it's not so difficult,
          just make sure that there is enough space on the way of move.)
      )
    }
    property MovePushesOthers: boolean
      read FMovePushesOthers write FMovePushesOthers default true;

    { This controls how MovePushesOthers works.
      There are two methods how this may work:
      @orderedList(
        @item(When MovePushesOthersUsesBoxes, then we check each
          possible collider (being it player, creature or item) bounding box
          with bounding box of this level object. If there's a collision,
          then we decide that the collider moves along with this level object
          --- because it's inside this level object (e.g. inside
          the elevator), or it's pushed by this object.

          Disadvantage: we approximate whole level object as it's bounding box.
          It's good only if level object's bounding box is a good approximation
          of it's actual shape. And the level object is relatively small
          --- because even when e.g. player is flying and is exactly in the
          middle of the elevator, he is moved instantly with the elevator
          (instead of hitting elevator floor/ceiling first).
        )

        @item(When not MovePushesOthersUsesBoxes, then we try to check
          using precise octree of level objects (assuming they are
          used in SphereCollision/BoxCollision overrides).
          And for player and creatures with UseBoundingSphere = @true,
          we use their spheres instead of their bounding boxes.
          This is the way of checking that more resembles reality
          (as we use the actual geometry within the octrees instead of
          just the bounding box).

          Disadvantage: if player/creature is pushed up by this level object,
          it takes some time for them to "raise up" to a standing position
          (because we compare using their spheres). Conversely,
          if the level object moves down, it takes some time for a
          player/creature to actually fall down on the level object.
          This means that if the level object doesn't move too fast,
          you get strange effect when e.g. player is standing within
          the level object: the player position seems to bounce within
          the elevator.)) }
    property MovePushesOthersUsesBoxes: boolean
      read FMovePushesOthersUsesBoxes write FMovePushesOthersUsesBoxes
      default true;

    procedure Idle(const CompSpeed: Single); override;
  end;

  { This is a TLevelMovingObject that moves with a constant speed
    from Translation (0, 0, 0) to Translation TranslationEnd.
    They are called @italic(begin position) and @italic(end position).

    In practice, this is less flexible than TLevelMovingObject but often
    more comfortable: you get easy to use GoBeginPosition, GoEndPosition
    properties, you can easily set sounds by SoundGoBeginPosition and
    SoundGoEndPosition etc.
  }
  TLevelLinearMovingObject = class(TLevelMovingObject)
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

    procedure Idle(const CompSpeed: Single); override;
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

    procedure Idle(const CompSpeed: Single); override;
  end;

  TLevel = class(TCastleSceneManager)
  private
    FCameraRadius: Single;
    FCameraPreferredHeight: Single;
    FLevelProjectionNear: Single;
    FLevelProjectionFar: Single;
    FMoveHorizontalSpeed: Single;
    FMoveVerticalSpeed: Single;
    FItemsOnLevel: TItemOnLevelList;
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

    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;

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

    { Load T3DPrecalculatedAnimation from *.kanim file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadows enabled by RenderShadowsPossible))
        @item FreeExternalResources, since they will not be needed anymore
        @item TimePlaying is by default @false, so the animation is not playing.
      ) }
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean): T3DPrecalculatedAnimation;

    { See @link(Picked), you can call this from
      overriden implementations of these. }
    procedure NotificationInteractFailed(const S: string);

    procedure RenderFromViewEverything; override;
    procedure InitializeLights(const Lights: TLightInstancesList); override;
    procedure RenderNeverShadowed(const Params: TRenderParams); override;
    procedure ApplyProjection; override;
  public
    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this.

      Note that ARequiredCreatures reference is simply copied (as we don't
      require anywhere to modify it). }
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

    { Items lying on the level.
      These Items are owned by level object, so everything remaining
      on this list when we will destroy level will be freed. }
    property ItemsOnLevel: TItemOnLevelList read FItemsOnLevel;

    { Creatures on the level. Note that objects on this list are owned
      by level object. }
    property Creatures: TCreatureList read FCreatures;

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
      soemthing stands on another creature/player/item.

      @groupBegin }
    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
      virtual;

    function MoveAllowed(const Position: TVector3Single;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; virtual;

    function MoveAllowedSimple(const Position: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; virtual;

    function MoveBoxAllowedSimple(
      const Position, NewPos: TVector3Single;
      const NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; virtual;

    procedure GetHeightAbove(const Position: TVector3Single;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: PTriangle);
      virtual;
    { @groupEnd }

    procedure RenderShadowVolume; override;

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { This is the time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: TFloatTime read FAnimationTime;

    { Tests for collisions with level @link(Items).

      Ray0 and RayVector describe picking
      ray, RayVector is always normalized (i.e. has length 1).
      If there was a pick: set IntersectionDistance. }
    function TryPick(out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): T3DCollision;

    { Called when level was picked.

      Set InteractionOccurred to true (it will be false on enter)
      if indeed some interaction occurred. Note that "interaction occurred"
      is not the same as "interaction succeeded". This means that
      even if you just did a message like "You cannot open this door",
      you should still set InteractionOccurred to @true.
      When InteractionOccurred is @false then picking routine may try
      to pick other points around the screen center.

      The right way to signal to user that interaction occurred
      but failed (like the "You cannot open this door" example above)
      is to make a @code(Sound(stPlayerInteractFailed)).
      In fact, you can call NotificationInteractFailed to do
      Notification and @code(Sound(stPlayerInteractFailed)) at once.

      Never call this when Player is Dead. Implementation of this may
      assume that Player is not Dead.

      @param(CollisionInfo contains all details.)
    }
    procedure Picked(const Distance: Single; CollisionInfo: T3DCollision;
      var InteractionOccurred: boolean); virtual;

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

    { Just load T3DScene from file, doing some common tasks:
      @unorderedList(
        @item sets Attributes according to AttributesSet
        @item optionally create triangle octree
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadows enabled by RenderShadowsPossible), optionally
          with prBackground)
        @item FreeExternalResources, since they will not be needed anymore
      ) }
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean): T3DScene;

    procedure BeforeDraw; override;
    function CreateDefaultCamera(AOwner: TComponent): TCamera; override;

    { CameraMoveAllowed and CameraGetHeight just
      call appropriate non-player methods.
      They use Camera.Position, and they use level's CameraRadius
      (i.e. they assume that it's the player who's moving).
      Use these to perform collision detection between player and the level.

      In addition, CameraMoveAllowed checks collisions with Creatures.

      @groupBegin }
    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; override;
    procedure CameraGetHeight(ACamera: TWalkCamera;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: P3DTriangle); override;
    { @groupEnd }

    property SickProjection: boolean
      read FSickProjection write SetSickProjection;
    property SickProjectionSpeed: TFloatTime
      read FSickProjectionSpeed write SetSickProjectionSpeed;
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

{ TLevelMovingObject --------------------------------------------------------- }

constructor TLevelMovingObject.Create(AOwner: TComponent);
begin
  inherited;
  FMovePushesOthers := true;
  FMovePushesOthersUsesBoxes := true;
  FAnimationTime := 0;
end;

function TLevelMovingObject.ParentLevel: TLevel;
begin
  Result := Owner as TLevel;
end;

function TLevelMovingObject.GetTranslation: TVector3Single;
begin
  Result := GetTranslationFromTime(AnimationTime);
end;

procedure TLevelMovingObject.BeforeTimeIncrease(
  const NewAnimationTime: TFloatTime);

  function BoundingBoxAssumeTranslation(
    const AssumeTranslation: TVector3Single): TBox3D;
  begin
    if Exists and Collides then
      Result := (inherited BoundingBox).Translate(AssumeTranslation) else
      Result := EmptyBox3D;
  end;

  function SphereCollisionAssumeTranslation(
    const AssumeTranslation: TVector3Single;
    const Pos: TVector3Single; const Radius: Single;
    const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
  begin
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
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
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
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
  Crea: TCreature;
  Item: TItemOnLevel;
begin
  if Exists and Collides and MovePushesOthers then
  begin
    CurrentTranslation := GetTranslationFromTime(AnimationTime);
    NewTranslation := GetTranslationFromTime(NewAnimationTime);

    { It's an often expected situation that TLevelMovingObject
      doesn't move at all, and then Translation doesn't change at all
      (even when compared precisely, without usual epsilon used to compare
      floats). So the check below is worth the time, because we expect
      that usually is will render the rest if the code unnecessary. }

    if not VectorsPerfectlyEqual(CurrentTranslation, NewTranslation) then
    begin
      CurrentBox := BoundingBox;
      NewBox := BoundingBoxAssumeTranslation(NewTranslation);
      Move := NewTranslation - CurrentTranslation;

      if MovePushesOthersUsesBoxes then
      begin
        if Player <> nil then
        begin
          Box := Player.BoundingBox;
          if Box.Collision(NewBox) or
             Box.Collision(CurrentBox) then
            Player.Camera.Position := Player.Camera.Position + Move;
        end;

        for I := 0 to ParentLevel.Creatures.Count - 1 do
        begin
          Crea := ParentLevel.Creatures[I];
          Box := Crea.BoundingBox;
          if Box.Collision(NewBox) or
             Box.Collision(CurrentBox) then
            Crea.LegsPosition := Crea.LegsPosition + Move;
        end;

        for I := 0 to ParentLevel.ItemsOnLevel.Count - 1 do
        begin
          Item := ParentLevel.ItemsOnLevel[I];
          Box := Item.BoundingBox;
          if Box.Collision(NewBox) or
             Box.Collision(CurrentBox) then
            Item.Position := Item.Position + Move;
        end;
      end else
      begin
        if Player <> nil then
        begin
          if SphereCollisionAssumeTranslation(NewTranslation,
            Player.Camera.Position, ParentLevel.CameraRadius,
            @ParentLevel.CollisionIgnoreItem) then
            Player.Camera.Position := Player.Camera.Position + Move;
        end;

        for I := 0 to ParentLevel.Creatures.Count - 1 do
        begin
          Crea := ParentLevel.Creatures[I];
          if Crea.UseBoundingSphere then
          begin
            if SphereCollisionAssumeTranslation(NewTranslation,
              Crea.MiddlePosition, Crea.Kind.CameraRadius,
              @ParentLevel.CollisionIgnoreItem) then
              Crea.LegsPosition := Crea.LegsPosition + Move;
          end else
          begin
            if BoxCollisionAssumeTranslation(NewTranslation,
              Crea.BoundingBox,
              @ParentLevel.CollisionIgnoreItem) then
              Crea.LegsPosition := Crea.LegsPosition + Move;
          end;
        end;

        for I := 0 to ParentLevel.ItemsOnLevel.Count - 1 do
        begin
          Item := ParentLevel.ItemsOnLevel[I];
          if BoxCollisionAssumeTranslation(NewTranslation,
            Item.BoundingBox,
            @ParentLevel.CollisionIgnoreItem) then
            Item.Position := Item.Position + Move;
        end;
      end;
    end;
  end;
end;

procedure TLevelMovingObject.Idle(const CompSpeed: Single);
var
  NewAnimationTime: TFloatTime;
begin
  inherited;

  NewAnimationTime := AnimationTime + CompSpeed;
  BeforeTimeIncrease(NewAnimationTime);
  FAnimationTime := NewAnimationTime;
end;

{ TLevelLinearMovingObject --------------------------------------------------- }

constructor TLevelLinearMovingObject.Create(AOwner: TComponent);
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

destructor TLevelLinearMovingObject.Destroy;
begin
  { Otherwise, if you exit from the game while some sound was played,
    and the sound was e.g. looping (like the elevator on "Tower" level),
    the sound will never get stopped. }
  if UsedSound <> nil then
    UsedSound.DoUsingEnd;

  inherited;
end;

procedure TLevelLinearMovingObject.SoundSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = UsedSound);
  UsedSound.OnUsingEnd := nil;
  UsedSound := nil;
end;

function TLevelLinearMovingObject.SoundPosition: TVector3Single;
begin
  Result := BoundingBox.Middle;
end;

procedure TLevelLinearMovingObject.PlaySound(SoundType: TSoundType;
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

procedure TLevelLinearMovingObject.GoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure TLevelLinearMovingObject.GoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoBeginPosition, SoundGoBeginPositionLooping);
end;

procedure TLevelLinearMovingObject.RevertGoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure TLevelLinearMovingObject.RevertGoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoBeginPositionLooping);
end;

procedure TLevelLinearMovingObject.GoOtherPosition;
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

function TLevelLinearMovingObject.GetTranslationFromTime(
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

function TLevelLinearMovingObject.CompletelyEndPosition: boolean;
begin
  Result := EndPosition and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

function TLevelLinearMovingObject.CompletelyBeginPosition: boolean;
begin
  Result := (not EndPosition) and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

procedure TLevelLinearMovingObject.Idle(const CompSpeed: Single);
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

procedure TLevelHintArea.Idle(const CompSpeed: Single);
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

  FName := AName;
  FSceneFileName := ASceneFileName;
  FTitle := ATitle;
  FTitleHint := ATitleHint;
  FNumber := ANumber;
  FMenuBackground := AMenuBackground;
  FRequiredCreatures := ARequiredCreatures;

  if not DebugNoCreatures then
    RequireCreatures(BaseLights, FRequiredCreatures);

  Progress.Init(1, 'Loading level "' + Title + '"');
  try
    MainScene := T3DScene.CreateCustomCache(Self, GLContextCache);
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
      FItemsOnLevel := TItemOnLevelList.Create(true);
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

    MainScene.CastsShadow := SceneDynamicShadows;

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

  MainScene.Input_PointingDeviceActivate := CastleInput_Interact.Shortcut;
  MainScene.ProcessEvents := true;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunderEffect);
  FreeAndNil(FSectors);
  FreeAndNil(FWaypoints);
  FreeAndNil(FItemsOnLevel);
  FreeAndNil(FCreatures);
  if (FRequiredCreatures <> nil) and not DebugNoCreatures then
    UnRequireCreatures(FRequiredCreatures);
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
  for I := 0 to Items.List.Count - 1 do
  begin
    if Items.List[I] is TLevelArea then
      TLevelArea(Items.List[I]).ChangeLevelScene(Self);
  end;
end;

procedure TLevel.TraverseForItems(Shape: TShape);

  procedure CreateNewItem(const ItemNodeName: string);
  var
    ItemKind: TItemKind;
    IgnoredBegin, ItemQuantityBegin: Integer;
    ItemKindQuantity, ItemKindVRMLNodeName: string;
    ItemQuantity: Cardinal;
    ItemStubBoundingBox: TBox3D;
    ItemPosition: TVector3Single;
  begin
    { Calculate ItemKindQuantity }
    IgnoredBegin := Pos('_', ItemNodeName);
    if IgnoredBegin = 0 then
      ItemKindQuantity := ItemNodeName else
      ItemKindQuantity := Copy(ItemNodeName, 1, IgnoredBegin - 1);

    { Calculate ItemKindVRMLNodeName, ItemQuantity }
    ItemQuantityBegin := CharsPos(['0'..'9'], ItemKindQuantity);
    if ItemQuantityBegin = 0 then
    begin
      ItemKindVRMLNodeName := ItemKindQuantity;
      ItemQuantity := 1;
    end else
    begin
      ItemKindVRMLNodeName := Copy(ItemKindQuantity, 1, ItemQuantityBegin - 1);
      ItemQuantity := StrToInt(SEnding(ItemKindQuantity, ItemQuantityBegin));
    end;

    ItemKind := ItemKindWithVRMLNodeName(ItemKindVRMLNodeName);
    if ItemKind = nil then
      raise Exception.CreateFmt('Item kind with VRMLNodeName "%s" doesn''t exist',
        [ItemKindVRMLNodeName]);

    ItemStubBoundingBox := Shape.BoundingBox;
    ItemPosition[0] := (ItemStubBoundingBox.Data[0, 0] + ItemStubBoundingBox.Data[1, 0]) / 2;
    ItemPosition[1] := (ItemStubBoundingBox.Data[0, 1] + ItemStubBoundingBox.Data[1, 1]) / 2;
    ItemPosition[2] := ItemStubBoundingBox.Data[0, 2];

    FItemsOnLevel.Add(TItemOnLevel.Create(TItem.Create(ItemKind, ItemQuantity),
      ItemPosition));
  end;

const
  ItemPrefix = 'Item';
begin
  if IsPrefix(ItemPrefix, Shape.BlenderMeshName) then
  begin
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
    Creature: TCreature;
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
    CreatureKind := CreaturesKinds.FindByVRMLNodeName(CreatureKindName);
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

    { calculate Creature }
    Creature := CreatureKind.CreateDefaultCreature(CreaturePosition,
      CreatureDirection, AnimationTime, BaseLights, MaxLife);

    FCreatures.Add(Creature);
  end;

const
  CreaturePrefix = 'Crea';
begin
  if IsPrefix(CreaturePrefix, Shape.BlenderMeshName) then
  begin
    CreateNewCreature(SEnding(Shape.BlenderMeshName, Length(CreaturePrefix) + 1));
    { Don't remove BlenderObjectNode now --- will be removed later.
      This avoids problems with removing nodes while traversing. }
    ItemsToRemove.Add(Shape.BlenderObjectNode);
  end;
end;

function TLevel.LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := not Items.SegmentCollision(Pos1, Pos2,
    @MainScene.OctreeCollisions.IgnoreTransparentItem)
end;

function TLevel.MoveAllowed(const Position: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := Items.MoveAllowed(Position, ProposedNewPos, NewPos,
    MovingObjectCameraRadius, @CollisionIgnoreItem);

  if Result then
    Result := CameraBox.PointInside(NewPos);
end;

function TLevel.MoveAllowedSimple(const Position: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := Items.MoveAllowedSimple(Position, NewPos,
    MovingObjectCameraRadius, @CollisionIgnoreItem);

  if Result then
    Result := CameraBox.PointInside(NewPos);
end;

function TLevel.MoveBoxAllowedSimple(const Position: TVector3Single;
  const NewPos: TVector3Single;
  const NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Items.MoveBoxAllowedSimple(Position, NewPos, NewBox,
    @CollisionIgnoreItem);

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
  { Check collision Player <-> level. }
  Result := inherited CameraMoveAllowed(ACamera,
    ProposedNewPos, NewPos, BecauseOfGravity);

  if MenuBackground then Exit;

  { Check collision Player <-> Creatures here. }
  if Result then
    Result := Creatures.MoveAllowedSimple(
      Player.BoundingBox(false),
      Player.BoundingBoxAssuming(NewPos, false),
      ACamera.Position, NewPos, nil) = nil;
end;

procedure TLevel.CameraGetHeight(ACamera: TWalkCamera;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: P3DTriangle);
begin
  { Check is player standing over level. }
  inherited CameraGetHeight(ACamera, IsAbove, AboveHeight, AboveGround);

  if MenuBackground then Exit;

  { Check is player standing over one of the creatures. }
  Creatures.GetHeightAbove(ACamera.Position, IsAbove,
    AboveHeight, AboveGround, nil);
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

{ $define DEBUG_PICK}

function TLevel.TryPick(out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): T3DCollision;
{$ifdef DEBUG_PICK}
var
  S: string;
  I: Integer;
{$endif DEBUG_PICK}
begin
  Result := Items.RayCollision(IntersectionDistance, Ray0, RayVector, nil);

  {$ifdef DEBUG_PICK}
  if Result <> nil then
  begin
    S := 'TLevel.TryPick: [';
    for I := 0 to Result.Hierarchy.Count - 1 do
      S += Result.Hierarchy[I].ClassName + ' ';
    S += Format('], distance %f',
      [IntersectionDistance]);
    Notification(S);
  end;
  {$endif DEBUG_PICK}
end;

procedure TLevel.Picked(const Distance: Single;
  CollisionInfo: T3DCollision; var InteractionOccurred: boolean);
begin
  { Nothing to do in this class. }
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean): T3DScene;
var
  Options: TPrepareResourcesOptions;
begin
  Result := T3DScene.CreateCustomCache(Self, GLContextCache);
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

  Result.Input_PointingDeviceActivate := CastleInput_Interact.Shortcut;
  Result.ProcessEvents := true;
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean): T3DPrecalculatedAnimation;
var
  Options: TPrepareResourcesOptions;
begin
  Result := T3DPrecalculatedAnimation.CreateCustomCache(Self, GLContextCache);
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

procedure TLevel.NotificationInteractFailed(const S: string);
begin
  Notifications.Show(S);
  SoundEngine.Sound(stPlayerInteractFailed);
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

procedure TLevel.RenderNeverShadowed(const Params: TRenderParams);
begin
  inherited;

  { Creatures and items are never in shadow (this looks bad). }

  { for background level view, we do not show creatures / items
    (their kinds are possibly not loaded yet) }
  if MenuBackground then Exit;

  { When GameWin, don't render creatures (as we don't check
    collisions when MovingPlayerEndSequence). }
  if not GameWin then
    Creatures.Render(RenderingCamera.Frustum, Params);
  if not DebugRenderForLevelScreenshot then
    ItemsOnLevel.Render(RenderingCamera.Frustum, Params);
end;

procedure TLevel.RenderShadowVolume;
var
  I: Integer;
begin
  for I := 0 to Creatures.Count - 1 do
    Creatures.Items[I].RenderShadowVolume(ShadowVolumeRenderer);
  inherited;
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

  procedure UpdateCameraProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    Camera.ProjectionMatrix := ProjectionMatrix;
  end;

var
  S, C: Extended;
  Fov, Aspect: Single;
begin
  if Camera = nil then
    Camera := CreateDefaultCamera(Self);

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
  ProjectionGLPerspective(Fov, Aspect, LevelProjectionNear, LevelProjectionFarFinal);

  UpdateCameraProjectionMatrix;
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

end.
