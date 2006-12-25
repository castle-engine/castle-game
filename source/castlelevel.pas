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

{ TLevel class and some specialized descendants. }

unit CastleLevel;

interface

uses VectorMath, VRMLFlatScene, VRMLFlatSceneGL, VRMLLightSetGL, Boxes3d,
  VRMLNodes, VRMLFields, CastleItems, MatrixNavigation,
  VRMLTriangleOctree, CastleCreatures, VRMLSceneWaypoints, CastleSound,
  KambiUtils, KambiClassUtils, CastlePlayer, GLHeadlight, CastleThunder,
  ProgressUnit, VRMLGLAnimation, ALSourceAllocator, Matrix;

{$define read_interface}

const
  DefaultFootstepsSound = stPlayerFootstepsConcrete;
  DefaultGlobalAmbientLight: TVector4Single = (0.2, 0.2, 0.2, 1.0);

type
  TLevel = class;

  { This is a base class for various objects that can be added to the level.

    What's an "object on the level" ? Well, theoretically anything.
    In the future, all items and creatures and even the player
    could be also treated as "some objects on the level".
    For now, "object on the level" means a static VRML scene (TVRMLFlatSceneGL)
    or VRML animation (TVRMLAnimationGL) that is added to the level.
    Such "object on the level" has additional capabilities, not available
    to normal static parts of the scene. It can appear/disappear from the scene,
    it can move on the scene, etc. --- each TLevelObject descendant defines some
    particular behaviors and limitations that it implements.

    This can be used for things such as moving doors, elavators,
    push buttons, and much more. Roughly speaking, anything that from
    the perspective of the player is part of the level, but internally
    it's not as static as normal level parts.

    Note that you can achieve everything that any TLevelObject gives you
    by overriding TLevel methods instead. Overriding TLevel methods
    is more flexible, but it's also more tiresome --- since you have to
    repeat some dummy tasks every time (see where TLevel implementations
    handles @link(Objects) to know approximately what you may want to
    override in your TLevel descendats).
    Implementing and using universal (reusable) TLevelObject descendants
    is a better idea. }
  TLevelObject = class
    procedure Render(const Frustum: TFrustum); virtual; abstract;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;
  end;

  TObjectsListItem_2 = TLevelObject;
  {$I objectslist_2.inc}
  TLevelObjectsList = TObjectsList_2;

  { This a VRML scene that moves on the level.

    It's basically just a TVRMLFlatSceneGL instance,
    loaded with LoadLevelScene, always with a DefaultTriangleOctree.
    It has a SceneTranslation function (that must be actually defined
    in a descendant) that always says how this object is translated from
    it's original position.

    It can also be told to not participate in collision detection
    (useful to make "fake" geometric walls, i.e. things that
    are rendered but are not in fact seen by game mechanics,
    do not occlude creatures view etc.). See @link(Collides).

    It can also be told that given scene doesn't exist at all for now.
    This is useful for objects that disappear completely from the level
    when something happens. See @link(Exists).

    It has methods to render and query it, that take into account
    the Scene translated by SceneTranslation. }
  TLevelMovingObject = class(TLevelObject)
  private
    FParentLevel: TLevel;
    FScene: TVRMLFlatSceneGL;
    FExists: boolean;
    FCollides: boolean;
  public
    { Constructor. This loads scene (using LoadLevelScene). }
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string; PrepareBackground: boolean);
    destructor Destroy; override;

    property ParentLevel: TLevel read FParentLevel;
    property Scene: TVRMLFlatSceneGL read FScene;

    { @noAutoLinkHere
      TODO: ignored for now. }
    property Exists: boolean read FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection).
      TODO: ignored for now. }
    property Collides: boolean read FCollides default true;

    function SceneTranslation(const AnimationTime: Single):
      TVector3_Single; virtual; abstract;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
  end;

  TLevel = class
  private
    FScene: TVRMLFlatSceneGL;
    FLightSet: TVRMLLightSetGL;
    FCameraRadius: Single;
    FCameraPreferredHeight: Single;
    FProjectionNear: Single;
    FProjectionFar: Single;
    FNavigationSpeed: Single;
    FLevelBox: TBox3d;
    FItems: TItemsOnLevelList;
    FObjects: TLevelObjectsList;
    FHeadlight: TGLHeadlight;

    { Used only within constructor.
      We will process the scene graph, and sometimes it's not comfortable
      to remove the items while traversing --- so we will instead
      put them on this list.

      Be careful: never add here two nodes such that one may be parent
      of another, otherwise freeing one could free the other one too
      early. }
    ItemsToRemove: TVRMLNodesList;

    procedure TraverseForItems(Node: TVRMLNode; State: TVRMLGraphTraverseState);

    function LoadVRMLNode(const FileName: string): TVRMLNode;

    FCreatures: TCreaturesList;
    procedure TraverseForCreatures(Node: TVRMLNode;
      State: TVRMLGraphTraverseState);

    FHomeCameraPos: TVector3Single;
    FHomeCameraDir: TVector3Single;
    FHomeCameraUp: TVector3Single;

    FAnimationTime: Single;

    FSectors: TSceneSectorsList;
    FWaypoints: TSceneWaypointsList;
    FLightCastingShadowsPosition: TVector3Single;

    FWaterBox: TBox3d;
    FAboveWaterBox: TBox3d;

    FPlayedMusicSound: TSoundType;
    FFootstepsSound: TSoundType;
    FGlobalAmbientLight: TVector4Single;
    FThunderEffect: TThunderEffect;

    { Check collision (following MoveAllowedSimple mechanics) with
      all Objects (not with the base level geometry). }
    function ObjectsMoveAllowedSimple(
      const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean;
  protected
    FBossCreature: TCreature;

    { See [http://www.camelot.homedns.org/~michalis/castle-development.php]
      for description of LevelBox and WaterBox trick.
      Remember that this may change Scene.BoundingBox (in case we will
      find and remove the node from Scene). }
    function RemoveBoxNode(out Box: TBox3d; const NodeName: string): boolean;

    { Like RemoveBoxNode, but raise EInternalError if not found. }
    procedure RemoveBoxNodeCheck(out Box: TBox3d; const NodeName: string);

    { This will be called from our constructor before initializing
      our octrees. Even before initializing creatures and items.

      You can override this to do here some operations
      that change the Scene.RootNode (e.g. you can do here tricks like
      extracting some specific objects using RemoveBoxNode).
      Be very cautious what you do here --- remember that this is called
      while TLevel.Create constructor did not finish it's work yet ! }
    procedure ChangeLevelScene; virtual;

    { Just load TVRMLFlatSceneGL from file, doing some common tasks:
      @unorderedList(
        @item sets Attributes according to AttributesSet
        @item optionally create triangle octree
        @item call PrepareRender, optionally with PrepareBackground = @true
      ) }
    function LoadLevelScene(const FileName: string;
      CreateDefaultTriangleOctree, PrepareBackground: boolean): TVRMLFlatSceneGL;

    { Load TVRMLGLAnimation from file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene
        @item call PrepareRender
      ) }
    function LoadLevelAnimation(
      const RootNodes: array of TVRMLNode;
      AOwnsFirstRootNode: boolean;
      const ATimes: array of Single;
      ScenesPerTime: Cardinal;
      AOptimization: TGLRendererOptimization;
      const EqualityEpsilon: Single;
      ATimeLoop, ATimeBackwards: boolean;
      CreateDefaultTriangleOctree: boolean): TVRMLGLAnimation;
  public
    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this. }
    constructor Create; virtual;

    destructor Destroy; override;

    { These will be used in constructor to load level.
      @groupBegin }
    class function SceneFileName: string; virtual; abstract;
    class function LightSetFileName: string; virtual; abstract;
    { @groupEnd }

    { }
    property Scene: TVRMLFlatSceneGL read FScene;
    property LightSet: TVRMLLightSetGL read FLightSet;

    property CameraRadius: Single read FCameraRadius;
    property CameraPreferredHeight: Single read FCameraPreferredHeight;
    property ProjectionNear: Single read FProjectionNear;
    property ProjectionFar: Single read FProjectionFar;
    property NavigationSpeed: Single read FNavigationSpeed;

    { Player position should always be within this box. }
    property LevelBox: TBox3d read FLevelBox;

    property WaterBox: TBox3d read FWaterBox;

    { This is always calculated as a box with X, Y borders just like
      the WaterBox and the Z borders placed such that AboveWaterBox
      is exactly above WaterBox (top of WaterBox is equal to bottom of
      AboveWaterBox) and the height of AboveWaterBox is "rather small".

      The intention of "rather small" is such that when player CameraPos
      is within AboveWaterBox then the player is floating slightly
      above the water --- not immediately falling down, but also
      not drowning.

      In other words, this means that player head is above the water surface
      but his feet are in the water. In some sense he/she is swimming,
      in some not. }
    property AboveWaterBox: TBox3d read FAboveWaterBox;

    { Items lying on the level.
      These Items are owned by level object, so everything remaining
      on this list when we will destroy level will be freed. }
    property Items: TItemsOnLevelList read FItems;

    { Creatures on the level. Note that objects on this list are owned
      by level object. }
    property Creatures: TCreaturesList read FCreatures;

    { Other objects (not items, not creatures) on the level.
      Objects on this list are owned by level object.

      Note that you @italic(can) modify this property
      during TLevel lifetime. However, you have to be aware
      of a couple of limitations:
      @orderedList(
        @item(You cannot modify this
          when something iterates over these Objects.)

        @item(Usually you shouldn't create some TLevelObject
          instances when the game is running (as creating them sometimes needs
          some small but noticeable time, e.g. to build TVRMLFlatSceneGL octree).

          So even if you modify this property while the game plays,
          you should have already prepared instances (created in level
          constructor) to put on this list.)

        @item(Beware that this changes indexes of items, and they may
          be important for your SpecialObjectPicked.)
      )
      Usually, this means that it's more comfortable to just not modify
      Objects list after you added all you need in the constructor.
      You can instead toggle objects state by properties like
      @link(TLevelMovingObject.Exists). }
    property Objects: TLevelObjectsList read FObjects;

    property LightCastingShadowsPosition: TVector3Single
      read FLightCastingShadowsPosition;

    function CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean; virtual;

    { LineOfSight, MoveAllowed and GetCameraHeight perform
      collision detection with the level and level objects.

      Note that MoveAllowed and GetCameraHeight treat transparent
      objects as others --- i.e., they collide. You have to override
      CollisionIgnoreItem to eventually change this for some items
      (transparent or opaque) to make them not colliding.

      But LineOfSight checks just collision between segment (Pos1, Pos2)
      and it *does ignore transparent materials*. This means that
      e.g. creatures can see through glass --- even though they
      can't walk through it. CollisionIgnoreItem doesn't matter
      for LineOfSight.

      @groupBegin }
    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
      virtual;

    function MoveAllowed(const CameraPos: TVector3Single;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; virtual;

    function MoveAllowedSimple(const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; virtual;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
      virtual;
    { @groupEnd }

    { PlayerMoveAllowed and PlayerGetCameraHeight just
      call appropriate non-player methods above.
      They use Navigator.CameraPos, and they use level's CameraRadius
      (i.e. they assume that it's the player who's moving).
      Use these to perform collision detection between player and the level.

      In addition, PlayerMoveAllowed checks collisions with Creatures.

      @groupBegin }
    function PlayerMoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    procedure PlayerGetCameraHeight(Navigator: TMatrixWalker;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    { Call this to render level things. Frustum is current player's frustum. }
    procedure Render(const Frustum: TFrustum); virtual;

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single); virtual;

    { This is the time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: Single read FAnimationTime;

    { Call this when player picked some triangle on the level.
      Distance is the exact distance to picked point.

      Returns whether something was done with response to this pick
      (some message displayed, something happened ?) }
    function TrianglePicked(const Distance: Single;
      const Item: TOctreeItem): boolean; virtual;

    { Override this to allow player to pick some additional objects,
      not contained in @link(Scene) or @link(Objects).
      Ray0 and RayVector describe picking
      ray, RayVector is always normalized (i.e. has length 1).
      If there was a pick: set IntersectionDistance and return
      something >= 0. Otherwise return -1.

      Returned index will be passed to SpecialObjectPicked.

      Default implementation in this class checks here collision
      with all items in @link(Objects) list, in case of collision
      it returns their index. So in descendants, if you override this,
      you should use indexes larger than maximum @link(Objects) size to be safe. }
    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; virtual;

    { Override this to take some action when some special object was picked.
      This will be called only if you overriden also SpecialObjectsTryPick.

      Never call this when Player is Dead. Implementation of this may
      assume that Player is not Dead. }
    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); virtual;

    property HomeCameraPos: TVector3Single read FHomeCameraPos;
    property HomeCameraDir: TVector3Single read FHomeCameraDir;

    { Actually, this must be (0, 0, 1) for this game.
      Some things in this game are prepared to handle any
      HomeCameraUp value --- some not (for simplicity, and sometimes
      code efficiency). }
    property HomeCameraUp: TVector3Single read FHomeCameraUp;

    property Sectors: TSceneSectorsList read FSectors;
    property Waypoints: TSceneWaypointsList read FWaypoints;

    property PlayedMusicSound: TSoundType
      read FPlayedMusicSound write FPlayedMusicSound default stNone;

    property FootstepsSound: TSoundType
      read FFootstepsSound write FFootstepsSound default DefaultFootstepsSound;

    { This is the nice name of the level. }
    class function Title: string; virtual; abstract;

    { This is level number, shown for the player in the menu.
      This *does not* determine the order in which levels are played,
      as levels do not have to be played in linear order.
      However, they are displayed in menu in linear order, and that's
      why this is needed. }
    class function Number: Integer; virtual; abstract;

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

    { Properties of default level's headlight.
      @Nil if no headlight should be used by default for this level.

      Note for descendants: TLevel sets this to nil if NavigationInfo.headlight
      field of level's VRML is FALSE. Otherwise TLevel initializes it by
      TGLHeadlight.Create, i.e. creates an instance of TGLHeadlight
      with default params. Descendants can change the properties of this instance
      in their constructor.

      TLevel destructor frees this object. }
    property Headlight: TGLHeadlight read FHeadlight;

    { For thunder effect. nil if no thunder effect should be done for this level.

      Descendants can set this in their constructor.
      We will call it's Idle, CastlePlay will call it's InitGLLight and Render,
      our destructor will free it. }
    property ThunderEffect: TThunderEffect
      read FThunderEffect write FThunderEffect;

    { Instance of boss creature, if any, on the level. @nil is no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;
  end;

  TLevelClass = class of TLevel;

  TCastleHallLevel = class(TLevel)
  private
    SymbolOpen: boolean;
    SymbolOpenTime: Single;
    Symbol: TVRMLGLAnimation;

    ButtonPressed: boolean;
    ButtonPressedTime: Single;
    Button: TVRMLGLAnimation;

    { HintButtonBox, indicating when to display hint about pressing a button. }
    FHintButtonBox: TBox3d;
    FHintButtonShown: boolean;

    StairsBlocker: TVRMLFlatSceneGL;
    StairsBlockerExists: boolean;

    { Check collision only with Symbol, Button, StairsBlocker
      --- but not with real level geometry
      (i.e. not with things handled by inherited
      MoveAllowed, MoveAllowedSimple). }
    function MoveAllowedAdditionalSimple(
      const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean;

    FLevelExitBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
      override;

    function MoveAllowed(const CameraPos: TVector3Single;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    function MoveAllowedSimple(const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
      override;

    procedure Render(const Frustum: TFrustum); override;
    procedure Idle(const CompSpeed: Single); override;

    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure DestroyStairsBlocker;
  end;

  TGateLevel = class(TLevel)
  private
    FGateExitBox: TBox3d;

    TeleportOpaque, TeleportTransp: TVRMLFlatSceneGL;
    FTeleport1Box, FTeleport2Box: TBox3d;

    Teleport1Rotate: Single;
    Teleport2Rotate: Single;

    Teleport1Destination: TVector3Single;
    Teleport2Destination: TVector3Single;

    SacrilegeAmbushStartingPosition: array[0..5]of TVector3Single;
    SwordAmbushStartingPosition: array[0..2]of TVector3Single;

    SacrilegeAmbushDone: boolean;
    SwordAmbushDone: boolean;

    FSacrilegeBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    function CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single); override;

    procedure Render(const Frustum: TFrustum); override;
  end;

  TTowerLevel = class(TLevel)
  public
    constructor Create; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;
  end;

  TCagesLevel = class(TLevel)
  private
    FSpidersAppearing: TDynVector3SingleArray;
    NextSpidersAppearingTime: Single;

    FHintOpenDoorBox: TBox3d;
    HintOpenDoorBoxShown: boolean;

    FGateExit: TVRMLFlatSceneGL;

    FDoEndSequence: boolean;

    FEndSequence: TVRMLFlatSceneGL;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    procedure Idle(const CompSpeed: Single); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Render(const Frustum: TFrustum); override;

    property EndSequence: TVRMLFlatSceneGL read FEndSequence;

    { True means that GateExit will not be rendered (or collided)
      and EndSequence will be rendered. }
    property DoEndSequence: boolean
      read FDoEndSequence write FDoEndSequence default false;

    function LineOfSight(
      const Pos1, Pos2: TVector3Single): boolean; override;

    function MoveAllowed(const CameraPos: TVector3Single;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    function MoveAllowedSimple(const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single); override;

    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;
  end;

  TDoomLevelDoor = class(TLevelMovingObject)
  private
    UsedSound: TALAllocatedSource;
    procedure SoundSourceUsingEnd(Sender: TALAllocatedSource);
    procedure PlaySound(SoundType: TSoundType);
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string);
    destructor Destroy; override;

    { Constant fields, i.e. once set in TDoomE1M1Level constructor.
      Scene should have door placed in closed position.
      @groupBegin }
    OpenCloseTime: Single;
    OpenMove: TVector3_Single;
    StayOpenTime: Single;
    { @groupEnd }

    { Variable fields. They may change during level lifetime.
      They are initialized automatically in level constructor
      from constant fields above.
      @groupBegin }
    Open: boolean;
    OpenStateChangeTime: Single;
    { @groupEnd }

    procedure DoOpen;
    procedure DoClose;
    procedure RevertDoOpen;

    function SceneTranslation(const AnimationTime: Single):
      TVector3_Single; override;

    function CompletelyOpen: boolean;
    function CompletelyClosed: boolean;

    procedure Render(const Frustum: TFrustum); override;
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TVRMLNode);

    FHintOpenDoorBox: TBox3d;
    HintOpenDoorShown: boolean;

    FakeWall: TVRMLFlatSceneGL;

    Elevator49: TVRMLFlatSceneGL;
    Elevator49Down: boolean;
    Elevator49DownBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    procedure Render(const Frustum: TFrustum); override;

    procedure Idle(const CompSpeed: Single); override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

  TLevelAvailable = class
  public
    LevelClass: TLevelClass;
    AvailableForNewGame: boolean;
    DefaultAvailableForNewGame: boolean;
  end;

  TObjectsListItem_1 = TLevelAvailable;
  {$I objectslist_1.inc}
  TLevelsAvailableList = class(TObjectsList_1)
  private
    function IsSmallerByNumber(const A, B: TLevelAvailable): boolean;
  public
    procedure AddLevelClass(LevelClass: TLevelClass;
      DefaultAvailableForNewGame: boolean = false);

    { raises EInternalError if LevelClass not on the list. }
    function FindLevelClass(LevelClass: TLevelClass): TLevelAvailable;

    procedure SortByNumber;

    procedure LoadFromConfig;
    procedure SaveToConfig;
  end;

var
  { This lists all available TLevel classes, along with information
    whether they are allowed to be placed in "New Game" levels.

    Created in initialization of this unit, destroyed in finalization.
    Owns it's Items. }
  LevelsAvailable: TLevelsAvailableList;

{$undef read_interface}

implementation

uses SysUtils, OpenGLh, BackgroundGL,
  CastlePlay, KambiGLUtils, KambiFilesUtils, KambiStringUtils,
  CastleVideoOptions, CastleConfig, CastleTimeMessages,
  CastleKeys, CastleWindow;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

{ TLevelMovingObject --------------------------------------------------------- }

constructor TLevelMovingObject.Create(AParentLevel: TLevel;
  const SceneFileName: string; PrepareBackground: boolean);
begin
  inherited Create;
  FParentLevel := AParentLevel;
  FScene := ParentLevel.LoadLevelScene(SceneFileName, true, PrepareBackground);
  FExists := true;
  FCollides := true;
end;

destructor TLevelMovingObject.Destroy;
begin
  FreeAndNil(FScene);
  inherited;
end;

function TLevelMovingObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  { I have to check collision between
      Scene + SceneTranslation and (OldPos, ProposedNewPos).
    So it's equivalent to checking for collision between
      Scene and (OldPos, ProposedNewPos) - SceneTranslation
    And this way I can use Scene.DefaultTriangleOctree.MoveAllowedSimple. }

  T := SceneTranslation(ParentLevel.AnimationTime);
  Result := Scene.DefaultTriangleOctree.MoveAllowedSimple(
    VectorSubtract(OldPos, T.Data),
    VectorSubtract(ProposedNewPos, T.Data),
    CameraRadius, NoItemIndex, ItemsToIgnoreFunc);
end;

function TLevelMovingObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
    use Scene.DefaultTriangleOctree.SegmentCollsion with
    SceneTranslation. }

  T := SceneTranslation(ParentLevel.AnimationTime);
  Result := Scene.DefaultTriangleOctree.SegmentCollision(
    VectorSubtract(Pos1, T.Data),
    VectorSubtract(Pos2, T.Data),
    false, NoItemIndex, false, ItemsToIgnoreFunc)
    <> NoItemIndex;
end;

function TLevelMovingObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
    use Scene.DefaultTriangleOctree.RayCollsion with
    SceneTranslation. }

  T := SceneTranslation(ParentLevel.AnimationTime);
  Result := Scene.DefaultTriangleOctree.RayCollision(
    IntersectionDistance,
    VectorSubtract(Ray0, T.Data),
    RayVector,
    false, NoItemIndex, false, ItemsToIgnoreFunc)
    <> NoItemIndex;
end;

{ TLevel --------------------------------------------------------------------- }

constructor TLevel.Create;

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParentNodes;
    Scene.ChangedAll;
  end;

const
  SectorsMargin = 0.5;
var
  NavigationNode: TNodeNavigationInfo;
begin
  inherited Create;

  Progress.Init(1, 'Loading level "' + Title + '"');
  try
    FScene := TVRMLFlatSceneGL.Create(
      LoadVRMLNode(SceneFileName), true, roSeparateShapeStates, GLContextCache);

    { initialize FAnimationTime. Must be initialized before creating creatures. }
    FAnimationTime := 0.0;

    AttributesSet(Scene.Attributes, btIncrease);

    { Calculate HomeCameraPos, HomeCameraDir, HomeCameraUp.
      Must be done before initializing creatures, as they right now
      use HomeCameraPos. FHomeCameraDir, FHomeCameraUp will be
      actually changed later in this procedure. }
    Scene.GetPerspectiveViewpoint(FHomeCameraPos, FHomeCameraDir, FHomeCameraUp);

    ChangeLevelScene;

    ItemsToRemove := TVRMLNodesList.Create;
    try
      { Initialize Items }
      FItems := TItemsOnLevelList.Create;
      Scene.RootNode.TraverseFromDefaultState(TNodeGeneralShape, @TraverseForItems);

      { Initialize Creatures }
      FCreatures := TCreaturesList.Create;
      Scene.RootNode.TraverseFromDefaultState(TNodeGeneralShape, @TraverseForCreatures);

      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    { Calculate LevelBox. }
    if not RemoveBoxNode(FLevelBox, 'LevelBox') then
    begin
      { Set LevelBox to Scene.BoundingBox, and make maximum Z larger. }
      FLevelBox := Scene.BoundingBox;
      FLevelBox[1, 2] += 4 * (LevelBox[1, 2] - LevelBox[0, 2]);
    end;

    if RemoveBoxNode(FWaterBox, 'WaterBox') then
    begin
      FAboveWaterBox := FWaterBox;
      FAboveWaterBox[0, 2] := FWaterBox[1, 2];
      FAboveWaterBox[1, 2] := FAboveWaterBox[0, 2] + 0.4;
    end else
    begin
      FWaterBox := EmptyBox3d;
      FAboveWaterBox := EmptyBox3d;
    end;

    { calculate Sectors and Waypoints }
    FSectors := TSceneSectorsList.Create;
    FWaypoints := TSceneWaypointsList.Create;
    Waypoints.ExtractPositions(Scene.RootNode);
    Sectors.ExtractBoundingBoxes(Scene.RootNode);
    Sectors.LinkToWaypoints(Waypoints, SectorsMargin);
    Scene.ChangedAll;

    NavigationNode := Scene.RootNode.TryFindNode(TNodeNavigationInfo, true)
      as TNodeNavigationInfo;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      FCameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      FCameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      FCameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      FCameraPreferredHeight := FCameraRadius * 5;
    CorrectCameraPreferredHeight(FCameraPreferredHeight, CameraRadius,
      DefaultCrouchHeight, DefaultHeadBobbing);

    if NavigationNode <> nil then
      FNavigationSpeed := NavigationNode.FdSpeed.Value else
      FNavigationSpeed := 1.0;

    if (NavigationNode <> nil) and NavigationNode.FdHeadlight.Value then
      FHeadlight := TGLHeadlight.Create else
      FHeadlight := nil;

    FProjectionNear := CameraRadius * 0.75;
    FProjectionFar := Box3dMaxSize(Scene.BoundingBox) * 5;

    { Fix HomeCameraDir length. Uses CameraRadius and NavigationSpeed. }
    VectorAdjustToLengthTo1st(FHomeCameraDir, CameraRadius *
      0.8 * { I multiply just to get the same thing
      that view3dscene does at this time. }
      NavigationSpeed);

    { Check and fix HomeCameraUp. }
    if not VectorsEqual(Normalized(HomeCameraUp),
      Vector3Single(0, 0, 1), 0.001) then
      raise EInternalError.CreateFmt(
        'Initial camera up vector must be +Z, but is %s',
        [ VectorToRawStr(Normalized(HomeCameraUp)) ]) else
      { Make HomeCameraUp = (0, 0, 1) more "precisely" }
      FHomeCameraUp := Vector3Single(0, 0, 1);

    Scene.BackgroundSkySphereRadius := TBackgroundGL.NearFarToSkySphereRadius
      (ProjectionNear, ProjectionFar);
    Scene.PrepareRender(true, true, false, false);

    FLightSet := TVRMLLightSetGL.Create(LoadVRMLNode(LightSetFileName),
      true,
      { GL_LIGHT0 is reserved for headlight. }
      { GL_LIGHT1 is reserved for thunder effect in cages level.
        So first light is GL_LIGHT2. }
      2, -1);

    { Calculate LightCastingShadowsPosition }
    FLightCastingShadowsPosition := Box3dMiddle(Scene.BoundingBox);
    FLightCastingShadowsPosition[2] := Scene.BoundingBox[1, 2];

    FPlayedMusicSound := stNone;
    FFootstepsSound := DefaultFootstepsSound;

    LevelsAvailable.FindLevelClass(TLevelClass(Self.ClassType)).
      AvailableForNewGame := true;

    FGlobalAmbientLight := DefaultGlobalAmbientLight;

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { Loading octree have their own Progress, so we load them outside our
    progress. }

  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Loading level (triangle octree)');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Loading level (ShapeState octree)');

  FObjects := TLevelObjectsList.Create;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunderEffect);
  FreeAndNil(FHeadlight);
  FreeWithContentsAndNil(FSectors);
  FreeWithContentsAndNil(FWaypoints);
  FreeAndNil(FLightSet);
  FreeAndNil(FScene);
  FreeWithContentsAndNil(FItems);
  FreeWithContentsAndNil(FCreatures);
  FreeWithContentsAndNil(FObjects);
  inherited;
end;

function TLevel.RemoveBoxNode(out Box: TBox3d; const NodeName: string): boolean;
var
  BoxNodeIndex: Integer;
begin
  BoxNodeIndex := Scene.ShapeStates.IndexOfShapeWithParentNamed(NodeName);
  Result := BoxNodeIndex <> -1;
  if Result then
  begin
    { When node with name NodeName is found, then we calculate our
      Box from this node (and we delete this node from the scene,
      as it should not be visible).
      This way we can comfortably set such boxes from Blender. }
    Box := Scene.ShapeStates[BoxNodeIndex].BoundingBox;
    Scene.ShapeStates[BoxNodeIndex].ShapeNode.FreeRemovingFromAllParentNodes;
    Scene.ChangedAll;
  end;
end;

procedure TLevel.RemoveBoxNodeCheck(out Box: TBox3d; const NodeName: string);
begin
  if not RemoveBoxNode(Box, NodeName) then
    raise EInternalError.CreateFmt('Error in level "%s": no box named "%s" found',
      [Title, NodeName]);
end;

procedure TLevel.ChangeLevelScene;
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadVRMLNode(const FileName: string): TVRMLNode;
begin
  Result := ParseVRMLFile(FileName, false);
end;

procedure TLevel.TraverseForItems(Node: TVRMLNode;
  State: TVRMLGraphTraverseState);

  procedure CreateNewItem(const ItemNodeName: string);
  var
    ItemKind: TItemKind;
    IgnoredBegin, ItemQuantityBegin: Integer;
    ItemKindQuantity, ItemKindVRMLNodeName: string;
    ItemQuantity: Cardinal;
    ItemStubBoundingBox: TBox3d;
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

    ItemStubBoundingBox := (Node as TNodeGeneralShape).BoundingBox(State);
    ItemPosition[0] := (ItemStubBoundingBox[0, 0] + ItemStubBoundingBox[1, 0]) / 2;
    ItemPosition[1] := (ItemStubBoundingBox[0, 1] + ItemStubBoundingBox[1, 1]) / 2;
    ItemPosition[2] := ItemStubBoundingBox[0, 2];

    FItems.Add(TItemOnLevel.Create(TItem.Create(ItemKind, ItemQuantity),
      ItemPosition));
  end;

const
  ItemPrefix = 'Item';
var
  ParentIndex: Integer;
  Parent: TVRMLNode;
begin
  for ParentIndex := 0 to Node.ParentNodesCount - 1 do
  begin
    Parent := Node.ParentNodes[ParentIndex];
    if IsPrefix(ItemPrefix, Parent.NodeName) then
    begin
      CreateNewItem(SEnding(Parent.NodeName, Length(ItemPrefix) + 1));
      { Don't remove Parent now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      ItemsToRemove.Add(Parent);
      Break;
    end;
  end;
end;

procedure TLevel.TraverseForCreatures(Node: TVRMLNode;
  State: TVRMLGraphTraverseState);

  procedure CreateNewCreature(const CreatureNodeName: string);
  var
    StubBoundingBox: TBox3d;
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
    StubBoundingBox := (Node as TNodeGeneralShape).BoundingBox(State);
    CreaturePosition[0] := (StubBoundingBox[0, 0] + StubBoundingBox[1, 0]) / 2;
    CreaturePosition[1] := (StubBoundingBox[0, 1] + StubBoundingBox[1, 1]) / 2;
    CreaturePosition[2] := StubBoundingBox[0, 2];

    { calculate CreatureKind }
    CreatureKind := CreaturesKinds.FindByVRMLNodeName(CreatureKindName);

    { calculate CreatureDirection }
    { TODO --- CreatureDirection configurable.
      Right now, it just points to the player start pos --- this is
      more-or-less sensible, usually. }
    CreatureDirection := VectorSubtract(HomeCameraPos, CreaturePosition);
    if not CreatureKind.Flying then
      MakeVectorsOrthoOnTheirPlane(CreatureDirection, HomeCameraUp);

    { make sure that MaxLife is initialized now }
    if not IsMaxLife then
    begin
      IsMaxLife := true;
      MaxLife := CreatureKind.DefaultMaxLife;
    end;

    { calculate Creature }
    Creature := CreatureKind.CreateDefaultCreature(CreaturePosition,
      CreatureDirection, AnimationTime, MaxLife);

    FCreatures.Add(Creature);
  end;

const
  CreaturePrefix = 'Crea';
var
  ParentIndex: Integer;
  Parent: TVRMLNode;
begin
  for ParentIndex := 0 to Node.ParentNodesCount - 1 do
  begin
    Parent := Node.ParentNodes[ParentIndex];
    if IsPrefix(CreaturePrefix, Parent.NodeName) then
    begin
      if not WasParam_DebugNoCreatures then
        CreateNewCreature(SEnding(Parent.NodeName, Length(CreaturePrefix) + 1));
      { Don't remove Parent now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      ItemsToRemove.Add(Parent);
      Break;
    end;
  end;
end;

function TLevel.LineOfSight(
  const Pos1, Pos2: TVector3Single): boolean;
var
  I: Integer;
begin
  Result := Scene.DefaultTriangleOctree.SegmentCollision(
    Pos1, Pos2, false, NoItemIndex, false,
    @Scene.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex;

  if not Result then
    Exit;

  for I := 0 to Objects.High do
    if Objects[I].SegmentCollision(Pos1, Pos2,
      @Scene.DefaultTriangleOctree.IgnoreTransparentItem) then
    begin
      Result := false;
      Exit;
    end;
end;

function TLevel.ObjectsMoveAllowedSimple(
  const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
var
  I: Integer;
begin
  for I := 0 to Objects.High do
    if not Objects[I].MoveAllowedSimple(CameraPos, NewPos,
      MovingObjectCameraRadius, @CollisionIgnoreItem) then
    begin
      Result := false;
      Exit;
    end;

  Result := true;
end;

function TLevel.MoveAllowed(const CameraPos: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Scene.DefaultTriangleOctree.MoveAllowed(
      CameraPos, ProposedNewPos, NewPos, MovingObjectCameraRadius,
      NoItemIndex, @CollisionIgnoreItem) and
    Box3dPointInside(NewPos, LevelBox) and
    ObjectsMoveAllowedSimple(
      CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TLevel.MoveAllowedSimple(const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Box3dPointInside(NewPos, LevelBox) and
    Scene.DefaultTriangleOctree.MoveAllowedSimple(
      CameraPos, NewPos, MovingObjectCameraRadius,
      NoItemIndex, @CollisionIgnoreItem) and
    ObjectsMoveAllowedSimple(
      CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

procedure TLevel.GetCameraHeight(const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
begin
  Scene.DefaultTriangleOctree.GetCameraHeight(
    CameraPos, HomeCameraUp,
    IsAboveTheGround, SqrHeightAboveTheGround,
    NoItemIndex, @CollisionIgnoreItem);
  { TODO: for all Objects override this. Only for Doom E1M1 doors this
    didn't need overriding, since all doors opened down. }
end;

function TLevel.PlayerMoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    { Check collision Player <-> level. }
    MoveAllowed(Navigator.CameraPos, ProposedNewPos, NewPos,
      BecauseOfGravity, CameraRadius) and

    { Check collision Player <-> Creatures here. }
    (Creatures.MoveAllowedSimple(
      Player.BoundingBox(false),
      Player.BoundingBoxAssuming(NewPos, false),
      Navigator.CameraPos, NewPos, nil) = nil);
end;

procedure TLevel.PlayerGetCameraHeight(Navigator: TMatrixWalker;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
begin
  { Check is player standing over level. }
  GetCameraHeight(Navigator.CameraPos, IsAboveTheGround,
    SqrHeightAboveTheGround);

  { Check is player standing over one of the creatures. }
  Creatures.GetCameraHeight(Navigator.CameraPos, IsAboveTheGround,
    SqrHeightAboveTheGround, nil);
end;

procedure TLevel.Render(const Frustum: TFrustum);
var
  I: Integer;
begin
  Scene.RenderFrustumOctree(Frustum);

  for I := 0 to Objects.High do
    Objects[I].Render(Frustum);
end;

procedure TLevel.Idle(const CompSpeed: Single);
begin
  FAnimationTime += CompSpeed / 50;

  if ThunderEffect <> nil then
    ThunderEffect.Idle;
end;

function TLevel.TrianglePicked(const Distance: Single;
  const Item: TOctreeItem): boolean;
begin
  { Nothing to do in this class. }
  Result := false;
end;

function TLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;
var
  I: Integer;
  ThisIntersectionDistance: Single;
begin
  Result := -1;

  for I := 0 to Objects.High do
    if Objects[I].RayCollision(ThisIntersectionDistance, Ray0, RayVector, nil) and
       ( (Result = -1) or
         (ThisIntersectionDistance < IntersectionDistance) ) then
    begin
      IntersectionDistance := ThisIntersectionDistance;
      Result := I;
    end;
end;

procedure TLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
begin
  { Nothing to do in this class. }
end;

function TLevel.CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
  OctreeItemIndex: Integer): boolean;
begin
  { Don't ignore anything in this class. }
  Result := false;
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateDefaultTriangleOctree, PrepareBackground: boolean): TVRMLFlatSceneGL;
begin
  Result := TVRMLFlatSceneGL.Create(LoadVRMLNode(FileName),
    true, roSeparateShapeStates, GLContextCache);
  AttributesSet(Result.Attributes, btIncrease);

  Result.PrepareRender(PrepareBackground,
    true { true, because this is almost always needed anyway }, false, false);

  if CreateDefaultTriangleOctree then
    Result.DefaultTriangleOctree := Result.CreateTriangleOctree('');
end;

function TLevel.LoadLevelAnimation(
  const RootNodes: array of TVRMLNode;
  AOwnsFirstRootNode: boolean;
  const ATimes: array of Single;
  ScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
  const EqualityEpsilon: Single;
  ATimeLoop, ATimeBackwards: boolean;
  CreateDefaultTriangleOctree: boolean): TVRMLGLAnimation;
begin
  Result := TVRMLGLAnimation.Create(RootNodes, AOwnsFirstRootNode,
    ATimes, ScenesPerTime, AOptimization, EqualityEpsilon,
    ATimeLoop, ATimeBackwards, GLContextCache);

  AnimationAttributesSet(Result.Attributes, btIncrease);

  Result.PrepareRender(false,
    { DoPrepareBoundingBox is true, because this is almost always needed anyway }
    true,
    false, false, false, false);

  if CreateDefaultTriangleOctree then
    Result.FirstScene.DefaultTriangleOctree :=
      Result.FirstScene.CreateTriangleOctree('');
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create;
var
  CastleHallLevelPath: string;
begin
  inherited;

  PlayedMusicSound := stCastleHallMusic;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := LoadLevelAnimation(
    [ LoadVRMLNode(CastleHallLevelPath + 'castle_hall_symbol_closed.wrl'),
      LoadVRMLNode(CastleHallLevelPath + 'castle_hall_symbol_open.wrl') ],
    true, [ 0, 1.5 ], 30, roSeparateShapeStatesNoTransform, 0.1, false, false,
    true);

  Button := LoadLevelAnimation(
    [ LoadVRMLNode(CastleHallLevelPath + 'castle_hall_button_not_pressed.wrl'),
      LoadVRMLNode(CastleHallLevelPath + 'castle_hall_button_pressed.wrl') ],
    true, [ 0, 0.5 ], 30, roSeparateShapeStatesNoTransform, 0.01, false, false,
    true);

  StairsBlocker := LoadLevelScene(
    CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl', true, false);
  StairsBlockerExists := true;

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;
end;

destructor TCastleHallLevel.Destroy;
begin
  FreeAndNil(Button);
  FreeAndNil(StairsBlocker);
  FreeAndNil(Symbol);
  inherited;
end;

procedure TCastleHallLevel.ChangeLevelScene;
begin
  inherited;
  RemoveBoxNodeCheck(FLevelExitBox, 'LevelExitBox');
  RemoveBoxNodeCheck(FHintButtonBox, 'HintButtonBox');
end;

class function TCastleHallLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_final.wrl';
end;

class function TCastleHallLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_lights.wrl';
end;

class function TCastleHallLevel.Title: string;
begin
  Result := 'Castle Hall';
end;

class function TCastleHallLevel.Number: Integer;
begin
  Result := 2;
end;

function TCastleHallLevel.LineOfSight(
  const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := inherited;

  Result := Result and
    (Button.FirstScene.DefaultTriangleOctree.SegmentCollision(
      Pos1, Pos2, false, NoItemIndex, false,
      @Button.FirstScene.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex);

  if Result and StairsBlockerExists then
  begin
    Result :=
      (StairsBlocker.DefaultTriangleOctree.SegmentCollision(
        Pos1, Pos2, false, NoItemIndex, false,
        @StairsBlocker.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex);
  end;

  if Result and (not SymbolOpen) then
  begin
    Result :=
      Symbol.FirstScene.DefaultTriangleOctree.SegmentCollision(
        Pos1, Pos2, false, NoItemIndex, false,
        @Symbol.FirstScene.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex;
  end;
end;

function TCastleHallLevel.MoveAllowedAdditionalSimple(
  const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Button.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
      CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);

  if Result and StairsBlockerExists then
  begin
    Result :=
      StairsBlocker.DefaultTriangleOctree.MoveAllowedSimple(
        CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);
  end;

  if Result and (not SymbolOpen) then
  begin
    Result :=
      Symbol.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
        CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);
  end;
end;

function TCastleHallLevel.MoveAllowed(const CameraPos: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  Result := Result and MoveAllowedAdditionalSimple(
    CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TCastleHallLevel.MoveAllowedSimple(const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  Result := Result and MoveAllowedAdditionalSimple(
    CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

procedure TCastleHallLevel.GetCameraHeight(const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

  procedure MakeBonusScene(BonusScene: TVRMLFlatSceneGL);
  var
    IsAboveTheBonusScene: boolean;
    SqrHeightAboveTheBonusScene: Single;
  begin
    BonusScene.DefaultTriangleOctree.GetCameraHeight(
      CameraPos, HomeCameraUp,
      IsAboveTheBonusScene, SqrHeightAboveTheBonusScene,
      NoItemIndex, nil);

    if IsAboveTheBonusScene then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveTheBonusScene;
        SqrHeightAboveTheGround := SqrHeightAboveTheBonusScene;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveTheBonusScene);
    end;
  end;

begin
  inherited GetCameraHeight(CameraPos, IsAboveTheGround, SqrHeightAboveTheGround);

  if not SymbolOpen then
    MakeBonusScene(Symbol.FirstScene);

  if StairsBlockerExists then
    MakeBonusScene(StairsBlocker);

  MakeBonusScene(Button.FirstScene);
end;

procedure TCastleHallLevel.Render(const Frustum: TFrustum);
begin
  if SymbolOpen then
    Symbol.SceneFromTime(AnimationTime - SymbolOpenTime).Render(nil) else
    Symbol.SceneFromTime(0).RenderFrustum(Frustum);

  if ButtonPressed then
    Button.SceneFromTime(AnimationTime - ButtonPressedTime).Render(nil) else
    Button.SceneFromTime(0).RenderFrustum(Frustum);

  if StairsBlockerExists then
    StairsBlocker.RenderFrustum(Frustum);

  { Note that we render Symbol before inherited, i.e. before rendering
    real level --- to allow alpha objects on level to be rendered as last. }
  inherited;
end;

procedure TCastleHallLevel.Idle(const CompSpeed: Single);
const
  WerewolfStartPosition: TVector3Single = (0, 0, -4);
var
  WerewolfCreature: TCreature;
begin
  inherited;

  if Box3dPointInside(Player.Navigator.CameraPos, FLevelExitBox) then
  begin
    LevelFinished(TCagesLevel.Create);
  end;

  if ButtonPressed and
    (AnimationTime - ButtonPressedTime > Button.TimeDuration) then
  begin
    if not SymbolOpen then
    begin
      SymbolOpen := true;
      SymbolOpenTime := AnimationTime;

      WerewolfCreature := Werewolf.CreateDefaultCreature(
        WerewolfStartPosition,
        Vector3Single(0, 1, 0), AnimationTime, Werewolf.DefaultMaxLife);
      Creatures.Add(WerewolfCreature);
      WerewolfCreature.Sound3d(stWerewolfHowling, 1.0);

      FBossCreature := WerewolfCreature;

      Sound3d(stCastleHallSymbolMoving, Vector3Single(0, 0, 0));
    end;
  end;

  if (not FHintButtonShown) and
     Box3dPointInside(Player.Navigator.CameraPos, FHintButtonBox) then
  begin
    TimeMessage('Hint: press this red button with the ' +
      InteractKeyDescription);
    FHintButtonShown := true;
  end;
end;

function TCastleHallLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;

  procedure MakeBonusScene(Scene: TVRMLFlatSceneGL; SpecialObjectIndex: Integer);
  var
    ThisIntersectionDistance: Single;
  begin
    if (Scene.DefaultTriangleOctree.RayCollision(
      ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex,
      false, nil) <> NoItemIndex) and
      ( (Result = -1) or
        (ThisIntersectionDistance < IntersectionDistance) ) then
    begin
      IntersectionDistance := ThisIntersectionDistance;
      Result := SpecialObjectIndex;
    end;
  end;

begin
  Result := inherited SpecialObjectsTryPick(
    IntersectionDistance, Ray0, RayVector);

  MakeBonusScene(Button.FirstScene, 0);

  if StairsBlockerExists then
    MakeBonusScene(StairsBlocker, 1);
end;

procedure TCastleHallLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
begin
  inherited;

  case SpecialObjectIndex of
    0:
      begin
        if Distance < 10.0 then
        begin
          if ButtonPressed then
            TimeMessage('Button is already pressed') else
          begin
            ButtonPressed := true;
            ButtonPressedTime := AnimationTime;
            TimeMessage('You press the button');
          end;
        end else
          TimeMessage('You see a button. You cannot reach it from here');
      end;
    1:begin
        TimeMessage('You are not able to open it');
        Sound(stPlayerInteractFailed);
      end;
  end;
end;

procedure TCastleHallLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword. Otherwise player would start the level
    without any weapon, and there's no weapon to be found on
    the level... }
  NewPlayer.PickItem(TItem.Create(Sword, 1));
end;

procedure TCastleHallLevel.DestroyStairsBlocker;
begin
  if StairsBlockerExists then
  begin
    StairsBlockerExists := false;
    Sound3d(stStairsBlockerDestroyed, Box3dMiddle(StairsBlocker.BoundingBox));
  end;
end;

{ TGateLevel ----------------------------------------------------------------- }

constructor TGateLevel.Create;
begin
  inherited;

  PlayedMusicSound := stGateMusic;

  FFootstepsSound := stPlayerFootstepsGrass;

  TeleportOpaque := LoadLevelScene(
    CastleLevelsPath + 'gate' + PathDelim + 'teleport_opaque.wrl',
    false, false);

  TeleportTransp := LoadLevelScene(
    CastleLevelsPath + 'gate' + PathDelim + 'teleport_transp.wrl',
    false, false);

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
end;

destructor TGateLevel.Destroy;
begin
  FreeAndNil(TeleportOpaque);
  FreeAndNil(TeleportTransp);
  inherited;
end;

procedure TGateLevel.ChangeLevelScene;

  function AmbushStartingPos(const Box: TBox3d): TVector3Single;
  begin
    Result[0] := (Box[0, 0] + Box[1, 0]) / 2;
    Result[1] := (Box[0, 1] + Box[1, 1]) / 2;
    Result[2] := Box[0, 2];
  end;

var
  TempBox: TBox3d;
  I: Integer;
begin
  inherited;

  RemoveBoxNodeCheck(FGateExitBox, 'GateExitBox');

  RemoveBoxNodeCheck(FTeleport1Box, 'Teleport1Box');
  RemoveBoxNodeCheck(FTeleport2Box, 'Teleport2Box');

  RemoveBoxNodeCheck(FSacrilegeBox, 'SacrilegeBox');

  Teleport1Destination := Box3dMiddle(FTeleport2Box);
  Teleport1Destination[0] += 2;
  Teleport1Destination[1] += 2;

  Teleport2Destination := Box3dMiddle(FTeleport1Box);
  Teleport2Destination[0] -= 2;
  Teleport2Destination[1] -= 2;

  for I := 0 to High(SacrilegeAmbushStartingPosition) do
  begin
    RemoveBoxNodeCheck(TempBox, 'SacrilegeGhost_' + IntToStr(I));
    SacrilegeAmbushStartingPosition[I] := AmbushStartingPos(TempBox);
  end;

  for I := 0 to High(SwordAmbushStartingPosition) do
  begin
    RemoveBoxNodeCheck(TempBox, 'SwordGhost_' + IntToStr(I));
    SwordAmbushStartingPosition[I] := AmbushStartingPos(TempBox);
  end;
end;

class function TGateLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'gate' + PathDelim + 'gate_final.wrl';
end;

class function TGateLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'gate' + PathDelim + 'gate_lights.wrl';
end;

class function TGateLevel.Title: string;
begin
  Result := 'The Gate';
end;

class function TGateLevel.Number: Integer;
begin
  Result := 1;
end;

procedure TGateLevel.Idle(const CompSpeed: Single);

  procedure RejectGateExitBox;
  var
    NewPosition: TVector3Single;
  begin
    NewPosition := Player.Navigator.CameraPos;
    { Although I do him knockback, I also change the position
      to make sure that he is thrown outside of FGateExitBox. }
    NewPosition[1] := FGateExitBox[0, 1] - 0.1;
    Player.Navigator.CameraPos := NewPosition;

    Player.Knockback(0, 2, Vector3Single(0, -1, 0));
  end;

  procedure TeleportWork(const TeleportBox: TBox3d;
    const Destination: TVector3Single);
  begin
    if Box3dPointInside(Player.Navigator.CameraPos, TeleportBox) then
    begin
      Player.Navigator.CameraPos := Destination;
      Player.Navigator.CancelFallingDown;

      Sound(stTeleport);
    end;
  end;

  procedure SacrilegeAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
    Creature: TCreature;
  begin
    Sound(stSacrilegeAmbush);
    for I := 0 to High(SacrilegeAmbushStartingPosition) do
    begin
      CreaturePosition := SacrilegeAmbushStartingPosition[I];
      CreatureDirection := VectorSubtract(Player.Navigator.CameraPos,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, Ghost.DefaultMaxLife);
      FCreatures.Add(Creature);
    end;
  end;

  procedure SwordAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
    Creature: TCreature;
  begin
    for I := 0 to High(SwordAmbushStartingPosition) do
    begin
      CreaturePosition := SwordAmbushStartingPosition[I];
      CreatureDirection := VectorSubtract(Player.Navigator.CameraPos,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, Ghost.DefaultMaxLife);
      FCreatures.Add(Creature);
    end;
  end;

begin
  inherited;

  if Box3dPointInside(Player.Navigator.CameraPos, FGateExitBox) then
  begin
    if Player.Items.FindKind(KeyItemKind) = -1 then
    begin
      TimeMessage('You need a key to open this door');
      RejectGateExitBox;
    end else
    if Player.Items.FindKind(Sword) = -1 then
    begin
      TimeMessage('Better find a wepon first to protect yourself in the castle');
      RejectGateExitBox;
    end else
      LevelFinished(TCastleHallLevel.Create);
  end else
  begin
    Teleport1Rotate += 0.2 * CompSpeed;
    Teleport2Rotate += 0.2 * CompSpeed;
    TeleportWork(FTeleport1Box, Teleport1Destination);
    TeleportWork(FTeleport2Box, Teleport2Destination);

    if (not SacrilegeAmbushDone) and
      Box3dPointInside(Player.Navigator.CameraPos, FSacrilegeBox) then
    begin
      SacrilegeAmbushDone := true;
      SacrilegeAmbush;
    end;

    if (not SwordAmbushDone) and
      (Player.Items.FindKind(Sword) <> -1) then
    begin
      SwordAmbushDone := true;
      SwordAmbush;
    end;
  end;
end;

function TGateLevel.CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
  OctreeItemIndex: Integer): boolean;
var
  ItemPtr: POctreeItem;
begin
  Result := inherited;

  ItemPtr := @(Octree.OctreeItems.Items[OctreeItemIndex]);
  Result := Result or
    (ItemPtr^.State.LastNodes.Material.NodeName = 'MatWater');
end;

procedure TGateLevel.Render(const Frustum: TFrustum);

  procedure RenderTeleport(
    const TeleportRotation: Single;
    const TeleportBox: TBox3d;
    TeleportScene: TVRMLFlatSceneGL);
  begin
    if FrustumBox3dCollisionPossibleSimple(Frustum, TeleportBox) then
    begin
      glPushMatrix;
        glTranslatev(Box3dMiddle(TeleportBox));
        glRotatef(TeleportRotation, 1, 1, 0);
        TeleportScene.Render(nil);
      glPopMatrix;
    end;
  end;

begin
  RenderTeleport(Teleport1Rotate, FTeleport1Box, TeleportOpaque);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, TeleportOpaque);
  inherited;
  RenderTeleport(Teleport1Rotate, FTeleport1Box, TeleportTransp);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, TeleportTransp);
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create;
begin
  inherited;

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;
end;

class function TTowerLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_final.wrl';
end;

class function TTowerLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_lights.wrl';
end;

class function TTowerLevel.Title: string;
begin
  Result := 'Tower';
end;

class function TTowerLevel.Number: Integer;
begin
  Result := 99;
end;

{ TCagesLevel ---------------------------------------------------------------- }

constructor TCagesLevel.Create;
var
  BossIndex: Integer;
begin
  inherited;

  PlayedMusicSound := stCagesMusic;

  ThunderEffect := TThunderEffect.Create;

  if Headlight <> nil then
  begin
    Headlight.DiffuseColor := Vector4Single(1, 1, 0.29, 1.0);
    Headlight.SpecularColor := Vector4Single(1, 1, 0.29, 1.0);
    Headlight.Spot := true;
//    Headlight.Attenuation := Vector3Single(1, 0.1, 0);
    Headlight.SpotCutoff := 40;
    Headlight.SpotExponent := 0.1;
  end;

  FSpidersAppearing := TDynVector3SingleArray.Create;
  NextSpidersAppearingTime := 0;

  HintOpenDoorBoxShown := false;

  FEndSequence := LoadLevelScene(CastleLevelsPath +
    'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    false, true);

  FGateExit := LoadLevelScene(
    CastleLevelsPath + 'cages' + PathDelim + 'cages_gate_exit.wrl',
    true, false);

  BossIndex := Creatures.FindKind(SpiderQueen);
  if BossIndex <> -1 then
    FBossCreature := Creatures[BossIndex];
end;

destructor TCagesLevel.Destroy;
begin
  FreeAndNil(FSpidersAppearing);
  FreeAndNil(FEndSequence);
  FreeAndNil(FGateExit);
  inherited;
end;

procedure TCagesLevel.ChangeLevelScene;
begin
  inherited;

  RemoveBoxNodeCheck(FHintOpenDoorBox, 'HintOpenDoorBox');
end;

class function TCagesLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'cages' + PathDelim + 'cages_final.wrl';
end;

class function TCagesLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'cages' + PathDelim + 'cages_lights.wrl';
end;

class function TCagesLevel.Title: string;
begin
  Result := 'Cages';
end;

class function TCagesLevel.Number: Integer;
begin
  Result := 3;
end;

const
  { Remember to make it -1 lower than actual ceiling geometry,
    otherwise the spiders will be created on the ceiling of the model... }
  SpiderZ = 69.0;

procedure TCagesLevel.Idle(const CompSpeed: Single);
const
  { Some SpiderRadius is used to not put spider inside the wall. }
  SpiderRadius = 2;
  MinSpiderX = -11.0  + SpiderRadius;
  MaxSpiderX = 69.0   - SpiderRadius;
  MinSpiderY = -123.0 + SpiderRadius;
  MaxSpiderY = 162.0  - SpiderRadius;

  procedure AppearSpider(const Position: TVector3Single);
  begin
    FSpidersAppearing.AppendItem(Position);
  end;

  function RandomSpiderXY: TVector3Single;
  begin
    Result[0] := MapRange(Random, 0.0, 1.0, MinSpiderX, MaxSpiderX);
    Result[1] := MapRange(Random, 0.0, 1.0, MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

  function RandomSpiderXYAroundPlayer: TVector3Single;
  const
    RandomDist = 10.0;
  begin
    Result[0] := Player.Navigator.CameraPos[0] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[0] := Clamped(Result[0], MinSpiderX, MaxSpiderX);
    Result[1] := Player.Navigator.CameraPos[1] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[1] := Clamped(Result[1], MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

const
  SpidersFallingSpeed = 0.5;
  CreaturesCountToAddSpiders = 20;
var
  IsAboveTheGround: boolean;
  SqrHeightAboveTheGround: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3Single;
  SpiderMoveDistance: Single;
begin
  inherited;

  if not GameWin then
  begin
    { Torch light modify, to make an illusion of unstable light }
    LightSet.Lights.Items[0].LightNode.FdIntensity.Value := Clamped(
        LightSet.Lights.Items[0].LightNode.FdIntensity.Value +
          MapRange(Random, 0, 1, -0.1, 0.1) * CompSpeed,
        0.5, 1);
    LightSet.CalculateLights;

    { Maybe appear new spiders }
    if (Level.Creatures.Count < CreaturesCountToAddSpiders) and
       (not WasParam_DebugNoCreatures) then
    begin
      if NextSpidersAppearingTime = 0 then
      begin
        if AnimationTime > 1 then
        begin
          NextSpidersAppearingTime := AnimationTime + 5 + Random(20);
          for I := 1 to 5 + Random(3) do
            AppearSpider(RandomSpiderXY);
        end;
      end else
      if AnimationTime >= NextSpidersAppearingTime then
      begin
        NextSpidersAppearingTime := AnimationTime + 2 + Random(10);
        for I := 1 to 1 + Random(3) do
          AppearSpider(RandomSpiderXYAroundPlayer);
      end;
    end;

    { Move spiders down }
    I := 0;
    { 2 lines below only to get rid of compiler warnings }
    IsAboveTheGround := false;
    SqrHeightAboveTheGround := 0;
    while I < FSpidersAppearing.Count do
    begin
      GetCameraHeight(FSpidersAppearing.Items[I], IsAboveTheGround,
        SqrHeightAboveTheGround);
      if IsAboveTheGround and
        (SqrHeightAboveTheGround < Sqr(Spider.CameraRadius * 2)) then
      begin
        SpiderPosition := FSpidersAppearing.Items[I];
        SpiderDirection :=
          VectorSubtract(Player.Navigator.CameraPos, SpiderPosition);
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, Level.HomeCameraUp);
        SpiderCreature := Spider.CreateDefaultCreature(
          SpiderPosition, SpiderDirection, AnimationTime, Spider.DefaultMaxLife);
        Creatures.Add(SpiderCreature);
        SpiderCreature.Sound3d(stSpiderAppears, 1.0);
        FSpidersAppearing.Delete(I, 1);
      end else
      begin
        { calculate SpiderMoveDistance }
        SpiderMoveDistance := SpidersFallingSpeed * CompSpeed;
        { Actually IsAboveTheGround should be always @true, the way the
          "Cages" level is designed. However, I want to be safe here,
          in case someone will edit cages level, so I check IsAboveTheGround
          here. }
        if IsAboveTheGround then
          MinTo1st(SpiderMoveDistance,
            Sqrt(SqrHeightAboveTheGround) - Spider.CameraRadius);

        FSpidersAppearing.Items[I][2] -= SpiderMoveDistance;
        Inc(I);
      end;
    end;

    if (not HintOpenDoorBoxShown) and
      Box3dPointInside(Player.Navigator.CameraPos, FHintOpenDoorBox) then
    begin
      HintOpenDoorBoxShown := true;
      TimeMessage('Hint: open this door using the ' +
        InteractKeyDescription);
    end;
  end;
end;

procedure TCagesLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword and 1 bow, to have weapons. }
  NewPlayer.PickItem(TItem.Create(Sword, 1));
  NewPlayer.PickItem(TItem.Create(Bow, 1));
end;

procedure TCagesLevel.Render(const Frustum: TFrustum);
var
  I: Integer;
begin
  { Render spiders before rendering inherited,
    because spiders are not transparent. }
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
    glColorv(Black3Single);
    glBegin(GL_LINES);
      for I := 0 to FSpidersAppearing.High do
      begin
        glVertex3f(FSpidersAppearing.Items[I][0],
                   FSpidersAppearing.Items[I][1], SpiderZ);
        glVertexv(FSpidersAppearing.Items[I]);
      end;
    glEnd;
  glPopAttrib;

  for I := 0 to FSpidersAppearing.High do
  begin
    glPushMatrix;
      glTranslatev(FSpidersAppearing.Items[I]);
      Spider.StandAnimation.Scenes[0].Render(nil);
    glPopMatrix;
  end;

  if not DoEndSequence then
    FGateExit.RenderFrustum(Frustum);

  inherited;

  if DoEndSequence then
    EndSequence.RenderFrustum(Frustum);
end;

function TCagesLevel.LineOfSight(
  const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := inherited;

  if not DoEndSequence then
  begin
    Result := Result and
      (FGateExit.DefaultTriangleOctree.SegmentCollision(
        Pos1, Pos2, false, NoItemIndex, false,
        @FGateExit.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex);
  end;
end;

function TCagesLevel.MoveAllowed(const CameraPos: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  if not DoEndSequence then
  begin
    Result := Result and
      FGateExit.DefaultTriangleOctree.MoveAllowedSimple(
        CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);
  end;
end;

function TCagesLevel.MoveAllowedSimple(const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  if not DoEndSequence then
  begin
    Result := Result and
      FGateExit.DefaultTriangleOctree.MoveAllowedSimple(
        CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);
  end;
end;

procedure TCagesLevel.GetCameraHeight(const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

  procedure MakeBonusScene(BonusScene: TVRMLFlatSceneGL);
  var
    IsAboveTheBonusScene: boolean;
    SqrHeightAboveTheBonusScene: Single;
  begin
    BonusScene.DefaultTriangleOctree.GetCameraHeight(
      CameraPos, HomeCameraUp,
      IsAboveTheBonusScene, SqrHeightAboveTheBonusScene,
      NoItemIndex, nil);

    if IsAboveTheBonusScene then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveTheBonusScene;
        SqrHeightAboveTheGround := SqrHeightAboveTheBonusScene;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveTheBonusScene);
    end;
  end;

begin
  inherited GetCameraHeight(CameraPos, IsAboveTheGround, SqrHeightAboveTheGround);

  if not DoEndSequence then
    MakeBonusScene(FGateExit);
end;

function TCagesLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;

  procedure MakeBonusScene(Scene: TVRMLFlatSceneGL; SpecialObjectIndex: Integer);
  var
    ThisIntersectionDistance: Single;
  begin
    if (Scene.DefaultTriangleOctree.RayCollision(
      ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex,
      false, nil) <> NoItemIndex) and
      ( (Result = -1) or
        (ThisIntersectionDistance < IntersectionDistance) ) then
    begin
      IntersectionDistance := ThisIntersectionDistance;
      Result := SpecialObjectIndex;
    end;
  end;

begin
  Result := inherited SpecialObjectsTryPick(
    IntersectionDistance, Ray0, RayVector);

  if not DoEndSequence then
    MakeBonusScene(FGateExit, 0);
end;

procedure TCagesLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
var
  SpiderQueenIndex: Integer;
begin
  inherited;

  case SpecialObjectIndex of
    0:begin
        if Distance > 10 then
          TimeMessage('You see a door. You''re too far to open it from here') else
        begin
          if Player.Items.FindKind(RedKeyItemKind) <> -1 then
          begin
            SpiderQueenIndex := Creatures.FindKind(SpiderQueen);
            if (SpiderQueenIndex <> -1) and
              (not Creatures[SpiderQueenIndex].Dead) then
            begin
              Player.Knockback(2 + Random(5), 2, Vector3Single(0, -1, 0));
              Sound(stEvilLaugh);
              TimeMessage('No exit for the one who does not fight');
            end else
              LevelFinished(nil);
          end else
            TimeMessage('You need an appropriate key to open this door');
        end;
      end;
  end;
end;

{ TDoomLevelDoor ------------------------------------------------------------- }

constructor TDoomLevelDoor.Create(AParentLevel: TLevel;
  const SceneFileName: string);
begin
  inherited Create(AParentLevel, SceneFileName, false);
end;

destructor TDoomLevelDoor.Destroy;
begin
  if UsedSound <> nil then
  begin
    { We detach ourselved from UsedSound, but we let UsedSound to eventually
      continue playing. }
    UsedSound.OnUsingEnd := nil;
    UsedSound := nil;
  end;

  inherited;
end;

procedure TDoomLevelDoor.SoundSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = UsedSound);
  UsedSound.OnUsingEnd := nil;
  UsedSound := nil;
end;

procedure TDoomLevelDoor.PlaySound(SoundType: TSoundType);
begin
  { Door can play only one sound at a time. }
  if UsedSound <> nil then
    UsedSound.DoUsingEnd;
  UsedSound := Sound3d(SoundType, Box3dMiddle(Scene.BoundingBox));
  if UsedSound <> nil then
    UsedSound.OnUsingEnd := @SoundSourceUsingEnd;
end;

procedure TDoomLevelDoor.DoOpen;
begin
  Open := true;
  OpenStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(stDoorOpen);
end;

procedure TDoomLevelDoor.DoClose;
begin
  Open := false;
  OpenStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(stDoorClose);
end;

procedure TDoomLevelDoor.RevertDoOpen;
begin
  Open := true;
  OpenStateChangeTime := { ParentLevel.AnimationTime -
    (OpenCloseTime - (ParentLevel.AnimationTime - OpenStateChangeTime)) }
    { simplified : }
    2 * ParentLevel.AnimationTime - OpenCloseTime - OpenStateChangeTime;
  PlaySound(stDoorOpen);
end;

function TDoomLevelDoor.SceneTranslation(const AnimationTime: Single):
  TVector3_Single;
begin
  if not Open then
  begin
    if AnimationTime - OpenStateChangeTime > OpenCloseTime then
      { Completely closed. }
      Result.Init_Zero else
      { During closing. }
      Result := OpenMove *
        (1 - (AnimationTime - OpenStateChangeTime) / OpenCloseTime);
  end else
  begin
    if AnimationTime - OpenStateChangeTime > OpenCloseTime then
      { Completely open. }
      Result := OpenMove else
      { During opening. }
      Result := OpenMove *
        ((AnimationTime - OpenStateChangeTime) / OpenCloseTime);
  end;
end;

function TDoomLevelDoor.CompletelyOpen: boolean;
begin
  Result := Open and
    (ParentLevel.AnimationTime - OpenStateChangeTime > OpenCloseTime);
end;

function TDoomLevelDoor.CompletelyClosed: boolean;
begin
  Result := (not Open) and
    (ParentLevel.AnimationTime - OpenStateChangeTime > OpenCloseTime);
end;

procedure TDoomLevelDoor.Render(const Frustum: TFrustum);
begin
  { TODO: this should be implemented in TLevelMovingObject, in a manner
    that is able to use Scene.RenderFrustum always (by translating the
    frustum) }

  { The completely closed door is the most common case
    (since all doors close automatically, and initially all are closed...).
    Fortunately, it's also the case when we have constructed an octree,
    so we can efficiently render it by RenderFrustum call. }
  if CompletelyClosed then
    Scene.RenderFrustum(Frustum) else
    begin
      glPushMatrix;
        glTranslatev(SceneTranslation(ParentLevel.AnimationTime));
        Scene.Render(nil);
      glPopMatrix;
    end;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create;
var
  DoomDoorsPathPrefix: string;

  function MakeDoor(const FileName: string): TDoomLevelDoor;
  begin
    Result := TDoomLevelDoor.Create(Self, DoomDoorsPathPrefix + FileName);

    { Although I didn't know it initially, it turns out that all doors
      on Doom E1M1 level (maybe all doors totally ?) have the same
      values for parameters below. }
    Result.OpenCloseTime := 1.0;
    Result.OpenMove.Init(0, 0, 3.5);
    Result.StayOpenTime := 5.0;

    { All doors are initially closed. }
    Result.Open := false;

    { We set Result.OpenStateChangeTime to a past time, to be sure
      that we don't treat the door as "closing right now". }
    Result.OpenStateChangeTime := - 10 * Result.OpenCloseTime;

    Result.UsedSound := nil;
  end;

begin
  inherited;

  {}//PlayedMusicSound := stDoomE1M1Music;
  {TODO: use elevator}

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;

  DoomDoorsPathPrefix := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' +
    PathDelim;

  Objects.Add(MakeDoor('door2_3_closed.wrl'));
  Objects.Add(MakeDoor('door4_5_closed.wrl'));
  Objects.Add(MakeDoor('door4_7_closed.wrl'));
  Objects.Add(MakeDoor('door5_6_closed.wrl'));

  FakeWall := LoadLevelScene(DoomDoorsPathPrefix + 'fake_wall_final.wrl',
    false, false);

  Elevator49 := LoadLevelScene(DoomDoorsPathPrefix + 'elevator4_9_final.wrl',
    true, false);

  HintOpenDoorShown := false;
end;

destructor TDoomE1M1Level.Destroy;
begin
  FreeAndNil(FakeWall);
  FreeAndNil(Elevator49);

  inherited;
end;

class function TDoomE1M1Level.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_final.wrl';
end;

class function TDoomE1M1Level.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_lights.wrl';
end;

class function TDoomE1M1Level.Title: string;
begin
  Result := 'Doom E1M1';
end;

class function TDoomE1M1Level.Number: Integer;
begin
  Result := 90;
end;

procedure TDoomE1M1Level.Render(const Frustum: TFrustum);
begin
  inherited;

  { FakeWall is always rendered. That's it.
    It never participates in collision detection, line of sight collision, etc. }
  FakeWall.RenderFrustum(Frustum);
end;

procedure TDoomE1M1Level.Idle(const CompSpeed: Single);

  function SomethingWillBlockClosingDoor(const Door: TDoomLevelDoor;
    const NewAnimationTime: Single): boolean;
  var
    DoorBox: TBox3d;
    I: Integer;
  begin
    DoorBox := Box3dTranslate(Door.Scene.BoundingBox,
      Door.SceneTranslation(NewAnimationTime));

    Result := Boxes3dCollision(DoorBox, Player.BoundingBox);
    if Result then
      Exit;

    for I := 0 to Creatures.High do
    begin
      Result := Boxes3dCollision(DoorBox, Creatures[I].BoundingBox);
      if Result then
        Exit;
    end;
  end;

var
  I: Integer;
  Door: TDoomLevelDoor;
  NewAnimationTime: Single;
begin
  { First (before calling inherited) check for all the doors
    that are during closing: if the player or creatures will collide
    with them after inherited will change AnimationTime,
    then we must stop and open again (to avoid
    entering into collision with player/creature because of
    door move). }
  NewAnimationTime := AnimationTime + CompSpeed / 50;
  for I := 0 to Objects.High do
    if Objects[I] is TDoomLevelDoor then
    begin
      Door := TDoomLevelDoor(Objects[I]);

      if (not Door.Open) and
        (AnimationTime - Door.OpenStateChangeTime < Door.OpenCloseTime) and
        SomethingWillBlockClosingDoor(Door, NewAnimationTime) then
        Door.RevertDoOpen;
    end;

  inherited;

  for I := 0 to Objects.High do
    if Objects[I] is TDoomLevelDoor then
    begin
      Door := TDoomLevelDoor(Objects[I]);

      if Door.Open and
        (AnimationTime - Door.OpenStateChangeTime >
          Door.OpenCloseTime + Door.StayOpenTime) then
        Door.DoClose;

      { If the door open/close sound is longer than the Door.OpenCloseTime,
        stop this sound after the door is completely opened/closed. }
      if (AnimationTime - Door.OpenStateChangeTime > Door.OpenCloseTime) and
        (Door.UsedSound <> nil) then
        Door.UsedSound.DoUsingEnd;
    end;

  if (not HintOpenDoorShown) and
    Box3dPointInside(Player.Navigator.CameraPos, FHintOpenDoorBox) then
  begin
    HintOpenDoorShown := true;
    TimeMessage('Hint: open doors using the ' + InteractKeyDescription);
  end;
end;

procedure TDoomE1M1Level.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
var
  Door: TDoomLevelDoor;
begin
  inherited;

  if Between(SpecialObjectIndex, 0, Objects.High) and
     (Objects[SpecialObjectIndex] is TDoomLevelDoor) then
  begin
    Door := TDoomLevelDoor(Objects[SpecialObjectIndex]);
    if Distance > 7 then
      TimeMessage('You see a door. You''re too far to open it from here') else
    { Only if the door is completely closed
      (and not during closing right now) we allow player to open it. }
    if not Door.CompletelyClosed then
      TimeMessage('You see a door. It''s already open') else
      Door.DoOpen;
  end;
end;

procedure TDoomE1M1Level.RenameCreatures(Node: TVRMLNode);
const
  SCreaDoomZomb = 'CreaDoomZomb_';
  SCreaDoomSerg = 'CreaDoomSerg_';
begin
  { This is just a trick to rename all creatures 'DoomZomb' and 'DoomSerg'
    on level just to our 'Alien' creature. In the future maybe we will
    have real (and different) DoomZomb/Serg creatures, then the trick
    below will be removed. }
  if IsPrefix(SCreaDoomZomb, Node.NodeName) then
    Node.NodeName := 'CreaAlien_' + SEnding(Node.NodeName, Length(SCreaDoomZomb) + 1) else
  if IsPrefix(SCreaDoomSerg, Node.NodeName) then
    Node.NodeName := 'CreaAlien_' + SEnding(Node.NodeName, Length(SCreaDoomSerg) + 1);
end;

procedure TDoomE1M1Level.ChangeLevelScene;
begin
  inherited;
  RemoveBoxNodeCheck(FHintOpenDoorBox, 'HintOpenDoorBox');
  Scene.RootNode.EnumerateNodes(@RenameCreatures, true);
  RemoveBoxNodeCheck(Elevator49DownBox, 'Elevator49DownBox');
end;

procedure TDoomE1M1Level.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  NewPlayer.PickItem(TItem.Create(Bow, 1));
  NewPlayer.PickItem(TItem.Create(Quiver, 10));
end;

{ TLevelsAvailableList ------------------------------------------------------- }

procedure TLevelsAvailableList.AddLevelClass(LevelClass: TLevelClass;
  DefaultAvailableForNewGame: boolean);
var
  LevelAvailable: TLevelAvailable;
begin
  LevelAvailable := TLevelAvailable.Create;
  Add(LevelAvailable);
  LevelAvailable.LevelClass := LevelClass;
  LevelAvailable.DefaultAvailableForNewGame := DefaultAvailableForNewGame;
end;

function TLevelsAvailableList.FindLevelClass(
  LevelClass: TLevelClass): TLevelAvailable;
var
  I: Integer;
begin
  for I := 0 to High do
    if Items[I].LevelClass = LevelClass then
      Exit(Items[I]);

  raise EInternalError.CreateFmt(
    'Level %d "%s" not found on LevelsAvailable list',
    [LevelClass.Number, LevelClass.Title]);
end;

function TLevelsAvailableList.IsSmallerByNumber(
  const A, B: TLevelAvailable): boolean;
begin
  Result := A.LevelClass.Number < B.LevelClass.Number;
end;

procedure TLevelsAvailableList.SortByNumber;
begin
  Sort(@IsSmallerByNumber);
end;

procedure TLevelsAvailableList.LoadFromConfig;
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].AvailableForNewGame := ConfigFile.GetValue(
      'levels_available/' + LowerCase(Items[I].LevelClass.ClassName),
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelsAvailableList.SaveToConfig;
var
  I: Integer;
begin
  for I := 0 to High do
    ConfigFile.SetDeleteValue(
      'levels_available/' + LowerCase(Items[I].LevelClass.ClassName),
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

initialization
  LevelsAvailable := TLevelsAvailableList.Create;
  LevelsAvailable.AddLevelClass(TGateLevel, true);
  LevelsAvailable.AddLevelClass(TCastleHallLevel);
  LevelsAvailable.AddLevelClass(TTowerLevel);
  LevelsAvailable.AddLevelClass(TCagesLevel);
  LevelsAvailable.AddLevelClass(TDoomE1M1Level);
  LevelsAvailable.LoadFromConfig;
finalization
  LevelsAvailable.SaveToConfig;
  FreeWithContentsAndNil(LevelsAvailable);
end.