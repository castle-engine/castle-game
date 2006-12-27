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
  ProgressUnit, VRMLGLAnimation, ALSourceAllocator, Matrix,
  BackgroundGL;

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
  private
    FParentLevel: TLevel;
  public
    constructor Create(AParentLevel: TLevel);

    property ParentLevel: TLevel read FParentLevel;

    { This should render given object.

      It can be optimized to not
      render the object if it's not inside the Frustum.

      It will be called twice for each rendered frame:
      first time with ATransparent = @false, second time with
      ATransparent = @true. You should render only fully opaque
      parts in the first pass and only the transparent parts in the
      second pass. for the reason see TObjectKind.Transparent comments. }
    procedure Render(const Frustum: TFrustum;
      const ATransparent: boolean); virtual; abstract;

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

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single); virtual; abstract;
  end;

  TObjectsListItem_2 = TLevelObject;
  {$I objectslist_2.inc}
  TLevelObjectsList = TObjectsList_2;

  { This a VRML scene that can appear/disappear from the level.

    It's basically just a TVRMLFlatSceneGL instance
    (loaded with LoadLevelScene, always with a DefaultTriangleOctree).
    Plus properties @link(Exists) and @link(Collides) which allow you
    to hide this object from evrything (or only from the collision detection).

    You could as well use TLevelMovingObject with SceneTranslation always
    set to zero vector, but this class may be able to work a little faster. }
  TLevelStaticObject = class(TLevelObject)
  private
    FExists: boolean;
    FCollides: boolean;
    FScene: TVRMLFlatSceneGL;
    FTransparent: boolean;
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string; PrepareBackground: boolean);
    destructor Destroy; override;

    property Scene: TVRMLFlatSceneGL read FScene;

    { This level object must be either completely transparent or
      completely opaque. This property determines which one it is. }
    property Transparent: boolean read FTransparent write FTransparent
      default false;

    { @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection). }
    property Collides: boolean read FCollides write FCollides default true;

    procedure Render(const Frustum: TFrustum;
      const ATransparent: boolean); override;

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

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single); override;
  end;

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
    the Scene translated by SceneTranslation.

    Note that if SceneTranslation actually changes over time then
    you have to take care of at least one additional thing yourself:
    what happens when collision occurs because of the move of this object ?
    I.e. something blocks it's move ? See e.g. the Doom level Idle
    method to see how it can be handled. }
  TLevelMovingObject = class(TLevelObject)
  private
    FScene: TVRMLFlatSceneGL;
    FExists: boolean;
    FCollides: boolean;
    FTransparent: boolean;
  public
    { Constructor. This loads scene (using LoadLevelScene). }
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string; PrepareBackground: boolean);
    destructor Destroy; override;

    property Scene: TVRMLFlatSceneGL read FScene;

    { This level object must be either completely transparent or
      completely opaque. This property determines which one it is. }
    property Transparent: boolean read FTransparent write FTransparent
      default false;

    { @noAutoLinkHere

      TODO: ignored for now. Trivial to implement, but wouldn't be tested for now. }
    property Exists: boolean read FExists write FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection).

      TODO: ignored for now. Trivial to implement, but wouldn't be tested for now. }
    property Collides: boolean read FCollides write FCollides default true;

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

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single); override;
  end;

  { This a VRML scene that can be animated.
    Basically, a TVRMLGLAnimation wrapper.

    After passing here an animation
    (that should be created with LoadLevelAnimation) the animation
    instance is owned by this object.

    For now, collision will be checked (only if @link(Collides) and @link(Exists),
    naturally) by checking always with Animation.FirstScene.DefaultTriangleOctree.
    So you should create this octree, and make sure that the animation is done
    such that the first animation frame is always larger than the others.

    Although it seems like a totally stupid to check with collisions,
    it's suitable for many purposes (see e.g. uses on "castle hall" level),
    it's simple and not memory-consuming, and you don't have to take
    any action when animation frame changes (because AnimationTime changes
    don't change the colliding geometry, so the animation is static from
    the point of view of collision checking routines).

    In the future other collision methods may be available.
    First of all, checking with sum of all bounding boxes, or with particular
    scene time box, should be available.

    You should take care of setting AnimationTime as appropriate if you
    want to really use this animation. }
  TLevelAnimatedObject = class(TLevelObject)
  private
    FAnimation: TVRMLGLAnimation;
    FAnimationTime: Single;
    FExists: boolean;
    FCollides: boolean;
    FTransparent: boolean;
  public
    constructor Create(AParentLevel: TLevel; AnAnimation: TVRMLGLAnimation);
    destructor Destroy; override;

    property Animation: TVRMLGLAnimation read FAnimation;
    property AnimationTime: Single read FAnimationTime write FAnimationTime;
    property Exists: boolean read FExists write FExists default true;
    property Collides: boolean read FCollides write FCollides default true;

    { This level object must be either completely transparent or
      completely opaque. This property determines which one it is. }
    property Transparent: boolean read FTransparent write FTransparent
      default false;

    procedure Render(const Frustum: TFrustum;
      const ATransparent: boolean); override;

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

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single); override;
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
    FFootstepsSound: TSoundType;

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

    { Load TVRMLGLAnimation from *.kanim file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene
        @item call PrepareRender
      ) }
    function LoadLevelAnimation(
      const FileName: string;
      CreateDefaultTriangleOctree: boolean): TVRMLGLAnimation;

    { See @link(SpecialObjectPicked), you can call this from
      overriden SpecialObjectPicked. }
    procedure TimeMessageInteractFailed(const S: string);
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
      This will be called only if you overrided also SpecialObjectsTryPick,
      or you added some objects to @link(Objects) list.

      Set InteractionOccured to true (it will be false on enter)
      if indeed some interaction occured. Note that "interaction occured"
      is not the same as "interaction succeeded". This means that
      even if you just did a message like "You cannot open this door",
      you should still set InteractionOccured to @true.
      When InteractionOccured is @false then picking routine may try
      to pick other points around the screen center.

      The right way to signal to uset that interaction occured
      but failed (like the "You cannot open this door" example above)
      is to make a @code(Sound(stPlayerInteractFailed)).
      In fact, you can call TimeMessageInteractFailed to do
      TimeMessage and @code(Sound(stPlayerInteractFailed)) at once.

      Never call this when Player is Dead. Implementation of this may
      assume that Player is not Dead. }
    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer; var InteractionOccured: boolean); virtual;

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

    { Returns the background that you should use before calling Render
      of this level. @nil if no background should be rendered.

      The default implementation in this class is what
      is usually most natural: return Scene.Background. }
    function Background: TBackgroundGL; virtual;
  end;

  TLevelClass = class of TLevel;

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

uses SysUtils, OpenGLh, Object3dAsVRML,
  CastlePlay, KambiGLUtils, KambiFilesUtils, KambiStringUtils,
  CastleVideoOptions, CastleConfig, CastleTimeMessages,
  CastleKeys, CastleWindow;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TLevelObject --------------------------------------------------------------- }

constructor TLevelObject.Create(AParentLevel: TLevel);
begin
  inherited Create;
  FParentLevel := AParentLevel;
end;

{ TLevelStaticObject --------------------------------------------------------- }

constructor TLevelStaticObject.Create(AParentLevel: TLevel;
  const SceneFileName: string; PrepareBackground: boolean);
begin
  inherited Create(AParentLevel);
  FScene := ParentLevel.LoadLevelScene(SceneFileName, true, PrepareBackground);
  FExists := true;
  FCollides := true;
end;

destructor TLevelStaticObject.Destroy;
begin
  FreeAndNil(FScene);
  inherited;
end;

function TLevelStaticObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    Scene.DefaultTriangleOctree.MoveAllowedSimple(
      OldPos, ProposedNewPos,
      CameraRadius, NoItemIndex, ItemsToIgnoreFunc);
end;

function TLevelStaticObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Scene.DefaultTriangleOctree.SegmentCollision(
      Pos1, Pos2,
      false, NoItemIndex, false, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

function TLevelStaticObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Scene.DefaultTriangleOctree.RayCollision(
      IntersectionDistance,
      Ray0, RayVector,
      false, NoItemIndex, false, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

procedure TLevelStaticObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
var
  IsAboveThis: boolean;
  SqrHeightAboveThis: Single;
begin
  if Exists and Collides then
  begin
    Scene.DefaultTriangleOctree.GetCameraHeight(
      CameraPos, ParentLevel.HomeCameraUp,
      IsAboveThis, SqrHeightAboveThis,
      NoItemIndex, ItemsToIgnoreFunc);

    if IsAboveThis then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveThis;
        SqrHeightAboveTheGround := SqrHeightAboveThis;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveThis);
    end;
  end;
end;

procedure TLevelStaticObject.Render(const Frustum: TFrustum;
  const ATransparent: boolean);
begin
  if Exists and (ATransparent = Transparent) then
    Scene.RenderFrustum(Frustum);
end;

{ TLevelMovingObject --------------------------------------------------------- }

constructor TLevelMovingObject.Create(AParentLevel: TLevel;
  const SceneFileName: string; PrepareBackground: boolean);
begin
  inherited Create(AParentLevel);
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

procedure TLevelMovingObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
var
  IsAboveThis: boolean;
  SqrHeightAboveThis: Single;
  T: TVector3_Single;
begin
  if Exists and Collides then
  begin
    T := SceneTranslation(ParentLevel.AnimationTime);

    Scene.DefaultTriangleOctree.GetCameraHeight(
      VectorSubtract(CameraPos, T.Data),
      ParentLevel.HomeCameraUp,
      IsAboveThis, SqrHeightAboveThis,
      NoItemIndex, ItemsToIgnoreFunc);

    if IsAboveThis then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveThis;
        SqrHeightAboveTheGround := SqrHeightAboveThis;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveThis);
    end;
  end;
end;

{ TLevelAnimatedObject ------------------------------------------------------- }

constructor TLevelAnimatedObject.Create(AParentLevel: TLevel;
  AnAnimation: TVRMLGLAnimation);
begin
  inherited Create(AParentLevel);
  FAnimation := AnAnimation;
  FCollides := true;
  FExists := true;
end;

destructor TLevelAnimatedObject.Destroy;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

function TLevelAnimatedObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    Animation.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
      OldPos, ProposedNewPos,
      CameraRadius, NoItemIndex, ItemsToIgnoreFunc);
end;

function TLevelAnimatedObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Animation.FirstScene.DefaultTriangleOctree.SegmentCollision(
      Pos1, Pos2,
      false, NoItemIndex, false, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

function TLevelAnimatedObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Animation.FirstScene.DefaultTriangleOctree.RayCollision(
      IntersectionDistance,
      Ray0, RayVector,
      false, NoItemIndex, false, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

procedure TLevelAnimatedObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
var
  IsAboveThis: boolean;
  SqrHeightAboveThis: Single;
begin
  if Exists and Collides then
  begin
    Animation.FirstScene.DefaultTriangleOctree.GetCameraHeight(
      CameraPos, ParentLevel.HomeCameraUp,
      IsAboveThis, SqrHeightAboveThis,
      NoItemIndex, ItemsToIgnoreFunc);

    if IsAboveThis then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveThis;
        SqrHeightAboveTheGround := SqrHeightAboveThis;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveThis);
    end;
  end;
end;

procedure TLevelAnimatedObject.Render(const Frustum: TFrustum;
  const ATransparent: boolean);
begin
  if Exists and (ATransparent = Transparent) then
    Animation.SceneFromTime(AnimationTime).RenderFrustum(Frustum);
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
    FScene := TVRMLFlatSceneGL.Create(LoadAsVRML(SceneFileName, false),
      true, roSeparateShapeStates, GLContextCache);

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

    FLightSet := TVRMLLightSetGL.Create(LoadAsVRML(LightSetFileName),
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
var
  I: Integer;
begin
  Scene.DefaultTriangleOctree.GetCameraHeight(
    CameraPos, HomeCameraUp,
    IsAboveTheGround, SqrHeightAboveTheGround,
    NoItemIndex, @CollisionIgnoreItem);

  for I := 0 to Objects.High do
    Objects[I].GetCameraHeight(CameraPos, @CollisionIgnoreItem,
      IsAboveTheGround, SqrHeightAboveTheGround);
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
  { First pass rendering Objects: render non-transparent parts }
  for I := 0 to Objects.High do
    Objects[I].Render(Frustum, false);

  Scene.RenderFrustumOctree(Frustum);

  { Second pass rendering Objects: render transparent parts }
  for I := 0 to Objects.High do
    Objects[I].Render(Frustum, true);
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
  SpecialObjectIndex: Integer; var InteractionOccured: boolean);
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
  Result := TVRMLFlatSceneGL.Create(LoadAsVRML(FileName),
    true, roSeparateShapeStates, GLContextCache);
  AttributesSet(Result.Attributes, btIncrease);

  Result.PrepareRender(PrepareBackground,
    true { true, because this is almost always needed anyway }, false, false);

  if CreateDefaultTriangleOctree then
    Result.DefaultTriangleOctree := Result.CreateTriangleOctree('');
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateDefaultTriangleOctree: boolean): TVRMLGLAnimation;
begin
  Result := TVRMLGLAnimation.CreateFromFile(FileName, GLContextCache);

  AnimationAttributesSet(Result.Attributes, btIncrease);

  Result.PrepareRender(false,
    { DoPrepareBoundingBox is true, because this is almost always needed anyway }
    true,
    false, false, false, false);

  if CreateDefaultTriangleOctree then
    Result.FirstScene.DefaultTriangleOctree :=
      Result.FirstScene.CreateTriangleOctree('');
end;

function TLevel.Background: TBackgroundGL;
begin
  Result := Scene.Background;
end;

procedure TLevel.TimeMessageInteractFailed(const S: string);
begin
  TimeMessage(S);
  Sound(stPlayerInteractFailed);
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
  { LevelsAvailable.LoadFromConfig; will be called from main program,
    because it must be called after LevelsAvailable list if filled. }
finalization
  LevelsAvailable.SaveToConfig;
  FreeWithContentsAndNil(LevelsAvailable);
end.