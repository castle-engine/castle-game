{
  Copyright 2006,2007 Michalis Kamburelis.

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

{ TLevel class and some specialized descendants.

  About usage of TBase3D stuff in castle:

  @unorderedList(
    @item(
      T3DCollision.Hierarchy is allowed to be empty.
      This means that Level.Scene itself collided.
    )
  )
}

unit CastleLevel;

interface

uses VectorMath, VRMLScene, VRMLGLScene, VRMLLightSetGL, Boxes3d,
  VRMLNodes, VRMLFields, CastleItems, Cameras,
  VRMLTriangle, GLBase3D,
  CastleCreatures, VRMLSceneWaypoints, CastleSound,
  KambiUtils, KambiClassUtils, CastlePlayer, CastleThunder,
  ProgressUnit, VRMLGLAnimation, ALSourceAllocator, Matrix,
  BackgroundGL, DOM, GameSoundEngine, Base3D,
  ShadowVolumes, Classes, KambiTimeUtils, Frustum;

{$define read_interface}

const
  DefaultGlobalAmbientLight: TVector4Single = (0.2, 0.2, 0.2, 1.0);

type
  TLevel = class;

  ICastle3D = interface
  ['{7B42F707-3510-475D-ACA0-D83B218CF54F}']
    function GetParentLevel: TLevel;
    procedure SetParentLevel(const Value: TLevel);

    { Level object that contains this 3D object. You must assign this
      to something non-nil right after creation, and usually you cannot
      change it later. }
    property ParentLevel: TLevel read GetParentLevel write SetParentLevel;

    { This will be called at the beginning of TLevel.Idle, @italic(before)
      ParentLevel.AnimationTime changes to NewAnimationTime.

      This is usefull for taking care of collision detection issues,
      as our assumption always is that "nothing collides". Which means
      that if you don't want your TLevelMovingObject to collide
      with e.g. player or creatures or items, then you should
      prevent the collision @italic(before it happens).
      This is the place to do it. }
    procedure BeforeIdle(const NewAnimationTime: TKamTime);
  end;

  { This a VRML scene that can appear/disappear from the level.

    It's basically just a TVRMLGLScene instance
    (loaded with LoadLevelScene, always with a OctreeCollisions).
    Plus properties @link(Exists) and @link(Collides) which allow you
    to hide this object from evrything (or only from the collision detection). }
  TLevelStaticObject = class(TBase3D)
  private
    FScene: TVRMLGLScene;
    FParentLevel: TLevel;
    function GetParentLevel: TLevel;
    procedure SetParentLevel(const Value: TLevel);
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string; PrepareBackground: boolean); reintroduce;
    destructor Destroy; override;

    property ParentLevel: TLevel read GetParentLevel write SetParentLevel;

    property Scene: TVRMLGLScene read FScene;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;

    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision; override;

    procedure GetCameraHeight(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle); override;

    function BoundingBox: TBox3d; override;
  end;

  { See TCustomTranslated3D

    Note that if Translation actually changes over time then
    you have to think of at least one additional thing yourself:
    what happens when collision occurs because of the move of this object ?
    Should the moving object push other objects (player, creatures, items),
    like the elevator going up ?
    That's the default behavior, but you can turn it off by setting
    MovePushesOthers to @false.
    See e.g. the DoomLevelDoor.BeforeIdle method to see how the behavior
    "something blocks the move" can be handled. }
  TLevelMovingObject = class(TCustomTranslated3D, ICastle3D)
  private
    FMovePushesOthers: boolean;
    FMovePushesOthersUsesBoxes: boolean;

    FParentLevel: TLevel;
    function GetParentLevel: TLevel;
    procedure SetParentLevel(const Value: TLevel);
  public
    constructor Create(AParentLevel: TLevel); reintroduce;

    property ParentLevel: TLevel read GetParentLevel write SetParentLevel;

    { Implements TBase3D.GetTranslation by always calling
      GetTranslationFromTime(ParentLevel.AnimationTime).
      Descendants should override GetTranslationFromTime. }
    function GetTranslation: TVector3Single; override;

    function GetTranslationFromTime(const AnimationTime: TKamTime):
      TVector3Single; virtual; abstract;

    procedure BeforeIdle(const NewAnimationTime: TKamTime); virtual;

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

    UsedSound: TALAllocatedSource;
    procedure SoundSourceUsingEnd(Sender: TALAllocatedSource);
    function SoundPosition: TVector3Single;
    procedure PlaySound(SoundType: TSoundType; Looping: boolean);
  public
    constructor Create(AParentLevel: TLevel); reintroduce;
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
    TranslationEnd: TVector3_Single;

    function GetTranslationFromTime(const AnimationTime: TKamTime):
      TVector3Single; override;

    procedure Idle(const CompSpeed: Single); override;
  end;

  { This a VRML scene that can be animated.
    Basically, a TVRMLGLAnimation wrapper.

    After passing here an animation
    (that should be created with LoadLevelAnimation) the animation
    instance is owned by this object.

    You should take care of setting AnimationTime as appropriate if you
    want to really use this animation. }
  TLevelAnimatedObject = class(TBase3D)
  private
    FAnimation: TVRMLGLAnimation;
    FAnimationTime: TKamTime;
    FCollisionUseLastScene: boolean;

    FParentLevel: TLevel;
    function GetParentLevel: TLevel;
    procedure SetParentLevel(const Value: TLevel);
  public
    constructor Create(AParentLevel: TLevel; AnAnimation: TVRMLGLAnimation); reintroduce;
    destructor Destroy; override;

    property ParentLevel: TLevel read GetParentLevel write SetParentLevel;

    property Animation: TVRMLGLAnimation read FAnimation;
    property AnimationTime: TKamTime read FAnimationTime write FAnimationTime;

    { See TVRMLGLAnimation.CollisionUseLastScene. }
    property CollisionUseLastScene: boolean
      read FCollisionUseLastScene
      write FCollisionUseLastScene default false;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;

    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision; override;

    procedure GetCameraHeight(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle); override;

    function BoundingBox: TBox3d; override;
  end;

  { This is the TLevelAnimatedObject descendant that is easier to use:
    instead of taking care of AnimationTime, you only call it's Play method.
    When the animation is stopped, AnimationTime is kept at 0. }
  TLevelSimpleAnimatedObject = class(TLevelAnimatedObject)
  private
    FStarted: boolean;
    FPlayStartTime: Single;
  public
    constructor Create(AParentLevel: TLevel; AnAnimation: TVRMLGLAnimation);

    property Started: boolean read FStarted;
    property PlayStartTime: Single read FPlayStartTime;

    procedure Play;
    procedure Stop;

    procedure Idle(const CompSpeed: Single); override;
  end;

  { This is an abstract class for a special group of objects:
    they define an invisible and non-colliding areas on the level
    that...well, have @italic(some purpose). What exactly this
    "purpose" is, is defined in each TLevelArea descendant.

    This class defines only a properties to define the area.
    For now, each area is just one TBox3d. }
  TLevelArea = class(TBase3D)
  private
    FVRMLName: string;
    FBox: TBox3d;

    FParentLevel: TLevel;
    function GetParentLevel: TLevel;
    procedure SetParentLevel(const Value: TLevel);

    { Area. Default value is EmptyBox3d. }
    property Box: TBox3d read FBox write FBox;
  public
    constructor Create(AParentLevel: TLevel); reintroduce;

    property ParentLevel: TLevel read GetParentLevel write SetParentLevel;

    { Name used to recognize this object's area in level VRML file.

      If this object is present during ChangeLevelScene call from
      TLevel constructor then the shape with a parent named like VRMLName
      will be removed from VRML file, and it's BoundingBox will be used
      as Box3d of this object.

      This way you can easily configure area of this object in Blender:
      just add a cube, set it's mesh name to match with this VRMLName,
      and then this cube defines Box3d of this object. }
    property VRMLName: string read FVRMLName write FVRMLName;

    function PointInside(const Point: TVector3Single): boolean;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;
    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const ABox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision; override;
    procedure GetCameraHeight(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle); override;
    function BoundingBox: TBox3d; override;

    { Called from TLevel constructor. This is the place when you
      can modify ParentLevel.Scene.RootNode, e.g. by calling
      RemoveBoxNode. }
    procedure ChangeLevelScene(AParentLevel: TLevel);
  end;

  { This defines area on the level that causes
    a TimeMessage to be displayed when player enters inside.
    The natural use for it is to display various hint messages when player
    is close to something. }
  TLevelHintArea = class(TLevelArea)
  private
    FMessage: string;
    FMessageDone: boolean;
  public
    { Message to this display when player enters Box3d.
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

  TLevel = class
  private
    FScene: TVRMLGLScene;
    FLightSet: TVRMLLightSetGL;
    FCameraRadius: Single;
    FCameraPreferredHeight: Single;
    FProjectionNear: Single;
    FProjectionFar: Single;
    FMoveHorizontalSpeed: Single;
    FMoveVerticalSpeed: Single;
    FLevelBox: TBox3d;
    FItems: TItemsOnLevelList;
    FObjects: TBase3DList;

    { Used only within constructor.
      We will process the scene graph, and sometimes it's not comfortable
      to remove the items while traversing --- so we will instead
      put them on this list.

      Be careful: never add here two nodes such that one may be parent
      of another, otherwise freeing one could free the other one too
      early. }
    ItemsToRemove: TVRMLNodesList;

    procedure TraverseForItems(
      BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
      BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
      Geometry: TVRMLGeometryNode;
      StateStack: TVRMLGraphTraverseStateStack);
  private
    FCreatures: TCreaturesList;
    procedure TraverseForCreatures(
      BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
      BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
      Geometry: TVRMLGeometryNode;
      StateStack: TVRMLGraphTraverseStateStack);
  private
    FInitialPosition: TVector3Single;
    FInitialDirection: TVector3Single;
    FInitialUp: TVector3Single;
    FGravityUp: TVector3Single;

    FAnimationTime: TKamTime;

    FSectors: TSceneSectorsList;
    FWaypoints: TSceneWaypointsList;

    FWaterBox: TBox3d;
    FAboveWaterBox: TBox3d;

    FPlayedMusicSound: TSoundType;
    FGlobalAmbientLight: TVector4Single;
    FThunderEffect: TThunderEffect;

    { Check collision (following MoveAllowedSimple mechanics) with
      all Objects (not with the base level geometry).
      @groupBegin }
    function ObjectsMoveAllowedSimple(
      const Position: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean;
    function ObjectsMoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const BecauseOfGravity: boolean): boolean;
    { @groupEnd }
  private
    FName: string;
    FSceneFileName: string;
    FTitle: string;
    FTitleHint: string;
    FNumber: Integer;
    FLightSetFileName: string;

    procedure LoadFromDOMElement(Element: TDOMElement);
  private
    FMenuBackground: boolean;
    FSceneDynamicShadows: boolean;

    FRequiredCreatures: TStringList;

    FBumpMappingLightAmbientColor: array [boolean] of TVector4Single;
    function GetBumpMappingLightAmbientColor(const Lighted: boolean): TVector4Single;
    procedure SetBumpMappingLightAmbientColor(const Lighted: boolean; const Value: TVector4Single);
  private
    FBumpMappingLightDiffuseColor: TVector4Single;
    procedure SetBumpMappingLightDiffuseColor(const Value: TVector4Single);
  protected
    FBossCreature: TCreature;
    FFootstepsSound: TSoundType;

    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;

    { See [http://vrmlengine.sourceforge.net/castle-development.php]
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
      while TLevel.Create constructor did not finish it's work yet !

      This is your only chance to insert into Objects list some
      object that has meaningfull ChangeLevelScene method. }
    procedure ChangeLevelScene; virtual;

    { Just load TVRMLGLScene from file, doing some common tasks:
      @unorderedList(
        @item sets Attributes according to AttributesSet
        @item optionally create triangle octree
        @item(call PrepareRender, with prBoundingBox, prShadowVolume
          (if shadows enabled by RenderShadowsPossible), optionally
          with prBackground)
        @item FreeExternalResources, since they will not be needed anymore
      ) }
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean): TVRMLGLScene;

    { Load TVRMLGLAnimation from *.kanim file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareRender, with prBoundingBox, prShadowVolume
          (if shadows enabled by RenderShadowsPossible))
        @item FreeExternalResources, since they will not be needed anymore
      ) }
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean): TVRMLGLAnimation;

    { See @link(SpecialObjectPicked) and @link(Picked), you can call this from
      overriden implementations of these. }
    procedure TimeMessageInteractFailed(const S: string);
  public
    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this.

      Note that ARequiredCreatures reference is simply copied (as we don't
      require anywhere to modify it). }
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); virtual;

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
    property LightSetFileName: string read FLightSetFileName;
    { @groupEnd }

    { }
    property Scene: TVRMLGLScene read FScene;
    property LightSet: TVRMLLightSetGL read FLightSet;

    property CameraRadius: Single read FCameraRadius;
    property CameraPreferredHeight: Single read FCameraPreferredHeight;
    property ProjectionNear: Single read FProjectionNear;
    property ProjectionFar: Single read FProjectionFar;
    property MoveHorizontalSpeed: Single read FMoveHorizontalSpeed;
    property MoveVerticalSpeed: Single read FMoveVerticalSpeed;

    { Player position should always be within this box. }
    property LevelBox: TBox3d read FLevelBox;

    property WaterBox: TBox3d read FWaterBox;

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
          some small but noticeable time, e.g. to build TVRMLGLScene octree).

          So even if you modify this property while the game plays,
          you should have already prepared instances (created in level
          constructor) to put on this list.)

        @item(Beware that this changes indexes of items, and they may
          be important for your @link(Picked).)
      )
      Usually, this means that it's more comfortable to just not modify
      Objects list after you added all you need in the constructor.
      You can instead toggle objects state by properties like
      @link(TLevelMovingObject.Exists). }
    property Objects: TBase3DList read FObjects;

    function CollisionIgnoreItem(
      const Octree: TVRMLBaseTrianglesOctree;
      const Triangle: PVRMLTriangle): boolean; virtual;

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

      Note about GroundItem: it is set to @nil if the ground item
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
      const NewBox: TBox3d;
      const BecauseOfGravity: boolean): boolean; virtual;

    procedure GetCameraHeight(const Position: TVector3Single;
      out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle);
      virtual;
    { @groupEnd }

    { PlayerMoveAllowed and PlayerGetCameraHeightSqr just
      call appropriate non-player methods above.
      They use Camera.Position, and they use level's CameraRadius
      (i.e. they assume that it's the player who's moving).
      Use these to perform collision detection between player and the level.

      In addition, PlayerMoveAllowed checks collisions with Creatures.

      In addition, PlayerGetCameraHeightSqr sets Player.Ground
      (to the ground item under the player, or to @nil
      if player is not IsAboveTheGround).

      @groupBegin }
    function PlayerMoveAllowed(Camera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    procedure PlayerGetCameraHeightSqr(Camera: TWalkCamera;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    { Call this to render level things: level scene and level objects
      in @link(Objects). Frustum is current player's frustum. }
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); virtual;

    { Render shadow quads for all the things rendered by @link(Render).
      It does shadow volumes culling inside  (so ShadowVolumes should
      have FrustumCullingInit already initialized). }
    procedure RenderShadowVolume(
      ShadowVolumes: TShadowVolumes); virtual;

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single); virtual;

    { This is the time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: TKamTime read FAnimationTime;

    { Override this to allow player to pick some additional objects,
      not contained in @link(Scene) or @link(Objects).
      Ray0 and RayVector describe picking
      ray, RayVector is always normalized (i.e. has length 1).
      If there was a pick: set IntersectionDistance and return
      something >= 0. Otherwise return -1.

      Returned index will be passed to SpecialObjectPicked. }
    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; virtual;

    { Override this to take some action when some special object was picked.
      This will be called only if you overrided also SpecialObjectsTryPick.

      Set InteractionOccured to true (it will be false on enter)
      if indeed some interaction occured. Note that "interaction occured"
      is not the same as "interaction succeeded". This means that
      even if you just did a message like "You cannot open this door",
      you should still set InteractionOccured to @true.
      When InteractionOccured is @false then picking routine may try
      to pick other points around the screen center.

      The right way to signal to user that interaction occured
      but failed (like the "You cannot open this door" example above)
      is to make a @code(Sound(stPlayerInteractFailed)).
      In fact, you can call TimeMessageInteractFailed to do
      TimeMessage and @code(Sound(stPlayerInteractFailed)) at once.

      Never call this when Player is Dead. Implementation of this may
      assume that Player is not Dead. }
    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer; var InteractionOccured: boolean); virtual;

    { This tests for collisions with level base Scene and level @link(Objects). }
    function TryPick(out IntersectionDistance: Single;
      out LevelObjectIndex: Integer;
      const Ray0, RayVector: TVector3Single): T3DCollision;

    { This is called when level was picked --- either the level Scene
      itself, or one of @link(Objects) on the level.

      @param CollisionInfo contains all details.
      @param(LevelObjectIndex contains index to @link(Objects), or -1
        if it was the Scene itself.)
      @param(Distance,InteractionOccured see SpecialObjectPicked)
    }
    procedure Picked(const Distance: Single; CollisionInfo: T3DCollision;
      LevelObjectIndex: Integer;
      var InteractionOccured: boolean); virtual;

    property InitialPosition : TVector3Single read FInitialPosition;
    property InitialDirection: TVector3Single read FInitialDirection;
    property InitialUp       : TVector3Single read FInitialUp;

    { Actually, this must be (0, 0, 1) for this game.
      Some things in this game are prepared to handle any
      GravityUp value --- but some not (for simplicity, and sometimes
      code efficiency). }
    property GravityUp: TVector3Single read FGravityUp;

    property Sectors: TSceneSectorsList read FSectors;
    property Waypoints: TSceneWaypointsList read FWaypoints;

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
      We will call it's Idle, CastlePlay will call it's InitGLLight and Render,
      our destructor will free it. }
    property ThunderEffect: TThunderEffect
      read FThunderEffect write FThunderEffect;

    { This returns whether and what to show on boss creature indicator.
      Default implementation in this class uses BossCreature property:
      if it's non-nil and BossCreature is alive, then indicator shows
      BossCreature life. }
    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; virtual;

    { Returns the background that you should use before calling Render
      of this level. @nil if no background should be rendered.

      The default implementation in this class is what
      is usually most natural: return Scene.Background. }
    function Background: TBackgroundGL; virtual;

    property MenuBackground: boolean read FMenuBackground write FMenuBackground;

    { If @true, we will render dynamic shadows (shadow volumes) for
      all scene geometry. This allows the whole level to use dynamic
      shadows. It's normally read from data/levels/index.xml,
      attribute scene_dynamic_shadows. }
    property SceneDynamicShadows: boolean
      read FSceneDynamicShadows write FSceneDynamicShadows default false;

    { Turn off lights not supposed to light in the shadow. }
    procedure PushLightsOff;
    procedure PopLightsOff;

    { Ambient color of light used for bump mapping,
      for shadowed (@false) and lighted (@true) versions. }
    property BumpMappingLightAmbientColor[Lighted: boolean]: TVector4Single
      read GetBumpMappingLightAmbientColor
      write SetBumpMappingLightAmbientColor;

    { Diffuse color of light used for bump mapping. }
    property BumpMappingLightDiffuseColor: TVector4Single
      read FBumpMappingLightDiffuseColor write SetBumpMappingLightDiffuseColor;
  end;

  TLevelClass = class of TLevel;

{$undef read_interface}

implementation

uses SysUtils, GL, GLU, Object3dAsVRML,
  CastlePlay, KambiGLUtils, KambiFilesUtils, KambiStringUtils,
  CastleVideoOptions, CastleConfig, CastleTimeMessages,
  CastleInputs, CastleWindow, KambiOpenAL, ALUtils, KambiXMLUtils,
  CastleRequiredResources, VRMLOpenGLRenderer, VRMLShape;

{$define read_implementation}

{ TLevelStaticObject --------------------------------------------------------- }

constructor TLevelStaticObject.Create(AParentLevel: TLevel;
  const SceneFileName: string; PrepareBackground: boolean);
begin
  inherited Create(nil);
  ParentLevel := AParentLevel;
  FScene := ParentLevel.LoadLevelScene(SceneFileName, true, PrepareBackground);
end;

destructor TLevelStaticObject.Destroy;
begin
  FreeAndNil(FScene);
  inherited;
end;

function TLevelStaticObject.GetParentLevel: TLevel;
begin
  Result := FParentLevel;
end;

procedure TLevelStaticObject.SetParentLevel(const Value: TLevel);
begin
  FParentLevel := Value;
end;

function TLevelStaticObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    Scene.OctreeCollisions.MoveAllowedSimple(
      OldPos, ProposedNewPos,
      CameraRadius, nil, TrianglesToIgnoreFunc);
end;

function TLevelStaticObject.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    Scene.OctreeCollisions.MoveBoxAllowedSimple(
      OldPos, ProposedNewPos, ProposedNewBox,
      nil, TrianglesToIgnoreFunc);
end;

function TLevelStaticObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    Scene.OctreeCollisions.IsSegmentCollision(
      Pos1, Pos2,
      nil, false, TrianglesToIgnoreFunc);
end;

function TLevelStaticObject.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    Scene.OctreeCollisions.IsSphereCollision(
      Pos, Radius,  nil, TrianglesToIgnoreFunc);
end;

function TLevelStaticObject.BoxCollision(const Box: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    Scene.OctreeCollisions.IsBoxCollision(
      Box,  nil, TrianglesToIgnoreFunc);
end;

function TLevelStaticObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision;
var
  Triangle: PVRMLTriangle;
begin
  Result := nil;
  if Exists and Collides then
  begin
    Triangle := Scene.OctreeCollisions.RayCollision(
      IntersectionDistance,
      Ray0, RayVector,
      false, nil, false, TrianglesToIgnoreFunc);
    if Triangle <> nil then
    begin
      Result := T3DCollision.Create;
      Result.Triangle := Triangle;
      Result.Hierarchy.Add(Self);
    end;
  end;
end;

procedure TLevelStaticObject.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
begin
  inherited;
  if Exists and Collides then
  begin
    Scene.OctreeCollisions.GetCameraHeight(
      Position, GravityUp,
      IsAboveTheGround, SqrHeightAboveTheGround, GroundItem,
      nil, TrianglesToIgnoreFunc);
  end;
end;

procedure TLevelStaticObject.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  if Exists then
    Scene.RenderFrustum(Frustum, TransparentGroup);
end;

procedure TLevelStaticObject.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  if Exists and CastsShadow then
  begin
    Scene.RenderShadowVolume(ShadowVolumes,
      ParentTransformIsIdentity, ParentTransform);
  end;
end;

function TLevelStaticObject.BoundingBox: TBox3d;
begin
  if Exists and Collides then
    Result := Scene.BoundingBox else
    Result := EmptyBox3d;
end;

{ TLevelMovingObject --------------------------------------------------------- }

constructor TLevelMovingObject.Create(AParentLevel: TLevel);
begin
  inherited Create(nil);
  ParentLevel := AParentLevel;
  FMovePushesOthers := true;
  FMovePushesOthersUsesBoxes := true;
end;

function TLevelMovingObject.GetParentLevel: TLevel;
begin
  Result := FParentLevel;
end;

procedure TLevelMovingObject.SetParentLevel(const Value: TLevel);
begin
  FParentLevel := Value;
end;

function TLevelMovingObject.GetTranslation: TVector3Single;
begin
  Result := GetTranslationFromTime(ParentLevel.AnimationTime);
end;

procedure TLevelMovingObject.BeforeIdle(
  const NewAnimationTime: TKamTime);

  function BoundingBoxAssumeTranslation(
    const AssumeTranslation: TVector3_Single): TBox3d;
  begin
    if Exists and Collides then
      Result := Box3dTranslate(Child.BoundingBox, AssumeTranslation) else
      Result := EmptyBox3d;
  end;

  function SphereCollisionAssumeTranslation(
    const AssumeTranslation: TVector3_Single;
    const Pos: TVector3Single; const Radius: Single;
    const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
  begin
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
        use Child.SphereCollsion with Translation. }

      Result := Child.SphereCollision(
        VectorSubtract(Pos, AssumeTranslation.Data), Radius, TrianglesToIgnoreFunc);
    end;
  end;

  function BoxCollisionAssumeTranslation(
    const AssumeTranslation: TVector3_Single;
    const Box: TBox3d;
    const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
  begin
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
        use Child.BoxCollsion with Translation. }

      Result := Child.BoxCollision(
        Box3dAntiTranslate(Box, AssumeTranslation.Data), TrianglesToIgnoreFunc);
    end;
  end;

var
  CurrentBox, NewBox, Box: TBox3d;
  I: Integer;
  Move: TVector3Single;
  CurrentTranslation, NewTranslation: TVector3_Single;
  Crea: TCreature;
  Item: TItemOnLevel;
begin
  if Supports(Child, ICastle3D) then
    (Child as ICastle3D).BeforeIdle(NewAnimationTime);

  if Exists and Collides and MovePushesOthers then
  begin
    CurrentTranslation := GetTranslationFromTime(ParentLevel.AnimationTime);
    NewTranslation := GetTranslationFromTime(NewAnimationTime);

    { It's an often expected situation that TLevelMovingObject
      doesn't move at all, and then Translation doesn't change at all
      (even when compared precisely, without usual epsilon used to compare
      floats). So the check below is worth the time, because we expect
      that usually is will render the rest if the code unnecessary. }

    if not VectorsPerfectlyEqual(CurrentTranslation.Data, NewTranslation.Data) then
    begin
      CurrentBox := BoundingBox;
      NewBox := BoundingBoxAssumeTranslation(NewTranslation);
      Move := NewTranslation - CurrentTranslation;

      if MovePushesOthersUsesBoxes then
      begin
        Box := Player.BoundingBox;
        if Boxes3dCollision(NewBox, Box) or
           Boxes3dCollision(CurrentBox, Box) then
          Player.Camera.Position := Player.Camera.Position + Move;

        for I := 0 to ParentLevel.Creatures.High do
        begin
          Crea := ParentLevel.Creatures[I];
          Box := Crea.BoundingBox;
          if Boxes3dCollision(NewBox, Box) or
             Boxes3dCollision(CurrentBox, Box) then
            Crea.LegsPosition := Crea.LegsPosition + Move;
        end;

        for I := 0 to ParentLevel.Items.High do
        begin
          Item := ParentLevel.Items[I];
          Box := Item.BoundingBox;
          if Boxes3dCollision(NewBox, Box) or
             Boxes3dCollision(CurrentBox, Box) then
            Item.Position := Item.Position + Move;
        end;
      end else
      begin
        if SphereCollisionAssumeTranslation(NewTranslation,
          Player.Camera.Position, ParentLevel.CameraRadius,
          @ParentLevel.CollisionIgnoreItem) then
          Player.Camera.Position := Player.Camera.Position + Move;

        for I := 0 to ParentLevel.Creatures.High do
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

        for I := 0 to ParentLevel.Items.High do
        begin
          Item := ParentLevel.Items[I];
          if BoxCollisionAssumeTranslation(NewTranslation,
            Item.BoundingBox,
            @ParentLevel.CollisionIgnoreItem) then
            Item.Position := Item.Position + Move;
        end;
      end;
    end;
  end;
end;

{ TLevelLinearMovingObject --------------------------------------------------- }

constructor TLevelLinearMovingObject.Create(AParentLevel: TLevel);
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

procedure TLevelLinearMovingObject.SoundSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = UsedSound);
  UsedSound.OnUsingEnd := nil;
  UsedSound := nil;
end;

function TLevelLinearMovingObject.SoundPosition: TVector3Single;
begin
  Result := Box3dMiddle(BoundingBox);
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
  FEndPositionStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure TLevelLinearMovingObject.GoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(SoundGoBeginPosition, SoundGoBeginPositionLooping);
end;

procedure TLevelLinearMovingObject.RevertGoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := { ParentLevel.AnimationTime -
    (MoveTime - (ParentLevel.AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * ParentLevel.AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure TLevelLinearMovingObject.RevertGoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := { ParentLevel.AnimationTime -
    (MoveTime - (ParentLevel.AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * ParentLevel.AnimationTime - MoveTime - EndPositionStateChangeTime;
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
  const AnimationTime: TKamTime): TVector3Single;
var
  R: TVector3_Single;
begin
  if not EndPosition then
  begin
    if AnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely closed. }
      R.Init_Zero else
      { During closing. }
      R := TranslationEnd *
        (1 - (AnimationTime - EndPositionStateChangeTime) / MoveTime);
  end else
  begin
    if AnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely open. }
      R := TranslationEnd else
      { During opening. }
      R := TranslationEnd *
        ((AnimationTime - EndPositionStateChangeTime) / MoveTime);
  end;

  { TODO: This really sucks without overloaded operators for TVector3Single }
  Result := R;
end;

function TLevelLinearMovingObject.CompletelyEndPosition: boolean;
begin
  Result := EndPosition and
    (ParentLevel.AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

function TLevelLinearMovingObject.CompletelyBeginPosition: boolean;
begin
  Result := (not EndPosition) and
    (ParentLevel.AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

procedure TLevelLinearMovingObject.Idle(const CompSpeed: Single);
begin
  inherited;

  { Update sound position when object is moving }
  if (UsedSound <> nil) and SoundTracksCurrentPosition then
    alSourceVector3f(UsedSound.ALSource, AL_POSITION, SoundPosition);

  { If the SoundGoBegin/EndPosition is longer than the MoveTime
    (or it's looping),
    stop this sound once we're completely in Begin/EndPosition. }
  if (ParentLevel.AnimationTime - EndPositionStateChangeTime > MoveTime) and
    (UsedSound <> nil) then
    UsedSound.DoUsingEnd;
end;

{ TLevelAnimatedObject ------------------------------------------------------- }

constructor TLevelAnimatedObject.Create(AParentLevel: TLevel;
  AnAnimation: TVRMLGLAnimation);
begin
  inherited Create(nil);
  ParentLevel := AParentLevel;
  FAnimation := AnAnimation;
end;

destructor TLevelAnimatedObject.Destroy;
begin
  FreeAndNil(FAnimation);
  inherited;
end;

function TLevelAnimatedObject.GetParentLevel: TLevel;
begin
  Result := FParentLevel;
end;

procedure TLevelAnimatedObject.SetParentLevel(const Value: TLevel);
begin
  FParentLevel := Value;
end;

function TLevelAnimatedObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    (Animation.FirstScene.OctreeCollisions.MoveAllowedSimple(
       OldPos, ProposedNewPos,
       CameraRadius, nil, TrianglesToIgnoreFunc) and
       ( (not CollisionUseLastScene) or
         Animation.LastScene.OctreeCollisions.MoveAllowedSimple(
           OldPos, ProposedNewPos,
           CameraRadius, nil, TrianglesToIgnoreFunc) ));
end;

function TLevelAnimatedObject.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    (Animation.FirstScene.OctreeCollisions.MoveBoxAllowedSimple(
       OldPos, ProposedNewPos, ProposedNewBox,
       nil, TrianglesToIgnoreFunc) and
       ( (not CollisionUseLastScene) or
         Animation.LastScene.OctreeCollisions.MoveBoxAllowedSimple(
           OldPos, ProposedNewPos, ProposedNewBox,
           nil, TrianglesToIgnoreFunc) ));
end;

function TLevelAnimatedObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.OctreeCollisions.IsSegmentCollision(
         Pos1, Pos2,
         nil, false, TrianglesToIgnoreFunc) ) or
      (CollisionUseLastScene and
        (Animation.LastScene.OctreeCollisions.IsSegmentCollision(
           Pos1, Pos2,
           nil, false, TrianglesToIgnoreFunc)))
    );
end;

function TLevelAnimatedObject.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.OctreeCollisions.IsSphereCollision(
         Pos, Radius, nil, TrianglesToIgnoreFunc) ) or
      (CollisionUseLastScene and
        (Animation.LastScene.OctreeCollisions.IsSphereCollision(
           Pos, Radius, nil, TrianglesToIgnoreFunc)))
    );
end;

function TLevelAnimatedObject.BoxCollision(
  const Box: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.OctreeCollisions.IsBoxCollision(
         Box, nil, TrianglesToIgnoreFunc) ) or
      (CollisionUseLastScene and
        (Animation.LastScene.OctreeCollisions.IsBoxCollision(
           Box, nil, TrianglesToIgnoreFunc)))
    );
end;

function TLevelAnimatedObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision;
var
  Triangle: PVRMLTriangle;
  Octree: TVRMLBaseTrianglesOctree;
begin
  Result := nil;
  if Exists and Collides then
  begin
    Octree := Animation.FirstScene.OctreeCollisions;
    Triangle := Octree.RayCollision(IntersectionDistance,
      Ray0, RayVector, false, nil, false, TrianglesToIgnoreFunc);
    if Triangle <> nil then
    begin
      Result := T3DCollision.Create;
      Result.Triangle := Triangle;
      Result.Hierarchy.Add(Self);
    end else
    if CollisionUseLastScene then
    begin
      { try the same thing on LastScene }
      Octree := Animation.LastScene.OctreeCollisions;
      Triangle := Octree.RayCollision(IntersectionDistance,
        Ray0, RayVector, false, nil, false, TrianglesToIgnoreFunc);
      if Triangle <> nil then
      begin
        Result := T3DCollision.Create;
        Result.Triangle := Triangle;
        Result.Hierarchy.Add(Self);
      end
    end;
  end;
end;

procedure TLevelAnimatedObject.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);

  procedure MakeScene(Scene: TVRMLScene);
  var
    IsAboveThis: boolean;
    SqrHeightAboveThis: Single;
    GroundItemThis: PVRMLTriangle;
  begin
    Scene.OctreeCollisions.GetCameraHeight(
      Position, GravityUp, IsAboveThis, SqrHeightAboveThis, GroundItemThis,
      nil, TrianglesToIgnoreFunc);

    if IsAboveThis and
      ((not IsAboveTheGround) or (SqrHeightAboveThis < SqrHeightAboveTheGround)) then
    begin
      IsAboveTheGround := true;
      SqrHeightAboveTheGround := SqrHeightAboveThis;
      GroundItem := GroundItemThis;
    end;
  end;

begin
  inherited;
  if Exists and Collides then
  begin
    MakeScene(Animation.FirstScene);
    if CollisionUseLastScene then
      MakeScene(Animation.LastScene);
  end;
end;

procedure TLevelAnimatedObject.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  if Exists then
    Animation.SceneFromTime(AnimationTime).RenderFrustum(Frustum, TransparentGroup);
end;

procedure TLevelAnimatedObject.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  if Exists and CastsShadow then
  begin
    Animation.SceneFromTime(AnimationTime).
      RenderShadowVolume(ShadowVolumes,
        ParentTransformIsIdentity, ParentTransform);
  end;
end;

function TLevelAnimatedObject.BoundingBox: TBox3d;
begin
  if Exists and Collides then
  begin
    Result := Animation.FirstScene.BoundingBox;
    if CollisionUseLastScene then
      Box3dSumTo1st(Result, Animation.LastScene.BoundingBox);
  end else
    Result := EmptyBox3d;
end;

{ TLevelSimpleAnimatedObject ------------------------------------------------- }

constructor TLevelSimpleAnimatedObject.Create(
  AParentLevel: TLevel; AnAnimation: TVRMLGLAnimation);
begin
  inherited;
  FStarted := false;
end;

procedure TLevelSimpleAnimatedObject.Play;
begin
  FStarted := true;
  FPlayStartTime := ParentLevel.AnimationTime;
end;

procedure TLevelSimpleAnimatedObject.Stop;
begin
  FStarted := false;
end;

procedure TLevelSimpleAnimatedObject.Idle(const CompSpeed: Single);
begin
  inherited;
  if Started then
    AnimationTime := ParentLevel.AnimationTime - FPlayStartTime else
    AnimationTime := 0;
end;

{ TLevelArea ----------------------------------------------------------------- }

constructor TLevelArea.Create(AParentLevel: TLevel);
begin
  inherited Create(nil);
  FParentLevel := AParentLevel;
  FBox := EmptyBox3d;
end;

procedure TLevelArea.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  { This object is invisible and non-colliding. }
end;

procedure TLevelArea.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  { This object is invisible and non-colliding. }
end;

function TLevelArea.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := true;
end;

function TLevelArea.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := true;
end;

function TLevelArea.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.BoxCollision(const ABox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision;
begin
  { This object is invisible and non-colliding. }
  Result := nil;
end;

procedure TLevelArea.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
begin
  inherited;
  { This object is invisible and non-colliding. }
end;

function TLevelArea.BoundingBox: TBox3d;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3d;
end;

procedure TLevelArea.ChangeLevelScene(AParentLevel: TLevel);
begin
  inherited;
  AParentLevel.RemoveBoxNodeCheck(FBox, VRMLName);
end;

function TLevelArea.PointInside(const Point: TVector3Single): boolean;
begin
  Result := Box3dPointInside(Point, Box);
end;

function TLevelArea.GetParentLevel: TLevel;
begin
  Result := FParentLevel;
end;

procedure TLevelArea.SetParentLevel(const Value: TLevel);
begin
  FParentLevel := Value;
end;

{ TLevelHintArea ----------------------------------------------------------- }

procedure TLevelHintArea.Idle(const CompSpeed: Single);
var
  ReplaceInteractInput: TPercentReplace;
begin
  inherited;
  if (not MessageDone) and PointInside(Player.Camera.Position) then
  begin
    ReplaceInteractInput.C := 'i';
    ReplaceInteractInput.S := InteractInputDescription;
    TimeMessage(SPercentReplace(Message, [ReplaceInteractInput], true));
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
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
    Scene.ChangedAll;
  end;

const
  SectorsMargin = 0.5;
var
  NavigationNode: TNodeNavigationInfo;
  NavigationSpeed: Single;
  Options: TPrepareRenderOptions;
  TG: TTransparentGroups;
begin
  inherited Create;

  FName := AName;
  FSceneFileName := ASceneFileName;
  FLightSetFileName := ALightSetFileName;
  FTitle := ATitle;
  FTitleHint := ATitleHint;
  FNumber := ANumber;
  FMenuBackground := AMenuBackground;
  FRequiredCreatures := ARequiredCreatures;

  RequireCreatures(FRequiredCreatures);

  Progress.Init(1, 'Loading level "' + Title + '"');
  try
    FScene := TVRMLGLScene.CreateCustomCache(nil, GLContextCache);
    FScene.Load(SceneFileName);

    { initialize FAnimationTime. Must be initialized before creating creatures. }
    FAnimationTime := 0.0;

    AttributesSet(Scene.Attributes, btIncrease);
    if BumpMapping then
      Scene.Attributes.BumpMappingMaximum := High(TBumpMappingMethod) else
      Scene.Attributes.BumpMappingMaximum := bmNone;
    Scene.Attributes.UseOcclusionQuery := UseOcclusionQuery;

    { Calculate InitialPosition, InitialDirection, InitialUp.
      Must be done before initializing creatures, as they right now
      use InitialPosition. FInitialDirection, FInitialUp will be
      actually changed later in this procedure. }
    Scene.GetPerspectiveViewpoint(FInitialPosition,
      FInitialDirection, FInitialUp, FGravityUp);

    FObjects := TBase3DList.Create(nil);

    LoadFromDOMElement(DOMElement);

    ChangeLevelScene;

    ItemsToRemove := TVRMLNodesList.Create;
    try
      { Initialize Items }
      FItems := TItemsOnLevelList.Create;
      Scene.RootNode.TraverseBlenderObjects(@TraverseForItems);

      { Initialize Creatures }
      FCreatures := TCreaturesList.Create;
      Scene.RootNode.TraverseBlenderObjects(@TraverseForCreatures);

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

    NavigationNode := Scene.NavigationInfoStack.Top as TNodeNavigationInfo;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      FCameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      FCameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      FCameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      FCameraPreferredHeight := FCameraRadius * 5;
    CorrectCameraPreferredHeight(FCameraPreferredHeight, CameraRadius,
      DefaultCrouchHeight, DefaultHeadBobbing);

    if NavigationNode <> nil then
      NavigationSpeed := NavigationNode.FdSpeed.Value else
      NavigationSpeed := 1.0;

    FProjectionNear := CameraRadius * 0.75;
    FProjectionFar := Box3dMaxSize(Scene.BoundingBox) * 5;

    { Fix InitialDirection length, and set MoveXxxSpeed.

      We want to have horizontal and vertical speeds controlled independently,
      so we just normalize InitialDirection and set speeds in appropriate
      MoveXxxSpeed. }
    NormalizeTo1st(FInitialDirection);
    FMoveHorizontalSpeed := NavigationSpeed / 50;
    FMoveVerticalSpeed := 0.4;

    { Check and fix GravityUp. }
    if not VectorsEqual(Normalized(GravityUp),
             Vector3Single(0, 0, 1), 0.001) then
      raise EInternalError.CreateFmt(
        'Gravity up vector must be +Z, but is %s',
        [ VectorToRawStr(Normalized(GravityUp)) ]) else
      { Make GravityUp = (0, 0, 1) more "precisely" }
      FGravityUp := Vector3Single(0, 0, 1);

    Scene.BackgroundSkySphereRadius := TBackgroundGL.NearFarToSkySphereRadius
      (ProjectionNear, ProjectionFar);

    { calcualte Options for PrepareRender }
    Options := [prBackground, prBoundingBox];
    if RenderShadowsPossible and SceneDynamicShadows then
      Options := Options + prShadowVolume;

    TG := [tgAll];
    if RenderShadowsPossible then
      TG := TG + [tgOpaque, tgTransparent];

    Scene.PrepareRender(TG, Options, false);

    Scene.FreeResources([frTextureDataInNodes]);

    FLightSet := TVRMLLightSetGL.Create(LoadAsVRML(LightSetFileName),
      true,
      { GL_LIGHT0 is reserved for headlight. }
      { GL_LIGHT1 is reserved for thunder effect in cages level.
        So first light is GL_LIGHT2. }
      2, -1);

    FGlobalAmbientLight := DefaultGlobalAmbientLight;

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { Loading octree have their own Progress, so we load them outside our
    progress. }

  if not MenuBackground then
  begin
    Scene.TriangleOctreeProgressTitle := 'Loading level (triangle octree)';
    Scene.ShapeOctreeProgressTitle := 'Loading level (Shape octree)';
    Scene.Spatial := [ssRendering, ssCollisionOctree];

    { TrianglesList was created for triangle octree. We don't need it anymore.

      Hm, for now we actually don't create ssCollidableTriangles,
      so TrianglesList is not actually used.
    Scene.FreeResources([frTrianglesListNotOverTriangulate]);
    }
  end;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunderEffect);
  FreeWithContentsAndNil(FSectors);
  FreeWithContentsAndNil(FWaypoints);
  FreeAndNil(FLightSet);
  FreeAndNil(FScene);
  FreeWithContentsAndNil(FItems);
  FreeWithContentsAndNil(FCreatures);
  FreeAndNil(FObjects);
  if FRequiredCreatures <> nil then
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

  function LevelObjectFromDOMElement(Element: TDOMElement): TBase3D;
  begin
    if Element.TagName = 'area' then
      Result := LevelAreaFromDOMElement(Element) else
    if (Element.TagName = 'required_resources') or
       (Element.TagName = 'bump_mapping_light') then
    begin
      { These are handled elsewhere, and don't produce any TBase3D. }
      Result := nil;
    end else
      raise Exception.CreateFmt('Not allowed children element of <level>: "%s"',
        [Element.TagName]);
  end;

  procedure ReadBumpMappingLightProperties(BMElement: TDOMElement);

    function Vector3SingleFromElementData(const ChildName: string;
      const DefaultValue: TVector3Single): TVector3Single;
    var
      E: TDOMElement;
    begin
      E := DOMGetChildElement(BMElement, ChildName, false);
      if E <> nil then
        Result := Vector3SingleFromStr(DOMGetTextData(E)) else
        Result := DefaultValue;
    end;

    function Vector4SingleFromElementData(const ChildName: string;
      const DefaultValue: TVector4Single): TVector4Single;
    var
      E: TDOMElement;
    begin
      E := DOMGetChildElement(BMElement, ChildName, false);
      if E <> nil then
        Result := Vector4Single(Vector3SingleFromStr(DOMGetTextData(E)), 1) else
        Result := DefaultValue;
    end;

  const
    DefaultBumpMappingLightPosition: TVector3Single = (0, 0, 0);
  begin
    { Later: it can be updated during runtime by TLevel subclasses
      if BumpMappingMethod in bmGLSLAll. }
    Scene.BumpMappingLightPosition := Vector3SingleFromElementData(
      'position', DefaultBumpMappingLightPosition);
    BumpMappingLightAmbientColor[false] := Vector4SingleFromElementData(
      'ambient_color_shadows', DefaultBumpMappingLightAmbientColor);
    BumpMappingLightAmbientColor[true] := Vector4SingleFromElementData(
      'ambient_color_lighted', DefaultBumpMappingLightAmbientColor);
    BumpMappingLightDiffuseColor := Vector4SingleFromElementData(
      'diffuse_color', DefaultBumpMappingLightDiffuseColor);
  end;

var
  ObjectsList: TDOMNodeList;
  ObjectNode: TDOMNode;
  SoundName: string;
  I: Integer;
  NewObject: TBase3D;
  BMElement: TDOMElement;
begin
  { Load Objects }
  ObjectsList := Element.ChildNodes;
  try
    for I := 0 to Integer(ObjectsList.Count) - 1 do
    begin
      ObjectNode := ObjectsList.Item[I];
      if ObjectNode.NodeType = ELEMENT_NODE then
      begin
        NewObject := LevelObjectFromDOMElement(ObjectNode as TDOMElement);
        if NewObject <> nil then
          Objects.Add(NewObject);
      end;
    end;
  finally FreeChildNodes(ObjectsList) end;

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

  BMElement := DOMGetChildElement(Element, 'bump_mapping_light', false);
  if BMElement <> nil then
    ReadBumpMappingLightProperties(BMElement);
end;

function TLevel.RemoveBoxNode(out Box: TBox3d; const NodeName: string): boolean;
var
  BoxShape: TVRMLShape;
begin
  BoxShape := Scene.Shapes.FindBlenderMesh(NodeName);
  Result := BoxShape <> nil;
  if Result then
  begin
    { When node with name NodeName is found, then we calculate our
      Box from this node (and we delete this node from the scene,
      as it should not be visible).
      This way we can comfortably set such boxes from Blender. }
    Box := BoxShape.BoundingBox;
    BoxShape.Geometry.FreeRemovingFromAllParents;
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
var
  I: Integer;
begin
  for I := 0 to Objects.List.Count - 1 do
  begin
    if Objects.List[I] is TLevelArea then
      TLevelArea(Objects.List[I]).ChangeLevelScene(Self);
  end;
end;

procedure TLevel.TraverseForItems(
  BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
  Geometry: TVRMLGeometryNode;
  StateStack: TVRMLGraphTraverseStateStack);

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

    ItemStubBoundingBox := Geometry.BoundingBox(StateStack.Top);
    ItemPosition[0] := (ItemStubBoundingBox[0, 0] + ItemStubBoundingBox[1, 0]) / 2;
    ItemPosition[1] := (ItemStubBoundingBox[0, 1] + ItemStubBoundingBox[1, 1]) / 2;
    ItemPosition[2] := ItemStubBoundingBox[0, 2];

    FItems.Add(TItemOnLevel.Create(TItem.Create(ItemKind, ItemQuantity),
      ItemPosition));
  end;

const
  ItemPrefix = 'Item';
begin
  if IsPrefix(ItemPrefix, BlenderMeshName) then
  begin
    CreateNewItem(SEnding(BlenderMeshName, Length(ItemPrefix) + 1));
    { Don't remove BlenderObjectNode now --- will be removed later.
      This avoids problems with removing nodes while traversing. }
    ItemsToRemove.Add(BlenderObjectNode);
  end;
end;

procedure TLevel.TraverseForCreatures(
  BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
  Geometry: TVRMLGeometryNode;
  StateStack: TVRMLGraphTraverseStateStack);

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
    StubBoundingBox := Geometry.BoundingBox(StateStack.Top);
    CreaturePosition[0] := (StubBoundingBox[0, 0] + StubBoundingBox[1, 0]) / 2;
    CreaturePosition[1] := (StubBoundingBox[0, 1] + StubBoundingBox[1, 1]) / 2;
    CreaturePosition[2] := StubBoundingBox[0, 2];

    { calculate CreatureKind }
    CreatureKind := CreaturesKinds.FindByVRMLNodeName(CreatureKindName);
    { The creature kind may be unprepared here only because
      --debug-no-creatures was specified. In this case, leave this
      creature kind unprepared and don't add this creature. }
    if not CreatureKind.PrepareRenderDone then
    begin
      Assert(ResourcesStrategy = rsDebugKeepOnlyForExistingItems);
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
      CreatureDirection, AnimationTime, MaxLife);

    FCreatures.Add(Creature);
  end;

const
  CreaturePrefix = 'Crea';
begin
  if IsPrefix(CreaturePrefix, BlenderMeshName) then
  begin
    CreateNewCreature(SEnding(BlenderMeshName, Length(CreaturePrefix) + 1));
    { Don't remove BlenderObjectNode now --- will be removed later.
      This avoids problems with removing nodes while traversing. }
    ItemsToRemove.Add(BlenderObjectNode);
  end;
end;

function TLevel.LineOfSight(
  const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := not Scene.OctreeCollisions.IsSegmentCollision(
    Pos1, Pos2, nil, false,
    @Scene.OctreeCollisions.IgnoreTransparentItem);

  if not Result then
    Exit;

  if Objects.SegmentCollision(Pos1, Pos2,
      @Scene.OctreeCollisions.IgnoreTransparentItem) then
    Result := false;
end;

function TLevel.ObjectsMoveAllowedSimple(
  const Position: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := Objects.MoveAllowedSimple(Position, NewPos,
    MovingObjectCameraRadius, @CollisionIgnoreItem);
end;

function TLevel.ObjectsMoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Objects.MoveBoxAllowedSimple(OldPos, ProposedNewPos, ProposedNewBox,
    @CollisionIgnoreItem);
end;

function TLevel.MoveAllowed(const Position: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Scene.OctreeCollisions.MoveAllowed(
      Position, ProposedNewPos, NewPos, MovingObjectCameraRadius,
      nil, @CollisionIgnoreItem) and
    Box3dPointInside(NewPos, LevelBox) and
    ObjectsMoveAllowedSimple(
      Position, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TLevel.MoveAllowedSimple(const Position: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Box3dPointInside(NewPos, LevelBox) and
    Scene.OctreeCollisions.MoveAllowedSimple(
      Position, NewPos, MovingObjectCameraRadius,
      nil, @CollisionIgnoreItem) and
    ObjectsMoveAllowedSimple(
      Position, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TLevel.MoveBoxAllowedSimple(const Position: TVector3Single;
  const NewPos: TVector3Single;
  const NewBox: TBox3d;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    Box3dPointInside(NewPos, LevelBox) and
    Scene.OctreeCollisions.MoveBoxAllowedSimple(
      Position, NewPos, NewBox,
      nil, @CollisionIgnoreItem) and
    ObjectsMoveBoxAllowedSimple(
      Position, NewPos, NewBox, BecauseOfGravity);
end;

procedure TLevel.GetCameraHeight(const Position: TVector3Single;
  out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
var
  ThisIsAbove: boolean;
  ThisSqrHeightAbove, ThisHeightAbove: Single;
  ThisItem: PVRMLTriangle;
begin
  Scene.OctreeCollisions.GetCameraHeightZ(
    Position,
    IsAboveTheGround, HeightAboveTheGround, GroundItem,
    nil, @CollisionIgnoreItem);

  Objects.GetCameraHeight(Position, GravityUp, @CollisionIgnoreItem,
    ThisIsAbove, ThisSqrHeightAbove, ThisItem);
  if ThisIsAbove then
    ThisHeightAbove := Sqrt(ThisSqrHeightAbove);

  if ThisIsAbove and
    ((not IsAboveTheGround) or (ThisHeightAbove < HeightAboveTheGround)) then
  begin
    IsAboveTheGround := true;
    HeightAboveTheGround := ThisHeightAbove;
    GroundItem := ThisItem;
  end;
end;

function TLevel.PlayerMoveAllowed(Camera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    { Check collision Player <-> level. }
    MoveAllowed(Camera.Position, ProposedNewPos, NewPos,
      BecauseOfGravity, CameraRadius) and

    { Check collision Player <-> Creatures here. }
    (Creatures.MoveAllowedSimple(
      Player.BoundingBox(false),
      Player.BoundingBoxAssuming(NewPos, false),
      Camera.Position, NewPos, nil) = nil);
end;

procedure TLevel.PlayerGetCameraHeightSqr(Camera: TWalkCamera;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  HeightAboveTheGround: Single;
  GroundItem: PVRMLTriangle;
begin
  { Check is player standing over level. }
  GetCameraHeight(Camera.Position, IsAboveTheGround,
    HeightAboveTheGround, GroundItem);

  { Check is player standing over one of the creatures. }
  Creatures.GetCameraHeight(Camera.Position, IsAboveTheGround,
    HeightAboveTheGround, GroundItem, nil);

  if IsAboveTheGround then
  begin
    { Below is a waste of time. Because Camera requires a callback
      that passes only SqrHeightAboveTheGround, we Sqr here the
      HeightAboveTheGround... Inside Camera will possibly call Sqrt
      on this, thus wasting time on useless Sqrt operation.
      For now this is not a problem, in the future Camera should
      be extended to have callback that returns ready HeightAboveTheGround. }
    SqrHeightAboveTheGround := Sqr(HeightAboveTheGround);

    Player.Ground := GroundItem;
  end else
    Player.Ground := nil;
end;

procedure TLevel.Render(const Frustum: TFrustum; TransparentGroup: TTransparentGroup);
const
  InShadow = false; { TODO: doesn't matter for castle objects }
begin
  case TransparentGroup of
    tgAll:
      begin
        { First pass rendering Objects: render non-transparent parts }
        Objects.Render(Frustum, tgOpaque, InShadow);

        Scene.RenderFrustum(Frustum, tgAll);

        { Second pass rendering Objects: render transparent parts }
        Objects.Render(Frustum, tgTransparent, InShadow);
      end;
    tgOpaque:
      begin
        { First pass rendering Objects: render non-transparent parts }
        Objects.Render(Frustum, tgOpaque, InShadow);

        Scene.RenderFrustum(Frustum, tgOpaque);
      end;
    tgTransparent:
      begin
        Scene.RenderFrustum(Frustum, tgTransparent);

        { Second pass rendering Objects: render transparent parts }
        Objects.Render(Frustum, tgTransparent, InShadow);
      end;
    else raise EInternalError.Create('castleplay 324hsdf32');
  end;
end;

procedure TLevel.RenderShadowVolume(
  ShadowVolumes: TShadowVolumes);
begin
  Objects.RenderShadowVolume(ShadowVolumes, true, IdentityMatrix4Single);

  if SceneDynamicShadows then
  begin
    { Useless to optimize this by shadow culling in ShadowVolumes,
      Scene will be visible practically always. }
    ShadowVolumes.InitSceneAlwaysVisible;
    Scene.RenderShadowVolumeCore(ShadowVolumes, true, IdentityMatrix4Single);
  end;
end;

procedure TLevel.Idle(const CompSpeed: Single);
var
  I: Integer;
  NewAnimationTime: TKamTime;
begin
  NewAnimationTime := AnimationTime + CompSpeed;

  { TODO: lame, if ICastle3D will be within another non-ICastle3D,
    it's BeforeIdle will not be called. }
  for I := 0 to Objects.List.Count - 1 do
    if Supports(Objects.List[I], ICastle3D) then
      (Objects.List[I] as ICastle3D).BeforeIdle(NewAnimationTime);

  FAnimationTime := NewAnimationTime;

  Objects.Idle(CompSpeed);

  if ThunderEffect <> nil then
    ThunderEffect.Idle;
end;

function TLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;
begin
  Result := -1;
end;

procedure TLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer; var InteractionOccured: boolean);
begin
  { Nothing to do in this class. }
end;

{ $define DEBUG_PICK}

function TLevel.TryPick(out IntersectionDistance: Single;
  out LevelObjectIndex: Integer;
  const Ray0, RayVector: TVector3Single): T3DCollision;
var
  Triangle: PVRMLTriangle;
  ThisIntersectionDistance: Single;
  ThisCollision: T3DCollision;
  {$ifdef DEBUG_PICK}
  S: string;
  I: Integer;
  {$endif DEBUG_PICK}
begin
  { First, assume no collision }
  Result := nil;

  { Collision with Level.Scene }
  Triangle := Scene.OctreeCollisions.RayCollision(
    ThisIntersectionDistance, Ray0, RayVector, true, nil, false, nil);
  if Triangle <> nil then
  begin
    IntersectionDistance := ThisIntersectionDistance;
    LevelObjectIndex := -1;
    Result := T3DCollision.Create;
    Result.Triangle := Triangle;
  end;

  { Collisions with Objects[] }
  ThisCollision := Objects.RayCollision(
    ThisIntersectionDistance, Ray0, RayVector, nil);
  if ThisCollision <> nil then
  begin
    if (Result = nil) or (ThisIntersectionDistance < IntersectionDistance) then
    begin
      IntersectionDistance := ThisIntersectionDistance;

      { TODO: remove Objects from Hierarchy }
      ThisCollision.Hierarchy.Delete(0);

      { TODO: LevelObjectIndex really needed for anything? }
      LevelObjectIndex := Objects.List.IndexOf(ThisCollision.Hierarchy[0]);

      FreeAndNil(Result);
      Result := ThisCollision;
    end else
      FreeAndNil(ThisCollision);
  end;

  {$ifdef DEBUG_PICK}
  if Result <> nil then
  begin
    S := 'TLevel.TryPick: [';
    for I := 0 to Result.Hierarchy.High do
      S += Result.Hierarchy[I].ClassName + ' ';
    S += Format('], index %d, distance %f',
      [LevelObjectIndex, IntersectionDistance]);
    TimeMessage(S);
  end;
  {$endif DEBUG_PICK}
end;

procedure TLevel.Picked(const Distance: Single;
  CollisionInfo: T3DCollision; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
begin
  { Nothing to do in this class. }
end;

function TLevel.CollisionIgnoreItem(
  const Octree: TVRMLBaseTrianglesOctree;
  const Triangle: PVRMLTriangle): boolean;
begin
  { Don't ignore anything in this class. }
  Result := false;
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean): TVRMLGLScene;
var
  Options: TPrepareRenderOptions;
begin
  Result := TVRMLGLScene.CreateCustomCache(nil, GLContextCache);
  Result.Load(FileName);
  AttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareRender }
  Options := [prBoundingBox { BoundingBox is practically always needed }];
  if PrepareBackground then
    Include(Options, prBackground);
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareRender([tgOpaque, tgTransparent], Options, false);

  if CreateOctreeCollisions then
    Result.Spatial := [ssCollisionOctree];

  Result.FreeResources([frTextureDataInNodes]);
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean): TVRMLGLAnimation;
var
  Options: TPrepareRenderOptions;
begin
  Result := TVRMLGLAnimation.CreateCustomCache(nil, GLContextCache);
  Result.LoadFromFile(FileName);

  AnimationAttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareRender }
  Options := [prBoundingBox { BoundingBox is practically always needed }];
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareRender([tgOpaque, tgTransparent], Options, false);

  if CreateFirstOctreeCollisions then
    Result.FirstScene.Spatial := [ssCollisionOctree];

  if CreateLastOctreeCollisions then
    Result.LastScene.Spatial := [ssCollisionOctree];

  Result.FreeResources([frTextureDataInNodes]);
end;

function TLevel.Background: TBackgroundGL;
begin
  Result := Scene.Background;
end;

procedure TLevel.TimeMessageInteractFailed(const S: string);
begin
  TimeMessage(S);
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

procedure TLevel.PushLightsOff;
begin
  glPushAttrib(GL_LIGHTING_BIT);

  { Headlight will stay on here, but lights in Level.LightSet are off. }
  LightSet.TurnLightsOffForShadows;

  Scene.BumpMappingLightAmbientColor := FBumpMappingLightAmbientColor[false];
  Scene.BumpMappingLightDiffuseColor := Black4Single;
end;

procedure TLevel.PopLightsOff;
begin
  glPopAttrib();

  Scene.BumpMappingLightAmbientColor := FBumpMappingLightAmbientColor[true];
  Scene.BumpMappingLightDiffuseColor := FBumpMappingLightDiffuseColor;
end;

function TLevel.GetBumpMappingLightAmbientColor(const Lighted: boolean):
  TVector4Single;
begin
  Result := FBumpMappingLightAmbientColor[Lighted];
end;

procedure TLevel.SetBumpMappingLightAmbientColor(const Lighted: boolean;
  const Value: TVector4Single);
begin
  FBumpMappingLightAmbientColor[Lighted] := Value;

  { Scene.BumpMappingLightAmbientColor should normally correspond to
    BumpMappingLightAmbientColor[true], so update it if it changed. }
  if Lighted then
    Scene.BumpMappingLightAmbientColor := Value;
end;

procedure TLevel.SetBumpMappingLightDiffuseColor(const Value: TVector4Single);
begin
  FBumpMappingLightDiffuseColor := Value;
  Scene.BumpMappingLightDiffuseColor := Value;
end;

end.
