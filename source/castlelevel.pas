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

{ TLevel class and some specialized descendants. }

unit CastleLevel;

interface

uses VectorMath, VRMLScene, VRMLGLScene, VRMLLightSetGL, Boxes3d,
  VRMLNodes, VRMLFields, CastleItems, Navigation,
  VRMLTriangleOctree, CastleCreatures, VRMLSceneWaypoints, CastleSound,
  KambiUtils, KambiClassUtils, CastlePlayer, CastleThunder,
  ProgressUnit, VRMLGLAnimation, ALSourceAllocator, Matrix,
  BackgroundGL, VRMLGLHeadlight, DOM, GameSoundEngine,
  ShadowVolumesHelper, Classes, KambiTimeUtils;

{$define read_interface}

const
  DefaultGlobalAmbientLight: TVector4Single = (0.2, 0.2, 0.2, 1.0);

type
  TLevel = class;
  TLevelObjectsList = class;

  { This represents a collision with level. }
  TCollisionInfo = class
  public
    constructor Create;
    destructor Destroy; override;
    { Hierarchy is a list of TLevelObjects that are hit, from the container
      to the most detailed one (e.g., if you place an object within
      TLevelMovingObject.MovingOject, and you hit it --- then Hierarchy
      will contain first TLevelMovingObject, then TLevelMovingObject.MovingObject).
      Hierarchy is empty if the level Scene itself was hit. }
    Hierarchy: TLevelObjectsList;
    { OctreeItem is the item that was hit, from appropriate object triangle octree. }
    OctreeItem: POctreeItem;
  end;

  { This is a base class for various objects that can be added to the level.

    What's an "object on the level" ? Well, theoretically anything.
    In the future, all items and creatures and even the player
    could be also treated as "some objects on the level".
    For now, "object on the level" means a static VRML scene (TVRMLGLScene)
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
    handles @link(TLevel.Objects) to know approximately what you may want to
    override in your TLevel descendats).
    Implementing and using universal (reusable) TLevelObject descendants
    is a better idea. }
  TLevelObject = class
  private
    FParentLevel: TLevel;
    FCastsShadow: boolean;
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
      second pass. For the reason see TVRMLGLScene.Render comments. }
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); virtual; abstract;

    { Render shadow quads for all the things rendered by @link(Render).
      Does nothing if not CastsShadow.
      It does shadow volumes culling inside  (so ShadowVolumesHelper should
      have FrustumCullingInit already initialized).

      ParentTransform and ParentTransformIsIdentity describe the transformation
      of this object set by parent level object.
      Level objects may be organized in hierarchy when
      parent transforms it's children. When ParentTransformIsIdentity,
      ParentTransform must be IdentityMatrix4Single (it's not guaranteed
      that when ParentTransformIsIdentity = @true, Transform value will be
      ignored !).

      @italic(Implementation note:) In @link(Render), it was possible
      to implement this by glPush/PopMatrix and FrustumMove tricks.
      But RenderShadowVolume needs actual transformation explicitly:
      ShadowMaybeVisible needs actual box position in world coordinates,
      so bounding box has to be transformed by ParentTransform.
      And TVRMLGLScene.RenderShadowVolume needs explicit ParentTransform
      to correctly detect front/back sides (for silhouette edges and
      volume capping). }
    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); virtual; abstract;

    property CastsShadow: boolean read FCastsShadow write FCastsShadow
      default true;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    function BoxCollision(const Box: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; virtual; abstract;

    { @returns(A collision as TCollisionInfo instance, or @nil if no collision).
      You're responsible for freeing this TCollisionInfo instance. }
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; virtual; abstract;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); virtual; abstract;

    { This is called at the beginning of TLevel.Idle, @italic(before)
      ParentLevel.AnimationTime changes to NewAnimationTime.

      This is usefull for taking care of collision detection issues,
      as our assumption always is that "nothing collides". Which means
      that if you don't want your TLevelMovingObject to collide
      with e.g. player or creatures or items, then you should
      prevent the collision @italic(before it happens).
      This is the place to do it. }
    procedure BeforeIdle(const NewAnimationTime: TKamTime); virtual;

    { This is called somewhere from TLevel.Idle, @italic(after)
      ParentLevel.AnimationTime was updated. }
    procedure Idle; virtual;

    { Bounding box for collisions.

      Note that for non-colliding objects, this can
      be empty, even though something is visible on the level. In other words,
      this is not suitable for render culling --- this is only for detecting
      collisions. }
    function BoundingBox: TBox3d; virtual; abstract;

    { Called from TLevel constructor. This is the place when you
      can modify ParentLevel.Scene.RootNode, e.g. by calling
      RemoveBoxNode. }
    procedure ChangeLevelScene; virtual;
  end;

  TObjectsListItem_2 = TLevelObject;
  {$I objectslist_2.inc}
  TLevelObjectsList = class(TObjectsList_2)
  public
    { Checks RayCollision on all items, returning collision with smallest
      IntersectionDistance. Returns @nil if no collision. }
    function RayCollision(
      out IntersectionDistance: Single; out Index: Integer;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
  end;

  { This is a TLevelObject descendant that is a @italic(sum of any number
    of other TLevelObject descendants). In other words, this
    is a grouping object: it groups other level objects inside as a single level
    object.

    This is usefull for some purposes, e.g. if you want to sometimes
    treat some objects as separate and sometimes you want to treat them as
    one single level object. E.g. you can use this as MovingObject inside
    TLevelMovingObject, and you can put here a combination of static level
    object and animated level object --- like an elevetor (static level
    object) and a lever inside the elevator (animated level object),
    both moved together by moving level object. }
  TLevelObjectSum = class(TLevelObject)
  private
    FList: TLevelObjectsList;
  public
    constructor Create(AParentLevel: TLevel);
    destructor Destroy; override;

    { List of contained level objects. Objects placed here are owned
      by this object, i.e. they are freed in our destructor. }
    property List: TLevelObjectsList read FList;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); override;

    procedure BeforeIdle(const NewAnimationTime: TKamTime); override;

    procedure Idle; override;

    function BoundingBox: TBox3d; override;
  end;

  { This a VRML scene that can appear/disappear from the level.

    It's basically just a TVRMLGLScene instance
    (loaded with LoadLevelScene, always with a DefaultTriangleOctree).
    Plus properties @link(Exists) and @link(Collides) which allow you
    to hide this object from evrything (or only from the collision detection). }
  TLevelStaticObject = class(TLevelObject)
  private
    FExists: boolean;
    FCollides: boolean;
    FScene: TVRMLGLScene;
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string; PrepareBackground: boolean);
    destructor Destroy; override;

    property Scene: TVRMLGLScene read FScene;

    { @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection). }
    property Collides: boolean read FCollides write FCollides default true;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); override;

    function BoundingBox: TBox3d; override;
  end;

  { This an object that moves on the level.

    It is used to move other TLevelObject --- which can be e.g. a static object
    (basically just a TVRMLGLScene instance), an animated object,
    a TLevelObjectSum (allowing you to move a couple of level objects together),
    or even another TLevelMovingObject.

    It has a Translation function (that must be actually defined
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
    the MovingObject translated by Translation.

    Note that if Translation actually changes over time then
    you have to think of at least one additional thing yourself:
    what happens when collision occurs because of the move of this object ?
    Should the moving object push other objects (player, creatures, items),
    like the elevator going up ?
    That's the default behavior, but you can turn it off by setting
    MovePushesOthers to @false.
    See e.g. the DoomLevelDoor.BeforeIdle method to see how the behavior
    "something blocks the move" can be handled. }
  TLevelMovingObject = class(TLevelObject)
  private
    FMovingObject: TLevelObject;
    FExists: boolean;
    FCollides: boolean;
    FMovePushesOthers: boolean;
    FMovePushesOthersUsesBoxes: boolean;
  public
    { Constructor. }
    constructor Create(AParentLevel: TLevel);
    destructor Destroy; override;

    { Actual moving object. It is owned
      by this object, i.e. it is freed in our destructor. }
    property MovingObject: TLevelObject read FMovingObject write FMovingObject;

    { @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection). }
    property Collides: boolean read FCollides write FCollides default true;

    function Translation(const AnimationTime: TKamTime):
      TVector3_Single; virtual; abstract;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); override;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    procedure BeforeIdle(const NewAnimationTime: TKamTime); override;
    procedure Idle; override;

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

    function BoundingBox: TBox3d; override;
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
    constructor Create(AParentLevel: TLevel);
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

    MoveTime: Single;
    TranslationEnd: TVector3_Single;

    function Translation(const AnimationTime: TKamTime):
      TVector3_Single; override;

    procedure Idle; override;
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
    You don't have to make the octree only if you're sure that object will
    always be not @link(Collides) or not @link(Exists).

    You can also set CollisionUseLastScene, then collision will be done
    versus the sum of Animation.FirstScene.DefaultTriangleOctree +
    Animation.LastScene.DefaultTriangleOctree. CollisionUseLastScene
    is useful is the object is moving, but the move is very slight,
    so that the sum of first and last scenes geometry is good enough
    approximation of the whole geometry at any point of the animation.

    Although it seems like a totally stupid way to check for collisions,
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
    FAnimationTime: TKamTime;
    FExists: boolean;
    FCollides: boolean;
    FCollisionUseLastScene: boolean;
  public
    constructor Create(AParentLevel: TLevel; AnAnimation: TVRMLGLAnimation);
    destructor Destroy; override;

    property Animation: TVRMLGLAnimation read FAnimation;
    property AnimationTime: TKamTime read FAnimationTime write FAnimationTime;
    property Exists: boolean read FExists write FExists default true;
    property Collides: boolean read FCollides write FCollides default true;

    property CollisionUseLastScene: boolean
      read FCollisionUseLastScene
      write FCollisionUseLastScene default false;

    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function BoxCollision(const Box: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); override;

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

    procedure Idle; override;
  end;

  { This is an abstract class for a special group of objects:
    they define an invisible and non-colliding areas on the level
    that...well, have @italic(some purpose). What exactly this
    "purpose" is, is defined in each TLevelArea descendant.

    This class defines only a properties to define the area.
    For now, each area is just one TBox3d. }
  TLevelArea = class(TLevelObject)
  private
    FVRMLName: string;
    FBox: TBox3d;
    { Area. Default value is EmptyBox3d. }
    property Box: TBox3d read FBox write FBox;
  public
    constructor Create(AParentLevel: TLevel);

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
      TransparentGroup: TTransparentGroup); override;
    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
    function BoxCollision(const ABox: TBox3d;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean; override;
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo; override;
    procedure GetCameraHeight(const CameraPos: TVector3Single;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
      var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
      var GroundItem: POctreeItem); override;
    function BoundingBox: TBox3d; override;

    procedure ChangeLevelScene; override;
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

    procedure Idle; override;
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
    FObjects: TLevelObjectsList;
    FHeadlight: TVRMLGLHeadlight;

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
      GeometryNode: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);

    FCreatures: TCreaturesList;
    procedure TraverseForCreatures(
      BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
      BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
      GeometryNode: TVRMLGeometryNode;
      State: TVRMLGraphTraverseState);

    FInitialCameraPos: TVector3Single;
    FInitialCameraDir: TVector3Single;
    FInitialCameraUp: TVector3Single;
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
      const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean;
    function ObjectsMoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const BecauseOfGravity: boolean): boolean;
    { @groupEnd }

    FName: string;
    FSceneFileName: string;
    FTitle: string;
    FTitleHint: string;
    FNumber: Integer;
    FLightSetFileName: string;

    procedure LoadFromDOMElement(Element: TDOMElement);

    FMenuBackground: boolean;
    FSceneDynamicShadows: boolean;

    FRequiredCreatures: TStringList;

    FBumpMappingLightAmbientColor: array [boolean] of TVector4Single;
    function GetBumpMappingLightAmbientColor(const Lighted: boolean): TVector4Single;
    procedure SetBumpMappingLightAmbientColor(const Lighted: boolean; const Value: TVector4Single);

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
      CreateDefaultTriangleOctree, PrepareBackground: boolean): TVRMLGLScene;

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
      CreateFirstDefaultTriangleOctree,
      CreateLastDefaultTriangleOctree: boolean): TVRMLGLAnimation;

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
    property Objects: TLevelObjectsList read FObjects;

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

      Note about GroundItem: it is set to @nil if the ground item
      can't be represented as any octree item. Right now, this means that
      soemthing stands on another creature/player/item.

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

    function MoveBoxAllowedSimple(
      const CameraPos, NewPos: TVector3Single;
      const NewBox: TBox3d;
      const BecauseOfGravity: boolean): boolean; virtual;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
      out GroundItem: POctreeItem);
      virtual;
    { @groupEnd }

    { PlayerMoveAllowed and PlayerGetCameraHeightSqr just
      call appropriate non-player methods above.
      They use Navigator.CameraPos, and they use level's CameraRadius
      (i.e. they assume that it's the player who's moving).
      Use these to perform collision detection between player and the level.

      In addition, PlayerMoveAllowed checks collisions with Creatures.

      In addition, PlayerGetCameraHeightSqr sets Player.Ground
      (to the ground item under the player, or to @nil
      if player is not IsAboveTheGround).

      @groupBegin }
    function PlayerMoveAllowed(Navigator: TWalkNavigator;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    procedure PlayerGetCameraHeightSqr(Navigator: TWalkNavigator;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    { Call this to render level things: level scene and level objects
      in @link(Objects). Frustum is current player's frustum. }
    procedure Render(const Frustum: TFrustum); virtual;

    { Render shadow quads for all the things rendered by @link(Render).
      It does shadow volumes culling inside  (so ShadowVolumesHelper should
      have FrustumCullingInit already initialized). }
    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper); virtual;

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
      const Ray0, RayVector: TVector3Single): TCollisionInfo;

    { This is called when level was picked --- either the level Scene
      itself, or one of @link(Objects) on the level.

      @param CollisionInfo contains all details.
      @param(LevelObjectIndex contains index to @link(Objects), or -1
        if it was the Scene itself.)
      @param(Distance,InteractionOccured see SpecialObjectPicked)
    }
    procedure Picked(const Distance: Single; CollisionInfo: TCollisionInfo;
      LevelObjectIndex: Integer;
      var InteractionOccured: boolean); virtual;

    property InitialCameraPos: TVector3Single read FInitialCameraPos;
    property InitialCameraDir: TVector3Single read FInitialCameraDir;
    property InitialCameraUp : TVector3Single read FInitialCameraUp;

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

    { Properties of level's headlight.
      @Nil if no headlight should be used by default for this level.

      Note for descendants: TLevel sets this to nil if NavigationInfo.headlight
      field of level's VRML is FALSE. Otherwise TLevel initializes it by
      Scene.CreateHeadLight, i.e. uses KambiHeadLight VRML node if present.

      TLevel destructor frees this object. }
    property Headlight: TVRMLGLHeadlight read FHeadlight;

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

    { Turn off lights not supposed to light in the shadow, and detect position
      (if any) of the main light that produces shadows.

      Returns @true, sets MainLightPosition if there's light that produces shadows.
      In this case you should follow by rendering the scene without lighting,
      and calling PopLightsOff.

      Returns @false if there's no such light, and in this case no lights are
      turned off (you should revert to no-shadows algorithm in this case). }
    function PushLightsOff(out MainLightPosition: TVector4Single): boolean;
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

var
  { Properties of the Level octree constructed in TLevel constructor.
    -1 means "use default values", which means values taken from
    Level description in XML (or just VRMLTriangleOctree unit
    defaults). }
  DebugLevelOctreeMaxDepth: Integer = -1;
  DebugLevelOctreeMaxLeafItemsCount: Integer = -1;

{$undef read_interface}

implementation

uses SysUtils, GL, GLU, GLExt, Object3dAsVRML,
  CastlePlay, KambiGLUtils, KambiFilesUtils, KambiStringUtils,
  CastleVideoOptions, CastleConfig, CastleTimeMessages,
  CastleInputs, CastleWindow, KambiOpenAL, ALUtils, KambiXMLUtils,
  CastleRequiredResources, VRMLOpenGLRenderer;

{$define read_implementation}
{$I objectslist_2.inc}

{ TCollisionInfo ------------------------------------------------------------- }

constructor TCollisionInfo.Create;
begin
  inherited;
  Hierarchy := TLevelObjectsList.Create;
end;

destructor TCollisionInfo.Destroy;
begin
  FreeAndNil(Hierarchy);
end;

{ TLevelObject --------------------------------------------------------------- }

constructor TLevelObject.Create(AParentLevel: TLevel);
begin
  inherited Create;
  FParentLevel := AParentLevel;
  FCastsShadow := true;
end;

procedure TLevelObject.BeforeIdle(const NewAnimationTime: TKamTime);
begin
  { Nothing to do in this class. }
end;

procedure TLevelObject.Idle;
begin
  { Nothing to do in this class. }
end;

procedure TLevelObject.ChangeLevelScene;
begin
  { Nothing to do in this class. }
end;

{ TLevelObjectsList ---------------------------------------------------------- }

function TLevelObjectsList.RayCollision(
  out IntersectionDistance: Single; out Index: Integer;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
var
  I: Integer;
  ThisIntersectionDistance: Single;
  ThisCollision: TCollisionInfo;
begin
  Result := nil;
  IntersectionDistance := 0; { Only to silence compiler warning }
  for I := 0 to High do
  begin
    ThisCollision := Items[I].RayCollision(
      ThisIntersectionDistance, Ray0, RayVector, nil);
    if ThisCollision <> nil then
    begin
      if (Result = nil) or (ThisIntersectionDistance < IntersectionDistance) then
      begin
        IntersectionDistance := ThisIntersectionDistance;
        Index := I;
        SysUtils.FreeAndNil(Result);
        Result := ThisCollision;
      end else
        SysUtils.FreeAndNil(ThisCollision);
    end;
  end;
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
      CameraRadius, false, NoItemIndex, ItemsToIgnoreFunc);
end;

function TLevelStaticObject.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    Scene.DefaultTriangleOctree.MoveBoxAllowedSimple(
      OldPos, ProposedNewPos, ProposedNewBox,
      false, NoItemIndex, ItemsToIgnoreFunc);
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

function TLevelStaticObject.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Scene.DefaultTriangleOctree.SphereCollision(
      Pos, Radius,  NoItemIndex, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

function TLevelStaticObject.BoxCollision(const Box: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    (Scene.DefaultTriangleOctree.BoxCollision(
      Box,  NoItemIndex, ItemsToIgnoreFunc)
      <> NoItemIndex);
end;

function TLevelStaticObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
var
  OctreeItemIndex: Integer;
begin
  Result := nil;
  if Exists and Collides then
  begin
    OctreeItemIndex := Scene.DefaultTriangleOctree.RayCollision(
      IntersectionDistance,
      Ray0, RayVector,
      false, NoItemIndex, false, ItemsToIgnoreFunc);
    if OctreeItemIndex <> NoItemIndex then
    begin
      Result := TCollisionInfo.Create;
      Result.OctreeItem :=
        Scene.DefaultTriangleOctree.OctreeItems.Pointers[OctreeItemIndex];
      Result.Hierarchy.Add(Self);
    end;
  end;
end;

procedure TLevelStaticObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
  var GroundItem: POctreeItem);
var
  IsAboveThis: boolean;
  HeightAboveThis: Single;
  GroundItemIndexThis: Integer;
begin
  if Exists and Collides then
  begin
    Scene.DefaultTriangleOctree.GetCameraHeightZ(
      CameraPos,
      IsAboveThis, HeightAboveThis, GroundItemIndexThis,
      NoItemIndex, ItemsToIgnoreFunc);

    if IsAboveThis and
      ((not IsAboveTheGround) or (HeightAboveThis < HeightAboveTheGround)) then
    begin
      IsAboveTheGround := true;
      HeightAboveTheGround := HeightAboveThis;
      GroundItem := Scene.DefaultTriangleOctree.OctreeItems.Pointers[GroundItemIndexThis];
    end;
  end;
end;

procedure TLevelStaticObject.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
  if Exists then
    Scene.RenderFrustum(Frustum, TransparentGroup);
end;

procedure TLevelStaticObject.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  if Exists and CastsShadow then
  begin
    Scene.InitAndRenderShadowVolume(ShadowVolumesHelper,
      ParentTransformIsIdentity, ParentTransform);
  end;
end;

function TLevelStaticObject.BoundingBox: TBox3d;
begin
  if Exists and Collides then
    Result := Scene.BoundingBox else
    Result := EmptyBox3d;
end;

{ TLevelObjectSum ------------------------------------------------------------ }

constructor TLevelObjectSum.Create(AParentLevel: TLevel);
begin
  inherited;
  FList := TLevelObjectsList.Create;
end;

destructor TLevelObjectSum.Destroy;
begin
  FreeWithContentsAndNil(FList);
  inherited;
end;

procedure TLevelObjectSum.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
var
  I: Integer;
begin
  for I := 0 to List.High do
    List.Items[I].Render(Frustum, TransparentGroup);
end;

procedure TLevelObjectSum.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  I: Integer;
begin
  if CastsShadow then
  begin
    for I := 0 to List.High do
      List.Items[I].RenderShadowVolume(ShadowVolumesHelper,
        ParentTransformIsIdentity, ParentTransform);
  end;
end;

function TLevelObjectSum.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 0 to List.High do
  begin
    Result := List.Items[I].MoveAllowedSimple(OldPos, ProposedNewPos,
      CameraRadius, ItemsToIgnoreFunc);
    if not Result then Exit;
  end;
end;

function TLevelObjectSum.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 0 to List.High do
  begin
    Result := List.Items[I].MoveBoxAllowedSimple(OldPos, ProposedNewPos,
      ProposedNewBox, ItemsToIgnoreFunc);
    if not Result then Exit;
  end;
end;

function TLevelObjectSum.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to List.High do
  begin
    Result := List.Items[I].SegmentCollision(Pos1, Pos2, ItemsToIgnoreFunc);
    if Result then Exit;
  end;
end;

function TLevelObjectSum.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to List.High do
  begin
    Result := List.Items[I].SphereCollision(Pos, Radius, ItemsToIgnoreFunc);
    if Result then Exit;
  end;
end;

function TLevelObjectSum.BoxCollision(const Box: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to List.High do
  begin
    Result := List.Items[I].BoxCollision(Box, ItemsToIgnoreFunc);
    if Result then Exit;
  end;
end;

function TLevelObjectSum.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
var
  Index: Integer;
begin
  Result := List.RayCollision(IntersectionDistance, Index, Ray0, RayVector,
    ItemsToIgnoreFunc);
  if Result <> nil then
    Result.Hierarchy.Insert(0, Self);
end;

procedure TLevelObjectSum.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
  var GroundItem: POctreeItem);
var
  I: Integer;
begin
  for I := 0 to List.High do
    List.Items[I].GetCameraHeight(CameraPos, ItemsToIgnoreFunc,
      IsAboveTheGround, HeightAboveTheGround, GroundItem);
end;

procedure TLevelObjectSum.BeforeIdle(const NewAnimationTime: TKamTime);
var
  I: Integer;
begin
  for I := 0 to List.High do
    List.Items[I].BeforeIdle(NewAnimationTime);
end;

procedure TLevelObjectSum.Idle;
var
  I: Integer;
begin
  for I := 0 to List.High do
    List.Items[I].Idle;
end;

function TLevelObjectSum.BoundingBox: TBox3d;
var
  I: Integer;
begin
  Result := EmptyBox3d;
  for I := 0 to List.High do
    Box3dSumTo1st(Result, List.Items[I].BoundingBox);
end;

{ TLevelMovingObject --------------------------------------------------------- }

constructor TLevelMovingObject.Create(AParentLevel: TLevel);
begin
  inherited Create(AParentLevel);
  FExists := true;
  FCollides := true;
  FMovePushesOthers := true;
  FMovePushesOthersUsesBoxes := true;
end;

destructor TLevelMovingObject.Destroy;
begin
  FreeAndNil(FMovingObject);
  inherited;
end;

function TLevelMovingObject.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  Result := (not Exists) or (not Collides);
  if not Result then
  begin
    { I have to check collision between
        MovingObject + Translation and (OldPos, ProposedNewPos).
      So it's equivalent to checking for collision between
        MovingObject and (OldPos, ProposedNewPos) - Translation
      And this way I can use MovingObject.MoveAllowedSimple. }

    T := Translation(ParentLevel.AnimationTime);
    Result := MovingObject.MoveAllowedSimple(
      VectorSubtract(OldPos, T.Data),
      VectorSubtract(ProposedNewPos, T.Data),
      CameraRadius, ItemsToIgnoreFunc);
  end;
end;

function TLevelMovingObject.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
  B: TBox3d;
begin
  Result := (not Exists) or (not Collides);
  if not Result then
  begin
    { I have to check collision between
        MovingObject + Translation and (OldPos, ProposedNewPos).
      So it's equivalent to checking for collision between
        MovingObject and (OldPos, ProposedNewPos) - Translation
      And this way I can use MovingObject.MoveBoxAllowedSimple. }

    T := Translation(ParentLevel.AnimationTime);
    B[0] := VectorSubtract(ProposedNewBox[0], T.Data);
    B[1] := VectorSubtract(ProposedNewBox[1], T.Data);
    Result := MovingObject.MoveBoxAllowedSimple(
      VectorSubtract(OldPos, T.Data),
      VectorSubtract(ProposedNewPos, T.Data),
      B, ItemsToIgnoreFunc);
  end;
end;

function TLevelMovingObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  Result := Exists and Collides;
  if Result then
  begin
    { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
      use MovingObject.SegmentCollsion with Translation. }

    T := Translation(ParentLevel.AnimationTime);
    Result := MovingObject.SegmentCollision(
      VectorSubtract(Pos1, T.Data),
      VectorSubtract(Pos2, T.Data), ItemsToIgnoreFunc);
  end;
end;

function TLevelMovingObject.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  Result := Exists and Collides;
  if Result then
  begin
    { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
      use MovingObject.SphereCollsion with Translation. }

    T := Translation(ParentLevel.AnimationTime);
    Result := MovingObject.SphereCollision(
      VectorSubtract(Pos, T.Data),  Radius, ItemsToIgnoreFunc);
  end;
end;

function TLevelMovingObject.BoxCollision(
  const Box: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
var
  T: TVector3_Single;
begin
  Result := Exists and Collides;
  if Result then
  begin
    { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
      use MovingObject.BoxCollsion with Translation. }

    T := Translation(ParentLevel.AnimationTime);
    Result := MovingObject.BoxCollision(
      Box3dAntiTranslate(Box, T.Data),  ItemsToIgnoreFunc);
  end;
end;

function TLevelMovingObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
var
  T: TVector3_Single;
begin
  Result := nil;
  if Exists and Collides then
  begin
    { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
      use MovingObject.RayCollsion with Translation. }

    T := Translation(ParentLevel.AnimationTime);
    Result := MovingObject.RayCollision(IntersectionDistance,
      VectorSubtract(Ray0, T.Data),
      RayVector, ItemsToIgnoreFunc);

    if Result <> nil then
      Result.Hierarchy.Insert(0, Self);
  end;
end;

procedure TLevelMovingObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
  var GroundItem: POctreeItem);
var
  T: TVector3_Single;
begin
  if Exists and Collides then
  begin
    T := Translation(ParentLevel.AnimationTime);

    MovingObject.GetCameraHeight(
      VectorSubtract(CameraPos, T.Data), ItemsToIgnoreFunc,
      IsAboveTheGround, HeightAboveTheGround, GroundItem);
  end;
end;

procedure TLevelMovingObject.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
var
  T: TVector3_Single;
begin
  if Exists then
  begin
    T := Translation(ParentLevel.AnimationTime);

    { We assume that Translation = 0,0,0 is the most common case
      (this is true e.g. for TDoomLevelDoor,
      since all doors close automatically, and initially all are closed...).

      In this case we can avoid FrustumMove (although I didn't do any tests,
      maybe this check is not worth the effort and we don't need to worry
      about FrustumMove time so much ?). }

    if IsZeroVector(T.Data) then
      MovingObject.Render(Frustum, TransparentGroup) else
      begin
        glPushMatrix;
          glTranslatev(T.Data);
          { MovingObject.Render expects Frustum in it's local coordinates,
            that's why we subtract Translation here. }
          MovingObject.Render(FrustumMove(Frustum, (-T).Data), TransparentGroup);
        glPopMatrix;
      end;
  end;
end;

procedure TLevelMovingObject.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  T: TVector3_Single;
begin
  if Exists and CastsShadow then
  begin
    T := Translation(ParentLevel.AnimationTime);

    { We assume that Translation = 0,0,0 is the most common case
      (this is true e.g. for TDoomLevelDoor,
      since all doors close automatically, and initially all are closed...).

      In this case we can avoid matrix multiplication. }

    if IsZeroVector(T.Data) then
      MovingObject.RenderShadowVolume(ShadowVolumesHelper,
        ParentTransformIsIdentity, ParentTransform) else
      MovingObject.RenderShadowVolume(ShadowVolumesHelper,
        false, MultMatrices(TranslationMatrix(T.Data), ParentTransform));
  end;
end;

procedure TLevelMovingObject.BeforeIdle(
  const NewAnimationTime: TKamTime);

  function BoundingBoxAssumeTranslation(
    const AssumeTranslation: TVector3_Single): TBox3d;
  begin
    if Exists and Collides then
      Result := Box3dTranslate(MovingObject.BoundingBox, AssumeTranslation) else
      Result := EmptyBox3d;
  end;

  function SphereCollisionAssumeTranslation(
    const AssumeTranslation: TVector3_Single;
    const Pos: TVector3Single; const Radius: Single;
    const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
  begin
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
        use MovingObject.SphereCollsion with Translation. }

      Result := MovingObject.SphereCollision(
        VectorSubtract(Pos, AssumeTranslation.Data), Radius, ItemsToIgnoreFunc);
    end;
  end;

  function BoxCollisionAssumeTranslation(
    const AssumeTranslation: TVector3_Single;
    const Box: TBox3d;
    const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
  begin
    Result := Exists and Collides;
    if Result then
    begin
      { We use the same trick as in TLevelMovingObject.MoveAllowedSimple to
        use MovingObject.BoxCollsion with Translation. }

      Result := MovingObject.BoxCollision(
        Box3dAntiTranslate(Box, AssumeTranslation.Data), ItemsToIgnoreFunc);
    end;
  end;

var
  CurrentBox, NewBox, Box: TBox3d;
  I: Integer;
  Move: TVector3_Single;
  CurrentTranslation, NewTranslation: TVector3_Single;
  Crea: TCreature;
  Item: TItemOnLevel;
begin
  MovingObject.BeforeIdle(NewAnimationTime);

  if Exists and Collides and MovePushesOthers then
  begin
    CurrentTranslation := Translation(ParentLevel.AnimationTime);
    NewTranslation := Translation(NewAnimationTime);

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
          Player.Navigator.CameraPos := Player.Navigator.CameraPos + Move;

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
          Player.Navigator.CameraPos, ParentLevel.CameraRadius,
          @ParentLevel.CollisionIgnoreItem) then
          Player.Navigator.CameraPos := Player.Navigator.CameraPos + Move;

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

procedure TLevelMovingObject.Idle;
begin
  MovingObject.Idle;
end;

function TLevelMovingObject.BoundingBox: TBox3d;
begin
  if Exists and Collides then
    Result := Box3dTranslate(MovingObject.BoundingBox,
      Translation(ParentLevel.AnimationTime)) else
    Result := EmptyBox3d;
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

function TLevelLinearMovingObject.Translation(const AnimationTime: TKamTime):
  TVector3_Single;
begin
  if not EndPosition then
  begin
    if AnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely closed. }
      Result.Init_Zero else
      { During closing. }
      Result := TranslationEnd *
        (1 - (AnimationTime - EndPositionStateChangeTime) / MoveTime);
  end else
  begin
    if AnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely open. }
      Result := TranslationEnd else
      { During opening. }
      Result := TranslationEnd *
        ((AnimationTime - EndPositionStateChangeTime) / MoveTime);
  end;
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

procedure TLevelLinearMovingObject.Idle;
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
    (Animation.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
       OldPos, ProposedNewPos,
       CameraRadius, false, NoItemIndex, ItemsToIgnoreFunc) and
       ( (not CollisionUseLastScene) or
         Animation.LastScene.DefaultTriangleOctree.MoveAllowedSimple(
           OldPos, ProposedNewPos,
           CameraRadius, false, NoItemIndex, ItemsToIgnoreFunc) ));
end;

function TLevelAnimatedObject.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or
    (Animation.FirstScene.DefaultTriangleOctree.MoveBoxAllowedSimple(
       OldPos, ProposedNewPos, ProposedNewBox,
       false, NoItemIndex, ItemsToIgnoreFunc) and
       ( (not CollisionUseLastScene) or
         Animation.LastScene.DefaultTriangleOctree.MoveBoxAllowedSimple(
           OldPos, ProposedNewPos, ProposedNewBox,
           false, NoItemIndex, ItemsToIgnoreFunc) ));
end;

function TLevelAnimatedObject.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.DefaultTriangleOctree.SegmentCollision(
         Pos1, Pos2,
         false, NoItemIndex, false, ItemsToIgnoreFunc)
         <> NoItemIndex) or
      (CollisionUseLastScene and
        (Animation.LastScene.DefaultTriangleOctree.SegmentCollision(
           Pos1, Pos2,
           false, NoItemIndex, false, ItemsToIgnoreFunc)
           <> NoItemIndex) ));
end;

function TLevelAnimatedObject.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.DefaultTriangleOctree.SphereCollision(
         Pos, Radius, NoItemIndex, ItemsToIgnoreFunc)
         <> NoItemIndex) or
      (CollisionUseLastScene and
        (Animation.LastScene.DefaultTriangleOctree.SphereCollision(
           Pos, Radius, NoItemIndex, ItemsToIgnoreFunc)
           <> NoItemIndex) ));
end;

function TLevelAnimatedObject.BoxCollision(
  const Box: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result := Exists and Collides and
    ( (Animation.FirstScene.DefaultTriangleOctree.BoxCollision(
         Box, NoItemIndex, ItemsToIgnoreFunc)
         <> NoItemIndex) or
      (CollisionUseLastScene and
        (Animation.LastScene.DefaultTriangleOctree.BoxCollision(
           Box, NoItemIndex, ItemsToIgnoreFunc)
           <> NoItemIndex) ));
end;

function TLevelAnimatedObject.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
var
  OctreeItemIndex: Integer;
  Octree: TVRMLTriangleOctree;
begin
  Result := nil;
  if Exists and Collides then
  begin
    Octree := Animation.FirstScene.DefaultTriangleOctree;
    OctreeItemIndex := Octree.RayCollision(IntersectionDistance,
      Ray0, RayVector,
      false, NoItemIndex, false, ItemsToIgnoreFunc);
    if OctreeItemIndex <> NoItemIndex then
    begin
      Result := TCollisionInfo.Create;
      Result.OctreeItem := Octree.OctreeItems.Pointers[OctreeItemIndex];
      Result.Hierarchy.Add(Self);
    end else
    if CollisionUseLastScene then
    begin
      { try the same thing on LastScene }
      Octree := Animation.LastScene.DefaultTriangleOctree;
      OctreeItemIndex := Octree.RayCollision(IntersectionDistance,
        Ray0, RayVector,
        false, NoItemIndex, false, ItemsToIgnoreFunc);
      if OctreeItemIndex <> NoItemIndex then
      begin
        Result := TCollisionInfo.Create;
        Result.OctreeItem := Octree.OctreeItems.Pointers[OctreeItemIndex];
        Result.Hierarchy.Add(Self);
      end
    end;
  end;
end;

procedure TLevelAnimatedObject.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
  var GroundItem: POctreeItem);

  procedure MakeScene(Scene: TVRMLScene);
  var
    IsAboveThis: boolean;
    HeightAboveThis: Single;
    GroundItemIndexThis: Integer;
  begin
    Scene.DefaultTriangleOctree.GetCameraHeightZ(
      CameraPos,
      IsAboveThis, HeightAboveThis, GroundItemIndexThis,
      NoItemIndex, ItemsToIgnoreFunc);

    if IsAboveThis and
      ((not IsAboveTheGround) or (HeightAboveThis < HeightAboveTheGround)) then
    begin
      IsAboveTheGround := true;
      HeightAboveTheGround := HeightAboveThis;
      GroundItem := Scene.DefaultTriangleOctree.OctreeItems.
        Pointers[GroundItemIndexThis];
    end;
  end;

begin
  if Exists and Collides then
  begin
    MakeScene(Animation.FirstScene);
    if CollisionUseLastScene then
      MakeScene(Animation.LastScene);
  end;
end;

procedure TLevelAnimatedObject.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
  if Exists then
    Animation.SceneFromTime(AnimationTime).RenderFrustum(Frustum, TransparentGroup);
end;

procedure TLevelAnimatedObject.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  if Exists and CastsShadow then
  begin
    Animation.SceneFromTime(AnimationTime).
      InitAndRenderShadowVolume(ShadowVolumesHelper,
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

procedure TLevelSimpleAnimatedObject.Idle;
begin
  inherited;
  if Started then
    AnimationTime := ParentLevel.AnimationTime - FPlayStartTime else
    AnimationTime := 0;
end;

{ TLevelArea ----------------------------------------------------------------- }

constructor TLevelArea.Create(AParentLevel: TLevel);
begin
  inherited;
  FBox := EmptyBox3d;
end;

procedure TLevelArea.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
  { This object is invisible and non-colliding. }
end;

procedure TLevelArea.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  { This object is invisible and non-colliding. }
end;

function TLevelArea.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := true;
end;

function TLevelArea.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := true;
end;

function TLevelArea.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.BoxCollision(const ABox: TBox3d;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  { This object is invisible and non-colliding. }
  Result := false;
end;

function TLevelArea.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): TCollisionInfo;
begin
  { This object is invisible and non-colliding. }
  Result := nil;
end;

procedure TLevelArea.GetCameraHeight(const CameraPos: TVector3Single;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc;
  var IsAboveTheGround: boolean; var HeightAboveTheGround: Single;
  var GroundItem: POctreeItem);
begin
  { This object is invisible and non-colliding. }
end;

function TLevelArea.BoundingBox: TBox3d;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3d;
end;

procedure TLevelArea.ChangeLevelScene;
begin
  inherited;
  ParentLevel.RemoveBoxNodeCheck(FBox, VRMLName);
end;

function TLevelArea.PointInside(const Point: TVector3Single): boolean;
begin
  Result := Box3dPointInside(Point, Box);
end;

{ TLevelHintArea ----------------------------------------------------------- }

procedure TLevelHintArea.Idle;
var
  ReplaceInteractInput: TPercentReplace;
begin
  if (not MessageDone) and PointInside(Player.Navigator.CameraPos) then
  begin
    ReplaceInteractInput.C := 'i';
    ReplaceInteractInput.S := InteractInputDescription;
    TimeMessage(SPercentReplace(Message, [ReplaceInteractInput], true));
    MessageDone := true;
  end;
end;

{ TLevel --------------------------------------------------------------------- }

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
  OctreeMaxDepth, OctreeMaxLeafItemsCount: Integer;
  NavigationSpeed: Single;
  Options: TPrepareRenderOptions;
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
    FScene := TVRMLGLScene.Create(SceneFileName, roSeparateShapeStates,
      GLContextCache);

    { initialize FAnimationTime. Must be initialized before creating creatures. }
    FAnimationTime := 0.0;

    AttributesSet(Scene.Attributes, btIncrease);
    if BumpMapping then
      Scene.Attributes.BumpMappingMaximum := High(TBumpMappingMethod) else
      Scene.Attributes.BumpMappingMaximum := bmNone;

    { Calculate InitialCameraPos, InitialCameraDir, InitialCameraUp.
      Must be done before initializing creatures, as they right now
      use InitialCameraPos. FInitialCameraDir, FInitialCameraUp will be
      actually changed later in this procedure. }
    Scene.GetPerspectiveViewpoint(FInitialCameraPos,
      FInitialCameraDir, FInitialCameraUp, FGravityUp);

    FObjects := TLevelObjectsList.Create;

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
      NavigationSpeed := NavigationNode.FdSpeed.Value else
      NavigationSpeed := 1.0;

    if (NavigationNode <> nil) and NavigationNode.FdHeadlight.Value then
      FHeadlight := Scene.CreateHeadlight else
      FHeadlight := nil;

    FProjectionNear := CameraRadius * 0.75;
    FProjectionFar := Box3dMaxSize(Scene.BoundingBox) * 5;

    { Fix InitialCameraDir length, and set MoveXxxSpeed.

      We want to have horizontal and vertical speeds controlled independently,
      so we just normalize InitialCameraDir and set speeds in appropriate
      MoveXxxSpeed. }
    NormalizeTo1st(FInitialCameraDir);
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

    Scene.PrepareRender([tgAll], Options);

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

  { calculate OctreeMaxDepth }
  OctreeMaxDepth := DebugLevelOctreeMaxDepth;
  { TODO: take from XML file }
  if OctreeMaxDepth = -1 then
    OctreeMaxDepth := DefTriangleOctreeMaxDepth;

  { calculate OctreeMaxLeafItemsCount }
  OctreeMaxLeafItemsCount := DebugLevelOctreeMaxLeafItemsCount;
  { TODO: take from XML file }
  if OctreeMaxLeafItemsCount = -1 then
    OctreeMaxLeafItemsCount := DefTriangleOctreeMaxLeafItemsCount;

  { Loading octree have their own Progress, so we load them outside our
    progress. }

  if not MenuBackground then
  begin
    Scene.DefaultTriangleOctree :=
      Scene.CreateTriangleOctree(
        OctreeMaxDepth, OctreeMaxLeafItemsCount,
        'Loading level (triangle octree)');
    Scene.DefaultShapeStateOctree :=
      Scene.CreateShapeStateOctree('Loading level (ShapeState octree)');

    { TrianglesList was created for triangle octree. We don't need it anymore. }
    Scene.FreeResources([frTrianglesListNotOverTriangulate]);
  end;
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

  function LevelObjectFromDOMElement(Element: TDOMElement): TLevelObject;
  begin
    if Element.TagName = 'area' then
      Result := LevelAreaFromDOMElement(Element) else
    if (Element.TagName = 'required_resources') or
       (Element.TagName = 'bump_mapping_light') then
    begin
      { These are handled elsewhere, and don't produce any TLevelObject. }
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
  NewObject: TLevelObject;
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
  finally ObjectsList.Release end;

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
  BoxNodeIndex: Integer;
begin
  BoxNodeIndex := Scene.ShapeStates.IndexOfBlenderMesh(NodeName);
  Result := BoxNodeIndex <> -1;
  if Result then
  begin
    { When node with name NodeName is found, then we calculate our
      Box from this node (and we delete this node from the scene,
      as it should not be visible).
      This way we can comfortably set such boxes from Blender. }
    Box := Scene.ShapeStates[BoxNodeIndex].BoundingBox;
    Scene.ShapeStates[BoxNodeIndex].GeometryNode.FreeRemovingFromAllParents;
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
  for I := 0 to Objects.High do
    Objects[I].ChangeLevelScene;
end;

procedure TLevel.TraverseForItems(
  BlenderObjectNode: TVRMLNode; const BlenderObjectName: string;
  BlenderMeshNode: TVRMLNode; const BlenderMeshName: string;
  GeometryNode: TVRMLGeometryNode;
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

    ItemStubBoundingBox := GeometryNode.BoundingBox(State);
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
  GeometryNode: TVRMLGeometryNode;
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
    StubBoundingBox := GeometryNode.BoundingBox(State);
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
    CreatureDirection := VectorSubtract(InitialCameraPos, CreaturePosition);
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

function TLevel.ObjectsMoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const BecauseOfGravity: boolean): boolean;
var
  I: Integer;
begin
  for I := 0 to Objects.High do
    if not Objects[I].MoveBoxAllowedSimple(
      OldPos, ProposedNewPos, ProposedNewBox,
      @CollisionIgnoreItem) then
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
      false, NoItemIndex, @CollisionIgnoreItem) and
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
      false, NoItemIndex, @CollisionIgnoreItem) and
    ObjectsMoveAllowedSimple(
      CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TLevel.MoveBoxAllowedSimple(const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const NewBox: TBox3d;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    Box3dPointInside(NewPos, LevelBox) and
    Scene.DefaultTriangleOctree.MoveBoxAllowedSimple(
      CameraPos, NewPos, NewBox,
      false, NoItemIndex, @CollisionIgnoreItem) and
    ObjectsMoveBoxAllowedSimple(
      CameraPos, NewPos, NewBox, BecauseOfGravity);
end;

procedure TLevel.GetCameraHeight(const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
  out GroundItem: POctreeItem);
var
  I: Integer;
  GroundItemIndex: Integer;
begin
  Scene.DefaultTriangleOctree.GetCameraHeightZ(
    CameraPos,
    IsAboveTheGround, HeightAboveTheGround, GroundItemIndex,
    NoItemIndex, @CollisionIgnoreItem);
  if IsAboveTheGround then
    GroundItem := Scene.DefaultTriangleOctree.OctreeItems.Pointers[GroundItemIndex];

  for I := 0 to Objects.High do
    Objects[I].GetCameraHeight(CameraPos, @CollisionIgnoreItem,
      IsAboveTheGround, HeightAboveTheGround, GroundItem);
end;

function TLevel.PlayerMoveAllowed(Navigator: TWalkNavigator;
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

procedure TLevel.PlayerGetCameraHeightSqr(Navigator: TWalkNavigator;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  HeightAboveTheGround: Single;
  GroundItem: POctreeItem;
begin
  { Check is player standing over level. }
  GetCameraHeight(Navigator.CameraPos, IsAboveTheGround,
    HeightAboveTheGround, GroundItem);

  { Check is player standing over one of the creatures. }
  Creatures.GetCameraHeight(Navigator.CameraPos, IsAboveTheGround,
    HeightAboveTheGround, GroundItem, nil);

  if IsAboveTheGround then
  begin
    { Below is a waste of time. Because Navigator requires a callback
      that passes only SqrHeightAboveTheGround, we Sqr here the
      HeightAboveTheGround... Inside Navigator will possibly call Sqrt
      on this, thus wasting time on useless Sqrt operation.
      For now this is not a problem, in the future Navigator should
      be extended to have callback that returns ready HeightAboveTheGround. }
    SqrHeightAboveTheGround := Sqr(HeightAboveTheGround);

    Player.Ground := GroundItem;
  end else
    Player.Ground := nil;
end;

procedure TLevel.Render(const Frustum: TFrustum);
var
  I: Integer;
begin
  { First pass rendering Objects: render non-transparent parts }
  for I := 0 to Objects.High do
    Objects[I].Render(Frustum, tgOpaque);

  if not MenuBackground then
    Scene.RenderFrustumOctree(Frustum, tgAll) else
    Scene.RenderFrustum(Frustum, tgAll);

  { Second pass rendering Objects: render transparent parts }
  for I := 0 to Objects.High do
    Objects[I].Render(Frustum, tgTransparent);
end;

procedure TLevel.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper);
var
  I: Integer;
begin
  for I := 0 to Objects.High do
    Objects[I].RenderShadowVolume(ShadowVolumesHelper,
      true, IdentityMatrix4Single);

  if SceneDynamicShadows then
  begin
    { Useless to optimize this by shadow culling in ShadowVolumesHelper,
      Scene will be visible practically always. }
    ShadowVolumesHelper.InitSceneAlwaysVisible;
    Scene.RenderShadowVolume(ShadowVolumesHelper, true, IdentityMatrix4Single);
  end;
end;

procedure TLevel.Idle(const CompSpeed: Single);
var
  I: Integer;
  NewAnimationTime: TKamTime;
begin
  NewAnimationTime := AnimationTime + CompSpeed;

  for I := 0 to Objects.High do
    Objects[I].BeforeIdle(NewAnimationTime);

  FAnimationTime := NewAnimationTime;

  for I := 0 to Objects.High do
    Objects[I].Idle;

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
  const Ray0, RayVector: TVector3Single): TCollisionInfo;
var
  OctreeItemIndex: Integer;
  ThisIntersectionDistance: Single;
  ThisCollision: TCollisionInfo;
  ThisLevelObjectIndex: Integer;
  {$ifdef DEBUG_PICK}
  S: string;
  I: Integer;
  {$endif DEBUG_PICK}
begin
  { First, assume no collision }
  Result := nil;

  { Collision with Level.Scene }
  OctreeItemIndex := Scene.DefaultTriangleOctree.RayCollision(
    ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex, false, nil);
  if OctreeItemIndex <> NoItemIndex then
  begin
    IntersectionDistance := ThisIntersectionDistance;
    LevelObjectIndex := -1;
    Result := TCollisionInfo.Create;
    Result.OctreeItem := Scene.DefaultTriangleOctree.OctreeItems.Pointers[
      OctreeItemIndex];
  end;

  { Collisions with Objects[] }
  ThisCollision := Objects.RayCollision(
    ThisIntersectionDistance, ThisLevelObjectIndex,
    Ray0, RayVector, nil);
  if ThisCollision <> nil then
  begin
    if (Result = nil) or (ThisIntersectionDistance < IntersectionDistance) then
    begin
      IntersectionDistance := ThisIntersectionDistance;
      LevelObjectIndex := ThisLevelObjectIndex;
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
  CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
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
  CreateDefaultTriangleOctree, PrepareBackground: boolean): TVRMLGLScene;
var
  Options: TPrepareRenderOptions;
begin
  Result := TVRMLGLScene.Create(FileName, roSeparateShapeStates, GLContextCache);
  AttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareRender }
  Options := [prBoundingBox { BoundingBox is practically always needed }];
  if PrepareBackground then
    Include(Options, prBackground);
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareRender([tgOpaque, tgTransparent], Options);

  if CreateDefaultTriangleOctree then
    Result.DefaultTriangleOctree := Result.CreateTriangleOctree('');

  Result.FreeResources([frTextureDataInNodes]);
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstDefaultTriangleOctree,
  CreateLastDefaultTriangleOctree: boolean): TVRMLGLAnimation;
var
  Options: TPrepareRenderOptions;
begin
  Result := TVRMLGLAnimation.Create(GLContextCache);
  Result.LoadFromFile(FileName);

  AnimationAttributesSet(Result.Attributes, btIncrease);

  { calculate Options for PrepareRender }
  Options := [prBoundingBox { BoundingBox is practically always needed }];
  if RenderShadowsPossible then
    Options := Options + prShadowVolume;

  Result.PrepareRender([tgOpaque, tgTransparent], Options, false);

  if CreateFirstDefaultTriangleOctree then
    Result.FirstScene.DefaultTriangleOctree :=
      Result.FirstScene.CreateTriangleOctree('');

  if CreateLastDefaultTriangleOctree then
    Result.LastScene.DefaultTriangleOctree :=
      Result.LastScene.CreateTriangleOctree('');

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

function TLevel.PushLightsOff(out MainLightPosition: TVector4Single): boolean;
begin
  glPushAttrib(GL_LIGHTING_BIT);

  { Headlight will stay on here, but lights in Level.LightSet are off. }
  Result := LightSet.TurnLightsOffForShadows(MainLightPosition);

  if not Result then
  begin
    glPopAttrib();
    Exit;
  end;

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
