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

{ TLevel class and some specialized descendants. }
unit GameLevel;

interface

uses VectorMath, CastleSceneCore, CastleScene, Boxes3D,
  X3DNodes, X3DFields, GameItems, Cameras,
  GameCreatures, SceneWaypoints, GameSound,
  CastleUtils, CastleClassUtils, GamePlayer, GameThunder, GameObjectKinds,
  ProgressUnit, PrecalculatedAnimation,
  DOM, XmlSoundEngine, Base3D, Shape,
  Classes, CastleTimeUtils, CastleSceneManager, GLRendererShader;

type
  TLevel = class;

  TCastleSceneClass = class of TCastleScene;
  TCastlePrecalculatedAnimationClass = class of TCastlePrecalculatedAnimation;

  { Invisible and non-colliding areas on the level that have some special purpose.
    What exactly this "purpose" is, is defined in each TLevelArea descendant.

    This class defines only a properties to define the area.
    For now, each area is just one TBox3D. }
  TLevelArea = class(T3D)
  private
    FId: string;
    FBox: TBox3D;

    { Area. Default value is EmptyBox3D. }
    property Box: TBox3D read FBox write FBox;
  public
    constructor Create(AOwner: TComponent); override;

    { Name used to recognize this object's area in level VRML/X3D file.

      If this object is present during ChangeLevelScene call from
      TLevel constructor then the shape with a parent named like @link(Id)
      will be removed from VRML/X3D file, and it's BoundingBox will be used
      as Box3D of this object.

      This way you can easily configure area of this object in Blender:
      just add a cube, set it's mesh name to match with this @link(Id),
      and then this cube defines Box3D of this object. }
    property Id: string read FId write FId;

    function PointInside(const Point: TVector3Single): boolean;

    function BoundingBox: TBox3D; override;

    { Called from TLevel constructor. This is the place when you
      can modify AParentLevel.MainScene.RootNode, e.g. by calling
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
    FCameraPreferredHeight: Single;
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
    FAnimationTime: TFloatTime;

    FSectors: TSceneSectorList;
    FWaypoints: TSceneWaypointList;

    FWaterBox: TBox3D;
    FAboveWaterBox: TBox3D;

    FPlayedMusicSound: TSoundType;
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

    FRequiredResources: T3DResourceList;
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
      ARequiredResources: T3DResourceList;
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
    property CameraPreferredHeight: Single read FCameraPreferredHeight;
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

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { This is the time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: TFloatTime read FAnimationTime;

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

    property SickProjection: boolean
      read FSickProjection write SetSickProjection;
    property SickProjectionSpeed: TFloatTime
      read FSickProjectionSpeed write SetSickProjectionSpeed;

    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;
  end;

  TLevelClass = class of TLevel;

implementation

uses SysUtils, GL,
  GamePlay, CastleGLUtils, CastleFilesUtils, CastleStringUtils,
  GameVideoOptions, GameConfig, GameNotifications,
  GameInputs, GameWindow, CastleXMLUtils,
  GameRequiredResources, GLRenderer, RenderingCameraUnit, Math;

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
  AParentLevel.RemoveBoxNodeCheck(FBox, Id);
end;

function TLevelArea.PointInside(const Point: TVector3Single): boolean;
begin
  Result := Box.PointInside(Point);
end;

{ TLevelHintArea ----------------------------------------------------------- }

procedure TLevelHintArea.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  ReplaceInteractInput: TPercentReplace;
begin
  inherited;
  if (not MessageDone) and
     (Player <> nil) and
     PointInside(Player.Position) then
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
  ARequiredResources: T3DResourceList;
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

  { Assign Camera, knowing MainScene and APlayer.
    We need to assign Camera early, as initial Camera also is used
    when placing initial creatures on the level (to determine their
    gravity up, initial direciton etc.) }
  procedure InitializeCamera;
  var
    InitialPosition: TVector3Single;
    InitialDirection: TVector3Single;
    InitialUp: TVector3Single;
    GravityUp: TVector3Single;
    CameraRadius: Single;
    NavigationNode: TNavigationInfoNode;
    NavigationSpeed: Single;
    WalkCamera: TWalkCamera;
  begin
    MainScene.GetPerspectiveViewpoint(InitialPosition,
      InitialDirection, InitialUp, GravityUp);

    NavigationNode := MainScene.NavigationInfoStack.Top as TNavigationInfoNode;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      CameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      CameraRadius := MainScene.BoundingBox.AverageSize(false, 1) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      FCameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      FCameraPreferredHeight := CameraRadius * 5;
    CorrectPreferredHeight(FCameraPreferredHeight, CameraRadius,
      DefaultCrouchHeight, DefaultHeadBobbing);

    if NavigationNode <> nil then
      NavigationSpeed := NavigationNode.FdSpeed.Value else
      NavigationSpeed := 1.0;

    { Fix InitialDirection length, and set MoveXxxSpeed.

      We want to have horizontal and vertical speeds controlled independently,
      so we just normalize InitialDirection and set speeds in appropriate
      MoveXxxSpeed. }
    NormalizeTo1st(InitialDirection);
    FMoveHorizontalSpeed := NavigationSpeed;
    FMoveVerticalSpeed := 20;

    { Check and fix GravityUp. }
    if not VectorsEqual(Normalized(GravityUp),
             Vector3Single(0, 0, 1), 0.001) then
      raise EInternalError.CreateFmt(
        'Gravity up vector must be +Z, but is %s',
        [ VectorToRawStr(Normalized(GravityUp)) ]) else
      { Make GravityUp = (0, 0, 1) more "precisely" }
      GravityUp := Vector3Single(0, 0, 1);

    if Player <> nil then
      WalkCamera := GamePlay.Player.Camera else
      { Camera suitable for background level and castle-view-level.
        For actual game, camera will be taken from APlayer.Camera. }
      WalkCamera := TWalkCamera.Create(Self);

    Camera := WalkCamera;

    WalkCamera.Init(InitialPosition, InitialDirection,
      InitialUp, GravityUp, CameraPreferredHeight, CameraRadius);
    WalkCamera.CancelFallingDown;
  end;

const
  SectorsMargin = 0.5;
var
  Options: TPrepareResourcesOptions;
  NewCameraBox: TBox3D;
  SI: TShapeTreeIterator;
begin
  inherited Create(nil);

  Player := GamePlay.Player;

  UseGlobalLights := true;
  ApproximateActivation := true;
  Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);

  FName := AName;
  FSceneFileName := ASceneFileName;
  FTitle := ATitle;
  FTitleHint := ATitleHint;
  FNumber := ANumber;
  FMenuBackground := AMenuBackground;
  FRequiredResources := T3DResourceList.Create(false);
  FRequiredResources.Assign(ARequiredResources);

  if not DebugNoCreatures then
    RequireCreatures(BaseLights, FRequiredResources);

  Progress.Init(1, 'Loading level "' + Title + '"');
  try
    MainScene := TCastleScene.CreateCustomCache(Self, GLContextCache);
    MainScene.Load(SceneFileName);

    { initialize FAnimationTime. Must be initialized before creating creatures. }
    FAnimationTime := 0.0;

    AttributesSet(MainScene.Attributes);
    MainScene.Attributes.UseSceneLights := true;
    if BumpMapping then
      MainScene.Attributes.BumpMapping := bmBasic else
      MainScene.Attributes.BumpMapping := bmNone;
    MainScene.Attributes.UseOcclusionQuery := UseOcclusionQuery;

    { Scene must be the first one on Items, this way MoveAllowed will
      use Scene for wall-sliding (see T3DList.MoveAllowed implementation). }
    Items.Add(MainScene);

    if Player <> nil then
      Items.Add(Player);

    LoadFromDOMElement(DOMElement);

    ChangeLevelScene;

    InitializeCamera;

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

    MainScene.CastShadowVolumes := SceneDynamicShadows;

    { calculate Options for PrepareResources }
    Options := [prRender, prBackground, prBoundingBox];
    if RenderShadowsPossible and SceneDynamicShadows then
      Options := Options + prShadowVolume;

    MainScene.PrepareResources(Options, false, BaseLights);

    MainScene.FreeResources([frTextureDataInNodes]);

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

  { Needed for sick projection effect, that potentially updates
    DistortFieldOfViewY and such every frame. }
  AlwaysApplyProjection := true;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunderEffect);
  FreeAndNil(FSectors);
  FreeAndNil(FWaypoints);
  FreeAndNil(FCreatures);
  if (FRequiredResources <> nil) and not DebugNoCreatures then
    UnRequireCreatures(FRequiredResources);
  FreeAndNil(FRequiredResources);
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

    if not DOMGetAttribute(Element, 'name', Result.FId) then
      MissingRequiredAttribute('name', 'area');
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
    Resource: T3DResource;
    ItemKind: TItemKind;
    IgnoredBegin, ItemQuantityBegin: Integer;
    ItemKindQuantity, ItemKindId: string;
    ItemQuantity: Cardinal;
    ItemStubBoundingBox: TBox3D;
    ItemPosition: TVector3Single;
  begin
    { Calculate ItemKindQuantity }
    IgnoredBegin := Pos('_', ItemNodeName);
    if IgnoredBegin = 0 then
      ItemKindQuantity := ItemNodeName else
      ItemKindQuantity := Copy(ItemNodeName, 1, IgnoredBegin - 1);

    { Calculate ItemKindId, ItemQuantity }
    ItemQuantityBegin := CharsPos(['0'..'9'], ItemKindQuantity);
    if ItemQuantityBegin = 0 then
    begin
      ItemKindId := ItemKindQuantity;
      ItemQuantity := 1;
    end else
    begin
      ItemKindId := Copy(ItemKindQuantity, 1, ItemQuantityBegin - 1);
      ItemQuantity := StrToInt(SEnding(ItemKindQuantity, ItemQuantityBegin));
    end;

    { Hack for castle 1: item kind name 'Arrow' should be interpreted as
      'Quiver', to not mess with creature kind 'Arrow'.
      We do it this way, since there's no way anymore to edit and reexport
      castle VRML 1.0 blend files. }
    if ItemKindId = 'Arrow' then
      ItemKindId := 'Quiver';

    Resource := ItemsKinds.FindId(ItemKindId);
    if not (Resource is TItemKind) then
      raise Exception.CreateFmt('Resource "%s" is not an item, but is referenced in model with Item prefix',
        [ItemKindId]);
    ItemKind := TItemKind(Resource);

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
    Resource: T3DResource;
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
    Resource := CreaturesKinds.FindId(CreatureKindName);
    if not (Resource is TCreatureKind) then
      raise Exception.CreateFmt('Resource "%s" is not a creature, but is referenced in model with Crea prefix',
        [CreatureKindName]);
    CreatureKind := TCreatureKind(Resource);
    { The creature kind may be unprepared here only because
      --debug-no-creatures was specified. In this case, leave this
      creature kind unprepared and don't add this creature. }
    if not CreatureKind.Prepared then
    begin
      Assert(DebugNoCreatures);
      Exit;
    end;

    { calculate CreatureDirection }
    { TODO --- CreatureDirection configurable.
      Right now, it just points to the player start pos --- this is
      more-or-less sensible, usually. }
    CreatureDirection := VectorSubtract(Camera.GetPosition, CreaturePosition);
    if not CreatureKind.Flying then
      MakeVectorsOrthoOnTheirPlane(CreatureDirection, GravityUp);

    { make sure that MaxLife is initialized now }
    if not IsMaxLife then
    begin
      IsMaxLife := true;
      MaxLife := CreatureKind.DefaultMaxLife;
    end;

    CreatureKind.CreateCreature(Items, CreaturePosition, CreatureDirection, MaxLife);
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
  AttributesSet(Result.Attributes);

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

  AttributesSet(Result.Attributes);

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

procedure TLevel.ApplyProjection;
var
  S, C: Extended;
begin
  Assert(Camera <> nil, 'TLevel always creates camera in constructor');

  ShadowVolumesDraw := DebugRenderShadowVolume;
  ShadowVolumesPossible := RenderShadowsPossible;
  ShadowVolumes := RenderShadows;

  DistortFieldOfViewY := 1;
  DistortViewAspect := 1;
  if SickProjection then
  begin
    SinCos(AnimationTime * SickProjectionSpeed, S, C);
    DistortFieldOfViewY += C * 0.03;
    DistortViewAspect += S * 0.03;
  end;

  inherited;
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
