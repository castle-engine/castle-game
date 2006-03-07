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
  VRMLTriangleOctree;

type
  TLevel = class
  private
    FScene: TVRMLFlatSceneGL;
    FLightSet: TVRMLLightSetGL;
    FCameraRadius: Single;
    FCameraPreferredHeight: Single;
    FProjectionNear: Single;
    FProjectionFar: Single;
    FNavigationSpeed: Single;
    FTitle: string;
    FLevelBox: TBox3d;
    FItems: TItemsOnLevelList;
    FHeadlight: boolean;

    { Used only within constructor.
      We will process the scene graph, and sometimes it's not comfortable
      to remove the items while traversing --- so we will instead
      put them on this list.

      Be careful: never add here two nodes such that one may be parent
      of another, otherwise freeing one could free the other one too
      early. }
    ItemsToRemove: TVRMLNodesList;

    FHintButtonBox: TBox3d;
    FHintButtonShown: boolean;

    procedure TraverseForItems(Node: TVRMLNode; State: TVRMLGraphTraverseState);

    function LoadVRMLNode(const FileName: string): TVRMLNode;
  protected
    { This will be called from our constructor before initializing
      our octrees. You can override this to do here some operations
      that change the Scene.RootNode (e.g. you can do here tricks like
      extracting some specific objects like HintButtonBox).
      Be very cautious what you do here --- remember that this is called
      while TLevel.Create constructor did not finish it's work yet ! }
    procedure ChangeLevelScene; virtual;
  public
    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this. }
    constructor Create(const ASceneFileName, ALightSetFileName: string);

    destructor Destroy; override;

    property Scene: TVRMLFlatSceneGL read FScene;
    property LightSet: TVRMLLightSetGL read FLightSet;

    property CameraRadius: Single read FCameraRadius;
    property CameraPreferredHeight: Single read FCameraPreferredHeight;
    property ProjectionNear: Single read FProjectionNear;
    property ProjectionFar: Single read FProjectionFar;
    property NavigationSpeed: Single read FNavigationSpeed;

    { Player position should always be within this box. }
    property LevelBox: TBox3d read FLevelBox;

    { HintButtonBox, see README for meaning of this.
      This will be empty if level does not contain HintButtonBox. }
    property HintButtonBox: TBox3d read FHintButtonBox;

    { HintButtonShown is considered to be part of the level.
      This means that player will get second "hint: you can press the button..."
      message on second level --- I think that it's good. }
    property HintButtonShown: boolean
      read FHintButtonShown write FHintButtonShown;

    { Title of the level, taken from WorldInfo node
      or just basename of ASceneFileName. }
    property Title: string read FTitle;

    { Items lying on the level.
      These Items are owned by level object, so everything remaining
      on this list when we will destroy level will be freed. }
    property Items: TItemsOnLevelList read FItems;

    property Headlight: boolean read FHeadlight;

    { MoveAllowed and GetCameraHeight perform collision detection
      with the level.

      @groupBegin }
    function MoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; virtual;

    procedure GetCameraHeight(Navigator: TMatrixWalker;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
      virtual;
    { @groupEnd }

    { Call this to render level things. Frustum is current player's frustum. }
    procedure Render(const Frustum: TFrustum); virtual;

    { Call this to allow level object to update some things,
      animate level objects etc. }
    procedure Idle(const CompSpeed: Single); virtual;

    { Call this when player picked some triangle on the level.
      Distance is the exact distance to picked point. }
    procedure TrianglePicked(const Distance: Single;
      const Item: TOctreeItem); virtual;

    { Override this to allow player to pick some additional objects,
      not contained in @link(Scene). Ray0 and RayVector describe picking
      ray, RayVector is always normalized (i.e. has length 1).
      If there was a pick: set IntersectionDistance and return
      something >= 0. Otherwise return -1.

      Returned index will be passed to SpecialObjectPicked.
      In the future, TLevel may show some property like SpecialObjects
      that will allow to explicitly enumerate special objects. }
    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; virtual;

    { Override this to take some action when some special object was picked.
      This will be called only if you overriden also SpecialObjectsTryPick. }
    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); virtual;
  end;

  TCastleHallLevel = class(TLevel)
  private
    ButtonPressed: boolean;
    AnimationOpenDownRotation: Single;
    AnimationButtonPress: Single;
    Symbol_TL, Symbol_BL, Symbol_TR, Symbol_BR: TVRMLFlatSceneGL;
    Button: TVRMLFlatSceneGL;
  public
    constructor Create;
    destructor Destroy; override;

    function MoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; override;

    procedure GetCameraHeight(Navigator: TMatrixWalker;
      var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
      override;

    procedure Render(const Frustum: TFrustum); override;
    procedure Idle(const CompSpeed: Single); override;

    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;
  end;

procedure SceneCorrectBlenderTexture2(Scene: TVRMLFlatScene);

implementation

uses SysUtils, OpenGLh, KambiUtils, BackgroundGL, KambiClassUtils,
  CastlePlay, KambiGLUtils;

{ TLevel --------------------------------------------------------------------- }

constructor TLevel.Create(const ASceneFileName, ALightSetFileName: string);

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
  end;

  { See README for description of LevelBox and HintButtonBox trick.
    Remember that this may change Scene.BoundingBox (in case we will
    find and remove the node from Scene). }
  function RemoveBoxNode(var Box: TBox3d; const NodeName: string): boolean;
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
      Scene.ShapeStates[BoxNodeIndex].ShapeNode.FreeRemovingFromAllParents;
      Scene.ChangedAll;
    end;
  end;

var
  NavigationNode: TNodeNavigationInfo;
  WorldInfoNode: TNodeWorldInfo;
begin
  inherited Create;

  FScene := TVRMLFlatSceneGL.Create(
    LoadVRMLNode(ASceneFileName), true, roSeparateShapeStates);

  { This is the slowest one, but it looks perfect.
    In fact, it's not so very slow on my system, so I think that
    this can be the default.
    Maybe it the future I'll make some configuration option to control this. }
  Scene.Attrib_TextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;

  SceneCorrectBlenderTexture2(Scene);

  { Initialize Items }
  FItems := TItemsOnLevelList.Create;
  ItemsToRemove := TVRMLNodesList.Create;
  try
    Scene.RootNode.TraverseFromDefaultState(TNodeGeneralShape, TraverseForItems);
    RemoveItemsToRemove;
  finally ItemsToRemove.Free end;
  Scene.ChangedAll;

  ChangeLevelScene;

  { Calculate LevelBox. }
  if not RemoveBoxNode(FLevelBox, 'LevelBox') then
  begin
    { Set LevelBox to Scene.BoundingBox, and make maximum Z larger. }
    FLevelBox := Scene.BoundingBox;
    FLevelBox[1, 2] += 4 * (LevelBox[1, 2] - LevelBox[0, 2]);
  end;

  if not RemoveBoxNode(FHintButtonBox, 'HintButtonBox') then
    FHintButtonBox := EmptyBox3d;

  NavigationNode := Scene.RootNode.TryFindNode(TNodeNavigationInfo, true)
    as TNodeNavigationInfo;

  if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
    FCameraRadius := NavigationNode.FdAvatarSize.Items[0] else
    FCameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.007;

  if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
    FCameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
    FCameraPreferredHeight := FCameraRadius * 5;
  CorrectCameraPreferredHeight(FCameraPreferredHeight, CameraRadius,
    DefaultCrouchHeight);

  if NavigationNode <> nil then
    FNavigationSpeed := NavigationNode.FdSpeed.Value else
    FNavigationSpeed := 1.0;

  if NavigationNode <> nil then
    FHeadlight := NavigationNode.FdHeadlight.Value else
    FHeadlight := false;

  FProjectionNear := CameraRadius * 0.75;
  FProjectionFar := Box3dMaxSize(Scene.BoundingBox) * 5;

  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Building triangle octree');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Building ShapeState octree');

  Scene.BackgroundSkySphereRadius := TBackgroundGL.NearFarToSkySphereRadius
    (ProjectionNear, ProjectionFar);
  Scene.PrepareRender(true);

  FLightSet := TVRMLLightSetGL.Create(LoadVRMLNode(ALightSetFileName),
    true, 1, -1);

  WorldInfoNode := Scene.RootNode.TryFindNode(TNodeWorldInfo, true)
    as TNodeWorldInfo;

  { calculate FTitle }
  FTitle := '';
  if WorldInfoNode <> nil then
    FTitle := WorldInfoNode.FdTitle.Value;
  if FTitle = '' then
    FTitle := ExtractFileName(DeleteFileExt(ASceneFileName));
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FLightSet);
  FreeAndNil(FScene);
  FreeWithContentsAndNil(FItems);
  inherited;
end;

procedure TLevel.ChangeLevelScene;
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadVRMLNode(const FileName: string): TVRMLNode;
begin
  Result := ParseVRMLFile(ProgramDataPath + 'data' + PathDelim +
    'levels' + PathDelim + FileName, false);
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
  for ParentIndex := 0 to Node.ParentsCount - 1 do
  begin
    Parent := Node.Parents[ParentIndex];
    if IsPrefix(ItemPrefix, Parent.NodeName) then
    begin
      CreateNewItem(SEnding(Parent.NodeName, Length(ItemPrefix) + 1));
      { Don't remove Parent now --- will be removed.
        This avoids problems with removing nodes while traversing. }
      ItemsToRemove.Add(Parent);
      Break;
    end;
  end;
end;

function TLevel.MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    Box3dPointInside(ProposedNewPos, LevelBox) and
    Scene.DefaultTriangleOctree.MoveAllowed(
      Navigator.CameraPos, ProposedNewPos, NewPos, CameraRadius);
end;

procedure TLevel.GetCameraHeight(Navigator: TMatrixWalker;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
begin
  Scene.DefaultTriangleOctree.GetCameraHeight(
    Navigator.CameraPos, Navigator.HomeCameraUp,
    IsAboveTheGround, SqrHeightAboveTheGround);
end;

procedure TLevel.Render(const Frustum: TFrustum);
begin
  Scene.RenderFrustumOctree(Frustum);
end;

procedure TLevel.Idle(const CompSpeed: Single);
begin
  { Nothing to do in this class. }
end;

procedure TLevel.TrianglePicked(const Distance: Single; const Item: TOctreeItem);
begin
  { Nothing to do in this class. }
end;

function TLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;
begin
  Result := -1;
end;

procedure TLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
begin
  { Nothing to do in this class. }
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create;

  function LoadSymbol(const Suffix: string): TVRMLFlatSceneGL;
  begin
    Result := TVRMLFlatSceneGL.Create(LoadVRMLNode(
      'castle_hall_symbol_' + Suffix + '.wrl'),
      true, roSeparateShapeStates);

    Result.Attrib_TextureMinFilter := Scene.Attrib_TextureMinFilter;

    SceneCorrectBlenderTexture2(Result);

    Result.DefaultTriangleOctree := Result.CreateTriangleOctree('');
  end;

begin
  inherited Create('castle_hall_final.wrl', 'castle_hall_lights.wrl');

  Symbol_TL := LoadSymbol('tl');
  Symbol_BL := LoadSymbol('bl');
  Symbol_TR := LoadSymbol('tr');
  Symbol_BR := LoadSymbol('br');

  Button := TVRMLFlatSceneGL.Create(LoadVRMLNode('castle_hall_button.wrl'),
    true, roSeparateShapeStates);
  Button.DefaultTriangleOctree := Button.CreateTriangleOctree('');

  AnimationButtonPress := 1.0;
end;

destructor TCastleHallLevel.Destroy;
begin
  FreeAndNil(Button);
  FreeAndNil(Symbol_TL);
  FreeAndNil(Symbol_BL);
  FreeAndNil(Symbol_TR);
  FreeAndNil(Symbol_BR);
  inherited;
end;

function TCastleHallLevel.MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;

  function MakeSymbol(SymbolScene: TVRMLFlatSceneGL): boolean;
  begin
    Result := SymbolScene.DefaultTriangleOctree.MoveAllowedSimple(
      Navigator.CameraPos, NewPos, CameraRadius);
  end;

begin
  Result := inherited MoveAllowed(Navigator,
    ProposedNewPos, NewPos, BecauseOfGravity);

  Result := Result and
    Button.DefaultTriangleOctree.MoveAllowedSimple(
      Navigator.CameraPos, NewPos, CameraRadius);

  if Result and (not ButtonPressed) then
  begin
    Result :=
      MakeSymbol(Symbol_TL) and
      MakeSymbol(Symbol_BL) and
      MakeSymbol(Symbol_TR) and
      MakeSymbol(Symbol_BR);
  end;
end;

procedure TCastleHallLevel.GetCameraHeight(Navigator: TMatrixWalker;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);

  procedure MakeBonusScene(BonusScene: TVRMLFlatSceneGL);
  var
    IsAboveTheBonusScene: boolean;
    SqrHeightAboveTheBonusScene: Single;
  begin
    BonusScene.DefaultTriangleOctree.GetCameraHeight(
      Navigator.CameraPos, Navigator.HomeCameraUp,
      IsAboveTheBonusScene, SqrHeightAboveTheBonusScene);

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
  inherited GetCameraHeight(Navigator, IsAboveTheGround, SqrHeightAboveTheGround);

  if not ButtonPressed then
  begin
    MakeBonusScene(Symbol_TL);
    MakeBonusScene(Symbol_BL);
    MakeBonusScene(Symbol_TR);
    MakeBonusScene(Symbol_BR);
  end;

  MakeBonusScene(Button);
end;

procedure TCastleHallLevel.Render(const Frustum: TFrustum);

  procedure RenderRotated(SymbolScene: TVRMLFlatSceneGL;
    const RotationX, RotationY, TranslationX, TranslationY: Integer);
  const
    SymbolSize = 30;
  begin
    glPushMatrix;
      glTranslatef(+TranslationX * SymbolSize, +TranslationY * SymbolSize, 0);
      glRotatef(AnimationOpenDownRotation, RotationX, RotationY, 0);
      glTranslatef(-TranslationX * SymbolSize, -TranslationY * SymbolSize, 0);
      { SymbolScene BoundingBox doesn't account for transformations abovem
        so we can't do here RenderFrustum, because this could make false
        results (objects could be visible, but RenderFrustum would not
        render it). So below we do normal Render. }
      SymbolScene.Render(nil);
    glPopMatrix;
  end;

  procedure RenderButtonScaled;
  var
    Translation: TVector3Single;
  begin
    Translation[0] := 0;
    Translation[1] := 0;
    Translation[2] := Button.BoundingBox[0, 2];

    glPushMatrix;
      glTranslatev(Translation);
      glScalef(1, 1, AnimationButtonPress);
      glTranslatev(VectorNegate(Translation));
      { Scaling of Button will always make it smaller,
        so it's BoundingBox is OK, even though we do glScale above.
        So we can call RenderFrustum below. }
      Button.RenderFrustum(Frustum);
    glPopMatrix;
  end;

begin
  if ButtonPressed then
  begin
    RenderRotated(Symbol_TL, +1, +1, -1, +1);
    RenderRotated(Symbol_BL, -1, +1, -1, -1);
    RenderRotated(Symbol_BR, -1, -1, +1, -1);
    RenderRotated(Symbol_TR, +1, -1, +1, +1);
  end else
  begin
    Symbol_TL.RenderFrustum(Frustum);
    Symbol_BL.RenderFrustum(Frustum);
    Symbol_TR.RenderFrustum(Frustum);
    Symbol_BR.RenderFrustum(Frustum);
  end;

  if ButtonPressed then
    RenderButtonScaled else
    Button.RenderFrustum(Frustum);

  { Note that we render Symbol before inherited, i.e. before rendering
    real level --- to allow alpha objects on level to be rendered as last. }
  inherited;
end;

procedure TCastleHallLevel.Idle(const CompSpeed: Single);
const
  MaxAnimationOpenDownRotation = 80;
  MinAnimationButtonPress = 0.5;
begin
  inherited;

  if Player.Navigator.CameraPos[2] < -20 then
    LevelFinished(nil);

  if ButtonPressed then
  begin
    if AnimationButtonPress > MinAnimationButtonPress then
    begin
      AnimationButtonPress := Max(MinAnimationButtonPress,
        AnimationButtonPress - 0.02 * CompSpeed);
    end else
    begin
      if AnimationOpenDownRotation < MaxAnimationOpenDownRotation then
        AnimationOpenDownRotation := Min(MaxAnimationOpenDownRotation,
          AnimationOpenDownRotation + 0.1 * CompSpeed);
    end;
  end;
end;

function TCastleHallLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;
begin
  Result := inherited SpecialObjectsTryPick(
    IntersectionDistance, Ray0, RayVector);

  if Button.DefaultTriangleOctree.RayCollision(
    IntersectionDistance, Ray0, RayVector, true, NoItemIndex,
    false) <> NoItemIndex then
  begin
    Result := 0;
  end;
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
            GameMessage('Button is already pressed') else
          begin
            ButtonPressed := true;
            GameMessage('You press the button. Level exit is uncovered');
          end;
        end else
          GameMessage('You see a button. You cannot reach it from here');
      end;
  end;
end;

{ global things -------------------------------------------------------------- }

type
  TDummy = class
    class procedure CorrectBlenderTexture2(Node: TVRMLNode);
  end;

class procedure TDummy.CorrectBlenderTexture2(Node: TVRMLNode);
var
  TextureFileName: TSFString;
begin
  TextureFileName := (Node as TNodeTexture2).FdFileName;
  if IsPrefix('//..', TextureFileName.Value) then
    TextureFileName.Value := SEnding(TextureFileName.Value, 3);
end;

procedure SceneCorrectBlenderTexture2(Scene: TVRMLFlatScene);
begin
  Scene.RootNode.EnumNodes(TNodeTexture2, TDummy.CorrectBlenderTexture2, false);
  Scene.ChangedAll;
end;

end.