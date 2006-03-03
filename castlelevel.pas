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

{ TCastleLevel class. }

unit CastleLevel;

interface

uses VRMLFlatSceneGL, VRMLLightSetGL, Boxes3d,
  VRMLNodes, VRMLFields, CastleItems;

type
  TCastleLevel = class
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

    { Used only within constructor.
      We will process the scene graph, and sometimes it's not comfortable
      to remove the items while traversing --- so we will instead
      put them on this list.

      Be careful: never add here two nodes such that one may be parent
      of another, otherwise freeing one could free the other one too
      early. }
    ItemsToRemove: TVRMLNodesList;

    procedure CorrectBlenderTexture2(Node: TVRMLNode);
    procedure TraverseForItems(Node: TVRMLNode; State: TVRMLGraphTraverseState);
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

    { Title of the level, taken from WorldInfo node
      or just basename of ASceneFileName. }
    property Title: string read FTitle;

    { Items lying on the level.
      These Items are owned by level object, so everything remaining
      on this list when we will destroy level will be freed. }
    property Items: TItemsOnLevelList read FItems;
  end;

implementation

uses SysUtils, OpenGLh, KambiUtils, BackgroundGL, MatrixNavigation,
  KambiClassUtils, VectorMath;

constructor TCastleLevel.Create(const ASceneFileName, ALightSetFileName: string);

  function LoadVRMLNode(const FileName: string): TVRMLNode;
  begin
    Result := ParseVRMLFile(ProgramDataPath + 'data' + PathDelim +
      'levels' + PathDelim + FileName, false);
  end;

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
  end;

var
  NavigationNode: TNodeNavigationInfo;
  WorldInfoNode: TNodeWorldInfo;
  LevelBoxIndex: Integer;
begin
  inherited Create;

  FScene := TVRMLFlatSceneGL.Create(
    LoadVRMLNode(ASceneFileName), true, roSeparateShapeStates);
  { TODO -- check later, maybe change GL_LINEAR_MIPMAP_LINEAR
    to something simpler. }
  Scene.Attrib_TextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;

  Scene.RootNode.EnumNodes(TNodeTexture2, CorrectBlenderTexture2, false);
  Scene.ChangedAll;

  { Initialize Items }
  FItems := TItemsOnLevelList.Create;
  ItemsToRemove := TVRMLNodesList.Create;
  try
    Scene.RootNode.TraverseFromDefaultState(TNodeGeneralShape, TraverseForItems);
    RemoveItemsToRemove;
  finally ItemsToRemove.Free end;
  Scene.ChangedAll;

  { Calculate LevelBox.
    Remember that this may change Scene.BoundingBox (in case we remove
    'LevelBox' from Scene. }
  LevelBoxIndex := Scene.ShapeStates.IndexOfShapeWithParentNamed('LevelBox');
  if LevelBoxIndex <> -1 then
  begin
    { When node with name 'LevelBox' is found, then we calculate our
      LevelBox from this node (and we delete 'LevelBox' from the scene,
      as it should not be visible).
      This way we can comfortably set LevelBox from Blender. }
    FLevelBox := Scene.ShapeStates[LevelBoxIndex].BoundingBox;
    Scene.ShapeStates[LevelBoxIndex].ShapeNode.FreeRemovingFromAllParents;
    Scene.ChangedAll;
  end else
  begin
    { Set LevelBox to Scene.BoundingBox, and make maximum Z larger. }
    FLevelBox := Scene.BoundingBox;
    FLevelBox[1, 2] += 4 * (LevelBox[1, 2] - LevelBox[0, 2]);
  end;

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

destructor TCastleLevel.Destroy;
begin
  FreeAndNil(FLightSet);
  FreeAndNil(FScene);
  FreeWithContentsAndNil(FItems);
  inherited;
end;

procedure TCastleLevel.CorrectBlenderTexture2(Node: TVRMLNode);
var
  TextureFileName: TSFString;
begin
  TextureFileName := (Node as TNodeTexture2).FdFileName;
  if IsPrefix('//..', TextureFileName.Value) then
    TextureFileName.Value := SEnding(TextureFileName.Value, 3);
end;

procedure TCastleLevel.TraverseForItems(Node: TVRMLNode;
  State: TVRMLGraphTraverseState);

  procedure CreateNewItem(const ItemNodeName: string);
  var
    ItemKind: TItemKind;
    ItemKindEnd: Integer;
    ItemKindVRMLNodeName, ItemInfo: string;
    ItemStubBoundingBox: TBox3d;
    ItemPosition: TVector3Single;
  begin
    ItemKindEnd := Pos('_', ItemNodeName);
    if ItemKindEnd = 0 then
    begin
      ItemKindVRMLNodeName := ItemNodeName;
      ItemInfo := '';
    end else
    begin
      ItemKindVRMLNodeName := Copy(ItemNodeName, 1, ItemKindEnd - 1);
      ItemInfo := SEnding(ItemNodeName, ItemKindEnd + 1);
    end;

    { ItemInfo is ignored for now, may be used later to encode something }

    ItemKind := ItemKindWithVRMLNodeName(ItemKindVRMLNodeName);
    if ItemKind = nil then
      raise Exception.CreateFmt('Item kind with VRMLNodeName "%s" doesn''t exist',
        [ItemKindVRMLNodeName]);

    ItemStubBoundingBox := (Node as TNodeGeneralShape).BoundingBox(State);
    ItemPosition[0] := (ItemStubBoundingBox[0, 0] + ItemStubBoundingBox[1, 0]) / 2;
    ItemPosition[1] := (ItemStubBoundingBox[0, 1] + ItemStubBoundingBox[1, 1]) / 2;
    ItemPosition[2] := ItemStubBoundingBox[0, 2];

    FItems.Add(TItemOnLevel.Create(TItem.Create(ItemKind), ItemPosition));
  end;

const
  ItemPrefix = 'Item_';
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

end.