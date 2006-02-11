unit CastleLevel;

interface

uses VRMLFlatSceneGL, VRMLLightSetGL;

type
  TCastleLevel = class
  private
    FScene: TVRMLFlatSceneGL;
    FLightSet: TVRMLLightSetGL;
    FCameraRadius: Single;
    FProjectionNear: Single;
    FProjectionFar: Single;
  public
    property Scene: TVRMLFlatSceneGL read FScene;
    property LightSet: TVRMLLightSetGL read FLightSet;

    property CameraRadius: Single read FCameraRadius;
    property ProjectionNear: Single read FProjectionNear;
    property ProjectionFar: Single read FProjectionFar;

    { Load level from file, create octrees, prepare for OpenGL etc.
      This uses ProgressUnit while loading creating octrees,
      be sure to initialize Progress.UserInterface before calling this. }
    constructor Create(const ASceneFileName, ALightSetFileName: string);

    destructor Destroy; override;
  end;

implementation

uses SysUtils, OpenGLh, KambiUtils, Boxes3d, VRMLNodes, BackgroundGL;

constructor TCastleLevel.Create(const ASceneFileName, ALightSetFileName: string);

  function LoadVRMLNode(const FileName: string): TVRMLNode;
  begin
    Result := ParseVRMLFile(ProgramDataPath + 'data' + PathDelim +
      FileName, false);
  end;

begin
  inherited Create;

  FScene := TVRMLFlatSceneGL.Create(
    LoadVRMLNode(ASceneFileName), true,
    roSeparateShapeStates{roSceneAsAWhole});
  Scene.Attrib_TextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;

  FCameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.015;
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
end;

destructor TCastleLevel.Destroy;
begin
  FreeAndNil(FLightSet);
  FreeAndNil(FScene);
  inherited;
end;

end.