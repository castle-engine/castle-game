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
    LoadVRMLNode(ASceneFileName), true, roSeparateShapeStates);
  { TODO -- check later, maybe change GL_LINEAR_MIPMAP_LINEAR
    so something simpler. }
  Scene.Attrib_TextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;

  FCameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.007;
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