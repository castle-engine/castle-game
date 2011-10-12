{
  Copyright 2010-2011 Michalis Kamburelis.

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

{ Process 3D castle models, to add to them VRML/X3D features that couldn't
  be produced by normal 3D modelling programs (like Blender) exporters. }

unit GameX3DProcessing;

interface

uses X3DNodes;

{ Find all Appearance nodes using texture with given name,
  and replace then with KambiApperance nodes, adding normalMap field. }
procedure AddNormalMapToTexture(Node: TX3DNode;
  const TextureName, NormalMapName, NormalMapUrl: string);

procedure LevelFountainProcess(Node: TX3DNode);

implementation

uses SysUtils, X3DFields, VectorMath;

{ AddNormalMapToTexture ------------------------------------------------------ }

type
  TEnumerateAddNormalMapToTexture = class
  public
    TextureName: string;
    NormalMap: TImageTextureNode;
    NormalMapUsed: boolean;
    procedure Enumerate(ParentNode: TX3DNode; var Node: TX3DNode);
  end;

procedure TEnumerateAddNormalMapToTexture.Enumerate(ParentNode: TX3DNode; var Node: TX3DNode);
var
  A: TAppearanceNode;
begin
  if Node is TAppearanceNode then
  begin
    A := TAppearanceNode(Node);
    if (A.FdTexture.Value <> nil) and
       (A.FdTexture.Value.NodeName = TextureName) then
    begin
      { add NormalMap }
      A.FdNormalMap.Value := NormalMap;
      NormalMapUsed := true;
    end;
  end;
end;

procedure AddNormalMapToTexture(Node: TX3DNode;
  const TextureName, NormalMapName, NormalMapUrl: string);
var
  E: TEnumerateAddNormalMapToTexture;
begin
  E := TEnumerateAddNormalMapToTexture.Create;
  try
    E.TextureName := TextureName;
    E.NormalMap := TImageTextureNode.Create(NormalMapName, Node.WWWBasePath);
    E.NormalMap.FdUrl.Items.Add(NormalMapUrl);
    Node.EnumerateReplaceChildren(@E.Enumerate);
    if not E.NormalMapUsed then
      FreeAndNil(E.NormalMap);
  finally FreeAndNil(E) end;
end;

{ AddShaderToWater ----------------------------------------------------------- }

type
  TEnumerateAddShaderToWater = class
    MatName: string;
    RootNode: TX3DNode;
    procedure Handle(Node: TX3DNode);
  end;

procedure TEnumerateAddShaderToWater.Handle(Node: TX3DNode);
var
  M: TX3DNode;
  Mat: TMaterialNode;
  CS: TComposedShaderNode;
  CM: TImageCubeMapTextureNode;
  MT: TMovieTextureNode;
  Part: TShaderPartNode;
  ShaderCamMatrix: TSFMatrix3f;
  V: TViewpointNode;
  Route: TX3DRoute;
begin
  M := (Node as TAppearanceNode).FdMaterial.Value;
  if (M <> nil) and
     (M is TMaterialNode) and
     (TMaterialNode(M).NodeName = MatName) then
  begin
    { we could set mat diffuse in Blender and export to VRML,
      but it's easier for now to hardcode it here. }
    Mat := M as TMaterialNode;
    Mat.FdDiffuseColor.Value := Vector3Single(0.5, 0.5, 1.0);

    CS := TComposedShaderNode.Create('', '');
    CS.NodeName := 'WaterShader';
    (Node as TAppearanceNode).FdShaders.Add(CS);
    CS.FdLanguage.Value := 'GLSL';

{    CM := TGeneratedCubeMapTextureNode.Create('', '');
    CS.AddCustomField(TSFNode.Create(CS, 'envMap', [], CM));
    CM.FdUpdate.Value := 'NEXT_FRAME_ONLY';
    CM.FdSize.Value := 512;}

    CM := TImageCubeMapTextureNode.Create('', RootNode.WWWBasePath);
    CS.AddCustomField(TSFNode.Create(CS, 'envMap', [], CM));
    CM.FdUrl.Items.Add('water_reflections/water_environment_map.dds');

    MT := TMovieTextureNode.Create('', RootNode.WWWBasePath);
    CS.AddCustomField(TSFNode.Create(CS, 'normalMap', [], MT));
    MT.FdUrl.Items.Add('water_reflections/baked_normals_low_res_seamless/baked_normals_%4d.png');
    MT.FdLoop.Value := true;

    ShaderCamMatrix := TSFMatrix3f.Create(CS, 'cameraRotationInverseMatrix', IdentityMatrix3Single);
    CS.AddCustomField(ShaderCamMatrix, true);

    Part := TShaderPartNode.Create('', RootNode.WWWBasePath);
    CS.FdParts.Add(Part);
    Part.FdType.Value := 'FRAGMENT';
    Part.FdUrl.Items.Add('water_reflections/water_reflections_normalmap.fs');

    Part := TShaderPartNode.Create('', RootNode.WWWBasePath);
    CS.FdParts.Add(Part);
    Part.FdType.Value := 'VERTEX';
    Part.FdUrl.Items.Add('water_reflections/water_reflections_normalmap.vs');

    V := RootNode.TryFindNode(TViewpointNode, true) as TViewpointNode;
    if V <> nil then
    begin
      { Add V.NodeName, to allow saving the route to file.
        Usable for "castle-process-3d-model fountain_final.wrl | view3dscene -" }
      if V.NodeName = '' then V.NodeName := 'DefaultViewport';

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(V.EventCameraRotationInverseMatrix);
      Route.SetDestinationDirectly(ShaderCamMatrix);
      Route.PositionInParent := 100000; { at the end of the file }

      RootNode.Routes.Add(Route);
    end;
  end;
end;

{ Find Appearance with given material name, fill there "shaders" field
  to make nice water. }
procedure AddShaderToWater(Node: TX3DNode; const MatName: string);
var
  E: TEnumerateAddShaderToWater;
begin
  E := TEnumerateAddShaderToWater.Create;
  try
    E.MatName := MatName;
    E.RootNode := Node;
    Node.EnumerateNodes(TAppearanceNode, @E.Handle, false);
  finally FreeAndNil(E) end;
end;

{ level-specific processing -------------------------------------------------- }

procedure LevelFountainProcess(Node: TX3DNode);
begin
  AddNormalMapToTexture(Node, '_016marbre_jpg', '_016marbre_jpg_normalMap', '../../textures/normal_maps/016marbre.png');
  AddNormalMapToTexture(Node, '_012marbre_jpg', '_012marbre_jpg_normalMap', '../../textures/normal_maps/012marbre.png');
  AddShaderToWater(Node, 'MA_MatWater');
end;

end.