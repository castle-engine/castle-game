{
  Copyright 2010-2010 Michalis Kamburelis.

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

unit CastleVRMLProcessing;

interface

uses VRMLNodes;

{ Find all Appearance nodes using texture with given name,
  and replace then with KambiApperance nodes, adding normalMap field. }
procedure AddNormalMapToTexture(Node: TVRMLNode;
  const TextureName, NormalMapName, NormalMapUrl: string);

procedure LevelFountainProcess(Node: TVRMLNode);

implementation

uses SysUtils, VRMLFields, VectorMath;

{ AddNormalMapToTexture ------------------------------------------------------ }

type
  TEnumerateAddNormalMapToTexture = class
  public
    TextureName: string;
    NormalMap: TNodeImageTexture;
    NormalMapUsed: boolean;
    procedure Enumerate(ParentNode: TVRMLNode; var Node: TVRMLNode);
  end;

procedure TEnumerateAddNormalMapToTexture.Enumerate(ParentNode: TVRMLNode; var Node: TVRMLNode);
var
  A: TNodeAppearance;
  KA: TNodeKambiAppearance;
begin
  if Node is TNodeAppearance then
  begin
    A := TNodeAppearance(Node);
    if (A.FdTexture.Value <> nil) and
       (A.FdTexture.Value.NodeName = TextureName) then
    begin
      { create KambiApperance, with all props copied from A }
      KA := TNodeKambiAppearance.Create(A.NodeName, A.WWWBasePath);
      KA.FdfillProperties.Assign(A.FdfillProperties);
      KA.FdlineProperties.Assign(A.FdlineProperties);
      KA.Fdmaterial.Assign(A.Fdmaterial);
      KA.Fdshaders.Assign(A.Fdshaders);
      KA.Fdtexture.Assign(A.Fdtexture);
      KA.FdtextureTransform.Assign(A.FdtextureTransform);

      { add NormalMap }
      KA.FdNormalMap.Value := NormalMap;
      NormalMapUsed := true;

      Node := KA;
    end;
  end;
end;

procedure AddNormalMapToTexture(Node: TVRMLNode;
  const TextureName, NormalMapName, NormalMapUrl: string);
var
  E: TEnumerateAddNormalMapToTexture;
begin
  E := TEnumerateAddNormalMapToTexture.Create;
  try
    E.TextureName := TextureName;
    E.NormalMap := TNodeImageTexture.Create(NormalMapName, Node.WWWBasePath);
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
    RootNode: TVRMLNode;
    procedure Handle(Node: TVRMLNode);
  end;

procedure TEnumerateAddShaderToWater.Handle(Node: TVRMLNode);
var
  M: TVRMLNode;
  CS: TNodeComposedShader;
  CM: TNodeImageCubeMapTexture;
  MT: TNodeMovieTexture;
  Part: TNodeShaderPart;
  ShaderCamMatrix: TSFMatrix3f;
  V: TNodeViewpoint;
  Route: TVRMLRoute;
begin
  M := (Node as TNodeAppearance).FdMaterial.Value;
  if (M <> nil) and
     (M is TNodeMaterial_2) and
     (TNodeMaterial_2(M).NodeName = MatName) then
  begin
    CS := TNodeComposedShader.Create('', '');
    CS.NodeName := 'WaterShader';
    (Node as TNodeAppearance).FdShaders.AddItem(CS);
    CS.FdLanguage.Value := 'GLSL';

{    CM := TNodeGeneratedCubeMapTexture.Create('', '');
    CS.AddCustomField(TSFNode.Create(CS, 'envMap', [], CM));
    CM.FdUpdate.Value := 'NEXT_FRAME_ONLY';
    CM.FdSize.Value := 512;}

    CM := TNodeImageCubeMapTexture.Create('', RootNode.WWWBasePath);
    CS.AddCustomField(TSFNode.Create(CS, 'envMap', [], CM));
    CM.FdUrl.Items.Add('water_reflections/water_environment_map.dds');

    MT := TNodeMovieTexture.Create('', RootNode.WWWBasePath);
    CS.AddCustomField(TSFNode.Create(CS, 'normalMap', [], MT));
    MT.FdUrl.Items.Add('water_reflections/baked_normals_low_res_seamless/baked_normals_%4d.png');
    MT.FdLoop.Value := true;

    ShaderCamMatrix := TSFMatrix3f.Create(CS, 'cameraRotationInverseMatrix', IdentityMatrix3Single);
    CS.AddCustomField(ShaderCamMatrix, true);

    Part := TNodeShaderPart.Create('', RootNode.WWWBasePath);
    CS.FdParts.AddItem(Part);
    Part.FdType.Value := 'FRAGMENT';
    Part.FdUrl.Items.Add('water_reflections/water_reflections_normalmap.fs');

    Part := TNodeShaderPart.Create('', RootNode.WWWBasePath);
    CS.FdParts.AddItem(Part);
    Part.FdType.Value := 'VERTEX';
    Part.FdUrl.Items.Add('water_reflections/water_reflections_normalmap.vs');

    V := RootNode.TryFindNode(TNodeViewpoint, true) as TNodeViewpoint;
    if V <> nil then
    begin
      { Add V.NodeName, to allow saving the route to file.
        Usable for "castle-process-3d-model fountain_final.wrl | view3dscene -" }
      if V.NodeName = '' then V.NodeName := 'DefaultViewport';

      Route := TVRMLRoute.Create;
      Route.SetSourceDirectly(V.EventCameraRotationInverseMatrix);
      Route.SetDestinationDirectly(ShaderCamMatrix);
      Route.PositionInParent := 100000; { at the end of the file }

      RootNode.Routes.Add(Route);
    end;
  end;
end;

{ Find Appearance with given material name, fill there "shaders" field
  to make nice water. }
procedure AddShaderToWater(Node: TVRMLNode; const MatName: string);
var
  E: TEnumerateAddShaderToWater;
begin
  E := TEnumerateAddShaderToWater.Create;
  try
    E.MatName := MatName;
    E.RootNode := Node;
    Node.EnumerateNodes(TNodeAppearance, @E.Handle, false);
  finally FreeAndNil(E) end;
end;

{ level-specific processing -------------------------------------------------- }

procedure LevelFountainProcess(Node: TVRMLNode);
begin
  AddNormalMapToTexture(Node, '_016marbre_jpg', '_016marbre_jpg_normalMap', '../../textures/normal_maps/016marbre.png');
  AddNormalMapToTexture(Node, '_012marbre_jpg', '_012marbre_jpg_normalMap', '../../textures/normal_maps/012marbre.png');
  AddShaderToWater(Node, 'MA_MatWater');
end;

end.