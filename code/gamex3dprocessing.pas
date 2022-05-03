{
  Copyright 2010-2022 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Process 3D castle models, to add to them VRML/X3D features that couldn't
  be produced by normal 3D modelling programs (like Blender) exporters. }

unit GameX3DProcessing;

interface

uses X3DNodes;

procedure LevelFountainProcess(Node: TX3DNode);

implementation

uses SysUtils,
  X3DFields, CastleVectors, CastleRenderOptions;

{ Find all Appearance nodes using texture with given name,
  and replace then with KambiApperance nodes, adding normalMap field. }
procedure AddNormalMapToTexture(Node: TX3DNode;
  const TextureName, NormalMapName, NormalMapUrl: string); forward;

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
    if (A.Texture <> nil) and
       (A.Texture.X3DName = TextureName) then
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
    E.NormalMap := TImageTextureNode.Create(NormalMapName, Node.BaseUrl);
    E.NormalMap.FdUrl.Items.Add(NormalMapUrl);
    Node.EnumerateReplaceChildren(@E.Enumerate);
    if not E.NormalMapUsed then
      FreeAndNil(E.NormalMap);
  finally FreeAndNil(E) end;
end;

{ level-specific processing -------------------------------------------------- }

procedure LevelFountainProcess(Node: TX3DNode);
begin
  AddNormalMapToTexture(Node, '_016marbre_jpg', '_016marbre_jpg_normalMap', '../../textures/normal_maps/016marbre.png');
  AddNormalMapToTexture(Node, '_012marbre_jpg', '_012marbre_jpg_normalMap', '../../textures/normal_maps/012marbre.png');
end;

end.
