{
  Copyright 2010 Michalis Kamburelis.

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

implementation

uses SysUtils;

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

end.