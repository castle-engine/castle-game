{
  Copyright 2007 Michalis Kamburelis.

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

{ }
unit CastleTextures;

interface

uses KambiUtils, KambiClassUtils, CastleSound, VRMLTriangleOctree;

{$define read_interface}

type
  TTextureRule = class
    BaseName: string;

    HasFootstepsSound: boolean;
    FootstepsSound: TSoundType;

    Lava: boolean;
    LavaDamageConst: Single;
    LavaDamageRandom: Single;
    LavaDamageTime: Single;
  end;

  TObjectsListItem_1 = TTextureRule;
  {$I objectslist_1.inc}
  TTextureRulesList = class(TObjectsList_1)
  private
    GroundFootstepsSound_Cache: boolean;
    GroundFootstepsSound_LastGround: POctreeItem;
    GroundFootstepsSound_LastSound: TSoundType;
    GroundFootstepsSound_LastResult: boolean;
  public
    { Load contents of this object from textures/index.xml file. }
    procedure LoadFromFile;

    { Checks for footsteps sound on this Ground.

      This checks looking at Ground texture and then looking at TTextureRule
      for given texture. If there is a matching texture rule with
      HasFootstepsSound, then we return @true and it's FootstepsSound.
      In other words: if the appropriate texture was specified
      in textures/index.xml file with @code(footsteps_sound) attribute,
      then the appropriate sound is returned.

      Ground = @nil is allowed here (and will always result in @false
      returned). }
    function GroundFootstepsSound(Ground: POctreeItem;
      out Sound: TSoundType): boolean;
  end;

var
  TextureRulesList: TTextureRulesList;

{$undef read_interface}

implementation

uses SysUtils, DOM, XMLRead, KambiXMLUtils, KambiFilesUtils, VRMLNodes;

{$define read_implementation}
{$I objectslist_1.inc}

procedure TTextureRulesList.LoadFromFile;
var
  TextureConfig: TXMLDocument;
  TextureElement: TDOMElement;
  TextureElements: TDOMNodeList;
  TextureRule: TTextureRule;
  I: Integer;
  BaseName, FootstepsSoundName: string;
begin
  FreeContents;

  ReadXMLFile(TextureConfig, ProgramDataPath + 'data' +
    PathDelim + 'textures' + PathDelim + 'index.xml');
  try
    Check(TextureConfig.DocumentElement.TagName = 'textures',
      'Root node of textures/index.xml must be <textures>');

    TextureElements := TextureConfig.DocumentElement.ChildNodes;
    try
      for I := 0 to TextureElements.Count - 1 do
        if TextureElements.Item[I].NodeType = ELEMENT_NODE then
        begin
          TextureElement := TextureElements.Item[I] as TDOMElement;
          Check(TextureElement.TagName = 'texture',
            'Each child of texture/index.xml root node must be the <texture> element');

          TextureRule := TTextureRule.Create;
          Add(TextureRule);

          if not DOMGetAttribute(TextureElement, 'base_name', BaseName) then
            raise Exception.Create('<texture> element must have "base_name" attribute');
          TextureRule.BaseName := BaseName;

          TextureRule.HasFootstepsSound := DOMGetAttribute(TextureElement,
            'footsteps_sound', FootstepsSoundName) and
            (FootstepsSoundName <> '');
          if TextureRule.HasFootstepsSound then
            TextureRule.FootstepsSound := SoundFromName(FootstepsSoundName);

          { TODO: read Lava properties }
        end;
    finally TextureElements.Release; end;
  finally
    SysUtils.FreeAndNil(TextureConfig);
  end;
end;

function TTextureRulesList.GroundFootstepsSound(Ground: POctreeItem;
  out Sound: TSoundType): boolean;
var
  HasTextureUrl: boolean;
  TextureUrl: string;
  I: Integer;
begin
  { Results of GroundFootstepsSound are cached, since this is very often
    asked with the same Ground pointer. }
  if GroundFootstepsSound_Cache and
    (GroundFootstepsSound_LastGround = Ground) then
  begin
    Sound := GroundFootstepsSound_LastSound;
    Result := GroundFootstepsSound_LastResult;
    Exit;
  end;

  HasTextureUrl := false;

  if Ground <> nil then
  begin
    if Ground^.State.ParentShape <> nil then
    begin
      { VRML 2.0 path }
      if (Ground^.State.ParentShape.Texture <> nil) and
         (Ground^.State.ParentShape.Texture is TNodeImageTexture) then
      begin
        TextureUrl := TNodeImageTexture(
          Ground^.State.ParentShape.Texture).FdUrl.Items[0];
        HasTextureUrl := true;
      end;
    end else
    begin
      { VRML 1.0 path }
      TextureUrl := Ground^.State.LastNodes.Texture2.FdFileName.Value;
      HasTextureUrl := true;
    end;
  end;

  if HasTextureUrl then
    TextureUrl := DeleteFileExt(ExtractFileName(TextureUrl));

  Result := false;
  for I := 0 to Count - 1 do
  begin
    if SameText(Items[I].BaseName, TextureUrl) then
    begin
      Result := Items[I].HasFootstepsSound;
      if Result then
        Sound := Items[I].FootstepsSound;
      break;
    end;
  end;

  GroundFootstepsSound_Cache := true;
  GroundFootstepsSound_LastGround := Ground;
  GroundFootstepsSound_LastSound := Sound;
  GroundFootstepsSound_LastResult := Result;
end;

initialization
  TextureRulesList := TTextureRulesList.Create;
  TextureRulesList.LoadFromFile;
finalization
  FreeWithContentsAndNil(TextureRulesList);
end.
