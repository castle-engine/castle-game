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

uses KambiUtils, KambiClassUtils, CastleSound, VRMLTriangleOctree, DOM;

{$define read_interface}

type
  TTextureRule = class
  private
    procedure LoadFromDOMElement(Element: TDOMElement);
  public
    BaseName: string;

    HasFootstepsSound: boolean;
    { This is footsteps sound, relevant (and for sure not stNone)
      if HasFootstepsSound. }
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
    GroundRule_Cache: boolean;
    GroundRule_LastGround: POctreeItem;
    GroundRule_LastResult: TTextureRule;
  public
    { Load contents of this object from textures/index.xml file. }
    procedure LoadFromFile;

    { Checks for TTextureRule on this Ground.

      This checks looking at Ground texture and then looking at TTextureRule
      for given texture. In other words: if the appropriate texture
      was specified in textures/index.xml file
      then the appropriate TTextureRule is returned.
      Otherwise @nil is returned.

      Ground = @nil is allowed here (and will always result in @nil
      returned). }
    function GroundRule(Ground: POctreeItem): TTextureRule;
  end;

var
  TextureRulesList: TTextureRulesList;

{$undef read_interface}

implementation

uses SysUtils, XMLRead, KambiXMLUtils, KambiFilesUtils, VRMLNodes;

{$define read_implementation}
{$I objectslist_1.inc}

{ TTextureRule --------------------------------------------------------------- }

procedure TTextureRule.LoadFromDOMElement(Element: TDOMElement);
var
  FootstepsSoundName: string;
  SubElement, LavaDamage: TDOMElement;
  SubElements: TDOMNodeList;
  I: Integer;
begin
  if not DOMGetAttribute(Element, 'base_name', BaseName) then
    raise Exception.Create('<texture> element must have "base_name" attribute');

  HasFootstepsSound := DOMGetAttribute(Element,
    'footsteps_sound', FootstepsSoundName) and
    (FootstepsSoundName <> '');
  if HasFootstepsSound then
    FootstepsSound := SoundFromName(FootstepsSoundName);

  SubElements := Element.ChildNodes;
  try
    for I := 0 to Integer(SubElements.Count) - 1 do
      if SubElements.Item[I].NodeType = ELEMENT_NODE then
      begin
        SubElement := SubElements.Item[I] as TDOMElement;

        if SubElement.TagName = 'lava' then
        begin
          Lava := true;
          LavaDamage := DOMGetOneChildElement(SubElement);
          if (LavaDamage = nil) or (LavaDamage.TagName <> 'damage') then
            raise Exception.Create('Missing <damage> inside <lava> element');
          if not DOMGetSingleAttribute(LavaDamage, 'const', LavaDamageConst) then
            LavaDamageConst := 0;
          if not DOMGetSingleAttribute(LavaDamage, 'random', LavaDamageRandom) then
            LavaDamageRandom := 0;
          if not DOMGetSingleAttribute(LavaDamage, 'time', LavaDamageTime) then
            LavaDamageTime := 0;
        end else
          raise Exception.CreateFmt('Unknown element inside <texture>: "%s"',
            [SubElement.TagName]);
      end;
  finally SubElements.Release; end;
end;

{ TTextureRulesList ---------------------------------------------------------- }

procedure TTextureRulesList.LoadFromFile;
var
  TextureConfig: TXMLDocument;
  TextureElement: TDOMElement;
  TextureElements: TDOMNodeList;
  TextureRule: TTextureRule;
  I: Integer;
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

          TextureRule.LoadFromDOMElement(TextureElement);
        end;
    finally TextureElements.Release; end;
  finally
    SysUtils.FreeAndNil(TextureConfig);
  end;
end;

function TTextureRulesList.GroundRule(Ground: POctreeItem): TTextureRule;
var
  HasTextureUrl: boolean;
  TextureUrl: string;
  I: Integer;
begin
  { Results of GroundRule are cached, since this is very often
    asked with the same Ground pointer. }
  if GroundRule_Cache and
    (GroundRule_LastGround = Ground) then
  begin
    Result := GroundRule_LastResult;
    Exit;
  end;

  Result := nil;

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
  begin
    TextureUrl := DeleteFileExt(ExtractFileName(TextureUrl));

    for I := 0 to Count - 1 do
    begin
      if SameText(Items[I].BaseName, TextureUrl) then
      begin
        Result := Items[I];
        break;
      end;
    end;
  end;

  GroundRule_Cache := true;
  GroundRule_LastGround := Ground;
  GroundRule_LastResult := Result;
end;

initialization
  TextureRulesList := TTextureRulesList.Create;
  TextureRulesList.LoadFromFile;
finalization
  FreeWithContentsAndNil(TextureRulesList);
end.
