{
  Copyright 2007-2012 Michalis Kamburelis.

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

{ }
unit GameRequiredResources;

interface

uses Classes, DOM, GameCreatures, X3DNodes;

{ These increment/decrement RequiredCount for creatures on given list.

  Currently resources are loaded only for current level, when level changes
  resources no longer needed are freed.

  Advantage: you have to wait
  only at loading level time, and memory use is moderate.
  Disadvantage: every level load needs to initialize it's creatures.
  If you frequently change between game levels, this means that you
  have to load each time.

  @groupBegin }
procedure RequireCreatures(const BaseLights: TLightInstancesList; Names: TStringList);
procedure UnRequireCreatures(Names: TStringList);
{ @groupEnd }

procedure RequireCreature(const BaseLights: TLightInstancesList; Kind: TCreatureKind);
procedure UnRequireCreature(Kind: TCreatureKind);

{ Loads a <resources_required> XML element (required child of given
  ParentElement) into a RequiredCreatures list.

  Passs here a created RequiredCreatures instance. It's contents
  will be wiped by this procedure at the beginning. }
procedure LoadRequiredResources(
  ParentElement: TDOMElement;
  RequiredCreatures: TStringList);

var
  DebugNoCreatures: boolean = false;

implementation

uses SysUtils, CastleLog, ProgressUnit, CastleTimeUtils, GameConfig,
  CastleXMLUtils;

{ [Un]RequireCreatures ------------------------------------------------------- }

procedure RequireCreatures(const BaseLights: TLightInstancesList; Names: TStringList);
var
  I: Integer;
  Kind: TCreatureKind;
  PrepareSteps: Cardinal;
  TimeBegin: TProcessTimerResult;
begin
  { We iterate two times over Names, first time only to calculate
    PrepareSteps, 2nd time does actual work.
    1st time increments RequiredCount (as 2nd pass may be optimized
    out, if not needed). }

  PrepareSteps := 0;
  for I := 0 to CreaturesKinds.Count - 1 do
  begin
    Kind := CreaturesKinds[I];
    if Names.IndexOf(Kind.ShortName) <> -1 then
    begin
      Kind.RequiredCount := Kind.RequiredCount + 1;
      if Kind.RequiredCount = 1 then
      begin
        Assert(not Kind.Prepared);
        PrepareSteps += Kind.PrepareSteps;
      end;
    end;
  end;

  if PrepareSteps <> 0 then
  begin
    if Log then
      TimeBegin := ProcessTimerNow;

    Progress.Init(PrepareSteps, 'Loading creatures');
    try
      for I := 0 to CreaturesKinds.Count - 1 do
      begin
        Kind := CreaturesKinds[I];
        if (Names.IndexOf(Kind.ShortName) <> -1) and
           (Kind.RequiredCount = 1) then
        begin
          if Log then
            WritelnLog('Resources', Format(
              'Creature "%s" becomes required, loading', [Kind.ShortName]));
          Kind.Prepare(BaseLights);
        end;
      end;
    finally Progress.Fini end;

    if Log then
      WritelnLog('Loading creatures time', Format('%f seconds',
        [ ProcessTimerDiff(ProcessTimerNow, TimeBegin) / ProcessTimersPerSec ]));
  end;
end;

procedure UnRequireCreatures(Names: TStringList);
var
  I: Integer;
  Kind: TCreatureKind;
begin
  for I := 0 to CreaturesKinds.Count - 1 do
  begin
    Kind := CreaturesKinds[I];

    if Names.IndexOf(Kind.ShortName) <> -1 then
    begin
      Assert(Kind.RequiredCount > 0);

      Kind.RequiredCount := Kind.RequiredCount - 1;
      if Kind.RequiredCount = 0 then
      begin
        if Log then
          WritelnLog('Resources', Format(
            'Creature "%s" is no longer required, freeing', [Kind.ShortName]));

        { If everything went OK, I could place here an assertion

            Assert(Kind.Prepared);

          However, if creature loading inside RequireCreatures will fail,
          then TLevel destructor is forced to call UnRequireCreatures
          on creatures that, although had RequiredCount
          increased, didn't have actually Prepare call.
          Still, a correct run of the program (when creature loading goes 100% OK)
          should always have Kind.RequiredCount > 0 here. }

        if Kind.Prepared then
          Kind.Release;
      end;
    end;
  end;
end;

procedure RequireCreature(const BaseLights: TLightInstancesList; Kind: TCreatureKind);
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.Append(Kind.ShortName);
    RequireCreatures(BaseLights, Names);
  finally FreeAndNil(Names) end;
end;

procedure UnRequireCreature(Kind: TCreatureKind);
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.Append(Kind.ShortName);
    UnRequireCreatures(Names);
  finally FreeAndNil(Names) end;
end;

{ XML stuff ------------------------------------------------------------------ }

procedure LoadRequiredResources(
  ParentElement: TDOMElement;
  RequiredCreatures: TStringList);
var
  RequiredResources, Resource: TDOMElement;
  Children: TDOMNodeList;
  ResourceName: string;
  I: Integer;
  Node: TDOMNode;
begin
  RequiredCreatures.Clear;

  RequiredResources := DOMGetChildElement(ParentElement, 'required_resources',
    true);

  Children := RequiredResources.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
    begin
      Node := Children.Item[I];
      if Node.NodeType = ELEMENT_NODE then
      begin
        Resource := Node as TDOMElement;
        if Resource.TagName <> 'creature' then
          raise Exception.CreateFmt(
            'Element "%s" is not allowed in <required_resources>',
            [Resource.TagName]);
        if not DOMGetAttribute(Resource, 'name', ResourceName) then
          raise Exception.Create('<creature> must have a "name" attribute');
        RequiredCreatures.Append(ResourceName);
      end;
    end;
  finally FreeChildNodes(Children) end;
end;

end.
