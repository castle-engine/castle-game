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
unit CastleRequiredResources;

interface

uses Classes, DOM, CastleCreatures;

type
  { We have three different memory behaviors. }
  TResourcesStrategy = (
    { This means that resources are preferably loaded all at once, and kept
      in memory.

      Advantage: you have to wait only once, then everything
      is fast. Disadvantage: huge memory use throughout whole program. }
    rsKeepAllLoaded,
    { Resources are loaded only for current level, when level changes
      resources no longer needed are freed.

      Advantage: you have to wait
      only at loading level time, and memory use moderate.
      Disadvantage: every level load needs to initialize it's creatures.
      If you frequently change between game levels, this means that you
      have to load each time. }
    rsKeepOnlyForCurrentLevel,

    { Resources are only loaded when creature instance is explicitly
      created... this is only for debug purposes, set by --debug-no-creatures. }
    rsDebugKeepOnlyForExistingItems
  );

var
  { Current resource strategy.

    Doesn't change during game run. Initializeed from
    ConserveResources and --debug-no-creatures presence. }
  ResourcesStrategy: TResourcesStrategy;

const
  DefaultConserveResources = true;

var
  { Should we conserve memory by keeping only the required creatures
    for current level ?

    Actually, all RequireCreatures / UnRequireCreatures mechanism
    works the same, regardless of this setting. But if this is
    @false, then RequireAllCreatures should be called at first new game
    start, and in effect, all creatures will always have RequiredCount > 0. }
  ConserveResources: boolean;

{ These increment/decrement RequiredCount for creatures on given list.

  It's used to implement ResourcesStrategy = rsKeepOnlyForCurrentLevel case.
  Ignored for other ResourcesStrategy values. (Although actually this could
  be left working even when ResourcesStrategy = rsKeepAllLoaded, it's no harm
  in this case.)

  @groupBegin }
procedure RequireCreatures(Names: TStringList);
procedure UnRequireCreatures(Names: TStringList);
{ @groupEnd }

{ This requires all creatures, incrementing their RequiredCount by 1.
  UnRequire unrequires all creatures, decrementing their RequiredCount by 1.

  It's used to implement ResourcesStrategy = rsKeepAllLoaded case.
  Ignored for other ResourcesStrategy values.

  They are implemented as "saturate" operations. Contrary to normal
  RequireCreatures, UnRequireCreatures that depend on suming the RequiredCount.
  Which means that calling RequireAllCreatures twice in a row does nothing,
  calling UnRequireAllCreatures without calling RequireAllCreatures does nothing
  etc. We have internal variable whether were in "require all mode", and
  only when this variable changes --- we actually change RequiredCount of all
  creatures.

  This allows you to use these procedures more carelessly, they don't have
  to be "paired" like normal RequireCreatures, UnRequireCreatures.

  @groupBegin }
procedure RequireAllCreatures;
procedure UnRequireAllCreatures;
{ @groupEnd }

{ These increment/decrement RequiredCount only for specified creature.

  It's used to implement ResourcesStrategy = rsKeepOnlyForExistingItems case.
  For ResourcesStrategy = rsKeepOnlyForCurrentLevel this is also needed,
  in case user will use debug menu to add some creature to level
  (that was not listed in level's required resources).
  So these calls are honoured for all ResourcesStrategy values.

  @groupBegin }
procedure RequireCreature(Kind: TCreatureKind);
procedure UnRequireCreature(Kind: TCreatureKind);
{ @groupEnd }

{ Loads a <resources_required> XML element (required child of given
  ParentElement) into a RequiredCreatures list.

  Passs here a created RequiredCreatures instance. It's contents
  will be wiped by this procedure at the beginning. }
procedure LoadRequiredResources(
  ParentElement: TDOMElement;
  RequiredCreatures: TStringList);

implementation

uses SysUtils, KambiLog, ProgressUnit, KambiTimeUtils, CastleConfig,
  KambiXMLUtils;

{ ----------------------------------------------------------------------------
  TCreatureKindFunc and sample implementations of it }

type
  TCreatureKindFunc = function (Kind: TCreatureKind): boolean;

function CreatureKind_Always(Kind: TCreatureKind): boolean;
begin
  Result := true;
end;

var
  KindNames: TStringList;

function CreatureKind_Names(Kind: TCreatureKind): boolean;
begin
  Result := KindNames.IndexOf(Kind.VRMLNodeName) <> -1;
end;

var
  SpecificKind: TCreatureKind;

function CreatureKind_Specific(Kind: TCreatureKind): boolean;
begin
  Result := Kind = SpecificKind;
end;

{ ----------------------------------------------------------------------------
  [Un]RequireCreaturesCore using TCreatureKindFunc }

procedure RequireCreaturesCore(Func: TCreatureKindFunc);
var
  I: Integer;
  Kind: TCreatureKind;
  PrepareRenderSteps: Cardinal;
  TimeBegin: TProcessTimerResult;
begin
  { We iterate two times over Names, first time only to calculate
    PrepareRenderSteps, 2nd time does actual work.
    1st time increments RequiredCount (as 2nd pass may be optimized
    out, if not needed). }

  PrepareRenderSteps := 0;
  for I := 0 to CreaturesKinds.Count - 1 do
  begin
    Kind := CreaturesKinds[I];
    if Func(Kind) then
    begin
      Kind.RequiredCount := Kind.RequiredCount + 1;
      if Kind.RequiredCount = 1 then
      begin
        Assert(not Kind.PrepareRenderDone);
        PrepareRenderSteps += Kind.PrepareRenderSteps;
      end;
    end;
  end;

  if PrepareRenderSteps <> 0 then
  begin
    if Log then
      TimeBegin := ProcessTimerNow;

    Progress.Init(PrepareRenderSteps, 'Loading creatures');
    try
      for I := 0 to CreaturesKinds.Count - 1 do
      begin
        Kind := CreaturesKinds[I];
        if Func(Kind) and (Kind.RequiredCount = 1) then
        begin
          if Log then
            WritelnLog('Resources', Format(
              'Creature "%s" becomes required, loading', [Kind.VRMLNodeName]));
          Kind.PrepareRender;
        end;
      end;
    finally Progress.Fini end;

    if Log then
      WritelnLog('Loading creatures time', Format('%f seconds',
        [ ProcessTimerDiff(ProcessTimerNow, TimeBegin) / ProcessTimersPerSec ]));
  end;
end;

procedure UnRequireCreaturesCore(Func: TCreatureKindFunc);
var
  I: Integer;
  Kind: TCreatureKind;
begin
  for I := 0 to CreaturesKinds.Count - 1 do
  begin
    Kind := CreaturesKinds[I];

    if Func(Kind) then
    begin
      { If everything went OK, I could place here an assertion
        Assert(Kind.RequiredCount > 0);
        However, if creature loading inside RequireCreatures will fail,
        then TLevel destructor is forced to call UnRequireCreatures
        possibly "unrequiring" more than was actually initialized.
        So we have to silently ignore cases when creature is unrequired
        even though it already has "Kind.RequiredCount = 0".
        Still, a correct run of the program (when creature loading goes 100% OK)
        should always have Kind.RequiredCount > 0 here. }

      if Kind.RequiredCount = 0 then Exit;

      Kind.RequiredCount := Kind.RequiredCount - 1;
      if Kind.RequiredCount = 0 then
      begin
        if Log then
          WritelnLog('Resources', Format(
            'Creature "%s" is no longer required, freeing', [Kind.VRMLNodeName]));
        Assert(Kind.PrepareRenderDone);

        Kind.FreePrepareRender;
      end;
    end;
  end;
end;

{ ----------------------------------------------------------------------------
  Public comfortable [Un]RequireCreatures }

procedure RequireCreatures(Names: TStringList);
begin
  if ResourcesStrategy = rsKeepOnlyForCurrentLevel then
  begin
    KindNames := Names;
    RequireCreaturesCore(@CreatureKind_Names);
  end;
end;

procedure UnRequireCreatures(Names: TStringList);
begin
  if ResourcesStrategy = rsKeepOnlyForCurrentLevel then
  begin
    KindNames := Names;
    UnRequireCreaturesCore(@CreatureKind_Names);
  end;
end;

var
  RequireAllMode: boolean = false;

procedure RequireAllCreatures;
begin
  if (ResourcesStrategy = rsKeepAllLoaded) and (not RequireAllMode) then
  begin
    RequireAllMode := true;
    RequireCreaturesCore(@CreatureKind_Always);
  end;
end;

procedure UnRequireAllCreatures;
begin
  if (ResourcesStrategy = rsKeepAllLoaded) and RequireAllMode then
  begin
    RequireAllMode := false;
    UnRequireCreaturesCore(@CreatureKind_Always);
  end;
end;

procedure RequireCreature(Kind: TCreatureKind);
begin
  SpecificKind := Kind;
  RequireCreaturesCore(@CreatureKind_Specific);
end;

procedure UnRequireCreature(Kind: TCreatureKind);
begin
  SpecificKind := Kind;
  UnRequireCreaturesCore(@CreatureKind_Specific);
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


initialization
  ConserveResources := ConfigFile.GetValue(
    'video_options/conserve_resources', DefaultConserveResources);

  if ConserveResources then
    ResourcesStrategy := rsKeepOnlyForCurrentLevel else
    ResourcesStrategy := rsKeepAllLoaded;
finalization
  ConfigFile.SetDeleteValue('video_options/conserve_resources',
    ConserveResources, DefaultConserveResources);
end.
