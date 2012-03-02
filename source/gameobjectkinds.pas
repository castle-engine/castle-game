{
  Copyright 2006-2012 Michalis Kamburelis.

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
unit GameObjectKinds;

interface

uses Classes, CastleXMLConfig, PrecalculatedAnimation,
  GameVideoOptions, CastleScene, X3DNodes, Base3D, DOM,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  { Resource used for rendering and processing of 3D objects.
    By itself this doesn't render or do anything.
    But some 3D objects may need to have such resource prepared to work.

    It can also load it's configuration from XML config file.
    For this purpose, it has a unique identifier in @link(Id) property. }
  T3DResource = class
  private
  { Internal design notes: Having resource expressed as
    T3DResource instance, as opposed to overusing dummy T3D instances
    for it, is sometimes good. That's because such resource may be shared by many
    3D objects, may be used for different purposes by various 3D objects
    (e.g. various creatures may be in different state / animation time),
    it's users (3D objects) may not always initially exist on the level
    (e.g. TItem, that is not even T3D, may refer to it), etc.
    There were ideas to unify T3DResource to be like a T3D descendant
    (or ancestor), but they turned out to cause more confusion (special cases,
    special treatment) than the gain from unification (which would
    be no need of Resources list in TCastleSceneManager, simple
    TCastleSceneManager.Items would suffice.) }

    FId: string;
    FPrepared: boolean;
    Resources: T3DListCore;
    FRequiredCount: Cardinal;
  protected
    { Prepare 3D resource loading it from given filename.
      Loads the resource only if filename is not empty,
      and only if it's not already loaded (that is, when Anim = nil).
      Sets rendering attributes and prepares for fast rendering
      and other processing by T3D.PrepareResources.

      Call only in Prepare overrides.

      It calls Progress.Step 2 times.

      Animation or Scene is automatically added to our list of prepared
      3D resources.
      So it's OpenGL resources will be automatically released in
      @link(GLContextClose), it will be fully released
      in @link(Release) and destructor.

      @param(AnimationName is here only for debug purposes (it may be used
      by some debug messages etc.))

      @groupBegin }
    procedure PreparePrecalculatedAnimation(
      const AnimationName: string;
      var Anim: TCastlePrecalculatedAnimation;
      const AnimationFile: string;
      const BaseLights: TLightInstancesList);
    procedure PrepareScene(
      var Scene: TCastleScene;
      const SceneFileName: string;
      const BaseLights: TLightInstancesList);
    { @groupEnd }
  public
    constructor Create(const AId: string); virtual;
    destructor Destroy; override;

    { Prepare everything needed for using this resource.
      It must call Progress.Step PrepareSteps times. }
    procedure Prepare(const BaseLights: TLightInstancesList); virtual;

    { How many times Progress.Step will be called during Prepare
      of this object. In this class this returns 0. }
    function PrepareSteps: Cardinal; virtual;

    { Are we in prepared state, that is after @link(Prepare) call and before @link(Release). }
    property Prepared: boolean read FPrepared;

    { Release everything done by Prepare.

      Useful to call e.g. because Prepare must be done once again,
      because some attributes (e.g. things set by AttributesSet) changed.

      In this class this just sets Prepared to @false. }
    procedure Release; virtual;

    { Free any association with current OpenGL context. }
    procedure GLContextClose; virtual;

    { Unique identifier of this creature kind.
      Used to refer to this kind from VRML/X3D models, XML files and other data.

      This must be composed of only letters, use CamelCase.
      (Reason: This must be a valid identifier in all possible languages.
      Also digits and underscore are reserved, as we may use them internally
      for other info in VRML/X3D and XML node names.) }
    property Id: string read FId;

    procedure LoadFromFile(KindsConfig: TCastleConfig); virtual;

    { Release and then immediately prepare again this resource.
      Does progress (by Progress.Init...Fini inside). }
    procedure RedoPrepare(const BaseLights: TLightInstancesList);

    { Used by Require, UnRequire to count
      how many times this kind is required. Idea is that when this drops
      to zero, we can Release to free resources. }
    property RequiredCount: Cardinal
      read FRequiredCount write FRequiredCount default 0;

    procedure Require(const BaseLights: TLightInstancesList);
    procedure UnRequire;
  end;

  T3DResourceClass = class of T3DResource;

  T3DResourceList = class(specialize TFPGObjectList<T3DResource>)
  public
    { Find resource with given T3DResource.Id.
      @raises Exception if not found. }
    function FindId(const AId: string): T3DResource;

    { Load all items configuration from XML files. }
    procedure LoadFromFile;

    { Prepare all items. This does progress bar rendering
      (using Progress.Init, Progress.Step, Progress.Fini). }
    procedure Prepare(const BaseLights: TLightInstancesList;
      const ResourcesName: string = 'resources');

    { Release all items. }
    procedure Release;

    { Reads <resources_required> XML element. <resources_required> element
      is required child of given ParentElement.
      Sets current list value with all mentioned required
      resources (subset of AllResources). }
    procedure LoadRequiredResources(ParentElement: TDOMElement);

    { Make sure given resource is required.
      Internally, requiring a resource increases it's usage count.
      The actual allocated memory is only released one required count gets back
      to zero.
      @groupBegin }
    procedure Require(const BaseLights: TLightInstancesList;
      const ResourcesName: string = 'resources');
    procedure UnRequire;
    { @groupEnd }
  end;

var
  AllResources: T3DResourceList;

{ Register a class, to allow user to create creatures/items of this class
  by using appropriate type="xxx" inside index.xml file. }
procedure RegisterResourceClass(const AClass: T3DResourceClass; const TypeName: string);

implementation

uses SysUtils, ProgressUnit, GameWindow, CastleXMLUtils, CastleTimeUtils,
  CastleStringUtils, CastleLog, CastleFilesUtils, PrecalculatedAnimationCore;

type
  TResourceClasses = specialize TFPGMap<string, T3DResourceClass>;
var
  ResourceClasses: TResourceClasses;

{ T3DResource ---------------------------------------------------------------- }

constructor T3DResource.Create(const AId: string);
begin
  inherited Create;
  FId := AId;
  Resources := T3DListCore.Create(true, nil);

  { if for some weird reason (when using RTTI), this constructor happens
    before unit finalization --- then make sure to create AllResources first. }
  if AllResources = nil then
    AllResources := T3DResourceList.Create(false);
  AllResources.Add(Self);
end;

destructor T3DResource.Destroy;
begin
  Release;
  FreeAndNil(Resources);

  if AllResources <> nil then
    AllResources.Remove(Self);

  inherited;
end;

procedure T3DResource.Prepare(const BaseLights: TLightInstancesList);
begin
  FPrepared := true;
end;

function T3DResource.PrepareSteps: Cardinal;
begin
  Result := 0;
end;

procedure T3DResource.Release;
begin
  if Resources <> nil then
  begin
    { since Resources owns all it's items, this is enough to free them }
    Resources.Clear;
  end;
  FPrepared := false;
end;

procedure T3DResource.GLContextClose;
var
  I: Integer;
begin
  for I := 0 to Resources.Count - 1 do
    Resources[I].GLContextClose;
end;

procedure T3DResource.LoadFromFile(KindsConfig: TCastleConfig);
begin
  { Nothing to do in this class. }
end;

procedure T3DResource.RedoPrepare(const BaseLights: TLightInstancesList);
begin
  Progress.Init(PrepareSteps, 'Loading ' + Id);
  try
    { It's important to do Release after Progress.Init.
      That is because Progress.Init does TCastleWindowBase.SaveScreenToDisplayList,
      and this may call Window.OnDraw, and this may want to redraw
      the object (e.g. if creature of given kind already exists
      on the screen) and this requires Prepare to be already done.

      So we should call Progress.Init before we make outselves unprepared. }
    Release;

    Prepare(BaseLights);
  finally Progress.Fini; end;
end;

procedure T3DResource.PreparePrecalculatedAnimation(
  const AnimationName: string;
  var Anim: TCastlePrecalculatedAnimation;
  const AnimationFile: string;
  const BaseLights: TLightInstancesList);
begin
  if (AnimationFile <> '') and (Anim = nil) then
  begin
    Anim := TCastlePrecalculatedAnimation.CreateCustomCache(nil, GLContextCache);
    Resources.Add(Anim);
    Anim.LoadFromFile(AnimationFile, { AllowStdIn } false, { LoadTime } true,
      { rescale scenes_per_time }
      AnimationScenesPerTime / DefaultKAnimScenesPerTime);
  end;
  Progress.Step;

  if Anim <> nil then
  begin
    if Log then
      WritelnLog('Animation info',
        Format('%40s %3d scenes * %8d triangles',
        [ Id + '.' + AnimationName + ' animation: ',
          Anim.ScenesCount,
          Anim.Scenes[0].TrianglesCount(true) ]));

    AttributesSet(Anim.Attributes);
    Anim.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  end;
  Progress.Step;
end;

procedure T3DResource.PrepareScene(
  var Scene: TCastleScene;
  const SceneFileName: string;
  const BaseLights: TLightInstancesList);
begin
  if (SceneFileName <> '') and (Scene = nil) then
  begin
    Scene := TCastleScene.CreateCustomCache(nil, GLContextCache);
    Resources.Add(Scene);
    Scene.Load(SceneFileName);
  end;
  Progress.Step;

  if Scene <> nil then
  begin
    AttributesSet(Scene.Attributes);
    Scene.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  end;
  Progress.Step;
end;

procedure T3DResource.Require(const BaseLights: TLightInstancesList);
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.Require(BaseLights);
  finally FreeAndNil(List) end;
end;

procedure T3DResource.UnRequire;
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.UnRequire;
  finally FreeAndNil(List) end;
end;

{ T3DResourceList ------------------------------------------------------------- }

procedure T3DResourceList.Prepare(const BaseLights: TLightInstancesList;
  const ResourcesName: string);
var
  I: Integer;
  PrepareSteps: Cardinal;
begin
  PrepareSteps := 0;
  for I := 0 to Count - 1 do
    PrepareSteps += Items[I].PrepareSteps;

  Progress.Init(PrepareSteps, 'Loading ' + ResourcesName);
  try
    for I := 0 to Count - 1 do
      Items[I].Prepare(BaseLights);
  finally Progress.Fini; end;
end;

procedure T3DResourceList.Release;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Release;
end;

procedure T3DResourceList.LoadFromFile;

  procedure LoadFromPath(const Path: string);
  var
    F: TSearchRec;
    XmlName: string;
    Xml: TCastleConfig;
    ResourceClassName, ResourceId: string;
    ResourceClassIndex: Integer;
    Resource: T3DResource;
  begin
    if FindFirst(Path + '*', faDirectory, F) = 0 then
    repeat
      if F.Attr and faDirectory = faDirectory then
      begin
        XmlName := Path + F.Name + PathDelim + 'index.xml';
        if FileExists(XmlName) then
        begin
          Xml := TCastleConfig.Create(nil);
          try
            Xml.RootName := 'Resource';
            Xml.NotModified; { otherwise changing RootName makes it modified, and saved back at freeing }
            Xml.FileName := XmlName;
            if Log then
              WritelnLog('Resources', Format('Loading T3DResource from "%s"', [XmlName]));
            ResourceClassName := Xml.GetNonEmptyValue('type');
            ResourceId := Xml.GetNonEmptyValue('id');
            ResourceClassIndex := ResourceClasses.IndexOf(ResourceClassName);
            if ResourceClassIndex <> -1 then
            begin
              Resource := ResourceClasses.Data[ResourceClassIndex].Create(ResourceId);
              Add(Resource);
              Resource.LoadFromFile(Xml);
            end else
              raise Exception.CreateFmt('Resource type "%s" not found, mentioned in file "%s"',
                [ResourceClassName, XmlName]);
          finally FreeAndNil(Xml) end;
        end;
      end;
    until FindNext(F) <> 0;
    FindClose(F);
  end;

begin
  LoadFromPath(ProgramDataPath + 'data' + PathDelim + 'creatures' + PathDelim);
  LoadFromPath(ProgramDataPath + 'data' + PathDelim + 'items' + PathDelim);
end;

function T3DResourceList.FindId(const AId: string): T3DResource;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Id = AId then
      Exit;
  end;

  raise Exception.CreateFmt('Not existing resource name "%s"', [AId]);
end;

procedure T3DResourceList.LoadRequiredResources(ParentElement: TDOMElement);
var
  RequiredResources: TDOMElement;
  ResourceName: string;
  I: TXMLElementIterator;
begin
  Clear;

  RequiredResources := DOMGetChildElement(ParentElement, 'required_resources',
    true);

  I := TXMLElementIterator.Create(RequiredResources);
  try
    while I.GetNext do
    begin
      if I.Current.TagName <> 'resource' then
        raise Exception.CreateFmt(
          'Element "%s" is not allowed in <required_resources>',
          [I.Current.TagName]);
      if not DOMGetAttribute(I.Current, 'name', ResourceName) then
        raise Exception.Create('<resource> must have a "name" attribute');
      Add(AllResources.FindId(ResourceName));
    end;
  finally FreeAndNil(I) end;
end;

procedure T3DResourceList.Require(const BaseLights: TLightInstancesList;
  const ResourcesName: string);
var
  I: Integer;
  Resource: T3DResource;
  PrepareSteps: Cardinal;
  TimeBegin: TProcessTimerResult;
  PrepareNeeded: boolean;
begin
  { We iterate two times over Items, first time only to calculate
    PrepareSteps, 2nd time does actual work.
    1st time increments RequiredCount (as 2nd pass may be optimized
    out, if not needed). }

  PrepareSteps := 0;
  PrepareNeeded := false;
  for I := 0 to Count - 1 do
  begin
    Resource := Items[I];
    Resource.RequiredCount := Resource.RequiredCount + 1;
    if Resource.RequiredCount = 1 then
    begin
      Assert(not Resource.Prepared);
      PrepareSteps += Resource.PrepareSteps;
      PrepareNeeded := true;
    end;
  end;

  if PrepareNeeded then
  begin
    if Log then
      TimeBegin := ProcessTimerNow;

    Progress.Init(PrepareSteps, 'Loading ' + ResourcesName);
    try
      for I := 0 to Count - 1 do
      begin
        Resource := Items[I];
        if Resource.RequiredCount = 1 then
        begin
          if Log then
            WritelnLog('Resources', Format(
              'Resource "%s" becomes required, loading', [Resource.Id]));
          Resource.Prepare(BaseLights);
        end;
      end;
    finally Progress.Fini end;

    if Log then
      WritelnLog('Resources', Format('Loading %s time: %f seconds',
        [ ResourcesName,
          ProcessTimerDiff(ProcessTimerNow, TimeBegin) / ProcessTimersPerSec ]));
  end;
end;

procedure T3DResourceList.UnRequire;
var
  I: Integer;
  Resource: T3DResource;
begin
  for I := 0 to Count - 1 do
  begin
    Resource := Items[I];
    Assert(Resource.RequiredCount > 0);

    Resource.RequiredCount := Resource.RequiredCount - 1;
    if Resource.RequiredCount = 0 then
    begin
      if Log then
        WritelnLog('Resources', Format(
          'Creature "%s" is no longer required, freeing', [Resource.Id]));

      { If everything went OK, I could place here an assertion

          Assert(Resource.Prepared);

        However, if resource loading inside Require will fail,
        then scene manager destructor is forced to call UnRequire
        on resources that, although had RequiredCount
        increased, didn't have actually Prepare call.
        Still, a correct run of the program (when resource loading goes 100% OK)
        should always have Resource.RequiredCount > 0 here. }

      if Resource.Prepared then
        Resource.Release;
    end;
  end;
end;

{ resource classes ----------------------------------------------------------- }

procedure RegisterResourceClass(const AClass: T3DResourceClass; const TypeName: string);
begin
  ResourceClasses.KeyData[TypeName] := AClass;
end;

initialization
  AllResources := T3DResourceList.Create(false);
  ResourceClasses := TResourceClasses.Create;
finalization
  FreeAndNil(AllResources);
  FreeAndNil(ResourceClasses);
end.