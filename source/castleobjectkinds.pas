{
  Copyright 2006,2007 Michalis Kamburelis.

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
unit CastleObjectKinds;

interface

uses Classes, KambiXMLCfg, VRMLGLAnimation, VRMLGLAnimationInfo,
  CastleVideoOptions, VRMLScene, VRMLGLScene;

type
  { This is a common class for item kind and creature kind. }
  TObjectKind = class
  private
    FVRMLNodeName: string;
    FPrepareRenderDone: boolean;
    FBlendingType: TBlendingType;
    { This is internal for PrepareRender. }
    AnimationsPrepared: TVRMLGLAnimationsList;
  protected
    { Use this in PrepareRender to share RootNodes[0]
      of your animations in subclasses. In our destructor and FreePrepareRender
      we will free and clear objects on this list. }
    FirstRootNodesPool: TStringList;

    ManifoldEdgesPool: TStringList;

    { This creates Anim from given AnimInfo, only if Anim = nil.
      Then it sets attributes for the animation and then prepares
      the animation by calling animation's @noAutoLink(PrepareRender).

      So this may be helpful to use in PrepareRenderInside implementations.

      It uses FirstRootNodesPool. It calls Progress.Step 2 times.

      @param(AnimationName is here only for debug purposes (it may be used
      by some debug messages etc.)) }
    procedure CreateAnimationIfNeeded(
      const AnimationName: string;
      var Anim: TVRMLGLAnimation;
      AnimInfo: TVRMLGLAnimationInfo;
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions);

    { Add AnimInfo.ModelFileNames[0] to FirstRootNodesPool.
      AnimInfo may be @nil, then this is ignored. }
    procedure AddFirstRootNodesPool(AnimInfo: TVRMLGLAnimationInfo);

    { Similar to AddFirstRootNodesPool, this allows us to share
      ManifoldEdges / BorderEdges instances between different animations,
      if they start from the same VRML scene. }
    procedure AddManifoldEdgesPool(AnimInfo: TVRMLGLAnimationInfo;
      AnimationToShareEdges: TVRMLGLAnimation);

    { Create AnimInfo instance, reading animation properties from
      XML file KindsConfig. The path of responsible XML element
      depends on VRMLNodeName and given AnimationName.

      If NilIfNoElement, then this will just set AnimInfo to @nil
      if appropriate XML element file not found. Otherwise
      (when NilIfNoElement = @false, this is default),
      error will be raised.

      @param(AnimationName determines the XML element name, so it must
        be a valid part of XML name) }
    procedure AnimationFromConfig(var AnimInfo: TVRMLGLAnimationInfo;
      KindsConfig: TKamXMLConfig; const AnimationName: string;
      NilIfNoElement: boolean = false); virtual;

    { Prepare anything needed when starting new game.
      It must call Progress.Step PrepareRenderSteps times.
      It has a funny name to differentiate from PrepareRender,
      that should be called outside. }
    procedure PrepareRenderInternal; virtual;
  public
    constructor Create(const AVRMLNodeName: string);
    destructor Destroy; override;

    procedure PrepareRender;

    { How many times Progress.Step will be called during PrepareRender
      of this object.

      In this class this returns 1 and PrepareRender will actually do one
      dummy Progress.Step call. That's because this must be > 0,
      some code depends on it, and will optimize out (i.e. not call)
      PrepareRender if sum of some PrepareRenderSteps will be 0. }
    function PrepareRenderSteps: Cardinal; virtual;

    { Are we between PrepareRender and FreePrepareRender. }
    property PrepareRenderDone: boolean read FPrepareRenderDone;

    { This should release everything done by PrepareRender.

      When such thing should be called ? E.g. because PrepareRender
      must be done once again, because some attributes (e.g. things
      set by AnimationAttributesSet) changed.

      In this class this just sets PrepareRenderDone to @false,
      and takes care of clearing FirstRootNodesPool. }
    procedure FreePrepareRender; virtual;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;

    { This will be used to refer to this kind from VRML models
      (or some other places too).

      This should be a valid VRML node name.
      Also, this mustn't contain '_' or '0' ... '9' chars (we may use them
      internally to encode other info in the same VRML node name)
      --- so actually this should only contain letters, 'a'..'z' and 'A'..'Z'.
      Make this in 'CamelCase' to be consistent. }
    property VRMLNodeName: string read FVRMLNodeName;

    property BlendingType: TBlendingType
      read FBlendingType write FBlendingType default DefaultBlendingType;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); virtual;

    { This is a debug command, will cause FreePrepareRender
      and then (wrapped within Progress.Init...Fini) will
      call PrepareRender. This should reload / regenerate all
      things prepared in PrepareRender. }
    procedure RedoPrepareRender;
  end;

implementation

uses SysUtils, ProgressUnit, Object3dAsVRML, DOM, CastleWindow,
  KambiStringUtils, KambiLog;

constructor TObjectKind.Create(const AVRMLNodeName: string);
begin
  inherited Create;
  FVRMLNodeName := AVRMLNodeName;
  FBlendingType := DefaultBlendingType;
  FirstRootNodesPool := TStringList.Create;
  ManifoldEdgesPool := TStringList.Create;
  AnimationsPrepared := TVRMLGLAnimationsList.Create;
end;

destructor TObjectKind.Destroy;
var
  I: Integer;
begin
  for I := 0 to FirstRootNodesPool.Count - 1 do
  begin
    FirstRootNodesPool.Objects[I].Free;
    FirstRootNodesPool.Objects[I] := nil;
  end;
  FreeAndNil(FirstRootNodesPool);

  { No need to free ManifoldEdgesPool objects --- they are animations,
    will be freed from other places (usually from FreePrepareRender). }
  FreeAndNil(ManifoldEdgesPool);

  FreeAndNil(AnimationsPrepared);

  inherited;
end;

procedure TObjectKind.PrepareRender;
var
  I: Integer;
begin
  FPrepareRenderDone := true;

  { call this to satisfy Progress.Step = 1 in this class. }
  Progress.Step;

  Assert(AnimationsPrepared.Count = 0);

  try
    PrepareRenderInternal;

    { During AnimationsPrepared we collect prepared animations,
      to now handle them in general.

      For now, we just call FreeResources on them. It would be bad
      to call Anim.FreeResources inside CreateAnimationIfNeeded,
      right after preparing animation, since then animations for
      the same object kind would not share the texture image
      (in ImagesCache). And usually the same texture image is used
      in all animations.

      On the other hand, we want to call FreeResources on them at some
      time, to free memory. }

    for I := 0 to AnimationsPrepared.Count - 1 do
    begin
      { I could add here frRootNode.

        There were weird problems for 0.7.0
        with this (most probably FPC 2.0.4 bug --- see TODO file
        in revision 2231 about "wrong anim when Alien dying" problem).
        Later, these problems disappeared
        (most probably fixed in FPC 2.2.0, but also many things
        changed since 0.7.0, including some creature loading code).

        But since the invention of "conserve memory" feature,
        frRootNode is not really useful
        anymore, it saves only 6 MB memory for DOOM level (most
        resource-costly level for now). So it's not turned on,
        as it's still risky
        (freeing RootNode always was risky) and has little use.

        Do not free here frTrianglesListShadowCasters
        or frManifoldAndBorderEdges --- we will need them during
        rendering to render shadow volumes. }

      AnimationsPrepared[I].FreeResources([frTextureDataInNodes]);
    end;
  finally
    { keep AnimationsPrepared empty when outside of PrepareRender. }
    AnimationsPrepared.Clear;
  end;
end;

procedure TObjectKind.PrepareRenderInternal;
begin
  { Nothing to do here in this class. }
end;

function TObjectKind.PrepareRenderSteps: Cardinal;
begin
  Result := 1;
end;

procedure TObjectKind.FreePrepareRender;
var
  I: Integer;
begin
  for I := 0 to FirstRootNodesPool.Count - 1 do
  begin
    FirstRootNodesPool.Objects[I].Free;
    FirstRootNodesPool.Objects[I] := nil;
  end;
  FirstRootNodesPool.Clear;

  ManifoldEdgesPool.Clear;

  FPrepareRenderDone := false;
end;

procedure TObjectKind.CloseGL;
begin
  { Nothing to do in this class. }
end;

procedure TObjectKind.LoadFromFile(KindsConfig: TKamXMLConfig);
const
  SBlendingTypeIncrease = 'increase';
  SBlendingTypeScale = 'scale';
var
  SBlendingType: string;
begin
  SBlendingType := KindsConfig.GetValue(VRMLNodeName + '/blending_type',
    SBlendingTypeIncrease);
  if SBlendingType = SBlendingTypeIncrease then
    BlendingType := btIncrease else
  if SBlendingType = SBlendingTypeScale then
    BlendingType := btScale else
    raise Exception.CreateFmt('Wrong blending_type value "%s"', [SBlendingType]);
end;

procedure TObjectKind.RedoPrepareRender;
begin
  Progress.Init(PrepareRenderSteps, 'Loading object ' + VRMLNodeName);
  try
    { It's important to do FreePrepareRender after Progress.Init.
      Why ? Because Progress.Init does TGLWindow.SaveScreeToDisplayList,
      and this may call Glw.OnDraw, and this may want to redraw
      the object (e.g. if creature of given kind already exists
      on the screen) and this requires PrepareRender to be already done.

      So we should call Progress.Init before we invalidate PrepareRender
      work. }
    FreePrepareRender;

    PrepareRender;
  finally Progress.Fini; end;
end;

procedure TObjectKind.AddFirstRootNodesPool(AnimInfo: TVRMLGLAnimationInfo);
var
  FileName: string;
begin
  if AnimInfo <> nil then
  begin
    FileName := AnimInfo.ModelFileNames[0];
    if FirstRootNodesPool.IndexOf(FileName) = -1 then
      FirstRootNodesPool.AddObject(FileName, LoadAsVRML(FileName, false));
  end;
end;

procedure TObjectKind.AddManifoldEdgesPool(AnimInfo: TVRMLGLAnimationInfo;
  AnimationToShareEdges: TVRMLGLAnimation);
var
  FileName: string;
  Index: Integer;
begin
  FileName := AnimInfo.ModelFileNames[0];
  Index := ManifoldEdgesPool.IndexOf(FileName);
  if Index = -1 then
    ManifoldEdgesPool.AddObject(FileName, AnimationToShareEdges) else
    ManifoldEdgesPool.Objects[Index] := AnimationToShareEdges;
end;

procedure TObjectKind.CreateAnimationIfNeeded(
  const AnimationName: string;
  var Anim: TVRMLGLAnimation;
  AnimInfo: TVRMLGLAnimationInfo;
  TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions);
var
  FileName: string;
  FileNameIndex: Integer;
  IsSharedManifoldAndBorderEdges: boolean;
  SharedManifoldEdges: TDynManifoldEdgeArray;
  SharedBorderEdges: TDynBorderEdgeArray;
  ActualOptions: TPrepareRenderOptions;
begin
  if (AnimInfo <> nil) and (Anim = nil) then
    Anim := AnimInfo.CreateAnimation(FirstRootNodesPool);
  Progress.Step;

  if Anim <> nil then
  begin
    { calculate IsSharedManifoldAndBorderEdges, SharedManifoldEdges,
      SharedBorderEdges }
    IsSharedManifoldAndBorderEdges := false;
    if (AnimInfo <> nil) then
    begin
      FileName := AnimInfo.ModelFileNames[0];
      FileNameIndex := ManifoldEdgesPool.IndexOf(FileName);
      if FileNameIndex <> -1 then
      begin
        IsSharedManifoldAndBorderEdges := true;
        SharedManifoldEdges := (ManifoldEdgesPool.Objects[FileNameIndex] as
          TVRMLGLAnimation).ManifoldEdges;
        SharedBorderEdges := (ManifoldEdgesPool.Objects[FileNameIndex] as
          TVRMLGLAnimation).BorderEdges;
      end;
    end;

    { Write info before PrepareRender, otherwise it could not
      be available after freeing scene RootNodes in Anim.PrepareRender. }
    if Log then
      WritelnLog('Animation info',
        Format('%40s %3d scenes * %8d triangles',
        [ VRMLNodeName + '.' + AnimationName + ' animation: ',
          Anim.ScenesCount,
          Anim.Scenes[0].TrianglesCount(true) ]));

    { calculate ActualOptions: Options, but possibly without
      prManifoldAndBorderEdges }
    ActualOptions := Options;
    if IsSharedManifoldAndBorderEdges then
      Exclude(ActualOptions, prManifoldAndBorderEdges);

    AnimationAttributesSet(Anim.Attributes, BlendingType);
    Anim.PrepareRender(TransparentGroups, ActualOptions, false);

    if (prManifoldAndBorderEdges in Options) and
      IsSharedManifoldAndBorderEdges then
      Anim.ShareManifoldAndBorderEdges(SharedManifoldEdges, SharedBorderEdges);

    AnimationsPrepared.Add(Anim);
  end;
  Progress.Step;
end;

procedure TObjectKind.AnimationFromConfig(var AnimInfo: TVRMLGLAnimationInfo;
  KindsConfig: TKamXMLConfig; const AnimationName: string;
  NilIfNoElement: boolean);
var
  Element: TDOMElement;
begin
  FreeAndNil(AnimInfo);

  Element := KindsConfig.PathElement(
    VRMLNodeName + '/' + AnimationName + '_animation/animation');
  if Element = nil then
  begin
    if not NilIfNoElement then
      raise Exception.CreateFmt(
        'No <%s_animation>/<animation> elements for object "%s"',
        [AnimationName, VRMLNodeName]);
  end else
  begin
    AnimInfo := TVRMLGLAnimationInfo.CreateFromDOMElement(
      Element, ExtractFilePath(KindsConfig.FileName),
      GLContextCache);
  end;
end;

end.