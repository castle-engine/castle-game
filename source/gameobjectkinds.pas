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
  GameVideoOptions, CastleSceneCore, CastleScene, X3DNodes;

type
  { This is a common class for item kind and creature kind. }
  TObjectKind = class
  private
    FShortName: string;
    FPrepareRenderDone: boolean;
    FBlendingType: TBlendingType;
  protected
    { Create Anim from given AnimationFile, only if Anim = nil.
      Then it sets attributes for the animation and then prepares
      the animation by calling animation's @noAutoLink(PrepareRender).

      So this may be helpful to use in PrepareRenderInside implementations.

      It calls Progress.Step 2 times.

      @param(AnimationName is here only for debug purposes (it may be used
      by some debug messages etc.)) }
    procedure CreateAnimationIfNeeded(
      const AnimationName: string;
      var Anim: TCastlePrecalculatedAnimation;
      AnimationFile: string;
      Options: TPrepareResourcesOptions;
      const BaseLights: TLightInstancesList);

    { Read animation filename, reading from XML file KindsConfig.
      The path of responsible XML attribute
      depends on ShortName and given AnimationName.

      If EmptyIfNoAttribute, then this will just set AnimationFile to ''
      if appropriate XML attribute not found. Otherwise
      (when EmptyIfNoAttribute = @false, this is default),
      error will be raised.

      @param(AnimationName determines the XML attribute name, so it must
        be a valid part of XML name) }
    procedure AnimationFromConfig(var AnimationFile: string;
      KindsConfig: TCastleConfig; const AnimationName: string;
      EmptyIfNoAttribute: boolean = false); virtual;

    { Prepare anything needed when starting new game.
      It must call Progress.Step PrepareRenderSteps times.
      It has a funny name to differentiate from PrepareRender,
      that should be called outside. }
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); virtual;
  public
    constructor Create(const AShortName: string);

    procedure PrepareRender(const BaseLights: TLightInstancesList);

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

      In this class this just sets PrepareRenderDone to @false. }
    procedure FreePrepareRender; virtual;

    { Free any association with current OpenGL context. }
    procedure GLContextClose; virtual;

    { Unique identifier of this creature kind.
      Used to refer to this kind from VRML/X3D models, XML files and other data.

      This must be composed of only letters, use CamelCase.
      (Reason: This must be a valid identifier in all possible languages.
      Also digits and underscore are reserved, as we may use them internally
      to other other info in VRML/X3D and XML node names.) }
    property ShortName: string read FShortName;

    property BlendingType: TBlendingType
      read FBlendingType write FBlendingType default DefaultBlendingType;

    procedure LoadFromFile(KindsConfig: TCastleConfig); virtual;

    { This is a debug command, will cause FreePrepareRender
      and then (wrapped within Progress.Init...Fini) will
      call PrepareRender. This should reload / regenerate all
      things prepared in PrepareRender. }
    procedure RedoPrepareRender(const BaseLights: TLightInstancesList);
  end;

implementation

uses SysUtils, ProgressUnit, X3DLoad, DOM, GameWindow,
  CastleStringUtils, CastleLog, CastleFilesUtils, PrecalculatedAnimationCore;

constructor TObjectKind.Create(const AShortName: string);
begin
  inherited Create;
  FShortName := AShortName;
  FBlendingType := DefaultBlendingType;
end;

procedure TObjectKind.PrepareRender(const BaseLights: TLightInstancesList);
var
  I: Integer;
begin
  FPrepareRenderDone := true;

  { call this to satisfy Progress.Step = 1 in this class. }
  Progress.Step;

  PrepareRenderInternal(BaseLights);
end;

procedure TObjectKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  { Nothing to do here in this class. }
end;

function TObjectKind.PrepareRenderSteps: Cardinal;
begin
  Result := 1;
end;

procedure TObjectKind.FreePrepareRender;
begin
  FPrepareRenderDone := false;
end;

procedure TObjectKind.GLContextClose;
begin
  { Nothing to do in this class. }
end;

procedure TObjectKind.LoadFromFile(KindsConfig: TCastleConfig);
const
  SBlendingTypeIncrease = 'increase';
  SBlendingTypeScale = 'scale';
var
  SBlendingType: string;
begin
  SBlendingType := KindsConfig.GetValue(ShortName + '/blending_type',
    SBlendingTypeIncrease);
  if SBlendingType = SBlendingTypeIncrease then
    BlendingType := btIncrease else
  if SBlendingType = SBlendingTypeScale then
    BlendingType := btScale else
    raise Exception.CreateFmt('Wrong blending_type value "%s"', [SBlendingType]);
end;

procedure TObjectKind.RedoPrepareRender(const BaseLights: TLightInstancesList);
begin
  Progress.Init(PrepareRenderSteps, 'Loading object ' + ShortName);
  try
    { It's important to do FreePrepareRender after Progress.Init.
      Why ? Because Progress.Init does TCastleWindowBase.SaveScreeToDisplayList,
      and this may call Window.OnDraw, and this may want to redraw
      the object (e.g. if creature of given kind already exists
      on the screen) and this requires PrepareRender to be already done.

      So we should call Progress.Init before we invalidate PrepareRender
      work. }
    FreePrepareRender;

    PrepareRender(BaseLights);
  finally Progress.Fini; end;
end;

procedure TObjectKind.CreateAnimationIfNeeded(
  const AnimationName: string;
  var Anim: TCastlePrecalculatedAnimation;
  AnimationFile: string;
  Options: TPrepareResourcesOptions;
  const BaseLights: TLightInstancesList);
begin
  if (AnimationFile <> '') and (Anim = nil) then
  begin
    Anim := TCastlePrecalculatedAnimation.CreateCustomCache(nil, GLContextCache);
    Anim.LoadFromFile(AnimationFile, { AllowStdIn } false, { LoadTime } true,
      { rescale scenes_per_time }
      AnimationScenesPerTime / DefaultKAnimScenesPerTime);
  end;
  Progress.Step;

  if Anim <> nil then
  begin
    { Write info before PrepareRender, otherwise it could not
      be available after freeing scene RootNodes in Anim.PrepareRender. }
    if Log then
      WritelnLog('Animation info',
        Format('%40s %3d scenes * %8d triangles',
        [ ShortName + '.' + AnimationName + ' animation: ',
          Anim.ScenesCount,
          Anim.Scenes[0].TrianglesCount(true) ]));

    AnimationAttributesSet(Anim.Attributes, BlendingType);
    Anim.PrepareResources(Options, false, BaseLights);
  end;
  Progress.Step;
end;

procedure TObjectKind.AnimationFromConfig(var AnimationFile: string;
  KindsConfig: TCastleConfig; const AnimationName: string;
  EmptyIfNoAttribute: boolean);
var
  FileName: string;
begin
  AnimationFile := '';

  FileName := KindsConfig.GetValue(ShortName + '/' + AnimationName + '_animation', '');
  if FileName = '' then
  begin
    if not EmptyIfNoAttribute then
      raise Exception.CreateFmt('Missing "%s_animation" for object "%s"',
        [AnimationName, ShortName]);
  end else
  begin
    AnimationFile := CombinePaths(ExtractFilePath(KindsConfig.FileName), FileName);
  end;
end;

end.