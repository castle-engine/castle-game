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
  GameVideoOptions, CastleScene, X3DNodes, Base3D;

type
  { This is a common class for item kind and creature kind. }
  TObjectKind = class
  private
    FShortName: string;
    FPrepared: boolean;
    Resources: T3DListCore;
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
    constructor Create(const AShortName: string);
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
    property ShortName: string read FShortName;

    procedure LoadFromFile(KindsConfig: TCastleConfig); virtual;

    { Release and then immediately prepare again this resource.
      Does progress (by Progress.Init...Fini inside). }
    procedure RedoPrepare(const BaseLights: TLightInstancesList);
  end;

implementation

uses SysUtils, ProgressUnit, DOM, GameWindow,
  CastleStringUtils, CastleLog, CastleFilesUtils, PrecalculatedAnimationCore;

constructor TObjectKind.Create(const AShortName: string);
begin
  inherited Create;
  FShortName := AShortName;
  Resources := T3DListCore.Create(true, nil);
end;

destructor TObjectKind.Destroy;
begin
  Release;
  FreeAndNil(Resources);
  inherited;
end;

procedure TObjectKind.Prepare(const BaseLights: TLightInstancesList);
begin
  FPrepared := true;
end;

function TObjectKind.PrepareSteps: Cardinal;
begin
  Result := 0;
end;

procedure TObjectKind.Release;
begin
  if Resources <> nil then
  begin
    { since Resources owns all it's items, this is enough to free them }
    Resources.Clear;
  end;
  FPrepared := false;
end;

procedure TObjectKind.GLContextClose;
var
  I: Integer;
begin
  for I := 0 to Resources.Count - 1 do
    Resources[I].GLContextClose;
end;

procedure TObjectKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  { Nothing to do in this class. }
end;

procedure TObjectKind.RedoPrepare(const BaseLights: TLightInstancesList);
begin
  Progress.Init(PrepareSteps, 'Loading object ' + ShortName);
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

procedure TObjectKind.PreparePrecalculatedAnimation(
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
        [ ShortName + '.' + AnimationName + ' animation: ',
          Anim.ScenesCount,
          Anim.Scenes[0].TrianglesCount(true) ]));

    AttributesSet(Anim.Attributes);
    Anim.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  end;
  Progress.Step;
end;

procedure TObjectKind.PrepareScene(
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

end.