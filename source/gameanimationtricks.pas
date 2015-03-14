{
  Copyright 2010-2014 Michalis Kamburelis.

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

{ Tricks with precalculated animation for "The Castle". }
unit GameAnimationTricks;

{$I castleconf.inc}

interface

uses Classes, CastlePrecalculatedAnimation, CastleFrustum, CastleVectors,
  CastleGLShaders, Castle3D;

type
  { Animation forced to seamlessly loop by blending the beginning frames
    with end frames. This is a really brutal (often looking bad),
    but universal way to make animation seamlessly loop (works
    even with animations that are not structurally equal, so meshes cannot
    be interpolated etc. in a usual way).

    Note that the normal blending control (for rendering transparent materials)
    has to be disabled for this, as this requires full control over blending.
    So Attributes.Blending must always remain @false.
    We also have to modify the alpha value, using Attributes.Opacity.

    Attributes.BlendingSourceFactor and Attributes.BlendingDestinationFactor
    are used, to control our blending. So you can e.g.
    set BlendingDestinationFactor to GL_ONE_MINUS_SRC_ALPHA to get (sometimes)
    better alpha look.

    This also ignores TimeLoop (works like it's always @true) and
    TimeBackwards (works like it's always @false). }
  TBlendedLoopingAnimation = class(TCastlePrecalculatedAnimation)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;
  end;

  { TBlendedLoopingAnimation with a GLSL shader with a cubemap. }
  TBlendedLoopingAnimationShader = class(TBlendedLoopingAnimation)
  private
    CustomShader: TGLSLProgram;
  public
    procedure GLContextClose; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

implementation

uses Math, CastleUtils, CastleGLUtils, CastleStringUtils, SysUtils,
  CastleFilesUtils, CastleRenderingCamera, CastleDDS, CastleGL, CastleGLImages, CastleRenderer;

{ TBlendedLoopingAnimation --------------------------------------------------- }

constructor TBlendedLoopingAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.Blending := false;
end;

procedure TBlendedLoopingAnimation.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
var
  SceneIndex, MiddleIndex, HalfIndex: Integer;
  Amount: Single;
begin
  if Loaded and GetExists and
    Params.Transparent and Params.ShadowVolumesReceivers then
  begin
    SceneIndex := Floor(MapRange(Time, TimeBegin, TimeEnd, 0, ScenesCount)) mod ScenesCount;
    if SceneIndex < 0 then SceneIndex += ScenesCount; { we wanted "unsigned mod" above }
    MiddleIndex := ScenesCount div 2;

    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
    glBlendFunc(Attributes.BlendingSourceFactor, Attributes.BlendingDestinationFactor); // saved by GL_COLOR_BUFFER_BIT
    glDepthMask(GL_FALSE); // saved by GL_DEPTH_BUFFER_BIT

    { calculate Amount.

      On TimeBegin (ModResult = 0) and
      TimeEnd (ModResult = ScenesCount - 1), it's 0.
      Exactly in the middle (ModResult = MiddleIndex), it's 1.
      Between, it's linearly interpolated.
      This is the visibility of the 1st (unshifted) copy of animation.
      Since it's not visible at TimeBegin and TimeEnd, the looping seam
      is not visible.

      The second (shifted) copy of the animation has always visibility
      1-Amount. And it's shifted by half time range (MiddleIndex).
      This way the seam happens at MiddleIndex, when the shifted animation
      is not visible, so the looping seam is again not visible. }
    if SceneIndex >= MiddleIndex then
    begin
      HalfIndex := MiddleIndex - 1 - (SceneIndex - MiddleIndex);

      { Note that when ScenesCount is odd, SceneIndex may be (at max)
        ScenesCount - 1 = (MiddleIndex * 2 + 1) - 1 = MiddleIndex * 2.
        Then HalfIndex is calculated as -1 above. Fix it. }
      MaxTo1st(HalfIndex, 0);
    end else
      HalfIndex := SceneIndex;
    Assert((ScenesCount <= 1) or ((0 <= HalfIndex) and (HalfIndex < MiddleIndex)));
    Amount := HalfIndex / (MiddleIndex - 1);

    { Since we use alpha < 1 here (and disable material control by scenes),
      actually everything renderer here is a transparent object
      (that's why we check Params.Transparent above).

      However, with Blending := false, TCastleScene will assume that
      everything is opaque and should be rendered only when Transparent = false.
      So temporarily switch Transparent. }
    Params.Transparent := false;

      Attributes.Opacity := Amount;
      Scenes[SceneIndex].Render(Frustum, Params);

      Attributes.Opacity := 1 - Amount;
      Scenes[(SceneIndex + MiddleIndex) mod ScenesCount].Render(Frustum, Params);

    Params.Transparent := true;

    { restore glDepthMask and blending state to default values }
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);
  end;
end;

{ TWaterShader --------------------------------------------------------------- }

type
  TWaterShader = class(TGLSLProgram)
  private
    WaterEnvMap: TGLuint;
  public
    constructor Create(Attributes: TRenderingAttributes);
    destructor Destroy; override;
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
  end;

constructor TWaterShader.Create(Attributes: TRenderingAttributes);

  function LoadWaterEnvMap: TGLuint;
  var
    DDS: TDDSImage;
  begin
    glGenTextures(1, @Result);

    DDS := TDDSImage.Create;
    try
      DDS.LoadFromFile(ApplicationData(
        'levels/fountain/water_reflections/water_environment_map.dds'));

      glBindTexture(GL_TEXTURE_CUBE_MAP, Result);
      SetTextureFilter(GL_TEXTURE_CUBE_MAP, Attributes.TextureFilter);
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTextureCubeMap(Result,
        DDS.CubeMapImage(dcsPositiveX),
        DDS.CubeMapImage(dcsNegativeX),
        { Swap meaning of positive/negative Y faces from DDS,
          see TDDSCubeMapSide for explanation. }
        DDS.CubeMapImage(dcsNegativeY),
        DDS.CubeMapImage(dcsPositiveY),
        DDS.CubeMapImage(dcsPositiveZ),
        DDS.CubeMapImage(dcsNegativeZ),
        DDS,
        Attributes.TextureFilter.NeedsMipmaps);
    finally FreeAndNil(DDS); end;
  end;

var
  ShadersPath: string;
begin
  inherited Create;
  if (GLFeatures.TextureCubeMap <> gsNone) and
     (Support <> gsNone) then
  begin
    WaterEnvMap := LoadWaterEnvMap;

    ShadersPath := ApplicationData('levels/fountain/water_reflections/water_reflections.');
    AttachVertexShader(FileToString(ShadersPath + 'vs'));
    AttachFragmentShader(FileToString(ShadersPath + 'fs'));
    Link(true);
  end;
end;

destructor TWaterShader.Destroy;
begin
  glFreeTexture(WaterEnvMap);
  inherited;
end;

function TWaterShader.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;
begin
  Result := inherited SetupUniforms(BoundTextureUnits);

  glActiveTexture(GL_TEXTURE0 + BoundTextureUnits);
  glBindTexture(GL_TEXTURE_CUBE_MAP, WaterEnvMap);
  SetUniform('envMap', TGLint(BoundTextureUnits));
  Inc(BoundTextureUnits);

  RenderingCamera.RotationInverseMatrixNeeded;
  SetUniform('cameraRotationInverseMatrix', RenderingCamera.RotationInverseMatrix3);
end;

{ TBlendedLoopingAnimationShader --------------------------------------------- }

procedure TBlendedLoopingAnimationShader.GLContextClose;
begin
  if CustomShader <> nil then
    FreeAndNil(CustomShader);
  inherited;
end;

procedure TBlendedLoopingAnimationShader.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  if Attributes.CustomShader = nil then
  begin
    CustomShader := TWaterShader.Create(Attributes);
    Attributes.CustomShader := CustomShader;
  end;
  inherited;
end;

end.
