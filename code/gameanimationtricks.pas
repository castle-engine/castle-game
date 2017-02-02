{
  Copyright 2010-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Tricks with precalculated animation for "The Castle". }
unit GameAnimationTricks;

{$I castleconf.inc}

interface

uses Classes, CastlePrecalculatedAnimation, CastleFrustum, CastleVectors,
  CastleGLShaders, Castle3D, CastleRendererInternalShader;

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
    CustomShader: TX3DShaderProgramBase;
  public
    procedure GLContextClose; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

implementation

uses Math, CastleUtils, CastleGLUtils, CastleStringUtils, SysUtils,
  CastleFilesUtils, CastleRenderingCamera, CastleCompositeImage, CastleGL,
  CastleGLImages, CastleRenderer,
  GameVideoOptions;

{ TBlendedLoopingAnimation --------------------------------------------------- }

constructor TBlendedLoopingAnimation.Create(AOwner: TComponent);
begin
  inherited;
  //Attributes.Blending := false;
end;

procedure TBlendedLoopingAnimation.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  { on a still screenshot, the trick is easily visible and looks bad }
  inherited;
end;

{ TWaterShader --------------------------------------------------------------- }

type
  TWaterShader = class(TX3DShaderProgramBase)
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
    Image: TCompositeImage;
  begin
    glGenTextures(1, @Result);

    Image := TCompositeImage.Create;
    try
      Image.LoadFromFile(ApplicationData(
        'levels/fountain/water_reflections/water_environment_map.dds'));

      glBindTexture(GL_TEXTURE_CUBE_MAP, Result);
      SetTextureFilter(GL_TEXTURE_CUBE_MAP, Attributes.TextureFilter);
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTextureCubeMap(Result,
        Image.CubeMapImage(csPositiveX),
        Image.CubeMapImage(csNegativeX),
        Image.CubeMapImage(csPositiveY),
        Image.CubeMapImage(csNegativeY),
        Image.CubeMapImage(csPositiveZ),
        Image.CubeMapImage(csNegativeZ),
        Image,
        Attributes.TextureFilter.NeedsMipmaps);
    finally FreeAndNil(Image); end;
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
    Link;
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
  {$ifndef OpenGLES}
  { our shader cannot work with OpenGLES }
  if Attributes.CustomShader = nil then
  begin
    CustomShader := TWaterShader.Create(Attributes);
    Attributes.CustomShader := CustomShader;
  end;
  {$endif}
  inherited;
end;

end.
