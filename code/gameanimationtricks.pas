{
  Copyright 2010-2022 Michalis Kamburelis.

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

{$I castlegameconf.inc}

interface

uses Classes,
  CastleFrustum, CastleVectors, CastleScene, CastleGL,
  CastleGLShaders, CastleTransform, CastleRendererInternalShader,
  CastleRenderOptions, CastleCameras;

type
  { Scene with a GLSL shader with a cubemap. }
  TSceneWaterShader = class(TCastleScene)
  strict private
    type
      TWaterShader = class(TX3DShaderProgramBase)
      private
        WaterEnvMap: TGLuint;
        Camera: TCastleCamera;
      public
        constructor Create(RenderOptions: TCastleRenderOptions);
        destructor Destroy; override;
        function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
      end;
    var
      CustomShader: TWaterShader;
  public
    procedure GLContextClose; override;
    procedure LocalRender(const Params: TRenderParams); override;
  end;

implementation

uses Math, CastleUtils, CastleGLUtils, CastleStringUtils, SysUtils,
  CastleFilesUtils, CastleInternalCompositeImage, CastleLog,
  CastleGLImages;

{ TWaterShader --------------------------------------------------------------- }

constructor TSceneWaterShader.TWaterShader.Create(RenderOptions: TCastleRenderOptions);

  function LoadWaterEnvMap: TGLuint;
  var
    Image: TCompositeImage;
    TexFilter: TTextureFilter;
  begin
    glGenTextures(1, @Result);

    Image := TCompositeImage.Create;
    try
      Image.LoadFromFile('castle-data:/levels/fountain/water_reflections/water_environment_map.dds');

      glBindTexture(GL_TEXTURE_CUBE_MAP, Result);
      TexFilter := TextureFilter(minLinearMipmapLinear, magLinear);
      SetTextureFilter(GL_TEXTURE_CUBE_MAP, TexFilter);
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
        TexFilter.NeedsMipmaps);
    finally FreeAndNil(Image); end;
  end;

var
  ShadersPath: string;
begin
  inherited Create;
  if (GLFeatures.TextureCubeMap <> gsNone) and
     (GLFeatures.Shaders <> gsNone) then
  begin
    WaterEnvMap := LoadWaterEnvMap;

    ShadersPath := 'castle-data:/levels/fountain/water_reflections/water_reflections.';
    AttachVertexShader(FileToString(ShadersPath + 'vs'));
    AttachFragmentShader(FileToString(ShadersPath + 'fs'));
    Link;
  end;
end;

destructor TSceneWaterShader.TWaterShader.Destroy;
begin
  glFreeTexture(WaterEnvMap);
  inherited;
end;

function TSceneWaterShader.TWaterShader.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;

  function ToMatrix3(const M: TMatrix4): TMatrix3;
  begin
    System.Move(M.Data[0], Result.Data[0], SizeOf(Single) * 3);
    System.Move(M.Data[1], Result.Data[1], SizeOf(Single) * 3);
    System.Move(M.Data[2], Result.Data[2], SizeOf(Single) * 3);
  end;

var
  RotationInverseMatrix: TMatrix4;
  RotationInverseMatrix3: TMatrix3;
begin
  Result := inherited SetupUniforms(BoundTextureUnits);

  glActiveTexture(GL_TEXTURE0 + BoundTextureUnits);
  glBindTexture(GL_TEXTURE_CUBE_MAP, WaterEnvMap);
  SetUniform('envMap', TGLint(BoundTextureUnits));
  Inc(BoundTextureUnits);

  if not Camera.RotationMatrix.TryInverse(RotationInverseMatrix) then
  begin
    WritelnWarning('Camera rotation matrix cannot be inverted');
    Exit;
  end;
  RotationInverseMatrix3 := ToMatrix3(RotationInverseMatrix);
  SetUniform('cameraRotationInverseMatrix', RotationInverseMatrix3);
end;

{ TSceneWaterShader --------------------------------------------- }

procedure TSceneWaterShader.GLContextClose;
begin
  if CustomShader <> nil then
    FreeAndNil(CustomShader);
  inherited;
end;

procedure TSceneWaterShader.LocalRender(const Params: TRenderParams);
begin
  {$ifndef OpenGLES}
  { TODO: our shader cannot work with OpenGLES yet }
  if RenderOptions.CustomShader = nil then
  begin
    CustomShader := TWaterShader.Create(RenderOptions);
    RenderOptions.CustomShader := CustomShader;
  end;
  CustomShader.Camera := World.MainCamera;
  {$endif}
  inherited;
end;

end.
