{
  Copyright 2010-2011 Michalis Kamburelis.

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
unit CastleAnimationTricks;

interface

uses VRMLGLScene, Classes, VRMLGLAnimation, Frustum, VectorMath, GLShaders, GL;

type
  { Animation forced to seamlessly loop by blending the beginning frames
    with end frames. This is a really brutal (often looking bad),
    but universal way to make animation seamlessly loop (works
    even with animations that are not structurally equal, so meshes cannot
    be interpolated etc. in a usual way).

    Note that the normal blending control (for rendering transparent materials)
    has to be disabled for this, as this requires full control over blending.
    So Attributes.Blending and Attributes.ControlBlending must always remain
    @false. We also have to modify the alpha value, using
    Attributes.Opacity.

    Attributes.BlendingSourceFactor and Attributes.BlendingDestinationFactor
    are used, to control our blending. So you can e.g.
    set BlendingDestinationFactor to GL_ONE_MINUS_SRC_ALPHA to get (sometimes)
    better alpha look.

    This also ignores TimeLoop (works like it's always @true) and
    TimeBackwards (works like it's always @false). }
  TBlendedLoopingAnimation = class(TVRMLGLAnimation)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render(const Frustum: TFrustum;
      const LightsEnabled: Cardinal;
      const TransparentGroup: TTransparentGroup;
      InShadow: boolean); override;
  end;

  { TBlendedLoopingAnimation with a GLSL shader with a cubemap. }
  TBlendedLoopingAnimationShader = class(TBlendedLoopingAnimation)
  private
    GLSLProgram: TGLSLProgram;
    WaterEnvMap: TGLuint;
    function UseShader: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GLContextClose; override;
    procedure Render(const Frustum: TFrustum;
      const LightsEnabled: Cardinal;
      const TransparentGroup: TTransparentGroup;
      InShadow: boolean); override;
  end;

implementation

uses Math, KambiUtils, KambiGLUtils, KambiStringUtils, SysUtils,
  KambiFilesUtils, RenderStateUnit, DDS, GLExt, GLImages, VRMLGLRenderer;

{ TBlendedLoopingAnimation --------------------------------------------------- }

constructor TBlendedLoopingAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.Blending := false;
  Attributes.ControlBlending := false;
end;

procedure TBlendedLoopingAnimation.Render(const Frustum: TFrustum;
  const LightsEnabled: Cardinal;
  const TransparentGroup: TTransparentGroup; InShadow: boolean);
var
  SceneIndex, MiddleIndex, HalfIndex: Integer;
  Amount: Single;
begin
  if Loaded and Exists and (TransparentGroup in [tgAll, tgTransparent]) then
  begin
    SceneIndex := Floor(MapRange(Time, TimeBegin, TimeEnd, 0, ScenesCount)) mod ScenesCount;
    if SceneIndex < 0 then SceneIndex += ScenesCount; { we wanted "unsigned mod" above }
    MiddleIndex := ScenesCount div 2;

    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_LIGHTING_BIT or GL_DEPTH_BUFFER_BIT);
    try
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

      { We pass tgAll to our Scenes[].Render.
        Since we use alpha < 1 here (and disable material control by scenes),
        actually the whole scene is always a transparent object. }

      Attributes.Opacity := Amount;
      Scenes[SceneIndex].Render(Frustum, LightsEnabled, tgAll, InShadow);

      Attributes.Opacity := 1 - Amount;
      Scenes[(SceneIndex + MiddleIndex) mod ScenesCount].Render(
        Frustum, LightsEnabled, tgAll, InShadow);
    finally glPopAttrib end;
  end;
end;

{ TBlendedLoopingAnimationShader --------------------------------------------- }

constructor TBlendedLoopingAnimationShader.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.Shaders := srDisable;
end;

function TBlendedLoopingAnimationShader.UseShader: boolean;
begin
  Result := GL_ARB_texture_cube_map and (TGLSLProgram.ClassSupport <> gsNone);
end;

procedure TBlendedLoopingAnimationShader.GLContextClose;
begin
  if GLSLProgram <> nil then FreeAndNil(GLSLProgram);
  glFreeTexture(WaterEnvMap);
  inherited;
end;

procedure TBlendedLoopingAnimationShader.Render(const Frustum: TFrustum;
  const LightsEnabled: Cardinal;
  const TransparentGroup: TTransparentGroup; InShadow: boolean);

  function LoadWaterEnvMap: TGLuint;
  var
    DDS: TDDSImage;
  begin
    glGenTextures(1, @Result);

    DDS := TDDSImage.Create;
    try
      DDS.LoadFromFile(ProgramDataPath + 'data' + PathDelim + 'levels' +
        PathDelim + 'fountain' + PathDelim +  'water_reflections' +
        PathDelim + 'water_environment_map.dds');

      glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, Result);

      glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAG_FILTER, Attributes.TextureMagFilter);
      glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MIN_FILTER, Attributes.TextureMinFilter);

      glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTextureCubeMap(
        DDS.CubeMapImage(dcsPositiveX),
        DDS.CubeMapImage(dcsNegativeX),
        { Swap meaning of positive/negative Y faces from DDS,
          see TDDSCubeMapSide for explanation. }
        DDS.CubeMapImage(dcsNegativeY),
        DDS.CubeMapImage(dcsPositiveY),
        DDS.CubeMapImage(dcsPositiveZ),
        DDS.CubeMapImage(dcsNegativeZ),
        DDS,
        TextureMinFilterNeedsMipmaps(Attributes.TextureMinFilter));
    finally FreeAndNil(DDS); end;
  end;

var
  ShadersPath: string;
begin
  if UseShader then
  begin
    if WaterEnvMap = 0 then
      WaterEnvMap := LoadWaterEnvMap;

    if GLSLProgram = nil then
    begin
      GLSLProgram := TGLSLProgram.Create;
      ShadersPath := ProgramDataPath + 'data' + PathDelim + 'levels' +
        PathDelim + 'fountain' + PathDelim +  'water_reflections' +
        PathDelim + 'water_reflections.';
      GLSLProgram.AttachVertexShader(FileToString(ShadersPath + 'vs'));
      GLSLProgram.AttachFragmentShader(FileToString(ShadersPath + 'fs'));
      GLSLProgram.Link(true);
    end;

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, WaterEnvMap);

    GLSLProgram.Enable;
    RenderState.CameraRotationInverseMatrixNeeded;
    GLSLProgram.SetUniform('cameraRotationInverseMatrix', RenderState.CameraRotationInverseMatrix3);
    GLSLProgram.SetUniform('envMap', 0);
  end;

  inherited;

  if UseShader then
    GLSLProgram.Disable;
end;

end.
