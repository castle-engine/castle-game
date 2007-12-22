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

{ Variables and utilities for things in "Video options" menu. }
unit CastleVideoOptions;

interface

uses GL, GLU, GLExt, VRMLFlatSceneGL;

type
  { This determines OpenGL blending factors used when rendering given model. }
  TBlendingType = (

    { Use OpenGL blending factors (GL_SRC_ALPHA, GL_ONE).
      These are the default OpenGL blending factors.
      Disadvantage: it only increases the image color, so partially
      transparent objects have a tendency to look all white on the level. }
    btIncrease,

    { Use OpenGL blending factors (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA).
      Disadvantage: it has a tendency to make color of the level
      (things behind the partially transparent object) to look too dark
      (since it scales image color down). }
    btScale);

const
  DefaultBlendingType = btIncrease;

type
  { Approximately from worst to best. }
  TTextureMinificationQuality =
  ( tqNearest, tqLinear,
    tqNearestMipmapNearest, tqNearestMipmapLinear,
    tqLinearMipmapNearest, tqLinearMipmapLinear );

const
  TextureMinificationQualityToStr:
    array[TTextureMinificationQuality] of string =
  ( 'NEAREST', 'LINEAR',
    'NEAREST_MIPMAP_NEAREST', 'NEAREST_MIPMAP_LINEAR',
    'LINEAR_MIPMAP_NEAREST', 'LINEAR_MIPMAP_LINEAR' );

  { Default value for TextureMinificationQuality.

    This is the slowest one, but it looks perfect.
    In fact, it's not so very slow on my system, so I think that
    this can be the default. }
  DefaultTextureMinificationQuality = tqLinearMipmapLinear;

var
  TextureMinificationQuality: TTextureMinificationQuality;

{ Set Attributes as needed. Right now this means setting
  current TextureMinificationQuality. }
procedure AttributesSet(Attributes: TVRMLSceneRenderingAttributes;
  BlendingType: TBlendingType);

{ Set Attributes of animation as needed. In theory you should
  call AttributesSet here, but in practice animation's attributes
  must be a little different, otherwise animations can
  take too much memory to load. }
procedure AnimationAttributesSet(Attributes: TVRMLSceneRenderingAttributes;
  BlendingType: TBlendingType);

const
  DefaultAllowScreenChange = true;

var
  AllowScreenChange: boolean;

const
  DefaultCreatureAnimationScenesPerTime = 30;
  MinCreatureAnimationScenesPerTime = 5;
  MaxCreatureAnimationScenesPerTime = 40;

var
  CreatureAnimationScenesPerTime: Cardinal;

var
  { You can set this to true for testing purposes. }
  RenderBoundingBoxes: boolean = false;

  RenderDebugCaptions: boolean = false;

const
  DefaultRenderShadows = false;

var
  { If false then you should do *nothing* related to the shadows,
    i.e. you even shouldn't request stencil buffer for our window
    or do in PrepareRender appropriate preparations for shadows.

    In other words, RenderShadowsPossible = @false means that
    for the whole lifetime of this program RenderShadows will
    be treated like @false. }
  RenderShadowsPossible: boolean = true;

  { Should we actually render shadows ?
    This is meaningfull only if RenderShadowsPossible. }
  RenderShadows: boolean;

  { You can set this to true for debug purposes.
    This is meaningull only if RenderShadowsPossible and RenderShadows. }
  DebugRenderShadowVolume: boolean = false;

const
  DefaultColorDepthBits = 0;
var
  { 0 means "use system default" }
  ColorDepthBits: Cardinal;

const
  DefaultVideoFrequency = 0 ;
var
  { 0 means "use system default" }
  VideoFrequency: Cardinal;

{ ViewAngleDegX and ViewAngleDegY specify field of view in the game.
  You can freely change ViewAngleDegX at runtime, just make sure
  that our OnResize will be called after. }
var
  ViewAngleDegX: Single = 70.0;
function ViewAngleDegY: Single;

implementation

uses KambiUtils, CastleConfig, CastleWindow, RaysWindow;

procedure AttributesSet(Attributes: TVRMLSceneRenderingAttributes;
  BlendingType: TBlendingType);
const
  TextureMinificationQualityToGL:
    array[TTextureMinificationQuality] of TGLint =
  ( GL_NEAREST, GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR );
begin
  Attributes.TextureMinFilter :=
    TextureMinificationQualityToGL[TextureMinificationQuality];

  case BlendingType of
    btIncrease:
      begin
        Attributes.BlendingSourceFactor := GL_SRC_ALPHA;
        Attributes.BlendingDestinationFactor := GL_ONE;
      end;
    btScale:
      begin
        Attributes.BlendingSourceFactor := GL_SRC_ALPHA;
        Attributes.BlendingDestinationFactor := GL_ONE_MINUS_SRC_ALPHA;
      end;
    else
      raise EInternalError.Create('20061126-case BlendingType');
  end;
end;

procedure AnimationAttributesSet(Attributes: TVRMLSceneRenderingAttributes;
  BlendingType: TBlendingType);
begin
  { Despite the comments in the interface, this is the same thing
    as AttributesSet for now. }
  AttributesSet(Attributes, BlendingType);
end;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX,
    Glw.Height / Glw.Width);
end;

initialization
  TextureMinificationQuality := TTextureMinificationQuality(
    ConfigFile.GetValue(
    'video_options/texture_minification_quality',
    Ord(DefaultTextureMinificationQuality)));
  AllowScreenChange := ConfigFile.GetValue(
    'video_options/allow_screen_change', DefaultAllowScreenChange);
  CreatureAnimationScenesPerTime := ConfigFile.GetValue(
    'video_options/creature_animation_smoothness',
    DefaultCreatureAnimationScenesPerTime);
  RenderShadows := ConfigFile.GetValue(
    'video_options/shadows', DefaultRenderShadows);
  ColorDepthBits := ConfigFile.GetValue(
    'video_options/color_depth_bits', DefaultColorDepthBits);
  VideoFrequency := ConfigFile.GetValue(
    'video_options/frequency', DefaultVideoFrequency);
finalization
  ConfigFile.SetDeleteValue(
    'video_options/texture_minification_quality',
    Ord(TextureMinificationQuality), Ord(DefaultTextureMinificationQuality));
  ConfigFile.SetDeleteValue(
    'video_options/allow_screen_change',
    AllowScreenChange, DefaultAllowScreenChange);
  ConfigFile.SetDeleteValue(
    'video_options/creature_animation_smoothness',
    CreatureAnimationScenesPerTime, DefaultCreatureAnimationScenesPerTime);
  ConfigFile.SetDeleteValue(
    'video_options/shadows',
    RenderShadows, DefaultRenderShadows);
  ConfigFile.SetDeleteValue(
    'video_options/color_depth_bits',
    ColorDepthBits, DefaultColorDepthBits);
  ConfigFile.SetDeleteValue(
    'video_options/frequency',
    VideoFrequency, DefaultVideoFrequency);
end.