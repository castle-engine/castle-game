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

{ Variables and utilities for things in "Video options" menu. }
unit GameVideoOptions;

interface

uses GL, CastleGLUtils, CastleScene, X3DNodes,
  PrecalculatedAnimationCore;

const
  DefaultAllowScreenChange = true;

var
  AllowScreenChange: boolean;

var
  { You can set this to true for testing purposes. }
  RenderBoundingBoxes: boolean = false;

  RenderDebugCaptions: boolean = false;

const
  DefaultRenderShadows = true;

var
  { If false then you should do *nothing* related to the shadows,
    i.e. you even shouldn't request stencil buffer for our window
    or do in Prepare appropriate preparations for shadows.

    In other words, RenderShadowsPossible = @false means that
    for the whole lifetime of this program RenderShadows will
    be treated like @false. }
  RenderShadowsPossible: boolean = true;

  { Should we actually render shadows ?
    This is meaningfull only if RenderShadowsPossible. }
  RenderShadows: boolean = DefaultRenderShadows;

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

const
  DefaultBumpMapping = true;

var
  BumpMapping: boolean;

const
  DefaultUseOcclusionQuery = false;
var
  { Should we use use occlusion query for levels. }
  UseOcclusionQuery: boolean;

implementation

uses SysUtils, CastleUtils, CastleGameConfig, CastleGameWindow, RaysWindow,
  GLAntiAliasing;

procedure AttributesSet(Attributes: TSceneRenderingAttributes);
begin
  { Disadvantage: it only increases the image color, so partially
    transparent objects have a tendency to look all white on the level.
    Advantage: no sorting problems. }
  Attributes.BlendingSourceFactor := GL_SRC_ALPHA;
  Attributes.BlendingDestinationFactor := GL_ONE;

  { Disadvantage: it has a tendency to make color of the level
    (things behind the partially transparent object) seem too dark
    (since it scales image color down).
  Attributes.BlendingSourceFactor := GL_SRC_ALPHA;
  Attributes.BlendingDestinationFactor := GL_ONE_MINUS_SRC_ALPHA; }

  { main scene will override UseSceneLights back to @true,
    for other scenes we ignore lights --- for historic reasons,
    we couldn't support them well in the past. }
  Attributes.UseSceneLights := false;
end;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX,
    Window.Height / Window.Width);
end;

initialization
  TSceneRenderingAttributes.OnCreate := @AttributesSet;

  AllowScreenChange := ConfigFile.GetValue(
    'video_options/allow_screen_change', DefaultAllowScreenChange);
  RenderShadows := ConfigFile.GetValue(
    'video_options/shadows', DefaultRenderShadows);
  ColorDepthBits := ConfigFile.GetValue(
    'video_options/color_depth_bits', DefaultColorDepthBits);
  VideoFrequency := ConfigFile.GetValue(
    'video_options/frequency', DefaultVideoFrequency);
  BumpMapping := ConfigFile.GetValue(
    'video_options/bump_mapping', DefaultBumpMapping);
  AntiAliasing := TAntiAliasing(ConfigFile.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
  UseOcclusionQuery := ConfigFile.GetValue(
    'video_options/use_occlusion_query', DefaultUseOcclusionQuery);
finalization
  ConfigFile.SetDeleteValue(
    'video_options/allow_screen_change',
    AllowScreenChange, DefaultAllowScreenChange);
  ConfigFile.SetDeleteValue(
    'video_options/shadows',
    RenderShadows, DefaultRenderShadows);
  ConfigFile.SetDeleteValue(
    'video_options/color_depth_bits',
    ColorDepthBits, DefaultColorDepthBits);
  ConfigFile.SetDeleteValue(
    'video_options/frequency',
    VideoFrequency, DefaultVideoFrequency);
  ConfigFile.SetDeleteValue('video_options/bump_mapping',
    BumpMapping, DefaultBumpMapping);
  ConfigFile.SetDeleteValue('video_options/anti_aliasing',
    Ord(AntiAliasing), Ord(DefaultAntiAliasing));
  ConfigFile.SetDeleteValue('video_options/use_occlusion_query',
    UseOcclusionQuery, DefaultUseOcclusionQuery);
end.
