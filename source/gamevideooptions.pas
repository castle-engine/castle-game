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

uses GL, CastleGLUtils, CastleScene, X3DNodes;

const
  DefaultAllowScreenChange = true;

var
  AllowScreenChange: boolean;

const
  DefaultShadowVolumes = true;

var
  { Render shadow volumes.
    This is meaningfull only if GLShadowVolumesPossible,
    otherwise we will never render shadow volumes. }
  ShadowVolumes: boolean = DefaultShadowVolumes;

  { You can set this to true for debug purposes.
    This is meaningull only if GLShadowVolumesPossible and ShadowVolumes. }
  ShadowVolumesDraw: boolean = false;

const
  DefaultColorBits = 0;
var
  { 0 means "use system default" }
  ColorBits: Cardinal;

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

uses SysUtils, CastleUtils, CastleGameCache, RaysWindow, GameWindow, 
  CastleConfig, CastleWindow;

type
  TGameVideoOptions = class
    class procedure AttributesSet(Attributes: TSceneRenderingAttributes);
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TGameVideoOptions.AttributesSet(Attributes: TSceneRenderingAttributes);
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

class procedure TGameVideoOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  AllowScreenChange := Config.GetValue(
    'video_options/allow_screen_change', DefaultAllowScreenChange);
  ShadowVolumes := Config.GetValue(
    'video_options/shadows', DefaultShadowVolumes);
  ColorBits := Config.GetValue(
    'video_options/color_bits', DefaultColorBits);
  VideoFrequency := Config.GetValue(
    'video_options/frequency', DefaultVideoFrequency);
  Window.AntiAliasing := TAntiAliasing(Config.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
end;

class procedure TGameVideoOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue(
    'video_options/allow_screen_change',
    AllowScreenChange, DefaultAllowScreenChange);
  Config.SetDeleteValue(
    'video_options/shadows',
    ShadowVolumes, DefaultShadowVolumes);
  Config.SetDeleteValue(
    'video_options/color_bits',
    ColorBits, DefaultColorBits);
  Config.SetDeleteValue(
    'video_options/frequency',
    VideoFrequency, DefaultVideoFrequency);
  Config.SetDeleteValue('video_options/anti_aliasing',
    Ord(Window.AntiAliasing), Ord(DefaultAntiAliasing));
end;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX,
    Window.Height / Window.Width);
end;

initialization
  TSceneRenderingAttributes.OnCreate := @TGameVideoOptions(nil).AttributesSet;
  Config.OnLoad.Add(@TGameVideoOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TGameVideoOptions(nil).SaveToConfig);
end.
