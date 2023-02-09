{
  Copyright 2006-2023 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Variables and utilities for things in "Video options" menu. }
unit GameVideoOptions;

interface

uses CastleScene, X3DNodes;

const
  DefaultShadowVolumes =
    {$ifdef ANDROID} false {$else}
    {$ifdef IOS}     false {$else}
                     true {$endif}
                          {$endif};
var
  { Render shadow volumes.
    This is meaningfull only if GLFeatures.ShadowVolumesPossible,
    otherwise we will never render shadow volumes. }
  ShadowVolumes: boolean = DefaultShadowVolumes;

  { You can set this to true for debug purposes.
    This is meaningull only if GLFeatures.ShadowVolumesPossible and ShadowVolumes. }
  ShadowVolumesRender: boolean = false;

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

var
  RenderDebug: boolean;

implementation

uses SysUtils, CastleUtils, GameWindow, CastleConfig, CastleWindow,
  CastleGLUtils, X3DLoad, CastlePlayer, CastleRenderOptions;

type
  TGameVideoOptions = class
    class procedure RenderOptionsSet(const RenderOptions: TCastleRenderOptions);
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TGameVideoOptions.RenderOptionsSet(const RenderOptions: TCastleRenderOptions);
begin
  { Disadvantage: it only increases the image color, so partially
    transparent objects have a tendency to look all white on the level.
    Advantage: no sorting problems. }
  RenderOptions.BlendingSourceFactor := bsSrcAlpha;
  RenderOptions.BlendingDestinationFactor := bdOne;

  { Disadvantage: it has a tendency to make color of the level
    (things behind the partially transparent object) seem too dark
    (since it scales image color down).
  RenderOptions.BlendingSourceFactor := GL_SRC_ALPHA;
  RenderOptions.BlendingDestinationFactor := GL_ONE_MINUS_SRC_ALPHA; }
end;

class procedure TGameVideoOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  ShadowVolumes := Config.GetValue(
    'video_options/shadows', DefaultShadowVolumes);
  ColorBits := Config.GetValue(
    'video_options/color_bits', DefaultColorBits);
  VideoFrequency := Config.GetValue(
    'video_options/frequency', DefaultVideoFrequency);
  Window.AntiAliasing := TAntiAliasing(Config.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
  BakedAnimationSmoothness := Config.GetFloat(
    'video_options/animation_smoothness', DefaultBakedAnimationSmoothness);

  { not really a "video option", but for now there's no better place to put this }
  AutoOpenInventory := Config.GetValue(
    'auto_open_inventory', DefaultAutoOpenInventory);
end;

class procedure TGameVideoOptions.SaveToConfig(const Config: TCastleConfig);
begin
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
  Config.SetDeleteFloat(
    'video_options/animation_smoothness',
    BakedAnimationSmoothness, DefaultBakedAnimationSmoothness);

  { not really a "video option", but for now there's no better place to put this }
  Config.SetDeleteValue('auto_open_inventory',
    AutoOpenInventory, DefaultAutoOpenInventory);
end;

initialization
  TCastleRenderOptions.OnCreate := @TGameVideoOptions(nil).RenderOptionsSet;
  UserConfig.AddLoadListener(@TGameVideoOptions(nil).LoadFromConfig);
  UserConfig.AddSaveListener(@TGameVideoOptions(nil).SaveToConfig);
end.
