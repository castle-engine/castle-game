{
  Copyright 2006 Michalis Kamburelis.

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

uses OpenGLh;

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

  TextureMinificationQualityToGL:
    array[TTextureMinificationQuality] of TGLint =
  ( GL_NEAREST, GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR );

  { Default value for TextureMinificationQuality.

    This is the slowest one, but it looks perfect.
    In fact, it's not so very slow on my system, so I think that
    this can be the default. }
  DefaultTextureMinificationQuality = tqLinearMipmapLinear;

var
  TextureMinificationQuality: TTextureMinificationQuality;

const
  DefaultAllowScreenResize = true;
  AllowScreenResizeToStr: array[boolean] of string = ('No', 'Yes');

var
  AllowScreenResize: boolean;

const
  DefaultCreatureAnimationScenesPerTime = 30;
  MinCreatureAnimationScenesPerTime = 10;
  MaxCreatureAnimationScenesPerTime = 50;

var
  CreatureAnimationScenesPerTime: Cardinal;

implementation

uses CastleConfig;

initialization
  TextureMinificationQuality := TTextureMinificationQuality(
    ConfigFile.GetValue(
    'video_options/texture_minification_quality',
    Ord(DefaultTextureMinificationQuality)));
  AllowScreenResize := ConfigFile.GetValue(
    'video_options/allow_screen_resize', DefaultAllowScreenResize);
  CreatureAnimationScenesPerTime := ConfigFile.GetValue(
    'video_options/creature_animation_smoothness',
    DefaultCreatureAnimationScenesPerTime);
finalization
  ConfigFile.SetDeleteValue(
    'video_options/texture_minification_quality',
    Ord(TextureMinificationQuality), Ord(DefaultTextureMinificationQuality));
  ConfigFile.SetDeleteValue(
    'video_options/allow_screen_resize',
    AllowScreenResize, DefaultAllowScreenResize);
  ConfigFile.SetDeleteValue(
    'video_options/creature_animation_smoothness',
    CreatureAnimationScenesPerTime, DefaultCreatureAnimationScenesPerTime);
end.